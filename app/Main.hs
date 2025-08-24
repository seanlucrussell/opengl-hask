{-# LANGUAGE OverloadedStrings,LinearTypes,ScopedTypeVariables,OverloadedStrings,MultilineStrings,DataKinds,TypeOperators,FlexibleInstances,UndecidableInstances,TypeApplications,AllowAmbiguousTypes #-}
module Main where
import           Control.Applicative
import           Control.Monad (unless,when)
import           Data.Finite
import qualified Foreign
import           GHC.TypeNats
import qualified Data.Array
import qualified Foreign.C.String
import qualified Graphics.GL.Core46 as GL
import qualified SDL
import qualified SDL.Raw.Event
import qualified SDL.Raw.Video

import qualified System.Random

import qualified System.IO as IO
import qualified System.Exit


import qualified Text.Megaparsec            as MP
import qualified Text.Megaparsec.Char       as C
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.Char  as Char
import           Data.Void  (Void)
import           Control.Applicative ((<|>), optional)
import qualified Control.Monad (void)
import qualified System.Exit

import qualified Codec.Picture
import qualified Data.Vector.Storable

import qualified Data.Set


--- LINEAR ALGEBRA LIBRARY -----------------------------------------------------

type Vec n = Finite n -> Float
type Mat m n = Finite m -> Finite n -> Float

x :: (KnownNat n, 1 <= n) => Finite n; x = 0
y :: (KnownNat n, 2 <= n) => Finite n; y = 1
z :: (KnownNat n, 3 <= n) => Finite n; z = 2
w :: (KnownNat n, 4 <= n) => Finite n; w = 3

r :: (KnownNat n, 1 <= n) => Finite n; r = 0
g :: (KnownNat n, 2 <= n) => Finite n; g = 1
b :: (KnownNat n, 3 <= n) => Finite n; b = 2

v2 :: Float -> Float -> Vec 2;                   v2 x y i = case i of {0->x;1->y}
v3 :: Float -> Float -> Float -> Vec 3;          v3 x y z i = case i of {0->x;1->y;2->z}
v4 :: Float -> Float -> Float -> Float -> Vec 4; v4 x y z w i = case i of {0->x;1->y;2->z;3->w}

class Flatten a where flatten :: a -> [GL.GLfloat]
instance KnownNat n => Flatten (Vec n) where flatten v = v <$> finites
instance (KnownNat m, KnownNat n) => Flatten (Mat m n) where flatten m = flip m <$> finites <*> finites
instance Flatten Vertex where flatten vertex = flatten (position vertex) ++ flatten (color vertex)
instance Flatten a => Flatten [a] where flatten = concatMap flatten

instance KnownNat n => Show (Vec n) where show v = "v" ++ show (1+fromIntegral (maxBound :: Finite n)) ++ show (v <$> finites)

instance Num (Vec n) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  fromInteger = pure . fromInteger
  (-) = liftA2 (-)
  abs = fmap abs
  signum = fmap signum

instance Fractional (Vec n) where
  fromRational = pure . fromRational
  (/) = liftA2 (/)

translate :: Vec 3 -> Vec 4 -> Vec 4
translate d v 0 = v w * d x + v x
translate d v 1 = v w * d y + v y
translate d v 2 = v w * d z + v z
translate d v 3 = v w

projection :: Float -> Float -> Float -> Float -> Vec 4 -> Vec 4
projection fov aspect near far v 0 = v x / (aspect * tan (pi*fov/360))
projection fov aspect near far v 1 = v y / tan (pi*fov/360)
projection fov aspect near far v 2 = -(v z*(far+near) + v w*2*far*near)/(far-near)
projection fov aspect near far v 3 = -v z

rotation :: Float -> Vec 3 -> Vec 4 -> Vec 4
rotation angle axis v i =
  let
    theta = pi * angle / 180
    u :: Vec 3 = memo (normalize axis)
  in case i of
    0 -> v x*cos theta + (u y*v z - u z*v y) * sin theta + u x*(u x*v x + u y*v y + u z*v z)*(1 - cos theta)
    1 -> v y*cos theta + (u z*v x - u x*v z) * sin theta + u y*(u x*v x + u y*v y + u z*v z)*(1 - cos theta)
    2 -> v z*cos theta + (u x*v y - u y*v x) * sin theta + u z*(u x*v x + u y*v y + u z*v z)*(1 - cos theta)
    3 -> v w

transformToMat :: (KnownNat m, KnownNat n) => (Vec n -> Vec m) -> Mat m n
transformToMat f row column = f (\k -> if k == column then 1 else 0) row

contract :: KnownNat n => Vec n -> Float; contract v = sum (v <$> finites)
dot v1 v2 = contract (\k -> v1 k * v2 k)
magnitude v = sqrt (v `dot` v)
normalize :: KnownNat n => Vec n -> Vec n; normalize v = (/(magnitude v)) . v

identity i j = if i == j then 1 else 0

cross :: Vec 3 -> Vec 3 -> Vec 3
cross a b i = a (i+1) * b (i+2) - b (i+1) * a (i+2) 

lookAt :: Vec 3 -> Vec 3 -> Vec 3 -> Vec 4 -> Vec 4
lookAt eye center up v i =
  let
    v' = v3 (v x) (v y) (v z)
    f = normalize (center - eye)
    r = normalize (cross f up)     -- right  (s  in glm docs)
    u =         (cross r f)        -- true up
    tR = - dot r eye               -- translation components
    tU = - dot u eye
    tF =   dot f eye
  in case i of
       0 ->  dot r v' + tR * v w     -- x′
       1 ->  dot u v' + tU * v w     -- y′
       2 -> -dot f v' + tF * v w     -- z′  (note the minus)
       3 ->  v w                    -- w′

worldToViewMatrix :: Vec 3 -> Vec 3 -> Vec 4 -> Vec 4
worldToViewMatrix position viewDirection = lookAt position (\k -> position k + viewDirection k) (v3 0 1 0)

memo :: forall a. KnownNat a => Vec a -> Vec a
memo v = (Data.Array.!) (Data.Array.listArray (0, fromIntegral (maxBound :: Finite a)) (map v finites)) . fromIntegral


--- SMD PARSER -----------------------------------------------------------------

data SMD_Node = SMD_Node Int String Int deriving (Show)
data SMD_BoneFrame = SMD_BoneFrame { boneId :: Int, pos :: Vec 3, rot :: Vec 3 } deriving (Show)
data SMD_SkeletonFrame = SMD_SkeletonFrame { frameNum :: Int, bones :: [SMD_BoneFrame] } deriving (Show)
data SMD_Vertex = SMD_Vertex
  { vParent  :: Int
  , vPos     :: Vec 3
  , vNormal  :: Vec 3
  , vUV      :: Vec 2
  , vWeights :: [(Int, Float)]
  } deriving (Show)
data SMD_Triangle = SMD_Triangle { material :: String, verts :: (SMD_Vertex,SMD_Vertex,SMD_Vertex) } deriving (Show)
data SMD = SMD { nodes :: [SMD_Node], skeleton :: [SMD_SkeletonFrame], triangles :: [SMD_Triangle] } deriving (Show)

pSMD :: Parser SMD
pSMD =
  let sc            = L.space C.space1 empty empty                                        :: Parser ()              
      lexeme        = L.lexeme sc                                                         :: Parser a -> Parser a
      integer       = lexeme (L.signed sc L.decimal)                                      :: Parser Int             
      float         = lexeme (L.signed sc L.float)                                        :: Parser Float           
      symbol        = L.symbol sc                                                         :: String -> Parser String
      stringLiteral = lexeme (C.char '"' *> MP.manyTill L.charLiteral (C.char '"'))       :: Parser String          
      pNodes        = symbol "nodes" >> MP.manyTill (lexeme pNode) (symbol "end")         :: Parser [SMD_Node]      
      pNode         = SMD_Node <$> integer <*> stringLiteral <*> integer                  :: Parser SMD_Node        
      pBoneFrame :: Parser SMD_BoneFrame = do
        i <- integer
        x <- float; y <- float; z <- float
        rx <- float; ry <- float; rz <- float
        pure (SMD_BoneFrame i (v3 x y z) (v3 rx ry rz))
      pSkeletonFrame :: Parser SMD_SkeletonFrame = do
        symbol "time"
        t <- integer
        fs <- MP.manyTill (lexeme pBoneFrame) (MP.lookAhead (symbol "time" <|> symbol "end"))
        pure (SMD_SkeletonFrame t fs)
      pSkeleton :: Parser [SMD_SkeletonFrame] = symbol "skeleton" >> MP.manyTill pSkeletonFrame (symbol "end")
      pVertex :: Parser SMD_Vertex = lexeme $ do
        pb <- integer
        x  <- float; y <- float; z <- float
        nx <- float; ny <- float; nz <- float
        u  <- float; v <- float
        mw <- optional integer
        extras <- case mw of
          Nothing -> pure []
          Just k  -> MP.count k ((,) <$> integer <*> float)
        let weights
              | null extras             = [(pb, 1)]
              | any ((pb==).fst) extras = extras
              | otherwise               = (pb, max 0 (1-(sum (map snd extras)))) : extras
        pure (SMD_Vertex pb (v3 x y z) (v3 nx ny nz) (v2 u v) weights)
      pTriangle :: Parser SMD_Triangle =
        SMD_Triangle
          <$> lexeme (MP.someTill MP.anySingle C.eol)
          <*> ((,,) <$> pVertex <*> pVertex <*> pVertex)
      pTriangles    = symbol "triangles" >> MP.manyTill (lexeme pTriangle) (symbol "end") :: Parser [SMD_Triangle]  
  in do
  _ <- symbol "version" *> integer
  ns <- pNodes
  sk <- pSkeleton
  ts <- pTriangles
  MP.eof
  pure (SMD ns sk ts)

zigscale = 0.03
-- zigscale = 1

smdVertices :: SMD -> [Vertex]
smdVertices smd = do
  (va,vb,vc) <- verts <$> triangles smd
  [  Vertex (zigscale * vPos va) (v3 1 1 1)
   , Vertex (zigscale * vPos vb) (v3 1 1 1)
   , Vertex (zigscale * vPos vc) (v3 1 1 1)
   ]

smdFaces :: SMD -> [Face]
smdFaces smd = do
  n <- [0..length (triangles smd)-1]
  pure (3*n,3*n+1,3*n+2)

smdToObj :: SMD -> Obj
smdToObj smd = Obj (smdVertices smd) (smdFaces smd)

smdNormals :: SMD -> [Vec 3]
smdNormals smd = do
  (va,vb,vc) <- verts <$> triangles smd
  [  vNormal va
   , vNormal vb
   , vNormal vc
   ]

smdUVs :: SMD -> [Vec 2]
smdUVs smd = do
  (va,vb,vc) <- verts <$> triangles smd
  [  vUV va
   , vUV vb
   , vUV vc
   ]

smdMats :: SMD -> Data.Set.Set String
smdMats = Data.Set.fromList . fmap material . triangles

--- OBJ PARSER -----------------------------------------------------------------

type Parser = MP.Parsec Void String
type Face = (Int, Int, Int)
data Vertex = Vertex {position :: Vec 3, color :: Vec 3} deriving Show
data Obj = Obj { vertices :: [Vertex], faces :: [Face] } deriving Show

pObj :: Parser Obj
pObj =
  let
    hspace1 = Control.Monad.void $ MP.some (MP.satisfy Char.isSpace) :: Parser ()
    float = L.signed (pure ()) L.float :: Parser Float
    vertexLine :: Parser (Obj -> Obj) = do
      C.char 'v'
      v <- v3 <$> (hspace1 >> float) <*> (hspace1 >> float) <*> (hspace1 >> float)
      return (\o -> o { vertices = Vertex v (v3 1 1 1):vertices o })
    faceLine :: Parser (Obj -> Obj)= do
      C.char 'f'
      a <- hspace1 >> L.decimal
      b <- hspace1 >> L.decimal
      c <- hspace1 >> L.decimal
      return (\o -> o { faces = (a-1, b-1, c-1) : faces o })
    skipLine :: Parser (Obj -> Obj) = MP.manyTill MP.anySingle (MP.lookAhead (C.eol >> pure () <|> MP.eof)) >> pure id
    line :: Parser (Obj -> Obj) = MP.try vertexLine <|> MP.try faceLine <|> skipLine
  in fmap (foldr ($) (Obj [] [])) (MP.many (line <* optional C.eol) <* MP.eof)

objVerts :: Obj -> [GL.GLfloat] = flatten . vertices
objIndices :: Obj -> [GL.GLushort] = fmap fromIntegral . foldr (\(a,b,c) -> ([a,b,c]++)) [] . faces

normals :: Obj -> [Vec 3]
normals obj =
  let verts = position <$> vertices obj
      vertsArr  = Data.Array.listArray (0, length verts - 1) verts
      base      = Data.Array.listArray (0, length verts - 1) (repeat 0)
      perFace (a,b,c) =
        let va = vertsArr Data.Array.! a
            vb = vertsArr Data.Array.! b
            vc = vertsArr Data.Array.! c
            fn = (vb - va) `cross` (vc - va)
        in [(a,fn),(b,fn),(c,fn)]
      updates  = concatMap perFace (faces obj)
      summed   = Data.Array.accum (+) base updates
  in map normalize (Data.Array.elems summed)

normalModel :: Obj -> [Vertex]
normalModel obj = do
  (normal,vert) <- zip (normals obj) (vertices obj)
  [ Vertex { position = position vert, color = v3 1 1 1 }
    , Vertex { position = position vert + 0.3 * normal, color = v3 1 1 1 }
    ]

objBufferSize obj = bufferSize (objVerts obj) + bufferSize (objIndices obj)

cubeVerts :: [GL.GLfloat]
cubeVerts = objVerts cubeObj
cubeIndices :: [GL.GLushort]
cubeIndices = objIndices cubeObj

pyramidVerts :: [GL.GLfloat]
pyramidVerts = objVerts pyramidObj
pyramidIndices :: [GL.GLushort]
pyramidIndices = objIndices pyramidObj

cubeObj = Obj
  { vertices =
    [ Vertex {position =  v3 (-1) ( 1.0) ( 1.0), color = v3 1 0.0 0.0}
    , Vertex {position =  v3 ( 1) ( 1.0) ( 1.0), color = v3 0 1.0 0.0}
    , Vertex {position =  v3 ( 1) ( 1.0) (-1.0), color = v3 0 0.0 1.0}
    , Vertex {position =  v3 (-1) ( 1.0) (-1.0), color = v3 1 1.0 1.0}
    , Vertex {position =  v3 (-1) ( 1.0) (-1.0), color = v3 1 0.0 1.0}
    , Vertex {position =  v3 ( 1) ( 1.0) (-1.0), color = v3 0 0.5 0.2}
    , Vertex {position =  v3 ( 1) (-1.0) (-1.0), color = v3 0 0.6 0.4}
    , Vertex {position =  v3 (-1) (-1.0) (-1.0), color = v3 0 1.0 0.5}
    , Vertex {position =  v3 ( 1) ( 1.0) (-1.0), color = v3 0 0.5 0.2}
    , Vertex {position =  v3 ( 1) ( 1.0) ( 1.0), color = v3 0 0.3 0.7}
    , Vertex {position =  v3 ( 1) (-1.0) ( 1.0), color = v3 0 0.7 1.0}
    , Vertex {position =  v3 ( 1) (-1.0) (-1.0), color = v3 0 0.7 0.5}
    , Vertex {position =  v3 (-1) ( 1.0) ( 1.0), color = v3 0 0.8 0.2}
    , Vertex {position =  v3 (-1) ( 1.0) (-1.0), color = v3 0 0.7 0.3}
    , Vertex {position =  v3 (-1) (-1.0) (-1.0), color = v3 0 0.7 0.7}
    , Vertex {position =  v3 (-1) (-1.0) ( 1.0), color = v3 0 0.5 1.0}
    , Vertex {position =  v3 ( 1) ( 1.0) ( 1.0), color = v3 0 1.0 0.7}
    , Vertex {position =  v3 (-1) ( 1.0) ( 1.0), color = v3 0 0.4 0.8}
    , Vertex {position =  v3 (-1) (-1.0) ( 1.0), color = v3 0 0.8 0.7}
    , Vertex {position =  v3 ( 1) (-1.0) ( 1.0), color = v3 0 0.7 1.0}
    , Vertex {position =  v3 ( 1) (-1.0) (-1.0), color = v3 0 0.3 0.7}
    , Vertex {position =  v3 (-1) (-1.0) (-1.0), color = v3 0 0.9 0.5}
    , Vertex {position =  v3 (-1) (-1.0) ( 1.0), color = v3 0 0.8 0.5}
    , Vertex {position =  v3 ( 1) (-1.0) ( 1.0), color = v3 0 1.0 0.2}
    ]
  , faces =
      [ (0,   1,  2), ( 0,  2,  3) -- Top
      , (4,   5,  6), ( 4,  6,  7) -- Front
      , (8,   9, 10), ( 8, 10, 11) -- Right
      , (12, 13, 14), (12, 14, 15) -- Left
      , (16, 17, 18), (16, 18, 19) -- Back
      , (20, 22, 21), (20, 23, 22) -- Bottom
      ]
  }

cubeNormalIndices :: [GL.GLushort]
cubeNormalIndices = [0..fromIntegral (length cubeNormalVerts')-1]
cubeNormalVerts :: [GL.GLfloat]
cubeNormalVerts = flatten cubeNormalVerts'  
cubeNormalVerts' :: [Vertex]
cubeNormalVerts' = normalModel cubeObj  

pyramidObj = Obj
  { vertices = 
    [ Vertex {position = v3 1 1 (-1), color = v3 1 0 0}
    , Vertex {position = v3 1 1 (-1), color = v3 1 0 0}
    , Vertex {position = v3 1 1 (-1), color = v3 1 0 0}
    , Vertex {position = v3 (-1) 1 1, color = v3 0 1 0}
    , Vertex {position = v3 (-1) 1 1, color = v3 0 1 0}
    , Vertex {position = v3 (-1) 1 1, color = v3 0 1 0}
    , Vertex {position = v3 (-1) (-1) (-1), color = v3 0 0 1}
    , Vertex {position = v3 (-1) (-1) (-1), color = v3 0 0 1}
    , Vertex {position = v3 (-1) (-1) (-1), color = v3 0 0 1}
    , Vertex {position = v3 1 (-1) 1, color = v3 0.7 0.6 0.6}
    , Vertex {position = v3 1 (-1) 1, color = v3 0.7 0.6 0.6}
    , Vertex {position = v3 1 (-1) 1, color = v3 0.7 0.6 0.6}
    ]
  , faces =
    [ (0,6,3)
    , (1,4,9)
    , (10,5,7)
    , (11,8,2)
    ]
  }

pyramidNormalIndices :: [GL.GLushort]
pyramidNormalIndices = [0..fromIntegral (length pyramidNormalVerts')-1]
pyramidNormalVerts :: [GL.GLfloat]
pyramidNormalVerts = flatten pyramidNormalVerts'  
pyramidNormalVerts' :: [Vertex]
pyramidNormalVerts' = normalModel pyramidObj  

squareIndices tl tr bl br = [(tl,bl,tr),(br,tr,bl)]

planeHeight=20
planeWidth=20

seed = 10

planeVerts :: [GL.GLfloat]
planeVerts = objVerts planeObj

planeIndices :: [GL.GLushort]
planeIndices = objIndices planeObj

planeObj = Obj
  { vertices = do
    i <- [0..planeHeight*planeHeight-1]
    let x = fromIntegral (i `mod` planeWidth)
    let y = fromIntegral (i `div` planeHeight)
    let intPairInject y = let t n = if n<0 then -2*n-1 else 2*n; a=t i; b=t (seed+y) in (a+b)*(a+b+1)`div`2+b
    let randColor idx = (fst (System.Random.uniformR (0,1) (System.Random.mkStdGen (intPairInject idx))))
    pure (Vertex
        { position = v3 (x - fromIntegral planeWidth / 2) 0 (y - fromIntegral planeHeight / 2)
        , color = v3 (randColor 0) (randColor 1) (randColor 2)
        })
  , faces = do
      i <- [0..planeHeight-2]
      j <- [0..planeWidth-2]
      squareIndices (i*planeWidth+j) (i*planeWidth+j+1) ((i+1)*planeWidth+j) ((i+1)*planeWidth+j+1)
  }

planeNormalIndices :: [GL.GLushort]
planeNormalIndices = [0..fromIntegral (length planeNormalVerts')-1]
planeNormalVerts :: [GL.GLfloat]
planeNormalVerts = flatten planeNormalVerts'  
planeNormalVerts' :: [Vertex]
planeNormalVerts' = normalModel planeObj  


--- MAIN GRAPHICS PROGRAM ------------------------------------------------------

size :: forall a.Foreign.Storable a => Int
size = Foreign.sizeOf (undefined :: a)

bufferSize :: forall a n.(Foreign.Storable a, Num n) => [a] -> n
bufferSize = fromIntegral . (*) (size @a) . length

data AppState = AppState
  { timeToQuit :: Bool
  , windowWidth :: Int
  , windowHeight :: Int
  , viewDirection :: Vec 4
  , cameraPitch :: Float
  , cameraYaw :: Float
  , cameraPosition :: Vec 4
  }

affineTo3d :: Vec 4 -> Vec 3
affineTo3d v = v . weaken

moveSpeed = 0.1

initialAppState = AppState
        { timeToQuit = False
        , windowWidth = 800
        , windowHeight = 600
        , viewDirection = v4 0 0 (-1) 1
        , cameraYaw = 0
        , cameraPitch = 0
        , cameraPosition = v4 0 0 0 1
        }

handleEvent event appState = case SDL.eventPayload event of
  SDL.WindowResizedEvent e -> 
    let SDL.V2 w h = SDL.windowResizedEventSize e
    in appState { windowWidth = fromIntegral w, windowHeight = fromIntegral h}
  SDL.KeyboardEvent e -> case SDL.keysymKeycode (SDL.keyboardEventKeysym e) of
    SDL.KeycodeQ -> appState { timeToQuit = timeToQuit appState || (SDL.keyboardEventKeyMotion e==SDL.Pressed)}
    _ -> appState
  SDL.MouseMotionEvent mme ->
    let SDL.V2 x y = SDL.mouseMotionEventRelMotion mme
        limit = 89
        pitchUpdateProposal = - fromIntegral y / 3
        pitchUpdate =
          if cameraPitch appState + pitchUpdateProposal > limit
          then min 0 pitchUpdateProposal
          else if cameraPitch appState - pitchUpdateProposal < -limit
          then max 0 pitchUpdateProposal
          else pitchUpdateProposal
        yawUpdate = - fromIntegral x / 3
    in appState
        { viewDirection = memo
            ( rotation pitchUpdate (cross (viewDirection appState . weaken) (v3 0 1 0))
            ( rotation yawUpdate (v3 0 1 0) (viewDirection appState)))
        , cameraYaw = cameraYaw appState + yawUpdate
        , cameraPitch = cameraPitch appState + pitchUpdate
        }
  _ ->
    appState

moveCamera direction appState = appState { cameraPosition = memo (\k ->
  case strengthen k of
    Just k -> (cameraPosition appState . weaken) k + moveSpeed * (normalize (direction . weaken)) k
    Nothing -> 1)}

moveForward appState =
   moveCamera (\k -> if k == y then 0 else viewDirection appState k) appState
moveLeft appState =
   moveCamera (\k -> - maybe 0 (cross (viewDirection appState . weaken) (v3 0 1 0)) (strengthen k)) appState
moveBackward appState =
   moveCamera (\k -> if k == y then 0 else - viewDirection appState k) appState
moveRight appState =
   moveCamera (\k -> maybe 0 (cross (viewDirection appState . weaken) (v3 0 1 0)) (strengthen k)) appState  
moveUp =
   moveCamera (v4 0 1 0 1) -- alt for vector literals: ijkl -> v4 0 1 0 1 = j + l
moveDown =
   moveCamera (v4 0 (-1) 0 1) -- or v4 0 (-1) 0 1 = l - j

data ObjMetadata = ObjMetadata { vertexArrayID :: GL.GLuint, indexCount :: GL.GLsizei, indexOffset :: GL.GLintptr }

main :: IO ()
main = do
  SDL.initializeAll
  window <- SDL.createWindow "App" (SDL.defaultWindow {SDL.windowGraphicsContext=SDL.OpenGLContext SDL.defaultOpenGL})
  SDL.glCreateContext window
  SDL.Raw.Event.setRelativeMouseMode True

  --- LOAD MODELS --------------------------------------------------------------

  teapotObj <- do
    let filename = "resources/teapot.obj"
    src <- readFile filename
    case MP.parse pObj filename src of
      Left err -> print err >> System.Exit.exitFailure
      Right teapot -> pure teapot

  zigzagoon <- do
    let filename = "resources/Zigzagoon/Zigzagoon.SMD"
    src <- readFile filename
    case MP.parse pSMD filename src of
      Left err -> print err >> System.Exit.exitFailure
      Right model -> pure model


  tex <- Codec.Picture.readImage "resources/Zigzagoon/images/pm0263_00_Body1.png"
  textureID <- case tex of
    Left err -> print err >> System.Exit.exitFailure
    Right pic -> do
      let img = Codec.Picture.convertRGBA8 pic
      print (Codec.Picture.imageWidth img)
      print (Codec.Picture.imageHeight img)
      
      textureID <- Foreign.alloca $ \textureIDPtr -> do
          GL.glGenTextures 1 textureIDPtr
          Foreign.peek textureIDPtr
      GL.glBindTexture GL.GL_TEXTURE_2D textureID
      -- opengl textures start at the bottom left so gotta flip
      let flippedY = Codec.Picture.generateImage
            (\x y -> Codec.Picture.pixelAt img x (Codec.Picture.imageHeight img - 1 - y))
            (Codec.Picture.imageWidth img) (Codec.Picture.imageHeight img)
      Data.Vector.Storable.unsafeWith
         (Codec.Picture.imageData flippedY)
         (GL.glTexImage2D
            GL.GL_TEXTURE_2D
            0
            (fromIntegral GL.GL_SRGB8_ALPHA8)
            (fromIntegral $ Codec.Picture.imageWidth img)
            (fromIntegral $ Codec.Picture.imageHeight img)
            0
            GL.GL_RGBA
            GL.GL_UNSIGNED_BYTE
            . Foreign.castPtr)

      GL.glPixelStorei GL.GL_UNPACK_ALIGNMENT 1

      GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_MIN_FILTER (fromIntegral GL.GL_LINEAR_MIPMAP_LINEAR)
      GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_MAG_FILTER (fromIntegral GL.GL_LINEAR)
      GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_WRAP_S (fromIntegral GL.GL_REPEAT)
      GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_WRAP_T (fromIntegral GL.GL_REPEAT)

      GL.glGenerateMipmap GL.GL_TEXTURE_2D

      pure textureID

  -- need to generate a map: material -> (texture,indices)


  --- INITIALIZE OPENGL --------------------------------------------------------

  GL.glEnable GL.GL_DEPTH_TEST
  GL.glEnable GL.GL_CULL_FACE
  -- GL.glPolygonMode GL.GL_FRONT_AND_BACK GL.GL_LINE
   

  --- CONFIGURE SHADERS --------------------------------------------------------

  let initShader vertexShaderCode fragmentShaderCode = do
        programID <- GL.glCreateProgram

        vertexShaderID <- GL.glCreateShader GL.GL_VERTEX_SHADER
        Foreign.C.String.withCString vertexShaderCode (\cstr -> Foreign.with cstr $ \cstrPtr ->
          GL.glShaderSource vertexShaderID 1 cstrPtr Foreign.nullPtr)
        GL.glCompileShader vertexShaderID
        Foreign.alloca $ \compileStatusPtr -> do
          GL.glGetShaderiv vertexShaderID GL.GL_COMPILE_STATUS compileStatusPtr
          compileStatus <- Foreign.peek compileStatusPtr
          when (compileStatus /= 1) $ do
            Foreign.alloca $ \infoLogLengthPtr -> do
              GL.glGetShaderiv vertexShaderID GL.GL_INFO_LOG_LENGTH infoLogLengthPtr
              infoLogLength <- Foreign.peek infoLogLengthPtr
              Foreign.allocaArray (fromIntegral infoLogLength) $ \logPtr -> do
                GL.glGetShaderInfoLog vertexShaderID infoLogLength Foreign.nullPtr logPtr
                errorMessage <- Foreign.C.String.peekCString logPtr
                putStrLn errorMessage
                System.Exit.exitFailure
        GL.glAttachShader programID vertexShaderID
        GL.glDeleteShader vertexShaderID

        fragmentShaderID <- GL.glCreateShader GL.GL_FRAGMENT_SHADER
        Foreign.C.String.withCString fragmentShaderCode (\cstr -> Foreign.with cstr $ \cstrPtr ->
          GL.glShaderSource fragmentShaderID 1 cstrPtr Foreign.nullPtr)
        GL.glCompileShader fragmentShaderID
        Foreign.alloca $ \compileStatusPtr -> do
          GL.glGetShaderiv fragmentShaderID GL.GL_COMPILE_STATUS compileStatusPtr
          compileStatus <- Foreign.peek compileStatusPtr
          when (compileStatus /= 1) $ do
            Foreign.alloca $ \infoLogLengthPtr -> do
              GL.glGetShaderiv fragmentShaderID GL.GL_INFO_LOG_LENGTH infoLogLengthPtr
              infoLogLength <- Foreign.peek infoLogLengthPtr
              Foreign.allocaArray (fromIntegral infoLogLength) $ \logPtr -> do
                GL.glGetShaderInfoLog fragmentShaderID infoLogLength Foreign.nullPtr logPtr
                errorMessage <- Foreign.C.String.peekCString logPtr
                putStrLn errorMessage
                System.Exit.exitFailure
        GL.glAttachShader programID fragmentShaderID
        GL.glDeleteShader fragmentShaderID

        GL.glLinkProgram programID
        Foreign.alloca $ \linkStatusPtr -> do
          GL.glGetProgramiv programID GL.GL_LINK_STATUS linkStatusPtr
          linkStatus <- Foreign.peek linkStatusPtr
          when (linkStatus /= 1) $ do
            Foreign.alloca $ \infoLogLengthPtr -> do
              GL.glGetProgramiv programID GL.GL_INFO_LOG_LENGTH infoLogLengthPtr
              infoLogLength <- Foreign.peek infoLogLengthPtr
              Foreign.allocaArray (fromIntegral infoLogLength) $ \logPtr -> do
                GL.glGetProgramInfoLog programID infoLogLength Foreign.nullPtr logPtr
                errorMessage <- Foreign.C.String.peekCString logPtr
                putStrLn errorMessage
                System.Exit.exitFailure

        pure programID


  basicShader <- initShader
        """#version 430\r\n
        uniform mat4 modelToProjectionMatrix;
        uniform mat4 modelToWorldTransformMatrix;
        in layout(location=0) vec4 vertexPositionModelSpace;
        in layout(location=1) vec3 vertexColor;
        in layout(location=2) vec3 normalModelSpace;
        out vec3 normalWorldSpace;
        out vec3 color;
        out vec3 vertexPositionWorldSpace;
        void main() {
          gl_Position = modelToProjectionMatrix * vertexPositionModelSpace;
          color = vertexColor;
          normalWorldSpace = vec3(modelToWorldTransformMatrix * vec4(normalModelSpace,0));
          vertexPositionWorldSpace = vec3(modelToWorldTransformMatrix * vertexPositionModelSpace);
        }
        """

        """#version 430\r\n
        uniform vec3 lightPosition;
        uniform vec3 eyePosition;
        uniform vec4 ambientLight;
        in vec3 normalWorldSpace;
        in vec3 vertexPositionWorldSpace;
        in vec3 color;
        out vec4 fragmentColor;
        void main() {
          vec3 lightVectorWorldSpace = normalize(lightPosition - vertexPositionWorldSpace);
          float brightness = dot(lightVectorWorldSpace, normalize(normalWorldSpace));
          vec4 diffuseLight = vec4(brightness,brightness,brightness,1.0);
          vec3 reflectedLightWorldSpace = reflect(-lightVectorWorldSpace,normalWorldSpace);
          vec3 eyeVectorWorldSpace = normalize(eyePosition - vertexPositionWorldSpace);
          float s = pow(clamp(dot(reflectedLightWorldSpace, eyeVectorWorldSpace),0,1),64);
          vec4 specularLight = vec4(s,s,s,1.0);
          vec4 lighting = clamp(diffuseLight,0.0,1.0) + ambientLight + clamp(specularLight,0,1);
          fragmentColor = lighting * vec4(color,1);
        }
        """

  modelToProjectionMatrixUniform <-
     Foreign.C.String.withCString "modelToProjectionMatrix" (GL.glGetUniformLocation basicShader)
  ambientLightUniform <-
     Foreign.C.String.withCString "ambientLight" (GL.glGetUniformLocation basicShader)
  lightPositionUniform <-
     Foreign.C.String.withCString "lightPosition" (GL.glGetUniformLocation basicShader)
  eyePositionUniform <-
     Foreign.C.String.withCString "eyePosition" (GL.glGetUniformLocation basicShader)
  modelToWorldTransformMatrixUniform <-
     Foreign.C.String.withCString "modelToWorldTransformMatrix" (GL.glGetUniformLocation basicShader)


  textureShader <- initShader 
        """#version 430\r\n
        uniform mat4 modelToProjectionMatrix;
        uniform mat4 modelToWorldTransformMatrix;
        in layout(location=0) vec4 vertexPositionModelSpace;
        in layout(location=1) vec3 vertexColor;
        in layout(location=2) vec3 normalModelSpace;
        in layout(location=3) vec2 vertex_uv;
        out vec3 normalWorldSpace;
        out vec2 fragment_uv;
        out vec3 vertexPositionWorldSpace;
        void main() {
          gl_Position = modelToProjectionMatrix * vertexPositionModelSpace;
          normalWorldSpace = vec3(modelToWorldTransformMatrix * vec4(normalModelSpace,0));
          vertexPositionWorldSpace = vec3(modelToWorldTransformMatrix * vertexPositionModelSpace);
          fragment_uv = vertex_uv;
        }
        """

        """#version 430\r\n
        uniform vec3 lightPosition;
        uniform vec3 eyePosition;
        uniform vec4 ambientLight;
        uniform sampler2D base_texture;
        in vec3 normalWorldSpace;
        in vec3 vertexPositionWorldSpace;
        in vec2 fragment_uv;
        out vec4 fragmentColor;
        void main() {
          vec3 lightVectorWorldSpace = normalize(lightPosition - vertexPositionWorldSpace);
          float brightness = dot(lightVectorWorldSpace, normalize(normalWorldSpace));
          vec4 diffuseLight = vec4(brightness,brightness,brightness,1.0);
          vec3 reflectedLightWorldSpace = reflect(-lightVectorWorldSpace,normalWorldSpace);
          vec3 eyeVectorWorldSpace = normalize(eyePosition - vertexPositionWorldSpace);
          float s = pow(clamp(dot(reflectedLightWorldSpace, eyeVectorWorldSpace),0,1),512);
          vec4 specularLight = vec4(s,s,s,1.0);
          vec4 lighting = clamp(diffuseLight,0.0,1.0) + ambientLight + clamp(specularLight,0,1);
          fragmentColor = lighting * texture(base_texture,fragment_uv);
        }
        """

  texture_modelToProjectionMatrixUniform <-
     Foreign.C.String.withCString "modelToProjectionMatrix" (GL.glGetUniformLocation textureShader)
  texture_ambientLightUniform <-
     Foreign.C.String.withCString "ambientLight" (GL.glGetUniformLocation textureShader)
  texture_lightPositionUniform <-
     Foreign.C.String.withCString "lightPosition" (GL.glGetUniformLocation textureShader)
  texture_eyePositionUniform <-
     Foreign.C.String.withCString "eyePosition" (GL.glGetUniformLocation textureShader)
  texture_modelToWorldTransformMatrixUniform <-
     Foreign.C.String.withCString "modelToWorldTransformMatrix" (GL.glGetUniformLocation textureShader)
  texture_textureUniform <-
     Foreign.C.String.withCString "base_texture" (GL.glGetUniformLocation textureShader)


  --- SEND DATA TO GPU ---------------------------------------------------------

  let initializeObject obj = do
        let verts = objVerts obj
        let indices = objIndices obj
        let vertexNormals = flatten (normals obj)
        let objBufferSize obj = bufferSize verts + bufferSize indices + bufferSize vertexNormals
        let vertsStride = fromIntegral (size @GL.GLfloat) * 6
        let vertsOffset = 0
        let indicesOffset = bufferSize verts
        let normalsOffset = indicesOffset + bufferSize indices
        bufferID <- Foreign.alloca $ \bufferIDPtr -> do
            GL.glGenBuffers 1 bufferIDPtr
            Foreign.peek bufferIDPtr
        GL.glBindBuffer GL.GL_ARRAY_BUFFER bufferID
        GL.glBufferData GL.GL_ARRAY_BUFFER (objBufferSize obj) Foreign.nullPtr GL.GL_STATIC_DRAW
        Foreign.withArray verts         (GL.glBufferSubData GL.GL_ARRAY_BUFFER vertsOffset   (bufferSize verts))
        Foreign.withArray indices       (GL.glBufferSubData GL.GL_ARRAY_BUFFER indicesOffset (bufferSize indices))
        Foreign.withArray vertexNormals (GL.glBufferSubData GL.GL_ARRAY_BUFFER normalsOffset (bufferSize vertexNormals))
        vertexArrayObjectID <- Foreign.alloca $ \idPtr -> do
            GL.glGenVertexArrays 1 idPtr
            Foreign.peek idPtr
        GL.glBindVertexArray vertexArrayObjectID
        GL.glEnableVertexAttribArray 0
        GL.glEnableVertexAttribArray 1
        GL.glEnableVertexAttribArray 2
        GL.glBindBuffer GL.GL_ARRAY_BUFFER bufferID
        GL.glVertexAttribPointer 0 3 GL.GL_FLOAT GL.GL_FALSE vertsStride (Foreign.plusPtr Foreign.nullPtr (fromIntegral 0))
        GL.glVertexAttribPointer 1 3 GL.GL_FLOAT GL.GL_FALSE vertsStride (Foreign.plusPtr Foreign.nullPtr (fromIntegral 0 + 3*size @GL.GLfloat))
        GL.glVertexAttribPointer 2 3 GL.GL_FLOAT GL.GL_FALSE (fromIntegral (size @GL.GLfloat) * 3) (Foreign.plusPtr Foreign.nullPtr (fromIntegral normalsOffset))
        GL.glBindBuffer GL.GL_ELEMENT_ARRAY_BUFFER bufferID
        pure (ObjMetadata
              { vertexArrayID = vertexArrayObjectID
              , indexOffset = indicesOffset
              , indexCount = fromIntegral (length (objIndices obj))
              })
  
  cubeMetadata <- initializeObject cubeObj
  pyramidMetadata <- initializeObject pyramidObj
  planeMetadata <- initializeObject planeObj
  teapotMetadata <- initializeObject teapotObj


  let initializeZig = do
        let obj = smdToObj zigzagoon
        let verts = objVerts obj
        let indices = objIndices obj
        let normals = flatten (smdNormals zigzagoon)
        let uvs = flatten (smdUVs zigzagoon)
        let objBufferSize obj = bufferSize verts + bufferSize indices + bufferSize normals + bufferSize uvs
        let vertsOffset = 0
        let indicesOffset = bufferSize verts
        let normalsOffset = indicesOffset + bufferSize indices
        let uvsOffset = normalsOffset + bufferSize normals
        bufferID <- Foreign.alloca $ \bufferIDPtr -> do
            GL.glGenBuffers 1 bufferIDPtr
            Foreign.peek bufferIDPtr
        GL.glBindBuffer GL.GL_ARRAY_BUFFER bufferID
        GL.glBufferData GL.GL_ARRAY_BUFFER (objBufferSize obj) Foreign.nullPtr GL.GL_STATIC_DRAW
        Foreign.withArray verts   (GL.glBufferSubData GL.GL_ARRAY_BUFFER vertsOffset   (bufferSize verts))
        Foreign.withArray indices (GL.glBufferSubData GL.GL_ARRAY_BUFFER indicesOffset (bufferSize indices))
        Foreign.withArray normals (GL.glBufferSubData GL.GL_ARRAY_BUFFER normalsOffset (bufferSize normals))
        Foreign.withArray uvs     (GL.glBufferSubData GL.GL_ARRAY_BUFFER uvsOffset     (bufferSize uvs))
        vertexArrayObjectID <- Foreign.alloca $ \idPtr -> do
            GL.glGenVertexArrays 1 idPtr
            Foreign.peek idPtr
        GL.glBindVertexArray vertexArrayObjectID
        GL.glEnableVertexAttribArray 0
        GL.glEnableVertexAttribArray 1
        GL.glEnableVertexAttribArray 2
        GL.glEnableVertexAttribArray 3
        GL.glBindBuffer GL.GL_ARRAY_BUFFER bufferID
        GL.glVertexAttribPointer 0 3 GL.GL_FLOAT GL.GL_FALSE (fromIntegral (size @GL.GLfloat) * 6) (Foreign.plusPtr Foreign.nullPtr (fromIntegral 0))
        GL.glVertexAttribPointer 1 3 GL.GL_FLOAT GL.GL_FALSE (fromIntegral (size @GL.GLfloat) * 6) (Foreign.plusPtr Foreign.nullPtr (fromIntegral 0 + 3*size @GL.GLfloat))
        GL.glVertexAttribPointer 2 3 GL.GL_FLOAT GL.GL_FALSE (fromIntegral (size @GL.GLfloat) * 3) (Foreign.plusPtr Foreign.nullPtr (fromIntegral normalsOffset))
        GL.glVertexAttribPointer 3 2 GL.GL_FLOAT GL.GL_FALSE (fromIntegral (size @GL.GLfloat) * 2) (Foreign.plusPtr Foreign.nullPtr (fromIntegral uvsOffset))
        GL.glBindBuffer GL.GL_ELEMENT_ARRAY_BUFFER bufferID
        pure (ObjMetadata
              { vertexArrayID = vertexArrayObjectID
              , indexOffset = indicesOffset
              , indexCount = fromIntegral (length (objIndices obj))
              })
  
  zigMetadata <- initializeZig


  --- MAIN LOOP ----------------------------------------------------------------
  
  let loop prevAppState = do


        --- HANDLE EVENTS ------------------------------------------------------
        
        events <- SDL.pollEvents
        SDL.pollEvents -- set keystate
        ks <- SDL.getKeyboardState
        let positionTransform =
               (if ks SDL.ScancodeW then moveForward else id)
             . (if ks SDL.ScancodeA then moveLeft else id)
             . (if ks SDL.ScancodeS then moveBackward else id)
             . (if ks SDL.ScancodeD then moveRight else id)
             . (if ks SDL.ScancodeR then moveUp else id)
             . (if ks SDL.ScancodeF then moveDown else id)
        let appState = positionTransform (foldr handleEvent prevAppState events)
        when (windowWidth appState /= windowWidth appState || windowHeight appState /= windowHeight appState)
          (GL.glViewport 0 0 (fromIntegral (windowWidth appState)) (fromIntegral (windowHeight appState)))
        SDL.glSwapWindow window


        --- DRAW ---------------------------------------------------------------
        
        GL.glClear GL.GL_COLOR_BUFFER_BIT
        GL.glClear GL.GL_DEPTH_BUFFER_BIT

        GL.glUseProgram basicShader

        let lightPosition = v3 3 0 2

        Foreign.withArray (flatten (v4 0.1 0.1 0.1 1)) (GL.glUniform4fv ambientLightUniform 1)
        Foreign.withArray (flatten lightPosition) (GL.glUniform3fv lightPositionUniform 1)
        Foreign.withArray (flatten (cameraPosition appState . weaken)) (GL.glUniform3fv eyePositionUniform 1)

        let aspectRatio = fromIntegral (windowWidth appState) / fromIntegral (windowHeight appState)
        let worldToView = worldToViewMatrix (cameraPosition appState . weaken) (viewDirection appState . weaken)
        let projectionMatrix = projection 60 aspectRatio 0.1 50
        let toScreenspace t = (flatten @(Mat 4 4)) (transformToMat (projectionMatrix . worldToView . t))
        let drawTriangulation obj transform = do
              Foreign.withArray
                (flatten (transformToMat transform))
                (GL.glUniformMatrix4fv modelToWorldTransformMatrixUniform 1 0)
              GL.glBindVertexArray (vertexArrayID obj)
              Foreign.withArray
                (toScreenspace transform)
                (GL.glUniformMatrix4fv modelToProjectionMatrixUniform 1 0)
              GL.glDrawElements
                GL.GL_TRIANGLES
                (indexCount obj)
                GL.GL_UNSIGNED_SHORT
                (Foreign.plusPtr Foreign.nullPtr (fromIntegral (indexOffset obj)))


        drawTriangulation cubeMetadata (translate (v3 1 0 (-4)) . rotation 45 (v3 1 1 0))
        drawTriangulation cubeMetadata (translate (v3 (-1) 0 (-4)) . rotation 45 (v3 1 1 1))
        drawTriangulation pyramidMetadata (translate (v3 0 0 (-2)))
        drawTriangulation planeMetadata (translate (v3 0 (-2) (-4)))
        drawTriangulation teapotMetadata (translate (v3 (3) 1 (-8)))

        GL.glUseProgram textureShader
        GL.glActiveTexture GL.GL_TEXTURE0
        GL.glBindTexture GL.GL_TEXTURE_2D textureID
        GL.glUniform1i texture_textureUniform 0
        drawTriangulation zigMetadata (translate (v3 (-2) (-1) (-1)) . rotation 90 (v3 (-1) 0 0))

        unless (timeToQuit appState) (loop appState)

  loop initialAppState


  --- CLEANUP ------------------------------------------------------------------

  GL.glUseProgram 0
  GL.glDeleteProgram basicShader
  SDL.destroyWindow window
