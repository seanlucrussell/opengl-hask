{-# LANGUAGE OverloadedStrings,LinearTypes,ScopedTypeVariables,MultilineStrings,DataKinds,TypeOperators,FlexibleInstances,UndecidableInstances,TypeApplications,AllowAmbiguousTypes,GADTs,TypeFamilies,RankNTypes #-}
module Main where
import           Control.Applicative
import           Control.Monad (unless,when)
import           Data.Finite
import           GHC.TypeNats
import qualified System.Process
import qualified Codec.Picture
import qualified Control.Monad (void)
import qualified Data.Array
import qualified Data.Char
import qualified Data.List
import qualified Data.Map
import qualified Data.Map.Internal
import qualified Data.Set
import qualified Data.Vector.Storable
import qualified Foreign
import qualified Foreign.C.String
import qualified Graphics.GL.Core46         as GL
import qualified SDL
import qualified SDL.Raw.Event
import qualified SDL.Raw.Video
import qualified System.Random
import qualified Text.Megaparsec            as MP
import qualified Text.Megaparsec.Char       as C
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Network.HTTP.Simple
import qualified Data.Aeson
import qualified Control.Concurrent.Async
import qualified Data.String


--- LINEAR ALGEBRA LIBRARY -----------------------------------------------------

type Array n s = Finite n -> s
type Vec n = Array n Float
type Mat m n = Finite m -> Finite n -> Float

x :: (KnownNat n, 1 <= n) => Finite n; x = 0
y :: (KnownNat n, 2 <= n) => Finite n; y = 1
z :: (KnownNat n, 3 <= n) => Finite n; z = 2
w :: (KnownNat n, 4 <= n) => Finite n; w = 3

r :: (KnownNat n, 1 <= n) => Finite n; r = 0
g :: (KnownNat n, 2 <= n) => Finite n; g = 1
b :: (KnownNat n, 3 <= n) => Finite n; b = 2

v2 :: Float -> Float -> Vec 2;                   v2 = a2
v3 :: Float -> Float -> Float -> Vec 3;          v3 = a3
v4 :: Float -> Float -> Float -> Float -> Vec 4; v4 = a4

a2 :: a -> a -> Array 2 a;           a2 x y i = case i of {0->x;1->y}
a3 :: a -> a -> a -> Array 3 a;      a3 x y z i = case i of {0->x;1->y;2->z}
a4 :: a -> a -> a -> a -> Array 4 a; a4 x y z w i = case i of {0->x;1->y;2->z;3->w}

instance (KnownNat n,Show a) => Show (Array n a) where show v = "v" ++ show (1+fromIntegral (maxBound :: Finite n)) ++ show (v <$> finites)

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

instance Floating (Vec n) where
  cos = fmap cos
  sin = fmap sin
  pi = pure pi
  exp = fmap exp
  log = fmap log
  asin = fmap asin
  acos = fmap acos
  atan = fmap atan
  sinh = fmap sinh
  cosh = fmap cosh
  asinh = fmap asinh
  acosh = fmap acosh
  atanh = fmap atanh

translate :: Vec 3 -> Vec 4 -> Vec 4
translate d v = pure (v w) * extend d 0 + v

projection :: Float -> Float -> Float -> Float -> Vec 4 -> Vec 4
projection fov aspect near far v =
  v4 (v x / (aspect * tan (pi*fov/360)))
     (v y /           tan (pi*fov/360))
     ((near*v z + far*v z + 2*near*far*v w)/(near-far))
     (-v z)

extend v x = maybe x v . strengthen


-- presumes angles are in degrees
rotateAround :: Vec 3 -> Float -> Vec 4 -> Vec 4
rotateAround axis angle v =
  let
    theta = pure (toRadians angle)
    u     = normalize axis
    v'    = v . weaken
  in extend (cos theta*v' + sin theta*(u `cross` v') + (1-cos theta)*pure (u`dot`v')*u) (v w)

transformToMat :: (KnownNat m, KnownNat n) => (Vec n -> Vec m) -> Mat m n
transformToMat f row column = f (\k -> if k == column then 1 else 0) row

contract :: KnownNat n => Vec n -> Float; contract v = sum (v <$> finites)
dot v1 v2 = contract (v1 * v2)
magnitude v = sqrt (v `dot` v)
normalize :: KnownNat n => Vec n -> Vec n; normalize v = v / pure (magnitude v)

identity i j = if i == j then 1 else 0

cross :: Vec 3 -> Vec 3 -> Vec 3
cross a b i = a (i+1) * b (i+2) - b (i+1) * a (i+2) 

lookAt :: Vec 3 -> Vec 3 -> Vec 3 -> Vec 4 -> Vec 4
lookAt eye center up v =
  let
    v' = v . weaken
    f = normalize (eye - center)
    r = normalize (cross up f)
    u = normalize (cross f r)
  in v4 (dot r v' - dot r eye * v w)
        (dot u v' - dot u eye * v w)
        (dot f v' - dot f eye * v w)
        (                       v w)

worldToViewMatrix :: Vec 3 -> Vec 3 -> Vec 4 -> Vec 4
worldToViewMatrix position viewDirection = lookAt position (position + viewDirection) (v3 0 1 0)

memo :: forall a. KnownNat a => Vec a -> Vec a
memo v = (Data.Array.!) (Data.Array.listArray (0, fromIntegral (maxBound :: Finite a)) (map v finites)) . fromIntegral

project :: KnownNat n => Vec n -> Vec n -> Vec n
project a b = pure ((a `dot` b) / magnitude b) * b 


north :: Vec 3 = v3 1 0 0
south :: Vec 3 = v3 (-1) 0 0
east :: Vec 3 = v3 0 0 1
west :: Vec 3 = v3 0 0 (-1)
up :: Vec 3 = v3 0 1 0
down :: Vec 3 = v3 0 (-1) 0

--- OBJ PARSER -----------------------------------------------------------------

type Parser = MP.Parsec () String
type Face = (Int, Int, Int)
data OBJ_Vertex = OBJ_Vertex {position :: Vec 3, color :: Vec 3} deriving Show
data Obj = Obj { vertices :: [OBJ_Vertex], faces :: [Face] } deriving Show

pObj :: Parser Obj
pObj =
  let
    hspace1 = Control.Monad.void $ MP.some (MP.satisfy Data.Char.isSpace) :: Parser ()
    float = L.signed (pure ()) L.float :: Parser Float
    vertexLine :: Parser (Obj -> Obj) = do
      C.char 'v'
      v <- v3 <$> (hspace1 >> float) <*> (hspace1 >> float) <*> (hspace1 >> float)
      return (\o -> o { vertices = OBJ_Vertex v (v3 1 1 1):vertices o })
    faceLine :: Parser (Obj -> Obj)= do
      C.char 'f'
      a <- hspace1 >> L.decimal
      b <- hspace1 >> L.decimal
      c <- hspace1 >> L.decimal
      return (\o -> o { faces = (a-1, b-1, c-1) : faces o })
    skipLine = MP.manyTill MP.anySingle (MP.lookAhead (C.eol >> pure () <|> MP.eof)) >> pure id
    line = MP.try vertexLine <|> MP.try faceLine <|> skipLine
  in fmap (foldr ($) (Obj [] [])) (MP.many (line <* optional C.eol) <* MP.eof)

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

normalModel :: Obj -> [OBJ_Vertex]
normalModel obj = do
  (normal,vert) <- zip (normals obj) (vertices obj)
  [  OBJ_Vertex { position = position vert, color = v3 1 1 1 }
   , OBJ_Vertex { position = position vert + 0.3 * normal, color = v3 1 1 1 }
   ]

cubeObj = Obj
  { vertices =
    [ OBJ_Vertex {position = v3 (-1)   1    1 , color = v3 1 0.0 0.0}
    , OBJ_Vertex {position = v3   1    1    1 , color = v3 0 1.0 0.0}
    , OBJ_Vertex {position = v3   1    1  (-1), color = v3 0 0.0 1.0}
    , OBJ_Vertex {position = v3 (-1)   1  (-1), color = v3 1 1.0 1.0}
    , OBJ_Vertex {position = v3 (-1)   1  (-1), color = v3 1 0.0 1.0}
    , OBJ_Vertex {position = v3   1    1  (-1), color = v3 0 0.5 0.2}
    , OBJ_Vertex {position = v3   1  (-1) (-1), color = v3 0 0.6 0.4}
    , OBJ_Vertex {position = v3 (-1) (-1) (-1), color = v3 0 1.0 0.5}
    , OBJ_Vertex {position = v3   1    1  (-1), color = v3 0 0.5 0.2}
    , OBJ_Vertex {position = v3   1    1    1 , color = v3 0 0.3 0.7}
    , OBJ_Vertex {position = v3   1  (-1)   1 , color = v3 0 0.7 1.0}
    , OBJ_Vertex {position = v3   1  (-1) (-1), color = v3 0 0.7 0.5}
    , OBJ_Vertex {position = v3 (-1)   1    1 , color = v3 0 0.8 0.2}
    , OBJ_Vertex {position = v3 (-1)   1  (-1), color = v3 0 0.7 0.3}
    , OBJ_Vertex {position = v3 (-1) (-1) (-1), color = v3 0 0.7 0.7}
    , OBJ_Vertex {position = v3 (-1) (-1)   1 , color = v3 0 0.5 1.0}
    , OBJ_Vertex {position = v3   1    1    1 , color = v3 0 1.0 0.7}
    , OBJ_Vertex {position = v3 (-1)   1    1 , color = v3 0 0.4 0.8}
    , OBJ_Vertex {position = v3 (-1) (-1)   1 , color = v3 0 0.8 0.7}
    , OBJ_Vertex {position = v3   1  (-1)   1 , color = v3 0 0.7 1.0}
    , OBJ_Vertex {position = v3   1  (-1) (-1), color = v3 0 0.3 0.7}
    , OBJ_Vertex {position = v3 (-1) (-1) (-1), color = v3 0 0.9 0.5}
    , OBJ_Vertex {position = v3 (-1) (-1)   1 , color = v3 0 0.8 0.5}
    , OBJ_Vertex {position = v3   1  (-1)   1 , color = v3 0 1.0 0.2}
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

pyramidObj = Obj
  { vertices = 
    [ OBJ_Vertex {position = v3 1 1 (-1), color = v3 1 0 0}
    , OBJ_Vertex {position = v3 1 1 (-1), color = v3 1 0 0}
    , OBJ_Vertex {position = v3 1 1 (-1), color = v3 1 0 0}
    , OBJ_Vertex {position = v3 (-1) 1 1, color = v3 0 1 0}
    , OBJ_Vertex {position = v3 (-1) 1 1, color = v3 0 1 0}
    , OBJ_Vertex {position = v3 (-1) 1 1, color = v3 0 1 0}
    , OBJ_Vertex {position = v3 (-1) (-1) (-1), color = v3 0 0 1}
    , OBJ_Vertex {position = v3 (-1) (-1) (-1), color = v3 0 0 1}
    , OBJ_Vertex {position = v3 (-1) (-1) (-1), color = v3 0 0 1}
    , OBJ_Vertex {position = v3 1 (-1) 1, color = v3 0.7 0.6 0.6}
    , OBJ_Vertex {position = v3 1 (-1) 1, color = v3 0.7 0.6 0.6}
    , OBJ_Vertex {position = v3 1 (-1) 1, color = v3 0.7 0.6 0.6}
    ]
  , faces =
    [ (0,6,3)
    , (1,4,9)
    , (10,5,7)
    , (11,8,2)
    ]
  }

planeObj =
  let planeHeight=20
      planeWidth=20
      squareIndices tl tr bl br = [(tl,bl,tr),(br,tr,bl)]
  in Obj
  { vertices = do
    i <- [0..planeHeight*planeHeight-1]
    let seed = 10
    let x = fromIntegral (i `mod` planeWidth)
    let y = fromIntegral (i `div` planeHeight)
    let intPairInject y = let t n = if n<0 then -2*n-1 else 2*n; a=t i; b=t (seed+y) in (a+b)*(a+b+1)`div`2+b
    let randColor idx = fst (System.Random.uniformR (0,1) (System.Random.mkStdGen (intPairInject idx)))
    pure (OBJ_Vertex
        { position = v3 (x - fromIntegral planeWidth / 2) 0 (y - fromIntegral planeHeight / 2)
        , color = v3 (randColor 0) (randColor 1) (randColor 2)
        })
  , faces = do
      i <- [0..planeHeight-2]
      j <- [0..planeWidth-2]
      squareIndices (i*planeWidth+j) (i*planeWidth+j+1) ((i+1)*planeWidth+j) ((i+1)*planeWidth+j+1)
  }

icosahedron =
 let phi = (1 + sqrt 5) / 2
     a = 1
     b = 1/phi
 in Obj
  { vertices =
    [ OBJ_Vertex {position = v3 0 b (-a), color = v3 1 1 1}
    , OBJ_Vertex {position = v3 b a 0, color = v3 1 1 1}
    , OBJ_Vertex {position = v3 (-b) a 0, color = v3 1 1 1}
    , OBJ_Vertex {position = v3 0 b a, color = v3 1 1 1}
    , OBJ_Vertex {position = v3 0 (-b) a, color = v3 1 1 1}
    , OBJ_Vertex {position = v3 (-a) 0 b, color = v3 1 1 1}
    , OBJ_Vertex {position = v3 0 (-b) (-a), color = v3 1 1 1}
    , OBJ_Vertex {position = v3 a 0 (-b), color = v3 1 1 1}
    , OBJ_Vertex {position = v3 a 0 b, color = v3 1 1 1}
    , OBJ_Vertex {position = v3 (-a) 0 (-b), color = v3 1 1 1}
    , OBJ_Vertex {position = v3 b (-a) 0, color = v3 1 1 1}
    , OBJ_Vertex {position = v3 (-b) (-a) 0, color = v3 1 1 1}
    ]
  , faces = fmap (\(a,b,c) -> (a-1,b-1,c-1))
    [ (3, 2, 1)
    , (2, 3, 4)
    , (6, 5, 4)
    , (5, 9, 4)
    , (8, 7, 1)
    , (7, 10, 1)
    , (12, 11, 5)
    , (11, 12, 7)
    , (10, 6, 3)
    , (6, 10, 12)
    , (9, 8, 2)
    , (8, 9, 11)
    , (3, 6, 4)
    , (9, 2, 4)
    , (10, 3, 1)
    , (2, 8, 1)
    , (12, 10, 7)
    , (8, 11, 7)
    , (6, 12, 5)
    , (11, 9, 5)
    ]
  }

--- SMD PARSER -----------------------------------------------------------------

data SMDNode = SMDNode Int String Int deriving (Show)
data SMDBoneFrame = SMDBoneFrame { boneId :: Int, pos :: Vec 3, rot :: Vec 3 } deriving (Show)
data SMDSkeletonFrame = SMDSkeletonFrame { frameNum :: Int, bones :: [SMDBoneFrame] } deriving (Show)
data SMDVertex = SMDVertex { vParent :: Int, vPos :: Vec 3, vNormal :: Vec 3, vUV :: Vec 2, vWeights :: [(Int, Float)] } deriving (Show)
data SMDTriangle = SMDTriangle { material :: String, verts :: (SMDVertex,SMDVertex,SMDVertex) } deriving (Show)
data SMD = SMD { nodes :: [SMDNode], skeleton :: [SMDSkeletonFrame], triangles :: [SMDTriangle] } deriving (Show)

-- idea: SMD has a base reference pose + you can add animations to it? animation parser modifies existing SMD structure? verifies that the two files are compatible?
-- tbh tho it would be nice to have direct references to the animations themselves

smdscale factor smd =
  let vscale v = v {vPos = factor * vPos v}
      triscale t = let (a,b,c) = verts t in t {verts = (vscale a, vscale b, vscale c)}
      bonescale b = b {pos = factor * pos b}
      framescale f = f {bones = bonescale <$> bones f}
  in smd {triangles = triscale <$> triangles smd, skeleton = framescale <$> skeleton smd}

pSMD :: Parser SMD
pSMD =
  let
    sc            = L.space C.space1 empty empty                                  :: Parser ()
    lexeme        = L.lexeme sc                                                   :: Parser a -> Parser a
    symbol        = L.symbol sc                                                   :: String -> Parser String
    integer       = lexeme (L.signed sc L.decimal)                                :: Parser Int
    float         = lexeme (L.signed sc L.float)                                  :: Parser Float
    stringLiteral = lexeme (C.char '"' *> MP.manyTill L.charLiteral (C.char '"')) :: Parser String
    vec2          = v2 <$> float <*> float
    vec3          = v3 <$> float <*> float <*> float
    weights = optional integer >>= maybe (pure []) (`MP.count` ((,) <$> integer <*> float))
    pVertex = lexeme (SMDVertex <$> integer <*> vec3 <*> vec3 <*> vec2 <*> weights)
  in 
  SMD <$> (symbol "version" >> integer >>
           symbol "nodes" >>
           MP.manyTill
              (lexeme (SMDNode <$> integer <*> stringLiteral <*> integer))
          (symbol "end"))
      <*> (symbol "skeleton" >>
           MP.manyTill
              (symbol "time" >> SMDSkeletonFrame <$>
               integer <*>
               MP.many (lexeme (SMDBoneFrame <$> integer <*> vec3 <*> vec3)))
          (symbol "end"))
      <*> (maybe [] id <$> MP.optional
          (symbol "triangles" >>
           MP.manyTill (lexeme (SMDTriangle <$>
                lexeme (MP.someTill MP.anySingle C.eol) <*>
                ((,,) <$> pVertex <*> pVertex <*> pVertex)))
          (symbol "end"))
        <* MP.eof)


data Rose a = Rose a [Rose a] deriving (Show)

roseEdges :: Rose a -> [(Int,Int)]
roseEdges = 
  let
    f (currentID,edges,idsSoFar) child =
         let (nextID, newEdges) = go currentID child
         in (nextID, edges ++ newEdges, idsSoFar ++ [currentID])
    go currentID (Rose _ children) =
      let (nextID, edges, childIds) = foldl f (currentID + 1, [], []) children
      in (nextID, [(currentID , childID) | childID <- childIds] ++ edges)
  in snd . go 0

flattenRose :: Rose a -> [a]
flattenRose (Rose a rs) = a:concatMap flattenRose rs

smdPoses :: SMD -> [Rose (Int, Vec 4)]
smdPoses smd = let go (Rose (i,t) ts) = Rose (i,t (v4 0 0 0 1)) (go <$> ts) in fmap go (smdPoseTransforms smd <$> skeleton smd)

smdPoseTransforms :: SMD -> SMDSkeletonFrame -> Rose (Int, Vec 4 -> Vec 4)
smdPoseTransforms smd frame =
  let skeletonIndices = filter (\(a,b) -> a /= -1 && b /= -1) (fmap (\(SMDNode start _ end) -> (start,end)) (nodes smd))
      childMap = Data.Map.fromListWith (++) ((\(v,k) -> (k,[v])) <$> skeletonIndices)
      subtree transform n =
         let bone = bones frame !! n
             newTransform =
                 transform
                 . memo
                 . translate (pos bone)
                 . rotateAround (v3 0 0 1) (toDegrees $ rot bone z)
                 . rotateAround (v3 0 1 0) (toDegrees $ rot bone y)
                 . rotateAround (v3 1 0 0) (toDegrees $ rot bone x)
         in Rose (boneId bone, newTransform) (subtree newTransform <$> maybe [] id (Data.Map.lookup n childMap))
  in subtree id 0

-- really we should distinguish a base pose from an animation. i think the right way to do this is ONE parser for all formats, but then have a postprocessing step that does convention checks for references (e.g. only one time 0) and animations (no triangles) and puts things into more structured data types
smdReferencePoseTransform :: SMD -> SMDSkeletonFrame -> Rose (Int, Vec 4 -> Vec 4)
smdReferencePoseTransform smd frame =
  let skeletonIndices = filter (\(a,b) -> a /= -1 && b /= -1) (fmap (\(SMDNode start _ end) -> (start,end)) (nodes smd))
      childMap = Data.Map.fromListWith (++) ((\(v,k) -> (k,[v])) <$> skeletonIndices)
      subtree transform n =
       let bone = bones frame !! n
           newTransform =
             memo
             . rotateAround (v3 1 0 0) (-(toDegrees $ rot bone x))
             . rotateAround (v3 0 1 0) (-(toDegrees $ rot bone y))
             . rotateAround (v3 0 0 1) (-(toDegrees $ rot bone z))
             . translate (-pos bone)
             . transform
       in Rose (boneId bone, newTransform) (subtree newTransform <$> maybe [] id (Data.Map.lookup n childMap))
  in subtree id 0

refposflat :: SMD -> SMDSkeletonFrame -> [Vec 4 -> Vec 4]
refposflat smd frame = fmap snd (Data.List.sortOn fst (flattenRose (smdReferencePoseTransform smd frame)))

animposflat :: SMD -> SMDSkeletonFrame -> [Vec 4 -> Vec 4]
animposflat smd frame = fmap snd (Data.List.sortOn fst (flattenRose (smdPoseTransforms smd frame)))

pose :: SMD -> SMDSkeletonFrame -> SMDSkeletonFrame -> [Vec 4 -> Vec 4]
pose smd ref frame = zipWith (.)  (animposflat frame) (refposflat ref)
  where
    skeletonIndices = filter (\(a,b) -> a /= -1 && b /= -1) (fmap (\(SMDNode start _ end) -> (start,end)) (nodes smd))
    childMap = Data.Map.fromListWith (++) ((\(v,k) -> (k,[v])) <$> skeletonIndices)
    smdPoseTransforms frame =
      let subtree transform n =
             let bone = bones frame !! n
                 newTransform =
                     memo
                     . transform
                     . memo
                     . translate (pos bone)
                     . memo
                     . rotateAround (v3 0 0 1) (toDegrees $ rot bone z)
                     . memo
                     . rotateAround (v3 0 1 0) (toDegrees $ rot bone y)
                     . memo
                     . rotateAround (v3 1 0 0) (toDegrees $ rot bone x)
             in Rose (boneId bone, newTransform) (subtree newTransform <$> maybe [] id (Data.Map.lookup n childMap))
      in subtree id 0
    smdReferencePoseTransform frame =
      let subtree transform n =
           let bone = bones frame !! n
               newTransform =
                 memo
                 . rotateAround (v3 1 0 0) (-(toDegrees $ rot bone x))
                 . memo
                 . rotateAround (v3 0 1 0) (-(toDegrees $ rot bone y))
                 . memo
                 . rotateAround (v3 0 0 1) (-(toDegrees $ rot bone z))
                 . memo
                 . translate (-pos bone)
                 . memo
                 . transform
           in Rose (boneId bone, newTransform) (subtree newTransform <$> maybe [] id (Data.Map.lookup n childMap))
      in subtree id 0
    refposflat frame = fmap snd (Data.List.sortOn fst (flattenRose (smdReferencePoseTransform  frame)))
    animposflat frame = fmap snd (Data.List.sortOn fst (flattenRose (smdPoseTransforms frame)))


--- GL UTILS -------------------------------------------------------------------

class SetUniform u where setUniform :: GL.GLint -> u -> IO ()
instance SetUniform Float where setUniform = GL.glUniform1f
instance SetUniform Int where setUniform loc = GL.glUniform1i loc . fromIntegral
instance SetUniform (Vec 2) where setUniform loc v = Foreign.with v (GL.glUniform2fv loc 1 . Foreign.castPtr)
instance SetUniform (Vec 3) where setUniform loc v = Foreign.with v (GL.glUniform3fv loc 1 . Foreign.castPtr)
instance SetUniform (Vec 4) where setUniform loc v = Foreign.with v (GL.glUniform4fv loc 1 . Foreign.castPtr)
instance SetUniform (Mat 4 4) where setUniform loc m = Foreign.with m (GL.glUniformMatrix4fv loc 1 1 . Foreign.castPtr)
instance SetUniform (Vec 4 -> Vec 4) where setUniform loc = setUniform loc . transformToMat

setShaderUniform :: SetUniform u => GL.GLuint -> String -> u -> IO ()
setShaderUniform programID uniformName value = do
  uniformLoc <- Foreign.C.String.withCString uniformName (GL.glGetUniformLocation programID)
  setUniform uniformLoc value

smdToTexturedSkeletonVertex :: SMDVertex -> TexturedSkeletonVertex
smdToTexturedSkeletonVertex v =
     vPos v
  :& vNormal v
  :& vUV v
  :& (case vWeights v of
        [(x,_),(y,_),(z,_),(w,_)] -> fromIntegral <$> a4 x y z w
        [(x,_),(y,_),(z,_)] -> fromIntegral <$> a4 x y z (-1)
        [(x,_),(y,_)] -> fromIntegral <$> a4 x y (-1) (-1)
        [(x,_)] -> fromIntegral <$> a4 x (-1) (-1) (-1)
        _ -> fromIntegral <$> a4 (-1) (-1) (-1) (-1))
  :& (case vWeights v of
        [(_,x),(_,y),(_,z),(_,w)] -> v4 x y z w
        [(_,x),(_,y),(_,z)] -> v4 x y z (-1)
        [(_,x),(_,y)] -> v4 x y (-1) (-1)
        [(_,x)] -> v4 x (-1) (-1) (-1)
        _ -> v4 (-1) (-1) (-1) (-1))


--- VERTEX/ATTRIBUTE TYPES -----------------------------------------------------

size :: forall a.Foreign.Storable a => Int
size = Foreign.sizeOf (undefined :: a)
alignment :: forall a.Foreign.Storable a => Int
alignment = Foreign.alignment (undefined :: a)

roundUp :: Int -> Int -> Int
roundUp val base = (val + base - 1) `div` base * base

data a :& b = a :& b deriving (Show)
instance (Foreign.Storable a, Foreign.Storable b) => Foreign.Storable (a :& b) where
  sizeOf _ = roundUp (size @a) (alignment @b) + size @b
  alignment _ = max (alignment @a) (alignment @b)
  peek p = (:&) <$> Foreign.peekByteOff p 0  <*> Foreign.peekByteOff p (roundUp (size @a) (alignment @b))
  poke p (a :& b) = Foreign.pokeByteOff p 0 a >> Foreign.pokeByteOff p (roundUp (size @a) (alignment @b)) b

instance (Foreign.Storable s, KnownNat n) => Foreign.Storable (Array n s) where
  sizeOf _ = (1+fromIntegral (maxBound :: Finite n)) * size @s
  alignment _ = alignment @s
  peek p = do
    let n = fromIntegral (maxBound :: Finite n)
    xs <- Foreign.peekArray n (Foreign.castPtr p)
    pure (Data.Vector.Storable.unsafeIndex (Data.Vector.Storable.fromListN n xs) . fromIntegral . getFinite)
  poke p f = mapM_ (\i -> Foreign.pokeElemOff (Foreign.castPtr p) (fromIntegral i) (f i)) (finites @n)

class Attribute a where configureAttribute :: GL.GLuint -> GL.GLsizei -> Foreign.Ptr p -> IO ()
instance Attribute (Vec 2) where configureAttribute index = GL.glVertexAttribPointer index 2 GL.GL_FLOAT GL.GL_FALSE
instance Attribute (Vec 3) where configureAttribute index = GL.glVertexAttribPointer index 3 GL.GL_FLOAT GL.GL_FALSE
instance Attribute (Vec 4) where configureAttribute index = GL.glVertexAttribPointer index 4 GL.GL_FLOAT GL.GL_FALSE
instance Attribute (Array 2 GL.GLint) where configureAttribute index = GL.glVertexAttribIPointer index 2 GL.GL_INT
instance Attribute (Array 3 GL.GLint) where configureAttribute index = GL.glVertexAttribIPointer index 3 GL.GL_INT
instance Attribute (Array 4 GL.GLint) where configureAttribute index = GL.glVertexAttribIPointer index 4 GL.GL_INT

class Vertex v where
  configureAttributesOpen :: GL.GLsizei -> GL.GLuint -> Foreign.Ptr a -> (GL.GLuint -> Foreign.Ptr a -> IO ()) -> IO ()

instance {-# OVERLAPPABLE #-} (Attribute a, Foreign.Storable a) => Vertex a where
  configureAttributesOpen stride index offset cont = do
    let alignedOffset = Foreign.alignPtr offset (alignment @a)
    GL.glEnableVertexAttribArray index
    configureAttribute @a index stride alignedOffset
    cont (index+1) (Foreign.plusPtr alignedOffset (size @a))

instance {-# OVERLAPPING #-} (Foreign.Storable b, Vertex a, Vertex b) => Vertex (a :& b) where
  configureAttributesOpen stride index0 offset0 cont =
     configureAttributesOpen @a stride index0 offset0
         (\index1 offset1 -> configureAttributesOpen @b stride index1 offset1 cont)

configureAttributes :: forall v.(Foreign.Storable v, Vertex v) => IO ()
configureAttributes = configureAttributesOpen @v (fromIntegral (size @v)) 0 Foreign.nullPtr (\_ _ -> pure ())


--- GENERATE BUFFERS -----------------------------------------------------------

class Index i where
  flatten :: i -> [GL.GLushort]
  indexDim :: Int

instance Integral i => Index (i,i,i) where
  flatten (a,b,c) = fromIntegral <$> [a,b,c]
  indexDim = 3

instance Integral i => Index (i,i) where
  flatten (a,b) = fromIntegral <$> [a,b]
  indexDim = 2

instance Index Int where
  flatten = pure . fromIntegral
  indexDim = 1

data BufferMetadata = BufferMetadata { vertexArrayID :: GL.GLuint, indexCount :: GL.GLsizei } deriving Show

genBuffer :: forall v i.(Foreign.Storable v, Vertex v, Index i) => [i] -> [v] -> IO BufferMetadata
genBuffer indices verts = do
  vertexArrayObjectID <- Foreign.alloca $ \idPtr -> do
      GL.glGenVertexArrays 1 idPtr
      Foreign.peek idPtr
  GL.glBindVertexArray vertexArrayObjectID
  indexBufferID <- Foreign.alloca $ \indexBufferIDPtr -> do
      GL.glGenBuffers 1 indexBufferIDPtr
      Foreign.peek indexBufferIDPtr
  GL.glBindBuffer GL.GL_ELEMENT_ARRAY_BUFFER indexBufferID
  let indexBufferContent :: [GL.GLushort] = concatMap flatten indices
  let indexBufferSize = fromIntegral (size @GL.GLushort * length indexBufferContent)
  Foreign.withArray indexBufferContent (flip (GL.glBufferData GL.GL_ELEMENT_ARRAY_BUFFER indexBufferSize) GL.GL_STATIC_DRAW)
  vertexBufferID <- Foreign.alloca $ \vertexBufferIDPtr -> do
      GL.glGenBuffers 1 vertexBufferIDPtr
      Foreign.peek vertexBufferIDPtr
  GL.glBindBuffer GL.GL_ARRAY_BUFFER vertexBufferID
  let vertexBufferSize = fromIntegral (size @v * length verts)
  Foreign.withArray verts (flip (GL.glBufferData GL.GL_ARRAY_BUFFER vertexBufferSize) GL.GL_STATIC_DRAW)
  configureAttributes @v
  pure (BufferMetadata { vertexArrayID = vertexArrayObjectID , indexCount = fromIntegral (indexDim @i * length indices) })

--- TEXTURED SKELETON SHADER ---------------------------------------------------

type TexturedSkeletonVertex = Vec 3 :& Vec 3 :& Vec 2 :& Array 4 GL.GLint :& Vec 4

texturedSkeletonVertexSrc = 
  """#version 430\r\n
  uniform mat4 modelToProjectionMatrix;
  uniform mat4 modelToWorldTransformMatrix;
  uniform mat4 bones[128];
  in layout(location=0) vec4 vertexPositionModelSpace;
  in layout(location=1) vec3 normalModelSpace;
  in layout(location=2) vec2 vertex_uv;
  in layout(location=3) ivec4 parentId;
  in layout(location=4) vec4 weights;
  out vec3 normalWorldSpace;
  out vec2 fragment_uv;
  out vec3 vertexPositionWorldSpace;
  void main() {
    vec4 posePosition = vec4(0);
    if (parentId[0] != -1) posePosition += weights[0] * bones[parentId[0]] * vertexPositionModelSpace;
    if (parentId[1] != -1) posePosition += weights[1] * bones[parentId[1]] * vertexPositionModelSpace;
    if (parentId[2] != -1) posePosition += weights[2] * bones[parentId[2]] * vertexPositionModelSpace;
    if (parentId[3] != -1) posePosition += weights[3] * bones[parentId[3]] * vertexPositionModelSpace;
    gl_Position = modelToProjectionMatrix * posePosition;
    normalWorldSpace = vec3(modelToWorldTransformMatrix * vec4(normalModelSpace,0));
    vertexPositionWorldSpace = vec3(modelToWorldTransformMatrix * vertexPositionModelSpace);
    fragment_uv = vertex_uv;
  }
  """

texturedSkeletonFragmentSrc = 
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
    float s = pow(clamp(dot(reflectedLightWorldSpace, eyeVectorWorldSpace),0,1),2048);
    vec4 specularLight = vec4(s,s,s,1.0);
    vec4 lighting = clamp(diffuseLight,0.0,1.0) + ambientLight + clamp(specularLight,0,1);
    fragmentColor = lighting * texture(base_texture,fragment_uv);
  }
  """


--- COLORED NORMAL SHADER ------------------------------------------------------

type ColoredNormalVertex = Vec 3 :& Vec 3 :& Vec 3

coloredNormalVertexSrc =
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

coloredNormalFragmentSrc =
  """#version 430\r\n
  uniform vec3 lightPosition;
  uniform vec3 eyePosition;
  uniform vec4 ambientLight;
  uniform int specularity;
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
    float s = pow(clamp(dot(reflectedLightWorldSpace, eyeVectorWorldSpace),0,1),specularity);
    vec4 specularLight = vec4(s,s,s,s);
    vec4 lighting = clamp(diffuseLight,0.0,1.0) + ambientLight + clamp(specularLight,0,1);
    fragmentColor = lighting * vec4(color,1);
  }
  """

--- UNIFORM COLOR SHADER -------------------------------------------------------


uniformColorVertexSrc =
  """#version 430\r\n
  uniform mat4 modelToProjectionMatrix;
  in layout(location=0) vec4 vertexPositionModelSpace;
  void main() {
    gl_Position = modelToProjectionMatrix * vertexPositionModelSpace;
  }
  """

uniformColorFragmentSrc =
  """#version 430\r\n
  uniform vec4 color;
  out vec4 fragmentColor;
  void main() {
    fragmentColor = color;
  }
  """

--- SPARKLE SHADER -------------------------------------------------------------

type SparkleVertex = Vec 3 :& Vec 3 :& Vec 3

sparkleVertexSrc =
  """#version 430\r\n
  layout(std430, binding=1) buffer OutPos {vec4 pos[];};
  uniform mat4 modelToProjectionMatrix;
  out float opacity;
  void main() {
    vec4 vertexPositionModelSpace = pos[gl_VertexID];
    gl_Position = modelToProjectionMatrix * vertexPositionModelSpace;
    gl_PointSize = 1;
    opacity = pow((1-gl_Position.z/gl_Position.w)/2,0.5);
  }
  """

sparkleFragmentSrc =
  """#version 430\r\n
  out vec4 fragmentColor;
  in float opacity;
  void main() {
    fragmentColor = vec4(1,1,1,0.1);
    fragmentColor = vec4(1,1,1,opacity);
  }
  """

lorenzComputeSrc =
  """#version 430\r\n
  layout(local_size_x=256) in;
  layout(std430,binding=0) buffer InPos {vec4 inPos[];};
  layout(std430,binding=1) buffer OutPos {vec4 outPos[];};
  uniform uint N;
  vec3 f(vec3 x) {
    return vec3(10*(x.y-x.x),x.x*(28-x.z)-x.y,x.x*x.y-8/3*x.z);
  }
  void main() {
    uint i = gl_GlobalInvocationID.x;
    if (i >= N) return;
    float dt=0.004;
    vec3 x = inPos[i].xyz;
    for(int s=0;s<5;++s){
      vec3 k1 = f(x);
      vec3 k2 = f(x + 0.5*dt*k1);
      vec3 k3 = f(x + 0.5*dt*k2);
      vec3 k4 = f(x + dt*k3);
      x = x + (dt/6.0)*(k1 + 2.0*k2 + 2.0*k3 + k4);
    }
    outPos[i] = vec4(x, 1.0);
  }
  """

aizawaComputeSrc =
  """#version 430
  layout(local_size_x = 256) in;

  layout(std430, binding = 0) buffer InPos  { vec4 inPos[];  };
  layout(std430, binding = 1) buffer OutPos { vec4 outPos[]; };

  uniform uint  N;

  vec3 F(vec3 X) {
    float a=0.95;float b=0.7;float c=0.6; float d=3.5; float e=0.25; float f=0.1;
    float x = X.x, y = X.y, z = X.z;
    float r2 = x*x + y*y;
    return vec3(
      (z-b)*x - d*y,
      d*x + (z-b)*y,
      c + a*z - (z*z*z)/3 - r2*(1+e*z) + f*z*x*x*x
    );
  }

  void main() {
    uint i = gl_GlobalInvocationID.x;
    if (i >= N) return;
    float dt=0.004;
    vec3 x = inPos[i].xyz;
    for(int s=0; s<5;++s) {
      vec3 k1 = F(x);
      vec3 k2 = F(x + 0.5*dt*k1);
      vec3 k3 = F(x + 0.5*dt*k2);
      vec3 k4 = F(x + dt*k3);
      x = x + (dt/6.0)*(k1 + 2.0*k2 + 2.0*k3 + k4);
    }
    outPos[i] = vec4(x, 1.0);
  }
  """

--- BASIC POSITION SHADER ------------------------------------------------------

type PositionVertex = Vec 4

positionVertexSrc =
  """#version 430\r\n
  uniform mat4 modelToProjectionMatrix;
  in layout(location=0) vec4 vertexPositionModelSpace;
  void main() {
    gl_Position = modelToProjectionMatrix * vertexPositionModelSpace;
  }
  """

positionFragmentSrc =
  """#version 430\r\n
  out vec4 fragmentColor;
  void main() {
    fragmentColor = vec4(1,1,1,1);
  }
  """


--- PICTURE FRAME SHADER -------------------------------------------------------

type PictureVertex = Vec 4

pictureVertexSrc =
  """#version 430\r\n
  uniform mat4 modelToProjectionMatrix;
  uniform float aspectRatio;
  in layout(location=0) vec4 vertexPositionModelSpace;
  in layout(location=1) vec2 vertex_uv;
  out vec2 fragment_uv;
  void main() {
    mat4 scaler = mat4 (
      aspectRatio,0,0,0,
      0,1,0,0,
      0,0,1,0,
      0,0,0,1
    );
    gl_Position = modelToProjectionMatrix * scaler * vertexPositionModelSpace;
    fragment_uv = vertex_uv;
  }
  """

pictureFragmentSrc =
  """#version 430\r\n
  uniform sampler2D base_texture;
  in vec2 fragment_uv;
  out vec4 fragmentColor;
  void main() {
    fragmentColor = texture(base_texture,fragment_uv);
  }
  """


--- MAIN GRAPHICS PROGRAM ------------------------------------------------------

affineTo3d :: Vec 4 -> Vec 3
affineTo3d v = v . weaken / pure (v 3)

moveSpeed = 0.1
cameraSpeed = 1/3

toDegrees = (*(180/pi))
toRadians = (*(pi/180))
clamp low high = max low . min high

handleEvent event appState = case SDL.eventPayload event of
  SDL.WindowResizedEvent e -> 
    let SDL.V2 w h = SDL.windowResizedEventSize e
    in appState { windowWidth = fromIntegral w, windowHeight = fromIntegral h}
  SDL.KeyboardEvent e -> case SDL.keysymKeycode (SDL.keyboardEventKeysym e) of
    SDL.KeycodeQ -> appState { timeToQuit = timeToQuit appState || (SDL.keyboardEventKeyMotion e==SDL.Pressed)}
    _ -> appState
  SDL.MouseMotionEvent mme ->
    let SDL.V2 x y = SDL.mouseMotionEventRelMotion mme -- xy coords are inverted
        limit = 89.999
    in appState
        { cameraYaw = cameraYaw appState - cameraSpeed * fromIntegral x
        , cameraPitch = clamp (-limit) limit (cameraPitch appState - (cameraSpeed * fromIntegral y))
        }
  SDL.MouseButtonEvent mbe ->
    appState { buttonDown =
       if SDL.mouseButtonEventButton mbe == SDL.ButtonLeft
       then case SDL.mouseButtonEventMotion mbe of
         SDL.Released -> False
         SDL.Pressed -> True
       else buttonDown appState}
  _ -> appState

moveCamera direction appState =
   appState { cameraPosition = memo (translate (moveSpeed * direction . weaken) (cameraPosition appState))}

moveForward appState = moveCamera (rotateAround up (cameraYaw appState) (extend north 1)) appState
moveRight appState = moveCamera (rotateAround up (cameraYaw appState) (extend east 1)) appState
moveBackward appState = moveCamera (rotateAround up (cameraYaw appState) (extend south 1)) appState
moveLeft appState = moveCamera (rotateAround up (cameraYaw appState) (extend west 1)) appState
moveUp = moveCamera (v4 0 1 0 1) -- alt for vector literals: ijkl -> v4 0 1 0 1 = j + l
moveDown = moveCamera (v4 0 (-1) 0 1) -- or v4 0 (-1) 0 1 = l - j


main :: IO ()
main = do
  SDL.initializeAll
  window <- SDL.createWindow "App" (SDL.defaultWindow {SDL.windowGraphicsContext=SDL.OpenGLContext SDL.defaultOpenGL})
  SDL.glCreateContext window
  SDL.Raw.Event.setRelativeMouseMode True

  homeAssistantAuthToken <- takeWhile (/='\n') <$> readFile "homeAssistantAuthToken"

  --- INITIALIZE OPENGL --------------------------------------------------------

  GL.glEnable GL.GL_DEPTH_TEST
  GL.glEnable GL.GL_CULL_FACE
  -- GL.glPolygonMode GL.GL_FRONT_AND_BACK GL.GL_LINE
   

  --- CONFIGURE SHADERS --------------------------------------------------------

  let buildShader shaderType shaderSrc programID = do
        shaderID <- GL.glCreateShader shaderType
        Foreign.C.String.withCString shaderSrc (\cstr -> Foreign.with cstr $ \cstrPtr ->
          GL.glShaderSource shaderID 1 cstrPtr Foreign.nullPtr)
        GL.glCompileShader shaderID
        compileStatus <- Foreign.alloca $ \compileStatusPtr -> do
          GL.glGetShaderiv shaderID GL.GL_COMPILE_STATUS compileStatusPtr
          Foreign.peek compileStatusPtr
        when (compileStatus /= 1) $ do
          infoLogLength <- Foreign.alloca $ \infoLogLengthPtr -> do
            GL.glGetShaderiv shaderID GL.GL_INFO_LOG_LENGTH infoLogLengthPtr
            Foreign.peek infoLogLengthPtr
          errorMessage <- Foreign.allocaArray (fromIntegral infoLogLength) $ \logPtr -> do
            GL.glGetShaderInfoLog shaderID infoLogLength Foreign.nullPtr logPtr
            Foreign.C.String.peekCString logPtr
          error errorMessage
        GL.glAttachShader programID shaderID
        GL.glDeleteShader shaderID

  let linkShader programID = do
        GL.glLinkProgram programID
        linkStatus <- Foreign.alloca $ \linkStatusPtr -> do
          GL.glGetProgramiv programID GL.GL_LINK_STATUS linkStatusPtr
          Foreign.peek linkStatusPtr
        when (linkStatus /= 1) $ do
          infoLogLength <- Foreign.alloca $ \infoLogLengthPtr -> do
            GL.glGetProgramiv programID GL.GL_INFO_LOG_LENGTH infoLogLengthPtr
            Foreign.peek infoLogLengthPtr
          errorMessage <- Foreign.allocaArray (fromIntegral infoLogLength) $ \logPtr -> do
            GL.glGetProgramInfoLog programID infoLogLength Foreign.nullPtr logPtr
            Foreign.C.String.peekCString logPtr
          error errorMessage
        pure programID

  let initComputeShader computeShaderCode = do
        programID <- GL.glCreateProgram
        buildShader GL.GL_COMPUTE_SHADER computeShaderCode programID
        linkShader programID

  let initShader vertexShaderCode fragmentShaderCode = do
        programID <- GL.glCreateProgram
        buildShader GL.GL_VERTEX_SHADER vertexShaderCode programID
        buildShader GL.GL_FRAGMENT_SHADER fragmentShaderCode programID
        linkShader programID

  lorenzComputeShader <- initComputeShader lorenzComputeSrc
  aizawaComputeShader <- initComputeShader aizawaComputeSrc

  basicShader <- initShader coloredNormalVertexSrc coloredNormalFragmentSrc
  sparkleShader <- initShader sparkleVertexSrc sparkleFragmentSrc
  textureShader <- initShader texturedSkeletonVertexSrc texturedSkeletonFragmentSrc
  skellyShader <- initShader positionVertexSrc positionFragmentSrc
  pictureShader <- initShader pictureVertexSrc pictureFragmentSrc
  uniformColorShader <- initShader uniformColorVertexSrc uniformColorFragmentSrc

  --- LOAD OBJECTS -------------------------------------------------------------

  let initializeObject obj =
        genBuffer (faces obj) (zipWith (\v n -> position v :& color v :& n) (vertices obj) (normals obj))
  
  cubeMetadata <- initializeObject cubeObj
  pyramidMetadata <- initializeObject pyramidObj
  planeMetadata <- initializeObject planeObj
  icosahedronMetadata <- initializeObject icosahedron

  teapotMetadata <- do
    let filename = "resources/teapot.obj"
    readFile filename >>= either (error . show) initializeObject . MP.parse pObj filename

  zigzagoon <- do
    let filename = "resources/Zigzagoon/Zigzagoon.SMD"
    readFile filename >>= either (error . show) (pure . smdscale 0.03) . MP.parse pSMD filename

  ziganim <- do
    let filename = "resources/Zigzagoon/anims/Bounce.smd"
    readFile filename >>= either (error . show) (pure . smdscale 0.03) . MP.parse pSMD filename

  pictureFrame <-
     genBuffer @(Vec 3 :& Vec 2) @(Int,Int,Int)
        [(0,1,2),(2,1,3)]
        [ v3 1 0 1 :& v2 0 1
        , v3 1 0 (-1) :& v2 0 0
        , v3 (-1) 0 1 :& v2 1 1
        , v3 (-1) 0 (-1) :& v2 1 0
        ]

  let readImg path = do
        img <- Codec.Picture.convertRGBA8 <$> (Codec.Picture.readImage path >>= either error pure)
        -- opengl textures start at the bottom left so gotta flip
        let flippedY = Codec.Picture.generateImage
              (\x y -> Codec.Picture.pixelAt img x (Codec.Picture.imageHeight img - 1 - y))
              (Codec.Picture.imageWidth img) (Codec.Picture.imageHeight img)
        textureID <- Foreign.alloca $ \textureIDPtr -> do
            GL.glGenTextures 1 textureIDPtr
            Foreign.peek textureIDPtr
        GL.glBindTexture GL.GL_TEXTURE_2D textureID
        Data.Vector.Storable.unsafeWith
           (Codec.Picture.imageData flippedY)
           (GL.glTexImage2D
              GL.GL_TEXTURE_2D
              0
              (fromIntegral GL.GL_SRGB8_ALPHA8)
              (fromIntegral (Codec.Picture.imageWidth img))
              (fromIntegral (Codec.Picture.imageHeight img))
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
        let aspectRatio = fromIntegral (Codec.Picture.imageWidth img) / fromIntegral (Codec.Picture.imageHeight img)
        pure (textureID,aspectRatio)

  matMap <- sequenceA
     (Data.Map.Internal.fromSet
          (\matName -> do
              (textureID,_) <- readImg ("resources/Zigzagoon/images/" ++ matName)
              let smd = zigzagoon {triangles = filter ((matName==).material) (triangles zigzagoon)}
              let smdTris = fmap (\n -> (3*n,3*n+1,3*n+2)) [0..length (triangles smd) - 1]
              let smdVerts = concatMap (\v -> let (a,b,c) = verts v in [a,b,c]) (triangles smd)
              md <- genBuffer smdTris (smdToTexturedSkeletonVertex <$> smdVerts)
              pure (textureID,md))
          (Data.Set.fromList (material <$> triangles zigzagoon)))

  -- skelly <- let pose = head (tail (smdPoses ziganim)) in genBuffer (roseEdges pose) (snd <$> flattenRose pose)

  tour <- readImg "resources/tour-of-the-universe-mccall-studios-1.jpg"


  --- CONFIGURE COMPUTE SHADER -------------------------------------------------
  
  let lorenzParticleCount = 100000
  (lorenzParticleBufferA,lorenzParticleBufferB) <- do 
      let lorenzInitialParticlePositions =
             [extend (fromIntegral s / fromIntegral lorenzParticleCount) 1 | s <- [1..lorenzParticleCount+1]] :: [Vec 4]
      let lorenzBufferSize = fromIntegral (size @(Vec 4) * lorenzParticleCount)
      lorenzParticleBufferA <- Foreign.alloca $ \ssboAPtr -> do
        GL.glCreateBuffers 1 ssboAPtr
        Foreign.peek ssboAPtr
      Foreign.withArray lorenzInitialParticlePositions (\ptr -> GL.glNamedBufferStorage lorenzParticleBufferA lorenzBufferSize ptr 0)
      lorenzParticleBufferB <- Foreign.alloca $ \ssboBPtr -> do
        GL.glCreateBuffers 1 ssboBPtr
        Foreign.peek ssboBPtr
      GL.glNamedBufferStorage lorenzParticleBufferB lorenzBufferSize Foreign.nullPtr 0
      pure (lorenzParticleBufferA, lorenzParticleBufferB)

  let pointGrid = do
        let dim = 50
        let axis = fromIntegral <$> [-dim..dim]
        x <- axis
        y <- axis
        z <- axis
        pure (v4 x y z 1)

  ssboAAizawa <- Foreign.alloca $ \ssboAPtr -> do
    GL.glCreateBuffers 1 ssboAPtr
    Foreign.peek ssboAPtr
  let initialPositionsAizawa = (*extend 0.1 1) <$> pointGrid
  let initialPositionsSizeAizawa = fromIntegral (size @(Vec 4) * length initialPositionsAizawa)
  Foreign.withArray initialPositionsAizawa (flip (GL.glNamedBufferStorage ssboAAizawa initialPositionsSizeAizawa) 0)
  ssboBAizawa <- Foreign.alloca $ \ssboBPtr -> do
    GL.glCreateBuffers 1 ssboBPtr
    Foreign.peek ssboBPtr
  GL.glNamedBufferStorage ssboBAizawa initialPositionsSizeAizawa Foreign.nullPtr 0

  emptyVao <- Foreign.alloca $ \ptr -> do
    GL.glCreateVertexArrays 1 ptr
    Foreign.peek ptr

  --- MAIN LOOP ----------------------------------------------------------------
  
  let loop prevAppState = do

        --- HANDLE EVENTS ------------------------------------------------------
        
        let (animState:remainingFrames) = animation prevAppState

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
        when (windowWidth prevAppState /= windowWidth appState || windowHeight prevAppState /= windowHeight appState)
          (GL.glViewport 0 0 (fromIntegral (windowWidth appState)) (fromIntegral (windowHeight appState)))
        SDL.glSwapWindow window

        --- COMPUTE SHADER -----------------------------------------------------

        GL.glUseProgram lorenzComputeShader
        uniformLoc <- Foreign.C.String.withCString "N" (GL.glGetUniformLocation lorenzComputeShader)
        GL.glUniform1ui uniformLoc (fromIntegral lorenzParticleCount)
        GL.glBindBufferBase GL.GL_SHADER_STORAGE_BUFFER 0 (lorenzParticleBufferIn appState)
        GL.glBindBufferBase GL.GL_SHADER_STORAGE_BUFFER 1 (lorenzParticleBufferOut appState)
        GL.glDispatchCompute ((fromIntegral lorenzParticleCount + 255) `div` 256) 1 1
        GL.glMemoryBarrier GL.GL_SHADER_STORAGE_BARRIER_BIT

        GL.glUseProgram aizawaComputeShader
        uniformLoc <- Foreign.C.String.withCString "N" (GL.glGetUniformLocation aizawaComputeShader)
        GL.glUniform1ui uniformLoc (fromIntegral (length initialPositionsAizawa))
        GL.glBindBufferBase GL.GL_SHADER_STORAGE_BUFFER 0 (aizawaParticleBufferIn appState)
        GL.glBindBufferBase GL.GL_SHADER_STORAGE_BUFFER 1 (aizawaParticleBufferOut appState)
        GL.glDispatchCompute ((fromIntegral (length initialPositionsAizawa) + 255) `div` 256) 1 1
        GL.glMemoryBarrier GL.GL_SHADER_STORAGE_BARRIER_BIT

        --- DRAW ---------------------------------------------------------------
        
        GL.glClear GL.GL_COLOR_BUFFER_BIT
        GL.glClear GL.GL_DEPTH_BUFFER_BIT

        let aspectRatio = fromIntegral (windowWidth appState) / fromIntegral (windowHeight appState)
        let cameraViewDirection = rotateAround up (cameraYaw appState) (rotateAround east (cameraPitch appState) (extend north 1))
        let worldToView = worldToViewMatrix (cameraPosition appState . weaken) (cameraViewDirection . weaken)
        let projectionMatrix = projection 60 aspectRatio 0.1 50
        let toScreenspace t = projectionMatrix . worldToView . t
        let drawTriangulation shader obj transform = do
              GL.glBindVertexArray (vertexArrayID obj)
              setShaderUniform shader "modelToWorldTransformMatrix" transform
              setShaderUniform shader "modelToProjectionMatrix" (toScreenspace transform)
              GL.glDrawElements GL.GL_TRIANGLES (indexCount obj) GL.GL_UNSIGNED_SHORT Foreign.nullPtr
        let lightPosition = 3*north + 2*east

        GL.glUseProgram basicShader
        setShaderUniform basicShader "ambientLight" (v4 0.3 0.3 0.3 1)
        setShaderUniform basicShader "specularity" (64 :: Int)
        setShaderUniform basicShader "lightPosition" lightPosition
        setShaderUniform basicShader "eyePosition" (cameraPosition appState . weaken)

        drawTriangulation basicShader cubeMetadata (translate (north + 4*west) . rotateAround (v3 1 1 0) 45)
        drawTriangulation basicShader cubeMetadata (translate (south + 4*west) . rotateAround (v3 1 1 1) 45)
        drawTriangulation basicShader pyramidMetadata (translate (2*west))
        drawTriangulation basicShader planeMetadata (translate (2*down + 4*west))
        drawTriangulation basicShader teapotMetadata (translate (3*north + up + 8*west))



        let cameraViewDirection3 = affineTo3d cameraViewDirection
        let cameraPosition3 = affineTo3d (cameraPosition appState) 

        let hitboxCenter = 4 * up

        let rayHit =
             let positionMinusCamera = hitboxCenter - cameraPosition3
             in 1 > magnitude (project positionMinusCamera cameraViewDirection3 - positionMinusCamera)

        let color = case (lightOn appState, buttonDown appState, rayHit) of
             (True,True,True) -> v4 0.3 0.9 0.3 1
             (True,True,False) -> v4 0.1 0.7 0.1 1
             (True,False,True) -> v4 0.2 0.8 0.2 1
             (True,False,False) -> v4 0.1 0.7 0.1 1
             (False,True,True) -> v4 0.9 0.3 0.3 1
             (False,True,False) -> v4 0.7 0.1 0.1 1
             (False,False,True) -> v4 0.8 0.1 0.1 1
             (False,False,False) -> v4 0.7 0.1 0.1 1

        let toggleLight = buttonDown prevAppState && not (buttonDown appState) && rayHit

        when toggleLight $ do
          let lightCommand =  if lightOn prevAppState then "off" else "on"
          -- System.Process.spawnProcess "/home/endless/laboratory/simulacrum/set-light.nu" [lightCommand]
          -- let token = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJhYzg3OGQ4NjQyOTE0MmEwYWY0ZWI5ZGMyODRjZmU0MCIsImlhdCI6MTc2MDc0ODYyNSwiZXhwIjoyMDc2MTA4NjI1fQ.icKT9894NC4FKtjo_ZZjv2bkh8i_vr1Hz8VHFlXpM_4"
          let request =
                Network.HTTP.Simple.setRequestMethod "POST"
                $ Network.HTTP.Simple.setRequestBodyJSON (Data.Aeson.object ["entity_id" Data.Aeson..= ("light.l" :: String)])
                $ Network.HTTP.Simple.setRequestHeader "Authorization" ["Bearer " <> Data.String.fromString homeAssistantAuthToken]
                $ Data.String.fromString ("http://homeassistant.local:8123/api/services/light/turn_" ++ lightCommand)
          Control.Concurrent.Async.async (Network.HTTP.Simple.httpNoBody request)
          putStrLn ("turning light " ++ lightCommand)
          

        
        GL.glUseProgram uniformColorShader
        GL.glBindVertexArray (vertexArrayID icosahedronMetadata)
        setShaderUniform uniformColorShader "color" color
        setShaderUniform uniformColorShader "modelToProjectionMatrix" (toScreenspace (translate hitboxCenter))
        GL.glDrawElements GL.GL_TRIANGLES (indexCount icosahedronMetadata) GL.GL_UNSIGNED_SHORT Foreign.nullPtr

        let zigTransform = translate (2*south + 2*down + west) . rotateAround up 45 . rotateAround south 90

        GL.glUseProgram textureShader

        let zigpose = transformToMat <$> pose zigzagoon (head (skeleton zigzagoon)) animState
        bonesloc <- Foreign.C.String.withCString "bones" (GL.glGetUniformLocation textureShader)
        Foreign.withArray zigpose (GL.glUniformMatrix4fv bonesloc (fromIntegral $ length zigpose) 1 . Foreign.castPtr)
        setShaderUniform textureShader "ambientLight" (v4 0.3 0.3 0.3 1)
        setShaderUniform textureShader "lightPosition" lightPosition
        setShaderUniform textureShader "eyePosition" (cameraPosition appState . weaken)
        mapM_
           (\(textureID,zigMetadata) -> do
                GL.glActiveTexture GL.GL_TEXTURE0
                GL.glBindTexture GL.GL_TEXTURE_2D textureID
                Foreign.C.String.withCString "texture" (GL.glGetUniformLocation textureShader)
                     >>= flip GL.glUniform1i 0
                drawTriangulation textureShader zigMetadata zigTransform)
           matMap


        let portraitTransform = translate (12*south + 2*up) . rotateAround down 90 . rotateAround south 90
        GL.glActiveTexture GL.GL_TEXTURE0
        GL.glBindTexture GL.GL_TEXTURE_2D (fst tour)
        Foreign.C.String.withCString "texture" (GL.glGetUniformLocation pictureShader) >>= flip GL.glUniform1i 0
        GL.glUseProgram pictureShader
        GL.glBindVertexArray (vertexArrayID pictureFrame)
        setShaderUniform pictureShader "aspectRatio" (snd tour :: Float)
        setShaderUniform pictureShader "modelToProjectionMatrix" (toScreenspace portraitTransform)
        GL.glDrawElements GL.GL_TRIANGLES 6 GL.GL_UNSIGNED_SHORT Foreign.nullPtr
        

        -- GL.glClear GL.GL_DEPTH_BUFFER_BIT
        -- GL.glUseProgram skellyShader
        -- GL.glBindVertexArray (vertexArrayID skelly)
        -- setShaderUniform skellyShader "modelToProjectionMatrix" (toScreenspace zigTransform)
        -- GL.glDrawElements GL.GL_LINES (indexCount skelly) GL.GL_UNSIGNED_SHORT Foreign.nullPtr

        GL.glEnable GL.GL_BLEND
        GL.glBlendFunc GL.GL_SRC_ALPHA GL.GL_ONE_MINUS_SRC_ALPHA
        GL.glDepthMask GL.GL_FALSE
        GL.glEnable GL.GL_PROGRAM_POINT_SIZE
        GL.glUseProgram sparkleShader
        let pointCloudTransform = translate (5*west + north*14 + up*2) . (*v4 0.1 0.1 0.1 1)
        GL.glBindVertexArray emptyVao
        GL.glBindBufferBase GL.GL_SHADER_STORAGE_BUFFER 1 (lorenzParticleBufferOut appState)
        setShaderUniform sparkleShader "modelToProjectionMatrix" (toScreenspace pointCloudTransform)
        GL.glDrawArrays GL.GL_POINTS 0 (fromIntegral lorenzParticleCount)
        GL.glDepthMask GL.GL_TRUE
        GL.glDisable GL.GL_BLEND

        GL.glEnable GL.GL_BLEND
        GL.glBlendFunc GL.GL_SRC_ALPHA GL.GL_ONE_MINUS_SRC_ALPHA
        GL.glDepthMask GL.GL_FALSE
        GL.glEnable GL.GL_PROGRAM_POINT_SIZE
        GL.glUseProgram sparkleShader
        let pointCloudTransform = translate (12*east + north*14 + up*2) . (*v4 0.4 0.4 0.4 1)
        GL.glBindVertexArray emptyVao
        GL.glBindBufferBase GL.GL_SHADER_STORAGE_BUFFER 1 (aizawaParticleBufferOut appState)
        setShaderUniform sparkleShader "modelToProjectionMatrix" (toScreenspace pointCloudTransform)
        GL.glDrawArrays GL.GL_POINTS 0 (fromIntegral $ length initialPositionsAizawa)
        GL.glDepthMask GL.GL_TRUE
        GL.glDisable GL.GL_BLEND

        unless (timeToQuit appState) (loop appState
             { lorenzParticleBufferIn = lorenzParticleBufferOut appState
             , lorenzParticleBufferOut = lorenzParticleBufferIn appState
             , aizawaParticleBufferIn = aizawaParticleBufferOut appState
             , aizawaParticleBufferOut = aizawaParticleBufferIn appState
             , animation = remainingFrames
             , lightOn = if toggleLight && rayHit then not (lightOn appState) else lightOn appState
             })

  let initialAppState = AppState
          { timeToQuit = False
          , windowWidth = 800
          , windowHeight = 600
          , viewDirection = v4 0.98 (-0.15) (-0.11) 1
          , cameraYaw = 0
          , cameraPitch = 0
          , cameraPosition = v4 12.5 2.2 12.3 1
          , animation = cycle (skeleton ziganim)
          , lorenzParticleBufferIn = lorenzParticleBufferA
          , lorenzParticleBufferOut = lorenzParticleBufferB
          , aizawaParticleBufferIn = ssboAAizawa
          , aizawaParticleBufferOut = ssboBAizawa
          , buttonDown = False
          , lightOn = False
          }

  loop initialAppState


  --- CLEANUP ------------------------------------------------------------------

  GL.glUseProgram 0
  GL.glDeleteProgram basicShader
  SDL.destroyWindow window

data AppState = AppState
  { timeToQuit :: Bool
  , windowWidth :: Int
  , windowHeight :: Int
  , viewDirection :: Vec 4
  , cameraPitch :: Float
  , cameraYaw :: Float
  , cameraPosition :: Vec 4
  , animation :: [SMDSkeletonFrame]
  , lorenzParticleBufferIn :: GL.GLuint
  , lorenzParticleBufferOut :: GL.GLuint
  , aizawaParticleBufferIn :: GL.GLuint
  , aizawaParticleBufferOut :: GL.GLuint
  , buttonDown :: Bool
  , lightOn :: Bool
  }

