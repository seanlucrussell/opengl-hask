{-# LANGUAGE OverloadedStrings,LinearTypes,ScopedTypeVariables,OverloadedStrings,MultilineStrings,DataKinds,TypeOperators,FlexibleInstances,UndecidableInstances,TypeApplications,AllowAmbiguousTypes,GADTs,TypeFamilies #-}
module Main where
import           Control.Applicative
import           Control.Monad (unless,when)
import           Data.Finite
import qualified Foreign
import qualified Data.List
import           GHC.TypeNats
import qualified Data.Array
import qualified Foreign.C.String
import qualified Graphics.GL.Core46 as GL
import qualified SDL
import qualified SDL.Raw.Event
import qualified SDL.Raw.Video

import qualified System.Random

import qualified System.IO as IO


import qualified Text.Megaparsec            as MP
import qualified Text.Megaparsec.Char       as C
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.Char  as Char
import           Data.Void  (Void)
import           Control.Applicative ((<|>), optional)
import qualified Control.Monad (void)

import qualified Codec.Picture
import qualified Data.Vector.Storable

import qualified Data.Set
import qualified Data.Map
import qualified Data.Map.Internal


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

v2 :: Float -> Float -> Vec 2;                   v2 x y i = case i of {0->x;1->y}
v3 :: Float -> Float -> Float -> Vec 3;          v3 x y z i = case i of {0->x;1->y;2->z}
v4 :: Float -> Float -> Float -> Float -> Vec 4; v4 x y z w i = case i of {0->x;1->y;2->z;3->w}

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


--- OBJ PARSER -----------------------------------------------------------------

type Parser = MP.Parsec Void String
type Face = (Int, Int, Int)
data OBJ_Vertex = OBJ_Vertex {position :: Vec 3, color :: Vec 3} deriving Show
data Obj = Obj { vertices :: [OBJ_Vertex], faces :: [Face] } deriving Show

pObj :: Parser Obj
pObj =
  let
    hspace1 = Control.Monad.void $ MP.some (MP.satisfy Char.isSpace) :: Parser ()
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
  ([ OBJ_Vertex { position = position vert, color = v3 1 1 1 }
   , OBJ_Vertex { position = position vert + 0.3 * normal, color = v3 1 1 1 }
   ])

cubeObj = Obj
  { vertices =
    [ OBJ_Vertex {position = v3 (-1) ( 1.0) ( 1.0), color = v3 1 0.0 0.0}
    , OBJ_Vertex {position = v3 ( 1) ( 1.0) ( 1.0), color = v3 0 1.0 0.0}
    , OBJ_Vertex {position = v3 ( 1) ( 1.0) (-1.0), color = v3 0 0.0 1.0}
    , OBJ_Vertex {position = v3 (-1) ( 1.0) (-1.0), color = v3 1 1.0 1.0}
    , OBJ_Vertex {position = v3 (-1) ( 1.0) (-1.0), color = v3 1 0.0 1.0}
    , OBJ_Vertex {position = v3 ( 1) ( 1.0) (-1.0), color = v3 0 0.5 0.2}
    , OBJ_Vertex {position = v3 ( 1) (-1.0) (-1.0), color = v3 0 0.6 0.4}
    , OBJ_Vertex {position = v3 (-1) (-1.0) (-1.0), color = v3 0 1.0 0.5}
    , OBJ_Vertex {position = v3 ( 1) ( 1.0) (-1.0), color = v3 0 0.5 0.2}
    , OBJ_Vertex {position = v3 ( 1) ( 1.0) ( 1.0), color = v3 0 0.3 0.7}
    , OBJ_Vertex {position = v3 ( 1) (-1.0) ( 1.0), color = v3 0 0.7 1.0}
    , OBJ_Vertex {position = v3 ( 1) (-1.0) (-1.0), color = v3 0 0.7 0.5}
    , OBJ_Vertex {position = v3 (-1) ( 1.0) ( 1.0), color = v3 0 0.8 0.2}
    , OBJ_Vertex {position = v3 (-1) ( 1.0) (-1.0), color = v3 0 0.7 0.3}
    , OBJ_Vertex {position = v3 (-1) (-1.0) (-1.0), color = v3 0 0.7 0.7}
    , OBJ_Vertex {position = v3 (-1) (-1.0) ( 1.0), color = v3 0 0.5 1.0}
    , OBJ_Vertex {position = v3 ( 1) ( 1.0) ( 1.0), color = v3 0 1.0 0.7}
    , OBJ_Vertex {position = v3 (-1) ( 1.0) ( 1.0), color = v3 0 0.4 0.8}
    , OBJ_Vertex {position = v3 (-1) (-1.0) ( 1.0), color = v3 0 0.8 0.7}
    , OBJ_Vertex {position = v3 ( 1) (-1.0) ( 1.0), color = v3 0 0.7 1.0}
    , OBJ_Vertex {position = v3 ( 1) (-1.0) (-1.0), color = v3 0 0.3 0.7}
    , OBJ_Vertex {position = v3 (-1) (-1.0) (-1.0), color = v3 0 0.9 0.5}
    , OBJ_Vertex {position = v3 (-1) (-1.0) ( 1.0), color = v3 0 0.8 0.5}
    , OBJ_Vertex {position = v3 ( 1) (-1.0) ( 1.0), color = v3 0 1.0 0.2}
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
    let randColor idx = (fst (System.Random.uniformR (0,1) (System.Random.mkStdGen (intPairInject idx))))
    pure (OBJ_Vertex
        { position = v3 (x - fromIntegral planeWidth / 2) 0 (y - fromIntegral planeHeight / 2)
        , color = v3 (randColor 0) (randColor 1) (randColor 2)
        })
  , faces = do
      i <- [0..planeHeight-2]
      j <- [0..planeWidth-2]
      squareIndices (i*planeWidth+j) (i*planeWidth+j+1) ((i+1)*planeWidth+j) ((i+1)*planeWidth+j+1)
  }


--- SMD PARSER -----------------------------------------------------------------

data SMD_Node = SMD_Node Int String Int deriving (Show)
data SMD_BoneFrame = SMD_BoneFrame { boneId :: Int, pos :: Vec 3, rot :: Vec 3 } deriving (Show)
data SMD_SkeletonFrame = SMD_SkeletonFrame { frameNum :: Int, bones :: [SMD_BoneFrame] } deriving (Show)
data SMD_Vertex = SMD_Vertex { vParent :: Int, vPos :: Vec 3, vNormal :: Vec 3, vUV :: Vec 2, vWeights :: [(Int, Float)] } deriving (Show)
data SMD_Triangle = SMD_Triangle { material :: String, verts :: (SMD_Vertex,SMD_Vertex,SMD_Vertex) } deriving (Show)
data SMD = SMD { nodes :: [SMD_Node], skeleton :: [SMD_SkeletonFrame], triangles :: [SMD_Triangle] } deriving (Show)
-- data SMD_Reference = SMD { reference_nodes :: [SMD_Node], reference_frame :: SMD_SkeletonFrame, triangles :: [SMD_Triangle] } deriving (Show)
-- data SMD_Animation = SMD { animation_nodes :: [SMD_Node], skeleton_frames :: [SMD_SkeletonFrame] } deriving (Show)

smdscale factor smd =
  let vscale v = v {vPos = factor * vPos v}
      triscale t = let (a,b,c) = verts t in t {verts = (vscale a, vscale b, vscale c)}
      bonescale b = b {pos = factor * pos b}
      framescale f = f {bones = bonescale <$> bones f}
  in smd {triangles = triscale <$> triangles smd, skeleton = framescale <$> skeleton smd}

pSMD :: Parser SMD
pSMD =
  let
    sc            = L.space C.space1 empty empty                                      :: Parser ()
    lexeme        = L.lexeme sc                                                   :: Parser a -> Parser a
    symbol        = L.symbol sc                                                   :: String -> Parser String
    integer       = lexeme (L.signed sc L.decimal)                            :: Parser Int
    float         = lexeme (L.signed sc L.float)                              :: Parser Float
    stringLiteral = lexeme (C.char '"' *> MP.manyTill L.charLiteral (C.char '"')) :: Parser String
    vec2          = v2 <$> float <*> float
    vec3          = v3 <$> float <*> float <*> float
    weights = optional integer >>= maybe (pure []) (flip MP.count ((,) <$> integer <*> float))
    pVertex = lexeme $ SMD_Vertex <$> integer <*> vec3 <*> vec3 <*> vec2 <*> weights
  in do
  symbol "version" >> integer
  ns <- symbol "nodes" >>
        MP.manyTill
           (lexeme (SMD_Node <$> integer <*> stringLiteral <*> integer))
        (symbol "end")     
  sk <- symbol "skeleton" >>
        MP.manyTill
           (symbol "time" >> SMD_SkeletonFrame <$>
            integer <*>
            MP.many (lexeme (SMD_BoneFrame <$> integer <*> vec3 <*> vec3)))
        (symbol "end")
  ts <- MP.optional
        (symbol "triangles" >>
         MP.manyTill (lexeme (SMD_Triangle <$>
              lexeme (MP.someTill MP.anySingle C.eol) <*>
              ((,,) <$> pVertex <*> pVertex <*> pVertex)))
         (symbol "end"))
  MP.eof
  pure (SMD ns sk (maybe [] id ts))

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

smdPoseTransforms :: SMD -> SMD_SkeletonFrame -> Rose (Int, Vec 4 -> Vec 4)
smdPoseTransforms smd frame =
  let skeletonIndices = filter (\(a,b) -> a /= -1 && b /= -1) (fmap (\(SMD_Node start _ end) -> (start,end)) (nodes smd))
      childMap = Data.Map.fromListWith (++) ((\(v,k) -> (k,[v])) <$> skeletonIndices)
      subtree transform n =
         let bone = bones frame !! n
             newTransform =
                 transform
                 . memo
                 . translate (pos bone)
                 . rotation (180/pi * rot bone z) (v3 0 0 1)
                 . rotation (180/pi * rot bone y) (v3 0 1 0)
                 . rotation (180/pi * rot bone x) (v3 1 0 0)
         in Rose (boneId bone, newTransform) (subtree newTransform <$> maybe [] id (Data.Map.lookup n childMap))
  in subtree id 0

-- would this be better if we represented a pose by `Map Int [Int,Vec 4 -> Vec 4]`

-- really we should distinguish a base pose from an animation. i think the right way to do this is ONE parser for all formats, but then have a postprocessing step that does convention checks for references (e.g. only one time 0) and animations (no triangles) and puts things into more structured data types
smdReferencePoseTransform :: SMD -> SMD_SkeletonFrame -> Rose (Int, Vec 4 -> Vec 4)
smdReferencePoseTransform smd frame =
  let skeletonIndices = filter (\(a,b) -> a /= -1 && b /= -1) (fmap (\(SMD_Node start _ end) -> (start,end)) (nodes smd))
      childMap = Data.Map.fromListWith (++) ((\(v,k) -> (k,[v])) <$> skeletonIndices)
      subtree transform n =
       let bone = bones frame !! n
           newTransform =
             memo
             . rotation (-180/pi * rot bone x) (v3 1 0 0)
             . rotation (-180/pi * rot bone y) (v3 0 1 0)
             . rotation (-180/pi * rot bone z) (v3 0 0 1)
             . translate (-pos bone)
             . transform
       in Rose (boneId bone, newTransform) (subtree newTransform <$> maybe [] id (Data.Map.lookup n childMap))
  in subtree id 0

refposflat :: SMD -> SMD_SkeletonFrame -> [Vec 4 -> Vec 4]
refposflat smd frame = fmap snd (Data.List.sortOn fst (flattenRose (smdReferencePoseTransform smd frame)))

animposflat :: SMD -> SMD_SkeletonFrame -> [Vec 4 -> Vec 4]
animposflat smd frame = fmap snd (Data.List.sortOn fst (flattenRose (smdPoseTransforms smd frame)))



pose :: SMD -> SMD_SkeletonFrame -> SMD_SkeletonFrame -> [Vec 4 -> Vec 4]
-- pose smd ref frame = replicate 30 id
pose smd ref frame = zipWith (.)  (animposflat frame) (refposflat ref)
  where
    skeletonIndices = filter (\(a,b) -> a /= -1 && b /= -1) (fmap (\(SMD_Node start _ end) -> (start,end)) (nodes smd))
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
                     . rotation (180/pi * rot bone z) (v3 0 0 1)
                     . memo
                     . rotation (180/pi * rot bone y) (v3 0 1 0)
                     . memo
                     . rotation (180/pi * rot bone x) (v3 1 0 0)
             in Rose (boneId bone, newTransform) (subtree newTransform <$> maybe [] id (Data.Map.lookup n childMap))
      in subtree id 0
    smdReferencePoseTransform frame =
      let subtree transform n =
           let bone = bones frame !! n
               newTransform =
                 memo
                 . rotation (-180/pi * rot bone x) (v3 1 0 0)
                 . memo
                 . rotation (-180/pi * rot bone y) (v3 0 1 0)
                 . memo
                 . rotation (-180/pi * rot bone z) (v3 0 0 1)
                 . memo
                 . translate (-pos bone)
                 . memo
                 . transform
           in Rose (boneId bone, newTransform) (subtree newTransform <$> maybe [] id (Data.Map.lookup n childMap))
      in subtree id 0
    refposflat frame = fmap snd (Data.List.sortOn fst (flattenRose (smdReferencePoseTransform  frame)))
    animposflat frame = fmap snd (Data.List.sortOn fst (flattenRose (smdPoseTransforms frame)))


-- honestly the simples way to do this is probably to track the vertex id and then pack things into an array after they are built.


-- challenge: keeping indices synced. the vertex attributes have to reference the same pose transforms as the references. so maybe we really SHOULD do a map-based op
--
-- cuz we only need the tree for visualizing right? for linking and visualizing. we should preserve the other stuff
--
-- err rather than map, this is really a list right? or array?
--
-- better for doing type level stuff if we ever get there too.

-- test: composition of reference pose as base and reference pose as anim should yield no change
-- test: uploadign a const noop to the ubo or whatever shouldn't do anything either

-- practically this should be a tree of vector-vector transformations that transform from some local space into global space


--- GL UTILS -------------------------------------------------------------------

-- okay so configureing an attribute. we got attribute index, size, type. from these we can compute stride, buffer offset, etc
-- index is presumet do start from 0

type Attribute = (GL.GLint, GL.GLenum)

-- possibly should have our own enum for this. if nothing else then just to track what has been implemented so far
enumSize :: Integral i => GL.GLenum -> i
enumSize GL.GL_FLOAT = fromIntegral $ size @GL.GLfloat
enumSize GL.GL_INT = fromIntegral $ size @GL.GLint

data ObjMetadata = ObjMetadata { vertexArrayID :: GL.GLuint, indexCount :: GL.GLsizei } deriving Show

class SetUniform u where setUniform :: GL.GLint -> u -> IO ()
instance SetUniform (Vec 2) where setUniform loc v = Foreign.with v (GL.glUniform2fv loc 1 . Foreign.castPtr)
instance SetUniform (Vec 3) where setUniform loc v = Foreign.with v (GL.glUniform3fv loc 1 . Foreign.castPtr)
instance SetUniform (Vec 4) where setUniform loc v = Foreign.with v (GL.glUniform4fv loc 1 . Foreign.castPtr)
instance SetUniform (Mat 4 4) where setUniform loc m = Foreign.with m (GL.glUniformMatrix4fv loc 1 1 . Foreign.castPtr)
instance SetUniform (Vec 4 -> Vec 4) where setUniform loc = setUniform loc . transformToMat

class Vertex v where layout :: [Attribute]
instance KnownNat n => Vertex (Vec n) where layout = [(fromIntegral (maxBound :: Finite n)+1,GL.GL_FLOAT)]


instance (Data.Vector.Storable.Storable s, KnownNat n) => Data.Vector.Storable.Storable (Array n s) where
  sizeOf _ = (1+fromIntegral (maxBound :: Finite n)) * size @s
  alignment _ = alignment @s
  peek p = do
    let n = fromIntegral (maxBound :: Finite n)
    xs <- Foreign.peekArray n (Foreign.castPtr p)
    pure (Data.Vector.Storable.unsafeIndex (Data.Vector.Storable.fromListN n xs) . fromIntegral . getFinite)
  poke p f = mapM_ (\i -> Foreign.pokeElemOff (Foreign.castPtr p) (fromIntegral i) (f i)) (finites @n)

roundUp :: Int -> Int -> Int
roundUp val base = (val + base - 1) `div` base * base

smdToTexturedSkeletonVertex :: SMD_Vertex -> TexturedSkeletonVertex
smdToTexturedSkeletonVertex v =
   TexturedSkeletonVertex
     (vPos v)
     (vNormal v)
     (vUV v)
     (case vWeights v of
        [(x,_),(y,_),(z,_),(w,_)] -> fromIntegral <$> a4 x y z w
        [(x,_),(y,_),(z,_)] -> fromIntegral <$> a4 x y z (-1)
        [(x,_),(y,_)] -> fromIntegral <$> a4 x y (-1) (-1)
        [(x,_)] -> fromIntegral <$> a4 x (-1) (-1) (-1)
        _ -> fromIntegral <$> a4 (-1) (-1) (-1) (-1))
     (case vWeights v of
        [(_,x),(_,y),(_,z),(_,w)] -> v4 x y z w
        [(_,x),(_,y),(_,z)] -> v4 x y z (-1)
        [(_,x),(_,y)] -> v4 x y (-1) (-1)
        [(_,x)] -> v4 x (-1) (-1) (-1)
        _ -> v4 (-1) (-1) (-1) (-1))

data TexturedSkeletonVertex = TexturedSkeletonVertex (Vec 3) (Vec 3) (Vec 2) (Array 4 GL.GLint) (Vec 4) deriving (Show)
instance Vertex TexturedSkeletonVertex where layout = [(3,GL.GL_FLOAT),(3,GL.GL_FLOAT),(2,GL.GL_FLOAT),(4,GL.GL_INT),(4,GL.GL_FLOAT)]
instance Data.Vector.Storable.Storable TexturedSkeletonVertex where
  sizeOf _ = roundUp (sum [size @(Vec 3), size @(Vec 3), size @(Vec 2), size @(Array 4 GL.GLint), size @(Vec 4)]) (alignment @TexturedSkeletonVertex)
  alignment _ = maximum [alignment @(Vec 3), alignment @(Vec 3), alignment @(Vec 2), alignment @(Array 4 GL.GLint), alignment @(Vec 4)]
  peek p =
    TexturedSkeletonVertex
       <$> Foreign.peekByteOff p (0                                                                           )
       <*> Foreign.peekByteOff p (0 + size @(Vec 3)                                                           )
       <*> Foreign.peekByteOff p (0 + size @(Vec 3) + size @(Vec 3)                                           )
       <*> Foreign.peekByteOff p (0 + size @(Vec 3) + size @(Vec 3) + size @(Vec 2)                           )
       <*> Foreign.peekByteOff p (0 + size @(Vec 3) + size @(Vec 3) + size @(Vec 2) + size @(Array 4 GL.GLint))
  poke p (TexturedSkeletonVertex a b c d e) = do
      Foreign.pokeByteOff p (0                                                                           ) a
      Foreign.pokeByteOff p (0 + size @(Vec 3)                                                           ) b
      Foreign.pokeByteOff p (0 + size @(Vec 3) + size @(Vec 3)                                           ) c
      Foreign.pokeByteOff p (0 + size @(Vec 3) + size @(Vec 3) + size @(Vec 2)                           ) d
      Foreign.pokeByteOff p (0 + size @(Vec 3) + size @(Vec 3) + size @(Vec 2) + size @(Array 4 GL.GLint)) e

-- ideally this should be some kind of running accumulator with alignments. lets see if it works as-is tho

data ColoredNormalVertex = ColoredNormalVertex (Vec 3) (Vec 3) (Vec 3) deriving (Show)
instance Vertex ColoredNormalVertex where layout = [(3,GL.GL_FLOAT),(3,GL.GL_FLOAT),(3,GL.GL_FLOAT)]
instance Data.Vector.Storable.Storable ColoredNormalVertex where
  sizeOf _ = roundUp (sum [size @(Vec 3), size @(Vec 3), size @(Vec 3)]) (alignment @ColoredNormalVertex)
  alignment _ = maximum [alignment @(Vec 3), alignment @(Vec 3), alignment @(Vec 3)]
  peek p =
    ColoredNormalVertex
       <$> Foreign.peekByteOff p (0 * size @GL.GLfloat)
       <*> Foreign.peekByteOff p (3 * size @GL.GLfloat)
       <*> Foreign.peekByteOff p (6 * size @GL.GLfloat)
  poke p (ColoredNormalVertex a b c) = do
      Foreign.pokeByteOff p (0 * size @GL.GLfloat) a
      Foreign.pokeByteOff p (3 * size @GL.GLfloat) b
      Foreign.pokeByteOff p (6 * size @GL.GLfloat) c


data PositionVertex = PositionVertex (Vec 4) deriving (Show)
instance Vertex PositionVertex where layout = [(4,GL.GL_FLOAT)]
instance Data.Vector.Storable.Storable PositionVertex where
  sizeOf _ = roundUp (sum [size @(Vec 4)]) (alignment @PositionVertex)
  alignment _ = maximum [alignment @(Vec 4)]
  peek p =
    PositionVertex
       <$> Foreign.peekByteOff p (0 * size @GL.GLfloat)
  poke p (PositionVertex a) = do
      Foreign.pokeByteOff p (0 * size @GL.GLfloat) a

-- not sure i can do this exactly. wonder if we could do a type level equivalent?
-- instance Vertex v => Data.Vector.Storable.Storable v where
--   sizeOf _ = _ (layout @v)
--   alignment _ = _ (layout @v)

-- i think in general what i want is a way to write something like
--
-- type Quux = [Vec 3, Int, Vec 3]
--
-- and then have all the layout automatically generated. vertex attribution, buffer uploads, allat
--
-- type like VertexData? or smth?
--
-- also should seperate this from particular file formats. vertex data should just be some basic thing

-- type Quux = '[Vec 3, Int, Vec 3]

-- data HList :: [*] -> * where
--   HNil  :: HList '[]
--   (:>)  :: x -> HList xs -> HList (x ': xs)




setShaderUniform :: SetUniform u => GL.GLuint -> String -> u -> IO ()
setShaderUniform programID uniformName value = do
  uniformLoc <- Foreign.C.String.withCString uniformName (GL.glGetUniformLocation programID)
  setUniform uniformLoc value

configureAttributes :: forall v.(Data.Vector.Storable.Storable v, Vertex v) => IO ()
configureAttributes =
  let stride = fromIntegral (size @v)
      go index offset [] = pure ()
      go index offset ((attributeDimension,scalarType):xs) = do
        GL.glEnableVertexAttribArray index
        case scalarType of
            GL.GL_FLOAT -> GL.glVertexAttribPointer index attributeDimension scalarType GL.GL_FALSE stride offset
            GL.GL_INT -> GL.glVertexAttribIPointer index attributeDimension scalarType stride offset
            _ -> error "BUFFER ERROR: we don't support that type yet"
        go (index + 1) (Foreign.plusPtr offset (fromIntegral (enumSize scalarType * attributeDimension))) xs
  in go 0 Foreign.nullPtr (layout @v)


class Index i where
  flatten :: i -> [GL.GLushort]
  indexDim :: Int

instance Integral i => Index (i,i,i) where
  flatten (a,b,c) = fromIntegral <$> [a,b,c]
  indexDim = 3

instance Integral i => Index (i,i) where
  flatten (a,b) = fromIntegral <$> [a,b]
  indexDim = 2

genBuffer :: forall v i.(Data.Vector.Storable.Storable v, Vertex v, Index i) => [i] -> [v] -> IO ObjMetadata
genBuffer indices verts = do
  vertexArrayObjectID <- Foreign.alloca $ \idPtr -> do
      GL.glGenVertexArrays 1 idPtr
      Foreign.peek idPtr
  GL.glBindVertexArray vertexArrayObjectID
  Foreign.alloca $ \indexBufferIDPtr -> do
      GL.glGenBuffers 1 indexBufferIDPtr
      indexBufferID <- Foreign.peek indexBufferIDPtr
      GL.glBindBuffer GL.GL_ELEMENT_ARRAY_BUFFER indexBufferID
      let indexBufferContent :: [GL.GLushort] = concatMap flatten indices
      let indexBufferSize = fromIntegral (size @GL.GLushort * length indexBufferContent)
      Foreign.withArray indexBufferContent (flip (GL.glBufferData GL.GL_ELEMENT_ARRAY_BUFFER indexBufferSize) GL.GL_STATIC_DRAW)
  Foreign.alloca $ \vertexBufferIDPtr -> do
      GL.glGenBuffers 1 vertexBufferIDPtr
      vertexBufferID <- Foreign.peek vertexBufferIDPtr
      GL.glBindBuffer GL.GL_ARRAY_BUFFER vertexBufferID
      let vertexBufferSize = fromIntegral (size @v * length verts)
      Foreign.withArray verts (flip (GL.glBufferData GL.GL_ARRAY_BUFFER vertexBufferSize) GL.GL_STATIC_DRAW)
  configureAttributes @v
  pure (ObjMetadata { vertexArrayID = vertexArrayObjectID , indexCount = fromIntegral (indexDim @i * length indices) })



--- MAIN GRAPHICS PROGRAM ------------------------------------------------------

size :: forall a.Foreign.Storable a => Int
size = Foreign.sizeOf (undefined :: a)
alignment :: forall a.Foreign.Storable a => Int
alignment = Foreign.alignment (undefined :: a)

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


main :: IO ()
main = do
  SDL.initializeAll
  window <- SDL.createWindow "App" (SDL.defaultWindow {SDL.windowGraphicsContext=SDL.OpenGLContext SDL.defaultOpenGL})
  SDL.glCreateContext window
  SDL.Raw.Event.setRelativeMouseMode True


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
                error errorMessage
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
                error errorMessage
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
                error errorMessage
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

  teapotObj <- do
    let filename = "resources/teapot.obj"
    src <- readFile filename
    case MP.parse pObj filename src of
      Left err -> error (show err)
      Right teapot -> pure teapot

  let initializeObject obj = genBuffer (faces obj) (fmap (\(v,n) -> ColoredNormalVertex (position v) (color v) n) (zipWith (,) (vertices obj) (normals obj)))
  
  cubeMetadata <- initializeObject cubeObj
  pyramidMetadata <- initializeObject pyramidObj
  planeMetadata <- initializeObject planeObj
  teapotMetadata <- initializeObject teapotObj

  textureShader <- initShader 
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
        out vec3 color;

        vec3 hsv2rgb(vec3 c) {
            vec3 K = vec3(1.0, 2.0/3.0, 1.0/3.0);
            vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - 3.0);
            return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
        }

        vec3 intToColor(int n) {
            float hue = float(n) / float(60);
            return hsv2rgb(vec3(hue, 0.8, 0.9));
        }
        

        void main() {
          //vec4 posePosition =
          //    weights[0] * bones[parentId[0]] * vertexPositionModelSpace
          //  + weights[1] * bones[parentId[1]] * vertexPositionModelSpace
          //  + weights[2] * bones[parentId[2]] * vertexPositionModelSpace
          //  + weights[3] * bones[parentId[3]] * vertexPositionModelSpace;
          vec4 posePosition = vec4(0);
          if (parentId[0] != -1) {
              posePosition += weights[0] * bones[parentId[0]] * vertexPositionModelSpace;
          }
          if (parentId[1] != -1) {
              posePosition += weights[1] * bones[parentId[1]] * vertexPositionModelSpace;
          }
          if (parentId[2] != -1) {
              posePosition += weights[2] * bones[parentId[2]] * vertexPositionModelSpace;
          }
          if (parentId[3] != -1) {
              posePosition += weights[3] * bones[parentId[3]] * vertexPositionModelSpace;
          }
          gl_Position = modelToProjectionMatrix * posePosition;
          normalWorldSpace = vec3(modelToWorldTransformMatrix * vec4(normalModelSpace,0));
          vertexPositionWorldSpace = vec3(modelToWorldTransformMatrix * vertexPositionModelSpace);
          fragment_uv = vertex_uv;
          //color = weights.xyz;
          color = intToColor(parentId[0]);
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
        in vec3 color;
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
          //fragmentColor = vec4(color,1);
        }
        """


  zigzagoon <- do
    let filename = "resources/Zigzagoon/Zigzagoon.SMD"
    src <- readFile filename
    case MP.parse pSMD filename src of
      Left err -> error (show err)
      Right model -> pure (smdscale 0.03 model)

  ziganim <- do
    let filename = "resources/Zigzagoon/anims/Action.smd"
    src <- readFile filename
    case MP.parse pSMD filename src of
      Left err -> error (show err)
      Right model -> pure (smdscale 0.03 model)


  -- we should figure out how to deform the skeleton first! then apply the basic principles from that simple situation to the model itself

  let readTex matName = do
          tex <- Codec.Picture.readImage ("resources/Zigzagoon/images/" ++ matName)
          case tex of
            Left err -> error err
            Right pic -> do
              let img = Codec.Picture.convertRGBA8 pic

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

              let smd = zigzagoon {triangles = filter (\v -> material v == matName) (triangles zigzagoon)}
              let smdTris = fmap (\n -> (3*n,3*n+1,3*n+2)) [0..length (triangles smd) - 1]
              let smdVerts = concatMap (\v -> let (a,b,c) = verts v in [a,b,c]) (triangles smd)
              md <- genBuffer smdTris (fmap smdToTexturedSkeletonVertex smdVerts)
              pure (textureID,md)

  matMap <- sequenceA (Data.Map.Internal.fromSet readTex (Data.Set.fromList (material <$> triangles zigzagoon)))

  skellyShader <- initShader
        """#version 430\r\n
        uniform mat4 modelToProjectionMatrix;
        in layout(location=0) vec4 vertexPositionModelSpace;
        void main() {
          gl_Position = modelToProjectionMatrix * vertexPositionModelSpace;
        }
        """

        """#version 430\r\n
        out vec4 fragmentColor;
        void main() {
          fragmentColor = vec4(1,1,1,1);
        }
        """

  skelly <- let pose = head $ tail (smdPoses ziganim) in genBuffer (roseEdges pose) (fmap (PositionVertex . snd) (flattenRose pose))


  --- MAIN LOOP ----------------------------------------------------------------
  
  let loop prevAppState animState = do


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
        when (windowWidth prevAppState /= windowWidth appState || windowHeight prevAppState /= windowHeight appState)
          (GL.glViewport 0 0 (fromIntegral (windowWidth appState)) (fromIntegral (windowHeight appState)))
        SDL.glSwapWindow window


        --- DRAW ---------------------------------------------------------------
        
        GL.glClear GL.GL_COLOR_BUFFER_BIT
        GL.glClear GL.GL_DEPTH_BUFFER_BIT

        let aspectRatio = fromIntegral (windowWidth appState) / fromIntegral (windowHeight appState)
        let worldToView = worldToViewMatrix (cameraPosition appState . weaken) (viewDirection appState . weaken)
        let projectionMatrix = projection 60 aspectRatio 0.1 50
        let toScreenspace t = projectionMatrix . worldToView . t
        let drawTriangulation shader obj transform = do
              GL.glBindVertexArray (vertexArrayID obj)
              setShaderUniform shader "modelToWorldTransformMatrix" transform
              setShaderUniform shader "modelToProjectionMatrix" (toScreenspace transform)
              GL.glDrawElements GL.GL_TRIANGLES (indexCount obj) GL.GL_UNSIGNED_SHORT Foreign.nullPtr
        let lightPosition = v3 3 0 2

        GL.glUseProgram basicShader
        setShaderUniform basicShader "ambientLight" (v4 0.3 0.3 0.3 1)
        setShaderUniform basicShader "lightPosition" lightPosition
        setShaderUniform basicShader "eyePosition" (cameraPosition appState . weaken)

        drawTriangulation basicShader cubeMetadata (translate (v3 1 0 (-4)) . rotation 45 (v3 1 1 0))
        drawTriangulation basicShader cubeMetadata (translate (v3 (-1) 0 (-4)) . rotation 45 (v3 1 1 1))
        drawTriangulation basicShader pyramidMetadata (translate (v3 0 0 (-2)))
        drawTriangulation basicShader planeMetadata (translate (v3 0 (-2) (-4)))
        drawTriangulation basicShader teapotMetadata (translate (v3 (3) 1 (-8)))

        let zigTransform = (translate (v3 (-2) (-2) (-1)) . rotation 45 (v3 0 1 0) . rotation 90 (v3 (-1) 0 0))        

        GL.glUseProgram textureShader


        let zigpose = transformToMat <$> pose zigzagoon (head (skeleton zigzagoon)) (head animState)
        bonesloc <- Foreign.C.String.withCString "bones" (GL.glGetUniformLocation textureShader)
        Foreign.withArray zigpose (GL.glUniformMatrix4fv bonesloc (fromIntegral $ length zigpose) 1 . Foreign.castPtr)
        setShaderUniform textureShader "ambientLight" (v4 0.3 0.3 0.3 1)
        setShaderUniform textureShader "lightPosition" lightPosition
        setShaderUniform textureShader "eyePosition" (cameraPosition appState . weaken)
        mapM
           (\(textureID,zigMetadata) -> do
                GL.glActiveTexture GL.GL_TEXTURE0
                GL.glBindTexture GL.GL_TEXTURE_2D textureID
                Foreign.C.String.withCString "texture" (GL.glGetUniformLocation textureShader) >>= flip GL.glUniform1i 0
                drawTriangulation textureShader zigMetadata zigTransform)
           matMap

        GL.glClear GL.GL_DEPTH_BUFFER_BIT
        GL.glUseProgram skellyShader
        GL.glBindVertexArray (vertexArrayID skelly)
        setShaderUniform skellyShader "modelToProjectionMatrix" (toScreenspace zigTransform)
        -- GL.glDrawElements GL.GL_LINES (indexCount skelly) GL.GL_UNSIGNED_SHORT Foreign.nullPtr
        

        unless (timeToQuit appState) (loop appState (tail animState))

  loop initialAppState (cycle (skeleton ziganim))


  --- CLEANUP ------------------------------------------------------------------

  GL.glUseProgram 0
  GL.glDeleteProgram basicShader
  SDL.destroyWindow window
