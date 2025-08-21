
A GLSL PROGRAM!!!

has an input type, characterized by something like a tuple of vectors
and an output type, similarly characterized

i also understand that some shaders have certain parameters built in. inputs and outputs might be implicit




concept: standard basis vectors for simple arithmetic to define vector literals

e.g. instead of writing

  v4 0 1 0 1

if we have the standard bases i j k l this would be equal to

  j + l

a lot of things would become very simple this way. any sparse literals are simple. if we also have constant vectors we can write them like

  j - 2 * k

and voila we get a nice vector representing 0 1 -2 0

perhaps we have canonical names for these bases up to 4d and then we have generic position vectors. no idea what to call them, but they'd basically be something like

  basis 3

representing a vector 0 0 0 1 ... where the elipses represent any length of zeros.
ybe this should really be a type parameter then? basis @3 represents a vector of length at least 4.


maybe the right way to describe meshes is via their faces? create triangles, link their faces together, let the system figure out orientations and all that. still need to describe vertices, but we can do those as intersection operators. same with edges. e.g. for triangles A,B, and C we have vertex ABC and edges AB and AC and BC. instead of vertex up to triangle, we have triangle down to vert
in practice what would this look like? for a triangular pyramid we have four faces, A,B,C,D.

we link those so something like
  sharedEdge A B
  sharedEdge A C
  sharedEdge A D
  sharedEdge B C
  sharedEdge B D
  sharedEdge C D

then points. we define colors and locations like
  vertex A B C {position = ..., color = ...}
  vertex A B D {position = ..., color = ...}
  vertex A C D {position = ..., color = ...}
  vertex B C D {position = ..., color = ...}

yeah this seems a bit more natural. define triangles, then define their derived values.

also for defining vertices we could have a special boundary value so expressions like vertex A B Edge make sense





wonder if i should do more world model stuff in 3d, not 4d. not affine coords. cuz i can just do translations manually and all that. no reason to complicate things right? we can just scale things up to 4d at the end.




using effect to generate models could be swell. idea is that you generate points by some function that generates fresh vars, give the points data like position and color, then link them into triangles. an interpreter generates the associated vertex and index lists to upload to the GPU automatically 

particularly useful for keeping winding orders of vertices appropriate. if you have index lists, remember to sort every triangle ordering so it is cyclic!

think there might be some useful linear constraints here somehow. each point can be used a bunch of times, but each line must be used exactly 2 times for a figure to be closed. and each triangle eats 3 different lines. so it seems like there could be some useful arithmetic here. linear logic type stuff



conceptually i wonder if there would be issues to having an API that has a single graphics buffer that we manually segment. we could in theory just have one shared buffer for all manipulations. seems like it could be simpler in principle to think in these terms




weird shader thing: locations for matrices take up multiple slots. mat4 at location 2 is really at location 2,3,4,5. think the rule is that locations are vectors of 4 numbers ALWAYS! so even if you send just a float, the remaining 3 slots are filled with zeros or something

you CAN use the gl linker + glGetAttribLocation to write your shaders like

in vec3 position;
in mat4 fullTransformMatrix
in vec3 vertexColor;

and it will generate locations that are retreivable with glGetAttribLocation. this might be the easiest method to deal w/ shaders


shaders really gotta be defined by their interface. you can send data to a shader, but that is entirely dependent on the interface of the shader. sending data should require some compatability checks





so for buffer allocation, we are doing basically arena memory management right? seems like we could benefit from a monadic approach. add things to the buffer queue, get a promise of a position. calculate size of buffer at the end once all queued elements are committed





wow so it is a huge PITA to make projection and rotation matrices. helps a lot to think in terms of vector transformations and then turn those into matrices using a helper function. also probably good to do some validation somehow of the basic operations that convert whatever matrix representation i've got into a flat array. ALSO really important to keep in mind what dimensions correspond to rows and which ones are for columns and how that relationship works. best to draw things out tbh, but it would be nice to have a mental shortcut bc i get lost easy

also i think if we go for a lot of defns of linear transforms instead of matrices (the diff being type: Vec n -> Vec m vs Mat m n) we can do functional composition and things. then maybe hopefully the haskell compiler can do optimizations? and only at the end do we need to reify a linear transform into a literal grid of numbers


i do really want a way to specify transformations more declaratively. "yo i want the affine transformation that sends this corner of the frustum to that corner of the viewing box, this other frustum corner to that other viewing box corner, etc" and then for the computer to figure out what i mean


relevant coordinate transformations
- model to world
- world to camera
- camera to projection
can't help but want this to be more implicit. i wanna arrange things by their relations to other things, the computations of these coordinate transforms should be implicit. esp considering how the viewing box has such nice coordinates; front and back, left and right, top and bottom. very intuitive, but then we have to trace transformations back and forth. ideally these transforms should be automatic, ya know?


maybe long term i wanna do character rigging? this + real time data from a camera to build some kind of virtual avatar system. would be fun. also afaict rigging is actually kind of intuitive? you define a function from vertices to a skeleton; the skeleton defines some transformations, the vertices inheret the transformation of the skeleton point they map to (could be a weighted mapping, in which case we do some kind of interpolation)


idk why but the 3d model is starting to make sense to me. basically we have a small room, seen from the front, w/ all coordinates from -1 to 1. we want to transport everything into this room. that's basically the essence of rendering triangles.


im intending to keep the shader code in the main file bc i wanna be able to programatically generate it. so no dispatch to external files, i want a dsl


oohh ya know what interpolation makes sense for the depth buffer. thats reasonable



forget if i said this already but using algebraic graphs w/ class to represent meshes could be kind of fun. tho that's not quite right, we need triangulations. idk seems like some kind of topology would be helpful. maybe some of my ideas from other things; we have something like a function of vertices that generates a valid graph. e.g.

\vertex1 vertex2 vertex3 -> makeTriangle vertex1 vertex2 vertex3

then something else handles making sure everything lines up correctly


tbh encoding state might be tricky. we might be able to get away with some super complicated type class tho.


vshader :: (Vector 2, Vector 3) -> VertexData (Vector 3)
vshader (position, vertexColor) =
  VertexData
    { gl_position = vec4 (x position) (y position) 0 1
    , other = vertexColor
    }

notes:

there seems to be a pretty consistent pattern of prepare some data, prepare gpu, send data. seems like this whole thing could be encapsulated

type coordination seems like it could be automated. like for uploading some data: if we have something like a [GL.GLfloat] we probably know that it should be interpreted using the GL.GL_FLOAT whatever enum thing. and the sizeOf op. that can also be automated. whole class o type errors that are avoided if ya do this

still wanna use linear types for ensuring things get configured in the correct order, or that we need a certificate proving correct configuration, but that might be better for api design and not hacking on internal projects

seems like representing meshes w/ point arrays and then triangles to connect things is brittle, and somehow i feel like you could track this better at the type level

it would be nice to have the "raw" data but with type guarantees. like too much abstraction is bad, but if data is expected to be grouped certain ways i wanna have some way to say "yeah the verts array should be a multiple of 2" or whatever

lots of things that opengl does seem like they should be operations performed by traditional languages on the CPU. array processing sorts of things ya know

would be nice to track value ranges. it's nice that colors and positions are mostly kept in range -1.0 to 1.0 or 0.0 to 1.0. but explicit. explicit at the type level pls

also coordination problems. send data on one end, recieve on the other. would be nice to have the shaders linked to the CPU code so if we change one we know where we gotta change the other. i think a lot of this stuff is just my desire to be explicit about recording our data model and having the type system help us with this. wonder how much type level nats would be useful

glad im sticking to the raw api. easy to cross reference and follow along w/ tutorials. also fun to learn about the ffi

SHADER ERRORS DON'T GET REPORTED. MASSIVE OPPORTUNITY TO MAKE SHADERS EASIER TO USE W/ A DSL. can also do the whatever it's call function shadowing thing too. also can be more explicit about inputs and outputs. would be VERY useful to explicitly parameterize shaders in terms of these inputs and outputs. tbh shaders really should be functions of inputs and outputs huh?

triangles feel like such a weird primitive. the way the shaders interpolate between vertices is also very weird. idk maybe nice for really high fidelity vertices? not sure this behavior makes sense as a default ever



video describes an OOP version of the opengl api. code looks like

Shader* myShader = new Shader(GL_VERTEX_SHADER);
myShader->setShaderSource(vertexShaderCode);
myShader->compile();

Program* myProgram = new Program();
myProgram->attachShader(myShader);
myProgram->link();
myProgram->use();

id really like to have something like this but w/ a type level state machine to ensure differnt invariants are held. might have to do the, uhh, whats it call, sequence enumeration to determine what the state machine actually is. yeah that would be an interesting exercise. enumerate valid sequences of opengl calls. but regardless i wanna have this look something like

do
myShader <- newVertexShader
myShader <- setShaderSource vertexShaderCode myShader
myShader <- compile myShader

myProgram <- newProgram
myProgram <- attachShader myShader myProgram
myProgram <- link myProgram
myProgram <- use myProgram




-- open :: () %1 -> Int
-- open () = 1

-- modify :: Int %1 -> Int
-- modify x = x+1

-- close :: Int %1 -> ()
-- close = consume

-- (>>>) :: (a %1 -> b) -> (b %1 -> c) -> (a %1 -> c)
-- f >>> g = g . f

-- pipeline = open >>> modify >>> close

-- -- pipeline =
-- --   let x0 = open ()
-- --       x1 = modify x0
-- --    in close x1

-- test :: a %1 -> a
-- test x = let y = x in y
-- data Cert_WindowSupportsOpenGL = Cert_WindowSupportsOpenGL

