FsRaster
--------

Continuing my decision that I will publish every bigger project done for my studies, I present you **FsRaster**, a F# (well, mostly) app developed for my Computer Graphics I course.

It consists of two main projects:

 1. FsRaster/FsRaster.UI - 2D rasterizer with image processing abilities (F# with a little bit of C# for color picker controls),
 2. FsRaster.D3 - a 3D renderer (F# and C).
 
It isn't a fully-featured 2D/3D renderer, as I've done only the things required for the course.
I've tried to stick with the functional way of programming, except where the performance matters. There, I use nasty pointers, SSE intrinsics and mutable state, but unfortunately I haven't found any other way to make it efficient.

So, to sum up, 2D-part has:

 1. Rendering of:
    * Lines - Bresenham algorithm,
    * Circles - Bresenham algorithm,
    * Polylines/polygons,
    * Filled polygons - Active edge algorithm using edge set.
 2. Filling figures with textures (using a first vertex as an origin),
 3. An octree algorithm (reduces to *at most* k colors),
 4. Filling (bucket) - 4-connected and 8-connected,
 5. Extracting Y/U/V plane (I _think_ it is correct),
 6. Clipping using Sutherland-Hodgman algorithm,
 7. Selecting color using RGB/HSV/xyY color pickers,
 8. Filtering (convolution and function filters),
 9. Scaling, rotating and gamma correction (a little bit rough, but works).
 
3D part has:

 1. Rendering basic OFF files,
 2. Wireframe/filled rendering (using random colors ;) ),
 3. Basic frustum culling, backface culling and a z-buffer support,
 4. Phong lightning and Gouraud shadind,
 5. A single light source (that you can move),
 6. (almost) Free camera.
 
The 3D part has many bugs (gaps between triangles being the most visible), but I wasn't developing a fully-featured renderer and this is only a proof-of-concept. I was trying to make it very performant, so the core part (rendering transformed triangles) is using C with SSE intrinsics. I don't think this is the best way to do it, but it works as expected, it is quite efficient and is developed solely by me in it's entirety (that's why it isn't the best approach ;) ).

## How to use

FsRaster.UI is quite simple to use - use a mouse to add figures, select them (left click) and move (left click and mouse move). Use a 'F' key to finish a multiline figure (a polyline, a polygon and a filled polygon). Everything else may be changed using the GUI.

FsRaster.D3 uses WASD to move the camera forward/left/backward/right, E to move in a *real up* direction, Q to move in a *real down* direction, Space to move in a *up direction* (Y coordinate) and LCtrl to move in *down direction* (Y coordinate). It also uses numpad to rotate the camera (left/right, top/down). Everything else may be changed using the GUI.

## The good parts

I don't think this is the best project of mine, but this is my **first** project in F#, my **first** attempt at 2D/3D rendering and the whole process of development was in crunch time, so I'm quite happy with the result.

There is one part that I find the most enjoyable - the `FsRaster.D3.Math` module. It supports 3D vectors (plain 3D and using homogeneous coordinates), 4x4 matrices and quaternions. It is able to produce `lookAt`, projection, translate, rotate and scale matrices, supports matrix, matrix/vector, vector/quaternion multiplication and most of the basic operations on them. This is the place to start with the project.

## Acknowledgements

OFF file format and sample modules are from Mr. Lutz Kettner from Max Planck Society.
Website: http://people.mpi-inf.mpg.de/~kettner/proj/obj3d/ .

CIE XYZ color matching functions are from Coulour & Vision Research Labs. Website: http://cvrl.ioo.ucl.ac.uk/ .

## License

The project is under the MIT License, except for the OFF files and the CIE XYZ color matching functions.