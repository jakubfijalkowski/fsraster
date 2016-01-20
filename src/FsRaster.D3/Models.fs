module FsRaster.D3.Models

open FsRaster.D3.Math

type Model =
    {
        Vertices : Vector4 array;
        Triangles : (int * int * int) array;
    }

let sampleTriangle =
    {
        Vertices = [| vec4 0.0 0.0 0.0 1.0; vec4 0.0 0.5 0.0 1.0; vec4 0.5 0.0 0.0 1.0 |];
        Triangles = [| 0, 1, 2 |]
    }