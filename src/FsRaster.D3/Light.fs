module FsRaster.D3.Light

open FsRaster.D3.Math
open FsRaster.D3.Camera

type Material =
    {
        SpecularCoeff : double;
        DiffuseCoeff : double;
        AmbientCoeff : double;
        Shininess : double
    }

let defaultMaterial = { SpecularCoeff = 0.0; DiffuseCoeff = 0.5; AmbientCoeff = 0.5; Shininess = 50.0 }

type Light =
    {
        Position : Vector3;
        Ambient : int;
        Diffuse : int;
        Specular : int
    }

let inline makeLight pos a d s = { Position = pos; Ambient = a; Diffuse = d; Specular = s }
let inline lightToCamera (light : Light) = makeCamera light.Position vec3Zero
let inline updateLight (light : Light) (cam : Camera) = { light with Position = cam.Position }