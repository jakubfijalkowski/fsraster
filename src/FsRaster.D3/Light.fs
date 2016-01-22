module FsRaster.D3.Light

open FsRaster
open FsRaster.D3.Math
open FsRaster.D3.Camera

type Material =
    {
        SpecularCoeff : double;
        DiffuseCoeff : double;
        AmbientCoeff : double;
        Shininess : double
    }

let defaultMaterial = { SpecularCoeff = 0.8; DiffuseCoeff = 0.5; AmbientCoeff = 0.3; Shininess = 2.0 }

type Light =
    {
        Position : Vector3;
        AmbientR : double;
        AmbientG : double;
        AmbientB : double;
        DiffuseR : double;
        DiffuseG : double;
        DiffuseB : double;
        SpecularR : double;
        SpecularG : double;
        SpecularB : double
    }

let inline makeLight pos a d s =
    {
        Position = pos;
        AmbientR = double (Colors.getR a);
        AmbientG = double (Colors.getG a);
        AmbientB = double (Colors.getB a);
        DiffuseR = double (Colors.getR d);
        DiffuseG = double (Colors.getG d);
        DiffuseB = double (Colors.getB d);
        SpecularR = double (Colors.getR s);
        SpecularG = double (Colors.getG s);
        SpecularB = double (Colors.getB s)
    }

let inline lightToCamera (light : Light) = makeCamera light.Position vec3Zero
let inline updateLight (light : Light) (cam : Camera) = { light with Position = cam.Position }

let defaultLight = makeLight (vec3 0.0 2.0 0.0) 0xff0000ff 0xff00ff00 0xffffffff