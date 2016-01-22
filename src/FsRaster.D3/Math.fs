module FsRaster.D3.Math

open System.Diagnostics

// Vector is represented as a column, matrix is row-major

let inline degToRad d = d * System.Math.PI / 180.0
let inline radToDeg r = r * 180.0 / System.Math.PI

[<DebuggerDisplay("X = {X}; Y = {Y}; Z = {Z}")>]
type Vector3 =
    { X : double; Y : double; Z : double }

    member x.Length =
        System.Math.Sqrt(x.X * x.X + x.Y * x.Y + x.Z * x.Z)

    member x.Normalized =
        let len = x.Length
        { X = x.X / len; Y = x.Y / len; Z = x.Z / len }

    static member (*) (a : double, b : Vector3) =
        { X = a * b.X; Y = a * b.Y; Z = a * b.Z }

    static member (*) (b : Vector3, a : double) =
        { X = a * b.X; Y = a * b.Y; Z = a * b.Z }

    static member (/) (b : Vector3, a : double) =
        { X = b.X / a; Y = b.Y / a; Z = b.Z / a }

    static member (+) (a : Vector3, b : Vector3) =
        { X = a.X + b.X; Y = a.Y + b.Y; Z = a.Z + b.Z }

    static member (-) (a : Vector3, b : Vector3) =
        { X = a.X - b.X; Y = a.Y - b.Y; Z = a.Z - b.Z }

    static member (~-) (a : Vector3) =
        { X = -a.X; Y = -a.Y; Z = -a.Z }

[<DebuggerDisplay("X = {X}; Y = {Y}; Z = {Z}; W = {W}")>]
type Vector4 =
    { X : double; Y : double; Z : double; W : double }

    member x.Normalized : Vector4 =
        { X = x.X / x.W; Y = x.Y / x.W; Z = x.Z / x.W; W = 1.0 }

    static member (+) (a : Vector4, b : Vector4) =
        let a' = a.Normalized
        let b' = b.Normalized
        { X = a'.X + b'.X; Y = a'.Y + b'.Y; Z = a'.Z + b'.Z; W = 1.0 }

let inline vec3 x y z : Vector3 = { X = x; Y = y; Z = z }
let inline vec4 x y z w : Vector4 = { X = x; Y = y; Z = z; W = w }

let vec3Zero = vec3 0.0 0.0 0.0
let vec4Zero = vec4 0.0 0.0 0.0 1.0

let inline toVec4 (a : Vector3) = vec4 a.X a.Y a.Z 1.0
let inline toVec3 (a : Vector4) : Vector3 =
    let n = a.Normalized
    { X = n.X; Y = n.Y; Z = n.Z }

let inline dot3 (a : Vector3) (b : Vector3) : double =
    let x = a.X * b.X
    let y = a.Y * b.Y
    let z = a.Z * b.Z
    x + y + z

let inline cross3 (a : Vector3) (b : Vector3) : Vector3 =
    let x = a.Y * b.Z - a.Z * b.Y
    let y = a.Z * b.X - a.X * b.Z
    let z = a.X * b.Y - a.Y * b.X
    { X = x; Y = y; Z = z }

let inline computeNormal3 v1 v2 v3 = cross3 (v2 - v1) (v3 - v1)
let inline computeNormal4 v1 v2 v3 = computeNormal3 (toVec3 v1) (toVec3 v2) (toVec3 v3)

let inline vec3Add (v1 : Vector3) (v2 : Vector3) : Vector3 = { X = v1.X + v2.X; Y = v1.Y + v2.Y; Z = v1.Z + v2.Z }

type Matrix4 =
    {
        M11 : double; M12 : double; M13 : double; M14 : double;
        M21 : double; M22 : double; M23 : double; M24 : double;
        M31 : double; M32 : double; M33 : double; M34 : double;
        M41 : double; M42 : double; M43 : double; M44 : double
    }

    static member (*) (mat : Matrix4, vec : Vector4) =
        let x = mat.M11 * vec.X + mat.M12 * vec.Y + mat.M13 * vec.Z + mat.M14 * vec.W
        let y = mat.M21 * vec.X + mat.M22 * vec.Y + mat.M23 * vec.Z + mat.M24 * vec.W
        let z = mat.M31 * vec.X + mat.M32 * vec.Y + mat.M33 * vec.Z + mat.M34 * vec.W
        let w = mat.M41 * vec.X + mat.M42 * vec.Y + mat.M43 * vec.Z + mat.M44 * vec.W
        { X = x; Y = y; Z = z; W = w }

    static member (*) (a : Matrix4, b : Matrix4) =
        // Lovin' it!
        let m11 = a.M11 * b.M11 + a.M12 * b.M21 + a.M13 * b.M31 + a.M14 * b.M41
        let m21 = a.M21 * b.M11 + a.M22 * b.M21 + a.M23 * b.M31 + a.M24 * b.M41
        let m31 = a.M31 * b.M11 + a.M32 * b.M21 + a.M33 * b.M31 + a.M34 * b.M41
        let m41 = a.M41 * b.M11 + a.M42 * b.M21 + a.M43 * b.M31 + a.M44 * b.M41

        let m12 = a.M11 * b.M12 + a.M12 * b.M22 + a.M13 * b.M32 + a.M14 * b.M42
        let m22 = a.M21 * b.M12 + a.M22 * b.M22 + a.M23 * b.M32 + a.M24 * b.M42
        let m32 = a.M31 * b.M12 + a.M32 * b.M22 + a.M33 * b.M32 + a.M34 * b.M42
        let m42 = a.M41 * b.M12 + a.M42 * b.M22 + a.M43 * b.M32 + a.M44 * b.M42

        let m13 = a.M11 * b.M13 + a.M12 * b.M23 + a.M13 * b.M33 + a.M14 * b.M43
        let m23 = a.M21 * b.M13 + a.M22 * b.M23 + a.M23 * b.M33 + a.M24 * b.M43
        let m33 = a.M31 * b.M13 + a.M32 * b.M23 + a.M33 * b.M33 + a.M34 * b.M43
        let m43 = a.M41 * b.M13 + a.M42 * b.M23 + a.M43 * b.M33 + a.M44 * b.M43

        let m14 = a.M11 * b.M14 + a.M12 * b.M24 + a.M13 * b.M34 + a.M14 * b.M44
        let m24 = a.M21 * b.M14 + a.M22 * b.M24 + a.M23 * b.M34 + a.M24 * b.M44
        let m34 = a.M31 * b.M14 + a.M32 * b.M24 + a.M33 * b.M34 + a.M34 * b.M44
        let m44 = a.M41 * b.M14 + a.M42 * b.M24 + a.M43 * b.M34 + a.M44 * b.M44
        {
            M11 = m11; M12 = m12; M13 = m13; M14 = m14;
            M21 = m21; M22 = m22; M23 = m23; M24 = m24;
            M31 = m31; M32 = m32; M33 = m33; M34 = m34;
            M41 = m41; M42 = m42; M43 = m43; M44 = m44
        }

let matZero =
    {
        M11 = 0.0; M12 = 0.0; M13 = 0.0; M14 = 0.0;
        M21 = 0.0; M22 = 0.0; M23 = 0.0; M24 = 0.0;
        M31 = 0.0; M32 = 0.0; M33 = 0.0; M34 = 0.0;
        M41 = 0.0; M42 = 0.0; M43 = 0.0; M44 = 0.0
    }
let matIdentity =
    {
        M11 = 1.0; M12 = 0.0; M13 = 0.0; M14 = 0.0;
        M21 = 0.0; M22 = 1.0; M23 = 0.0; M24 = 0.0;
        M31 = 0.0; M32 = 0.0; M33 = 1.0; M34 = 0.0;
        M41 = 0.0; M42 = 0.0; M43 = 0.0; M44 = 1.0
    }

let inline matTranspose m =
    {
        M11 = m.M11; M12 = m.M21; M13 = m.M31; M14 = m.M41;
        M21 = m.M12; M22 = m.M22; M23 = m.M32; M24 = m.M42;
        M31 = m.M13; M32 = m.M23; M33 = m.M33; M34 = m.M43;
        M41 = m.M14; M42 = m.M24; M43 = m.M34; M44 = m.M44
    }

let inline matTranslate x y z = 
    {
        M11 = 1.0; M12 = 0.0; M13 = 0.0; M14 = x;
        M21 = 0.0; M22 = 1.0; M23 = 0.0; M24 = y;
        M31 = 0.0; M32 = 0.0; M33 = 1.0; M34 = z;
        M41 = 0.0; M42 = 0.0; M43 = 0.0; M44 = 1.0
    }
let inline matTranslateVec (v : Vector3) = 
    {
        M11 = 1.0; M12 = 0.0; M13 = 0.0; M14 = v.X;
        M21 = 0.0; M22 = 1.0; M23 = 0.0; M24 = v.Y;
        M31 = 0.0; M32 = 0.0; M33 = 1.0; M34 = v.Z;
        M41 = 0.0; M42 = 0.0; M43 = 0.0; M44 = 1.0
    }

let inline matScale x y z =
    {
        M11 = x  ; M12 = 0.0; M13 = 0.0; M14 = 0.0;
        M21 = 0.0; M22 = y  ; M23 = 0.0; M24 = 0.0;
        M31 = 0.0; M32 = 0.0; M33 = z  ; M34 = 0.0;
        M41 = 0.0; M42 = 0.0; M43 = 0.0; M44 = 1.0
    }
let inline matRotX theta =
    let s = System.Math.Sin theta
    let c = System.Math.Cos theta
    {
        M11 = 1.0; M12 = 0.0; M13 = 0.0; M14 = 0.0;
        M21 = 0.0; M22 = c  ; M23 = -s ; M24 = 0.0;
        M31 = 0.0; M32 = s  ; M33 = c  ; M34 = 0.0;
        M41 = 0.0; M42 = 0.0; M43 = 0.0; M44 = 1.0
    }
let inline matRotY theta =
    let s = System.Math.Sin theta
    let c = System.Math.Cos theta
    {
        M11 = c  ; M12 = 0.0; M13 = s  ; M14 = 0.0;
        M21 = 0.0; M22 = 1.0; M23 = 0.0; M24 = 0.0;
        M31 = -s ; M32 = 0.0; M33 = c  ; M34 = 0.0;
        M41 = 0.0; M42 = 0.0; M43 = 0.0; M44 = 1.0
    }
let inline matRotZ theta =
    let s = System.Math.Sin theta
    let c = System.Math.Cos theta
    {
        M11 = c  ; M12 = -s ; M13 = 0.0; M14 = 0.0;
        M21 = s  ; M22 = c  ; M23 = 0.0; M24 = 0.0;
        M31 = 0.0; M32 = 0.0; M33 = 1.0; M34 = 0.0;
        M41 = 0.0; M42 = 0.0; M43 = 0.0; M44 = 1.0
    }

let matLookAt (eye : Vector3) at up =
    let zax = (eye - at).Normalized
    let xax = (cross3 up zax).Normalized
    let yax = cross3 zax xax
    let dx = -(dot3 xax eye)
    let dy = -(dot3 yax eye)
    let dz = -(dot3 zax eye)
    {
        M11 = xax.X; M12 = xax.Y; M13 = xax.Z; M14 = dx;
        M21 = yax.X; M22 = yax.Y; M23 = yax.Z; M24 = dy;
        M31 = zax.X; M32 = zax.Y; M33 = zax.Z; M34 = dz;
        M41 = 0.0  ; M42 = 0.0  ; M43 = 0.0  ; M44 = 1.0
    }

let matInvLookAt (eye : Vector3) at up =
    let zax = (eye - at).Normalized
    let xax = (cross3 up zax).Normalized
    let yax = cross3 zax xax
    let dx = dot3 xax eye
    let dy = dot3 yax eye
    let dz = dot3 zax eye
    {
        M11 = xax.X; M12 = yax.X; M13 = zax.X; M14 = dx;
        M21 = xax.Y; M22 = yax.Y; M23 = zax.Y; M24 = dy;
        M31 = xax.Z; M32 = yax.Z; M33 = zax.Z; M34 = dz;
        M41 = 0.0  ; M42 = 0.0  ; M43 = 0.0  ; M44 = 1.0
    }

let matProjection fovy aspect near far =
    let f = 1.0 / System.Math.Tan((degToRad fovy) / 2.0)
    let f1 = (far + near) / (near - far)
    let f2 = (2.0 * far * near) / (near - far)
    {
        M11 = f / aspect; M12 = 0.0; M13 = 0.0 ; M14 = 0.0;
        M21 = 0.0       ; M22 = f  ; M23 = 0.0 ; M24 = 0.0;
        M31 = 0.0       ; M32 = 0.0; M33 = f1  ; M34 = f2 ;
        M41 = 0.0       ; M42 = 0.0; M43 = -1.0; M44 = 0.0
    }

// Source - http://www.gamedev.net/topic/478055-perspective-projection-matrix/
let matInvProjection fovy aspect near far =
    let f = 1.0 / System.Math.Tan((degToRad fovy) / 2.0)
    let f1 = (near - far) / (2.0 * far * near)
    let f2 = (near + far) / (2.0 * far * near)
    {
        M11 = aspect / f; M12 = 0.0     ; M13 = 0.0; M14 =  0.0 ;
        M21 = 0.0       ; M22 = 1.0 / f ; M23 = 0.0; M24 =  0.0 ;
        M31 = 0.0       ; M32 = 0.0     ; M33 = 0.0; M34 = -1.0;
        M41 = 0.0       ; M42 = 0.0     ; M43 = f1 ; M44 = f2
    }

type Quaternion =
    { A : double; B : double; C : double; D : double }

    member q.Norm =
        System.Math.Sqrt(q.A * q.A + q.B * q.B + q.C * q.C + q.D * q.D)

    member q.Conjugate =
        { A = q.A; B = -q.B; C = -q.C; D = -q.D }

    member q.Reciprocal = q.Conjugate / q.Norm

    member q.Unit = q / q.Norm

    static member (~-) (q : Quaternion) =
        { A = -q.A; B = -q.B; C = -q.C; D = -q.D }

    static member (+) (a : Quaternion, b : Quaternion) =
        { A = a.A + b.A; B = a.B + b.B; C = a.C + b.C; D = a.D + b.D }

    static member (*) (q : Quaternion, c : double) =
        { A = q.A * c; B = q.B * c; C = q.C * c; D = q.D * c }

    static member (*) (c : double, q : Quaternion) =
        { A = q.A * c; B = q.B * c; C = q.C * c; D = q.D * c }

    static member (*) (a : Quaternion, b : Quaternion) =
        { A = a.A * b.A - a.B * b.B - a.C * b.C - a.D * b.D;
          B = a.A * b.B + a.B * b.A + a.C * b.D - a.D * b.C;
          C = a.A * b.C - a.B * b.D + a.C * b.A + a.D * b.B;
          D = a.A * b.D + a.B * b.C - a.C * b.B + a.D * b.A }

    static member (/) (q : Quaternion, c : double) =
        { A = q.A / c; B = q.B / c; C = q.C / c; D = q.D / c }

let inline quat w (v : Vector3) = { A = w; B = v.X; C = v.Y; D = v.Z }
let inline vecAsQuat v = quat 0.0 v
let inline quatAsVec (q : Quaternion) = vec3 q.B q.C q.D

let quatRot theta (axis : Vector3) =
    let c = System.Math.Cos(theta / 2.0)
    let s = System.Math.Sin(theta / 2.0)
    quat c (s * axis.Normalized)

let vecRotate theta axis v =
    let q = quatRot theta axis
    let q' = q.Reciprocal
    let v' = vecAsQuat v
    quatAsVec (q * v' * q')