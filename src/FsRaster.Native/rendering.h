#pragma once

#include <stdbool.h>
#include "common.h"

typedef struct
{
    double x;
    double y;
    double z;
} Vector3;

typedef struct
{
    Vector3 v1;
    Vector3 v2;
    Vector3 v3;
    int c1;
    int c2;
    int c3;
} RenderTriangle;

#define GETR(c) (c & 0x00ff0000 >> 16)
#define GETG(c) (c & 0x0000ff00 >> 8)
#define GETB(c) (c & 0x000000ff)
#define TORGB(r, g, b) (0xff000000 | (r & 0xff << 16) | (g & 0xff << 8) | (b & 0xff))
#define CLAMP(v, m) (min(max(0, v), m))

FSRASTEREXPORT void FSRASTERCALLCONV render_triangles(
    int width, int height, bool zBuffer, int *screen,
    RenderTriangle *triangles, int count);
