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

#define GETR(c) ((c >> 16) & 0xff)
#define GETG(c) ((c >> 8) & 0xff)
#define GETB(c) (c & 0xff)
#define TORGB(r, g, b) (0xff000000 | (r << 16) | (g << 8) | b)
#define TORGBD(r, g, b) TORGB((int)r, (int)g, (int)b)
#define CLAMP(v, m) (min(max(0, v), m))

FSRASTEREXPORT void FSRASTERCALLCONV render_triangles(
    int width, int height, bool zBuffer, int *screen,
    RenderTriangle *triangles, int count);
