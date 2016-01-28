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

FSRASTEREXPORT void FSRASTERCALLCONV render_triangles(
    int width, int height, bool zBuffer, int *screen,
    RenderTriangle *triangles, int count);
