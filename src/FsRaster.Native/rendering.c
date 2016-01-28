#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "rendering.h"

void render_triangles(
    int width, int height, bool zBuffer,
    int *screen,
    RenderTriangle *triangles, int count)
{
    double *newZBuffer = NULL;
    if (zBuffer)
    {
        newZBuffer = calloc(width * height, sizeof(double));
    }

    if (zBuffer)
    {
        free(newZBuffer);
    }
}