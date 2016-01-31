#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "rendering.h"

typedef struct
{
    int ymin;
    int ymax;

    float x;
    float z;
    float coeffX;
    float coeffZ;

    float r;
    float g;
    float b;

    float coeffR;
    float coeffG;
    float coeffB;
} ActiveEdge;

void sort_vertices(RenderTriangle *t)
{
    RenderTriangle output = *t;
    if (t->v1.y <= t->v2.y && t->v2.y <= t->v3.y)
    {
        return;
    }
    else if (t->v1.y <= t->v3.y && t->v3.y <= t->v2.y)
    {
        output.v1 = t->v1;
        output.v2 = t->v3;
        output.v3 = t->v2;
        output.c1 = t->c1;
        output.c2 = t->c3;
        output.c3 = t->c2;
    }
    else if (t->v2.y <= t->v1.y && t->v1.y <= t->v3.y)
    {
        output.v1 = t->v2;
        output.v2 = t->v1;
        output.v3 = t->v3;
        output.c1 = t->c2;
        output.c2 = t->c1;
        output.c3 = t->c3;
    }
    else if (t->v2.y <= t->v3.y && t->v3.y <= t->v1.y)
    {
        output.v1 = t->v2;
        output.v2 = t->v3;
        output.v3 = t->v1;
        output.c1 = t->c2;
        output.c2 = t->c3;
        output.c3 = t->c1;
    }
    else if (t->v3.y <= t->v1.y && t->v1.y <= t->v2.y)
    {
        output.v1 = t->v3;
        output.v2 = t->v1;
        output.v3 = t->v2;
        output.c1 = t->c3;
        output.c2 = t->c1;
        output.c3 = t->c2;
    }
    else if (t->v3.y <= t->v2.y && t->v2.y <= t->v1.y)
    {
        output.v1 = t->v3;
        output.v2 = t->v2;
        output.v3 = t->v1;
        output.c1 = t->c3;
        output.c2 = t->c2;
        output.c3 = t->c1;
    }
    *t = output;
}

void build_top_triangle(RenderTriangle *t, ActiveEdge *output)
{
    float dy = t->v1.y - t->v3.y;
    output[0].ymin = (int)t->v1.y;
    output[0].ymax = (int)t->v3.y;
    output[0].x = t->v1.x;
    output[0].z = t->v1.z;
    output[0].coeffX = (t->v1.x - t->v3.x) / dy;
    output[0].coeffZ = (t->v1.z - t->v3.z) / dy;

    output[0].r = GETRF(t->c1);
    output[0].g = GETGF(t->c1);
    output[0].b = GETBF(t->c1);
    output[0].coeffR = (GETRF(t->c1) - GETRF(t->c3)) / dy;
    output[0].coeffG = (GETGF(t->c1) - GETGF(t->c3)) / dy;
    output[0].coeffB = (GETBF(t->c1) - GETBF(t->c3)) / dy;


    output[1].ymin = (int)t->v1.y;
    output[1].ymax = (int)t->v3.y;
    output[1].x = t->v2.x;
    output[1].z = t->v2.z;
    output[1].coeffX = (t->v2.x - t->v3.x) / dy;
    output[1].coeffZ = (t->v2.z - t->v3.z) / dy;

    output[1].r = GETRF(t->c2);
    output[1].g = GETGF(t->c2);
    output[1].b = GETBF(t->c2);
    output[1].coeffR = (GETRF(t->c2) - GETRF(t->c3)) / dy;
    output[1].coeffG = (GETGF(t->c2) - GETGF(t->c3)) / dy;
    output[1].coeffB = (GETBF(t->c2) - GETBF(t->c3)) / dy;
}

void build_bottom_triangle(RenderTriangle *t, ActiveEdge *output)
{
    float dy = t->v1.y - t->v3.y;
    output[0].ymin = (int)t->v1.y;
    output[0].ymax = (int)t->v3.y;
    output[0].x = t->v1.x;
    output[0].z = t->v1.z;
    output[0].coeffX = (t->v1.x - t->v3.x) / dy;
    output[0].coeffZ = (t->v1.z - t->v3.z) / dy;

    output[0].r = GETRF(t->c1);
    output[0].g = GETGF(t->c1);
    output[0].b = GETBF(t->c1);
    output[0].coeffR = (GETRF(t->c1) - GETRF(t->c3)) / dy;
    output[0].coeffG = (GETGF(t->c1) - GETGF(t->c3)) / dy;
    output[0].coeffB = (GETBF(t->c1) - GETBF(t->c3)) / dy;


    output[1].ymin = (int)t->v1.y;
    output[1].ymax = (int)t->v3.y;
    output[1].x = t->v1.x;
    output[1].z = t->v1.z;
    output[1].coeffX = (t->v1.x - t->v2.x) / dy;
    output[1].coeffZ = (t->v1.z - t->v2.z) / dy;

    output[1].r = GETRF(t->c1);
    output[1].g = GETGF(t->c1);
    output[1].b = GETBF(t->c1);
    output[1].coeffR = (GETRF(t->c1) - GETRF(t->c2)) / dy;
    output[1].coeffG = (GETGF(t->c1) - GETGF(t->c2)) / dy;
    output[1].coeffB = (GETBF(t->c1) - GETBF(t->c2)) / dy;
}

void build_proper_triangle(RenderTriangle *t, ActiveEdge *output)
{
    float dy12 = t->v1.y - t->v2.y;
    float dy13 = t->v1.y - t->v3.y;
    float dy23 = t->v2.y - t->v3.y;

    output[0].ymin = (int)t->v1.y;
    output[0].ymax = (int)t->v2.y;
    output[0].x = t->v1.x;
    output[0].z = t->v1.z;
    output[0].coeffX = (t->v1.x - t->v3.x) / dy13;
    output[0].coeffZ = (t->v1.z - t->v3.z) / dy13;

    output[0].r = GETRF(t->c1);
    output[0].g = GETGF(t->c1);
    output[0].b = GETBF(t->c1);
    output[0].coeffR = (GETRF(t->c1) - GETRF(t->c3)) / dy13;
    output[0].coeffG = (GETGF(t->c1) - GETGF(t->c3)) / dy13;
    output[0].coeffB = (GETBF(t->c1) - GETBF(t->c3)) / dy13;


    output[1].ymin = (int)t->v1.y;
    output[1].ymax = (int)t->v2.y;
    output[1].x = t->v1.x;
    output[1].z = t->v1.z;
    output[1].coeffX = (t->v1.x - t->v2.x) / dy12;
    output[1].coeffZ = (t->v1.z - t->v2.z) / dy12;

    output[1].r = GETRF(t->c1);
    output[1].g = GETGF(t->c1);
    output[1].b = GETBF(t->c1);
    output[1].coeffR = (GETRF(t->c1) - GETRF(t->c2)) / dy12;
    output[1].coeffG = (GETGF(t->c1) - GETGF(t->c2)) / dy12;
    output[1].coeffB = (GETBF(t->c1) - GETBF(t->c2)) / dy12;


    output[2].ymin = (int)t->v2.y;
    output[2].ymax = (int)t->v3.y;
    output[2].x = t->v1.x - output[0].coeffX * dy12;
    output[2].z = t->v1.z - output[0].coeffZ * dy12;
    output[2].coeffX = output[0].coeffX;
    output[2].coeffZ = output[0].coeffZ;

    output[2].r = GETRF(t->c1) - output[0].coeffR * dy12;
    output[2].g = GETGF(t->c1) - output[0].coeffG * dy12;
    output[2].b = GETBF(t->c1) - output[0].coeffB * dy12;
    output[2].coeffR = output[0].coeffR;
    output[2].coeffG = output[0].coeffG;
    output[2].coeffB = output[0].coeffB;


    output[3].ymin = (int)t->v2.y;
    output[3].ymax = (int)t->v3.y;
    output[3].x = t->v2.x;
    output[3].z = t->v2.z;
    output[3].coeffX = (t->v2.x - t->v3.x) / dy23;
    output[3].coeffZ = (t->v2.z - t->v3.z) / dy23;

    output[3].r = GETRF(t->c2);
    output[3].g = GETGF(t->c2);
    output[3].b = GETBF(t->c2);
    output[3].coeffR = (GETRF(t->c2) - GETRF(t->c3)) / dy23;
    output[3].coeffG = (GETGF(t->c2) - GETGF(t->c3)) / dy23;
    output[3].coeffB = (GETBF(t->c2) - GETBF(t->c3)) / dy23;

}

void render_edges(int width, int height, int *screen, int *zBuffer, ActiveEdge ae1, ActiveEdge ae2)
{
    int ymin = CLAMP(ae1.ymin, height - 1);
    int ymax = CLAMP(ae1.ymax, height - 1);
    if (ymin != ae1.ymin)
    {
        int diff = ymin - ae1.ymin;
        ae1.x += ae1.coeffX * diff;
        ae1.z += ae1.coeffZ * diff;
        ae1.r += ae1.coeffR * diff;
        ae1.g += ae1.coeffG * diff;
        ae1.b += ae1.coeffB * diff;

        ae2.x += ae2.coeffX * diff;
        ae2.z += ae2.coeffZ * diff;
        ae2.r += ae2.coeffR * diff;
        ae2.g += ae2.coeffG * diff;
        ae2.b += ae2.coeffB * diff;
    }

    for (int y = ymin; y < ymax; y++)
    {
        int xmin = (int)CLAMP(ae1.x, width - 1);
        int xmax = (int)CLAMP(ae2.x, width - 1);

        int xoffset = (int)(xmin - ae1.x);
        float xdiff = max(1.0f, ae2.x - ae1.x);

        int mz = (int)((ae2.z - ae1.z) / xdiff);
        int z = (int)ae1.z + xoffset * mz;

        float mr = (ae2.r - ae1.r) / xdiff;
        float mg = (ae2.g - ae1.g) / xdiff;
        float mb = (ae2.b - ae1.b) / xdiff;

        float r = ae1.r + mr * xoffset,
               g = ae1.g + mg * xoffset,
               b = ae1.b + mb * xoffset;

        for (int x = xmin; x <= xmax; x++)
        {
            int idx = y * width + x;
            if (zBuffer == NULL)
            {
                screen[idx] = TORGBD(r, g, b);
            }
            else if (zBuffer[idx] <= z)
            {
                screen[idx] = TORGBD(r, g, b);
                zBuffer[idx] = z;
            }
            z += mz;
            r += mr;
            g += mg;
            b += mb;
        }

        ae1.x += ae1.coeffX;
        ae1.z += ae1.coeffZ;
        ae1.r += ae1.coeffR;
        ae1.g += ae1.coeffG;
        ae1.b += ae1.coeffB;

        ae2.x += ae2.coeffX;
        ae2.z += ae2.coeffZ;
        ae2.r += ae2.coeffR;
        ae2.g += ae2.coeffG;
        ae2.b += ae2.coeffB;
    }
}

void render_triangle(int width, int height, int *screen, int *zBuffer, RenderTriangle *triangle)
{
    ActiveEdge edges[4];
    sort_vertices(triangle);
    if (triangle->v1.y == triangle->v2.y)
    {
        build_top_triangle(triangle, edges);
        if (edges[0].coeffX <= edges[1].coeffX)
        {
            render_edges(width, height, screen, zBuffer, edges[0], edges[1]);
        }
        else
        {
            render_edges(width, height, screen, zBuffer, edges[1], edges[0]);
        }
    }
    else if (triangle->v2.y == triangle->v3.y)
    {
        build_bottom_triangle(triangle, edges);
        if (edges[0].coeffX <= edges[1].coeffX)
        {
            render_edges(width, height, screen, zBuffer, edges[0], edges[1]);
        }
        else
        {
            render_edges(width, height, screen, zBuffer, edges[1], edges[0]);
        }
    }
    else
    {
        build_proper_triangle(triangle, edges);
        if (edges[0].coeffX <= edges[1].coeffX)
        {
            render_edges(width, height, screen, zBuffer, edges[0], edges[1]);
        }
        else
        {
            render_edges(width, height, screen, zBuffer, edges[1], edges[0]);
        }
        if (edges[2].coeffX >= edges[3].coeffX)
        {
            render_edges(width, height, screen, zBuffer, edges[2], edges[3]);
        }
        else
        {
            render_edges(width, height, screen, zBuffer, edges[3], edges[2]);
        }
    }
}

void render_triangles(
    int width, int height, bool zBuffer,
    int *screen,
    RenderTriangle *triangles, int count)
{
    int *newZBuffer = NULL;
    if (zBuffer)
    {
        // We rely on the fact that the allocated array contains zeroes and that the z-coordinate
        // of every pixel is in range [0..intmax - 1] and 0 is the "far away" value. This way we don't
        // have to memset the buffer and we don't need to use doubles, which results in monumental
        // speed boost. That's the reason why the z-coordinate gets transformed the way it is.
        newZBuffer = calloc(width * height, sizeof(int));
    }

    for (int i = 0; i < count; i++)
    {
        render_triangle(width, height, screen, newZBuffer, triangles + i);
    }

    if (zBuffer)
    {
        free(newZBuffer);
    }
}