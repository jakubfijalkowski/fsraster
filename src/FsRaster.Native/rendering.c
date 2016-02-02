#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <intrin.h>
#include <math.h>
#include "rendering.h"

typedef struct
{
    float z;
    float b;
    float g;
    float r;
} AEVector;

typedef struct
{
    int ymin;
    int ymax;

    float x;
    float coeffX;

    AEVector coord;
    AEVector coeff;
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
    output[0].coord.z = t->v1.z;
    output[0].coeffX = (t->v1.x - t->v3.x) / dy;
    output[0].coeff.z = (t->v1.z - t->v3.z) / dy;

    output[0].coord.r = GETRF(t->c1);
    output[0].coord.g = GETGF(t->c1);
    output[0].coord.b = GETBF(t->c1);
    output[0].coeff.r = (GETRF(t->c1) - GETRF(t->c3)) / dy;
    output[0].coeff.g = (GETGF(t->c1) - GETGF(t->c3)) / dy;
    output[0].coeff.b = (GETBF(t->c1) - GETBF(t->c3)) / dy;


    output[1].ymin = (int)t->v1.y;
    output[1].ymax = (int)t->v3.y;
    output[1].x = t->v2.x;
    output[1].coord.z = t->v2.z;
    output[1].coeffX = (t->v2.x - t->v3.x) / dy;
    output[1].coeff.z = (t->v2.z - t->v3.z) / dy;

    output[1].coord.r = GETRF(t->c2);
    output[1].coord.g = GETGF(t->c2);
    output[1].coord.b = GETBF(t->c2);
    output[1].coeff.r = (GETRF(t->c2) - GETRF(t->c3)) / dy;
    output[1].coeff.g = (GETGF(t->c2) - GETGF(t->c3)) / dy;
    output[1].coeff.b = (GETBF(t->c2) - GETBF(t->c3)) / dy;
}

void build_bottom_triangle(RenderTriangle *t, ActiveEdge *output)
{
    float dy = t->v1.y - t->v3.y;
    output[0].ymin = (int)t->v1.y;
    output[0].ymax = (int)t->v3.y;
    output[0].x = t->v1.x;
    output[0].coord.z = t->v1.z;
    output[0].coeffX = (t->v1.x - t->v3.x) / dy;
    output[0].coeff.z = (t->v1.z - t->v3.z) / dy;

    output[0].coord.r = GETRF(t->c1);
    output[0].coord.g = GETGF(t->c1);
    output[0].coord.b = GETBF(t->c1);
    output[0].coeff.r = (GETRF(t->c1) - GETRF(t->c3)) / dy;
    output[0].coeff.g = (GETGF(t->c1) - GETGF(t->c3)) / dy;
    output[0].coeff.b = (GETBF(t->c1) - GETBF(t->c3)) / dy;


    output[1].ymin = (int)t->v1.y;
    output[1].ymax = (int)t->v3.y;
    output[1].x = t->v1.x;
    output[1].coord.z = t->v1.z;
    output[1].coeffX = (t->v1.x - t->v2.x) / dy;
    output[1].coeff.z = (t->v1.z - t->v2.z) / dy;

    output[1].coord.r = GETRF(t->c1);
    output[1].coord.g = GETGF(t->c1);
    output[1].coord.b = GETBF(t->c1);
    output[1].coeff.r = (GETRF(t->c1) - GETRF(t->c2)) / dy;
    output[1].coeff.g = (GETGF(t->c1) - GETGF(t->c2)) / dy;
    output[1].coeff.b = (GETBF(t->c1) - GETBF(t->c2)) / dy;
}

void build_proper_triangle(RenderTriangle *t, ActiveEdge *output)
{
    float dy12 = t->v1.y - t->v2.y;
    float dy13 = t->v1.y - t->v3.y;
    float dy23 = t->v2.y - t->v3.y;

    output[0].ymin = (int)t->v1.y;
    output[0].ymax = (int)t->v2.y;
    output[0].x = t->v1.x;
    output[0].coord.z = t->v1.z;
    output[0].coeffX = (t->v1.x - t->v3.x) / dy13;
    output[0].coeff.z = (t->v1.z - t->v3.z) / dy13;

    output[0].coord.r = GETRF(t->c1);
    output[0].coord.g = GETGF(t->c1);
    output[0].coord.b = GETBF(t->c1);
    output[0].coeff.r = (GETRF(t->c1) - GETRF(t->c3)) / dy13;
    output[0].coeff.g = (GETGF(t->c1) - GETGF(t->c3)) / dy13;
    output[0].coeff.b = (GETBF(t->c1) - GETBF(t->c3)) / dy13;


    output[1].ymin = (int)t->v1.y;
    output[1].ymax = (int)t->v2.y;
    output[1].x = t->v1.x;
    output[1].coord.z = t->v1.z;
    output[1].coeffX = (t->v1.x - t->v2.x) / dy12;
    output[1].coeff.z = (t->v1.z - t->v2.z) / dy12;

    output[1].coord.r = GETRF(t->c1);
    output[1].coord.g = GETGF(t->c1);
    output[1].coord.b = GETBF(t->c1);
    output[1].coeff.r = (GETRF(t->c1) - GETRF(t->c2)) / dy12;
    output[1].coeff.g = (GETGF(t->c1) - GETGF(t->c2)) / dy12;
    output[1].coeff.b = (GETBF(t->c1) - GETBF(t->c2)) / dy12;


    output[2].ymin = (int)t->v2.y;
    output[2].ymax = (int)t->v3.y;
    output[2].x = t->v1.x - output[0].coeffX * dy12;
    output[2].coord.z = t->v1.z - output[0].coeff.z * dy12;
    output[2].coeffX = output[0].coeffX;
    output[2].coeff.z = output[0].coeff.z;

    output[2].coord.r = GETRF(t->c1) - output[0].coeff.r * dy12;
    output[2].coord.g = GETGF(t->c1) - output[0].coeff.g * dy12;
    output[2].coord.b = GETBF(t->c1) - output[0].coeff.b * dy12;
    output[2].coeff.r = output[0].coeff.r;
    output[2].coeff.g = output[0].coeff.g;
    output[2].coeff.b = output[0].coeff.b;


    output[3].ymin = (int)t->v2.y;
    output[3].ymax = (int)t->v3.y;
    output[3].x = t->v2.x;
    output[3].coord.z = t->v2.z;
    output[3].coeffX = (t->v2.x - t->v3.x) / dy23;
    output[3].coeff.z = (t->v2.z - t->v3.z) / dy23;

    output[3].coord.r = GETRF(t->c2);
    output[3].coord.g = GETGF(t->c2);
    output[3].coord.b = GETBF(t->c2);
    output[3].coeff.r = (GETRF(t->c2) - GETRF(t->c3)) / dy23;
    output[3].coeff.g = (GETGF(t->c2) - GETGF(t->c3)) / dy23;
    output[3].coeff.b = (GETBF(t->c2) - GETBF(t->c3)) / dy23;

}

void render_edges(int width, int height, int *screen, float *zBuffer, ActiveEdge ae1, ActiveEdge ae2)
{
    __m128 coords1 = _mm_load_ps((float*)&ae1.coord), coords2 = _mm_load_ps((float*)&ae2.coord);
    const __m128 coeffs1 = _mm_load_ps((float*)&ae1.coeff), coeffs2 = _mm_load_ps((float*)&ae2.coeff);

    int ymin = CLAMP(ae1.ymin, height - 1);
    int ymax = CLAMP(ae1.ymax, height - 1);
    if (ymin != ae1.ymin)
    {
        int diff = ymin - ae1.ymin;
        ae1.x += ae1.coeffX * diff;
        ae2.x += ae2.coeffX * diff;

        __m128 multiplier = _mm_cvtepi32_ps(_mm_set1_epi32(diff));
        coords1 = _mm_add_ps(coords1, _mm_mul_ps(coeffs1, multiplier));
        coords2 = _mm_add_ps(coords2, _mm_mul_ps(coeffs2, multiplier));
    }

    for (int y = ymin; y < ymax; y++)
    {
        int xmin = (int)CLAMP(ae1.x, width - 1);
        int xmax = (int)ceilf(CLAMP(ae2.x, width - 1));

        const __m128 xdiff = _mm_set_ps1(max(1.0f, ae2.x - ae1.x + 2.0f));
        const __m128 coeffsLocal = _mm_div_ps(_mm_sub_ps(coords2, coords1), xdiff);

        float xoffset = (float)(int)(xmin - ae1.x);
        __m128 coordsLocal = _mm_add_ps(coords1, _mm_mul_ps(coeffsLocal, _mm_set_ps1(xoffset)));

        for (int x = xmin; x <= xmax; x++)
        {
            float z = coordsLocal.m128_f32[0];
            int idx = y * width + x;
            if (zBuffer == NULL)
            {
                __m128i clconv = _mm_cvtps_epi32(coordsLocal);
                clconv = _mm_or_si128(clconv, _mm_or_si128(_mm_srli_si128(clconv, 3), _mm_srli_si128(clconv, 6)));
                screen[idx] = clconv.m128i_u32[1] | 0xff000000;
            }
            else if (zBuffer[idx] <= z)
            {
                __m128i clconv = _mm_cvtps_epi32(coordsLocal);
                clconv = _mm_or_si128(clconv, _mm_or_si128(_mm_srli_si128(clconv, 3), _mm_srli_si128(clconv, 6)));

                screen[idx] = clconv.m128i_u32[1] | 0xff000000;
                zBuffer[idx] = z;
            }
            coordsLocal = _mm_add_ps(coordsLocal, coeffsLocal);
        }

        ae1.x += ae1.coeffX;
        ae2.x += ae2.coeffX;

        coords1 = _mm_add_ps(coords1, coeffs1);
        coords2 = _mm_add_ps(coords2, coeffs2);
    }
}

void render_triangle(int width, int height, int *screen, float *zBuffer, RenderTriangle *triangle)
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
    float *newZBuffer = NULL;
    if (zBuffer)
    {
        newZBuffer = calloc(width * height, sizeof(float));
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