#include <stdio.h>
#include <stdint.h>
#include <limits.h>


int main()
{
    int32_t x, y, z;
    double dx, dy, dz;
    x = INT32_MAX;
    y = INT32_MAX;
    z = 0;
    long double lx;
    long double ly;
    long double lz;
    dx = (double) x;
    dy = (double) y;
    dz = (double) z;
    lx = (long double) x;
    ly = (long double) y;
    lz = (long double) z;
    // printf("%f\n", (float)x);
    // printf("%f\n", (float)dx);
    // printf("%d\n", __INT32_MAX__);
    //int32_t res = ((float)x == (float)dx);
    // printf("%f\n", ((dx * dy) * dz));
    // printf("%f\n", (dx * (dy * dz)));
    // int32_t res = ((dx * dy) * dz == dx * (dy * dz));
    // long double res1 = (lx * ly) * lz;
    // double res2 = (dx * dy) * dz;
    // printf("%Lf\n", res1);
    // printf("%f\n", res2);
    int32_t res = dx / dx == dz / dz;
    printf("%d\n", 1);
    int num = -2;
    int32_t p;
    p = INT_MAX;
    double dp;
    dp = (double) p;
    printf("%f\n", dp);
    return 0;
}