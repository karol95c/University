#include <stdio.h>
#include <stdint.h>

void copyIthBitToKthPos(uint32_t *x, uint32_t i, uint32_t k)
{

}


uint32_t countSetBits(uint32_t x)
{
    uint32_t c;
    c = (x & 0x55555555) + ((x >> 1) & 0x55555555);
    c = (c & 0x33333333) + ((c >> 2) & 0x33333333);
    c = (c & 0x0F0F0F0F) + ((c >> 4) & 0x0F0F0F0F);
    c = (c & 0x00FF00FF) + ((c >> 8) & 0x00FF00FF);
    c = (c & 0x0000FFFF) + ((c >> 16)& 0x0000FFFF);
    return c;
}


struct A {
    int8_t a;
    int8_t c;
    int16_t d;
    void *b;
};


struct B {
    uint16_t a;
    double b;
    void *c;
};

struct A a1;
struct B b1;

int8_t a;
    void *b;
    int8_t c;
    int16_t d;
double e;

int main()
{
    printf("Size of struct A: %u \n", sizeof(a1));
    printf("Size of struct B: %u \n", sizeof(b1));
    printf("Size of a: %u \n", sizeof(a));
    printf("Size of b: %u \n", sizeof(b));
    printf("Size of c: %u \n", sizeof(c));
    printf("Size of d: %u \n", sizeof(d));
    printf("Size of d: %u \n", sizeof(e));
    
}
