#include <stdio.h>
#include <stdint.h>


void binprintf(uint32_t v)
{
    unsigned int mask=1<<((sizeof(uint32_t)<<3)-1);
    while(mask) {
        printf("%d", (v&mask ? 1 : 0));
        mask >>= 1;
    }
}


int main()
{
    uint32_t x, y;
    float a = 100.1202102;
    float b = 100.1202102;
    x = uint32_t
    binprintf(num);
    return 0;
}