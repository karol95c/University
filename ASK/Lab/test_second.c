#include <stdio.h>
#include <inttypes.h>
#include <stdlib.h>
#include <time.h>

int clz(uint64_t);

char* decimal_to_binary(uint64_t number)
{
    char* bitset;
    bitset = malloc(sizeof(char) * 64);
    for(uint64_t i=0; i<64; ++i)
    {
        if((number & (1ULL << i)) != 0)
        {
            bitset[63-i] = '1';
        }
        else
        {
            bitset[63-i] = '0';
        }
    }
    for(uint64_t i=0; i<64; ++i)
    {
        printf("%c", bitset[i]);
    }
    printf("\n");
    return bitset;
}

void test(uint64_t test1)
{
    char *bits1;
    int counter = 0;
    int result = 0;
    uint64_t t2;
    t2 = test1;
    result = clz(t2);
    bits1 = decimal_to_binary(test1);
    for(uint64_t i = 0; i<64; ++i)
    {
        if(bits1[i] == '0')
        {
            counter++;
        }
        else
        {
            break;
        }
    }

    if (counter == result)
    {
        free(bits1);
        printf("SUCCESS\n");
    }
    else
    {
        printf("FAILURE\n");
    }

}

int main() {
    test(10);
    test(11);
    test(0);
    test(1);
    test(29149214921);
    test(2841243812);
    test(2492149219842189484);
    test(2492149219842189483);

    return 0;
}