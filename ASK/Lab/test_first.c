#include <stdio.h>
#include <inttypes.h>
#include <stdlib.h>

uint64_t bitrev(uint64_t);

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
    char *bits2;
    uint64_t result;
    result = bitrev(test1);
    bits1 = decimal_to_binary(test1);
    bits2 = decimal_to_binary(result);
    int check = 0;
    for(uint64_t i=0; i<64; ++i)
    {
        if(bits1[i] != bits2[63 - i])
        {
            check = 1;
        }
    }

    if (check == 1)
    {
        printf("FAILURE\n");
    }
    else
    {
        printf("SUCCESS\n");
    }
    

    free(bits1);
    free(bits2);
}

int main() {
    // printf("%l\n", bitrev(1));
    // printf("%l\n", bitrev(2));
    // printf("%l\n", bitrev(2));
    test(52421421421421124);
    test(0);
    test(124);
    test(553124241424124);
    return 0;
}