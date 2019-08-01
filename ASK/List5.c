#include <stdio.h>
#include <stdint.h>



uint32_t puzzle3(uint32_t n, uint32_t d) {
  uint64_t long_n, long_d;
  uint32_t result, mask; 

  long_d = (int64_t)d << 32;
  long_n = n; 

  mask = 0x80000000;
  result = 0; 

  for (int i = 32; i != 0; i--, mask >>= 1) {
    long_n <<= 1;
    if ((int64_t)(long_n - long_d) >= 0) {  
      result |= mask; 
      long_n -= long_d; //long_n - long_d powinno zostać przypisane do n
    }	
  }
  return result;
}

int main()
{
    uint32_t res = puzzle3(124219, 24121);
    printf("%d", res);
    printf("%d", (5 * 24121));
}