#include <stdio.h>
#include <inttypes.h>


// #define BAUD 9600                          // baudrate
// #define UBRR_VALUE ((F_CPU)/16/(BAUD)-1)   // zgodnie ze wzorem

// // inicjalizacja UART
// void uart_init()
// {
//   // ustaw baudrate
//   UBRR0 = UBRR_VALUE;
//   // wyczyść rejestr UCSR0A
//   UCSR0A = 0;
//   // włącz odbiornik i nadajnik
//   UCSR0B = _BV(RXEN0) | _BV(TXEN0);
//   // ustaw format 8n1
//   UCSR0C = _BV(UCSZ00) | _BV(UCSZ01);
// }

// // transmisja jednego znaku
// int uart_transmit(char data, FILE *stream)
// {
//   // czekaj aż transmiter gotowy
//   while(!(UCSR0A & _BV(UDRE0)));
//   UDR0 = data;
//   return 0;
// }

// // odczyt jednego znaku
// int uart_receive(FILE *stream)
// {
//   // czekaj aż znak dostępny
//   while (!(UCSR0A & _BV(RXC0)));
//   return UDR0;
// }

void calculate_8bit()
{
    int8_t a = 1;
    int8_t b = 1;
    int8_t result = 0;
    printf("Input two int8_t\r\n");
    scanf("%"SCNd8, &a);
    scanf("%"SCNd8, &b);
    
    printf("Read: a = %"PRId8", b = %"PRId8"\r\n", a, b);
    result = a + b;
    printf("a + b = %"PRId8"\r\n", result);
    
    result = a - b;
    printf("a - b = %"PRId8"\r\n", result);

    result = a * b;
    printf("a * b = %"PRId8"\r\n", result);

    result = a / b;
    printf("a / b = %"PRId8"\r\n", result);

}

void calculate_16bit()
{
    int16_t a = 1;
    int16_t b = 1;
    int16_t result = 0;
    printf("Input two int16_t\r\n");
    scanf("%"SCNd16, &a);
    scanf("%"SCNd16, &b);
    
    printf("Read: a = %"PRId16", b = %"PRId16"\r\n", a, b);
    result = a + b;
    printf("a + b = %"PRId16"\r\n", result);
    
    result = a - b;
    printf("a - b = %"PRId16"\r\n", result);

    result = a * b;
    printf("a * b = %"PRId16"\r\n", result);

    result = a / b;
    printf("a / b = %"PRId16"\r\n", result);

}

void calculate_32bit()
{
    int32_t a = 1;
    int32_t b = 1;
    int32_t result = 0;
    printf("Input two int32_t\r\n");
    scanf("%"SCNd32, &a);
    scanf("%"SCNd32, &b);
    
    printf("Read: a = %"PRId32", b = %"PRId32"\r\n", a, b);
    result = a + b;
    printf("a + b = %"PRId32"\r\n", result);
    
    result = a - b;
    printf("a - b = %"PRId32"\r\n", result);

    result = a * b;
    printf("a * b = %"PRId32"\r\n", result);

    result = a / b;
    printf("a / b = %"PRId32"\r\n", result);

}

void calculate_64bit()
{
    int64_t a = 0;
    int64_t b = 0;
    int64_t result = 0;
    printf("Input two int64_t\r\n");
    scanf("%lld", &a);
    scanf("%lld", &b);
    
    printf("Read: a = %lld, b = %lld\r\n", a, b);
    result = a + b;
    printf("a + b = %lld\r\n", result);
    
    result = a - b;
    printf("a - b = %lld\r\n", result);

    result = a * b;
    printf("a * b = %lld\r\n", result);

    result = a / b;
    printf("a / b = %lld\r\n", result);

}

void calculate_float()
{
    float a = 1.0;
    float b = 1.0;
    float result = 0;
    printf("Input two floats\r\n");
    scanf("%f", &a);
    scanf("%f", &b);
    
    printf("Read: a = %f, b = %f\r\n", a, b);
    result = a + b;
    printf("a + b = %f\r\n", result);
    
    result = a - b;
    printf("a - b = %f\r\n", result);

    result = a * b;
    printf("a * b = %f\r\n", result);

    result = a / b;
    printf("a / b = %f\r\n", result);

}

FILE uart_file;

int main()
{
  // // zainicjalizuj UART
  // uart_init();
  // // skonfiguruj strumienie wejścia/wyjścia
  // fdev_setup_stream(&uart_file, uart_transmit, uart_receive, _FDEV_SETUP_RW);
  // stdin = stdout = stderr = &uart_file;
  // program testowy
  calculate_8bit();
  calculate_16bit();
  calculate_32bit();
  calculate_64bit();
  calculate_float();
//   while(1) {
//     int16_t a = 1;
//     scanf("%"SCNd16, &a);
//     printf("Odczytano: %"PRId16"\r\n", a);
//   }
}
