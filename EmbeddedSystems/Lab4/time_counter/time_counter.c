#include <avr/io.h>
#include <util/delay.h>
#include <stdio.h>
#include <inttypes.h>

#define BAUD 9600                          // baudrate
#define UBRR_VALUE ((F_CPU)/16/(BAUD)-1)   // zgodnie ze wzorem

// inicjalizacja UART
void uart_init()
{
  // ustaw baudrate
  UBRR0 = UBRR_VALUE;
  // wyczyść rejestr UCSR0A
  UCSR0A = 0;
  // włącz odbiornik i nadajnik
  UCSR0B = _BV(RXEN0) | _BV(TXEN0);
  // ustaw format 8n1
  UCSR0C = _BV(UCSZ00) | _BV(UCSZ01);
}

// transmisja jednego znaku
int uart_transmit(char data, FILE *stream)
{
  // czekaj aż transmiter gotowy
  while(!(UCSR0A & _BV(UDRE0)));
  UDR0 = data;
  return 0;
}

// odczyt jednego znaku
int uart_receive(FILE *stream)
{
  // czekaj aż znak dostępny
  while (!(UCSR0A & _BV(RXC0)));
  return UDR0;
}

void timer1_init()
{
  // ustaw tryb licznika
  // WGM1  = 0000 -- normal
  // CS1   = 001  -- prescaler 1  liczymy cykle
  TCCR1B = _BV(CS10);
}
FILE uart_file;

void measure_int8_t()
{
    volatile int8_t a = 124;
    volatile int8_t b = 33;
    volatile int8_t result = 0;
    int16_t start_point = 0;
    int16_t end_point = 0;

    start_point = TCNT1;
    result = a + b;
    end_point = TCNT1;
    printf("int8_t \r\n");
    printf("Czas a + b: %"PRIu16" cykli\r\n", end_point - start_point);
    result = 0;

    start_point = TCNT1;
    result = a - b;
    end_point = TCNT1;
    printf("Czas a - b: %"PRIu16" cykli\r\n", end_point - start_point);
    result = 0;

    start_point = TCNT1;
    result = a * b;
    end_point = TCNT1;
    printf("Czas a * b: %"PRIu16" cykli\r\n", end_point - start_point);
    result = 0;

    start_point = TCNT1;
    result = a / b;
    end_point = TCNT1;
    printf("Czas a / b: %"PRIu16" cykli\r\n", end_point - start_point);
    printf("---------------------------------------------------------\r\n");
}

void measure_int16_t()
{
    volatile int16_t a = 12214;
    volatile int16_t b = 330;
    volatile int16_t result = 0;
    int16_t start_point = 0;
    int16_t end_point = 0;

    printf("int16_t \r\n");
    start_point = TCNT1;
    result = a + b;
    end_point = TCNT1;
    printf("Czas a + b: %"PRIu16" cykli\r\n", end_point - start_point);
    result = 0;

    start_point = TCNT1;
    result = a - b;
    end_point = TCNT1;
    printf("Czas a - b: %"PRIu16" cykli\r\n", end_point - start_point);
    result = 0;

    start_point = TCNT1;
    result = a * b;
    end_point = TCNT1;
    printf("Czas a * b: %"PRIu16" cykli\r\n", end_point - start_point);
    result = 0;

    start_point = TCNT1;
    result = a / b;
    end_point = TCNT1;
    printf("Czas a / b: %"PRIu16" cykli\r\n", end_point - start_point);
    printf("---------------------------------------------------------\r\n");
}

void measure_int32_t()
{
    volatile int32_t a = 909124;
    volatile int32_t b = 3003;
    volatile int32_t result = 0;
    int16_t start_point = 0;
    int16_t end_point = 0;

    printf("int32_t \r\n");
    start_point = TCNT1;
    result = a + b;
    end_point = TCNT1;
    printf("Czas a + b: %"PRIu16" cykli\r\n", end_point - start_point);
    result = 0;

    start_point = TCNT1;
    result = a - b;
    end_point = TCNT1;
    printf("Czas a - b: %"PRIu16" cykli\r\n", end_point - start_point);
    result = 0;

    start_point = TCNT1;
    result = a * b;
    end_point = TCNT1;
    printf("Czas a * b: %"PRIu16" cykli\r\n", end_point - start_point);
    result = 0;

    start_point = TCNT1;
    result = a / b;
    end_point = TCNT1;
    printf("Czas a / b: %"PRIu16" cykli\r\n", end_point - start_point);
    printf("---------------------------------------------------------\r\n");
}

void measure_int64_t()
{
    volatile int64_t a = 124241214;
    volatile int64_t b = 3353312;
    volatile int64_t result = 0;
    int16_t start_point = 0;
    int16_t end_point = 0;

    printf("int64_t \r\n");
    start_point = TCNT1;
    result = a + b;
    end_point = TCNT1;
    printf("Czas a + b: %"PRIu16" cykli\r\n", end_point - start_point);
    result = 0;

    start_point = TCNT1;
    result = a - b;
    end_point = TCNT1;
    printf("Czas a - b: %"PRIu16" cykli\r\n", end_point - start_point);
    result = 0;

    start_point = TCNT1;
    result = a * b;
    end_point = TCNT1;
    printf("Czas a * b: %"PRIu16" cykli\r\n", end_point - start_point);
    result = 0;

    start_point = TCNT1;
    result = a / b;
    end_point = TCNT1;
    printf("Czas a / b: %"PRIu16" cykli\r\n", end_point - start_point);
    printf("---------------------------------------------------------\r\n");
}

void measure_float()
{
    volatile float a = 58385.531;
    volatile float b = 33.3303;
    volatile float result = 0;
    int16_t start_point = 0;
    int16_t end_point = 0;

    printf("float \r\n");
    start_point = TCNT1;
    result = a + b;
    end_point = TCNT1;
    printf("Czas a + b: %"PRIu16" cykli\r\n", end_point - start_point);
    result = 0;

    start_point = TCNT1;
    result = a - b;
    end_point = TCNT1;
    printf("Czas a - b: %"PRIu16" cykli\r\n", end_point - start_point);
    result = 0;

    start_point = TCNT1;
    result = a * b;
    end_point = TCNT1;
    printf("Czas a * b: %"PRIu16" cykli\r\n", end_point - start_point);
    result = 0;

    start_point = TCNT1;
    result = a / b;
    end_point = TCNT1;
    printf("Czas a / b: %"PRIu16" cykli\r\n", end_point - start_point);
    printf("---------------------------------------------------------\r\n");
}

void time_counter()
{
    printf("---------------------------------------------------------\r\n");
    measure_int8_t();
    measure_int16_t();
    measure_int32_t();
    measure_int64_t();
    measure_float();
}

int main()
{
    // zainicjalizuj UART
    uart_init();
    // skonfiguruj strumienie wejścia/wyjścia
    fdev_setup_stream(&uart_file, uart_transmit, uart_receive, _FDEV_SETUP_RW);
    stdin = stdout = stderr = &uart_file;
    // zainicjalizuj licznik
    timer1_init();
    // program testowy
    time_counter();

}
