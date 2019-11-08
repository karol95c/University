#include <avr/io.h>
#include <util/delay.h>
#include <stdio.h>
#include <inttypes.h>
#include <stdlib.h>
#include <avr/pgmspace.h>

#define LED_RED PB3
#define LED_GREEN PB2
#define LED_BLUE PB1
#define LED_DDR DDRB

#define OCR_RED OCR2A
#define OCR_GREEN OCR1B
#define OCR_BLUE OCR1A

#define HSV_C 1


#define BAUD 9600                          // baudrate
#define UBRR_VALUE ((F_CPU)/16/(BAUD)-1)   // zgodnie ze wzorem

#define SIN_TALBE_SIZE 127

static const uint8_t sin_table[SIN_TALBE_SIZE] PROGMEM = 
{ 
  255, 254, 254, 254, 254, 254, 253, 253, 252, 251, 251, 250, 249,
  248, 247, 246, 245, 243, 242, 241, 239, 238, 236, 235, 233, 231,
  229, 227, 225, 223, 221, 219, 217, 215, 212, 210, 208, 205, 203,
  200, 197, 195, 192, 189, 187, 184, 181, 178, 175, 172, 169, 166,
  163, 160, 157, 154, 151, 148, 145, 142, 139, 136, 132, 129, 126,
  123, 120, 117, 114, 111, 107, 104, 101, 98, 95, 92, 89, 86, 83,
  80, 77, 74, 72, 69, 66, 63, 61, 58, 55, 53, 50, 48, 45, 43, 41,
  38, 36, 34, 32, 30, 28, 26, 24, 22, 20, 19, 17, 15, 14, 13, 11,
  10, 9, 8, 6, 5, 5, 4, 3, 2, 2, 1, 1, 0, 0, 0, 0
};

void timer_init()
{
  // PB 1PB2
	TCCR1A |= (1 << WGM10) | (1 << COM1A1) | (1 << COM1B1);
	TCCR1B |= (1 << WGM12) | (1 << CS11);
  // PB3
	TCCR2A |= (1 << WGM20) | (1 << COM2A1);
	TCCR2B |= (1 << WGM21) | (1 << CS11);
}

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
  // CS1   = 001  -- prescaler 1
  TCCR1B = _BV(CS10);
}

typedef struct rgb_color_
{
    int8_t r;
    int8_t g;
    int8_t b;
} rgb_color;

void get_random_rgb(rgb_color *rgb)
{
    int16_t h = (rand() % 360) / 60;
    int16_t x = (1 - ((h %2) - 1));
    int16_t mask = x >> 15;
    (*rgb).r = 0;
    (*rgb).g = 0;
    (*rgb).b = 0;
  
    x = (mask ^ x ) - mask - 1; 
    switch (h){
      case 0:
        (*rgb).r = HSV_C;
        (*rgb).g = x;
        break;
      case 1:
        (*rgb).r = x;
        (*rgb).g = HSV_C;
        break;
      case 2:
        (*rgb).g = HSV_C;
        (*rgb).b = x;
        break;
      case 3:
        (*rgb).g = x;
        (*rgb).b = HSV_C;
        break;
      case 4:
        (*rgb).r = x;
        (*rgb).b = HSV_C;
        break;
      case 5:
        (*rgb).r = HSV_C;
        (*rgb).b = x;
        break;
      default:
        break;
    }
}

void set_rgb(rgb_color *rgb, int16_t i)
{
    OCR_RED = 255 - ((*rgb).r * pgm_read_byte(&(sin_table[i])));
    OCR_GREEN = 255 - ((*rgb).g * pgm_read_byte(&(sin_table[i])));
    OCR_BLUE = 255 - ((*rgb).b * pgm_read_byte(&(sin_table[i])));
}
FILE uart_file;

int main()
{
    // zainicjalizuj UART
    uart_init();

    // skonfiguruj strumienie wejścia/wyjścia
    fdev_setup_stream(&uart_file, uart_transmit, uart_receive, _FDEV_SETUP_RW);
    stdin = stdout = stderr = &uart_file;
    // zainicjalizuj licznik
    timer_init();
    // initialize random seed:
    srand(time(0));

    LED_DDR |= (1 << LED_RED);
    LED_DDR |= (1 << LED_GREEN);
    LED_DDR |= (1 << LED_BLUE);

    timer_init();
    rgb_color rgb;
    rgb.r = 0;
    rgb.g = 0;
    rgb.b = 0;

    while (1)
    {
      int16_t i =  SIN_TALBE_SIZE;
      get_random_rgb(&rgb);		

      while (i > 0)
      {
        set_rgb(&rgb, i);
        i--;
        _delay_ms(9);
      }
      while (i < SIN_TALBE_SIZE)
      {
        set_rgb(&rgb, i);
        i++;
        _delay_ms(9);
      }
    }
}