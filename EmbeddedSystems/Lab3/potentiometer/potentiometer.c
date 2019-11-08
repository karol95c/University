#include <avr/io.h>
#include <stdio.h>
#include <inttypes.h>
#include <util/delay.h>

#define BAUD 9600                          // baudrate
#define UBRR_VALUE ((F_CPU)/16/(BAUD)-1)   // zgodnie ze wzorem

#define LED PB5
#define LED_DDR DDRB
#define LED_PORT PORTB

#define LIGHT_MULT 2
#define LIGHT_LIMIT 1050

// inicjalizacja UART
void uart_init()
{
  // ustaw baudrate
  UBRR0 = UBRR_VALUE;
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

// inicjalizacja ADC
void adc_init()
{
  ADMUX   = _BV(REFS0); // referencja AVcc, wejście ADC0
  DIDR0   = _BV(ADC0D); // wyłącz wejście cyfrowe na ADC0
  // częstotliwość zegara ADC 125 kHz (16 MHz / 128)
  ADCSRA  = _BV(ADPS0) | _BV(ADPS1) | _BV(ADPS2); // preskaler 128
  // ADCSRA  = _BV(ADPS1); // preskaler 4
  ADCSRA |= _BV(ADEN); // włącz ADC
}

/* Found on internet */
const uint16_t gamma_table[65] = 
{ 
  0,    0,    1,    2,    4,    6,    9,    13,  
  16,   21,   26,   31,   37,   44,   51,   58,  
  66,   74,   84,   93,   103,  114,  125,  136,  
  148,  161,  174,  188,  202,  217,  232,  248,  
  264,  281,  298,  316,  334,  353,  372,  392,  
  412,  433,  455,  477,  499,  522,  545,  569,  
  594,  619,  644,  670,  697,  724,  752,  780,  
  808,  837,  867,  897,  928,  959,  991,  1023,
  LIGHT_LIMIT 
};
void delay_us(uint16_t count) {
  while(count--) {
    _delay_us(1);
  }
}
FILE uart_file;

int log_new(int16_t n)
{
  printf("(n) %"PRIu16"\r\n", n);
  int i = 1;
  while (n > 0)
  {
    n = n >> 1;
    ++i;
  }
  return i;
}

int main()
{
  // zainicjalizuj UART
  uart_init();
  // skonfiguruj strumienie wejścia/wyjścia
  fdev_setup_stream(&uart_file, uart_transmit, uart_receive, _FDEV_SETUP_RW);
  stdin = stdout = stderr = &uart_file;
  // zainicjalizuj ADC
  adc_init();
  // int8_t light_on = 10;
  uint16_t diff = 0;
  uint16_t light = 0;
  uint16_t avg = 0;
  // mierz napięcie
  while(1) {
    
    ADCSRA |= _BV(ADSC); // wykonaj konwersję
    while (!(ADCSRA & _BV(ADIF))); // czekaj na wynik
    ADCSRA |= _BV(ADIF); // wyczyść bit ADIF (pisząc 1!)
    uint16_t v = ADC; // weź zmierzoną wartość (0..1023);
    /* divide by 16 to limit in gamma table index [0,63] */
    light = (v / (1 << 4));

    diff = gamma_table[light + 1] - gamma_table[light];
    light = gamma_table[light];
    avg = v - gamma_table[light];
    if (avg > 0)
    {
      light += (diff / 2);
    }
      
    // light = gamma_table[light];
    LED_DDR |= _BV(LED);
    LED_PORT |= _BV(LED);

    delay_us(light);
    LED_PORT &= ~_BV(LED);
    delay_us(LIGHT_LIMIT - light);
  }
}
