#include <avr/io.h>
#include <stdio.h>
#include <inttypes.h>
#include <util/delay.h>

#define BAUD 9600                          // baudrate
#define UBRR_VALUE ((F_CPU)/16/(BAUD)-1)   // zgodnie ze wzorem

#define LED PB5
#define LED_DDR DDRB
#define LED_PORT PORTB

#define DELAY_TIME 1250

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
  ADCSRA |= _BV(ADEN); // włącz ADC
  ADMUX |= _BV(MUX1) | _BV(MUX2) |_BV(MUX3);
}

void meter()
{
    float result;
    uint16_t value;
    ADCSRA |= _BV(ADSC);
    while (!(ADCSRA & _BV(ADIF)));
        ADCSRA |= _BV(ADIF);
        value = ADC;
        result = (1.1 * 1024 )/ value;
    
    printf("Voltage: %f\r\n", result);
}

void meter_led(int16_t on)
{
  if (on)
  {
    printf("LED ON\r\n");
    LED_PORT |= _BV(LED);
  }
  else
  {
    printf("LED OFF\r\n");
    LED_PORT &= ~_BV(LED);
  }
  
  meter();
}
FILE uart_file;

int main()
{
  // zainicjalizuj UART
  uart_init();
  // skonfiguruj strumienie wejścia/wyjścia
  fdev_setup_stream(&uart_file, uart_transmit, uart_receive, _FDEV_SETUP_RW);
  stdin = stdout = stderr = &uart_file;
  // zainicjalizuj ADC
  adc_init();
  while(1) {
    meter_led(1);
    _delay_ms(DELAY_TIME);
    meter_led(0);
    _delay_ms(DELAY_TIME);
  }
}
