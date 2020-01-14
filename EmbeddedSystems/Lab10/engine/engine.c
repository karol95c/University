#include <avr/io.h>
#include <avr/interrupt.h>
#include <util/delay.h>
#include <avr/sleep.h>
#include <stdio.h>

#define BAUD 9600                            // baudrate
#define UBRR_VALUE ((F_CPU) / 16 / (BAUD)-1) // zgodnie ze wzorem


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

FILE uart_file;

// f = 16000000 / (64 * 300) = ~ 833 Hz
void init_timer1()
{
	ICR1 = 299;
	TCCR1A |= (1 << WGM11);
	TCCR1B |= (1 << WGM13) | (1 << WGM12);
	TCCR1A |= (1 << COM1A1);
	TCCR1B |= (1 << CS11) | (1 << CS10); // prescaler 64
}

void adc_init(void)
{
  ADMUX   = _BV(REFS0); // referencja AVcc, wejście ADC0
  DIDR0   = _BV(ADC0D); // wyłącz wejście cyfrowe na ADC0
  // częstotliwość zegara ADC 125 kHz (16 MHz / 128)
  ADCSRA  = _BV(ADPS0) | _BV(ADPS1) | _BV(ADPS2); // preskaler 128
  // ADCSRA  = _BV(ADPS1); // preskaler 4
  ADCSRA |= _BV(ADEN); // włącz ADC
}


int16_t read_adc() {
	ADCSRA |= (1 << ADSC);
	loop_until_bit_is_clear(ADCSRA, ADSC);
	return (ADC / 8);
}

int main() {
    // zainicjalizuj UART
    init_timer1();
    adc_init();
    // skonfiguruj strumienie wejścia/wyjścia
    uart_init();
    fdev_setup_stream(&uart_file, uart_transmit, uart_receive, _FDEV_SETUP_RW);
    stdin = stdout = stderr = &uart_file;

    volatile int16_t adc = 0;

    DDRB |= (1 << PB1);
    OCR1A = adc;

    while(1) {
      adc = read_adc();
      OCR1A = adc;
      printf("adc %"PRIu16"\r\n", adc);
  }
}