#include <avr/io.h>
#include <util/delay.h>
#include <avr/interrupt.h>
#include <stdio.h>

#define BTN PD3
#define BTN_PIN PIND
#define BTN_PORT PORTD

#define BAUD 9600                            // baudrate
#define UBRR_VALUE ((F_CPU) / 16 / (BAUD)-1) // zgodnie ze wzorem

#define PRINT_DELAY 500

#define RESISTOR_330K 330000.0


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


// inicjalizacja ADC
static void adc_init()
{
    ADMUX   = _BV(REFS0);
    DIDR0   = _BV(ADC0D);
    // częstotliwość zegara ADC 125 kHz (16 MHz / 128)
    ADCSRA  = _BV(ADPS0) | _BV(ADPS1) | _BV(ADPS2) | _BV(ADEN); // preskaler 128 ADC ON
}

static void init_interrupt()
{
	PORTD |= (1 << PORTD3);
    // failing edge ISC11
	EICRA |= (1 << ISC11);
	EIMSK |= _BV(INT1);
}

static int16_t read_adc()
{
	ADCSRA |= (1 << ADSC);
	// wait until ADSC is 0
	loop_until_bit_is_clear(ADCSRA, ADSC); 
	return ADC;
}

int16_t adc_value;
ISR(INT1_vect)
{
	adc_value = read_adc();
}

int main()
{
	// zainicjalizuj UART
	uart_init();
	// skonfiguruj strumienie wejścia/wyjścia
	fdev_setup_stream(&uart_file, uart_transmit, uart_receive, _FDEV_SETUP_RW);
	stdin = stdout = stderr = &uart_file;
	BTN_PORT |= (1 << BTN);

	adc_init();
	init_interrupt();
	sei();
	
	int32_t display = 0;
	float voltage = 0;
	while (1)
	{
		voltage = adc_value / 1024.0 * 5.0;
		display = (RESISTOR_330K * 5.0 / voltage - RESISTOR_330K);
		// printf("%d\r\n", voltage);
		// printf("%d\r\n", display);
		printf("Ohms: %"PRId32"\r\n",display);
		_delay_ms(PRINT_DELAY);
	}
}