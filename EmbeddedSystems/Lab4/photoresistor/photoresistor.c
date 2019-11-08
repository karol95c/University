#include <avr/io.h>
#include <util/delay.h>
#include <stdio.h>
#include <inttypes.h>
#include <math.h>
#include <avr/pgmspace.h>

#define LED PB2
#define LED_DDR DDRB
#define LED_PORT PORTB
#define OCR_LED OCR1B

#define LIGHT_LIMIT 1050 
#define BAUD 9600                          // baudrate
#define UBRR_VALUE ((F_CPU)/16/(BAUD)-1)   // zgodnie ze wzorem

FILE uart_file;

/* Found on internet */
static const uint16_t gamma_table[65] PROGMEM = 
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
    }

int16_t get_light(int16_t adc)
{
    int16_t diff = 0;
    int16_t avg = 0;
    int16_t light = 0;
    light = (adc / (1 << 4));
    int8_t idx = 64 - light;
    int16_t table_idx = pgm_read_word(&(gamma_table[idx]));
    int16_t table_idx1 = pgm_read_word(&(gamma_table[idx - 1]));
    diff = table_idx1 - table_idx;
    avg = adc - table_idx;
    light = table_idx;
    if (avg > 0)
    {
        light += (diff / 2);
    }
    
    return light / 3;
}

void timer_init()
{
    // CS11 prescaler 010 - 8 
    // COM1B1 OC1B(PB2)

    TCCR1A = (1 << WGM10) | (1 << COM1B1);
    TCCR1B = (1 << WGM12) | (1 << CS10);
    // TCCR1B |= (1 << WGM12) | (1 << CS12) | (1 << CS10);
}

int main()
{
    uart_init();
    // skonfiguruj strumienie wejścia/wyjścia
    fdev_setup_stream(&uart_file, uart_transmit, uart_receive, _FDEV_SETUP_RW);
    stdin = stdout = stderr = &uart_file;
    adc_init();
    timer_init();
    LED_DDR |= (1 << LED);
    int16_t v = 0;
	int16_t light = 0;

	while (1)
	{
		ADCSRA |= _BV(ADSC); // wykonaj konwersję
        while (!(ADCSRA & _BV(ADIF))); // czekaj na wynik
        ADCSRA |= _BV(ADIF); // wyczyść bit ADIF (pisząc 1!)
        v = ADC; // weź zmierzoną wartość (0..1023)
        printf("ADC: %"PRIu16, v);
        light = get_light(v);
        printf("ADC: %"PRIu16, v);
        printf("                light: %"PRIu16"\r\n", light);
    
        OCR_LED = light;
	}
}