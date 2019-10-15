 // ------- Preamble -------- //
#include <avr/io.h> /* Defines pins, ports, etc */
#include <util/delay.h> /* Functions to waste time */
#include <inttypes.h>

#define DELAYTIME 300 /* milliseconds */
#define LED_PORT PORTD
#define LED_PIN PIND
#define LED_DDR DDRD

#define DIGITS_COUNT 10

int8_t digits[DIGITS_COUNT] = {
        0b11000000, //0
		0b11111001, //1
		0b10100100, //2
		0b10110000, //3
		0b10011001, //4
		0b10010010, //5
		0b10000010, //6
		0b11111000, //7
		0b10000000, //8
		0b10010000  //9
};

int8_t dot = 0b00000000;
int main(void) {
    UCSR0B &= ~_BV(RXEN0) & ~_BV(TXEN0);
    // -------- Inits --------- //
    LED_DDR = 0xff; /* Data Direction Register B:
    all set up for output */
    // ------ Event loop ------ //
    while (1) {
        for (int i = 0; i < DIGITS_COUNT; ++i)
        {
            LED_PORT = digits[i];
            _delay_ms(DELAYTIME);
        }

        LED_PORT = dot;
        _delay_ms(DELAYTIME * 2);
    
    }
    return (0);
}