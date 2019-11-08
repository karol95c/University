 // ------- Preamble -------- //
#include <avr/io.h> /* Defines pins, ports, etc */
#include <util/delay.h> /* Functions to waste time */
#include <inttypes.h>

#define DELAYTIME 5 /* milliseconds */
#define LED_PORT PORTD
#define LED_PIN PIND
#define LED_DDR DDRD

#define DIGITS_PORT PORTC
#define DIGITS_DDR DDRC
#define DIGIT_TIME 5

#define SECOND 1000
#define NUMBERS_COUNT 59

int8_t digits[10] = {
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
    int unit = 0;
    int decimal = 0;
    int time = 0;
    UCSR0B &= ~_BV(RXEN0) & ~_BV(TXEN0);
    // -------- Inits --------- //
    LED_DDR = 0xff; /* Data Direction Register B:
    all set up for output */
    // ------ Event loop ------ //
    DIGITS_DDR = 0xff;
    while (1) {
        if (time > SECOND)
        {
            if (9 == unit)
            {
                decimal = (decimal + 1) % 6;
            }
            unit = (unit + 1) % 10;
            time = 0;
        }
        DIGITS_PORT = 0b00000001;
        LED_PORT = digits[unit];
        _delay_ms(DELAYTIME);
        DIGITS_PORT = 0b00000010;
        LED_PORT = digits[decimal];
        _delay_ms(DELAYTIME);
        time += 2 * DELAYTIME;
    }
    return (0);
}