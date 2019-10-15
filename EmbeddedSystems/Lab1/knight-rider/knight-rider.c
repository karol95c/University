 // ------- Preamble -------- //
#include <avr/io.h> /* Defines pins, ports, etc */
#include <util/delay.h> /* Functions to waste time */

#define DELAYTIME 100 /* milliseconds */
#define LED_PORT PORTD
#define LED_PIN PIND
#define LED_DDR DDRD

int main(void) {
    UCSR0B &= ~_BV(RXEN0) & ~_BV(TXEN0);
    // -------- Inits --------- //
    uint8_t i;
    LED_DDR = 0xff; /* Data Direction Register B:
    all set up for output */
    // ------ Event loop ------ //
    while (1) {
        while (i < 7) {
        LED_PORT = (1 << i); /* illuminate only i'th pin */
        _delay_ms(DELAYTIME); /* wait */
        i = i + 1; /* move to the next LED */
        }
        while (i > 0) {
        LED_PORT = (1 << i); /* illuminate only i'th pin */
        _delay_ms(DELAYTIME); /* wait */
        i = i - 1; /* move to the previous LED */
        }
    } /* End event loop */
    return (0);
}