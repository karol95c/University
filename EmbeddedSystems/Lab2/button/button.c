#include <avr/io.h>
#include <util/delay.h>
#include <inttypes.h>
#include <string.h>

#define LED PB5
#define LED_DDR DDRB
#define LED_PORT PORTB

#define BTN PB4
#define BTN_PIN PINB
#define BTN_PORT PORTB

int main() {
    uint8_t buffer[100];
    uint8_t i = 0;
    BTN_PORT |= _BV(BTN);
    LED_DDR |= _BV(LED);
    memset(buffer, 0, sizeof(uint8_t)*100);

    while (1) {
        LED_PORT |= _BV(LED);
        if (buffer[i])
        {
            LED_PORT &= ~_BV(LED);
            buffer[i] = 0;
        }
        if (BTN_PIN & _BV(BTN))
        {
            buffer[i] = 1;
        }
        if (i == 99){
            i = 0;
        }
        i++;
        _delay_ms(10);
    }
}