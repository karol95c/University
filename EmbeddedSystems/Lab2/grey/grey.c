#include <avr/io.h>
#include <util/delay.h>
#include <inttypes.h>
#include <string.h>

#define BTN1 PB4
#define BTN_PIN PINB
#define BTN_PORT PORTB

#define BTN2 PB3
#define BTN_PIN PINB
#define BTN_PORT PORTB

#define BTN3 PB2
#define BTN_PIN PINB
#define BTN_PORT PORTB

#define LED_PORT PORTD
#define LED_PIN PIND
#define LED_DDR DDRD


int8_t next_gray(int8_t i)
{
    return i ^ (i >> 1);
}

int main() {

UCSR0B &= ~_BV(RXEN0) & ~_BV(TXEN0);
    // -------- Inits --------- //
    LED_DDR = 0xff;

    int i = 0;
    uint8_t code = 0b00000000;
    BTN_PORT |= _BV(BTN1);
    BTN_PORT |= _BV(BTN2);
    BTN_PORT |= _BV(BTN3);
    uint8_t b1 = 0;
    uint8_t b2 = 0;
    uint8_t b3 = 0;
    LED_PORT = code;
    while (1) {
        if (!(BTN_PIN & _BV(BTN1)))
        {
            _delay_ms(1);
            if (!(BTN_PIN & _BV(BTN1)))
            {
                b1 = 1;
                code = 0b00000000;
                i = 0;
                LED_PORT = code;

                while (b1)
                {
                    _delay_ms(1);
                    if (BTN_PIN & _BV(BTN1))
                    {
                        b1 = 0;
                    }
                }
            }

        }
        if (!(BTN_PIN & _BV(BTN2)))
        {
            _delay_ms(1);
            if (!(BTN_PIN & _BV(BTN2)))
            {
                b2 = 1;
                if (i <= 1)
                {
                    code = 0b00000000;
                    i = 0;
                }
                else
                {
                    --i;
                    code = next_gray(i);
                }
            LED_PORT = code;

                while (b2)
                {
                    if (BTN_PIN & _BV(BTN2))
                    {
                        _delay_ms(1);
                        if (BTN_PIN & _BV(BTN2))
                        {
                            b2 = 0;
                        }
                    }
                }
            }

            
        }
        if (!(BTN_PIN & _BV(BTN3)))
        {
            _delay_ms(1);
            if (!(BTN_PIN & _BV(BTN3)))
            {
                b3 = 1;
                ++i;
                code = next_gray(i);
                LED_PORT = code;

                while (b3)
                {
                    if (BTN_PIN & _BV(BTN3))
                    {
                        _delay_ms(1);
                        if (BTN_PIN & _BV(BTN3))
                        {
                            b3 = 0;
                        }
                    }
                    
                }
            }


        }

        // _delay_ms(50);
    }
}