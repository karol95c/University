#include <avr/io.h>
#include <stdio.h>
#include <inttypes.h>
#include <avr/interrupt.h>
#include <util/delay.h>

#define LA DDB1
#define OE DDB2
#define LED_DDR DDRB
#define LED_PORT PORTB

static const int8_t digits[10] = {
	0b10111111, // 0
	0b10000110, // 1
	0b11011011, // 2
	0b11001111, // 3
    0b11100110, // 4
	0b11101101, // 5
	0b11111101, // 6
	0b10000111, // 7
    0b11111111, // 8
	0b11101111  // 9
};

void spi_init()
{
	SPCR |= (1 << SPE) | (1 << SPR1) | (1 << MSTR); // spi | f / 64(250kHz) | master
	DDRB |= (1 << DDB4) | (1 << DDB5); // MISO | SCK
	DDRB |= (1 << LA) | (1 << OE); // SS - LA | OE 
}

void print(int idx)
{
	SPDR = digits[idx];
	while(!bit_is_set(SPSR, SPIF));
}

int main()
{
	LED_DDR |= 0xFF;
	LED_PORT &= ~(1 << OE);
	spi_init();
	int i = 0;
	while(1) {
		if (i > 9){
			i = 0;
		}

		LED_PORT |= (1 << LA);
		print(i);
		LED_PORT &= ~(1 << LA);
		++i;

		_delay_ms(1000);
	}
}