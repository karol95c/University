#include <avr/io.h>
#include <util/delay.h>
#include <avr/interrupt.h>
#include <avr/sleep.h>

#define LED PB4
#define LED_DDR DDRB
#define LED_PORT PORTB

#define BTN PD3
#define BTN_PIN PIND
#define BTN_PORT PORTD

#define TIMER_LIMIT 99


static int8_t buff[100] = {0};
static int8_t i = 0;

static void init_interrupt()
{
	TCCR1B |= (1 << WGM12);			
	TCCR1B |= (1 << CS11) | (1 << CS10); //prescaler 8
	// 100Hz = (16 * 10^6 Hz) / (8 * (1 + 2499)
	TIMSK1 |= (1 << OCIE1A);
	OCR1A = 2499;
	TCNT1 = 0;
	sei();
}

ISR(TIMER1_COMPA_vect)					// timer interrupt vector
{
	if (buff[i])
	{
		LED_PORT |= (1 << LED);
		buff[i] = 0;
	}
	else
	{
		LED_PORT &= ~(1 << LED);
	}

	buff[i] = !(BTN_PIN & (1 << BTN));

	i++;
	if (i > TIMER_LIMIT)
	{
		i = 0;
	}
}

int main()
{
	BTN_PORT |= (1 << BTN);
	LED_DDR |= (1 << LED);

	init_interrupt();
	set_sleep_mode(SLEEP_MODE_IDLE);

	while (1)
	{
		sleep_mode();
	}
}