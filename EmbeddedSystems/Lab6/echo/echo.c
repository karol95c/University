#include <avr/io.h>
#include <stdio.h>
#include <inttypes.h>
#include <avr/interrupt.h>
#include <avr/sleep.h>

#define BAUD 9600                            // baudrate
#define UBRR_VALUE ((F_CPU) / 16 / (BAUD)-1) // zgodnie ze wzorem

#define INPUT_SIZE 16
volatile char read_char;

volatile char input[INPUT_SIZE];
volatile static int counter = 0;

void uart_init()
{
    // set baud rate
    UBRR0 = UBRR_VALUE;
    // turn on receiver and transmitter
    UCSR0B = (1 << RXEN0) | (1 << TXEN0);
    // set 8-bit character size for rec. and trans.
    UCSR0C = _BV(UCSZ00) | _BV(UCSZ01);
    // enable interrupt for receiver
    UCSR0B |= (1 << RXCIE0);
    sei();
}

// transmisja jednego znaku
int uart_transmit(char data)
{
  // czekaj aż transmiter gotowy
  while(!(UCSR0A & _BV(UDRE0)));
  UDR0 = data;
  return 0;
}

// odczyt jednego znaku
int uart_receive()
{
  // czekaj aż znak dostępny
  while (!(UCSR0A & _BV(RXC0)));
  return UDR0;
}

// interrupt handler for receiver
ISR(USART_RX_vect)
{
	read_char = uart_receive();
	if(read_char == '\r')
  {
    for (int i = 0; i < counter; ++i)
    {
      uart_transmit(input[i]);
    }
		uart_transmit('\n');
    uart_transmit('\r');
    counter = 0;
  }
  else
  {
    input[counter++] = read_char;
  }
}

int main()
{
	uart_init();						// initialize UART
	set_sleep_mode(SLEEP_MODE_IDLE);	// set sleep mode on idle

	while (1)
	{
		sleep_mode();
	}
}