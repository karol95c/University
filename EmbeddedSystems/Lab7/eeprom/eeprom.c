#include <avr/io.h>
#include <stdio.h>
#include <inttypes.h>
#include "i2c.h"
#include <util/delay.h>

#define BAUD 9600                          // baudrate
#define UBRR_VALUE ((F_CPU)/16/(BAUD)-1)   // zgodnie ze wzorem

#define WRITE_CALL 1
#define READ_CALL 0
#define UNKNOWN_CALL 2
#define WRITE_DELAY 100

const uint8_t eeprom_addr = 0xa0;

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

  #define i2cCheck(code, msg) \
    if ((TWSR & 0xf8) != (code)) { \
      printf(msg " failed, status: %.2x\r\n", TWSR & 0xf8); \
      i2cReset(); \
    }

void write(int16_t addr, uint8_t data)
{
	  i2cStart();
    i2cCheck(0x08, "I2C start writing");
    i2cSend(eeprom_addr);
    i2cCheck(0x18, "I2C EEPROM write request");
    i2cSend(addr);
    i2cCheck(0x28, "I2C EEPROM set address");
	  i2cSend(data);
    i2cCheck(0x28, "I2C EEPROM write value");
    i2cStop();
    i2cCheck(0xf8, "I2C stop writing");
    _delay_ms(WRITE_DELAY);
}

void read(int16_t addr)
{
    i2cStart();
    i2cCheck(0x08, "I2C start")
    i2cSend(eeprom_addr | ((addr & 0x100) >> 7));
    i2cCheck(0x18, "I2C EEPROM write request")
    i2cSend(addr & 0xff);
    i2cCheck(0x28, "I2C EEPROM set address")
    i2cStart();
    i2cCheck(0x10, "I2C second start")
    i2cSend(eeprom_addr | 0x1 | ((addr & 0x100) >> 7));
    i2cCheck(0x40, "I2C EEPROM read request")
    uint8_t data = i2cReadNoAck();
    i2cCheck(0x58, "I2C EEPROM read")
    i2cStop();
    i2cCheck(0xf8, "I2C stop")
    printf("Address %.3x data:%d\r\n", addr, data);
}

void read_from_address()
{
    int16_t address;
  	printf("Address to read: ");
		scanf("%x", &address);
		printf("Read from: %x\r\n", address);
		read(address);
}

void write_to_address()
{
  	int16_t address;
    uint8_t data;
		printf("Address to write: \r\n");
		scanf("%x", &address);
    printf("Data to write to address: %x: \r\n", address);
		scanf("%d", &data);
		printf("write %d to given address \r\n", data);
    write(address, data);
}

FILE uart_file;

int main()
{
  // zainicjalizuj UART
  uart_init();
  // skonfiguruj strumienie wejścia/wyjścia
  fdev_setup_stream(&uart_file, uart_transmit, uart_receive, _FDEV_SETUP_RW);
  stdin = stdout = stderr = &uart_file;
  // zainicjalizuj ADC
  // mierz napięcie
  i2cInit();
  // program testowy

  char input[16];
  int8_t type;
	while(1)
	{
    type = UNKNOWN_CALL;
    printf("Type read or write:\r\n");
		scanf("%s", &input);
    if (!strcmp(input, "read"))
    {
      type = READ_CALL;
    }
    else if (!strcmp(input, "write"))
    {
      type = WRITE_CALL;
    }

    switch(type)
    {
      case READ_CALL:
        read_from_address();
        break;
      case WRITE_CALL:
        write_to_address();
        break;
      default :
        printf("Unknown option\r\n");
        break;
    }
    printf("---------------------------------------------\r\n");

	}
}