#include <avr/io.h>
#include <stdio.h>
#include <util/delay.h>
#include <ctype.h>

#define LED PB5
#define LED_DDR DDRB
#define LED_PORT PORTB

#define BAUD 9600                          // baudrate
#define UBRR_VALUE ((F_CPU)/16/(BAUD)-1)   // zgodnie ze wzorem

#define ZERO_POS 48
#define A_POS 'A'

#define DOT_TIME 150

// inicjalizacja UART
void uart_init()
{
  // ustaw baudrate
  UBRR0 = UBRR_VALUE;
  // wyczyść rejestr UCSR0A
  UCSR0A = 0;
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

static const char *alpha[] = {
    ".-",   //A
    "-...", //B
    "-.-.", //C
    "-..",  //D
    ".",    //E
    "..-.", //F
    "--.",  //G
    "....", //H
    "..",   //I
    ".---", //J
    "-.-",  //K
    ".-..", //L
    "--",   //M
    "-.",   //N
    "---",  //O
    ".--.", //P
    "--.-", //Q
    ".-.",  //R
    "...",  //S
    "-",    //T
    "..-",  //U
    "...-", //V
    ".--",  //W
    "-..-", //X
    "-.--", //Y
    "--..", //Z
};
static const char *num[] = {
    "-----", //0
    ".----", //1
    "..---", //2
    "...--", //3
    "....-", //4
    ".....", //5
    "-....", //6
    "--...", //7
    "---..", //8
    "----.", //9
};

void print_morse(const char* morse)
{
    printf ("%s\r\n", morse);
    int i = 0;
    while (morse[i] != '\0')
    {
        printf ("morse[i]: %c", morse[i]);
        if (morse[i] == '-')
        {
            LED_DDR |= _BV(LED);
            LED_PORT |= _BV(LED);
            _delay_ms(3 * DOT_TIME);

        }
        else if (morse[i] == '.')
        {
            LED_DDR |= _BV(LED);
            LED_PORT |= _BV(LED);
            _delay_ms(DOT_TIME);
        }
        LED_PORT &= ~_BV(LED);
        _delay_ms(DOT_TIME);
        ++i;
    }

    

}

void translate_char(char ch)
{
    printf ("char: %c\r\n", ch);
    if (0 != isdigit(ch))
    {
        int index = ch - ZERO_POS;
        print_morse(num[index]);
    }
    else if (0 != isalpha(ch))
    {
        int index = toupper(ch) - A_POS;
        print_morse(alpha[index]);
    }
    LED_PORT &= ~_BV(LED);
    _delay_ms(3 * DOT_TIME);
}
void translate_word(char* word)
{
    int i = 0;
    while (word[i] != '\0')
    {
        if (word[i] == ' ')
        {
            LED_PORT &= ~_BV(LED);
            _delay_ms(4 * DOT_TIME);
        }
        translate_char(word[i]);
        //translate
        ++i;
    }
}
FILE uart_file;

int main() {
uart_init();
  // skonfiguruj strumienie wejścia/wyjścia
  fdev_setup_stream(&uart_file, uart_transmit, uart_receive, _FDEV_SETUP_RW);
  stdin = stdout = stderr = &uart_file;
  char word[64];
  printf("Input a word to change for Morse Code:\r\n");
  scanf ("%[^\r\n]s", &word);
  printf("Word: %s\r\n", word);

  translate_word(word);
//   while (1) {
//     LED_DDR |= _BV(LED);
//             LED_PORT |= _BV(LED);
//             _delay_ms(1000);
//             LED_PORT &= ~_BV(LED);
//             _delay_ms(300);
  }
