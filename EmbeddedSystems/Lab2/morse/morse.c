#include <avr/io.h>
#include <util/delay.h>
#include <inttypes.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>


#define LED PB5
#define LED_DDR DDRB
#define LED_PORT PORTB

#define BTN PB4
#define BTN_PIN PINB
#define BTN_PORT PORTB

#define BAUD 9600                          // baudrate
#define UBRR_VALUE ((F_CPU)/16/(BAUD)-1)   // zgodnie ze wzorem

#define DOT_TIME 50
#define LINE_TIME (3 * DOT_TIME)
#define NEXT_TIME (3 * DOT_TIME)
#define SPACE_TIME (7 * DOT_TIME)
#define EXIT_TIME (12 * DOT_TIME)
#define ALPHABET_LONG 26
#define NUM_LONG 10
#define ZERO_POS 48
#define A_POS 'A'

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

void append(char* s, char c) {
    printf("append '%c'\r\n", c);
    int len = strlen(s);
    s[len] = c;
    s[len+1] = '\0';
}

void convert_to_char(char *word, char *code)
{
    for (int i = 0; i < ALPHABET_LONG; ++i)
    {
        if (!(strcmp(alpha[i], code)))
        {
            // append(word, i + A_POS);
            char c = i + A_POS;
            append(word, c);
            code[0] = '\0';
            printf ("%c", c);
            return;
        }
    }

    for (int i = 0; i < NUM_LONG; ++i)
    {
        if (!(strcmp(num[i], code)))
        {
            // append(word, i + ZERO_POS);
            char c = i + ZERO_POS;
            append(word, c);
            printf ("%c", c);
            code[0] = '\0';
            return;
        }
    }
    printf("Code did not recognised.");
    code[0] = '\0';
}
FILE uart_file;
int main() {
        BTN_PORT |= _BV(BTN);
    LED_DDR |= _BV(LED);
    int button_on_time = 0;
    int button_off_time = 0;
    char code[8];
    char word[64];
    word[0] = '\0';
    int8_t line_signal = 0;
    int8_t space_signal = 0;

    uart_init();
    // skonfiguruj strumienie wejścia/wyjścia
    fdev_setup_stream(&uart_file, uart_transmit, uart_receive, _FDEV_SETUP_RW);
    stdin = stdout = stderr = &uart_file;

    while (1) {
        LED_PORT &= ~_BV(LED);
        // printf("button_on_time %d\r\n", button_on_time);
        //  printf("button_off_time %d\r\n", button_on_time);
        if (button_on_time > LINE_TIME && !line_signal)
        { 
            LED_PORT |= _BV(LED);
            line_signal = 1;
        }
        else if (button_off_time > NEXT_TIME && strlen(code) > 0)
        {
            // convert code to char
            printf("convert_to_char %s", code);
            convert_to_char(word, code);
            button_on_time = 0;
        }
        else if(button_off_time > SPACE_TIME && !space_signal)
        {
            LED_PORT |= _BV(LED);
            space_signal = 1;
            button_on_time = 0;
        }
        else if(button_off_time > EXIT_TIME)
        {
            printf("EXIT_TIME ELAPSED\r\n");
            printf("Word decoded: %s\r\n", word);
            return;
        }

        if (BTN_PIN & _BV(BTN))
        {  
            if(button_on_time > LINE_TIME)
            {
                // line
                append(code, '-');
            }
            else if (button_on_time > DOT_TIME)
            {
                //dot
                append(code, '.');
            }
            button_on_time = 0;
            button_off_time++;
            line_signal = 0;
        } else
        {
            
            if (button_off_time > SPACE_TIME)
            {
                // append space
                append(word, ' ');
                printf (" ");       
            }
            else if (button_off_time > NEXT_TIME)
            {
                // ready for next code sign
                button_on_time = 0;
                code[0] = '\0';
            }
            button_off_time = 0;
            button_on_time ++;
            space_signal = 0;
        }
        _delay_ms(10);
    }
}