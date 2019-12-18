#include "hd44780.h"
#include <avr/io.h>
#include <inttypes.h>
#include <stdio.h>
#include <util/delay.h>

#define BAUD 9600                            // baudrate
#define UBRR_VALUE ((F_CPU) / 16 / (BAUD)-1) // zgodnie ze wzorem

#define LINE_SIZE 16
#define LINE_NUM 2
#define UNDERSCORE 95
static char lines[LINE_NUM][LINE_SIZE] = {0};
static uint8_t counts[2] = {0, 0};
volatile int active_row = 0;

// inicjalizacja UART
void uart_init() {
    // ustaw baudrate
    UBRR0 = UBRR_VALUE;
    // włącz odbiornik i nadajnik
    UCSR0B = _BV(RXEN0) | _BV(TXEN0);
    // ustaw format 8n1
    UCSR0C = _BV(UCSZ00) | _BV(UCSZ01);
}

// transmisja jednego znaku
int uart_transmit(char data, FILE *stream) {
    // czekaj aż transmiter gotowy
    while (!(UCSR0A & _BV(UDRE0)))
        ;
    UDR0 = data;
    return 0;
}

// odczyt jednego znaku
int uart_receive(FILE *stream) {
    // czekaj aż znak dostępny
    while (!(UCSR0A & _BV(RXC0)))
        ;
    return UDR0;
}

int hd44780_transmit(char data, FILE *stream) {
    LCD_WriteData(data);
    return 0;
}

void print_line(int row) {
    LCD_Clear();
    if (lines[row][0] != 0) {
        LCD_GoTo(0, row);
        for (int i = 0; i < counts[row]; i++) {
            printf("%c", lines[row][i]);
        }
    }
    LCD_GoTo(0, row);
}

void switch_lines() {
    for (int i = 0; i < LINE_SIZE; ++i) {
        lines[0][i] = lines[1][i];
    }
    lines[1][0] = '\0';
    counts[0] = counts[1];
    counts[1] = 0;
    print_line(0);
}

void backspace() {
    if (counts[active_row] > 0) {
        --counts[active_row];
        lines[active_row][counts[active_row]] = '\0';
        LCD_GoTo(counts[active_row], active_row);
        printf("%c", UNDERSCORE);
        printf(" ");
    } else if (counts[active_row] == 0) {
        if (active_row == 1) {
            LCD_GoTo(0, active_row);
            printf(" ");
            active_row = 0;
            --counts[active_row];
        }
        LCD_GoTo(counts[active_row], active_row);
        printf("%c", UNDERSCORE);
    } else {
        if (active_row == 1) {
            active_row = 0;
            --counts[active_row];
            lines[active_row][counts[active_row]] = '\0';
            LCD_GoTo(counts[active_row], active_row);
            printf("%c", UNDERSCORE);
            printf(" ");
        }
    }
}

void full_line() {
    if (active_row == 1) {
        switch_lines();
    } else {
        ++active_row;
    }
    LCD_GoTo(0, active_row);
    printf("%c", UNDERSCORE);
}

void new_line() {
    if (counts[active_row] < LINE_SIZE) {
        LCD_GoTo(counts[active_row], active_row);
        printf(" ");
    }
    if (active_row == 1) {
        switch_lines();
    } else {
        ++active_row;
    }
    LCD_GoTo(counts[active_row], active_row);
    printf("%c", UNDERSCORE);
}

void handle_screen() {
    volatile char c;
    printf("%c", UNDERSCORE);
    while (1) {
        scanf("%c", &c);
        if (c == 127) {
            backspace();
            continue;
        }

        if (c == 13) {
            new_line();
            continue;
        }

        LCD_GoTo(counts[active_row], active_row);
        printf("%c", c);
        lines[active_row][counts[active_row]] = c;
        ++counts[active_row];
        printf("%c", UNDERSCORE);
        if (counts[active_row] == LINE_SIZE) {
            full_line();
        }
    }
}

FILE hd44780_file;

int main() {
    LCD_Initialize();
    LCD_Clear();
    uart_init();
    fdev_setup_stream(&hd44780_file, hd44780_transmit, uart_receive,
                      _FDEV_SETUP_RW);
    stdout = stdin = stderr = &hd44780_file;
    handle_screen();
}
