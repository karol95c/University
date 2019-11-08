#include <avr/io.h>
#include <avr/pgmspace.h>
#include <util/delay.h>

#define BUZZ PB4
#define BUZZ_DDR DDRB
#define BUZZ_PORT PORTB

#define TONE(step, delay) \
    for (uint16_t i = 0; i < (uint32_t)1000 * (delay) / (step) / 2; i++) { \
      BUZZ_PORT |= _BV(BUZZ); \
      _delay_us(step); \
      BUZZ_PORT &= ~_BV(BUZZ); \
      _delay_us(step); \
    }


static void delay_us(uint16_t count) {
  while(count--) {
    _delay_us(1);
  }
}
static void play_tone(int step, int delay)
{
     for (uint16_t i = 0; i < (uint32_t)1000 * (delay) / (step) / 2; i++) {
      BUZZ_PORT |= _BV(BUZZ);
      delay_us(step);
      BUZZ_PORT &= ~_BV(BUZZ);
      delay_us(step);
    }
}
#define C 523
#define D 587
#define E 659
#define F 698
#define G 782

#define PAUSE 0

#define NOTE_TIME 500

#define NOTES_COUNT 19

static const int16_t melody[] PROGMEM =
    {
        G, E, E, F, D, D, C, E, G, PAUSE,
        G, E, E, F, D, D, C, E, C
    };

static void play_note (int16_t note)
{
    if (PAUSE == note)
    {
        delay_us(NOTE_TIME * 2);
        return;
    }
    int16_t p = 100000 / note;
    play_tone(p, NOTE_TIME);
}


int main() {
  BUZZ_DDR |= _BV(BUZZ);

    int16_t note;
    for (int i = 0; i < NOTES_COUNT; ++i)
    {
        note = pgm_read_word(&(melody[i]));
        play_note(note);
    }
}