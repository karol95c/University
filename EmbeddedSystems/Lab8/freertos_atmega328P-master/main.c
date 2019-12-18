/******************************************************************************
 * Header file inclusions.
 ******************************************************************************/

#include "FreeRTOS.h"
#include "task.h"

#include <avr/io.h>


#include <stdio.h>
#include "uart.h"

/******************************************************************************
 * Private macro definitions.
 ******************************************************************************/
#define INCLUDE_vTaskDelay 1
#define mainLED_TASK_PRIORITY   2

#define mainSERIAL_TASK_PRIORITY 1
#define KNIGHT_DELAY 100
#define LED_PORT PORTD
#define LED PB5
#define BTN PB4
#define LED_DDR DDRD

/******************************************************************************
 * Private function prototypes.
 ******************************************************************************/

static void vButtonLed(void* pvParameters);

static void vKnightLed(void* pvParameters);

/******************************************************************************
 * Public function definitions.
 ******************************************************************************/

/**************************************************************************//**
 * \fn int main(void)
 *
 * \brief Main function.
 *
 * \return
 ******************************************************************************/
int main(void)
{
    // Create task.
    xTaskHandle button_handle;
    xTaskHandle knight_handle;

    xTaskCreate
        (
         vButtonLed,
         "button",
         configMINIMAL_STACK_SIZE,
         NULL,
         mainLED_TASK_PRIORITY,
         &button_handle
        );
    
    xTaskCreate
        (
         vKnightLed,
         "knight_led",
         configMINIMAL_STACK_SIZE,
         NULL,
         mainLED_TASK_PRIORITY,
         &knight_handle
        );

    // xTaskCreate
    //     (
    //      vSerial,
    //      "serial",
    //      configMINIMAL_STACK_SIZE,
    //      NULL,
    //      mainSERIAL_TASK_PRIORITY,
    //      &serial_handle
    //     );

    // Start scheduler.
    vTaskStartScheduler();

    return 0;
}

/**************************************************************************//**
 * \fn static vApplicationIdleHook(void)
 *
 * \brief
 ******************************************************************************/
void vApplicationIdleHook(void)
{

}

/******************************************************************************
 * Private function definitions.
 ******************************************************************************/

/**************************************************************************//**
 * \fn static void vButtonLed(void* pvParameters)
 *
 * \brief
 *
 * \param[in]   pvParameters
 ******************************************************************************/
static void vButtonLed(void* pvParameters)
{
    uint8_t buffer[100];
    uint8_t i = 0;

    DDRB |= _BV(LED);
    PORTB |= _BV(BTN);

    memset(buffer, 0, sizeof(uint8_t)*100);
    while (1) {
        PORTB |= _BV(LED);
        if (buffer[i])
        {
            PORTB &= ~_BV( LED);
            buffer[i] = 0;
        }
        if (PINB & _BV(BTN))
        {
            buffer[i] = 1;
        }
        if (i == 99){
            i = 0;
        }
        i++;
        vTaskDelay(10/portTICK_PERIOD_MS);
    }
}

static void vKnightLed(void* pvParameters)
{
    uint8_t i = 0;
    UCSR0B &= ~_BV(RXEN0) & ~_BV(TXEN0);
    LED_DDR = 0xff;

    DDRB |= _BV(LED);

    while (1) {
        while (i < 7) {
            LED_PORT = (1 << i); /* illuminate only i'th pin */
            vTaskDelay(KNIGHT_DELAY/portTICK_PERIOD_MS); /* wait */
        i = i + 1; /* move to the next LED */
        }
        while (i > 0) {
            LED_PORT = (1 << i); /* illuminate only i'th pin */
            vTaskDelay(KNIGHT_DELAY/portTICK_PERIOD_MS); /* wait */
        i = i - 1; /* move to the previous LED */
        }
    } /* End event loop */
}



/**************************************************************************//**
 * \fn static void vSerial(void* pvParameters)
 *
 * \brief
 *
 * \param[in]   pvParameters
 ******************************************************************************/
static void vSerial(void* pvParameters)
{
    uart_init();
    stdin = stdout = stderr = &uart_file;

    char input;

    for ( ;; )
    {
        puts("Hello World\r\n");
        input = getchar();
        printf("You wrote %c\r\n", input); 
    }
}
