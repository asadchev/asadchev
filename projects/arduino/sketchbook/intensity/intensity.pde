/*
  LiquidCrystal Library - Hello World
 
 Demonstrates the use a 16x2 LCD display.  The LiquidCrystal
 library works with all LCD displays that are compatible with the 
 Hitachi HD44780 driver. There are many of them out there, and you
 can usually tell them by the 16-pin interface.
 
 This sketch prints "Hello World!" to the LCD
 and shows the time.
 
  The circuit:
  * LCD RS pin to digital pin 12
  * LCD Enable pin to digital pin 11
  * LCD D4 pin to digital pin 5
  * LCD D5 pin to digital pin 4
  * LCD D6 pin to digital pin 3
  * LCD D7 pin to digital pin 2
  * 10K resistor:
  * ends to +5V and ground
  * wiper to LCD VO pin (pin 3)
 
 Library originally added 18 Apr 2008
 by David A. Mellis
 library modified 5 Jul 2009
 by Limor Fried (http://www.ladyada.net)
 example added 9 Jul 2009
 by Tom Igoe
 modified 8 Feb 2010
 by Tom Igoe
 
 This example code is in the public domain.

 http://www.arduino.cc/en/Tutorial/LiquidCrystal
*/

// include the library code:
#include <LiquidCrystal.h>

// initialize the library with the numbers of the interface pins
LiquidCrystal lcd(12, 11, 5, 4, 3, 2);

void setup() {
    // set up the LCD's number of columns and rows: 
    lcd.begin(16, 2);
    // Print a message to the LCD.
    lcd.print("hello, world!");
}

typedef char string2[3];
const string2& format(int time) {
    static string2 data;    
    return data;
}

template<typename T>
void print_time(T &output, const int (&time)[3]) {
    for (int i = 0; i < 3; ++i) {
	if (time[i] < 10) output.print("0");
	output.print(time[i]);
	if (i < 2) output.print(":");
    }
} 

int seconds0 = 4578;
int analog_in = 0;

void loop() {

    delay(200);
    lcd.setCursor(0, 0);
    lcd.print("intensity: ");
    int value = analogRead(analog_in);
    lcd.print(value);

    // set the cursor to column 0, line 1
    // (note: line 1 is the second row, since counting begins with 0):
    lcd.setCursor(0, 1);
    // print the number of seconds since reset:

    int seconds = seconds0 + millis()/1000;
    int time[3];
    time[2] = seconds%60;
    time[1] = (seconds/60)%60;
    time[0] = (seconds/3600)%24;
    /* if (hour > 23) hour = 0; */

    /* lcd.print(format(hour)); */
    /* lcd.print(":"); */
    /* lcd.print(format(minute)); */
    /* lcd.print(":"); */
    /* lcd.print(format(second)); */
    print_time(lcd, time);


}

