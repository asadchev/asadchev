
// include the library code:
#include <LiquidCrystal.h>

// initialize the library with the numbers of the interface pins
LiquidCrystal lcd(12, 11, 5, 4, 3, 2);


struct {
    const double freq[2];
    const double duty[2];
} range = {{ 10e3, 30e3 }, { 0.01, 0.95 }};


double freq;
double duty;

struct {
    static const int freq = 0;
    static const int duty = 1;
    static const int out = 8;    // out connected to digital pin 13
} pin;


void setup() {
    // initialize the LED pin as an output:
    pinMode(pin.freq, INPUT);      
    pinMode(pin.duty, INPUT);
    pinMode(pin.out, OUTPUT);

    lcd.begin(16, 2);
    // Print a message to the LCD.
    lcd.setCursor(0,0);
    lcd.print("Flyback driver");
    lcd.setCursor(0,1);
    lcd.print("KHz/%: ");
    
}

double read_value(int pin, const double (&r)[2]) {
    return r[0] + analogRead(pin)*(r[1] - r[0])/1024;
}

void loop() {

    double freq = read_value(pin.freq, range.freq);
    double duty = read_value(pin.duty, range.duty);

    lcd.setCursor(7,1);
    lcd.print(int(freq/1e3));
    lcd.print("/");
    lcd.print(int(duty*100));
    lcd.print(" ");
    lcd.print(analogRead(pin.freq));

    freq = 1e6/freq;
    duty *= freq;

    digitalWrite(pin.out, HIGH);
    delayMicroseconds(duty);
    digitalWrite(pin.out, LOW);
    delayMicroseconds(freq-duty);

}
