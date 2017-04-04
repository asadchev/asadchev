
int ledPin = 13;    // LED connected to digital pin 13


// constants won't change. They're used here to 
// set pin numbers:
const int buttonPin = 2;     // the number of the pushbutton pin

// variables will change:
int buttonState = 0;         // variable for reading the pushbutton status

void setup() {
    // initialize the LED pin as an output:
    pinMode(ledPin, OUTPUT);      
    // initialize the pushbutton pin as an input:
    pinMode(buttonPin, INPUT);     
    Serial.begin(9600);

}

// void loop(){
//     // read the state of the pushbutton value:
//     buttonState = digitalRead(buttonPin);

//     // check if the pushbutton is pressed.
//     // if it is, the buttonState is HIGH:
//     if (buttonState == HIGH) {     
// 	// turn LED on:    
// 	digitalWrite(ledPin, HIGH);  
// 	Serial.println("on");
//     } 
//     else {
// 	// turn LED off:
// 	digitalWrite(ledPin, LOW); 
//     }
// }

int time = 1;

void loop() {
    for (int i = 0; i <= 255; ++i) {
	analogWrite(ledPin, i);
	//analogWrite(ledPin+1, 255-i);
	delay(time);                  // wait for a second
    }
    for (int i = 255; i >= 0;  --i) {
	analogWrite(ledPin, i);
	//analogWrite(ledPin+1, 255-i);
	delay(time);                  // wait for a second
    }
}
