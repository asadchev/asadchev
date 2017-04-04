#include <Ethernet.h>

// network configuration.  gateway and subnet are optional.
byte mac[] = { 0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED };
byte ip[] = { 10, 0, 0, 177 };
byte gateway[] = { 10, 0, 0, 1 };
byte subnet[] = { 255, 255, 0, 0 };

// telnet defaults to port 23
Server server(23);

void setup() {
    Serial.begin(9600);// opens serial port, sets data rate to 9600 bps

    // initialize the ethernet device
    Ethernet.begin(mac, ip, gateway, subnet);

    // start listening for clients
    server.begin();
}

void loop() {
    if (Serial.available() > 0) {
	char byte = Serial.read();
	server.write(byte);
    }
}
