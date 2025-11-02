/*
* Arduino Wireless Communication Tutorial
*       Example 1 - Receiver Code
*                
* by Dejan Nedelkovski, www.HowToMechatronics.com
* 
* Library: TMRh20/RF24, https://github.com/tmrh20/RF24/
*/

#include <SPI.h>
#include <nRF24L01.h>
#include <RF24.h>

RF24 radio(7, 8); // CE, CSN

const byte address[6] = "00001";

bool transmissionStatus = false;


void setup() 
{
  Serial.begin(115200);
  Serial.setTimeout(100); // ms

  /*radio.begin();
  radio.openReadingPipe(0, address);
  radio.setPALevel(RF24_PA_MIN);
  radio.startListening();*/
}


void loop()
{
  if (Serial.available() > 0) {
    char buffer[64]; // ESP32 Serial Cache is 64 bytes
    const int bytes_read = Serial.readBytes(buffer, 64);
    if (bytes_read > 0 && buffer[0] == 0x01) {
      transmissionStatus = true;
    }
  }

  if (transmissionStatus) {
    Serial.write(0x42);
    Serial.print("tekstiä");
  }

  /*if (radio.available()) {
    char text[32] = "";
    radio.read(&text, sizeof(text));
    Serial.println(text);
  }*/

  delay(1000); // ms
}

