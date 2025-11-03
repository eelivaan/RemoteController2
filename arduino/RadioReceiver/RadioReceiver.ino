/*
* Arduino Wireless Communication Tutorial
*       Example 1 - Receiver Code
*                
* by Dejan Nedelkovski, www.HowToMechatronics.com
* 
* Library: TMRh20/RF24, https://github.com/tmrh20/RF24/
*/

//#define ENABLE_RADIO

#ifdef ENABLE_RADIO
#include <SPI.h>
#include <nRF24L01.h>
#include <RF24.h>

RF24 radio(7, 8); // CE pin, CSN pin

const byte addressR[6] = "00001";
const byte addressW[6] = "00002";
#endif

bool commStatus = false;


void setup() 
{
  Serial.begin(115200);
  Serial.setTimeout(100); // ms

#ifdef ENABLE_RADIO
  radio.begin();
  radio.openWritingPipe(addressW);
  radio.openReadingPipe(1, addressR);
  radio.setPALevel(RF24_PA_MIN); // Power Amplifier level
  
  radio.startListening();
#endif
}


void loop()
{
  if (Serial.available() > 0) 
  {
    char buffer[64]; // ESP32 Serial Data Cache is 64 bytes
    const int bytes_read = Serial.readBytes(buffer, 64);
    if (bytes_read > 0)
    {
      commStatus = (buffer[0] == 0x01);
    }
  }

  if (commStatus) 
  {
    Serial.write(0x42);
    Serial.print("tekstiä");
  }

#ifdef ENABLE_RADIO
  // forward data received from radio to Serial
  if (radio.available())
  {
    char data[32]; // won't read more at once
    radio.read(&data, 32);
    Serial.write(data, ??);
  }
  /*if Should Send Command {
    radio.stopListening();
    radio.write(command, ??);
    delay(5);
    radio.startListening();
  }*/
#endif

  delay(1000); // ms
}

