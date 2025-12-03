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

bool bScanning = false;
int scanAngle = 0;

void setup() 
{
  //Serial.begin(115200);
  Serial.begin(1000000);
  Serial.setTimeout(100); // ms

  pinMode(25, OUTPUT);
  digitalWrite(25, LOW);

#ifdef ENABLE_RADIO
  radio.begin();
  radio.openWritingPipe(addressW);
  radio.openReadingPipe(1, addressR);
  radio.setPALevel(RF24_PA_MIN); // Power Amplifier level
  
  radio.startListening();
#endif
}


void sendTestScanPacket()
{
  unsigned short angledeg = scanAngle * 64;
  unsigned short distmm = random(1000,3000) * 4;
  byte v0 = 0x00<<2 | 0x00<<1 | 0x01;
  byte v1 = angledeg<<1 | 0x01;
  byte v2 = angledeg>>7;
  byte v3 = distmm;
  byte v4 = distmm>>8;
  const byte buffer[] = {v0,v1,v2,v3,v4};
  Serial.write(buffer, 5);
  //Serial.print("tekstiä");
  delay(50); // ms
  scanAngle = (scanAngle + 1) % 360;
}


void loop()
{
  if (Serial.available() >= 2) 
  {
    //char buffer[64]; // ESP32 Serial Data Cache is 64 bytes
    //const int bytes_read = Serial.readBytes(buffer, 64);
    const int cmdType = Serial.read();
    // Lidar commands
    if (cmdType == 0xA5)
    {
      const int cmd = Serial.read();
      if (cmd == 0x20) {
        const byte response[7] = { 0xA5, 0x5A, 0x05, 0x00, 0x00, 0x40, 0x81 };
        Serial.write(response, 7);
        bScanning = true;        
      } else if (cmd == 0x25) {
        bScanning = false;
      }
    // driving commands
    } else if (cmdType == 0x01) 
    {
      const int cmd = Serial.read();
      if (cmd == 'w') {
        digitalWrite(25, HIGH);
      } else {
        digitalWrite(25, LOW);
      }
    }
  }

  if (bScanning)
  {
    // emulate Lidar scan data packets
    sendTestScanPacket();
  }

#ifdef ENABLE_RADIO
  // forward data received from radio to Serial
  if (radio.available())
  {
    char data[32]; // won't read more at once
    radio.read(&data, 32);
    //Serial.write(data, ??);
  }
  /*if Should Send Command {
    radio.stopListening();
    radio.write(command, ??);
    delay(5);
    radio.startListening();
  }*/
#endif

  delay(10); // ms
}

