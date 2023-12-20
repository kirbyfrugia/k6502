const char DATA[] = { 12, 11, 10, 9, 8, 7, 6, 5};

#define E  2
#define RW 3
#define RS 4

int lastRead[11];
byte lastReadByte = 0;

void setup()
{
  pinMode(E, INPUT_PULLUP);
  pinMode(RW, INPUT_PULLUP);
  pinMode(RS, INPUT_PULLUP);

  for (int i = 0; i < 8; i += 1) {
    pinMode(DATA[i], INPUT_PULLUP);
  }

  for (int i = 0; i < 11; ++i) {
    lastRead[i] = 35; // made up number
  }

  Serial.begin(57600);

  Serial.println("Waiting for E to trigger...");

}

void print() {
  char buf[100];
  sprintf(buf, "E:%d RW:%d RS:%d Data: %d%d%d%d%d%d%d%d Char :%c Int:%d Time: %lu", lastRead[0], lastRead[1], lastRead[2],
          lastRead[3], lastRead[4], lastRead[5], lastRead[6], lastRead[7],
          lastRead[8], lastRead[9], lastRead[10], lastReadByte, lastReadByte, millis());
  Serial.println(buf);
}

void loop() {

  bool changed = false;

  int enableBit = digitalRead(E) ? 1 : 0;
  int rwBit = digitalRead(RW) ? 1 : 0;
  int rsBit = digitalRead(RS) ? 1 : 0;

  if (enableBit != lastRead[0]) {
    changed = true;
    lastRead[0] = enableBit;
  }
  if (rwBit != lastRead[1]) {
    changed = true;
    lastRead[1] = rwBit;
  }
  if (rsBit != lastRead[2]) {
    changed = true;
    lastRead[2] = rsBit;
  }

  lastReadByte = 0;
  for (int i = 0; i < 8; i += 1){
    int bit = digitalRead(DATA[i]) ? 1 : 0;
    lastReadByte = (lastReadByte << 1) + bit;
    if (lastRead[i+3] != bit) {
      changed = true;
      lastRead[i+3] = bit;
    }
  }

  if (changed) {
    print();
  }
}
