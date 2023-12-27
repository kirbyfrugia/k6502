// Wiring
// Components:
// c64 keyboard (c64kb)
// Arduino (ar)
// Breadboard 6522 (via)
// Shift Register to Via (srVia)
// Shift Register to c64 keyboard (srC64)
// c64kb01         - GND
// c64kb02         - Float (unused)
// c64kb03         - GND - Restore (may implement sometime)
// c64kb04         - 5V
// c64kb05(portb3) - arD12
// c64kb06(portb6) - arD11
// c64kb07(portb5) - arD10
// c64kb08(portb4) - arD09
// c64kb09(portb7) - arD08
// c64kb10(portb2) - arD07
// c64kb11(portb1) - arD06
// c64kb12(portb0) - arD05
// c64kb13(porta0) - srC64QH
// c64kb14(porta6) - srC64QB
// c64kb15(porta5) - srC64QC
// c64kb16(porta4) - srC64QD
// c64kb17(porta3) - srC64QE
// c64kb18(porta2) - srC64QF
// c64kb19(porta1) - srC64QG
// c64kb20(porta7) - srC64QA
// srViaQA         - via2(PA0)
// srViaQB         - via3(PA1)
// srViaQC         - via4(PA2)
// srViaQD         - via5(PA3)
// srViaQE         - via6(PA4)
// srViaQF         - via7(PA5)
// srViaQG         - via8(PA6)
// srViaQH         - via9(PA7)
// arA0            - via40(CA1)
const char ROWS[] = {5, 6, 7, 12, 9, 10, 11, 8};
int charsPrintedSinceReturn = 0;

// map of chars, x-index is the row, y-index is the column
const char charmap_c64ascii[8][8] = {
  {'?','3','5','7','9','+','?','1'},
  {'?','W','R','Y','I','P','*','?'},
  {'?','A','D','G','J','L',';','?'},
  {'?','4','6','8','0','-','?','2'},
  {'?','Z','C','B','M','.','?',' '},
  {'?','S','F','H','K',':','=','?'},
  {'?','E','T','U','O','@','?','Q'},
  {'?','?','X','V','N',',','/','?'}
};

// Map of c64 keyboard to ps/2 scancodes.
// for c64 keys that don't have equivalents,
// use keys that don't exist on the c64.
// Also, avoided using any extended codes that
// require sending more scancodes.
//
// Here are some weirdly mapped keys:
// Left arrow -> Escape (0x76)
// Run/stop -> Scroll Lock (0x7e)
// GBP -> F2 (0x06)
// cursor left/right -> F4 (0x0c)
// clear/home -> F6 (0x0b)
// : -> F8 (0x0a)
// commodore -> F9 (0x01)
// @ -> F10 (0x09)
// up arrow -> F11 (0x78)
// cursor up/down -> F12 (0x07)
// shift-up, not sent but used on the breadboard (0x58 left, 0x77 right)
//
// And here are some control keys we won't print
// Left arrow -> Escape (0x76)
// Return (0x5a)
// Control (0x14)
// Cursor Left/Right (0x0c)
// Clear/Home (0x0b)
// F7 (0x83)
// F1 (0x05)
// F3 (0x04)
// F5 (0x03)
// Run/Stop (0x7e)
// Cursor Up/Down (0x07)
const int charmap_ps2[8][8] = {
  {0x66,0x26,0x2e,0x3d,0x46,0x79,0x06,0x16},
  {0x5a,0x1d,0x2d,0x35,0x43,0x4d,0x7c,0x76},
  {0x0c,0x1c,0x23,0x34,0x3b,0x4b,0x4c,0x14},
  {0x83,0x25,0x36,0x3e,0x45,0x7b,0x0b,0x1e},
  {0x05,0x1a,0x21,0x32,0x3a,0x49,0x59,0x29},
  {0x04,0x1b,0x2b,0x33,0x42,0x0a,0x55,0x01},
  {0x03,0x24,0x2c,0x3c,0x44,0x09,0x78,0x15},
  {0x07,0x12,0x22,0x2a,0x31,0x41,0x4a,0x7e}
};

unsigned long keys_pressed[8][8];
unsigned long keys_repeat[8][8];

#define SR_SER   4
#define SR_RCLK  3
#define SR_SRCLK 2
#define KB_IRQ   A0

// how often to scan for keypresses in microseconds
// about 60Hz give or take the time to send
#define SCAN_FREQUENCY_US 15880
// how long to hold the line inactive between scan codes (20-50us)
#define SCANCODE_INACTIVE_US 40
// number of scans to make before entering repeat mode per key
#define REPEAT_DELAY_SCANS 60
// number of scans to make between re-sending the key in repeat mode
#define REPEAT_RATE_SCANS 30

byte currentOutputToKeyboard = 255;
byte currentOutputToVia      = 0;
byte releaseKey              = 240;

void setup() {

  for (int i = 0; i < 8; ++i) {
    for (int j = 0; j < 8; ++j) {
      keys_pressed[i][j] = 0;
      keys_repeat[i][j] = 0;
    }
  }
  for (unsigned int i = 0; i < 8; ++i) {
    pinMode(ROWS[i], INPUT_PULLUP);
  }

  digitalWrite(SR_SER, LOW);
  digitalWrite(SR_RCLK, LOW);
  digitalWrite(SR_SRCLK, LOW);

  pinMode(SR_SER, OUTPUT);
  pinMode(SR_RCLK, OUTPUT);
  pinMode(SR_SRCLK, OUTPUT);

  digitalWrite(KB_IRQ, HIGH);
  pinMode(KB_IRQ, OUTPUT);

  Serial.begin(57600);
  while (!Serial) {
    ;
  }
  Serial.println("-----------------------------");

}

void writeShiftRegisters() {
  shiftOut(SR_SER, SR_SRCLK, LSBFIRST, currentOutputToKeyboard);
  shiftOut(SR_SER, SR_SRCLK, MSBFIRST, currentOutputToVia);

  // pulse to latch the data
  digitalWrite(SR_RCLK, LOW);
  digitalWrite(SR_RCLK, HIGH);
  digitalWrite(SR_RCLK, LOW);
}

void printBits(byte data) {
  for (int i = 7; i >= 0; --i) {
    int bit = data >> i;
    bit = bit & 1 ? 1 : 0;
    Serial.print(bit);
  }
}

void printColumnKeys(byte column) {
  for (int i = 7; i >= 0; --i) {
    Serial.print(charmap_c64ascii[i][column]);
  }
}

// actually send
void sendScancodeAndDelay() {
  digitalWrite(KB_IRQ, LOW);
  // Write out the data to the shift registers
  writeShiftRegisters();

  // Send interrupt to VIA
  digitalWrite(KB_IRQ, HIGH);

  // Keep the line low to give time to process
  delayMicroseconds(SCANCODE_INACTIVE_US);
}

void sendCharacterKey(byte c64Key, byte ps2Key) {
  currentOutputToVia = ps2Key;
  sendScancodeAndDelay();
}

void sendReleaseCharacterKey(byte c64Key, byte ps2Key) {
  currentOutputToVia = releaseKey;
  sendScancodeAndDelay();
  currentOutputToVia = ps2Key;
  sendScancodeAndDelay();
}

void handleKeyPressed(byte row, byte column) {
  byte c64Key = charmap_c64ascii[row][column];
  byte ps2Key = charmap_ps2[row][column];
  unsigned long pressed = keys_pressed[row][column];
  unsigned long repeat = keys_repeat[row][column];

  if (pressed == 0) {
    keys_pressed[row][column] = 1;
    keys_repeat[row][column] = 0;
    sendCharacterKey(c64Key, ps2Key);
    return;
  }

  if (pressed < REPEAT_DELAY_SCANS) {
    ++pressed;
    keys_pressed[row][column] = pressed;
  }

  if (pressed >= REPEAT_DELAY_SCANS && repeat == 0) {
    repeat = REPEAT_RATE_SCANS;
  }

  if (repeat < REPEAT_RATE_SCANS) {
    ++repeat;
  }
  else {
    sendCharacterKey(c64Key, ps2Key);
    repeat = 0;
  }

  keys_repeat[row][column] = repeat;
}

void handleKeyNotPressed(byte row, byte column) {
  byte c64Key = charmap_c64ascii[row][column];
  byte ps2Key = charmap_ps2[row][column];
  unsigned long pressed = keys_pressed[row][column];

  if (pressed >= 1) {
    keys_pressed[row][column] = 0;
    keys_repeat[row][column] = 0;
    sendReleaseCharacterKey(c64Key, ps2Key);
  }
}

byte scanColumn(byte column) {
  // flip 1s to 0s and vice versa
  currentOutputToKeyboard = 255 - pow(2, column);
  writeShiftRegisters();

  // read all the rows
  byte rows = 0;
  for (int i = 7; i >= 0; --i) {
    // these bits are active low, so key pressed if bit is zero
    int bit = digitalRead(ROWS[i]);
    rows = (rows << 1) | bit;
 
    if (!bit) {
      handleKeyPressed(i, column);
    }
    else {
      handleKeyNotPressed(i, column);
    }
  }
  return rows;
}

void printColumnAndDelay(byte column, byte rowBits) {
  byte columnBits = 255 - pow(2, column);
  Serial.print(column);
  Serial.print(": ");
  printBits(columnBits);
  Serial.print("  ");
  printBits(rowBits);
  Serial.print("  ");
  printColumnKeys(column);
  Serial.println();

  delay(2000);
}

void scanKeys() {
  for (int i = 0; i < 8; ++i) {
    byte rows = scanColumn(i);
    //printColumnAndDelay(i, rows);
  }
  //Serial.println("-------------------");
}

void loop() {
  scanKeys();
  delayMicroseconds(SCAN_FREQUENCY_US);
}
