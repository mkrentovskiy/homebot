#ifndef _h801_h
#define _H801_h

#define PWM_VALUE 63

int gamma_table[PWM_VALUE + 1] = {
  0, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 4, 4, 5, 5, 6, 6, 7, 8, 9, 10,
  11, 12, 13, 15, 17, 19, 21, 23, 26, 29, 32, 36, 40, 44, 49, 55,
  61, 68, 76, 85, 94, 105, 117, 131, 146, 162, 181, 202, 225, 250,
  279, 311, 346, 386, 430, 479, 534, 595, 663, 739, 824, 918, 1023
};

#define R_PIN     15
#define G_PIN     13
#define B_PIN     12

#define W1_PIN    14
#define W2_PIN    4

#define LED1_PIN  5
#define LED2_PIN  1

#define LED1_OFF  digitalWrite(LED1_PIN, HIGH)
#define LED1_ON   digitalWrite(LED1_PIN, LOW)
#define LED2_OFF  digitalWrite(LED2_PIN, HIGH)
#define LED2_ON   digitalWrite(LED2_PIN, LOW)

#define TIME_AT   1300 

#define SZ    5

#define P_R   0 
#define P_G   1
#define P_B   2
#define P_W1  3
#define P_W2  4

int pins[SZ] = { R_PIN, G_PIN, B_PIN, W1_PIN, W2_PIN };

unsigned long times[SZ];
int delays[SZ];
int target[SZ];
int current[SZ];

void device_init()
{
  pinMode(LED1_PIN, OUTPUT);  
  pinMode(LED2_PIN, OUTPUT);  
  
  for(int i = 0; i < SZ; i++) pinMode(pins[i], OUTPUT);
}

void device_transition() 
{  
  unsigned long m = millis();

  for(int i = 0; i < SZ; i++) {
    if((m - times[i]) >= delays[i]) {
      times[i] = m;
      if(current[i] != target[i]) {
        if(current[i] > target[i]) current[i]--;
        if(current[i] < target[i]) current[i]++;
        analogWrite(pins[i], current[i]);
      }
    }
  }
}
 
int val_to_gamma(int v) 
{
  return gamma_table[constrain(map(v, 0, 255, 0, PWM_VALUE), 0, PWM_VALUE)];
}

int val_to_range(int v) 
{
  return constrain(map(v, 0, 255, 0, 1023), 0, 1023);
}

void change(int r, int g, int b, int w1, int w2)
{
  target[P_R] = val_to_gamma(r);
  target[P_G] = val_to_gamma(g);
  target[P_B] = val_to_gamma(b);
  target[P_W1] = val_to_range(w1); 
  target[P_W2] = val_to_range(w2); 

  for(int i = 0; i < SZ; i++) {
    int diff = abs(current[i] - target[i]);
    delays[i] = TIME_AT / (diff > 0 ? diff : 1023); 
  }
}

int char_to_int(char C) {
  return (C >= 0x30 && C <= 0x39) ? C - 0x30 : ( (C >= 0x41 && C <= 0x46) ? C - 0x37 : 0);
}

int pair_to_int(char T, char B) 
{
  return (char_to_int(T) << 4) + char_to_int(B); 
}

#endif
