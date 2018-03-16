#include <ESP8266WiFi.h>

#define log_init(speed)   Serial1.begin(speed)   
#define log(m)            Serial1.print(m) 
#define logl(m)           Serial1.println(m) 

#include "h801.h"
#include "config.h"

WiFiServer server(80);

void setup()
{
  device_init();
  log_init(115200);

  WiFi.enableAP(IS_AP);
  WiFi.begin(STA_SSID, STA_PWD);

  LED1_ON;
  
  while(WiFi.status() != WL_CONNECTED) {
    LED2_OFF;
    delay(400);
    log(".");
    LED2_ON;
    delay(100);
  }
  
  logl("");
  logl("WiFi connected");
  logl("IP address: ");
  logl(WiFi.localIP());
  
  server.begin();
  logl("Webserver started");
}

void loop()
{
  WiFiClient client = server.available();

  if(client) {
    String req = client.readStringUntil('\r');
    client.flush();
    
    logl(req);
    
    String s = "HTTP/1.1 200 OK\r\n";
    s += "Content-Type: application/json\r\n\r\n";
      
    if((req.indexOf("/rgb/") != -1)) {
      int l = req.length();
      int i = req.indexOf("/rgb/") + 5;

      if(l >= i + 6) {
        int r, g, b;
        String RGB = req.substring(i, i + 6);
        char res[7];
        
        RGB.toUpperCase();
        r = pair_to_int(RGB[0], RGB[1]);
        g = pair_to_int(RGB[2], RGB[3]); 
        b = pair_to_int(RGB[4], RGB[5]); 
        change(r, g, b, 0, 0);
        sprintf(res, "%02x%02x%02x", r, g, b);
        logl(res);
        s += "{ 'rgb': '";
        s += res; 
        s += "' }\n";        
      } else {
        s += "{ 'error': 'invalid_params' }\n";  
      }
    } else {
      s += "{ 'error': 'no_params' }\n";
    }
      
    client.print(s);
    client.flush();
      
    logl("Client disonnected");
  };
  device_transition();  
}
