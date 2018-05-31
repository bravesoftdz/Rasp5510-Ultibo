program LCD5110_TinyFont_View;

// LCD5110_TinyFont_View

// This program is a demo of the tiniest font.

// This program requires a Nokia 5110 LCD module.

uses
  RaspberryPi,
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  SysUtils,
  Classes,
  Ultibo,
  Console,
  LCD5110_Graph,
  DefaultFonts;

var
  myGLCD: TLCD5110;
  Handle: TWindowHandle;

  procedure Setup();
  begin
    {Let's create a console window again but this time on the left side of the screen}
    Handle := ConsoleWindowCreate(ConsoleDeviceGetDefault, CONSOLE_POSITION_FULL, True);

    {To prove that worked let's output some text on the console window}
    ConsoleWindowWriteLn(Handle, 'Welcome to Example NOKIA 5110 display test');

    try
      myGLCD := TLCD5110.Create(stBCM2835, $3F);
      myGLCD.SetFont(TinyFont);
    except
      on E: Exception do
      begin
        myGLCD := nil;
        ConsoleWindowWriteLn(Handle, 'Setup() error: ' + E.Message);
      end;
    end;
  end;

  procedure Loop();
  begin
    try
      myGLCD.ClrScr();
      myGLCD.Print(' !"#$%&''()*+,-./', CENTER, 0);
      myGLCD.Print('0123456789:;<=>?', CENTER, 6);
      myGLCD.Print('@ABCDEFGHIJKLMNO', CENTER, 12);
      myGLCD.Print('PQRSTUVWXYZ[\\]^_', CENTER, 18);
      myGLCD.Print('`abcdefghijklmno', CENTER, 24);
      myGLCD.Print('pqrstuvwxyz{|}~ ', CENTER, 30);
      myGLCD.Update();

      while True do ;
    except
      on E: Exception do
      begin
        myGLCD := nil;
        ConsoleWindowWriteLn(Handle, 'Loop() error: ' + E.Message);
      end;
    end;
  end;

begin
  Setup();

  while Assigned(myGLCD) do
    Loop();

  ConsoleWindowWriteLn(Handle, #13#10'Bye');

  {Halt the main thread if we ever get to here}
  ThreadHalt(0);
end.

