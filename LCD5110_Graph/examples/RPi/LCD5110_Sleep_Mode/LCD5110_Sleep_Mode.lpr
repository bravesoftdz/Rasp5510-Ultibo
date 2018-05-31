program LCD5110_Sleep_Mode;

// LCD5110_Sleep_Mode

// This program is a demo of sleep mode.

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
      myGLCD.SetFont(SmallFont);
      Randomize;
    except
      on E: Exception do
      begin
        myGLCD := nil;
        ConsoleWindowWriteLn(Handle, 'Setup() error: ' + E.Message);
      end;
    end;
  end;

  procedure Loop();
  var
    s: integer;
  begin
    try
      myGLCD.SetFont(SmallFont);
      myGLCD.ClrScr();
      myGLCD.Print('Entering', CENTER, 0);
      myGLCD.InvertText(True);
      myGLCD.Print(' Sleep Mode ', CENTER, 8);
      myGLCD.InvertText(False);
      myGLCD.Print('in', CENTER, 16);
      myGLCD.Print('seconds', CENTER, 40);
      myGLCD.Update();

      myGLCD.SetFont(MediumNumbers);

      for s := 10 downto 0 do
      begin
        myGLCD.PrintNumI(s, CENTER, 24, 2, '0');
        myGLCD.Update();
        Sleep(1000);
      end;

      myGLCD.EnableSleep();

      myGLCD.ClrScr();
      myGLCD.SetFont(SmallFont);
      myGLCD.Print('Awake again!', CENTER, 0);
      myGLCD.Print('Text has been', CENTER, 16);
      myGLCD.Print('changed while', CENTER, 24);
      myGLCD.Print('in Sleep Mode.', CENTER, 32);
      Sleep(5000);

      myGLCD.DisableSleep();
      Sleep(5000);
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

