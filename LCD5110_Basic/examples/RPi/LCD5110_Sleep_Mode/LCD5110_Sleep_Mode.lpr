program LCD5110_Sleep_Mode;

// LCD5110_Sleep_Mode

// This program is a demo of sleep mode.

// This program requires a Nokia 5110 LCD module.

uses
  RaspberryPi2,
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  SysUtils,
  Classes,
  Ultibo,
  Console,
  LCD5110_Basic,
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
      myGLCD := TLCD5110.Create($3F);
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
    if not Assigned(myGLCD) then
      Exit;

    try
      myGLCD.SetFont(SmallFont);
      myGLCD.ClrScr();
      myGLCD.Print('Entering', CENTER, 0);
      myGLCD.Print('Sleep Mode', CENTER, 8);
      myGLCD.Print('in', CENTER, 16);
      myGLCD.Print('Seconds', CENTER, 40);

      myGLCD.SetFont(MediumNumbers);

      for s := 10 downto 0 do
      begin
        myGLCD.PrintNumI(s, CENTER, 24, 2, '0');
        Sleep(1000);
      end;

      myGLCD.enableSleep();
      Sleep(5000);
      myGLCD.DisableSleep();

      myGLCD.SetFont(SmallFont);
      myGLCD.Print('Awake again!', CENTER, 0);
      myGLCD.Print('The screen was', CENTER, 16);
      myGLCD.Print('cleared while', CENTER, 24);
      myGLCD.Print('in Sleep Mode.', CENTER, 32);
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

  ConsoleWindowWriteLn(Handle, 'Bye');

  {Halt the main thread if we ever get to here}
  ThreadHalt(0);
end.

