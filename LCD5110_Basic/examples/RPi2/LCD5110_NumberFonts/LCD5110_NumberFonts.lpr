program LCD5110_NumberFonts;

// LCD5110_NumberFonts

// This program is a demo of the included number-fonts,
// and how to use them.

// This program requires a Nokia 5110 LCD module

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
      myGLCD.SetFont(SmallFont);
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
    i: integer;
  begin
    if not Assigned(myGLCD) then
      Exit;

    try
      for i := 0 to 9999 do
      begin
        myGLCD.SetFont(MediumNumbers);
        myGLCD.PrintNumF(double(i / 3), 2, RIGHT, 0, DefaultFormatSettings.DecimalSeparator, 10, ' ');
        myGLCD.SetFont(BigNumbers);
        myGLCD.PrintNumI(i, RIGHT, 24, 10, ' ');
      end;

      myGLCD.SetFont(SmallFont);
      myGLCD.Print('|            |', CENTER, 16);
      Sleep(500);

      for i := 0 to 11 do
      begin
        myGLCD.Print('\', 6 + (i * 6), 16);
        Sleep(500);
      end;

      myGLCD.ClrScr();
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

