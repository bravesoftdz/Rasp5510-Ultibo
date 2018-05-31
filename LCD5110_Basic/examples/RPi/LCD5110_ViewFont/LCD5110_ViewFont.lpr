program LCD5110_ViewFont;

{$mode objfpc}{$H+}

// LCD5110_ViewFont

// This program is a demo of the included full font.

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
    c: char;
  begin
    if not Assigned(myGLCD) then
      Exit;

    try
      myGLCD.SetFont(SmallFont);

      myGLCD.ClrScr();
      myGLCD.Print('Upper case:', LEFT, 0);
      myGLCD.Print('ABCDEFGHIJKLM', CENTER, 16);
      myGLCD.Print('NOPQRSTUVWXYZ', CENTER, 24);
      Sleep(2000);

      myGLCD.ClrScr();
      myGLCD.Print('Lower case:', LEFT, 0);
      myGLCD.Print('abcdefghijklm', CENTER, 16);
      myGLCD.Print('nopqrstuvwxyz', CENTER, 24);
      Sleep(2000);

      myGLCD.ClrScr();
      myGLCD.Print('Numbers:', LEFT, 0);
      myGLCD.Print('0123456789', CENTER, 16);
      Sleep(2000);

      myGLCD.ClrScr();
      myGLCD.Print('Special:', LEFT, 0);
      myGLCD.Print('!"#$%&''()*+,-.', CENTER, 16);
      myGLCD.Print('/:;<=>?@[\]^_`', CENTER, 24);
      myGLCD.Print('{|}~', CENTER, 32);
      Sleep(2000);

      for i := 0 to 9 do
      begin
        myGLCD.InvertText(not myGLCD.InvertText);
        myGLCD.Print('Special:', LEFT, 0);
        myGLCD.Print('!"#$%&''()*+,-.', CENTER, 16);
        myGLCD.Print('/:;<=>?@[\]^_`', CENTER, 24);
        myGLCD.Print('{|}~', CENTER, 32);
        Sleep(50);
      end;

      myGLCD.ClrScr();
      myGLCD.Print('Medium Numbers:', LEFT, 0);
      myGLCD.SetFont(MediumNumbers);

      for c := '-' to '9' do
      begin
        myGLCD.Print(c, CENTER, 16);
        Sleep(1000);
      end;

      Sleep(1000);

      myGLCD.SetFont(SmallFont);
      myGLCD.ClrScr();
      myGLCD.Print('Big Numbers:', LEFT, 0);
      myGLCD.SetFont(BigNumbers);

      for c := '-' to '9' do
      begin
        myGLCD.Print(c, CENTER, 16);
        Sleep(1000);
      end;

      myGLCD.Free;
      myGLCD := nil;
    except
      on E: Exception do
      begin
        myGLCD.Free;
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

