program LCD5110_Bitmap;

// LCD5110_Bitmap

// This program is a demo of how to use bitmaps.
// You can also see how to use invert().

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
  DefaultFonts,
  LCD_Graphics;

var
  myGLCD: TLCD5110;
  Handle: TWindowHandle;

{$i arduino_logo.inc}
{$i oshw_logo.inc}

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
      myGLCD.DrawBitmap(0, 0, arduino_logo, 84, 48);
      Sleep(4000);

      for i := 0 to 1 do
      begin
        myGLCD.invert(True);
        Sleep(500);
        myGLCD.invert(False);
        Sleep(500);
      end;

      Sleep(3500);

      myGLCD.clrScr();
      myGLCD.DrawBitmap(14, 0, oshw_logo, 56, 48);
      Sleep(4000);

      for i := 0 to 1 do
      begin
        myGLCD.invert(True);
        Sleep(500);
        myGLCD.invert(False);
        Sleep(500);
      end;

      Sleep(3500);

      Sleep(2000);
      myGLCD.clrScr();

      for i := 1 to 20 do
      begin
        myGLCD.DrawBitmap(0, 0, pacman1, 20, 24);
        Sleep(150);
        myGLCD.DrawBitmap(0, 0, pacman2, 20, 24);
        Sleep(150);
        myGLCD.DrawBitmap(0, 0, pacman3, 20, 24);
        Sleep(150);
      end;

      Sleep(1000);

      //myGLCD.clrScr();
      //myGLCD.DrawBitmap(0, 0, pill, 9, 4);
      //Sleep(2000);

      myGLCD.clrScr();
      myGLCD.DrawBitmap(0, 0, The_End, 84, 24);
      Sleep(2000);

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

