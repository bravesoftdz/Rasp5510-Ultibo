program LCD5110_Graph_Demo;

//  LCD5110_Graph_Demo

//  This program is a demo of most of the functions
//  in the library.

//  This program requires a Nokia 5110 LCD module.

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
  LCD5110_Graph,
  DefaultFonts,
  LCD_Graphics;

var
  myGLCD: TLCD5110;
  y: double;
  bm: PByte;
  pacy: integer;
  Handle: TWindowHandle;

  procedure Setup();
  begin
    {Let's create a console window again but this time on the left side of the screen}
    Handle := ConsoleWindowCreate(ConsoleDeviceGetDefault, CONSOLE_POSITION_FULL, True);

    {To prove that worked let's output some text on the console window}
    ConsoleWindowWriteLn(Handle, 'Welcome to Example NOKIA 5110 display test');

    try
      myGLCD := TLCD5110.Create(stBCM2836, $3F);
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
    i, c, pc, p: integer;
  begin
    try
      myGLCD.ClrScr();
      //myGLCD.DrawBitmap(0, 0, arduino_logo, 84, 48);
      myGLCD.DrawBitmap(18, 0, logopi, 48, 48);
      myGLCD.Update();

      Sleep(2000);

      myGLCD.ClrScr();
      myGLCD.Print('LCD5110_Graph', CENTER, 0);
      myGLCD.Print('DEMO', CENTER, 20);
      myGLCD.DrawRect(28, 18, 56, 28);

      for i := 0 to 5 do
      begin
        myGLCD.DrawLine(57, 18 + (i * 2), 83 - (i * 3), 18 + (i * 2));
        myGLCD.DrawLine((i * 3), 28 - (i * 2), 28, 28 - (i * 2));
      end;

      myGLCD.SetFont(TinyFont);
      myGLCD.Print('(C)2015 by', CENTER, 36);
      myGLCD.Print('Henning Karlsen', CENTER, 42);
      myGLCD.Update();

      Sleep(5000);

      myGLCD.ClrScr();

      for i := 0 to 47 do
      begin
        myGLCD.DrawLine(0, i, 83, 47 - i);
        myGLCD.Update();
      end;

      i := 83;

      while i >= 0 do
      begin
        myGLCD.DrawLine(i, 0, 83 - i, 47);
        myGLCD.Update();
        Dec(i, 2);
      end;

      Sleep(2000);

      myGLCD.ClrScr();
      myGLCD.DrawRect(0, 0, 83, 47);

      i := 0;

      while i < 48 do
      begin
        myGLCD.DrawLine(0, i, Trunc(i * 1.75), 47);
        myGLCD.Update();
        Inc(i, 4);
      end;

      i := 0;

      while i < 48 do
      begin
        myGLCD.DrawLine(83, 47 - i, 83 - Trunc(i * 1.75), 0);
        myGLCD.Update();
        Inc(i, 4);
      end;

      Sleep(2000);

      myGLCD.ClrScr();

      for i := 0 to 7 do
      begin
        myGLCD.DrawRoundRect(i * 3, i * 3, 83 - (i * 3), 47 - (i * 3));
        myGLCD.Update();
      end;

      Sleep(2000);

      myGLCD.ClrScr();

      for i := 0 to 16 do
      begin
        myGLCD.DrawCircle(41, 23, i * 3);
        myGLCD.Update();
      end;

      Sleep(2000);

      myGLCD.ClrScr();
      myGLCD.DrawRect(0, 0, 83, 47);
      myGLCD.DrawLine(0, 23, 84, 23);
      myGLCD.DrawLine(41, 0, 41, 47);

      for c := 0 to 3 do
      begin
        for i := 0 to 83 do
        begin
          y := Trunc(i * 0.017453292519943295769236907684886);
          myGLCD.InvPixel(i, Trunc(sin(y * 6) * 20) + 23);
          myGLCD.Update();
          Sleep(20);
        end;
      end;

      Sleep(2000);

      for pc := 0 to 2 do
      begin
        pacy := Random(28);

        for i := -20 to 83 do
        begin
          myGLCD.ClrScr();

          for p := 4 to ((i + 20) div 20) - 1 do
            myGLCD.DrawBitmap(p * 20 - 8, pacy + 7, pill, 5, 5);

          case ((i + 20) div 3) mod 4 of
            0: bm := pacman1;
            1: bm := pacman2;
            2: bm := pacman3;
            3: bm := pacman2;
          end;

          myGLCD.DrawBitmap(i, pacy, bm, 20, 20);
          myGLCD.Update();
          Sleep(25);
        end;
      end;

      for i := 0 to 24 do
      begin
        myGLCD.ClrScr();
        myGLCD.DrawBitmap(0, i - 24, The_End, 84, 24);
        myGLCD.Update();
        Sleep(100);
      end;

      myGLCD.SetFont(SmallFont);
      myGLCD.Print('Runtime (ms): ', CENTER, 32);
      myGLCD.PrintNumI(GetTickCount(), CENTER, 40, 10, ' ');
      myGLCD.Update();

      for i := 0 to 4 do
      begin
        myGLCD.Invert(True);
        Sleep(1000);
        myGLCD.Invert(False);
        Sleep(1000);
      end;
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

