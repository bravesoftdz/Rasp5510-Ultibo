program LCD5110_Font_Demo;

//  LCD5110_Font_Demo

//  This program is a demo of most of the functions
//  in the library.

//  This program requires a Nokia 5110 LCD module.

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
    ConsoleWindowWriteLn(Handle, 'Welcome to Example NOKIA 5110 display font allign test');

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
  begin
    try
      try
        myGLCD.ClrScr();
        myGLCD.Print('LEFT', LEFT, 0);
        myGLCD.Print('CENTER', CENTER, 20);
        myGLCD.Print('RIGHT', RIGHT, 40);
        myGLCD.Update();
      finally
        myGLCD.Free;
        myGLCD := nil;
      end;
    except
      on E: Exception do
        ConsoleWindowWriteLn(Handle, 'Setup() error: ' + E.Message);
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

