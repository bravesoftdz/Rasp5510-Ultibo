program LCD5110_Scrolling_Text;

// LCD5110_Scrolling_Text

// This program is a demo of how to use print().

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
    y, i: integer;
  begin
    try
      y := Random(40);

      myGLCD.ClrScr();

      for i := 84 downto -(34 * 6) do
      begin
        myGLCD.Print('LCD5110_Graph Scrolling Text Demo ', i, y);
        myGLCD.Update();
        Sleep(50);
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

