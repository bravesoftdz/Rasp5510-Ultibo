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
    s: integer;
  begin
    try
      myGLCD.setFont(SmallFont);
      myGLCD.clrScr();
      myGLCD.print('Entering', CENTER, 0);
      myGLCD.invertText(True);
      myGLCD.print(' Sleep Mode ', CENTER, 8);
      myGLCD.invertText(False);
      myGLCD.print('in', CENTER, 16);
      myGLCD.print('seconds', CENTER, 40);
      myGLCD.update();

      myGLCD.setFont(MediumNumbers);

      for s := 10 downto 0 do
      begin
        myGLCD.printNumI(s, CENTER, 24, 2, '0');
        myGLCD.update();
        Sleep(1000);
      end;

      myGLCD.enableSleep();
      myGLCD.clrScr();
      myGLCD.setFont(SmallFont);
      myGLCD.print('Awake again!', CENTER, 0);
      myGLCD.print('Text has been', CENTER, 16);
      myGLCD.print('changed while', CENTER, 24);
      myGLCD.print('in Sleep Mode.', CENTER, 32);
      Sleep(5000);
      myGLCD.disableSleep();
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

