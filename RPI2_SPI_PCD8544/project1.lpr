program project1;

{$mode objfpc}{$H+}

{ Raspberry Pi 2 Application                                                   }
{  Add your program code below, add additional units to the "uses" section if  }
{  required and create new units by selecting File, New Unit from the menu.    }

{  To compile your program select Run, Compile (or Run, Build) from the menu.  }

uses
  RaspberryPi2,
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  SysUtils,
  Classes,
  Console,
  PCD8544,
  DefaultFonts,
  LCD_Graphics,
  Ultibo;

var
  Handle: TWindowHandle;

begin
  {Let's create a console window again but this time on the left side of the screen}
  Handle := ConsoleWindowCreate(ConsoleDeviceGetDefault, CONSOLE_POSITION_FULL, True);

  {To prove that worked let's output some text on the console window}
  ConsoleWindowWriteLn(Handle, 'Welcome to Example SPI test');

  test;
end.

