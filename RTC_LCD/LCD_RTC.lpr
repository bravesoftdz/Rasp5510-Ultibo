program LCD_RTC;

{$mode objfpc}{$H+}

{ Raspberry Pi Application                                                     }
{  Add your program code below, add additional units to the "uses" section if  }
{  required and create new units by selecting File, New Unit from the menu.    }
{                                                                              }
{  To compile your program select Run, Compile (or Run, Build) from the menu.  }

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
  I2C,
  Console,
  DefaultFonts,
  LCD5110_Basic;

const
  DS3231_I2C_ADDRESS = $68;
  DS3231_CONTROL_REG = $0E;
  DS3231_TMP_UP_REG = $11;
  DS3231_TMP_LOW_REG = $12;

  dow: array [0..7] of String = ('', 'Dom', 'Seg', 'Ter', 'Qua', 'Qui', 'Sex', 'Sab');

var
  myGLCD: TLCD5110;
  Handle: TWindowHandle;

function DecToBcd(val: Byte): Byte;
begin
  Result := (val div 10 * 16) + (val mod 10);
end;

// Convert binary coded decimal to normal decimal numbers
function BcdToDec(val: Byte): Byte;
begin
  Result := (val div 16 * 10) + (val mod 16);
end;

procedure ReadDS3231time(var second, minute, hour, dayOfWeek, dayOfMonth, month, year: Byte);
var
  Data: array [0..7] of Byte;
  Count: LongWord;
begin
  FillMemory(@Data, 8, 0);
  Count := 0;

  if SysI2CWrite(DS3231_I2C_ADDRESS, @Data, 1, Count) <> ERROR_SUCCESS then
    Exit;

  if SysI2CRead(DS3231_I2C_ADDRESS, @Data, 7, Count) <> ERROR_SUCCESS then
    Exit;

  second := BcdToDec(Data[0] and $7f);
  minute := BcdToDec(Data[1]);
  hour := BcdToDec(Data[2] and $3f);
  dayOfWeek := BcdToDec(Data[3]);
  dayOfMonth := BcdToDec(Data[4]);
  month := BcdToDec(Data[5]);
  year := BcdToDec(Data[6]);
end;

function ReadRegister(regaddress: Byte): Byte;
var
  Data: Byte;
  Count: LongWord;
begin
  Result := 0;
  Data := regaddress;

  if SysI2CWrite(DS3231_I2C_ADDRESS, @Data, 1, Count) <> ERROR_SUCCESS then
    Exit;

  if SysI2CRead(DS3231_I2C_ADDRESS, @Data, 1, Count) <> ERROR_SUCCESS then
    Exit;

  Result := Data;
end;

procedure WriteRegister(regaddress, value: Byte);
var
  Reg: Byte;
  Count: LongWord;
begin
  Reg := regaddress;
  SysI2CWrite(DS3231_I2C_ADDRESS, @Reg, 1, Count);
  Reg := value;
  SysI2CWrite(DS3231_I2C_ADDRESS, @Reg, 1, Count);
end;

//force temperature sampling and converting to registers. If this function is not used the temperature is sampled once 64 Sec.
procedure ConvertTemperature();
var
  ctReg: Byte;
begin
  // Set CONV
  ctReg := readRegister(DS3231_CONTROL_REG);
  ctReg := ctReg or $20; // 0b00100000;
  WriteRegister(DS3231_CONTROL_REG, ctReg);

  //wait until CONV is cleared. Indicates new temperature value is available in register.
  repeat
    //do nothing
  until (ReadRegister(DS3231_CONTROL_REG) and $20 {0b00100000}) <> $20; // 0b00100000;

end;

//Read the temperature value from the register and convert it into float (deg C)
function GetTemperature(): Real;
var
  fTemperatureCelsius: Real;
  tUBYTE, tLRBYTE: Byte;
begin
  tUBYTE := ReadRegister(DS3231_TMP_UP_REG);  //Two's complement form
  tLRBYTE := ReadRegister(DS3231_TMP_LOW_REG); //Fractional part

  if (tUBYTE and $80 <> 0) then // & 0b1000 0000) //check if -ve number
  begin
    tUBYTE := tUBYTE xor $FF; // 0b11111111;
    Inc(tUBYTE);
    fTemperatureCelsius := tUBYTE + ((tLRBYTE shr 6) * 0.25);
    fTemperatureCelsius := fTemperatureCelsius * -1;
  end
  else
    fTemperatureCelsius := tUBYTE + ((tLRBYTE shr 6) * 0.25);

  Result := fTemperatureCelsius;
end;

var
  second, minute, hour, dayOfWeek, dayOfMonth, month, year, temp: Byte;
  s: String;
  fg, bg: LongWord;

begin
  I2CInit;

  {Let's create a console window again but this time on the left side of the screen}
  Handle := ConsoleWindowCreate(ConsoleDeviceGetDefault, CONSOLE_POSITION_FULL, True);

  fg := ConsoleWindowGetForecolor(Handle);
  bg := ConsoleWindowGetBackcolor(Handle);

  {To prove that worked let's output some text on the console window}
  ConsoleWindowWriteLn(Handle, 'Real Time Clock');

  if SysI2CStart(100000) <> ERROR_SUCCESS then
    ConsoleWindowWriteEx(Handle, 'Can'' Start I2C...', 1, 3, COLOR_RED, bg)
  else
    try
      myGLCD := TLCD5110.Create(stBCM2835, $3F);
      myGLCD.SetFont(SmallFont);
      myGLCD.ClrScr();
      myGLCD.Print('Clock', CENTER, 0);

      while True do
      begin
        ReadDS3231time(second, minute, hour, dayOfWeek, dayOfMonth, month, year);
        s := Format('%0.2d/%0.2d/%0.2d - %s', [dayOfMonth, month, year, dow[dayOfWeek]]);
        myGLCD.Print(s, CENTER, 11);
        ConsoleWindowWriteEx(Handle, s, 1, 3, fg, bg);

        s := Format('%0.2d:%0.2d:%0.2d', [hour, minute, second]);
        myGLCD.Print(s, CENTER, 22);
        ConsoleWindowWriteEx(Handle, s, 16, 3, fg, bg);

        ConvertTemperature();
        s := '     ' + Format('%0.2f', [GetTemperature()]);
        myGLCD.Print(s, RIGHT, 33);
        ConsoleWindowWriteEx(Handle, Trim(s) + '     ', 1, 5, fg, bg);
      end;
    except
      on E: Exception do
      begin
        myGLCD := nil;
        ConsoleWindowWriteLn(Handle, 'Error: ' + E.Message);
      end;
    end;

  SysI2CStop;

  ConsoleWindowWriteLn(Handle, '');
  ConsoleWindowWriteLn(Handle, 'Bye');

  {Halt the main thread if we ever get to here}
  ThreadHalt(0);
end.

