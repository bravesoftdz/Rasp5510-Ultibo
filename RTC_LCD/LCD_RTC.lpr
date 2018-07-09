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
  DateUtils,
  DefaultFonts,
  ThreadSerial,
  LCD5110_Basic;

const
  DS3231_I2C_ADDRESS = $68;
  DS3231_CONTROL_REG = $0E;
  DS3231_TMP_UP_REG = $11;
  DS3231_TMP_LOW_REG = $12;
  DS3231_SEC_REG = $00;
  DS3231_MIN_REG = $01;
  DS3231_HOUR_REG = $02;
  DS3231_WDAY_REG = $03;
  DS3231_MDAY_REG = $04;
  DS3231_MONTH_REG = $05;
  DS3231_YEAR_REG = $06;

  dow: array [1..7] of String = ('Dom', 'Seg', 'Ter', 'Qua', 'Qui', 'Sex', 'Sab');

var
  myGLCD: TLCD5110;
  WindowHandle: TWindowHandle;
  FThreadSerial: TThreadSerial;

  second, minute, hour, dayOfWeek, dayOfMonth, month, year: Byte;
  s: String;
  fg, bg: LongWord;

function DecToBcd(val: Byte): Byte;
begin
  Result := (val div 10 * 16) + (val mod 10);
end;

// Convert binary coded decimal to normal decimal numbers
function BcdToDec(val: Byte): Byte;
begin
  Result := (val div 16 * 10) + (val mod 16);
end;

function WeekDay(year, month, day: Word): Word; forward;
procedure ReadDateTime(var second, minute, hour, dayOfWeek, dayOfMonth, month, year: Byte); forward;

procedure SetDayOfWeek();
var
  Data: array [0..1] of Byte;
  Count: LongWord;
  sec, min, hr, dw, dy, mh, yr, dayOfWeek: Byte;
begin
  sec := 0;
  min := 0;
  hr := 0;
  dw := 0;
  dy := 0;
  mh := 0;
  yr := 0;
  dayOfWeek := 0;
  Count := 0;

  ReadDateTime(sec, min, hr, dw, dy, mh, yr);

  dayOfWeek := WeekDay(yr, mh, dy);
  Data[0] := DS3231_WDAY_REG;
  Data[1] := DecToBcd(dayOfWeek);
  SysI2CWrite(DS3231_I2C_ADDRESS, @Data, 2, Count);
end;

//set the time-date specified in DateTime format
//writing any non-existent time-data may interfere with normal operation of the RTC
procedure SetDateTime(second, minute, hour, day, month, year: Integer);
var
  Data: array [0..1] of Byte;
  Count: LongWord;
begin
  Count := 0;

  if second > -1 then
  begin
    Data[0] := DS3231_SEC_REG;
    Data[1] := DecToBcd(second);
    SysI2CWrite(DS3231_I2C_ADDRESS, @Data, 2, Count);
  end;

  if minute > -1 then
  begin
    Data[0] := DS3231_MIN_REG;
    Data[1] := DecToBcd(minute);
    SysI2CWrite(DS3231_I2C_ADDRESS, @Data, 2, Count);
  end;

  if hour > -1 then
  begin
    Data[0] := DS3231_HOUR_REG;
    Data[1] := DecToBcd(hour and $BF); // 0b1011 1111)); //Make sure clock is still 24 Hour
    SysI2CWrite(DS3231_I2C_ADDRESS, @Data, 2, Count);
  end;

  if day > -1 then
  begin
    Data[0] := DS3231_MDAY_REG;
    Data[1] := DecToBcd(day);
    SysI2CWrite(DS3231_I2C_ADDRESS, @Data, 2, Count);
  end;

  if month > -1 then
  begin
    Data[0] := DS3231_MONTH_REG;
    Data[1] := DecToBcd(month);
    SysI2CWrite(DS3231_I2C_ADDRESS, @Data, 2, Count);
  end;

  if year > -1 then
  begin
    Data[0] := DS3231_YEAR_REG;
    Data[1] := DecToBcd(year);
    SysI2CWrite(DS3231_I2C_ADDRESS, @Data, 2, Count);
  end;

  SetDayOfWeek();
end;

procedure ProcAdjust(second, minute, hour, day, month, year: Integer);
begin
  SetDateTime(second, minute, hour, day, month, year);
end;

procedure ReadDateTime(var second, minute, hour, dayOfWeek, dayOfMonth, month, year: Byte);
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
  Count := 0;

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
  Count := 0;
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
  ConvertTemperature();

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

function IsLeapYear(year: Word): Boolean;
begin
  if (year mod 400 = 0) then
    Result := True
  else if ((year mod 4 = 0) and (year mod 100 = 0)) then
    Result := False
  else if ((year mod 4 = 0) and (year mod 100 <> 0)) then
    Result := True
  else
    Result := False;
end;

function WeekDay(year, month, day: Word): Word;
var
  A: Integer;
const
  RefMon: array[1..12] of Byte = (0, 3, 3, 6, 1, 4, 6, 2, 5, 0, 3, 5);
begin
  year := year mod 100;

  A := (RefMon[month] + day) mod 7;

  Result := (year mod 28) + (year div 4);

  if IsLeapYear(year) and (month < 3) then
    Dec(Result);

  Inc(Result, A);
  Result := Result mod 7;

  if Result = 0 then
    Result := 7;
end;

function FuncDTTemp(tp: Char): String;
var
  second, minute, hour, dayOfWeek, dayOfMonth, month, year: Byte;
begin
  if Pos(tp, 'DT@%') = 0 then
  begin
    Result := '';
    Exit;
  end;

  if tp <> '%' then
  begin
    second := 0;
    minute := 0;
    hour := 0;
    dayOfWeek := 0;
    dayOfMonth := 0;
    month := 0;
    year := 0;

    ReadDateTime(second, minute, hour, dayOfWeek, dayOfMonth, month, year);

    case tp of
      'D': Result := Format('%0.2d/%0.2d/%0.2d', [year, month, dayOfMonth]);
      'T': Result := Format('%0.2d:%0.2d:%0.2d', [hour, minute, second]);
      '@': Result := Format('%0.2d/%0.2d/%0.2d-%0.2d:%0.2d:%0.2d', [year, month, dayOfMonth, hour, minute, second]);
    end;
  end
  else
    Result := Format('%0.2F', [GetTemperature()]);
end;

begin
  I2CInit;

  {Let's create a console window again but this time on the left side of the screen}
  WindowHandle := ConsoleWindowCreate(ConsoleDeviceGetDefault, CONSOLE_POSITION_FULL, True);

  fg := ConsoleWindowGetForecolor(WindowHandle);
  bg := ConsoleWindowGetBackcolor(WindowHandle);

  second := 0;
  minute := 0;
  hour := 0;
  dayOfWeek := 0;
  dayOfMonth := 0;
  month := 0;
  year := 0;

  {To prove that worked let's output some text on the console window}
  ConsoleWindowWriteLn(WindowHandle, 'Real Time Clock');

  if SysI2CStart(100000) <> ERROR_SUCCESS then
    ConsoleWindowWriteEx(WindowHandle, 'Can'' Start I2C...', 1, 3, COLOR_RED, bg)
  else if SerialOpen(9600, SERIAL_DATA_8BIT, SERIAL_STOP_1BIT, SERIAL_PARITY_NONE,
      SERIAL_FLOW_NONE, 0, 0) <> ERROR_SUCCESS then
    ConsoleWindowWriteEx(WindowHandle, 'Can'' Start Serial...', 1, 3, COLOR_RED, bg)
  else
    try
      myGLCD := TLCD5110.Create(stBCM2835, $3F);
      myGLCD.SetFont(SmallFont);
      myGLCD.ClrScr();
      myGLCD.Print('Clock', CENTER, 0);

      FThreadSerial := TThreadSerial.Create(WindowHandle, @ProcAdjust, @FuncDTTemp, fg, bg);

      while True do
      begin
        ReadDateTime(second, minute, hour, dayOfWeek, dayOfMonth, month, year);
        s := Format('%0.2d/%0.2d/%0.2d - %s', [dayOfMonth, month, year, dow[dayOfWeek]]);
        myGLCD.Print(s, CENTER, 11);
        ConsoleWindowWriteEx(WindowHandle, s, 1, 3, fg, bg);

        s := Format('%0.2d:%0.2d:%0.2d', [hour, minute, second]);
        myGLCD.Print(s, CENTER, 22);
        ConsoleWindowWriteEx(WindowHandle, s, 16, 3, fg, bg);

        s := '     ' + Format('%0.2f', [GetTemperature()]);
        myGLCD.Print(s, RIGHT, 33);
        ConsoleWindowWriteEx(WindowHandle, Trim(s) + '     ', 1, 5, fg, bg);
      end;
    except
      on E: Exception do
      begin
        myGLCD := nil;
        ConsoleWindowWriteLn(WindowHandle, 'Error: ' + E.Message);
      end;
    end;

  SerialClose;
  SysI2CStop;

  ConsoleWindowWriteLn(WindowHandle, '');
  ConsoleWindowWriteLn(WindowHandle, 'Bye');

  ThreadHalt(0);
end.

