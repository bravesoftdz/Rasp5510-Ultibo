unit LCD5110_Basic;

{
  LCD5110_Graph.pas - Ultibo Raspberry Pi library support for Nokia 5110 compatible LCDs
  2018 B. Angelo Molizane, molizane@gmail.com

  *****************************************************************************************
  Based on Arduino/chipKit library support for Nokia 5110 compatible LCDs
  Copyright (C)2015 Rinky-Dink Electronics, Henning Karlsen. All right reserved

  Basic functionality of this library are based on the demo-code provided by
  ITead studio. You can find the latest version of the library at
  http://www.RinkyDinkElectronics.com/

  This library has been made to make it easy to use the basic functions of
  the Nokia 5110 LCD module on an Arduino or a chipKit.

  This library is free software; you can redistribute it and/or
  modify it under the terms of the CC BY-NC-SA 3.0 license.
  Please see the included documents for further information.

  Commercial use of this library requires you to buy a license that
  will allow commercial use. This includes using the library,
  modified or not, as a tool to sell products.

  The license applies to all part of the library including the
  examples and tools supplied with the library.
  *****************************************************************************************
}

{
     1-RST       -> GPIO17
     2-SCE       -> GPIO8 = SPI CE0
     3-D/C       -> GPIO27
     4-DN(MOSI)  -> GPIO10 = MOSI
     5-SCLK      -> GPIO11 = SCLK
     6-VCC       -> 3.3V
     7-LED       -> 3.3V
     8-GND       -> GND
}

{
1 RST Reset Input Active low
2 SCE Chip select Input Active low
3 D/C Mode select Input Select between command mode (low) and data mode (high).
4 DN(MOSI) Serial data in Input
5 SCLK Serial clock Input
7 LED LED backlight supply Input Maximum voltage supply is 3.3V.
}

interface

uses
  Platform,
  GlobalConst,
  GPIO,
  Devices,
  SPI;

const
  LEFT = 0;
  RIGHT = 9999;
  CENTER = 9998;
  LCD_COMMAND = 0;
  LCD_DATA = 1;

  // PCD8544 Commandset
  // ------------------
  // General commands
  LCD5110_POWERDOWN = $04;
  LCD5110_ENTRYMODE = $02;
  LCD5110_EXTENDEDINSTRUCTION = $01;
  LCD5110_DISPLAYBLANK = $00;
  LCD5110_DISPLAYNORMAL = $04;
  LCD5110_DISPLAYALLON = $01;
  LCD5110_DISPLAYINVERTED = $05;

  // Normal instruction set
  LCD5110_FUNCTIONSET = $20;
  LCD5110_DISPLAYCONTROL = $08;
  LCD5110_SETYADDR = $40;
  LCD5110_SETXADDR = $80;

  // Extended instruction set
  LCD5110_SETTEMP = $04;
  LCD5110_SETBIAS = $10;
  LCD5110_SETVOP = $80;

  // Display presets
  LCD_BIAS = $03;   // Range: 0-7 (= $00-= $07)
  LCD_TEMP = $02;   // Range: 0-3 (= $00-= $03)
  LCD_CONTRAST = $46;   // Range: 0-127 (= $00-= $7F)

type
  PByte = ^byte;

  TSOCType = (stBCM2835, stBCM2836, stBCM2837);

  TCurrentFont = record
    font: PByte;
    x_size: byte;
    y_size: byte;
    offset: byte;
    numchars: byte;
    inverted: boolean;
  end;

  { TLCD5110 }

  TLCD5110 = class
  private
    SPIDevice: PSPIDevice;
    cfont: TCurrentFont;
    FSleep: boolean;
    FContrast: byte;
    P_SCK: byte;
    P_RST: byte;
    P_MOSI: byte;
    P_DC: byte;
    P_CS: byte;
  protected
    procedure ConvertToFloat(var buf: string; num: double; Width, prec: byte);
    procedure PulseClock();
    procedure Reset();
    procedure Init(contrast: byte);
  public
    constructor Create(SOCType: TSOCType; contrast: byte);
    procedure SetContrast(contrast: byte);
    procedure EnableSleep();
    procedure DisableSleep();
    procedure Invert(mode: boolean);
    procedure InvertText(mode: boolean); overload;
    function InvertText(): boolean; overload;
    procedure SetFont(font: PByte);
    procedure WriteByte(Data: byte);
    procedure PrintChar(c: char; x, row: byte);
    procedure Write(Data, mode: byte);
    procedure Print(st: string; x, y: integer);
    procedure PrintNumI(num: longint; x, y: integer; len: byte; filler: char);
    procedure PrintNumF(num: double; Dec: byte; x, y: integer; divider: char; len: byte; filler: char);
    procedure DrawBitmap(x, y: byte; bitmap: array of byte; sx, sy: byte);
    procedure ClrScr();
    procedure ClrRow(row, start_x, end_x: byte);

  end;

implementation

uses
  SysUtils;

constructor TLCD5110.Create(SOCType: TSOCType; contrast: byte);
begin
  {
  4-RST       -> GPIO17
  5-D/C       -> GPIO27
  }

  GPIODeviceFunctionSelect(GPIODeviceGetDefault, GPIO_PIN_17, GPIO_FUNCTION_OUT);
  GPIODeviceFunctionSelect(GPIODeviceGetDefault, GPIO_PIN_27, GPIO_FUNCTION_OUT);

  if SOCType = stBCM2837 then
    SPIDevice := PSPIDevice(DeviceFindByDescription('BCM2837 SPI0 Master'))
  else if SOCType = stBCM2836 then
    SPIDevice := PSPIDevice(DeviceFindByDescription('BCM2836 SPI0 Master'))
  else
    SPIDevice := PSPIDevice(DeviceFindByDescription('BCM2835 SPI0 Master'));

  if SPIDeviceStart(SPIDevice, SPI_MODE_4WIRE, 100000, SPI_CLOCK_PHASE_LOW, SPI_CLOCK_POLARITY_LOW) = ERROR_SUCCESS then
  begin
  end; { if SPIDeviceStart }

  {
  3 SCE Chip select Input Active low
  4 RST Reset Input Active low
  5 D/C Mode select Input Select between command mode (low) and data mode (high).
  6 DN(MOSI) Serial data in Input
  7 SCLK Serial clock Input
  8 LED LED backlight supply Input Maximum voltage supply is 3.3V.
  }

  // Reset Input Active low
  GPIODeviceOutputSet(GPIODeviceGetDefault, GPIO_PIN_17, GPIO_LEVEL_HIGH);

  // D/C Mode select Input Select between command mode (low) and data mode (high)
  GPIODeviceOutputSet(GPIODeviceGetDefault, GPIO_PIN_27, GPIO_LEVEL_LOW);

  // *** RESET ***
  // Reset Input Active low
  GPIODeviceOutputSet(GPIODeviceGetDefault, GPIO_PIN_17, GPIO_LEVEL_LOW);
  MicrosecondDelay(20);
  GPIODeviceOutputSet(GPIODeviceGetDefault, GPIO_PIN_17, GPIO_LEVEL_HIGH);

  //Tell LCD extended commands follow
  Write($21, LCD_COMMAND);

  //Set LCD Vop (Contrast)
  //Write($B1, LCD_COMMAND);
  //Write($80 or $70, LCD_COMMAND);
  Write($BF, LCD_COMMAND);

  //Set Temp coefficent
  Write($04, LCD_COMMAND);

  //LCD bias mode 1:48 (try 0x13)
  Write($14, LCD_COMMAND);

  //We must send 0x20 before modifying the display control mode
  Write($20, LCD_COMMAND);

  //Set display control, normal mode.
  Write($0C, LCD_COMMAND);

  Init(contrast);
end;

procedure TLCD5110.Write(Data, mode: byte);
begin
  if mode = LCD_COMMAND then
    GPIODeviceOutputSet(GPIODeviceGetDefault, GPIO_PIN_27, GPIO_LEVEL_LOW)
  else
    GPIODeviceOutputSet(GPIODeviceGetDefault, GPIO_PIN_27, GPIO_LEVEL_HIGH);

  WriteByte(Data);
end;

procedure TLCD5110.Init(contrast: byte);
var
  c: integer;
begin
  if contrast > $7F then
    contrast := $7F;

  Reset;

  Write(LCD5110_FUNCTIONSET or LCD5110_EXTENDEDINSTRUCTION, LCD_COMMAND);
  Write(LCD5110_SETVOP or contrast, LCD_COMMAND);
  Write(LCD5110_SETTEMP or LCD_TEMP, LCD_COMMAND);
  Write(LCD5110_SETBIAS or LCD_BIAS, LCD_COMMAND);
  Write(LCD5110_FUNCTIONSET, LCD_COMMAND);
  Write(LCD5110_SETYADDR, LCD_COMMAND);
  Write(LCD5110_SETXADDR, LCD_COMMAND);

  for c := 0 to 503 do
    Write($00, LCD_DATA);

  Write(LCD5110_DISPLAYCONTROL or LCD5110_DISPLAYNORMAL, LCD_COMMAND);

  cfont.font := nil;
  FSleep := False;
  FContrast := contrast;
  ClrScr();
end;

procedure TLCD5110.SetContrast(contrast: byte);
begin
  if contrast > $7F then
    contrast := $7F;

  Write(LCD5110_FUNCTIONSET or LCD5110_EXTENDEDINSTRUCTION, LCD_COMMAND);
  Write(LCD5110_SETVOP or contrast, LCD_COMMAND);
  Write(LCD5110_FUNCTIONSET, LCD_COMMAND);

  FContrast := contrast;
end;

procedure TLCD5110.EnableSleep();
var
  b: integer;
begin
  FSleep := True;

  Write(LCD5110_SETYADDR, LCD_COMMAND);
  Write(LCD5110_SETXADDR, LCD_COMMAND);

  for b := 0 to 503 do
    Write(0, LCD_DATA);

  Write(LCD5110_FUNCTIONSET or LCD5110_POWERDOWN, LCD_COMMAND);
end;

procedure TLCD5110.DisableSleep();
begin
  Write(LCD5110_FUNCTIONSET or LCD5110_EXTENDEDINSTRUCTION, LCD_COMMAND);
  Write(LCD5110_SETVOP or FContrast, LCD_COMMAND);
  Write(LCD5110_SETTEMP or LCD_TEMP, LCD_COMMAND);
  Write(LCD5110_SETBIAS or LCD_BIAS, LCD_COMMAND);
  Write(LCD5110_FUNCTIONSET, LCD_COMMAND);
  Write(LCD5110_DISPLAYCONTROL or LCD5110_DISPLAYNORMAL, LCD_COMMAND);
  FSleep := False;
end;

procedure TLCD5110.ClrScr();
var
  c: integer;
begin
  if FSleep then
    Exit;

  Write(LCD5110_SETYADDR, LCD_COMMAND);
  Write(LCD5110_SETXADDR, LCD_COMMAND);

  for c := 0 to 503 do
    Write($00, LCD_DATA);
end;

procedure TLCD5110.ClrRow(row, start_x, end_x: byte);
var
  c: byte;
begin
  if FSleep then
    Exit;

  Write(LCD5110_SETYADDR or row, LCD_COMMAND);
  Write(LCD5110_SETXADDR or start_x, LCD_COMMAND);

  for c := start_x to end_x do
    Write($00, LCD_DATA);

  Write(LCD5110_SETYADDR, LCD_COMMAND);
  Write(LCD5110_SETXADDR, LCD_COMMAND);
end;

procedure TLCD5110.Invert(mode: boolean);
begin
  if FSleep then
    Exit;

  if mode then
    Write(LCD5110_DISPLAYCONTROL or LCD5110_DISPLAYINVERTED, LCD_COMMAND)
  else
    Write(LCD5110_DISPLAYCONTROL or LCD5110_DISPLAYNORMAL, LCD_COMMAND);
end;

procedure TLCD5110.InvertText(mode: boolean);
begin
  cfont.inverted := mode;
end;

function TLCD5110.InvertText: boolean;
begin
  Result := cfont.inverted;
end;

procedure TLCD5110.Print(st: string; x, y: integer);
var
  stl, row, cnt: integer;
begin
  if FSleep then
    Exit;

  stl := Length(st);

  if x = RIGHT then
    x := 84 - (stl * cfont.x_size)
  else if x = CENTER then
    x := (84 - (stl * cfont.x_size)) div 2
  else if x < 0 then
    x := 0;

  row := y div 8;

  for cnt := 1 to stl do
    PrintChar(st[cnt], x + ((cnt - 1) * (cfont.x_size)), row);
end;

procedure TLCD5110.PrintNumI(num: longint; x, y: integer; len: byte; filler: char);
var
  st: string;
begin
  if FSleep then
    Exit;

  st := IntToStr(num);

  while Length(st) < len do
    st := filler + st;

  Print(st, x, y);
end;

procedure TLCD5110.PrintNumF(num: double; Dec: byte; x, y: integer; divider: char; len: byte; filler: char);
var
  st: string;
  dsp: byte;
begin
  if FSleep then
    Exit;

  st := '';

  ConvertToFloat(st, num, len, Dec);

  if divider <> DefaultFormatSettings.DecimalSeparator then
  begin
    dsp := Pos(DefaultFormatSettings.DecimalSeparator, st);

    if dsp <> 0 then
      st[dsp] := divider;
  end;

  while Length(st) < len do
    st := filler + st;

  Print(st, x, y);
end;

procedure TLCD5110.PrintChar(c: char; x, row: byte);
var
  rowcnt, font_idx1, font_idx2, cnt: integer;
begin
  if FSleep or (Ord(c) < cfont.offset) then
    Exit;

  if ((x + cfont.x_size) <= 84) and (row + (cfont.y_size / 8) <= 6) then
  begin
    font_idx1 := ((Ord(c) - cfont.offset) * (cfont.x_size * (cfont.y_size div 8))) + 4;
    font_idx2 := font_idx1;

    for rowcnt := 0 to (cfont.y_size div 8) - 1 do
    begin
      Write(LCD5110_SETYADDR or (row + rowcnt), LCD_COMMAND);
      Write(LCD5110_SETXADDR or x, LCD_COMMAND);

      //font_idx2 := font_idx1+rowcnt*cfont.x_size;

      for cnt := 0 to cfont.x_size - 1 do
      begin
        if not cfont.inverted then
          Write(cfont.font[font_idx2], LCD_DATA)
        else
          Write(cfont.font[font_idx2] xor -1, LCD_DATA);

        Inc(font_idx2);
      end;
    end;

    Write(LCD5110_SETYADDR, LCD_COMMAND);
    Write(LCD5110_SETXADDR, LCD_COMMAND);
  end;
end;

procedure TLCD5110.ConvertToFloat(var buf: string; num: double; Width, prec: byte);
var
  str, mask: string;
begin
  str := '';
  mask := '';

  while Length(mask) < prec do
    mask := mask + '0';

  while Length(str) < Width - Length(mask) do
    str := str + '#';

  mask := str + '0.' + mask;
  buf := FormatFloat(mask, num);
end;

procedure TLCD5110.PulseClock();
begin
  GPIOOutputSet(P_SCK, GPIO_LEVEL_LOW);
  Sleep(1);
  GPIOOutputSet(P_SCK, GPIO_LEVEL_HIGH);
end;

procedure TLCD5110.Reset;
begin
  GPIOOutputSet(P_DC, GPIO_LEVEL_HIGH);
  GPIOOutputSet(P_MOSI, GPIO_LEVEL_HIGH);
  GPIOOutputSet(P_SCK, GPIO_LEVEL_HIGH);
  GPIOOutputSet(P_CS, GPIO_LEVEL_HIGH);
  GPIOOutputSet(P_RST, GPIO_LEVEL_LOW);
  GPIOOutputSet(P_RST, GPIO_LEVEL_HIGH);
end;

procedure TLCD5110.WriteByte(Data: byte);
var
  Source_Value, Dest_Value: byte;
  Count: longword;
  SourceBuffer: Pointer;
  DestBuffer: Pointer;
begin
  SourceBuffer := @Source_Value;
  DestBuffer := @Dest_Value;
  Source_Value := Data;
  Count := 0;

  if SPIDeviceWriteRead(SPIDevice, SPI_CS_0, SourceBuffer, DestBuffer, 1, SPI_TRANSFER_NONE, Count) = ERROR_SUCCESS then
  begin
    // do nothing
  end;
end;

procedure TLCD5110.SetFont(font: PByte);
begin
  cfont.font := font;
  cfont.x_size := cfont.font[0];
  cfont.y_size := cfont.font[1];
  cfont.offset := cfont.font[2];
  cfont.numchars := cfont.font[3];
  cfont.inverted := False;
end;

procedure TLCD5110.DrawBitmap(x, y: byte; bitmap: array of byte; sx, sy: byte);
var
  starty, rows, cy, cx: byte;
begin
  if FSleep then
    Exit;

  starty := y div 8;

  if sy mod 8 = 0 then
    rows := sy div 8
  else
    rows := (sy div 8) + 1;

  for cy := 0 to rows - 1 do
  begin
    Write(LCD5110_SETYADDR or (starty + cy), LCD_COMMAND);
    Write(LCD5110_SETXADDR or x, LCD_COMMAND);

    for cx := 0 to sx - 1 do
      Write(bitmap[cx + (cy * sx)], LCD_DATA);
  end;

  Write(LCD5110_SETYADDR, LCD_COMMAND);
  Write(LCD5110_SETXADDR, LCD_COMMAND);
end;

end.

