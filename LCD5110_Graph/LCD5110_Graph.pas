unit LCD5110_Graph;

{
  LCD5110_Graph.pas - Ultibo Raspberry Pi library support for Nokia 5110 compatible LCDs
  2018 B. Angelo Molizane, molizane@gmail.com

  *****************************************************************************************
  Based on Arduino/chipKit library support for Nokia 5110 compatible LCDs
  Copyright (C)2015 Rinky-Dink Electronics, Henning Karlsen. All right reserved

  Basic functionality of this library are based on the demo-code provided by
  ITead studio. You can find the latest version of the library at
  http://www.RinkyDinkElectronics.com/

  This library has been made to make it easy to use the Nokia 5110 LCD module
  as a graphics display on an Raspberry Pi bare metal programmation.

  This library is free software; you can redistribute it and/or
  modify it under the terms of the CC BY-NC-SA 3.0 license.
  Please see the included documents for further information.

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

  // LCD5110 Commandset
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
  LCD_BIAS = $03; // Range: 0-7 (= $00-= $07)
  LCD_TEMP = $02; // Range: 0-3 (= $00-= $03)
  LCD_CONTRAST = $46; // Range: 0-127 (= $00-= $7F)

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
    scrbuf: array[0..503] of byte;
  protected
    procedure ConvertToFloat(var buf: string; num: double; Width, prec: byte);
    procedure Reset();
    procedure Init(contrast: byte);
  public
    constructor Create(SOCType: TSOCType; contrast: byte);
    procedure SetContrast(contrast: byte);
    procedure EnableSleep();
    procedure DisableSleep();
    procedure Invert(mode: boolean);
    procedure InvertText(mode: boolean);
    procedure SetFont(font: PByte);
    procedure WriteByte(Data: byte);
    procedure PrintChar(c: char; x, y: integer);
    procedure Write(Data, mode: byte);
    procedure Print(st: string; x, y: integer);
    procedure PrintNumI(num: longint; x, y, len: word; filler: char);
    procedure PrintNumF(num: double; Dec: byte; x, y: byte; divider: char; len: byte; filler: char);
    procedure Update();
    procedure ClrScr();
    procedure FillScr();
    procedure SetPixel(x, y: shortint);
    procedure ClrPixel(x, y: shortint);
    procedure InvPixel(x, y: shortint);
    procedure DrawHLine(x, y, l: integer);
    procedure ClrHLine(x, y, l: integer);
    procedure DrawVLine(x, y, l: integer);
    procedure ClrVLine(x, y, l: integer);
    procedure DrawLine(x1, y1, x2, y2: integer);
    procedure ClrLine(x1, y1, x2, y2: integer);
    procedure DrawRect(x1, y1, x2, y2: integer);
    procedure ClrRect(x1, y1, x2, y2: integer);
    procedure DrawRoundRect(x1, y1, x2, y2: integer);
    procedure ClrRoundRect(x1, y1, x2, y2: integer);
    procedure DrawCircle(x, y, radius: integer);
    procedure ClrCircle(x, y, radius: integer);
    procedure DrawBitmap(x, y: integer; bitmap: PByte; sx, sy: integer);
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

procedure TLCD5110.Reset;
begin
  GPIOOutputSet(P_DC, GPIO_LEVEL_HIGH);
  GPIOOutputSet(P_MOSI, GPIO_LEVEL_HIGH);
  GPIOOutputSet(P_SCK, GPIO_LEVEL_HIGH);
  GPIOOutputSet(P_CS, GPIO_LEVEL_HIGH);
  GPIOOutputSet(P_RST, GPIO_LEVEL_LOW);
  GPIOOutputSet(P_RST, GPIO_LEVEL_HIGH);
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

  {
  Write(LCD5110_FUNCTIONSET or LCD5110_EXTENDEDINSTRUCTION, LCD_COMMAND);
  Write(LCD5110_SETVOP or contrast, LCD_COMMAND);
  Write(LCD5110_SETTEMP or LCD_TEMP, LCD_COMMAND);
  Write(LCD5110_SETBIAS or LCD_BIAS, LCD_COMMAND);
  Write(LCD5110_FUNCTIONSET, LCD_COMMAND);
  Write(LCD5110_SETYADDR, LCD_COMMAND);
  Write(LCD5110_SETXADDR, LCD_COMMAND);

  for c:=0 to 503 do
      Write($00, LCD_DATA);

  Write(LCD5110_DISPLAYCONTROL or LCD5110_DISPLAYNORMAL, LCD_COMMAND);
  }

  GPIODeviceOutputSet(GPIODeviceGetDefault, GPIO_PIN_27, GPIO_LEVEL_LOW);
  WriteByte(LCD5110_FUNCTIONSET or LCD5110_EXTENDEDINSTRUCTION);
  WriteByte(LCD5110_SETVOP or contrast);
  WriteByte(LCD5110_SETTEMP or LCD_TEMP);
  WriteByte(LCD5110_SETBIAS or LCD_BIAS);
  WriteByte(LCD5110_FUNCTIONSET);
  WriteByte(LCD5110_SETYADDR);
  WriteByte(LCD5110_SETXADDR);

  GPIODeviceOutputSet(GPIODeviceGetDefault, GPIO_PIN_27, GPIO_LEVEL_HIGH);
  for c := 0 to 503 do
    WriteByte($00);

  GPIODeviceOutputSet(GPIODeviceGetDefault, GPIO_PIN_27, GPIO_LEVEL_LOW);
  WriteByte(LCD5110_DISPLAYCONTROL or LCD5110_DISPLAYNORMAL);

  cfont.font := nil;
  FSleep := False;
  FContrast := contrast;
  ClrScr();
end;

procedure TLCD5110.SetContrast(contrast: byte);
begin
  if contrast > $7F then
    contrast := $7F;

  {
  Write(LCD5110_FUNCTIONSET or LCD5110_EXTENDEDINSTRUCTION, LCD_COMMAND);
  Write(LCD5110_SETVOP or contrast, LCD_COMMAND);
  Write(LCD5110_FUNCTIONSET, LCD_COMMAND);
  }

  GPIODeviceOutputSet(GPIODeviceGetDefault, GPIO_PIN_27, GPIO_LEVEL_LOW);
  WriteByte(LCD5110_FUNCTIONSET or LCD5110_EXTENDEDINSTRUCTION);
  WriteByte(LCD5110_SETVOP or contrast);
  WriteByte(LCD5110_FUNCTIONSET);

  FContrast := contrast;
end;

procedure TLCD5110.EnableSleep();
var
  b: integer;
begin
  FSleep := True;

  {
  Write(LCD5110_SETYADDR, LCD_COMMAND);
  Write(LCD5110_SETXADDR, LCD_COMMAND);

  for b:=0 to 503 do
    Write(0, LCD_DATA);

  Write(LCD5110_FUNCTIONSET or LCD5110_POWERDOWN, LCD_COMMAND);
  }

  GPIODeviceOutputSet(GPIODeviceGetDefault, GPIO_PIN_27, GPIO_LEVEL_LOW);
  WriteByte(LCD5110_SETYADDR);
  WriteByte(LCD5110_SETXADDR);

  GPIODeviceOutputSet(GPIODeviceGetDefault, GPIO_PIN_27, GPIO_LEVEL_HIGH);
  for b := 0 to 503 do
    WriteByte($00);

  GPIODeviceOutputSet(GPIODeviceGetDefault, GPIO_PIN_27, GPIO_LEVEL_LOW);
  WriteByte(LCD5110_FUNCTIONSET or LCD5110_POWERDOWN);
end;

procedure TLCD5110.DisableSleep();
begin
  FSleep := False;
  {
  Write(LCD5110_FUNCTIONSET or LCD5110_EXTENDEDINSTRUCTION, LCD_COMMAND);
  Write(LCD5110_SETVOP or FContrast, LCD_COMMAND);
  Write(LCD5110_SETTEMP or LCD_TEMP, LCD_COMMAND);
  Write(LCD5110_SETBIAS or LCD_BIAS, LCD_COMMAND);
  Write(LCD5110_FUNCTIONSET, LCD_COMMAND);
  Write(LCD5110_DISPLAYCONTROL or LCD5110_DISPLAYNORMAL, LCD_COMMAND);
  }

  GPIODeviceOutputSet(GPIODeviceGetDefault, GPIO_PIN_27, GPIO_LEVEL_LOW);
  WriteByte(LCD5110_FUNCTIONSET or LCD5110_EXTENDEDINSTRUCTION);
  WriteByte(LCD5110_SETVOP or FContrast);
  WriteByte(LCD5110_SETTEMP or LCD_TEMP);
  WriteByte(LCD5110_SETBIAS or LCD_BIAS);
  WriteByte(LCD5110_FUNCTIONSET);
  WriteByte(LCD5110_DISPLAYCONTROL or LCD5110_DISPLAYNORMAL);

  Update();
end;

procedure TLCD5110.Update();
var
  b: integer;
begin
  if not FSleep then
  begin
    {
    Write(LCD5110_SETYADDR, LCD_COMMAND);
    Write(LCD5110_SETXADDR, LCD_COMMAND);

    for b := 0 to 503 do
      Write(scrbuf[b], LCD_DATA);
    }

    GPIODeviceOutputSet(GPIODeviceGetDefault, GPIO_PIN_27, GPIO_LEVEL_LOW);
    WriteByte(LCD5110_SETYADDR);
    WriteByte(LCD5110_SETXADDR);

    GPIODeviceOutputSet(GPIODeviceGetDefault, GPIO_PIN_27, GPIO_LEVEL_HIGH);
    for b := 0 to 503 do
      WriteByte(scrbuf[b]);
  end;
end;

procedure TLCD5110.ClrScr();
var
  c: integer;
begin
  for c := 0 to 503 do
    scrbuf[c] := 0;
end;

procedure TLCD5110.FillScr();
var
  c: integer;
begin
  for c := 0 to 503 do
    scrbuf[c] := 255;
end;

procedure TLCD5110.Invert(mode: boolean);
begin
  if mode then
    Write(LCD5110_DISPLAYCONTROL or LCD5110_DISPLAYINVERTED, LCD_COMMAND)
  else
    Write(LCD5110_DISPLAYCONTROL or LCD5110_DISPLAYNORMAL, LCD_COMMAND);
end;

procedure TLCD5110.SetPixel(x, y: shortint);
var
  by, bi: integer;
begin
  if (x >= 0) and (x < 84) and (y >= 0) and (y < 48) then
  begin
    by := ((y div 8) * 84) + x;
    bi := y mod 8;
    scrbuf[by] := scrbuf[by] or (1 shl bi);
  end;
end;

procedure TLCD5110.ClrPixel(x, y: shortint);
var
  by, bi: integer;
begin
  if (x >= 0) and (x < 84) and (y >= 0) and (y < 48) then
  begin
    by := ((y div 8) * 84) + x;
    bi := y mod 8;
    scrbuf[by] := scrbuf[by] and ((1 shl bi) xor -1);
  end;
end;

procedure TLCD5110.InvPixel(x, y: shortint);
var
  by, bi: integer;
begin
  if (x >= 0) and (x < 84) and (y >= 0) and (y < 48) then
  begin
    by := ((y div 8) * 84) + x;
    bi := y mod 8;

    if (scrbuf[by] and (1 shl bi)) = 0 then
      scrbuf[by] := scrbuf[by] or (1 shl bi)
    else
      scrbuf[by] := scrbuf[by] and ((1 shl bi) xor -1);
  end;
end;

procedure TLCD5110.InvertText(mode: boolean);
begin
  cfont.inverted := mode;
end;

procedure TLCD5110.Print(st: string; x, y: integer);
var
  stl, cnt: integer;
begin
  stl := Length(st);

  if x = RIGHT then
    x := 84 - (stl * cfont.x_size)
  else if x = CENTER then
    x := (84 - (stl * cfont.x_size)) div 2;

  for cnt := 1 to stl do
  begin
    PrintChar(st[cnt], x, y);
    Inc(x, cfont.x_size);
  end;
end;

procedure TLCD5110.PrintNumI(num: longint; x, y, len: word; filler: char);
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

procedure TLCD5110.PrintNumF(num: double; Dec: byte; x, y: byte; divider: char; len: byte; filler: char);
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

procedure TLCD5110.PrintChar(c: char; x, y: integer);
var
  rowcnt, font_idx, cnt, b, cbyte, cbit, cx, cy: integer;
begin
  if (cfont.y_size mod 8) = 0 then
  begin
    font_idx := ((Ord(c) - cfont.offset) * (cfont.x_size * (cfont.y_size div 8))) + 4;

    for rowcnt := 0 to (cfont.y_size div 8) - 1 do
      for cnt := 0 to cfont.x_size - 1 do
        for b := 0 to 7 do
          if ((cfont.font[font_idx + cnt + (rowcnt * cfont.x_size)] and (1 shl b)) <> 0) then
            if not cfont.inverted then
              SetPixel(x + cnt, y + (rowcnt * 8) + b)
            else
              ClrPixel(x + cnt, y + (rowcnt * 8) + b)
          else if not cfont.inverted then
            ClrPixel(x + cnt, y + (rowcnt * 8) + b)
          else
            SetPixel(x + cnt, y + (rowcnt * 8) + b);
  end
  else
  begin
    font_idx := ((Ord(c) - cfont.offset) * (cfont.x_size * (cfont.y_size div 8))) + 4;

    cbyte := cfont.font[font_idx];
    cbit := 7;

    for cx := 0 to cfont.x_size - 1 do
    begin
      for cy := 0 to cfont.y_size - 1 do
      begin
        if (cbyte and (1 shl cbit)) <> 0 then
          if not cfont.inverted then
            SetPixel(x + cx, y + cy)
          else
            ClrPixel(x + cx, y + cy)
        else if not cfont.inverted then
          ClrPixel(x + cx, y + cy)
        else
          SetPixel(x + cx, y + cy);
        Dec(cbit);

        if cbit < 0 then
        begin
          cbit := 7;
          Inc(font_idx);
          cbyte := cfont.font[font_idx];
        end;
      end;
    end;
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

procedure TLCD5110.DrawHLine(x, y, l: integer);
var
  by, bi, cx: integer;
begin
  if (x >= 0) and (x < 84) and (y >= 0) and (y < 48) then
    for cx := 0 to l - 1 do
    begin
      by := ((y div 8) * 84) + x;
      bi := y mod 8;
      scrbuf[by + cx] := scrbuf[by + cx] or (1 shl bi);
    end;
end;

procedure TLCD5110.ClrHLine(x, y, l: integer);
var
  by, bi, cx: integer;
begin
  if (x >= 0) and (x < 84) and (y >= 0) and (y < 48) then
    for cx := 0 to l - 1 do
    begin
      by := ((y div 8) * 84) + x;
      bi := y mod 8;
      scrbuf[by + cx] := scrbuf[by + cx] and ((1 shl bi) xor -1);
    end;
end;

procedure TLCD5110.DrawVLine(x, y, l: integer);
var
  cy: integer;
begin
  if (x >= 0) and (x < 84) and (y >= 0) and (y < 48) then
    for cy := 0 to l - 1 do
      SetPixel(x, y + cy);
end;

procedure TLCD5110.ClrVLine(x, y, l: integer);
var
  cy: integer;
begin
  if (x >= 0) and (x < 84) and (y >= 0) and (y < 48) then
    for cy := 0 to l - 1 do
      ClrPixel(x, y + cy);
end;

procedure TLCD5110.DrawLine(x1, y1, x2, y2: integer);
var
  tmp, i: integer;
  delta, tx, ty: double;
begin
  if (x2 - x1) < 0 then
  begin
    tmp := x1;
    x1 := x2;
    x2 := tmp;
    tmp := y1;
    y1 := y2;
    y2 := tmp;
  end;

  if (y2 - y1) < 0 then
  begin
    tmp := x1;
    x1 := x2;
    x2 := tmp;
    tmp := y1;
    y1 := y2;
    y2 := tmp;
  end;

  if y1 = y2 then
  begin
    if x1 > x2 then
    begin
      tmp := x1;
      x1 := x2;
      x2 := tmp;
    end;
    DrawHLine(x1, y1, x2 - x1);
  end
  else if x1 = x2 then
  begin
    if y1 > y2 then
    begin
      tmp := y1;
      y1 := y2;
      y2 := tmp;
    end;
    DrawVLine(x1, y1, y2 - y1);
  end
  else if Abs(x2 - x1) > Abs(y2 - y1) then
  begin
    delta := (double(y2 - y1) / double(x2 - x1));
    ty := double(y1);

    if x1 > x2 then
    begin
      for i := x1 downto x2 do
      begin
        SetPixel(i, Trunc(ty + 0.5));
        ty := ty - delta;
      end;
    end
    else
    begin
      for i := x1 to x2 do
      begin
        SetPixel(i, Trunc(ty + 0.5));
        ty := ty + delta;
      end;
    end;
  end
  else
  begin
    delta := ((x2 - x1) / (y2 - y1));
    tx := x1;

    if y1 > y2 then
    begin
      for i := y2 + 1 downto y1 + 1 do
      begin
        SetPixel(Trunc(tx + 0.5), i);
        tx := tx + delta;
      end;
    end
    else
    begin
      for i := y1 to y2 do
      begin
        SetPixel(Trunc(tx + 0.5), i);
        tx := tx + delta;
      end;
    end;
  end;
end;

procedure TLCD5110.ClrLine(x1, y1, x2, y2: integer);
var
  tmp, i: integer;
  delta, tx, ty: double;
begin
  if (x2 - x1) < 0 then
  begin
    tmp := x1;
    x1 := x2;
    x2 := tmp;
    tmp := y1;
    y1 := y2;
    y2 := tmp;
  end;

  if (y2 - y1) < 0 then
  begin
    tmp := x1;
    x1 := x2;
    x2 := tmp;
    tmp := y1;
    y1 := y2;
    y2 := tmp;
  end;

  if y1 = y2 then
  begin
    if x1 > x2 then
    begin
      tmp := x1;
      x1 := x2;
      x2 := tmp;
    end;

    ClrHLine(x1, y1, x2 - x1);
  end
  else if x1 = x2 then
  begin
    if y1 > y2 then
    begin
      tmp := y1;
      y1 := y2;
      y2 := tmp;
    end;

    ClrVLine(x1, y1, y2 - y1);
  end
  else if Abs(x2 - x1) > Abs(y2 - y1) then
  begin
    delta := (y2 - y1) / (x2 - x1);
    ty := y1;

    if x1 > x2 then
    begin
      for i := x1 downto x2 do
      begin
        ClrPixel(i, Trunc(ty + 0.5));
        ty := ty - delta;
      end;
    end
    else
    begin
      for i := x1 to x2 do
      begin
        ClrPixel(i, Trunc(ty + 0.5));
        ty := ty + delta;
      end;
    end;
  end
  else
  begin
    delta := (x2 - x1) / (y2 - y1);
    tx := x1;

    if y1 > y2 then
    begin
      for i := y2 + 1 downto y1 - 1 do
      begin
        ClrPixel(Trunc(tx + 0.5), i);
        tx := tx + delta;
      end;
    end
    else
    begin
      for i := y1 to y2 do
      begin
        ClrPixel(Trunc(tx + 0.5), i);
        tx := tx + delta;
      end;
    end;
  end;
end;

procedure TLCD5110.DrawRect(x1, y1, x2, y2: integer);
var
  tmp: integer;
begin
  if x1 > x2 then
  begin
    tmp := x1;
    x1 := x2;
    x2 := tmp;
  end;

  if y1 > y2 then
  begin
    tmp := y1;
    y1 := y2;
    y2 := tmp;
  end;

  DrawHLine(x1, y1, x2 - x1);
  DrawHLine(x1, y2, x2 - x1);
  DrawVLine(x1, y1, y2 - y1);
  DrawVLine(x2, y1, y2 - y1 + 1);
end;

procedure TLCD5110.ClrRect(x1, y1, x2, y2: integer);
var
  tmp: integer;
begin
  if x1 > x2 then
  begin
    tmp := x1;
    x1 := x2;
    x2 := tmp;
  end;

  if y1 > y2 then
  begin
    tmp := y1;
    y1 := y2;
    y2 := tmp;
  end;

  ClrHLine(x1, y1, x2 - x1);
  ClrHLine(x1, y2, x2 - x1);
  ClrVLine(x1, y1, y2 - y1);
  ClrVLine(x2, y1, y2 - y1 + 1);
end;

procedure TLCD5110.DrawRoundRect(x1, y1, x2, y2: integer);
var
  tmp: integer;
begin
  if x1 > x2 then
  begin
    tmp := x1;
    x1 := x2;
    x2 := tmp;
  end;
  if y1 > y2 then
  begin
    tmp := y1;
    y1 := y2;
    y2 := tmp;
  end;

  if ((x2 - x1) > 4) and ((y2 - y1) > 4) then
  begin
    SetPixel(x1 + 1, y1 + 1);
    SetPixel(x2 - 1, y1 + 1);
    SetPixel(x1 + 1, y2 - 1);
    SetPixel(x2 - 1, y2 - 1);
    DrawHLine(x1 + 2, y1, x2 - x1 - 3);
    DrawHLine(x1 + 2, y2, x2 - x1 - 3);
    DrawVLine(x1, y1 + 2, y2 - y1 - 3);
    DrawVLine(x2, y1 + 2, y2 - y1 - 3);
  end;
end;

procedure TLCD5110.ClrRoundRect(x1, y1, x2, y2: integer);
var
  tmp: integer;
begin
  if x1 > x2 then
  begin
    tmp := x1;
    x1 := x2;
    x2 := tmp;
  end;

  if y1 > y2 then
  begin
    tmp := y1;
    y1 := y2;
    y2 := tmp;
  end;

  if ((x2 - x1) > 4) and ((y2 - y1) > 4) then
  begin
    ClrPixel(x1 + 1, y1 + 1);
    ClrPixel(x2 - 1, y1 + 1);
    ClrPixel(x1 + 1, y2 - 1);
    ClrPixel(x2 - 1, y2 - 1);
    ClrHLine(x1 + 2, y1, x2 - x1 - 3);
    ClrHLine(x1 + 2, y2, x2 - x1 - 3);
    ClrVLine(x1, y1 + 2, y2 - y1 - 3);
    ClrVLine(x2, y1 + 2, y2 - y1 - 3);
  end;
end;

procedure TLCD5110.DrawCircle(x, y, radius: integer);
var
  f, ddF_x, ddF_y, x1, y1: integer;
begin
  f := 1 - radius;
  ddF_x := 1;
  ddF_y := -2 * radius;
  x1 := 0;
  y1 := radius;

  SetPixel(x, y + radius);
  SetPixel(x, y - radius);
  SetPixel(x + radius, y);
  SetPixel(x - radius, y);

  while x1 < y1 do
  begin
    if f >= 0 then
    begin
      Dec(y1);
      ddF_y := ddF_y + 2;
      f := f + ddF_y;
    end;

    Inc(x1);
    ddF_x := ddF_x + 2;
    f := f + ddF_x;

    SetPixel(x + x1, y + y1);
    SetPixel(x - x1, y + y1);
    SetPixel(x + x1, y - y1);
    SetPixel(x - x1, y - y1);
    SetPixel(x + y1, y + x1);
    SetPixel(x - y1, y + x1);
    SetPixel(x + y1, y - x1);
    SetPixel(x - y1, y - x1);
  end;
end;

procedure TLCD5110.ClrCircle(x, y, radius: integer);
var
  f, ddF_x, ddF_y, x1, y1: integer;
begin
  f := 1 - radius;
  ddF_x := 1;
  ddF_y := -2 * radius;
  x1 := 0;
  y1 := radius;

  ClrPixel(x, y + radius);
  ClrPixel(x, y - radius);
  ClrPixel(x + radius, y);
  ClrPixel(x - radius, y);

  while x1 < y1 do
  begin
    if f >= 0 then
    begin
      Dec(y1);
      ddF_y := ddF_y + 2;
      f := f + ddF_y;
    end;

    Inc(x1);
    ddF_x := ddF_x + 2;
    f := f + ddF_x;

    ClrPixel(x + x1, y + y1);
    ClrPixel(x - x1, y + y1);
    ClrPixel(x + x1, y - y1);
    ClrPixel(x - x1, y - y1);
    ClrPixel(x + y1, y + x1);
    ClrPixel(x - y1, y + x1);
    ClrPixel(x + y1, y - x1);
    ClrPixel(x - y1, y - x1);
  end;
end;

procedure TLCD5110.DrawBitmap(x, y: integer; bitmap: PByte; sx, sy: integer);
var
  bit, cx, cy: integer;
  Data: byte;
begin
  for cy := 0 to sy - 1 do
  begin
    bit := cy mod 8;

    for cx := 0 to sx - 1 do
    begin
      Data := bitmap[cx + ((cy div 8) * sx)];

      if (Data and (1 shl bit)) > 0 then
        SetPixel(x + cx, y + cy)
      else
        ClrPixel(x + cx, y + cy);
    end;
  end;
end;

end.

