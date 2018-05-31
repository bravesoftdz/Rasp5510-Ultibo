unit PCD8544;

{
Copyright (C) 2016 - Ronald Daleske

some procedures in this unit have taken some ideas of the C sources of:

http://forum.arduino.cc/index.php?topic=271079.0
and
http://playground.arduino.cc/Code/PCD8544
and
https://learn.sparkfun.com/tutorials/graphic-lcd-hookup-guide/discuss

LcdInitialise.c

Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)
}

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Threads,
  Platform,
  Devices,
  SPI,
  GlobalConst,
  GPIO,
  DefaultFonts, LCD_Graphics;

type
  FontChar_5x8 = array [1..5] of byte;

  PByte = ^byte;

  TCurrentFont = record
    font: PByte;
    x_size: byte;
    y_size: byte;
    offset: byte;
    numchars: byte;
  end;


const

{$I Font_5x8.inc}

var
  X_Pos, Y_Pos: byte;
  SPIDevice: PSPIDevice;

{
     1-VCC       -> 3.3V
     2-GND       -> GND
     3-SCE       -> GPIO8 = SPI CE0
     4-RST       -> GPIO17
     5-D/C       -> GPIO27
     6-DN(MOSI)  -> GPIO10 = MOSI
     7-SCLK      -> GPIO11 = SCLK
     8-LED       ->
}

{
3 SCE Chip select Input Active low
4 RST Reset Input Active low
5 D/C Mode select Input Select between command mode (low) and data mode (high).
6 DN(MOSI) Serial data in Input
7 SCLK Serial clock Input
8 LED LED backlight supply Input Maximum voltage supply is 3.3V.
}

procedure LCD_GotoXY(pos_x, pos_y: byte);
procedure LCD_Clear;
procedure LCD_Picture(Picture_Array: array of byte; col_size: byte = 0);
procedure LCD_Character(Character: char);
procedure LCD_String(C_String: string);
procedure LCD_CharAt(pos_x, pos_y: byte; Character: char);
procedure LCD_StringAt(pos_x, pos_y: byte; C_String: string);
procedure LCD_Font(font: PByte);
procedure LCD_PrintChar(c: char);
procedure LCD_Reverse(rev: boolean); overload;
function LCD_Reverse: boolean; overload;

procedure test;

implementation

var
  cfont: TCurrentFont;
  FInternalFont: boolean = True;
  FReverse: boolean = False;

// Reset Input Active low
procedure RESET_active;
begin
  GPIODeviceOutputSet(GPIODeviceGetDefault, GPIO_PIN_17, GPIO_LEVEL_LOW);
end;

// Reset Input Active low
procedure RESET_inactive;
begin
  GPIODeviceOutputSet(GPIODeviceGetDefault, GPIO_PIN_17, GPIO_LEVEL_HIGH);
end;

// D/C Mode select Input Select between command mode (low) and data mode (high)
procedure DC_Command;
begin
  GPIODeviceOutputSet(GPIODeviceGetDefault, GPIO_PIN_27, GPIO_LEVEL_LOW);
end;

// D/C Mode select Input Select between command mode (low) and data mode (high)
procedure DC_Data;
begin
  GPIODeviceOutputSet(GPIODeviceGetDefault, GPIO_PIN_27, GPIO_LEVEL_HIGH);
end;

// Write one Byte to LCD, MSB first
procedure LCD_Write_Byte(LCD_byte: byte);
var
  Source_Value, Dest_Value: byte;
  Count: longword;
  SourceBuffer: Pointer;
  DestBuffer: Pointer;
begin
  SourceBuffer := @Source_Value;
  DestBuffer := @Dest_Value;
  Source_Value := LCD_byte;
  Count := 0;

  if SPIDeviceWriteRead(SPIDevice, SPI_CS_0, SourceBuffer, DestBuffer, 1, SPI_TRANSFER_NONE, Count) = ERROR_SUCCESS then
  begin
    // do nothing
  end;
end; { procedure LCD_Write_Byte }

// Write one Data-Byte to LCD
procedure LCD_Write_Data(LCD_data: byte);
begin
  DC_Data;
  LCD_Write_Byte(LCD_data);
end; { procedure LCD_Write_Data }

// Write one Command-Byte to LCD
procedure LCD_Write_Command(LCD_command: byte);
begin
  DC_Command;
  LCD_Write_Byte(LCD_command);
end; { procedure LCD_Write_Command }

// GotoXY
procedure LCD_GotoXY(pos_x, pos_y: byte);
var
  Value: byte;
begin
  Value := $80 or pos_x;
  LCD_Write_Command(Value);
  Value := $40 or pos_y;
  LCD_Write_Command(Value);
  X_Pos := pos_x;
  Y_Pos := pos_y;
end; { procedure LCD_GotoXY }

// LCD_Clear
procedure LCD_Clear;
var
  LCD_counter: word;
begin
  // (LCD_X * LCD_Y / 8) => (84x48 / 8) => 504
  for LCD_counter := 0 to 503 do
    LCD_Write_Data($00);

  LCD_GotoXY(0, 0);
end; { procedure LCD_Clear }

// LCD_Picture
procedure LCD_Picture(Picture_Array: array of byte; col_size: byte = 0);
var
  LCD_counter, x, y, i: word;
begin
  if col_size = 0 then
  begin
    //LCD_GotoXY(0,0);

    // (LCD_X * LCD_Y / 8) => (84x48 / 8) => 504
    for LCD_counter := Low(Picture_Array) to High(Picture_Array) do
      LCD_Write_Data(Picture_Array[LCD_counter]);
  end
  else
  begin
    x := X_Pos;
    y := Y_Pos;
    i := 0;

    for LCD_counter := Low(Picture_Array) to High(Picture_Array) do
    begin
      if i = col_size then
      begin
        i := 0;
        Inc(y);
        LCD_GotoXY(x, y);
      end;

      LCD_Write_Data(Picture_Array[LCD_counter]);
      Inc(i);
    end;
  end;
end; { procedure LCD_Picture }

// LCD_Character
procedure LCD_Character(Character: char);
var
  LCD_counter: word;
  temp_FontChar_5x8: FontChar_5x8;
  Font_Byte: byte;
begin
  if FInternalFont then
  begin
    if Character >= ' ' then
    begin
      temp_FontChar_5x8 := Font_5x8[byte(Character)];

      for LCD_counter := 1 to 5 do
      begin
        Font_Byte := temp_FontChar_5x8[LCD_counter];

        if FReverse then
          Font_Byte := Font_Byte xor -1;

        LCD_Write_Data(Font_Byte);
      end; { for LCD_counter:=1 to 5 do }

      LCD_Write_Data(0);
    end; { if Character>=' ' then }
  end
  else
    LCD_PrintChar(Character);
end; { procedure LCD_Character }

// LCD_String
procedure LCD_String(C_String: string);
var
  LCD_counter: word;
begin
  for LCD_counter := 1 to Length(C_String) do
    LCD_Character(C_String[LCD_counter]);
end; { procedure LCD_String }

procedure LCD_CharAt(pos_x, pos_y: byte; Character: char);
begin
  LCD_GotoXY(pos_x, pos_y);
  LCD_Character(Character);
end;

procedure LCD_StringAt(pos_x, pos_y: byte; C_String: string);
begin
  LCD_GotoXY(pos_x, pos_y);
  LCD_String(C_String);
end;

procedure Init_PCD8544;
begin
  X_Pos := 0;
  Y_Pos := 0;
  FInternalFont := True;

  {
  4-RST       -> GPIO17
  5-D/C       -> GPIO27
  }

  GPIODeviceFunctionSelect(GPIODeviceGetDefault, GPIO_PIN_17, GPIO_FUNCTION_OUT);
  GPIODeviceFunctionSelect(GPIODeviceGetDefault, GPIO_PIN_27, GPIO_FUNCTION_OUT);

  SPIDevice := PSPIDevice(DeviceFindByDescription('BCM2836 SPI0 Master'));

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
  RESET_inactive;

  // D/C Mode select Input Select between command mode (low) and data mode (high)
  DC_Command;

  // *** RESET ***
  // Reset Input Active low
  RESET_active;
  MicrosecondDelay(20);
  RESET_inactive;

  //Tell LCD extended commands follow
  LCD_Write_Command($21);

  //Set LCD Vop (Contrast)
  //LCD_Write_Command($B1);
  //LCD_Write_Command($80 or $70);
  LCD_Write_Command($BF);

  //Set Temp coefficent
  LCD_Write_Command($04);

  //LCD bias mode 1:48 (try 0x13)
  LCD_Write_Command($14);

  //We must send 0x20 before modifying the display control mode
  LCD_Write_Command($20);

  //Set display control, normal mode.
  LCD_Write_Command($0C);

  LCD_GotoXY(0, 0);
  LCD_Clear;
end; { procedure Init_PCD8544 }

procedure LCD_Font(font: PByte);
begin
  if font = nil then
    FInternalFont := True
  else
  begin
    cfont.font := font;
    cfont.x_size := cfont.font[0];
    cfont.y_size := cfont.font[1];
    cfont.offset := cfont.font[2];
    cfont.numchars := cfont.font[3];
    //cfont.inverted:=False;
    FInternalFont := False;
  end;
end;

procedure LCD_PrintChar(c: char);
var
  rowcnt, font_idx, cnt, x, row: integer;
begin
  if ((X_Pos + cfont.x_size) <= 84) and (Y_Pos + (cfont.y_size / 8) <= 6) then
  begin
    x := X_Pos;
    row := Y_Pos;

    for rowcnt := 0 to (cfont.y_size div 8) - 1 do
    begin
      LCD_GotoXY(x, row + rowcnt);
      font_idx := ((Ord(c) - cfont.offset) * (cfont.x_size * (cfont.y_size div 8))) + 4 + (rowcnt * cfont.x_size);

      for cnt := 0 to cfont.x_size - 1 do
      begin
        if not FReverse then
          LCD_Write_Data(cfont.font[font_idx])
        else
          LCD_Write_Data(cfont.font[font_idx] xor -1);

        Inc(font_idx);
      end;
    end;

    LCD_GotoXY(x + cfont.x_size, row);
  end;
end;

procedure LCD_Reverse(rev: boolean); overload;
begin
  FReverse := rev;
end;

function LCD_Reverse: boolean; overload;
begin
  Result := FReverse;
end;

procedure test;
var
  i: integer;
  c: char;
begin
  LCD_StringAt(0, 0, 'Hello World!!!');
  LCD_StringAt(0, 1, 'This is a test');
  LCD_StringAt(0, 2, 'of ultibo core');
  LCD_StringAt(0, 3, 'SPI-Driver');
  LCD_StringAt(0, 4, 'written by Ron');
  LCD_StringAt(0, 5, 'on 29.06.2016');
  Sleep(2000);

  LCD_Reverse(True);
  LCD_StringAt(0, 0, 'Hello World!!!');
  LCD_StringAt(0, 1, 'This is a test');
  LCD_StringAt(0, 2, 'of ultibo core');
  LCD_StringAt(0, 3, 'SPI-Driver');
  LCD_StringAt(0, 4, 'written by Ron');
  LCD_StringAt(0, 5, 'on 29.06.2016');
  Sleep(2000);

  LCD_Reverse(False);
  LCD_Font(SmallFont);
  LCD_Clear;
  LCD_StringAt(0, 0, 'Upper case:');  //, LEFT, 0);
  LCD_StringAt(0, 1, 'ABCDEFGHIJKLM');  //, CENTER, 16);
  LCD_StringAt(0, 2, 'NOPQRSTUVWXYZ');  //, CENTER, 24);
  Sleep(2000);

  LCD_Clear;
  LCD_StringAt(0, 0, 'Lower case:');  //, LEFT, 0);
  LCD_StringAt(0, 1, 'abcdefghijklm');  //, CENTER, 16);
  LCD_StringAt(0, 2, 'nopqrstuvwxyz');  //, CENTER, 24);
  Sleep(2000);

  LCD_Clear;
  LCD_StringAt(0, 0, 'Numbers:');  //, LEFT, 0);
  LCD_StringAt(0, 1, '0123456789');  //, CENTER, 16);
  Sleep(2000);

  LCD_Clear;
  LCD_StringAt(0, 0, 'Special:');  //, LEFT, 0);
  LCD_StringAt(0, 1, '!\"#$%&''()*+,-.');  //, CENTER, 16);
  LCD_StringAt(0, 2, '/:;<=>?@[\\]^_`');  //, CENTER, 24);
  LCD_StringAt(0, 3, '{|}~');  //, CENTER, 32);
  Sleep(2000);

  for i := 0 to 9 do
  begin
    LCD_Reverse(not LCD_Reverse);
    LCD_GotoXY(0, 0);
    LCD_StringAt(0, 0, 'Special:');  //, LEFT, 0);
    LCD_StringAt(0, 1, '!\"#$%&''()*+,-.');  //, CENTER, 16);
    LCD_StringAt(0, 2, '/:;<=>?@[\\]^_`');  //, CENTER, 24);
    LCD_StringAt(0, 3, '{|}~');  //, CENTER, 32);
    Sleep(50);
  end;

  //LCD_Reverse(False);
  Sleep(1000);

  {
  LCD_Clear;
  LCD_StringAt(0,0,'Medium Numbers:');  //, LEFT, 0);
  LCD_Font(MediumNumbers);

  LCD_String('-./');

  for c := '0' to '9' do
    LCD_Character(c);

  Sleep(2000);
  end;
  }

  LCD_Clear;
  LCD_Picture(arduino_logo);

  Sleep(2000);
  LCD_Clear;

  for i := 1 to 20 do
  begin
    LCD_GotoXY(0, 0);
    LCD_Picture(pacman1, 20);
    Sleep(100);
    LCD_GotoXY(0, 0);
    LCD_Picture(pacman2, 20);
    Sleep(100);
    LCD_GotoXY(0, 0);
    LCD_Picture(pacman3, 20);
    Sleep(100);
  end;

  Sleep(1000);

  LCD_Clear;
  LCD_Picture(pill);
  Sleep(2000);

  LCD_Clear;
  LCD_Picture(The_End);
end;

{==============================================================================}
{==============================================================================}

initialization
  Init_PCD8544;

  {==============================================================================}

finalization
  {Nothing}

  {==============================================================================}
  {==============================================================================}

end.

