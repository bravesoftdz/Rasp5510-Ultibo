unit LCD_Graphics;

interface

const
  arduino_logo: array [0..503] of byte = (
    $00, $00, $00, $00, $00, $00, $00, $00, $80, $C0, $E0, $F0, $F8, $F8, $FC, $FC,   // $0010 (16) pixels
    $FE, $FE, $7F, $7F, $7F, $3F, $3F, $3F, $3F, $3F, $7F, $7F, $7F, $FE, $FE, $FE,   // $0020 (32) pixels
    $FC, $FC, $F8, $F0, $F0, $E0, $C0, $80, $00, $00, $00, $00, $00, $80, $C0, $E0,   // $0030 (48) pixels
    $F0, $F0, $F8, $FC, $FC, $FE, $FE, $FE, $7F, $7F, $7F, $3F, $3F, $3F, $3F, $7F,   // $0040 (64) pixels
    $7F, $7F, $7F, $FE, $FE, $FC, $FC, $F8, $F8, $F0, $E0, $C0, $86, $06, $06, $00,   // $0050 (80) pixels
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $C0, $FC, $FF, $FF, $FF, $FF, $FF,   // $0060 (96) pixels
    $1F, $07, $01, $00, $00, $00, $00, $80, $80, $80, $80, $80, $80, $80, $80, $80,   // $0070 (112) pixels
    $80, $80, $00, $00, $01, $01, $03, $0F, $1F, $3F, $FF, $FF, $FF, $FE, $FC, $FE,   // $0080 (128) pixels
    $FF, $FF, $FF, $3F, $1F, $0F, $03, $01, $01, $00, $00, $80, $80, $80, $80, $F8,   // $0090 (144) pixels
    $F8, $F8, $F8, $80, $80, $80, $00, $00, $00, $00, $01, $07, $1F, $FF, $FF, $FF,   // $00A0 (160) pixels
    $FF, $FF, $FC, $E0, $00, $00, $00, $00, $00, $00, $00, $00, $00, $0F, $7F, $FF,   // $00B0 (176) pixels
    $FF, $FF, $FF, $FF, $F0, $C0, $00, $00, $00, $00, $00, $07, $07, $07, $07, $07,   // $00C0 (192) pixels
    $07, $07, $07, $07, $07, $03, $00, $00, $00, $00, $80, $C0, $F0, $F8, $FE, $FF,   // $00D0 (208) pixels
    $FF, $FF, $FF, $FF, $FF, $FF, $FE, $F8, $F0, $C0, $80, $00, $00, $00, $00, $07,   // $00E0 (224) pixels
    $07, $07, $07, $3F, $3F, $3F, $3F, $07, $07, $07, $00, $00, $00, $00, $00, $C0,   // $00F0 (240) pixels
    $F0, $FF, $FF, $FF, $FF, $FF, $7F, $0F, $00, $00, $00, $00, $00, $00, $00, $00,   // $0100 (256) pixels
    $00, $00, $00, $00, $03, $07, $0F, $1F, $3F, $7F, $7F, $FF, $FE, $FE, $FC, $FC,   // $0110 (272) pixels
    $F8, $F8, $F8, $F8, $F8, $F8, $F8, $FC, $FC, $FC, $FE, $FF, $7F, $7F, $3F, $3F,   // $0120 (288) pixels
    $1F, $0F, $07, $03, $01, $00, $00, $00, $01, $03, $07, $0F, $1F, $3F, $3F, $7F,   // $0130 (304) pixels
    $7F, $FF, $FE, $FC, $FC, $FC, $F8, $F8, $F8, $F8, $F8, $F8, $F8, $FC, $FC, $FE,   // $0140 (320) pixels
    $FE, $FF, $7F, $7F, $3F, $1F, $0F, $07, $03, $00, $00, $00, $00, $00, $00, $00,   // $0150 (336) pixels
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $C0, $C0, $C0, $80, $00, $00,   // $0160 (352) pixels
    $00, $00, $C1, $C1, $C1, $C1, $C1, $C1, $81, $81, $01, $01, $C1, $C1, $C0, $C0,   // $0170 (368) pixels
    $C0, $C0, $80, $00, $00, $00, $80, $C0, $C0, $00, $00, $00, $C0, $C0, $80, $00,   // $0180 (384) pixels
    $00, $C0, $C0, $C0, $C0, $C0, $C0, $C1, $C1, $01, $01, $C1, $C1, $C1, $01, $01,   // $0190 (400) pixels
    $01, $C1, $C1, $01, $00, $00, $80, $C0, $C0, $C0, $C0, $80, $80, $00, $00, $00,   // $01A0 (416) pixels
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $C0, $F8, $FF, $3F, $3B,   // $01B0 (432) pixels
    $3F, $7F, $FE, $F0, $00, $00, $FF, $FF, $FF, $1C, $3C, $FF, $FF, $C7, $00, $00,   // $01C0 (448) pixels
    $FF, $FF, $FF, $E0, $E0, $F1, $7F, $3F, $1F, $00, $3F, $7F, $FF, $E0, $C0, $E0,   // $01D0 (464) pixels
    $FF, $7F, $3F, $00, $00, $E0, $E0, $FF, $FF, $FF, $E0, $E0, $C0, $00, $00, $FF,   // $01E0 (480) pixels
    $FF, $07, $0F, $3E, $FC, $FF, $FF, $00, $00, $3F, $7F, $FF, $E0, $C0, $E1, $7F,   // $01F0 (496) pixels
    $7F, $0E, $00, $00, $00, $00, $00, $00);

  The_End: array [0..251] of byte = (
    $00, $80, $80, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $E0, $60, $00, $00,   // $0010 (16) pixels
    $80, $C0, $C0, $00, $00, $C0, $C0, $C0, $00, $00, $80, $C0, $C0, $C0, $C0, $E0,   // $0020 (32) pixels
    $C0, $C0, $C0, $C0, $E0, $60, $00, $00, $00, $00, $00, $C0, $C0, $C0, $C0, $E0,   // $0030 (48) pixels
    $E0, $C0, $C0, $C0, $E0, $E0, $00, $00, $00, $80, $80, $C0, $C0, $C0, $00, $00,   // $0040 (64) pixels
    $00, $00, $80, $E0, $F0, $F0, $60, $40, $F0, $F0, $F0, $F0, $E0, $E0, $C0, $C0,   // $0050 (80) pixels
    $80, $00, $00, $00, $00, $03, $03, $03, $81, $FC, $FF, $0F, $03, $00, $00, $00,   // $0060 (96) pixels
    $EE, $6F, $67, $FF, $FF, $7F, $71, $30, $F0, $FF, $3F, $39, $38, $18, $00, $01,   // $0070 (112) pixels
    $00, $F8, $FF, $1F, $0F, $0C, $0D, $8D, $80, $80, $00, $00, $00, $00, $00, $01,   // $0080 (128) pixels
    $00, $C0, $FF, $7F, $0F, $0C, $0D, $0D, $84, $80, $80, $07, $07, $83, $FF, $FF,   // $0090 (144) pixels
    $1F, $3F, $FE, $F8, $F8, $FE, $DF, $03, $00, $00, $00, $03, $FF, $FF, $1F, $00,   // $00A0 (160) pixels
    $00, $80, $81, $C3, $E7, $7F, $3E, $00, $00, $00, $00, $06, $07, $07, $03, $00,   // $00B0 (176) pixels
    $00, $00, $00, $00, $00, $00, $04, $0F, $07, $03, $00, $00, $0F, $0F, $07, $00,   // $00C0 (192) pixels
    $00, $06, $06, $3F, $1F, $0F, $0F, $0E, $06, $06, $06, $07, $07, $03, $00, $00,   // $00D0 (208) pixels
    $00, $00, $06, $1F, $3F, $1F, $0F, $0E, $06, $06, $06, $07, $07, $07, $03, $00,   // $00E0 (224) pixels
    $06, $0F, $07, $01, $00, $00, $00, $01, $0F, $07, $03, $00, $00, $18, $1C, $1F,   // $00F0 (240) pixels
    $0F, $07, $06, $07, $03, $03, $01, $01, $00, $00, $00, $00);

  pacman1: array [0..59] of byte = (
    $80, $E0, $F0, $F8, $FC, $FE, $FE, $FF, $FF, $FF, $FF, $FF, $FF, $7E, $3E, $1C,   // $0010 (16) pixels
    $0C, $00, $00, $00, $1F, $7F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $F9,   // $0020 (32) pixels
    $F0, $E0, $C0, $80, $00, $00, $00, $00, $00, $00, $00, $01, $03, $07, $07, $0F,   // $0030 (48) pixels
    $0F, $0F, $0F, $0F, $0F, $07, $07, $03, $03, $00, $00, $00);

  pacman2: array [0..59] of byte = (
    $80, $E0, $F0, $F8, $FC, $FE, $FE, $FF, $FF, $FF, $FF, $FF, $FF, $FE, $FE, $7C,   // $0010 (16) pixels
    $7C, $38, $20, $00, $1F, $7F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $F9,   // $0020 (32) pixels
    $F9, $F0, $F0, $E0, $E0, $C0, $40, $00, $00, $00, $00, $01, $03, $07, $07, $0F,   // $0030 (48) pixels
    $0F, $0F, $0F, $0F, $0F, $07, $07, $03, $03, $01, $00, $00);

  pacman3: array [0..59] of byte = (
    $80, $E0, $F0, $F8, $FC, $FE, $FE, $FF, $FF, $FF, $FF, $FF, $FF, $FE, $FE, $FC,   // $0010 (16) pixels
    $F8, $F0, $E0, $80, $1F, $7F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,   // $0020 (32) pixels
    $FF, $FF, $FF, $FF, $FB, $F9, $79, $19, $00, $00, $00, $01, $03, $07, $07, $0F,   // $0030 (48) pixels
    $0F, $0F, $0F, $0F, $0F, $07, $07, $03, $01, $00, $00, $00);

  pill: array [0..4] of byte = (
    $0E, $1F, $1F, $1F, $0E);

implementation

end.
