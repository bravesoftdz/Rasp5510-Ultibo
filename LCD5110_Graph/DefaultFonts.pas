unit DefaultFonts;

interface

var
  SmallFont: array[0..573] of byte = (
    $06, $08, $20, $5F,
    $00, $00, $00, $00, $00, $00, // sp
    $00, $00, $00, $2F, $00, $00, // !
    $00, $00, $07, $00, $07, $00, // "
    $00, $14, $7F, $14, $7F, $14, // #
    $00, $24, $2A, $7F, $2A, $12, // $
    $00, $23, $13, $08, $64, $62, // %
    $00, $36, $49, $55, $22, $50, // &
    $00, $00, $05, $03, $00, $00, // '
    $00, $00, $1C, $22, $41, $00, // (
    $00, $00, $41, $22, $1C, $00, // )
    $00, $14, $08, $3E, $08, $14, // *
    $00, $08, $08, $3E, $08, $08, // +
    $00, $00, $00, $A0, $60, $00, // ,
    $00, $08, $08, $08, $08, $08, // -
    $00, $00, $60, $60, $00, $00, // .
    $00, $20, $10, $08, $04, $02, // /

    $00, $3E, $51, $49, $45, $3E, // 0
    $00, $00, $42, $7F, $40, $00, // 1
    $00, $42, $61, $51, $49, $46, // 2
    $00, $21, $41, $45, $4B, $31, // 3
    $00, $18, $14, $12, $7F, $10, // 4
    $00, $27, $45, $45, $45, $39, // 5
    $00, $3C, $4A, $49, $49, $30, // 6
    $00, $01, $71, $09, $05, $03, // 7
    $00, $36, $49, $49, $49, $36, // 8
    $00, $06, $49, $49, $29, $1E, // 9
    $00, $00, $36, $36, $00, $00, // :
    $00, $00, $56, $36, $00, $00, // ;
    $00, $08, $14, $22, $41, $00, // <
    $00, $14, $14, $14, $14, $14, // =
    $00, $00, $41, $22, $14, $08, // >
    $00, $02, $01, $51, $09, $06, // ?

    $00, $32, $49, $59, $51, $3E, // @
    $00, $7C, $12, $11, $12, $7C, // A
    $00, $7F, $49, $49, $49, $36, // B
    $00, $3E, $41, $41, $41, $22, // C
    $00, $7F, $41, $41, $22, $1C, // D
    $00, $7F, $49, $49, $49, $41, // E
    $00, $7F, $09, $09, $09, $01, // F
    $00, $3E, $41, $49, $49, $7A, // G
    $00, $7F, $08, $08, $08, $7F, // H
    $00, $00, $41, $7F, $41, $00, // I
    $00, $20, $40, $41, $3F, $01, // J
    $00, $7F, $08, $14, $22, $41, // K
    $00, $7F, $40, $40, $40, $40, // L
    $00, $7F, $02, $0C, $02, $7F, // M
    $00, $7F, $04, $08, $10, $7F, // N
    $00, $3E, $41, $41, $41, $3E, // O

    $00, $7F, $09, $09, $09, $06, // P
    $00, $3E, $41, $51, $21, $5E, // Q
    $00, $7F, $09, $19, $29, $46, // R
    $00, $46, $49, $49, $49, $31, // S
    $00, $01, $01, $7F, $01, $01, // T
    $00, $3F, $40, $40, $40, $3F, // U
    $00, $1F, $20, $40, $20, $1F, // V
    $00, $3F, $40, $38, $40, $3F, // W
    $00, $63, $14, $08, $14, $63, // X
    $00, $07, $08, $70, $08, $07, // Y
    $00, $61, $51, $49, $45, $43, // Z
    $00, $00, $7F, $41, $41, $00, // [
    $AA, $55, $AA, $55, $AA, $55, // Backslash (Checker pattern)
    $00, $00, $41, $41, $7F, $00, // ]
    $00, $04, $02, $01, $02, $04, // ^
    $00, $40, $40, $40, $40, $40, // _

    $00, $00, $03, $05, $00, $00, // `
    $00, $20, $54, $54, $54, $78, // a
    $00, $7F, $48, $44, $44, $38, // b
    $00, $38, $44, $44, $44, $20, // c
    $00, $38, $44, $44, $48, $7F, // d
    $00, $38, $54, $54, $54, $18, // e
    $00, $08, $7E, $09, $01, $02, // f
    $00, $18, $A4, $A4, $A4, $7C, // g
    $00, $7F, $08, $04, $04, $78, // h
    $00, $00, $44, $7D, $40, $00, // i
    $00, $40, $80, $84, $7D, $00, // j
    $00, $7F, $10, $28, $44, $00, // k
    $00, $00, $41, $7F, $40, $00, // l
    $00, $7C, $04, $18, $04, $78, // m
    $00, $7C, $08, $04, $04, $78, // n
    $00, $38, $44, $44, $44, $38, // o

    $00, $FC, $24, $24, $24, $18, // p
    $00, $18, $24, $24, $18, $FC, // q
    $00, $7C, $08, $04, $04, $08, // r
    $00, $48, $54, $54, $54, $20, // s
    $00, $04, $3F, $44, $40, $20, // t
    $00, $3C, $40, $40, $20, $7C, // u
    $00, $1C, $20, $40, $20, $1C, // v
    $00, $3C, $40, $30, $40, $3C, // w
    $00, $44, $28, $10, $28, $44, // x
    $00, $1C, $A0, $A0, $A0, $7C, // y
    $00, $44, $64, $54, $4C, $44, // z
    $00, $00, $10, $7C, $82, $00, // {
    $00, $00, $00, $FF, $00, $00, // |
    $00, $00, $82, $7C, $10, $00, // }
    $00, $00, $06, $09, $09, $06 // ~ (Degrees)
    );

  MediumNumbers: array[0..315] of byte = (
    $0C, $10, $2D, $0D,
    $00, $00, $00, $80, $80, $80, $80, $80, $80, $00, $00, $00,
    $00, $00, $01, $03, $03, $03, $03, $03, $03, $01, $00, $00, // -
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $C0, $C0, $00, $00, $00, $00, $00, // .
    $00, $00, $02, $86, $86, $86, $86, $86, $86, $02, $00, $00, $00, $00, $81, $C3, $C3, $C3, $C3, $C3, $C3, $81, $00, $00, // /
    $00, $FC, $7A, $06, $06, $06, $06, $06, $06, $7A, $FC, $00,
    $00, $7E, $BC, $C0, $C0, $C0, $C0, $C0, $C0, $BC, $7E, $00, // 0
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $78, $FC, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $3C, $7E, $00, // 1
    $00, $00, $02, $86, $86, $86, $86, $86, $86, $7A, $FC, $00, $00, $7E, $BD, $C3, $C3, $C3, $C3, $C3, $C3, $81, $00, $00, // 2
    $00, $00, $02, $86, $86, $86, $86, $86, $86, $7A, $FC, $00, $00, $00, $81, $C3, $C3, $C3, $C3, $C3, $C3, $BD, $7E, $00, // 3
    $00, $FC, $78, $80, $80, $80, $80, $80, $80, $78, $FC, $00, $00, $00, $01, $03, $03, $03, $03, $03, $03, $3D, $7E, $00, // 4
    $00, $FC, $7A, $86, $86, $86, $86, $86, $86, $02, $00, $00, $00, $00, $81, $C3, $C3, $C3, $C3, $C3, $C3, $BD, $7E, $00, // 5
    $00, $FC, $7A, $86, $86, $86, $86, $86, $86, $02, $00, $00, $00, $7E, $BD, $C3, $C3, $C3, $C3, $C3, $C3, $BD, $7E, $00, // 6
    $00, $00, $02, $06, $06, $06, $06, $06, $06, $7A, $FC, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $3C, $7E, $00, // 7
    $00, $FC, $7A, $86, $86, $86, $86, $86, $86, $7A, $FC, $00, $00, $7E, $BD, $C3, $C3, $C3, $C3, $C3, $C3, $BD, $7E, $00, // 8
    $00, $FC, $7A, $86, $86, $86, $86, $86, $86, $7A, $FC, $00, $00, $00, $81, $C3, $C3, $C3, $C3, $C3, $C3, $BD, $7E, $00 // 9
    );

  BigNumbers: array[0..549] of byte = ($0E, $18, $2D, $0D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $10, $38, $38, $38, $38, $38, $38, $38, $38, $10, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, // -
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $40, $E0, $E0, $40, $00, $00, $00, $00, $00, // .
    $00, $00, $02, $06, $0E, $0E, $0E, $0E, $0E, $0E, $06, $02, $00, $00, $00, $00, $10, $38, $38, $38, $38, $38, $38, $38, $38, $10, $00, $00, $00, $00, $80, $C0, $E0, $E0, $E0, $E0, $E0, $E0, $C0, $80, $00, $00, // /
    $00, $FC, $FA, $F6, $0E, $0E, $0E, $0E, $0E, $0E, $F6, $FA, $FC, $00, $00, $EF, $C7, $83, $00, $00, $00, $00, $00, $00, $83, $C7, $EF, $00, $00, $7F, $BF, $DF, $E0, $E0, $E0, $E0, $E0, $E0, $DF, $BF, $7F, $00, // 0
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $F0, $F8, $FC, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $83, $C7, $EF, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $1F, $3F, $7F, $00, // 1
    $00, $00, $02, $06, $0E, $0E, $0E, $0E, $0E, $0E, $F6, $FA, $FC, $00, $00, $E0, $D0, $B8, $38, $38, $38, $38, $38, $38, $3B, $17, $0F, $00, $00, $7F, $BF, $DF, $E0, $E0, $E0, $E0, $E0, $E0, $C0, $80, $00, $00, // 2
    $00, $00, $02, $06, $0E, $0E, $0E, $0E, $0E, $0E, $F6, $FA, $FC, $00, $00, $00, $10, $38, $38, $38, $38, $38, $38, $38, $BB, $D7, $EF, $00, $00, $00, $80, $C0, $E0, $E0, $E0, $E0, $E0, $E0, $DF, $BF, $7F, $00, // 3
    $00, $FC, $F8, $F0, $00, $00, $00, $00, $00, $00, $F0, $F8, $FC, $00, $00, $0F, $17, $3B, $38, $38, $38, $38, $38, $38, $BB, $D7, $EF, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $1F, $3F, $7F, $00, // 4
    $00, $FC, $FA, $F6, $0E, $0E, $0E, $0E, $0E, $0E, $06, $02, $00, $00, $00, $0F, $17, $3B, $38, $38, $38, $38, $38, $38, $B8, $D0, $E0, $00, $00, $00, $80, $C0, $E0, $E0, $E0, $E0, $E0, $E0, $DF, $BF, $7F, $00, // 5
    $00, $FC, $FA, $F6, $0E, $0E, $0E, $0E, $0E, $0E, $06, $02, $00, $00, $00, $EF, $D7, $BB, $38, $38, $38, $38, $38, $38, $B8, $D0, $E0, $00, $00, $7F, $BF, $DF, $E0, $E0, $E0, $E0, $E0, $E0, $DF, $BF, $7F, $00, // 6
    $00, $00, $02, $06, $0E, $0E, $0E, $0E, $0E, $0E, $F6, $FA, $FC, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $83, $C7, $EF, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $1F, $3F, $7F, $00, // 7
    $00, $FC, $FA, $F6, $0E, $0E, $0E, $0E, $0E, $0E, $F6, $FA, $FC, $00, $00, $EF, $D7, $BB, $38, $38, $38, $38, $38, $38, $BB, $D7, $EF, $00, $00, $7F, $BF, $DF, $E0, $E0, $E0, $E0, $E0, $E0, $DF, $BF, $7F, $00, // 8
    $00, $FC, $FA, $F6, $0E, $0E, $0E, $0E, $0E, $0E, $F6, $FA, $FC, $00, $00, $0F, $17, $3B, $38, $38, $38, $38, $38, $38, $BB, $D7, $EF, $00, $00, $00, $80, $C0, $E0, $E0, $E0, $E0, $E0, $E0, $DF, $BF, $7F, $00 // 9
    );

  TinyFont: array[0..288] of byte = ($04, $06, $20, $5F, $00, $00, $00, $03, $A0, $00, $C0, $0C, $00, $F9, $4F, $80, $6B, $EB, $00, $98, $8C, $80, $52, $A5, $80, $03, $00, $00, // Space, !"#$%&'
    $01, $C8, $80, $89, $C0, $00, $50, $85, $00, $21, $C2, $00, $08, $40, $00, $20, $82, $00, $00, $20, $00, $18, $8C, $00, // ()*+,-./
    $FA, $2F, $80, $4B, $E0, $80, $5A, $66, $80, $8A, $A5, $00, $E0, $8F, $80, $EA, $AB, $00, $72, $A9, $00, $9A, $8C, $00, // 01234567
    $FA, $AF, $80, $4A, $A7, $00, $01, $40, $00, $09, $40, $00, $21, $48, $80, $51, $45, $00, $89, $42, $00, $42, $66, $00, // 89:;<=>?
    $72, $A6, $80, $7A, $87, $80, $FA, $A5, $00, $72, $25, $00, $FA, $27, $00, $FA, $A8, $80, $FA, $88, $00, $72, $2B, $00, // @ABCDEFG
    $F8, $8F, $80, $8B, $E8, $80, $8B, $E8, $00, $F8, $8D, $80, $F8, $20, $80, $F9, $0F, $80, $F9, $CF, $80, $72, $27, $00, // HIJKLMNO
    $FA, $84, $00, $72, $27, $40, $FA, $85, $80, $4A, $A9, $00, $83, $E8, $00, $F0, $2F, $00, $E0, $6E, $00, $F0, $EF, $00, // PQRSTUVW
    $D8, $8D, $80, $C0, $EC, $00, $9A, $AC, $80, $03, $E8, $80, $C0, $81, $80, $8B, $E0, $00, $42, $04, $00, $08, $20, $80, // XYZ[\]^_
    $02, $04, $00, $31, $23, $80, $F9, $23, $00, $31, $24, $80, $31, $2F, $80, $31, $62, $80, $23, $EA, $00, $25, $53, $80, // `abcdefg
    $F9, $03, $80, $02, $E0, $00, $06, $E0, $00, $F8, $42, $80, $03, $E0, $00, $79, $87, $80, $39, $03, $80, $31, $23, $00, // hijklmno
    $7D, $23, $00, $31, $27, $C0, $78, $84, $00, $29, $40, $00, $43, $E4, $00, $70, $27, $00, $60, $66, $00, $70, $67, $00, // pqrstuvw
    $48, $C4, $80, $74, $57, $80, $59, $E6, $80, $23, $E8, $80, $03, $60, $00, $8B, $E2, $00, $61, $0C, $00 // zyx{|}~
    );

implementation

end.

