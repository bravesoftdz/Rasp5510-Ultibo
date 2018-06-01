unit ThreadSerial;

{$mode objfpc}{$H+}

interface

uses
  Threads, Classes, SysUtils, Console, Platform, GlobalConfig, GlobalTypes;

type
  TProcAdjust = procedure (second, minute, hour, day, month, year: Integer);

  { TThreadSerial }
  TThreadSerial = class(TThread)
  private
    FProcAdjust: TProcAdjust;
    FWindowHandle: TWindowHandle;
    fsecond, fminute, fhour, fday, fmonth, fyear: Byte;
    ffg, fbg: LongWord;
  public
    constructor Create(WindowHandle: TWindowHandle; ProcAdjust: TProcAdjust; second, minute, hour, day, month, year: Byte; fg, bg: LongWord);
    procedure Execute; override;
  end;

implementation

{ TThreadSerial }

constructor TThreadSerial.Create(WindowHandle: TWindowHandle; ProcAdjust: TProcAdjust; second, minute, hour, day, month, year: Byte; fg, bg: LongWord);
begin
  inherited Create(False, THREAD_STACK_DEFAULT_SIZE);

  FProcAdjust := ProcAdjust;
  FWindowHandle := WindowHandle;
  ffg:=fg;
  fbg:=bg;
  fsecond:=second;
  fminute:=minute;
  fhour:=hour;
  fday:=day;
  fmonth:=month;
  fyear:=year;
end;

procedure TThreadSerial.Execute;
var
  Count: LongWord;
  Character: char;
  buffer: string;
  sc, mn, hr, dy, mt, yr: Integer;

  function Number: Integer;
  var
    c: Char;
    b: String;
  begin
    Result := -1;
    b := '';
    buffer := Copy(buffer, 2, Length(buffer) - 1);

    while buffer <> '' do
    begin
      c := buffer[1];
      buffer := Copy(buffer, 2, Length(buffer) - 1);

      if c in ['0'..'9'] then
        b := b + c
      else
        Break;
    end;

    Result := StrToIntDef(b, -1);
  end;

begin
  buffer := '';

  while not Suspended do
  begin
    try
      SerialRead(@Character, SizeOf(Character), Count);

      if (Character = #13) or (Character > #32) then
        if Character <> #13 then
          begin
            buffer := buffer + Character;
            Continue;
          end
        else
        begin
          buffer := UpperCase(Trim(buffer));

          if buffer = '' then
            Continue;

          sc := -1;
          mn := -1;
          hr := -1;
          dy := -1;
          mt := -1;
          yr := -1;

          while buffer <> '' do
          begin
            Character := buffer[1];

            case Character of
              'D': dy := Number;
              'M': mn := Number;
              'Y': yr := Number;
              'H': hr := Number;
              'I': mt := Number;
              'S': sc := Number;
            end;

            FProcAdjust(sc, mn, hr, dy, mt, yr);
          end;
      end;
    except
      on E: Exception do
        ConsoleWindowWriteEx(FWindowHandle, 'Thread error: ' + E.Message, 1, 14, ffg, fbg);
    end;
  end;
end;

end.

