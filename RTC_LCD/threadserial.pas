unit ThreadSerial;

{$mode objfpc}{$H+}

interface

uses
  Threads, Classes, SysUtils, Console, Platform, GlobalConfig, GlobalTypes;

type
  TProcAdjust = procedure (second, minute, hour, day, month, year: Integer);
  TFuncDTTemp = function(tp: Char): String;

  { TThreadSerial }
  TThreadSerial = class(TThread)
  private
    FProcAdjust: TProcAdjust;
    FFuncDTTemp: TFuncDTTemp;
    FWindowHandle: TWindowHandle;
    ffg, fbg: LongWord;
  public
    constructor Create(WindowHandle: TWindowHandle; ProcAdjust: TProcAdjust;
      FuncDTTemp: TFuncDTTemp; fg, bg: LongWord);
    procedure Execute; override;
  end;

implementation

{ TThreadSerial }

constructor TThreadSerial.Create(WindowHandle: TWindowHandle; ProcAdjust: TProcAdjust;
  FuncDTTemp: TFuncDTTemp; fg, bg: LongWord);
begin
  inherited Create(False, THREAD_STACK_DEFAULT_SIZE);

  FProcAdjust := ProcAdjust;
  FFuncDTTemp := FuncDTTemp;
  FWindowHandle := WindowHandle;
  ffg:=fg;
  fbg:=bg;
end;

procedure TThreadSerial.Execute;
var
  Count: LongWord;
  Character: char;
  buffer, buff2: string;
  sc, mn, hr, dy, mt, yr: Integer;
  MustAdjust: Boolean;

  function ReadChar: Char;
  begin
    if buffer = '' then
    begin
      Result := #0;
      Exit;
    end;

    Result := buffer[1];
    buffer := Copy(buffer, 2, Length(buffer) - 1);
  end;

  function FetchNumber: Integer;
  var
    c: Char;
    b: String;
  begin
    Result := -1;
    b := '';

    while buffer <> '' do
    begin
      c := buffer[1];

      if c in ['0'..'9'] then
      begin
        b := b + c;
        buffer := Copy(buffer, 2, Length(buffer) - 1);
      end
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
      begin
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
          MustAdjust := False;

          ConsoleWindowClearEx(FWindowHandle, 1, 16, 40, 16, False);

          while buffer <> '' do
          begin
            Character := ReadChar;

            case Character of
              '?':
                begin
                  Character := ReadChar;
                  buff2 := FFuncDTTemp(Character);

                  ConsoleWindowWriteEx(FWindowHandle, '?' + Character + ': ' + buff2, 1, 16, ffg, fbg);

                  if Length(buff2) > 0 then
                    SerialWrite(PChar(buff2), Length(buff2), Count)
                end;
              'D':
                begin
                  dy := FetchNumber;
                  MustAdjust := True;
                end;
              'M':
                begin
                  mt := FetchNumber;
                  MustAdjust := True;
                end;
              'Y':
                begin
                  yr := FetchNumber;
                  MustAdjust := True;
                end;
              'H':
                begin
                  hr := FetchNumber;
                  MustAdjust := True;
                end;
              'I':
                begin
                  mn := FetchNumber;
                  MustAdjust := True;
                end;
              'S':
                begin
                  sc := FetchNumber;
                  MustAdjust := True;
                end;
            end;

            if MustAdjust then
              FProcAdjust(sc, mn, hr, dy, mt, yr);
          end;

          ConsoleWindowClearEx(FWindowHandle, 1, 14, 40, 14, False);
        end;
      end;
    except
      on E: Exception do
        ConsoleWindowWriteEx(FWindowHandle, 'Thread error: ' + E.Message, 1, 14, ffg, fbg);
    end;
  end;
end;

end.

