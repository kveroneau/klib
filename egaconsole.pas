unit EGAConsole;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  PEGACell = ^TEGACell;
  TEGACell = record
    c: char;
    a: byte;
  end;

  PEGAScreen = ^TEGAScreen;
  TEGAScreen = Array[0..2000] of TEGACell;

  TEGAPalette = Array[0..15] of TColor;

  { TCursor }

  TCursor = class(TObject)
  private
    FForm: TForm;
    FFrames: Array of Char;
    FAnimated: Boolean;
    FRate, FCFrame: Integer;
    Ffg, Fbg: TColor;
  public
    constructor Create(form: TForm; fg, bg: TColor; frames: Array of Char);
    destructor Destroy; override;
    procedure Draw(Canvas: TCanvas; x, y: integer);
  end;

  { TEGAConsole }

  TEGAConsole = class(TImage)
  private
    FEGABuf: PEGAScreen;
    FPal: TEGAPalette;
    FForeground, FBackground: integer;
    FPos: Array[0..1] of byte;
    FWinSize: Array[0..1] of byte;
    FWrap: Array[0..1] of byte;
    FAssigned: boolean;
    FMouseX, FMouseY: byte;
    FKCursor: TCursor;
    FMCursor: TCursor;
    procedure EGAConsolePaint(Sender: TObject);
    procedure EGAConsole1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure LoadData;
    procedure Flatten;
    procedure SetBackground(AValue: Integer);
    procedure SetForeground(AValue: Integer);
  protected

  public
    property EGABuffer: PEGAScreen read FEGABuf write FEGABuf;
    property MouseX: byte read FMouseX;
    property MouseY: byte read FMouseY;
    property KCursor: TCursor write FKCursor;
    property MCursor: TCursor write FMCursor;
    property Palette: TEGAPalette read FPal;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AssignBuffer(buf: PEGAScreen);
    procedure Bload(const fname: string);
    procedure Bsave(const fname: string);
    procedure SetXY(const row, col: integer; const c: char; const fg, bg: integer);
    procedure SetXY(const row, col: integer; const c: char);
    procedure Write(const s: string);
    procedure SetColor(const fg, bg: integer);
    procedure ClearLine(const row: integer);
    procedure ScrollConsole;
    procedure ClrScr;
    procedure FormKeyPress(Sender: TObject; var Key: char);
  published
    property Foreground: Integer read FForeground write SetForeground;
    property Background: Integer read FBackground write SetBackground;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I egaconsole_icon.lrs}
  RegisterComponents('klib',[TEGAConsole]);
end;

{ TCursor }

constructor TCursor.Create(form: TForm; fg, bg: TColor; frames: array of Char);
var
  i: integer;
begin
  FForm:=form;
  FCFrame:=0;
  FRate:=3;
  Ffg:=fg;
  Fbg:=bg;
  SetLength(FFrames, Length(frames));
  for i:=0 to Length(frames)-1 do
    FFrames[i]:=frames[i];
  if Length(frames) > 1 then
    FAnimated:=True;
end;

destructor TCursor.Destroy;
begin
  if FAnimated then
    WriteLn(Length(FFrames));
  inherited Destroy;
end;

procedure TCursor.Draw(Canvas: TCanvas; x, y: integer);
var
  frame: integer;
begin
  if FAnimated then
  begin
    frame:=FCFrame div FRate mod Length(FFrames);
    Inc(FCFrame);
  end
  else
    frame:=0;
  Canvas.Brush.Color:=Fbg;
  Canvas.Font.Color:=Ffg;
  Canvas.TextOut(x, y, FFrames[frame]);
end;

{ TEGAConsole }

procedure TEGAConsole.EGAConsolePaint(Sender: TObject);
var
  x,y,fg,bg: integer;
  cell: PEGACell;
begin
  Canvas.Font.Name:='Monospace';
  Canvas.Font.Height:=14;
  Canvas.Brush.Color:=FPal[FBackground];
  Canvas.Font.Color:=FPal[FForeground];
  {Canvas.FloodFill(0,0, FPal[FBackground], fsSurface);}
  Canvas.FillRect(0,0,Canvas.Width,Canvas.Height);
  cell:=@FEGABuf^[0];
  for y := 0 to 24 do
    for x := 0 to 79 do
    begin
      if cell^.a > 0 then
      begin
        fg:=cell^.a and $f;
        bg:=(cell^.a and $f0) shr 4;
        Canvas.Brush.Color:=FPal[bg];
        Canvas.Font.Color:=FPal[fg];
      end;
      if ord(cell^.c) > 0 then
        Canvas.TextOut(x*8,y*16, cell^.c);
      Inc(cell);
    end;
  if Assigned(FMCursor) then
  begin
    Cursor:=crNone;
    FMCursor.Draw(Canvas, FMouseX*8, FMouseY*16);
  end;
  if Assigned(FKCursor) then
    FKCursor.Draw(Canvas, FPos[1]*8, FPos[0]*16);
end;

procedure TEGAConsole.EGAConsole1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  ox, oy: byte;
begin
  ox:=FMouseX;
  oy:=FMouseY;
  FMouseX:=X div 8;
  FMouseY:=Y div 16;
  if (ox <> FMouseX) or (oy <> FMouseY) then
    if Assigned(FMCursor) then
      Invalidate;
end;

procedure TEGAConsole.LoadData;
var
  s: TMemoryStream;
  i: integer;
  r,g,b: byte;
begin
  s:=TMemoryStream.Create;
  try
    s.LoadFromFile('VGA.bin');
    for i:=0 to 15 do
    begin
      r:=s.ReadByte;
      g:=s.ReadByte;
      b:=s.ReadByte;
      FPal[i]:=RGBToColor(r, g, b);
    end;
  finally
    s.Free;
  end;
end;

procedure TEGAConsole.Flatten;
var
  cell: PEGACell;
  i: integer;
begin
  cell:=@FEGABuf^[0];
  for i:=0 to High(FEGABuf^) do
    if cell^.a = 0 then
      cell^.a:=FForeground or (FBackground << 4);
end;

procedure TEGAConsole.SetBackground(AValue: Integer);
begin
  if FBackground=AValue then Exit;
  FBackground:=AValue;
  Invalidate;
end;

procedure TEGAConsole.SetForeground(AValue: Integer);
begin
  if FForeground=AValue then Exit;
  FForeground:=AValue;
  Invalidate;
end;

constructor TEGAConsole.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Height:=400;
  Width:=640;
  FForeground:=15;
  FBackground:=1;
  Canvas.Font.Name:='Monospace';
  Canvas.Font.Height:=14;
  Canvas.FloodFill(0,0,FPal[FBackground], fsSurface);
  Canvas.Brush.Color:=FPal[FBackground];
  Canvas.Font.Color:=FPal[FForeground];
  if not (csDesigning in ComponentState) then
    LoadData;
  New(FEGABuf);
  FAssigned:=False;
  OnPaint:=@EGAConsolePaint;
  OnMouseMove:=@EGAConsole1MouseMove;
  FPos[0]:=0;
  FPos[1]:=0;
  FWinSize[0]:=25;
  FWinSize[1]:=80;
  FWrap[0]:=0;
  FWrap[1]:=0;
end;

destructor TEGAConsole.Destroy;
begin
  if not FAssigned then
    Dispose(FEGABuf);
  inherited Destroy;
end;

procedure TEGAConsole.AssignBuffer(buf: PEGAScreen);
begin
  if not FAssigned then
    Dispose(FEGABuf);
  FEGABuf:=buf;
  FAssigned:=True;
end;

procedure TEGAConsole.Bload(const fname: string);
var
  s: TMemoryStream;
begin
  s:=TMemoryStream.Create;
  try
    s.LoadFromFile(fname);
    if s.ReadByte <> $fd then
      Exit;
    if s.ReadWord <> $b800 then
      Exit;
    s.ReadWord;
    s.Read(FEGABuf^, s.ReadWord);
  finally
    s.Free;
  end;
end;

procedure TEGAConsole.Bsave(const fname: string);
var
  s: TMemoryStream;
begin
  Flatten;
  s:=TMemoryStream.Create;
  try
    s.WriteByte($fd);
    s.WriteWord($b800);
    s.WriteWord($0000);
    s.WriteWord($0fa0);
    s.Write(FEGABuf^[0], 4000);
    s.SaveToFile(fname);
  finally
    s.Free;
  end;
end;

procedure TEGAConsole.SetXY(const row, col: integer; const c: char; const fg,
  bg: integer);
var
  cell: PEGACell;
begin
  if (row > 24) or (col > 79) then
    Exit;
  cell:=@FEGABuf^[(80*row+col)];
  cell^.c:=c;
  cell^.a:=fg or (bg << 4)
end;

procedure TEGAConsole.SetXY(const row, col: integer; const c: char);
begin
  SetXY(row, col, c, FForeground, FBackground);
end;

procedure TEGAConsole.Write(const s: string);
var
  i: integer;
begin
  for i:=1 to Length(s) do
  begin
    if s[i] = #10 then
    begin
      FPos[1]:=FWrap[0];
      Inc(FPos[0]);
    end
    else if s[i] = #9 then
      Inc(FPos[1], 8)
    else
    begin
      SetXY(FPos[0], FPos[1], s[i]);
      Inc(FPos[1]);
    end;
    if FPos[1] > (FWinSize[1]+FWrap[1]) then
    begin
      FPos[1]:=FWrap[1];
      Inc(FPos[0]);
    end;
  end;
end;

procedure TEGAConsole.SetColor(const fg, bg: integer);
begin
  FForeground:=fg;
  FBackground:=bg;
end;

procedure TEGAConsole.ClearLine(const row: integer);
begin
  FillByte(FEGABuf^[80*row*2], 80*2, 0);
end;

procedure TEGAConsole.ScrollConsole;
begin
  Move(FEGABuf^[80], FEGABuf^[0], 80*23*2);
  ClearLine(24);
end;

procedure TEGAConsole.ClrScr;
begin
  FillByte(FEGABuf^[0], SizeOf(FEGABuf^), 0);
  FPos[0]:=0;
  FPos[1]:=0;
end;

procedure TEGAConsole.FormKeyPress(Sender: TObject; var Key: char);
begin
  { This should only be used for testing purposes. }
  { A handler should exist in the application to take care of input. }
  if Key = #13 then
  begin
    Inc(FPos[0]);
    FPos[1]:=0;
  end
  else if Key = #8 then
  begin
    Dec(FPos[1]);
    Write(' ');
    Dec(FPos[1]);
  end
  else
    Write(Key);
  Invalidate;
end;

end.
