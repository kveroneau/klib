unit kbundle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TObjectType = (otUnknown, otBinary, otText);

  TBundle = class(TComponent)
  private
    Data: Array of Byte;
    FName: string;
    FSize: UInt32;
  end;

  { TKBundle }

  TKBundle = class(TComponent)
  private
    FBundles: Array of TBundle;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddBundle(AFile: string);
    procedure SaveToFile(AFile: string);
  end;

implementation

type
  THeader = record

  end;

const
  SIG = 'KBND*';

{ TKBundle }

constructor TKBundle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TKBundle.Destroy;
begin
  inherited Destroy;
end;

procedure TKBundle.AddBundle(AFile: string);
var
  ABundle: TBundle;
  b: integer;
begin
  ABundle:=TBundle.Create(Self);
  ABundle.FName:=AFile;
  with TFileStream.Create(AFile, fmOpenRead) do
  try
    ABundle.FSize:=Size;
    b:=Length(FBundles);
    SetLength(FBundles, b+1);
    Read(FBundles[b], Size);
  finally
    Free;
  end;
end;

procedure TKBundle.SaveToFile(AFile: string);
var
  f: TMemoryStream;
  i: integer;
begin
  f:=TMemoryStream.Create;
  try
    f.Write(SIG, Length(SIG));
    f.WriteByte(0);
    f.WriteWord(Length(FBundles));
    for i:=0 to Length(FBundles)-1 do
    begin
      f.WriteComponent(FBundles[i]);
    end;
    f.SaveToFile(AFile);
  finally
  end;
end;

end.

