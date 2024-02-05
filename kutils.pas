unit kutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure ReadNullString(strm: TStream; var s: string);
function VmRSS: string;

implementation

procedure ReadNullString(strm: TStream; var s: string);
var
  b: byte;
begin
  s:='';
  repeat
    b:=strm.ReadByte;
    if b <> 0 then
      s:=s+chr(b);
  until b = 0;
end;

function VmRSS: string;
var
  f: TStringList;
begin
  f:=TStringList.Create;
  try
    f.NameValueSeparator:=':';
    f.LoadFromFile('/proc/'+IntToStr(GetProcessID)+'/status');
    Result:=TrimLeft(f.Values['VmRSS']);
  finally
    f.Free;
  end;
end;

end.

