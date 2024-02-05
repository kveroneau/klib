unit qvfsjson;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, StrUtils, qvfs;

function QVFSToJSON(path: string): string;

implementation

function QVFSToJSON(path: string): string;
var
  json: TJSONObject;
  wh: string;
  f: PVFSFile;
begin
  json:=TJSONObject.Create;
  try
    wh:=Copy2SymbDel(path, ':');
    f:=GetVFSData(path, wh);
    if f = Nil then
      json.Add('error', 'File not found.')
    else
      with f^ do
      begin
        json.Add('fna', fna);
        json.Add('wh', wh);
        json.Add('typ', Ord(typ));
        json.Add('data', data);
      end;
    Result:=json.AsJSON;
  finally
    json.Free;
  end;
end;

end.

