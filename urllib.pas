unit urllib;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient;

function URLGet(const url: string): string;

implementation

function URLGet(const url: string): string;
begin
  Result:='';
  with TFPHTTPClient.Create(Nil) do
    try
      Result:=Get(url);
    finally
      Free;
    end;
end;

end.

