unit crthelper;

{$mode objfpc}{$H+}

interface

uses
  crt;

procedure RequestPassword(var p: string);

implementation

procedure RequestPassword(var p: string);
var
  c: char;
  done: boolean;
begin
  p:='';
  done:=False;
  repeat
    c:=ReadKey;
    if (c = #10) or (c = #13) then
      done:=True
    else
    begin
      p:=p+c;
      Write('?');
    end;
  until done;
  Write(#10#13);
end;

end.

