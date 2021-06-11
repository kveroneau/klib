unit piservice;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sockets;

procedure PiDisplayOff;
procedure PiDisplayOn;
procedure PiBrightness(value: integer);
function PiGetBrightness: integer;

implementation

const
  {$IFDEF CPUARM}
  API_BRIGHT = '/sys/class/backlight/rpi_backlight/brightness';
  {$ELSE}
  API_BRIGHT = '/sys/class/backlight/intel_backlight/brightness';
  {$ENDIF}

procedure sendOp(op, param: integer);
var
  s: LongInt;
  addr, addr2: TUnixSockAddr;
  addrLen, addrLen2: LongInt;
  d: Array[0..1] of Char;
begin
  Str2UnixSockAddr('', addr, addrLen);
  Str2UnixSockAddr('/tmp/pi.sock', addr2, addrLen2);
  s:=fpsocket(AF_UNIX, SOCK_DGRAM, 0);
  fpbind(s, @addr, addrLen);
  d[0]:=chr(op);
  d[1]:=chr(param);
  fpsendto(s, @d[0], 2, 0, @addr2, addrLen2);
  CloseSocket(s);
end;

procedure display(toggle: Boolean);
begin
  if toggle then
    sendOp(1,0)
  else
    sendOp(1,1);
end;

procedure PiDisplayOff;
begin
  display(False);
end;

procedure PiDisplayOn;
begin
  display(True);
end;

procedure PiBrightness(value: integer);
begin
  sendOp(2, value);
end;

function PiGetBrightness: integer;
var
  f: Text;
  d: string[20];
begin
  {$IFDEF CPUARM}
  system.Assign(f, API_BRIGHT);
  Reset(f);
  Read(f,d);
  Close(f);
  Result:=StrToInt(d);
  {$ELSE}
  Result:=50;
  {$ENDIF}
end;

end.

