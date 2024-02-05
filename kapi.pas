unit kapi;

{$mode objfpc}{$H+}

interface

type
  PKApi = ^TKApi;
  TKApi = record
    op: byte;
    s1, s2: string[32];
    data: Array[0..32] of byte;
  end;

implementation

end.

