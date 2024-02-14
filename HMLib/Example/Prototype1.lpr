program Prototype1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, proto1main, VMWindow
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='Prototype HackerMaker';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TPrototypeMainForm, PrototypeMainForm);
  Application.Run;
end.

