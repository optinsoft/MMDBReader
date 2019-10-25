program MMDBReaderSample;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  IPTypesX in '..\Source\IPTypesX.pas',
  uMMDBInfo in '..\Source\uMMDBInfo.pas',
  uMMDBIPAddress in '..\Source\uMMDBIPAddress.pas',
  uMMDBReader in '..\Source\uMMDBReader.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
