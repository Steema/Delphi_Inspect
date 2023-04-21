program Delphi_Globals_icons_3d;

uses
  FMX.Forms,
  Unit8 in 'Unit8.pas' {Form8},
  Main in 'Main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
