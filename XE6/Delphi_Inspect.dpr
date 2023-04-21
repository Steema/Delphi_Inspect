program Delphi_Inspect;

uses
  System.StartUpCopy,
  FMX.MobilePreview,
  FMX.Forms,
  Unit_Info in 'Unit_Info.pas' {SystemInfoForm},
  Unit_Splash in 'Unit_Splash.pas' {FormSplash},
  InspectDelphi in '..\InspectDelphi.pas',
  Unit_Benchmark_Canvas in '..\Unit_Benchmark_Canvas.pas',
  Unit_Utils in '..\Unit_Utils.pas';

{$R *.res}

begin
  Application.Initialize;

  {$IFNDEF MSWINDOWS}
  Application.CreateForm(TFormSplash, FormSplash);
  {$ENDIF}

  Application.CreateForm(TSystemInfoForm, SystemInfoForm);
  Application.Run;
end.
