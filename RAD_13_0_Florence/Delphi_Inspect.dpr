program Delphi_Inspect;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Skia,
  FMX.Types,
  Unit_Info in 'Unit_Info.pas' {SystemInfoForm},
  InspectDelphi in '..\InspectDelphi.pas',
  Unit_Benchmark_Canvas in '..\Unit_Benchmark_Canvas.pas',
  Unit_Utils in '..\Unit_Utils.pas';

{$R *.res}

begin
  GlobalUseSkia := True;
  //GlobalUseGPUCanvas:=True;
  //GlobalUseHWEffects:=True;
  //GlobalUseDirect2D:=True;
  //GlobalUseGPUCanvas:=True;
  //GlobalUseMetal:=True;
  //GlobalUseDX:=True;

  Application.Initialize;
  Application.CreateForm(TSystemInfoForm, SystemInfoForm);
  Application.Run;
end.
