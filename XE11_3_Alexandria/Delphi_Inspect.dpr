program Delphi_Inspect;

uses
  System.StartUpCopy,
  {$IFDEF SKIA}
  Skia.FMX,
  {$ENDIF}
  FMX.Forms,
  FMX.Types,
  Unit_Info in 'Unit_Info.pas' {SystemInfoForm},
  InspectDelphi in '..\InspectDelphi.pas',
  Unit_Benchmark_Canvas in '..\Unit_Benchmark_Canvas.pas',
  Unit_Utils in '..\Unit_Utils.pas';

{$R *.res}

begin
  {$IFDEF SKIA}
  GlobalUseSkia := True;
  {$ENDIF}
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
