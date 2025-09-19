unit Unit_Sensors;

interface

uses
  System.SysUtils, System.Classes, System.Sensors, IPPeerClient, IPPeerServer,
  System.Tether.Manager, System.Tether.AppProfile, System.Sensors.Components;

type
  TDataModule1 = class(TDataModule)
    OrientationSensor1: TOrientationSensor;
    MotionSensor1: TMotionSensor;
    TetheringManager2: TTetheringManager;
    TetheringManager1: TTetheringManager;
    TetheringAppProfile1: TTetheringAppProfile;
    LocationSensor1: TLocationSensor;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DataModule1: TDataModule1;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

end.
