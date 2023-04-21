unit Unit8;

interface

uses
  System.SysUtils, System.Types, System.Variants, System.UITypes,
  System.Classes, FMX.Types, FMX.Graphics, FMX.Dialogs, FMX.Types3D, FMX.Forms,
  FMX.Forms3D, FMX.Controls3D, System.Math.Vectors, FMX.MaterialSources,
  FMX.Objects3D;

type
  TForm8 = class(TForm3D)
    Light1: TLight;
    LightMaterialSource1: TLightMaterialSource;
    Light2: TLight;
    Text3D2: TText3D;
    LightMaterialSource2: TLightMaterialSource;
    LightMaterialSource3: TLightMaterialSource;
    LightMaterialSource4: TLightMaterialSource;
    LightMaterialSource5: TLightMaterialSource;
    Text3D1: TText3D;
    procedure Form3DCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

procedure TForm8.Form3DCreate(Sender: TObject);
var c : TColor;
begin
{
  c:=TAlphaColorRec.White;
  TAlphaColorRec(C).A:=0;
  Color:=C;
}
end;

end.
