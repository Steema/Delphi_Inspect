unit Unit_Splash;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects;

type
  TFormSplash = class(TForm)
    Image1: TImage;
    Timer1: TTimer;
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormSplash: TFormSplash;

implementation

{$R *.fmx}

uses Unit_Info;

procedure TFormSplash.FormShow(Sender: TObject);
begin
  Timer1.Enabled:=True;
end;

procedure TFormSplash.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled:=False;
  Image1.Visible:=False;

  SystemInfoForm.ShowModal(procedure(AResult:TModalResult)
    begin
      FormSplash.Close;
      Application.Terminate;
    end);
end;

end.
