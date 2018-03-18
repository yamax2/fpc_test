unit fmxMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs;

type

  { TfmMain }

  TfmMain = class(TForm)
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    FSessionID: String;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property SessionID: String read FSessionID write FSessionID;
  end;

var
  fmMain: TfmMain;

implementation
uses
  fmxOptions;

{$R *.lfm}

{ TfmMain }

procedure TfmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:=caFree;
end;

constructor TfmMain.Create(AOwner: TComponent);
begin
  inherited;
  Caption:=Application.Title;
end;

destructor TfmMain.Destroy;
begin
  fmOptions.Show;
  inherited;
end;

end.

