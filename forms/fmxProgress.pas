unit fmxProgress;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, Buttons, PlayerThreads;

type

  { TfmProgress }

  TfmProgress = class(TForm)
    btStop: TBitBtn;
    lbInfo: TLabel;
    ProgressBar: TProgressBar;
    procedure btStopClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    FManager: TPlayerThreadManager;
    FTrackCount: Integer;
    FFinished: Boolean;
    procedure SetTrackCount(AValue: Integer);
  public
    constructor Create(AOwner: TComponent); override;

    procedure Processed(Sender: TObject; const AProcessedCount: Integer);
    procedure ProcessFinished(Sender: TObject);

    property Manager: TPlayerThreadManager read FManager write FManager;
    property TrackCount: Integer read FTrackCount write SetTrackCount;
  end;

implementation

{$R *.lfm}

{ TfmProgress }

procedure TfmProgress.btStopClick(Sender: TObject);
begin
  FManager.Interrupt(True);
end;

procedure TfmProgress.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if not FFinished then CloseAction:=caNone else CloseAction:=caFree;
end;

procedure TfmProgress.SetTrackCount(AValue: Integer);
begin
  if FTrackCount = AValue then Exit;

  FTrackCount:=AValue;
  lbInfo.Caption:=Format('processed files: 0/%d', [TrackCount]);
end;

constructor TfmProgress.Create(AOwner: TComponent);
begin
  inherited;
  FFinished:=False;
end;

procedure TfmProgress.Processed(Sender: TObject; const AProcessedCount: Integer
  );
begin
  ProgressBar.Position:=Round(100 * AProcessedCount / TrackCount);
  lbInfo.Caption:=Format('processed files: %d/%d', [AProcessedCount, TrackCount]);
end;

procedure TfmProgress.ProcessFinished(Sender: TObject);
begin
  FFinished:=True;
  Close;
end;

end.

