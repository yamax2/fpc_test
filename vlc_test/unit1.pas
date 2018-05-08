unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Unix, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, PasLibVlcPlayerUnit;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    player: TPasLibVlcPlayer;
    ProgBar: TTrackBar;
    procedure Button1Click(Sender: TObject);
    procedure playerMediaPlayerLengthChanged(Sender: TObject; time: Int64);
    procedure playerMediaPlayerTimeChanged(Sender: TObject; time: Int64);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  Caption:='/win/video/2018_01 - Тотьма/zz01-02-01-2018/01/01021430_0006.MP4';
  player.Play(WideString(Caption));
  player.SetPlayRate(300);
end;

procedure TForm1.playerMediaPlayerLengthChanged(Sender: TObject; time: Int64);
begin
 ProgBar.Max := time; //player.GetVideoLenInMs();
end;

procedure TForm1.playerMediaPlayerTimeChanged(Sender: TObject; time: Int64);
begin
  ProgBar.Position := time; //player.GetVideoPosInMs();
end;

end.

