program dpl;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  cmem,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,

  // player
  CpuCount,
  PlayerThreads,
  PlayerExtractors,
  PlayerSubtitleExtractors,
  dmxPlayer,
  PlayerOptions,
  PlayerLogger,
  PlayerGPX,
  PlayerSessionStorage,
  PlayerExporters,

  // forms
  fmxOptions,
  fmxMain,
  fmxProgress;

{$R *.res}

begin
  opts.TempDir:='./tmp';
  opts.LogOptions:=[ploExtractor, ploDB];

  Application.Title:='Dashcam Player Light';
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfmOptions, fmOptions);
  Application.Run;
end.

