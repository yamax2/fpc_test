program Test;
{$MODE OBJFPC}{$H+}

uses
{$ifdef unix}
  cthreads,
  cmem,
{$endif}
  CpuCount in 'units/CpuCount',
  PlayerThreads in 'units/PlayerThreads',
  PlayerExtractors in 'units/PlayerExtractors',
  PlayerSubtitleExtractors in 'units/PlayerSubtitleExtractors',
  dmxPlayer in 'units/dmxPlayer',
  PlayerOptions in 'units/PlayerOptions',
  PlayerLogger in 'units/PlayerLogger',
  PlayerSessionStorage in 'units/PlayerSessionStorage',
  Classes, SysUtils, FileUtil, Process, crc;

const
  VIDEO_DIR = // '/win/c/downloads/Ночь (прошивка 1027)';
    '/win/video/2018_01a/zz01-02-01-2018';

var
  Extractor: TPlayerInfoExtractor;
  Files: TStringList;
begin
  opts.TempDir:='./tmp';

  Files:=TStringList.Create;
  try
    FindAllFiles(Files, VIDEO_DIR, '*.mp4;*.MP4', True);
    // Files.Add('/win/c/downloads/Ночь (прошивка 1027)/12101625_0018.MP4');
    Extractor:=TPlayerInfoExtractor.Create(Files);
    try
      if not Extractor.Loaded then
        Extractor.LoadData.WaitFor;

      WriteLn(Extractor.SessionID);
    finally
      Extractor.Free;
    end;
  finally
    Files.Free;
  end;
end.

