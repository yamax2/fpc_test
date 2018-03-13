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
  PlayerLogger in 'units/PlayerLogger',
  PlayerSessionStorage in 'units/PlayerSessionStorage',
  Classes, SysUtils, FileUtil, Process, crc;

const
  VIDEO_DIR = '/home/max/Загрузки/1'; //'/win/video/2018_01a/zz01-02-01-2018/01';
    //'/win/video/2018_01a/zz01-02-01-2018/04'; //'/win/video/2018_01a/16';
    //'/win/video/2018_01a/zz14-04-01-2018';

var
  Extractor: TPlayerInfoExtractor;
  Files: TStringList;

  Index: Integer;
begin
  Files:=TStringList.Create;
  try
    FindAllFiles(Files, VIDEO_DIR, '*.mp4;*.MP4', True);
    Extractor:=TPlayerInfoExtractor.Create(Files, './tmp');
    try
      Extractor.LoadData.WaitFor;

      for Index:=0 to Extractor.Count - 1 do
        WriteLn(Extractor[Index], ': ', Extractor.FileInfo[Index].TrackFile, ', ', Extractor.FileInfo[Index].Size);
    finally
      Extractor.Free;
    end;
  finally
    Files.Free;
  end;
end.

