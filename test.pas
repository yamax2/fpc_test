program Test;
{$MODE OBJFPC}{$H+}
{$define debug}

uses
{$ifdef unix}
  cthreads,
  cmem,
{$endif}
{$ifdef debug}
  HeapTrc,
{$endif}
  PlayerThreads in 'units/PlayerThreads',
  PlayerExtractors in 'units/PlayerExtractors',
  Classes, SysUtils, FileUtil;

const
  VIDEO_DIR = '/win/video/2018_01a/16'; //'/win/video/2018_01a/zz14-04-01-2018';

var
  Extractor: TPlayerInfoExtractor;
  Files: TStringList;
begin
  Files:=TStringList.Create;
  try
    FindAllFiles(Files, VIDEO_DIR, '*.mp4;*.MP4', True);
    Extractor:=TPlayerInfoExtractor.Create(Files, './tmp');
    try
      Extractor.LoadData.WaitFor;
    finally
      Extractor.Free;
    end;
  finally
    Files.Free;
  end;
end.

