unit PlayerSubtitleExtractors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TSubtitleExtractorType = (seFFmpeg, seMP4Box);
  TPlayerSubtitleExtractorClass = class of TPlayerSubtitleExtractor;

  { TPlayerSubtitleExtractor }

  TPlayerSubtitleExtractor = class
  private
    FFileName, FOutputFileName: String;
  public
    constructor Create(const AFileName, AOutputFileName: String); virtual;
    procedure Extract; virtual; abstract;

    property FileName: String read FFileName;
    property OutputFileName: String read FOutputFileName;
  end;

  { TPlayerSubtitleFfmpegExtractor }

  TPlayerSubtitleFfmpegExtractor = class(TPlayerSubtitleExtractor)
  public
    procedure Extract; override;
  end;

  { TPlayerSubtitleMP4BoxExtractor }

  TPlayerSubtitleMP4BoxExtractor = class(TPlayerSubtitleExtractor)
  public
    procedure Extract; override;
  end;

  { TPlayerTrackParser }

  TPlayerTrackParser = class
  private
    FFileName: String;
    FOutputFileName: String;
  public
    constructor Create(const AFileName, AOutputFileName: String); virtual;
    procedure Parse; virtual; abstract;

    property FileName: String read FFileName;
    property OutputFileName: String read FOutputFileName;
  end;

  { TPlayerNmeaTrackParser }

  TPlayerNmeaTrackParser = class(TPlayerTrackParser)
  public
    procedure Parse; override;
  end;

implementation
uses
  Process;

{ TPlayerNmeaTrackParser }

procedure TPlayerNmeaTrackParser.Parse;
const
  BUF_SIZE = 16384;
var
  Process: TProcess;
  OutputStream: TFileStream;
  Buffer: array[0..BUF_SIZE - 1] of Byte;
  BytesRead: LongInt;
begin
  Process:=TProcess.Create(nil);
  try
    with Process do
    begin
      // grep -aoE $'\$G[A-Z]+RMC[A-Z\.,*0-9]+\n' samples/trendvision.data
      Executable:='/bin/grep';

      Parameters.Add('-aoE');
      Parameters.Add('\$G[A-Z]+RMC[A-Z\.,*0-9]+');
      Parameters.Add(FileName);

      Options:=[poUsePipes];
    end;

    Process.Execute;

    OutputStream:=TFileStream.Create(OutputFileName, fmCreate);
    try
      repeat
        Buffer[0]:=0;
        BytesRead:=Process.Output.Read(Buffer, BUF_SIZE);
        OutputStream.Write(Buffer, BytesRead);
      until BytesRead = 0;
    finally
      OutputStream.Free;
    end;
  finally
    Process.Free;
  end;
end;

{ TPlayerTrackParser }

constructor TPlayerTrackParser.Create(const AFileName, AOutputFileName: String);
begin
  inherited Create;
  FFileName:=AFileName;
  FOutputFileName:=AOutputFileName;
end;

{ TPlayerSubtitleMP4BoxExtractor }

procedure TPlayerSubtitleMP4BoxExtractor.Extract;
var
  Process: TProcess;
begin
  Process:=TProcess.Create(nil);
  try
    with Process do
    begin
      // ./MP4Box -quiet -raw 3 /win/large2/Видео/2018_01a/16/01051451_0001.MP4 -out 1.txt
      Executable:='/projects/gpac/bin/gcc/MP4Box';

      Parameters.Add('-quiet');
      Parameters.Add('-raw');
      Parameters.Add('3');
      Parameters.Add(FileName);
      Parameters.Add('-out');
      Parameters.Add(OutputFileName);

      Options:=[poWaitOnExit, poUsePipes];
    end;

    Process.Execute;
  finally
    Process.Free;
  end;
end;

{ TPlayerSubtitleFfmpegExtractor }

procedure TPlayerSubtitleFfmpegExtractor.Extract;
var
  Process: TProcess;
begin
  Process:=TProcess.Create(nil);
  try
    with Process do
    begin
      // ffmpeg -f concat -i mylist.txt -map 0:s:0 -c copy -f data out.data
      Executable:='/usr/bin/ffmpeg';

      Parameters.Add('-i');
      Parameters.Add(FileName);
      Parameters.Add('-map');
      Parameters.Add('0:s:0');
      Parameters.Add('-c');
      Parameters.Add('copy');
      Parameters.Add('-f');
      Parameters.Add('data');
      Parameters.Add('-v');
      Parameters.Add('quiet');
      Parameters.Add(OutputFileName);

      Options:=[poWaitOnExit, poUsePipes];
    end;

    Process.Execute;
  finally
    Process.Free;
  end;
end;

{ TPlayerSubtitleExtractor }

constructor TPlayerSubtitleExtractor.Create(const AFileName,
  AOutputFileName: String);
begin
  inherited Create;
  FFileName:=AFileName;
  FOutputFileName:=AOutputFileName;
end;

end.

