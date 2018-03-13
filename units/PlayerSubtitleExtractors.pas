unit PlayerSubtitleExtractors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PlayerThreads;

type
  TPlayerSubtitleExtractorClass = class of TPlayerSubtitleExtractor;

  { TPlayerSubtitleExtractor }

  TPlayerSubtitleExtractor = class // mp4 -> raw subtitle file
  private
    FFileName, FTempFileName: String;
  public
    constructor Create(const AFileName, ATempFileName: String); virtual;
    procedure Extract; virtual; abstract;

    property FileName: String read FFileName;
  end;

  { TPlayerSubtitleFfmpegExtractor }

  TPlayerSubtitleFfmpegExtractor = class(TPlayerSubtitleExtractor)
  public
    procedure Extract; override;
  end;

  { TPlayerTrackParser }

  TPlayerTrackParser = class  // raw subtitle -> nmea -> track
  private
    FFileName: String;
    FTempFileName: String;
    FThread: TPlayerThread;
  public
    constructor Create(const AFileName, ATempFileName: String;
      AThread: TPlayerThread); virtual;
    procedure Parse; virtual; abstract;

    property FileName: String read FFileName;
  end;

  { TPlayerNmeaTrackParser }

  TPlayerNmeaTrackParser = class(TPlayerTrackParser)
  private
    procedure LoadNmea;
  public
    procedure Parse; override;
  end;

implementation
uses
  Process, PlayerSessionStorage, dateutils, Math;

{ TPlayerNmeaTrackParser }

procedure TPlayerNmeaTrackParser.LoadNmea;
var
  CSV: TextFile;
  Line: String;

  Values: TStringArray;
  PrevPoint, Point: TPlayerPoint;

  deg, min: String;
begin
  AssignFile(CSV, FTempFileName);
  Reset(CSV);
  try
    while not Eof(CSV) and not FThread.Terminated do
    begin
      ReadLn(CSV, Line);
      Values:=Line.Split(',');
      if Length(Values) <> 13 then
        raise Exception.Create('incorrent nmea line');

      Point.time:=ScanDateTime('DDMMYY HHMMSS', Values[9] + ' ' + Values[1]);
      Point.speed:=RoundTo(Values[7].ToDouble * 1.852, -2);
      Point.course:=Values[8].ToDouble;

      deg:=Copy(Values[3], 1, 2);
      min:=Copy(Values[3], 3, Length(Values[3]));

      Point.lat:=RoundTo(deg.ToDouble + (min.ToDouble / 60), -6);
      if Values[4] = 'S' then Point.lat:=-Point.lat;

      deg:=Copy(Values[5], 1, 3);
      min:=Copy(Values[5], 4, Length(Values[5]));

      Point.lon:=RoundTo(deg.ToDouble + (min.ToDouble / 60), -6);
      if Values[6] = 'S' then Point.lon:=-Point.lon;

      Point.ptype:=Values[2];

      if Point <> PrevPoint then
        WriteLn('lat: ', FloatToStr(Point.lat), ' lon: ', FloatToStr(Point.lon),
         ' Time: ' + FormatDateTime(PLAYER_DATE_FORMAT, Point.time),
         ' Speed: ', FloatToStr(Point.speed), ' Course: ',
         FloatToStr(Point.course), ' ptype: ',
         Point.ptype);

      PrevPoint:=Point;
    end;
  finally
    CloseFile(CSV);
  end;
end;

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

    OutputStream:=TFileStream.Create(FTempFileName, fmCreate);
    try
      repeat
        Buffer[0]:=0;
        BytesRead:=Process.Output.Read(Buffer, BUF_SIZE);
        OutputStream.Write(Buffer, BytesRead);
      until BytesRead = 0;
    finally
      OutputStream.Free;
    end;

    LoadNmea;
  finally
    Process.Free;
  end;
end;

{ TPlayerTrackParser }

constructor TPlayerTrackParser.Create(const AFileName, ATempFileName: String;
  AThread: TPlayerThread);
begin
  FThread:=AThread;
  inherited Create;
  FFileName:=AFileName;
  FTempFileName:=ATempFileName;
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
      Parameters.Add(FTempFileName);

      Options:=[poWaitOnExit, poUsePipes];
    end;

    Process.Execute;
  finally
    Process.Free;
  end;
end;

{ TPlayerSubtitleExtractor }

constructor TPlayerSubtitleExtractor.Create(const AFileName,
  ATempFileName: String);
begin
  inherited Create;
  FFileName:=AFileName;
  FTempFileName:=ATempFileName;
end;

end.

