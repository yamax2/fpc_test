unit PlayerGPX;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils;

type

  { TPlayerGPX }

  TPlayerGPX = class
  private
    FSessionId: String;
    FTripId: Integer;
    FFile: TextFile;
    FLines: TStringList;
    procedure AddGpxPoint;
    procedure AppendLines;
    function FormatGpxTime(const AValue: String): String;
  public
    constructor Create(const ASession: String;
      const ATripID: Integer);
    destructor Destroy; override;

    procedure Convert;
  end;

implementation
uses
  PlayerOptions, dmxPlayer, PlayerLogger, PlayerSessionStorage, DateUtils;

{ TPlayerGPX }

procedure TPlayerGPX.AddGpxPoint;
begin
  with dmPlayer.Query do
    FLines.Add(Format('    <trkpt lat="%.6n" lon="%.6n"><time>%s</time><course>%n</course><speed>%n</speed></trkpt>',
      [
        FieldByName('lat').AsFloat, FieldByName('lon').AsFloat,
        FormatGpxTime(FieldByName('time').AsString),
        FieldByName('course').AsFloat,
        FieldByName('speed').AsFloat
      ]));
end;

procedure TPlayerGPX.AppendLines;
begin
  if FLines.Count = 0 then Exit;

  Write(FFile, FLines.Text);
  FLines.Clear;
end;

function TPlayerGPX.FormatGpxTime(const AValue: String): String;
var
  dt: TDateTime;
begin
  dt:=ScanDateTime(PLAYER_DATE_FORMAT, AValue);
  Result:=FormatDateTime(PLAYER_DATE_FORMAT, dt).Replace(' ', 'T') + 'Z';
end;

constructor TPlayerGPX.Create(const ASession: String; const ATripID: Integer);
begin
  inherited Create;
  FLines:=TStringList.Create;
  FSessionId:=ASession;
  FTripID:=ATripID;

  AssignFile(FFile, IncludeTrailingPathDelimiter(opts.TempDir + FSessionId) +
    Format('trip_%d.gpx', [FTripId]));
  Rewrite(FFile);
end;

destructor TPlayerGPX.Destroy;
begin
  CloseFile(FFile);
  FLines.Free;
  inherited;
end;

procedure TPlayerGPX.Convert;
begin
  logger.Log('creating gpx for trip %d, session %s', [FTripId, FSessionId]);

  with dmPlayer do
  begin
    FLines.Add('<?xml version="1.0" encoding="UTF-8" ?>');
    FLines.Add('<gpx version="1.0"');
    FLines.Add('     creator="Dashcam Player Light"');
    FLines.Add('     xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns="http://www.topografix.com/GPX/1/0"');
    FLines.Add('     xsi:schemaLocation="http://www.topografix.com/GPX/1/0 http://www.topografix.com/GPX/1/0/gpx.xsd">');
    FLines.Add('<time>2018-01-05T05:00:09Z</time>');
    FLines.Add('<trk>');
    FLines.Add('  <name>Track 2018.01.05 10-00-09</name>');
    FLines.Add('  <desc>Duration: 1:14:15. Length: 77195m. AvgSpeed: 62,4km/h.</desc>');
    FLines.Add('  <time>2018-01-05T05:00:09Z</time>');
    FLines.Add('  <trkseg>');
    AppendLines;

    Query.SQL.Clear;
    LoadTextFromResource(Query.SQL, 'GPX');
    Query.ParamByName('trip_id').AsInteger:=FTripId;
    Query.Open;
    try
      while not Query.EOF do
      begin
        logger.Log('saving point %d, trip %d, session %s',
          [Query.FieldByName('id').AsInteger, FTripId, FSessionId]);

        AddGpxPoint;
        Query.Next;
      end;
    finally
      Query.Close;
    end;

    FLines.Add('  </trkseg>');
    FLines.Add('</trk>');
    FLines.Add('</gpx>');
    AppendLines;
  end;
end;

end.
