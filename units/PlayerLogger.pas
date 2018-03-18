unit PlayerLogger;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils;

type

  { TPlayerLogger }

  TPlayerLogger = class
  private class var
    FCriticalSection: TRTLCriticalSection;
    FFile: TextFile;
    FPrepared: Boolean;
  private
    class procedure Prepare;
  public
    class constructor ClassCreate;
    class destructor ClassDestroy;

    class procedure Log(const AMsg: String); overload;
    class procedure Log(const AMsg: String; const Values: array of const);
      overload;
  end;

  logger = TPlayerLogger;

implementation
uses
  PlayerSessionStorage, PlayerOptions;

{ TPlayerLogger }

class procedure TPlayerLogger.Prepare;
var
  LogFile: String;
begin
  LogFile:=opts.TempDir + 'player.log';
  AssignFile(FFile, LogFile);
  if FileExists(LogFile) then Append(FFile) else Rewrite(FFile);
  FPrepared:=True;
end;

class constructor TPlayerLogger.ClassCreate;
begin
  inherited;
  FPrepared:=False;
  InitCriticalSection(FCriticalSection);
end;

class destructor TPlayerLogger.ClassDestroy;
begin
  DoneCriticalsection(FCriticalSection);
  CloseFile(FFile);
  inherited;
end;

class procedure TPlayerLogger.Log(const AMsg: String);
begin
  EnterCriticalsection(FCriticalSection);
  try
    if not FPrepared then Prepare;
    WriteLn(FFile, '[', FormatDateTime(PLAYER_DATE_FORMAT, Now), ']: ',
      AMsg);
  finally
    LeaveCriticalsection(FCriticalSection);
  end;
end;

class procedure TPlayerLogger.Log(const AMsg: String;
  const Values: array of const);
begin
  Log(Format(AMsg, Values));
end;

end.
