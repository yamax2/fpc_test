unit PlayerOptions;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils;

type

  { TPlayerOptions }

  TPlayerLogOption = (ploExtractor, ploDB);
  TPlayerLogOptions = set of TPlayerLogOption;

  TPlayerOptions = class(TPersistent)
  private class var
    FOptions: TPlayerOptions;
  private
    FLogOptions: TPlayerLogOptions;
    FTempDir: String;
    procedure SetTempDir(AValue: String);
  public
    class constructor ClassCreate;
    class destructor ClassDestroy;

    class property Options: TPlayerOptions read FOptions;
  published
    property TempDir: String read FTempDir write SetTempDir;
    property LogOptions: TPlayerLogOptions
      read FLogOptions write FLogOptions default [];
  end;

  function opts: TPlayerOptions;

implementation

function opts: TPlayerOptions;
begin
  Result:=TPlayerOptions.Options;
end;

{ TPlayerOptions }

procedure TPlayerOptions.SetTempDir(AValue: String);
begin
  if FTempDir = AValue then Exit;
  FTempDir:=IncludeTrailingPathDelimiter(AValue);
  ForceDirectories(FTempDir);
end;

class constructor TPlayerOptions.ClassCreate;
begin
  inherited;
  FOptions:=TPlayerOptions.Create;
end;

class destructor TPlayerOptions.ClassDestroy;
begin
  FOptions.Free;
  inherited;
end;

end.
