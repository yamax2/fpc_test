program Test;
{$MODE OBJFPC}{$H+}
{$define debug}

uses
  PlayerThreads in 'units/PlayerThreads',
  PlayerExtractors in 'units/PlayerExtractors',
{$ifdef debug}
  HeapTrc,
{$endif}
  Process,
  Classes,
  SysUtils;

const
  BUF_SIZE = 16384;

type
  { TTestThread }

  TTestThread = class(TPlayerThread)
  private
    FNumber: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(AManager: TPlayerThreadManager; ANumber: Integer);
  end;

  { TTestThreadManager }

  TTestThreadManager = class(TPlayerThreadManager)
  private
    FCount: Integer;
  protected
    function GetNextThread: TPlayerThread; override;
  public
    constructor Create;
  end;

{ TTestThreadManager }

function TTestThreadManager.GetNextThread: TPlayerThread;
begin
  Inc(FCount);
  if FCount <= 9 then Result:=TTestThread.Create(Self, FCount) else Result:=nil;
end;

constructor TTestThreadManager.Create;
begin
  FCount:=0;
  inherited;
end;

{ TTestThread }

procedure TTestThread.Execute;
var
  AProcess: TProcess;
  Buffer: array[1..BUF_SIZE] of Byte;
  BytesRead: Longint;
  OutputStream: TFileStream;
begin
  AProcess:=TProcess.Create(nil);
  try
    with AProcess do
    begin
      Executable:='/bin/grep';
      Parameters.Add('\barchived\b');
      Parameters.Add('ads.csv');

      Options:=[poUsePipes];
    end;

    AProcess.Execute;
    OutputStream:=TFileStream.Create(Format('z%d.txt', [FNumber]), fmCreate);
    try
      repeat
        BytesRead:=AProcess.Output.Read(Buffer, BUF_SIZE);
        OutputStream.Write(Buffer, BytesRead);
      until Terminated or (BytesRead = 0);
    finally
      OutputStream.Free;
    end;
  finally
    AProcess.Free;
  end;
end;

constructor TTestThread.Create(AManager: TPlayerThreadManager; ANumber: Integer);
begin
 FNumber:=ANumber;
 inherited Create(AManager);
end;

begin
  TTestThreadManager.Create;
end.

