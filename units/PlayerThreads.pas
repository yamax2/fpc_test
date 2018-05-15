unit PlayerThreads;

{$mode objfpc}{$H+}

interface

uses
{$ifdef unix}
  cthreads,
  cmem,
{$endif}
  Classes, SysUtils, CpuCount;

type
  TPlayerThread = class;

  TCustomPlayerThread = class(TThread)
  public
    property Terminated;
  end;

  { TPlayerThreadManager }

  TPlayerThreadManager = class(TCustomPlayerThread)
  private
    FEvent, FStopEvent: pRTLEvent;
    FList: TThreadList;
    FForceTerminated: Boolean;
    FMaxThreadCount: Integer;
  protected
    procedure Execute; override;
    function GetNextThread: TPlayerThread; virtual;
    function GetMaxThreadCount: Integer; virtual;
    procedure Process(AFinishedThread: TPlayerThread = nil);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Interrupt(const Force: Boolean = False); virtual;

    property ThreadList: TThreadList read FList;
    property MaxThreadCount: Integer read FMaxThreadCount;
  end;

  { TPlayerThread }

  TPlayerThread = class(TCustomPlayerThread)
  private
    FManager: TPlayerThreadManager;
  public
    constructor Create(AManager: TPlayerThreadManager);
    destructor Destroy; override;

    property Manager: TPlayerThreadManager read FManager;
  end;


implementation

{ TPlayerThread }

constructor TPlayerThread.Create(AManager: TPlayerThreadManager);
begin
  FreeOnTerminate:=True;
  FManager:=AManager;
  inherited Create(True);
end;

destructor TPlayerThread.Destroy;
begin
  FManager.Process(Self);
  inherited;
end;

{ TPlayerThreadManager }

procedure TPlayerThreadManager.Execute;
var
  List: TList;
  Index: Integer;
begin
  Process;
  RtlEventWaitFor(FEvent);
  Terminate;

  List:=FList.LockList;
  try
    if List.Count = 0 then Exit;

    if FForceTerminated then
      for Index:=0 to List.Count - 1 do
        TThread(List[Index]).Terminate;
  finally
    FList.UnlockList;
  end;

  RTLeventWaitFor(FStopEvent);
end;

function TPlayerThreadManager.GetNextThread: TPlayerThread;
begin
  Result:=nil;
end;

function TPlayerThreadManager.GetMaxThreadCount: Integer;
begin
  Result:=GetLogicalCpuCount;
end;

procedure TPlayerThreadManager.Process(AFinishedThread: TPlayerThread);
var
  List: TList;
  NextThread: TPlayerThread;

  Stop: Boolean;
begin
  List:=FList.LockList;
  try
    Stop:=Terminated or not (List.Count < MaxThreadCount);

    if AFinishedThread <> nil then
    begin
      List.Remove(AFinishedThread);

      if List.Count = 0 then
        RtlEventSetEvent(FStopEvent);
    end;

    if Stop then Exit;

    repeat
      NextThread:=GetNextThread;
      if NextThread <> nil then
      begin
        List.Add(NextThread);
        NextThread.Start;
      end;
    until (NextThread = nil) or Terminated or not (List.Count < MaxThreadCount);

    if NextThread = nil then Interrupt;
  finally
    FList.UnlockList;
  end;
end;

constructor TPlayerThreadManager.Create;
begin
  FMaxThreadCount:=GetMaxThreadCount;
  FList:=TThreadList.Create;

  FEvent:=RTLEventCreate;
  FStopEvent:=RTLEventCreate;

  FForceTerminated:=False;
  FreeOnTerminate:=False;

  inherited Create(True);
end;

destructor TPlayerThreadManager.Destroy;
begin
  FList.Free;
  RTLeventdestroy(FEvent);
  RTLeventdestroy(FStopEvent);

  inherited;
end;

procedure TPlayerThreadManager.Interrupt(const Force: Boolean);
begin
  FForceTerminated:=Force;
  Terminate;
  RtlEventSetEvent(FEvent);
end;

end.

