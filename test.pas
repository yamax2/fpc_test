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
  Classes, SysUtils, Process;

type
  TMyThread = class;

  { TMyThreadManager }

  TMyThreadManager = class(TThread)
  private
    FEvent: pRTLEvent;
    FList: TThreadList;
  protected
    procedure Execute; override;
    function GetNextThread: TMyThread; virtual;
    function GetMaxThreadCount: Integer; virtual;
    procedure Process(AFinishedThread: TMyThread = nil);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Interrupt;
    property MaxThreadCount: Integer read GetMaxThreadCount;
  end;

  { TMyThread }

  TMyThread = class(TThread)
  private
    FManager: TMyThreadManager;
  public
    constructor Create(AManager: TMyThreadManager);
    destructor Destroy; override;
  end;

{ TMyThread }

constructor TMyThread.Create(AManager: TMyThreadManager);
begin
  FManager:=AManager;
  inherited Create(True);
  FreeOnTerminate:=True;
end;

destructor TMyThread.Destroy;
begin
  FManager.Process(Self);
  inherited;
end;

{ TMyThreadManager }

constructor TMyThreadManager.Create;
begin
  FList:=TThreadList.Create;
  FEvent:=RTLEventCreate;
  inherited Create(False);
  FreeOnTerminate:=True;
end;

destructor TMyThreadManager.Destroy;
begin
  FList.Free;
  RTLeventdestroy(FEvent);
  inherited;
end;

procedure TMyThreadManager.Interrupt;
begin
  RtlEventSetEvent(FEvent);
end;

function TMyThreadManager.GetNextThread: TMyThread;
begin
  Result:=nil;
end;

procedure TMyThreadManager.Process(AFinishedThread: TMyThread = nil);
var
  List: TList;
  NextThread: TMyThread;
begin
 List:=FList.LockList;
 try
   if AFinishedThread <> nil then List.Remove(AFinishedThread);
   if not (List.Count < GetMaxThreadCount) or Terminated then Exit;

   repeat
     NextThread:=GetNextThread;
     if NextThread <> nil then
     begin
       List.Add(NextThread);
       NextThread.Start;
     end;
   until (NextThread = nil) or not (List.Count < GetMaxThreadCount);

   if NextThread = nil then Interrupt;
 finally
   FList.UnlockList;
 end;
end;

function TMyThreadManager.GetMaxThreadCount: Integer;
begin
  Result:=2;
end;

procedure TMyThreadManager.Execute;
var
  List: TList;
  Handles: array of TThreadID;
  Index: Integer;
begin
 Process;
 RtlEventWaitFor(FEvent);
 Terminate;

 List:=FList.LockList;
 try
   SetLength(Handles, List.Count);
   for Index:=0 to List.Count - 1 do Handles[Index]:=TThread(List[Index]).Handle;
 finally
   FList.UnlockList;
 end;

 for Index:=0 to Length(Handles) - 1 do
   WaitForThreadTerminate(Handles[Index], 0);
end;

const
  BUF_SIZE = 16384;

type

  { TTestThread }

  TTestThread = class(TMyThread)
  private
    FNumber: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(AManager: TMyThreadManager; ANumber: Integer);
  end;

  { TTestThreadManager }

  TTestThreadManager = class(TMyThreadManager)
  private
    FCount: Integer;
  protected
    function GetNextThread: TMyThread; override;
  public
    constructor Create;
  end;

{ TTestThreadManager }

function TTestThreadManager.GetNextThread: TMyThread;
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

constructor TTestThread.Create(AManager: TMyThreadManager; ANumber: Integer);
begin
 FNumber:=ANumber;
 inherited Create(AManager);
end;

var
  T: TThread;
begin
  T:=TTestThreadManager.Create;
  T.WaitFor;
end.
