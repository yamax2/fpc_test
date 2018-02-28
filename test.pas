program Test;
{$MODE OBJFPC}{$H+}

uses
{$ifdef unix}
  cthreads,
  cmem,
{$endif}
  Classes, SysUtils, Process;

type
  TMyThread = class(TThread)
  private
    FFileName: String;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean; AFileName: String);
    property Terminated;
  end;

const
  BUF_SIZE = 16384;

procedure ReadIt(Thread: TMyThread);
var
  AProcess: TProcess;
  Buffer: array[1..BUF_SIZE] of byte;
  BytesRead: longint;
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

    OutputStream:=TFileStream.Create(Thread.FFileName, fmCreate);
    try
      repeat
        BytesRead:=AProcess.Output.Read(Buffer, BUF_SIZE);
        OutputStream.Write(Buffer, BytesRead);
      until Thread.Terminated or (BytesRead = 0);
    finally
      OutputStream.Free;
    end;
  finally
    AProcess.Free;
  end;
end;

constructor TMyThread.Create(CreateSuspended : Boolean; AFileName: String);
begin
  FFileName:=AFileName;
  inherited Create(CreateSuspended);
  FreeOnTerminate:=True;
end;

procedure TMyThread.Execute;
begin
  ReadIt(Self);
end;

var 
  t: TMyThread;
begin
  t:=TMyThread.Create(False, '1.txt');
  t.WaitFor;
end.
