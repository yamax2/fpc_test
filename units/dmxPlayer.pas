unit dmxPlayer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, sqldb;

type

  { TdmPlayer }

  TdmPlayer = class(TDataModule)
    Connection: TSQLite3Connection;
    Script: TSQLScript;
    Query: TSQLQuery;
    Transaction: TSQLTransaction;
  end;

implementation

{$R *.lfm}

end.

