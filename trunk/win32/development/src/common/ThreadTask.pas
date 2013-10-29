unit ThreadTask;

interface

uses
  SysUtils, Classes, Windows;

type
  TProcedure = procedure();

  TThreadTask = class;

  TDependence = record
    Task: TThreadTask;
    Critical: Boolean;
  end;

  TThreadTask = class(TThread)
  private
    FName: string;
    FSync: boolean;
    FTask: TProcedure;
    FDep: array of TDependence;
    FTimeout: Cardinal;
  protected
    procedure DoTask;
    procedure Execute; override;
  public
    procedure AddDependence(Task: TThreadTask; Critical: Boolean);
    constructor Create(const Name: string; Task: TProcedure; TimeOut: Cardinal; Sync: Boolean);
  end;

implementation

{ ThreadTask }

{==============================================================================}
procedure TThreadTask.AddDependence(Task: TThreadTask; Critical: Boolean);
{==============================================================================}
begin
  SetLength(FDep, Length(FDep) + 1);
  FDep[High(FDep)].Task := Task;
  FDep[High(FDep)].Critical := Critical; 
end;

{==============================================================================}
constructor TThreadTask.Create(const Name: string; Task: TProcedure; TimeOut: Cardinal; Sync: Boolean);
{==============================================================================}
begin
  inherited Create(True);
  FName := Name;
  FTask := Task;
  FTimeout := Timeout;
  FSync := Sync;
end;

{==============================================================================}
procedure TThreadTask.DoTask;
{==============================================================================}
begin
  if (Assigned(FTask)) then
    FTask();
end;

{==============================================================================}
procedure TThreadTask.Execute;
{==============================================================================}
var
  i: integer;
begin
  // ожидание
  for i := 0 to High(FDep) do
  begin
    WaitForSingleObject(FDep[i].Task.Handle, FDep[i].Task.FTimeout);

    if (FDep[i].Critical and (FDep[i].Task.ReturnValue <> 0)) then
    begin
      ReturnValue := 1;
      Exit;
    end;
  end;

  // выполнение
  try
    if (FSync) then
      Synchronize(DoTask)
    else
      DoTask();

    ReturnValue := 0;
  except
    on E: Exception do
    begin
      ReturnValue := 1;
      //raise E;
    end;
  end;
end;

end. 
