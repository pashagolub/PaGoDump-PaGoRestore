program PaGoRestore;

uses
  {$IFDEF EurekaLog}
  EMemLeaks,
  EResLeaks,
  ESendAPIMantis,
  EDialogWinAPIMSClassic,
  EDialogWinAPIEurekaLogDetailed,
  EDialogWinAPIStepsToReproduce,
  EDebugExports,
  EDebugJCL,
  EAppVCL,
  ExceptionLog7,
  {$ENDIF EurekaLog}
  Forms,
  uRestoreMain in 'uRestoreMain.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'PaGoRestore';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
