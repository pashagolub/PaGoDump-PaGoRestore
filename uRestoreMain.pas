unit uRestoreMain;

{$D+} {$L+}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls, Menus, ImgList, PSQLDump, DB,
  PSQLDbTables, ActnList, CheckLst, Grids, System.Actions, System.ImageList;

type
  TfrmMain = class(TForm)
    BottomPanel: TPanel;
    bbHelp: TBitBtn;
    bbSave: TBitBtn;
    bbFinish: TBitBtn;
    imglMain: TImageList;
    pgDB: TPSQLDatabase;
    pcOptions: TPageControl;
    tsTarget: TTabSheet;
    tsOptions: TTabSheet;
    chkVerbose: TCheckBox;
    edJobs: TLabeledEdit;
    chkNoPrivileges: TCheckBox;
    tsProfile: TTabSheet;
    bbCancel: TBitBtn;
    edProfileFile: TButtonedEdit;
    laProfile: TLabel;
    lbProfiles: TListBox;
    laMRU: TLabel;
    tsLog: TTabSheet;
    mmLog: TMemo;
    odProfile: TOpenDialog;
    sdOutput: TSaveDialog;
    ActionList: TActionList;
    aHelp: TAction;
    aSave: TAction;
    aExecute: TAction;
    tsSelection: TTabSheet;
    btnUpRow: TBitBtn;
    btnDownRow: TBitBtn;
    aUpRow: TAction;
    aDownRow: TAction;
    aClearSel: TAction;
    pgRestore: TPSQLRestore;
    odInputFile: TOpenDialog;
    chkExitOnError: TCheckBox;
    chkNoTablespaces: TCheckBox;
    chkSingleTransaction: TCheckBox;
    aList: TAction;
    btnList: TBitBtn;
    edRole: TLabeledEdit;
    chkNoOwner: TCheckBox;
    chkNoDataForFailed: TCheckBox;
    chkCreateDB: TCheckBox;
    chkClean: TCheckBox;
    chkDataOnly: TCheckBox;
    chkSchemaOnly: TCheckBox;
    chkDisableTriggers: TCheckBox;
    edSuperUser: TLabeledEdit;
    chkUseSetSessionAuthorization: TCheckBox;
    sgSelection: TStringGrid;
    btnClear: TBitBtn;
    tsInput: TTabSheet;
    edFilename: TButtonedEdit;
    laFileName: TLabel;
    cbArchiveFormat: TComboBox;
    laFormat: TLabel;
    gbDbTarget: TGroupBox;
    edHost: TLabeledEdit;
    edDbName: TComboBox;
    edUser: TLabeledEdit;
    edTimeout: TLabeledEdit;
    edPort: TLabeledEdit;
    edPassword: TLabeledEdit;
    laDBName: TLabel;
    gbFileTarget: TGroupBox;
    laSQLFilename: TLabel;
    edSQLFilename: TButtonedEdit;
    laSeconds: TLabel;
    rgOutput: TRadioGroup;
    laLibraryName: TLabel;
    cbLibs: TComboBox;
    chkIfExists: TCheckBox;
    chkRowSecurity: TCheckBox;
    procedure edDBNameEnter(Sender: TObject);
    procedure edProfileFileRightButtonClick(Sender: TObject);
    procedure edFilenameRightButtonClick(Sender: TObject);
    procedure edProfileFileKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lbProfilesDblClick(Sender: TObject);
    procedure UpdateMRUList;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lbProfilesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure aExecuteUpdate(Sender: TObject);
    function CanFileBeCreated(FileName: TFileName): boolean;
    procedure bbCancelClick(Sender: TObject);
    procedure aSaveUpdate(Sender: TObject);
    procedure aExecuteExecute(Sender: TObject);
    procedure aSaveExecute(Sender: TObject);
    procedure edProfileFileExit(Sender: TObject);
    procedure pgRestoreAfterDump(Sender: TObject);
    procedure pgRestoreBeforeDump(Sender: TObject);
    procedure pgRestoreLog(Sender: TObject; const LogMessage: string);
    procedure SetDBParams;
    procedure aHelpExecute(Sender: TObject);
    procedure aListUpdate(Sender: TObject);
    procedure aListExecute(Sender: TObject);
    procedure sgSelectionDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure sgSelectionMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure sgSelectionKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure aClearSelUpdate(Sender: TObject);
    procedure aClearSelExecute(Sender: TObject);
    procedure aUpRowUpdate(Sender: TObject);
    procedure aUpRowExecute(Sender: TObject);
    procedure aDownRowExecute(Sender: TObject);
    procedure aDownRowUpdate(Sender: TObject);
    procedure edSQLFilenameRightButtonClick(Sender: TObject);
    procedure cbLibsSelect(Sender: TObject);
    procedure pgRestoreLibraryLoad(Sender: TObject; var FileName: string);
  private
    LibVersion: integer;
    slTables, slSchemas: TStrings;
    function GetIniFileName: string;
    procedure ProcessParams();
    procedure UpdateCaption();
    procedure FillLibraries();
    function DBParamsDiffers(): boolean;
  public
    procedure SaveProfile(FileName: TFileName);
    procedure OpenProfile(FileName: TFileName);
    property IniFileName: string read GetIniFileName;
  end;

  THackGrid = class(TStringGrid);

var
  frmMain: TfrmMain;

resourcestring
  ErrMsg = 'PaGoRestore was unable to connect to database. ' +
    'Please check your Connection tab input.'#13#10 + 'Details:'#13#10 +
    '   %s';
  sRestoreFinished = 'Restore finished: ';
  sRestoreStarted = 'Restore started: ';
  sTempFileErr = 'Cannot create temporary file';

  SCaption = 'PaGoRestore - PostgreSQL %s DB restore';

implementation

{$R *.dfm}

Uses ShellAPI, StrUtils, Math;

var
  OperationTime: cardinal;

procedure ComponentToStream(Components: array of TComponent; AStream: TStream);
var
  i: integer;
begin
  for i := Low(Components) to High(Components) do
    AStream.WriteComponent(Components[i]);
end;

procedure StreamToComponent(Components: array of TComponent; AStream: TStream);
var
  i: integer;
begin
  AStream.Seek(0, soFromBeginning);
  for i := Low(Components) to High(Components) do
    AStream.ReadComponent(Components[i]);
end;

procedure TfrmMain.aClearSelExecute(Sender: TObject);
begin
  sgSelection.RowCount := 1;
  sgSelection.Cols[0].Clear;
  sgSelection.Enabled := False;
end;

procedure TfrmMain.aClearSelUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := sgSelection.Enabled and (sgSelection.RowCount > 1);
end;

procedure TfrmMain.aDownRowExecute(Sender: TObject);
begin
  sgSelection.Cols[0].Exchange(sgSelection.Row, sgSelection.Row + 1);
  sgSelection.Row := sgSelection.Row + 1;
end;

procedure TfrmMain.aDownRowUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := sgSelection.Enabled and (sgSelection.Row < sgSelection.RowCount - 1);
end;

procedure TfrmMain.aExecuteExecute(Sender: TObject);

  procedure AddOption(chk: TCheckBox; Option: TRestoreOption);
  begin
    if chk.Checked then pgRestore.Options := pgRestore.Options + [Option];
  end;

var
  OldCursor: TCursor;
  tmpLstFile: string;
begin
  OldCursor := Screen.Cursor;
  try
    Screen.Cursor := crSQLWait;

    pcOptions.ActivePage := tsLog;

    UpdateMRUList;

    aExecute.Enabled := False;

    SetDBParams();

    pgRestore.DBName := '';
    pgRestore.OutputFileName := '';
    pgRestore.ListFile := '';
    pgRestore.Options := [];

    pgRestore.RestoreFormat := TRestoreFormat(cbArchiveFormat.ItemIndex);
    if rgOutput.ItemIndex = 0 then
      pgRestore.DBName := pgDB.DatabaseName
    else
      pgRestore.OutputFileName := edSQLFilename.Text;

    pgRestore.Role := edRole.Text;
    pgRestore.SuperUserName := edSuperUser.Text;
    pgRestore.Jobs := StrToIntDef(edJobs.Text, 1);

    AddOption(chkCreateDB, roCreate);
    AddOption(chkDataOnly, roDataOnly);
    AddOption(chkSchemaOnly, roSchemaOnly);
    AddOption(chkClean, roClean);
    AddOption(chkIfExists, roIfExists);
    AddOption(chkExitOnError, roExitOnError);
    AddOption(chkSingleTransaction, roSingleTransaction);
    AddOption(chkVerbose, roVerbose);
    AddOption(chkNoPrivileges, roNoPrivileges);
    AddOption(chkNoTablespaces, roNoTablespaces);
    AddOption(chkNoOwner, roNoOwner);
    AddOption(chkNoDataForFailed, roNoDataForFailedTables);
    AddOption(chkDisableTriggers, roDisableTriggers);
    AddOption(chkUseSetSessionAuthorization, roUseSetSessionAuthorization);
    AddOption(chkRowSecurity, roEnableRowSecurity);

    if sgSelection.Enabled then
     begin
      tmpLstFile := GetTempFileName();
      sgSelection.Cols[0].SaveToFile(tmpLstFile);
      pgRestore.ListFile := tmpLstFile;
     end;

    try
      pgRestore.RestoreFromFile(edFilename.Text, '');
    except
      on E: EPSQLDatabaseError do ShowMessage(E.Message);
    end;

    if sgSelection.Enabled then DeleteFile(tmpLstFile);

    aExecute.Enabled := True;
  finally
    Screen.Cursor := OldCursor;
  end;
end;

procedure TfrmMain.aExecuteUpdate(Sender: TObject);
var
  i: integer;
begin
  (Sender as TAction).Enabled := FileExists(edFilename.Text);
  gbFileTarget.Visible := rgOutput.ItemIndex = 1;
  gbDbTarget.Visible := not gbFileTarget.Visible;
  chkDataOnly.Enabled := not chkSchemaOnly.Checked;
  chkSchemaOnly.Enabled := not chkDataOnly.Checked;
  chkNoOwner.Enabled := not chkDataOnly.Checked;
  chkCreateDB.Enabled := not chkDataOnly.Checked;
  chkDisableTriggers.Enabled := chkDataOnly.Enabled and chkDataOnly.Checked;
  edSuperUser.Enabled := chkDisableTriggers.Enabled and chkDisableTriggers.Checked;
  chkIfExists.Enabled := chkClean.Checked;
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TCheckBox then
      with Components[i] as TCheckBox do
        if not Enabled then
          Checked := False;
end;

procedure TfrmMain.aHelpExecute(Sender: TObject);
begin
  ShellExecute(0, 'open', 'Manual\PaGoRestore.html', '', '', SW_SHOWNORMAL);
end;

procedure TfrmMain.aListExecute(Sender: TObject);
var
  SL: TStrings;
  tmpLstFile: string;
begin
  tmpLstFile := GetTempFileName();
  pgRestore.OutputFileName := tmpLstFile;
  pgRestore.DBName := '';

  pgRestore.Options := [roList, roVerbose];
  pgRestore.RestoreFromFile(edFilename.Text, '');
  try
    SL := TStringList.Create;
    try
      SL.LoadFromFile(tmpLstFile, TEncoding.UTF8);
      sgSelection.RowCount := SL.Count;
      sgSelection.Cols[0].Assign(SL);
      sgSelection.Enabled := True;
      sgSelection.SetFocus;
    finally
      Sl.Free;
    end;
  finally
    DeleteFile(tmpLstFile);
  end;
end;

procedure TfrmMain.aListUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := FileExists(edFilename.Text);
  tsSelection.Enabled := (Sender as TAction).Enabled;
end;

procedure TfrmMain.aSaveExecute(Sender: TObject);
begin
  SaveProfile(edProfileFile.Text);
  UpdateMRUList;
end;

procedure TfrmMain.aSaveUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := CanFileBeCreated(edProfileFile.Text);
end;

procedure TfrmMain.aUpRowExecute(Sender: TObject);
begin
  sgSelection.Cols[0].Exchange(sgSelection.Row, sgSelection.Row - 1);
  sgSelection.Row := sgSelection.Row - 1;
end;

procedure TfrmMain.aUpRowUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := sgSelection.Enabled and (sgSelection.Row > 0);
end;

procedure TfrmMain.bbCancelClick(Sender: TObject);
begin
  pgDB.Close;
  Close();
end;

function TfrmMain.CanFileBeCreated(FileName: TFileName): boolean;
var
  FH: integer;
begin
  Result := FileExists(FileName);
  if Result then
    Exit;
  FH := FileCreate(FileName);
  Result := FH <> -1;
  if Result then
  begin
    FileClose(FH);
    DeleteFile(FileName);
  end;
end;

procedure TfrmMain.cbLibsSelect(Sender: TObject);
begin
  UpdateCaption();
end;

function TfrmMain.DBParamsDiffers: boolean;
begin
  Result := (pgDB.UserName <> edUser.Text) or
            (pgDB.UserPassword <> edPassword.Text) or
            (pgDB.Port <> StrToInt64Def(edPort.Text, 5432)) or
            (pgDB.DatabaseName <> edDBName.Text) or
            (pgDB.ConnectionTimeout <> StrToInt64Def(edTimeout.Text, 15));
end;

procedure TfrmMain.edDBNameEnter(Sender: TObject);
begin
  if not pgDB.Connected or DBParamsDiffers() then
    try
      SetDBParams();
      pgDB.DatabaseName := 'template1';
      pgDB.Connected := True;
    except
      Exit;
    end;

  edDbName.Items.Clear;
  pgDB.GetDatabases('', edDbName.Items);
end;

procedure TfrmMain.edFilenameRightButtonClick(Sender: TObject);
begin
  if odInputFile.Execute then
    edFilename.Text := odInputFile.FileName;
end;

procedure TfrmMain.edProfileFileExit(Sender: TObject);
begin
  if FileExists(odProfile.FileName) then
    OpenProfile(odProfile.FileName);
end;

procedure TfrmMain.edProfileFileKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    if FileExists(odProfile.FileName) then
      OpenProfile(odProfile.FileName);
end;

procedure TfrmMain.edProfileFileRightButtonClick(Sender: TObject);
begin
  if odProfile.Execute then
  begin
    edProfileFile.Text := odProfile.FileName;
    if FileExists(odProfile.FileName) then
      OpenProfile(odProfile.FileName);
  end;
end;

procedure TfrmMain.edSQLFilenameRightButtonClick(Sender: TObject);
begin
 if sdOutput.Execute then
  edSQLFilename.Text := sdOutput.FileName;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  lbProfiles.Items.SaveToFile(IniFileName, TEncoding.UTF8);
  slTables.Free;
  slSchemas.Free;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  i: integer;

begin
  edProfileFile.Hint := 'Enter here existing profile file name'#13#10 +
    'to open previously saved options or'#13#10 +
    'new name to have Save functionality enabled';

  if FileExists(IniFileName) then
   begin
    lbProfiles.Items.LoadFromFile(IniFileName, TEncoding.UTF8);
    for i := lbProfiles.Items.Count - 1 downto 0 do
      if not FileExists(lbProfiles.Items[i]) then
        lbProfiles.Items.Delete(i);
   end;
  pcOptions.ActivePage := tsProfile;
  slTables := TStringList.Create;
  slSchemas := TStringList.Create;
  FillLibraries();
  ProcessParams();
end;

procedure TfrmMain.FillLibraries;
var
  SR: TSearchRec;
  FileAttrs: Integer;
  h : Cardinal;
  pdmbvm_GetVersionAsInt : Tpdmbvm_GetVersionAsInt;
  MaxVers, Idx, Ver: integer;
begin
  MaxVers := 0;
  cbLibs.Items.BeginUpdate;
  try
    FileAttrs := SysUtils.faReadOnly;
    if SysUtils.FindFirst('*.dll', FileAttrs, SR) = 0 then
    begin
      repeat
        h := LoadLibrary(PChar(SR.Name));
        try
         @pdmbvm_GetVersionAsInt := GetProcAddress(h, PAnsiChar('pdmbvm_GetVersionAsInt'));
         if not Assigned(pdmbvm_GetVersionAsInt) then Continue;
         if not Assigned(GetProcAddress(h, PAnsiChar('v3_restore'))) then Continue;
         Ver := pdmbvm_GetVersionAsInt();
         if Ver > 0 then
          begin
           Idx := cbLibs.Items.Add(SR.Name);
           if Ver > MaxVers then
            begin
             MaxVers := Ver;
             cbLibs.ItemIndex := Idx;
            end;
          end;
        finally
         FreeLibrary(H);
        end;
      until FindNext(sr) <> 0;
      FindClose(sr);
    end;
  finally
   LibVersion := MaxVers;
   cbLibs.Items.EndUpdate;
   UpdateCaption();
  end;
end;

function TfrmMain.GetIniFileName: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + 'PaGoRestore.ini';
end;

procedure TfrmMain.lbProfilesDblClick(Sender: TObject);
begin
  if lbProfiles.ItemIndex = -1 then
    Exit;
  edProfileFile.Text := lbProfiles.Items[lbProfiles.ItemIndex];
  if FileExists(edProfileFile.Text) then
    OpenProfile(edProfileFile.Text);
end;

procedure TfrmMain.lbProfilesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    lbProfilesDblClick(Sender);
end;

procedure TfrmMain.OpenProfile(FileName: TFileName);
var
  FS: TStringList;
begin
  FS := TStringList.Create();
  try
    FS.LoadFromFile(FileName, TEncoding.UTF8);

//Input
    edFilename.Text := FS.Values['FileName'];
    cbArchiveFormat.ItemIndex := StrToIntDef(FS.Values['Format'], 0);

//Target
    rgOutput.ItemIndex := StrToIntDef(FS.Values['Target'], 0);
    edSQLFilename.Text := FS.Values['SQLFileName'];
    edHost.Text := FS.Values['Host'];
    edUser.Text := FS.Values['User'];
    edPassword.Text := FS.Values['Pass'];
    edPort.Text := FS.Values['Port'];
    edTimeout.Text := FS.Values['Timeout'];
    edDbName.Text := FS.Values['DBName'];

//Options
    edFilename.Text := FS.Values['FileName'];
    edRole.Text := FS.Values['Role'];
    edJobs.Text := FS.Values['Jobs'];
    chkCreateDB.Checked := StrToBoolDef(FS.Values['CreateDB'], False);
    chkDataOnly.Checked := StrToBoolDef(FS.Values['DataOnly'], False);
    chkSchemaOnly.Checked := StrToBoolDef(FS.Values['SchemaOnly'], False);
    chkClean.Checked := StrToBoolDef( FS.Values['Clean'], False);
    chkIfExists.Checked := StrToBoolDef( FS.Values['IfExists'], False);
    chkExitOnError.Checked := StrToBoolDef( FS.Values['ExitOnError'], False);
    chkSingleTransaction.Checked := StrToBoolDef( FS.Values['SingleTransaction'], False);
    chkVerbose.Checked := StrToBoolDef(FS.Values['Verbose'], True);
    chkNoTablespaces.Checked := StrToBoolDef( FS.Values['NoTablespaces'], False);
    chkNoPrivileges.Checked := StrToBoolDef(FS.Values['NoPrivileges'], False);
    chkNoOwner.Checked := StrToBoolDef(FS.Values['NoOwner'], False);
    chkNoDataForFailed.Checked := StrToBoolDef(FS.Values['NoDataForFailed'], False);
    chkUseSetSessionAuthorization.Checked := StrToBoolDef(FS.Values['UseSSAUTH'], False);
    chkDisableTriggers.Checked := StrToBoolDef(FS.Values['DisableTriggers'], False);
    edSuperUser.Text := FS.Values['Superuser'];
    chkRowSecurity.Checked := StrToBoolDef(FS.Values['RowSecurity'], False);

//Selection
    sgSelection.Cols[0].Text := FS.Values['Selection'];

    pcOptions.ActivePage := tsInput;
  finally
    FS.Free;
  end;
end;

procedure TfrmMain.pgRestoreAfterDump(Sender: TObject);
begin
  mmLog.Lines.Add(sRestoreFinished + DateTimeToStr(Now()));
end;

procedure TfrmMain.pgRestoreBeforeDump(Sender: TObject);
begin
  mmLog.Clear;
  mmLog.Lines.Add(sRestoreStarted + DateTimeToStr(Now()));
  OperationTime := GetTickCount();
end;

procedure TfrmMain.pgRestoreLibraryLoad(Sender: TObject; var FileName: string);
begin
  FileName := cbLibs.Text;
end;

procedure TfrmMain.pgRestoreLog(Sender: TObject; const LogMessage: string);
begin
  mmLog.Lines.Append(LogMessage + Format(' [%1.3f sec]',
      [(GetTickCount() - OperationTime) / 1000]));
  OperationTime := GetTickCount();
end;

procedure TfrmMain.ProcessParams;
var
  i: integer;
  SL: TStringList;
begin
  if ParamCount = 0 then
    Exit;
  SL := TStringList.Create;
  try
    for i := 1 to ParamCount do
      SL.Append(ParamStr(i));

    if SL.Values['--profile'] > '' then
      OpenProfile(SL.Values['--profile']);
    if SL.Values['--host'] > '' then
      edHost.Text := SL.Values['--host'];
    if SL.Values['--port'] > '' then
      edPort.Text := SL.Values['--port'];
    if SL.Values['--user'] > '' then
      edUser.Text := SL.Values['--user'];
    if SL.Values['--db'] > '' then
      edDbName.Text := SL.Values['--db'];
    if SL.Values['--pwd'] > '' then
      edPassword.Text := SL.Values['--pwd'];

    pcOptions.ActivePage := tsInput;

  finally
    SL.Free;
  end;
end;

procedure TfrmMain.SaveProfile(FileName: TFileName);
var
  FS: TStringList;

  procedure WriteString(const Name, Value: string);
  begin
    FS.Add(Name + FS.NameValueSeparator + Value);
  end;

  procedure WriteBool(const Name: string; Value: boolean);
  begin
    WriteString(Name, BoolToStr(Value));
  end;

begin
  FS := TStringList.Create();
  try
//Input
    WriteString('FileName', edFilename.Text);
    WriteString( 'Format', IntToStr(cbArchiveFormat.ItemIndex));

//Target
    WriteString('Target', IntToStr(rgOutput.ItemIndex));
    WriteString('SQLFileName', edSQLFilename.Text);
    WriteString('Host', edHost.Text);
    WriteString('User', edUser.Text);
    WriteString('Pass', edPassword.Text);
    WriteString('Port', edPort.Text);
    WriteString('Timeout', edTimeout.Text);
    WriteString('DBName', edDbName.Text);

//Options
    WriteString('Role', edRole.Text);
    WriteString('Jobs', edJobs.Text);
    WriteBool('CreateDB', chkCreateDB.Checked);
    WriteBool('DataOnly', chkDataOnly.Checked);
    WriteBool('SchemaOnly', chkSchemaOnly.Checked);
    WriteBool('Clean', chkClean.Checked);
    WriteBool('IfExists', chkIfExists.Checked);
    WriteBool('ExitOnError', chkExitOnError.Checked);
    WriteBool('SingleTransaction', chkSingleTransaction.Checked);
    WriteBool('Verbose', chkVerbose.Checked);
    WriteBool('NoTablespaces', chkNoTablespaces.Checked);
    WriteBool('NoPrivileges', chkNoPrivileges.Checked);
    WriteBool('NoOwner', chkNoOwner.Checked);
    WriteBool('NoDataForFailed', chkNoDataForFailed.Checked);
    WriteBool('UseSSAUTH', chkUseSetSessionAuthorization.Checked);
    WriteBool('DisableTriggers', chkDisableTriggers.Checked);
    WriteString('Superuser', edSuperUser.Text);
    WriteBool('RowSecurity', chkRowSecurity.Checked);

//Selection
    WriteString('Selection', sgSelection.Cols[0].Text);

    FS.SaveToFile(FileName, TEncoding.UTF8);
  finally
    FS.Free;
  end;
end;

procedure TfrmMain.SetDBParams;
begin
  pgDB.Close;
  pgDB.Host := edHost.Text;
  pgDB.UserName := edUser.Text;
  pgDB.UserPassword := edPassword.Text;
  pgDB.Port := StrToIntDef(edPort.Text, 5432);
  pgDB.DatabaseName := edDbName.Text;
  pgDB.ConnectionTimeout := StrToIntDef(edTimeout.Text, 15);
end;

procedure TfrmMain.sgSelectionDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var
  S: string;
begin
  if sgSelection.RowCount = 1 then
  begin
    sgSelection.Canvas.FillRect(Rect);
    Exit;
  end;

  S := sgSelection.Cells[ACol, ARow];
  if StartsStr(';', S) then
    begin
     if gdSelected in State then
       THackGrid(sgSelection).DrawCellHighlight(Rect, State, ACol, ARow)
     else
       THackGrid(sgSelection).DrawCellBackground(Rect, clInactiveCaption, State, ACol, ARow);
     if gdSelected in State then
       sgSelection.Canvas.Font.Color := Font.Color
     else
       sgSelection.Canvas.Font.Color := clInactiveCaptionText;
     sgSelection.Canvas.Font.Style := [fsItalic];
     DrawText(sgSelection.Canvas.Handle, PChar(S), length(S), Rect, DT_LEFT or DT_EXPANDTABS);
    end;
end;

procedure TfrmMain.sgSelectionKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F2 then
    sgSelectionMouseDown(nil, mbLeft, [], 0, 0);
end;

procedure TfrmMain.sgSelectionMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button <> mbLeft then Exit;
  THackGrid(sgSelection).ShowEditor;
  THackGrid(sgSelection).InplaceEditor.SelStart := 0;
end;

procedure TfrmMain.UpdateCaption;
begin
  Caption := Format(sCaption, [pgRestore.VersionAsStr]);
  LibVersion := pgRestore.VersionAsInt;
end;

procedure TfrmMain.UpdateMRUList;
var
  OldPos: integer;
begin
  OldPos := lbProfiles.Items.IndexOf(edProfileFile.Text);
  if OldPos > -1 then
    lbProfiles.Items.Move(OldPos, 0)
  else
    lbProfiles.Items.Insert(0, edProfileFile.Text);
end;

end.
