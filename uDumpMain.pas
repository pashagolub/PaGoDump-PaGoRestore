unit uDumpMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls, Menus, ImgList, PSQLDump, DB,
  PSQLDbTables, ActnList, ExtCtrls, System.Actions, Vcl.Grids, Vcl.AppEvnts, Vcl.CategoryButtons, PSQLDirectQuery,
  System.ImageList;

type
  TfrmMain = class(TForm)
    BottomPanel: TPanel;
    bbHelp: TBitBtn;
    bbSave: TBitBtn;
    bbFinish: TBitBtn;
    imglMain: TImageList;
    pgDB: TPSQLDatabase;
    pgDump: TPSQLDump;
    pcOptions: TPageControl;
    tsConnect: TTabSheet;
    edHost: TLabeledEdit;
    edPassword: TLabeledEdit;
    edPort: TLabeledEdit;
    edTimeout: TLabeledEdit;
    edUser: TLabeledEdit;
    laDBName: TLabel;
    tsOptions: TTabSheet;
    rgFormat: TRadioGroup;
    chkBLOBs: TCheckBox;
    chkWithOIDs: TCheckBox;
    chkDisableDollars: TCheckBox;
    chkVerbose: TCheckBox;
    edCompression: TLabeledEdit;
    tsExtOptions: TTabSheet;
    chkDataOnly: TCheckBox;
    chkSchemaOnly: TCheckBox;
    chkNoOwner: TCheckBox;
    chkCreateDB: TCheckBox;
    chkDropDB: TCheckBox;
    chkDisableTriggers: TCheckBox;
    chkUseSetSessionAuthorization: TCheckBox;
    chkNoPriveleges: TCheckBox;
    edSuperUser: TLabeledEdit;
    chkUseInserts: TCheckBox;
    chkUseColumnInserts: TCheckBox;
    tsProfile: TTabSheet;
    bbCancel: TBitBtn;
    edProfileFile: TButtonedEdit;
    laProfile: TLabel;
    edFilename: TButtonedEdit;
    laFileName: TLabel;
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
    edDbName: TComboBox;
    laEncoding: TLabel;
    edEncoding: TComboBox;
    tsSelection: TTabSheet;
    btnAddSel: TBitBtn;
    btnDelSel: TBitBtn;
    btnUpRow: TBitBtn;
    btnDownRow: TBitBtn;
    aAddSel: TAction;
    aDelSel: TAction;
    aUpRow: TAction;
    aDownRow: TAction;
    btnClearAll: TBitBtn;
    aClearSel: TAction;
    edLockWaitTimeout: TLabeledEdit;
    chkNoTablespace: TCheckBox;
    edRole: TLabeledEdit;
    cbLibs: TComboBox;
    Label1: TLabel;
    linkHomePage: TLinkLabel;
    lbVersion: TLabel;
    lbInplaceEditor: TListBox;
    sgSelection: TDrawGrid;
    pgQuery: TPSQLDirectQuery;
    chkIfExists: TCheckBox;
    chkRowSecurity: TCheckBox;
    procedure SelectionChanged(Sender: TObject);
    procedure edDBNameEnter(Sender: TObject);
    procedure edProfileFileRightButtonClick(Sender: TObject);
    procedure edFilenameRightButtonClick(Sender: TObject);
    procedure edProfileFileKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lbProfilesDblClick(Sender: TObject);
    procedure UpdateMRUList;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lbProfilesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure aExecuteUpdate(Sender: TObject);
    function CanFileBeCreated(FileName: TFileName): boolean;
    procedure bbCancelClick(Sender: TObject);
    procedure aSaveUpdate(Sender: TObject);
    procedure aExecuteExecute(Sender: TObject);
    procedure edCompressionChange(Sender: TObject);
    procedure aSaveExecute(Sender: TObject);
    procedure edProfileFileExit(Sender: TObject);
    procedure pgDumpAfterDump(Sender: TObject);
    procedure pgDumpBeforeDump(Sender: TObject);
    procedure pgDumpLog(Sender: TObject; const LogMessage: string);
    procedure SetDBParams;
    procedure sdOutputShow(Sender: TObject);
    procedure aHelpExecute(Sender: TObject);
    procedure edEncodingDropDown(Sender: TObject);
    procedure aDelSelUpdate(Sender: TObject);
    procedure aDownRowUpdate(Sender: TObject);
    procedure aUpRowUpdate(Sender: TObject);
    procedure aDownRowExecute(Sender: TObject);
    procedure aUpRowExecute(Sender: TObject);
    procedure aAddSelExecute(Sender: TObject);
    procedure pcOptionsChange(Sender: TObject);

    procedure aDelSelExecute(Sender: TObject);
    procedure aClearSelUpdate(Sender: TObject);
    procedure aClearSelExecute(Sender: TObject);
    procedure linkHomePageLinkClick(Sender: TObject; const Link: string; LinkType: TSysLinkType);
    procedure cbLibsSelect(Sender: TObject);
    procedure pgDumpLibraryLoad(Sender: TObject; var FileName: string);
    procedure rgFormatClick(Sender: TObject);
    procedure cbInplaceEditorDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure cbInplaceEditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ApplicationEventsDeactivate(Sender: TObject);
    procedure sgSelectionKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sgSelectionDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure sgSelectionDblClick(Sender: TObject);
    procedure lbInplaceEditorExit(Sender: TObject);
    procedure lbInplaceEditorDblClick(Sender: TObject);
    procedure aAddSelUpdate(Sender: TObject);
  private
    LibVersion: Integer;
    slTables, slSelection: TStringList;
    function GetIniFileName: string;
    function IsPlainFmt: boolean;
    function DbParamsDiffers(): boolean;
    procedure CreateSelectionNode(const Action, Kind: char; const Name: string);
    procedure ProcessParams();
    procedure UpdateCaption();
    procedure FillLibraries();
    procedure FillActions(SL: TStrings);
    procedure FillSelectionObjects(SL: TStrings);
    procedure CheckVerDependencies(const aVersion: Integer);
  public
    procedure SaveProfile(FileName: TFileName);
    procedure OpenProfile(FileName: TFileName);
    property IniFileName: string read GetIniFileName;
  end;

var
  frmMain: TfrmMain;

const
  ErrMsg: string = 'PaGoDump was unable to connect to database. ' + 'Please check your Connection tab input.'#13#10 +
    'Details:'#13#10 + '   %s';
  SCaption: string = 'PaGoDump - PostgreSQL %s (and lower) DB backup';

  ColIdxName = 0;
  ColIdxAction = 1;

  imgSchema: Integer = 1;
  imgTable: Integer = 2;
  imgInclude: Integer = 3;
  imgExclude: Integer = 4;
  imgExcludeData: Integer = 5;

implementation

{$R *.dfm}

Uses ShellAPI, StrUtils, Math, System.UITypes;

var
  OperationTime: cardinal;

procedure ComponentToStream(Components: array of TComponent; AStream: TStream);
var
  i: Integer;
begin
  for i := Low(Components) to High(Components) do
    AStream.WriteComponent(Components[i]);
end;

procedure StreamToComponent(Components: array of TComponent; AStream: TStream);
var
  i: Integer;
begin
  AStream.Seek(0, soFromBeginning);
  for i := Low(Components) to High(Components) do
    AStream.ReadComponent(Components[i]);
end;

procedure TfrmMain.CheckVerDependencies(const aVersion: Integer);
begin
  edRole.Enabled := aVersion >= 080400;
  edLockWaitTimeout.Enabled := aVersion >= 080400;
end;

procedure TfrmMain.CreateSelectionNode(const Action, Kind: char; const Name: string);
begin
  slSelection.AddObject(Name + slSelection.NameValueSeparator + Action, TObject(Kind));
end;

function TfrmMain.DbParamsDiffers: boolean;
begin
  Result := (pgDB.UserName <> edUser.Text) or (pgDB.UserPassword <> edPassword.Text) or
    (pgDB.Port <> StrToInt64Def(edPort.Text, 5432)) or (pgDB.DatabaseName <> edDbName.Text) or
    (pgDB.ConnectionTimeout <> StrToInt64Def(edTimeout.Text, 15));
end;

procedure TfrmMain.aAddSelExecute(Sender: TObject);
begin
  CreateSelectionNode('I', 'T', 'foo');
end;

procedure TfrmMain.aAddSelUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := slTables.Count > 0;
  sgSelection.Enabled := slTables.Count > 0;
end;

procedure TfrmMain.aClearSelExecute(Sender: TObject);
begin
  slSelection.Clear;
  sgSelection.Invalidate;
end;

procedure TfrmMain.aClearSelUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := slSelection.Count > 0;
end;

procedure TfrmMain.aDelSelExecute(Sender: TObject);
begin
  slSelection.Delete(sgSelection.Row - 1);
  sgSelection.Invalidate;
end;

procedure TfrmMain.aDelSelUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := sgSelection.Row < sgSelection.RowCount - 1;
end;

procedure TfrmMain.aDownRowExecute(Sender: TObject);
begin
  slSelection.Exchange(sgSelection.Row - 1, sgSelection.Row - 2);
  sgSelection.Invalidate;
end;

procedure TfrmMain.aDownRowUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := sgSelection.Row < sgSelection.RowCount - 1 - 1;
  // the last row always empty like in excel
end;

procedure TfrmMain.aExecuteExecute(Sender: TObject);

  procedure AddOption(chk: TCheckBox; Option: TDumpOption);
  begin
    if chk.Enabled and chk.Checked then
      pgDump.Options := pgDump.Options + [Option]
    else
      pgDump.Options := pgDump.Options - [Option];
  end;

var
  OldCursor: TCursor;
  i: Integer;
  ErrFile: string;
  TargetList: TStrings;
begin
  OldCursor := Screen.Cursor;
  try
    Screen.Cursor := crSQLWait;

    pcOptions.ActivePage := tsLog;

    UpdateMRUList;

    aExecute.Enabled := False;

    SetDBParams();

    pgDump.DumpFormat := TDumpFormat(rgFormat.ItemIndex);
    pgDump.CompressLevel := StrToIntDef(edCompression.Text, 0);
    pgDump.Encoding := edEncoding.Text;
    pgDump.SuperUserName := edSuperUser.Text;
    if edRole.Enabled then
      pgDump.Role := edRole.Text;
    if edLockWaitTimeout.Enabled then
      pgDump.LockWaitTimeout := StrToIntDef(edLockWaitTimeout.Text, 0);

    AddOption(chkBLOBs, doIncludeBLOBs);
    AddOption(chkWithOIDs, doOIDs);
    AddOption(chkDisableDollars, doDisableDollarQuoting);
    AddOption(chkNoPriveleges, doNoPrivileges);
    AddOption(chkVerbose, doVerbose);

    AddOption(chkDataOnly, doDataOnly);
    AddOption(chkSchemaOnly, doSchemaOnly);
    AddOption(chkNoOwner, doNoOwner);
    AddOption(chkCreateDB, PSQLDump.doCreate);
    AddOption(chkDropDB, doClean);
    AddOption(chkIfExists, doIfExists);
    AddOption(chkDisableTriggers, doDisableTriggers);
    AddOption(chkUseInserts, doInserts);
    AddOption(chkUseColumnInserts, doColumnInserts);
    AddOption(chkUseSetSessionAuthorization, doUseSetSessionAuthorization);
    AddOption(chkNoTablespace, doNoTablespaces);
    AddOption(chkRowSecurity, doEnableRowSecurity);

    pgDump.ExcludeTables.Clear;
    pgDump.ExcludeSchemas.Clear;
    pgDump.TableNames.Clear;
    pgDump.SchemaNames.Clear;

    for i := 0 to slSelection.Count - 1 do
    begin
      case slSelection.ValueFromIndex[i][1] of
        'I': // include
          if char(slSelection.Objects[i]) = 'S' then
            TargetList := pgDump.SchemaNames
          else
            TargetList := pgDump.TableNames;
        'E': // exclude
          if char(slSelection.Objects[i]) = 'S' then
            TargetList := pgDump.ExcludeSchemas
          else
            TargetList := pgDump.ExcludeTables;
      else // 'X' exclude data
        TargetList := pgDump.ExcludeTablesData;
      end;
      TargetList.Append(slSelection.Names[i]);
    end;

    ErrFile := GetTempFileName(); // ChangeFileExt(edFilename.Text, '.log');
    try
      pgDump.DumpToFile(edFilename.Text, ErrFile);
    except
      on E: EPSQLDatabaseError do ShowMessage(E.Message);
      on E: EPSQLDumpException do
      begin
        ShowMessage('Error occured: ' + E.Message);
        if FileExists(ErrFile) then
          mmLog.Lines.LoadFromFile(ErrFile);
      end;
    end;

    aExecute.Enabled := True;
  finally
    DeleteFile(ErrFile);
    Screen.Cursor := OldCursor;
  end;
end;

procedure TfrmMain.aExecuteUpdate(Sender: TObject);
var
  i: Integer;
begin
  (Sender as TAction).Enabled := CanFileBeCreated(edFilename.Text);
  chkBLOBs.Enabled := not IsPlainFmt;
  chkNoTablespace.Enabled := IsPlainFmt and (LibVersion >= 080400);;
  chkDataOnly.Enabled := IsPlainFmt and not chkSchemaOnly.Checked;
  chkSchemaOnly.Enabled := IsPlainFmt and not chkDataOnly.Checked;
  chkNoOwner.Enabled := IsPlainFmt and not chkDataOnly.Checked;
  chkCreateDB.Enabled := IsPlainFmt and not chkDataOnly.Checked;
  chkDropDB.Enabled := IsPlainFmt and not chkDataOnly.Checked;
  chkDisableTriggers.Enabled := chkDataOnly.Enabled and chkDataOnly.Checked;
  edSuperUser.Enabled := chkDisableTriggers.Enabled and chkDisableTriggers.Checked;
  chkUseInserts.Enabled := not chkWithOIDs.Checked;
  chkWithOIDs.Enabled := not chkUseInserts.Checked;
  chkUseColumnInserts.Enabled := chkUseInserts.Checked and chkUseInserts.Enabled;
  chkIfExists.Enabled := chkDropDB.Checked;
  CheckVerDependencies(LibVersion);
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TCheckBox then
      with Components[i] as TCheckBox do
        if not Enabled then
          Checked := False;
end;

procedure TfrmMain.aHelpExecute(Sender: TObject);
begin
  ShellExecute(0, 'open', 'Manual\PaGoDump.html', '', '', SW_SHOWNORMAL);
end;

procedure TfrmMain.ApplicationEventsDeactivate(Sender: TObject);
begin
  lbInplaceEditor.Hide;
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
  slSelection.Exchange(sgSelection.Row - 1, sgSelection.Row - 2);
  sgSelection.Invalidate;
end;

procedure TfrmMain.aUpRowUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := sgSelection.Row > 1;
end;

procedure TfrmMain.bbCancelClick(Sender: TObject);
begin
  pgDB.Close;
  Close();
end;

function TfrmMain.CanFileBeCreated(FileName: TFileName): boolean;
var
  FH: Integer;
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

procedure TfrmMain.cbInplaceEditorDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  CB: TListBox;
  S: string;
  ImgIdx: Integer;
begin
  CB := Control as TListBox;
  CB.Canvas.Fillrect(Rect);
  case char(CB.Items.Objects[Index]) of
    'I':
      ImgIdx := imgInclude;
    'E':
      ImgIdx := imgExclude;
    'X':
      ImgIdx := imgExcludeData;
    'S':
      ImgIdx := imgSchema;
  else // 'T':
    ImgIdx := imgTable;
  end;
  if ImgIdx = imgTable then
    Inc(Rect.Left, 10);
  imglMain.Draw(CB.Canvas, Rect.Left + 1, Rect.Top + 1, ImgIdx);
  S := CB.Items[Index];
  Inc(Rect.Left, imglMain.Width + 3);
  CB.Canvas.TextRect(Rect, S);
end;

procedure TfrmMain.cbInplaceEditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Idx: Integer;
  GridIdx: Integer;
begin
  case Key of
    VK_RETURN:
      begin
        Idx := lbInplaceEditor.ItemIndex;
        GridIdx := sgSelection.Row - 1;
        if GridIdx > slSelection.Count - 1 then
          CreateSelectionNode('I', 'T', '');
        if sgSelection.Col = ColIdxName then
        begin
          slSelection[GridIdx] := lbInplaceEditor.Items[Idx] + slSelection.NameValueSeparator +
            slSelection.ValueFromIndex[GridIdx];
          slSelection.Objects[GridIdx] := lbInplaceEditor.Items.Objects[Idx];
        end
        else // ColIdxAction
          slSelection.ValueFromIndex[GridIdx] := char(lbInplaceEditor.Items.Objects[Idx]);
        lbInplaceEditor.Hide;
        sgSelection.SetFocus;
      end;
    VK_ESCAPE:
      begin
        lbInplaceEditor.Hide;
        sgSelection.SetFocus;
      end;
  end;
end;

procedure TfrmMain.cbLibsSelect(Sender: TObject);
begin
  UpdateCaption();
end;

procedure TfrmMain.edCompressionChange(Sender: TObject);
begin
  if length(edCompression.Text) > 0 then
  begin
    edCompression.Text := edCompression.Text[1];
    if not CharInSet(edCompression.Text[1], ['0' .. '9']) then
      edCompression.Text := '9';
  end;
  rgFormatClick(nil);
end;

procedure TfrmMain.edDBNameEnter(Sender: TObject);
begin
  lbVersion.Caption := '';

  if not pgDB.Connected or DbParamsDiffers() then
    try
      SetDBParams();
      pgDB.DatabaseName := 'template1';
      pgDB.Connected := True;
    except
      Exit;
    end;

  lbVersion.Caption := pgDB.ServerVersion;
  edDbName.Items.Clear;
  pgDB.GetDatabases('', edDbName.Items);
  FillSelectionObjects(slTables);
  edEncoding.Items.Clear;
  pgDB.GetCharsets(edEncoding.Items);
  pgDB.Connected := False;
end;

procedure TfrmMain.edEncodingDropDown(Sender: TObject);
begin
  if edEncoding.Items.Count = 0 then
    edEncoding.Items.CommaText := 'BIG5,EUC_CN,EUC_JIS_2004,EUC_JP,EUC_KR,EUC_TW,' +
      'GB18030,GBK,ISO_8859_5,ISO_8859_6,ISO_8859_7,ISO_8859_8,JOHAB,KOI8,LATIN1,' +
      'LATIN10,LATIN2,LATIN3,LATIN4,LATIN5,LATIN6,LATIN7,LATIN8,LATIN9,MULE_INTERNAL,' +
      'SHIFT_JIS_2004,SJIS,SQL_ASCII,UHC,UTF8,WIN1250,WIN1251,WIN1252,WIN1253,' +
      'WIN1254,WIN1255,WIN1256,WIN1257,WIN1258,WIN866,WIN874';
end;

procedure TfrmMain.edFilenameRightButtonClick(Sender: TObject);
const
  fiCompress = 1;
  fiTar = 2;
  fiGZip = 3;
  fiSQL = 4;
begin
  case rgFormat.ItemIndex of
    0:
      if StrToIntDef(edCompression.Text, 0) > 0 then
        sdOutput.FilterIndex := fiGZip
      else
        sdOutput.FilterIndex := fiSQL;
    1:
      sdOutput.FilterIndex := fiTar;
    2:
      sdOutput.FilterIndex := fiCompress;
  end;
  case sdOutput.FilterIndex of
    fiGZip:
      sdOutput.DefaultExt := 'gzip';
    fiSQL:
      sdOutput.DefaultExt := 'sql';
    fiTar:
      sdOutput.DefaultExt := 'tar';
    fiCompress:
      sdOutput.DefaultExt := 'backup';
  end;
  if sdOutput.Execute then
    edFilename.Text := sdOutput.FileName;
end;

procedure TfrmMain.edProfileFileExit(Sender: TObject);
begin
  if FileExists(odProfile.FileName) then
    OpenProfile(odProfile.FileName);
end;

procedure TfrmMain.edProfileFileKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TfrmMain.FillActions(SL: TStrings);
begin
  SL.Clear;
  SL.AddObject('Include', TObject('I'));
  SL.AddObject('Exclude', TObject('E'));
  SL.AddObject('Exclude Data', TObject('X'));
end;

procedure TfrmMain.FillLibraries;
var
  SR: TSearchRec;
  FileAttrs: Integer;
  h: cardinal;
  pdmbvm_GetVersionAsInt: Tpdmbvm_GetVersionAsInt;
  MaxVers, Idx, Ver: Integer;
begin
  MaxVers := 0;
  cbLibs.Items.BeginUpdate;
  try
    FileAttrs := faNormal + SysUtils.faReadOnly;
    if SysUtils.FindFirst('*.dll', FileAttrs, SR) = 0 then
    begin
      repeat
        h := LoadLibrary(PChar(SR.Name));
        try
          @pdmbvm_GetVersionAsInt := GetProcAddress(h, PAnsiChar('pdmbvm_GetVersionAsInt'));
          if not Assigned(pdmbvm_GetVersionAsInt) then
            Continue;
          if not Assigned(GetProcAddress(h, PAnsiChar('v3_dump'))) then
            Continue;
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
          FreeLibrary(h);
        end;
      until FindNext(SR) <> 0;
      FindClose(SR);
    end;
  finally
    LibVersion := MaxVers;
    cbLibs.Items.EndUpdate;
    UpdateCaption();
  end;
end;

procedure TfrmMain.FillSelectionObjects(SL: TStrings);
var
  CurSchema: string;
const
  ErrMsg: string = 'Unable connect to the PostgreSQL server. Would you like to review connection parameters?';
begin
  SL.Clear;
  try
    if not pgDB.Connected then
    begin
      SetDBParams();
      pgDB.Open;
    end;
    pgQuery.Open;
    while not pgQuery.Eof do
    begin
      if CurSchema <> pgQuery.FieldValues[0] then
      begin
        CurSchema := pgQuery.FieldValues[0];
        SL.AddObject(CurSchema, TObject('S'))
      end
      else
        SL.AddObject(pgQuery.FieldValues[1], TObject('T'));
      pgQuery.Next;
    end;
    pgQuery.Close;
  except
    on E: EPSQLDatabaseError do
      if MessageDlg(ErrMsg, mtWarning, [mbYes, mbNo], 0) = mrYes then
        pcOptions.ActivePage := tsConnect;
  end;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  lbProfiles.Items.SaveToFile(IniFileName, TEncoding.UTF8);
  slTables.Free;
  slSelection.Free;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  edProfileFile.Hint := 'Enter here existing profile file name'#13#10 + 'to open previously saved options or'#13#10 +
    'new name to have Save functionality enabled';
  rgFormat.Hint := 'PLAIN: Output a plain-text SQL script file'#13#10 + 'TAR: Output a tar archive'#13#10 +
    'COMPRESS: Output a custom archive';

  if FileExists(IniFileName) then
  begin
    lbProfiles.Items.LoadFromFile(IniFileName, TEncoding.UTF8);
    for i := lbProfiles.Items.Count - 1 downto 0 do
      if not FileExists(lbProfiles.Items[i]) then
        lbProfiles.Items.Delete(i);
  end;
  pcOptions.ActivePage := tsProfile;
  slTables := TStringList.Create;
  slSelection := TStringList.Create;
  TStringList(slSelection).OnChange := SelectionChanged;
  lbInplaceEditor.Visible := False;
  FillLibraries();
  ProcessParams();
end;

function TfrmMain.GetIniFileName: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + 'PaGoDump.ini';
end;

function TfrmMain.IsPlainFmt: boolean;
begin
  Result := rgFormat.ItemIndex = 0;
end;

procedure TfrmMain.lbInplaceEditorDblClick(Sender: TObject);
var
  K: Word;
begin
  K := VK_RETURN;
  cbInplaceEditorKeyDown(Sender, K, []);
end;

procedure TfrmMain.lbInplaceEditorExit(Sender: TObject);
begin
  lbInplaceEditor.Hide;
  sgSelection.SetFocus;
end;

procedure TfrmMain.lbProfilesDblClick(Sender: TObject);
begin
  if lbProfiles.ItemIndex = -1 then
    Exit;
  edProfileFile.Text := lbProfiles.Items[lbProfiles.ItemIndex];
  if FileExists(edProfileFile.Text) then
    OpenProfile(edProfileFile.Text);
end;

procedure TfrmMain.lbProfilesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    lbProfilesDblClick(Sender);
end;

procedure TfrmMain.linkHomePageLinkClick(Sender: TObject; const Link: string; LinkType: TSysLinkType);
begin
  ShellExecute(0, PChar('open'), PChar(Link), nil, nil, SW_MAXIMIZE);
end;

procedure TfrmMain.OpenProfile(FileName: TFileName);
var
  FS: TStringList;
  i, Start: Integer;
  S: string;
begin
  FS := TStringList.Create();
  try
    FS.LoadFromFile(FileName, TEncoding.UTF8);
    edHost.Text := FS.Values['Host'];
    edUser.Text := FS.Values['User'];
    edPassword.Text := FS.Values['Pass'];
    edPort.Text := FS.Values['Port'];
    edTimeout.Text := FS.Values['Timeout'];
    edDbName.Text := FS.Values['DBName'];

    rgFormat.ItemIndex := StrToIntDef(FS.Values['Format'], 0);
    edFilename.Text := FS.Values['FileName'];
    edCompression.Text := FS.Values['Compression'];
    edEncoding.Text := FS.Values['Encoding'];
    edRole.Text := FS.Values['Role'];
    edLockWaitTimeout.Text := FS.Values['LockWaitTimeout'];
    chkBLOBs.Checked := StrToBoolDef(FS.Values['Blobs'], False);
    chkDisableDollars.Checked := StrToBoolDef(FS.Values['Dollars'], False);
    chkWithOIDs.Checked := StrToBoolDef(FS.Values['Oids'], False);
    chkNoPriveleges.Checked := StrToBoolDef(FS.Values['NoPriveleges'], False);
    chkVerbose.Checked := StrToBoolDef(FS.Values['Verbose'], True);

    chkDataOnly.Checked := StrToBoolDef(FS.Values['DataOnly'], False);
    chkSchemaOnly.Checked := StrToBoolDef(FS.Values['SchemaOnly'], False);
    chkNoOwner.Checked := StrToBoolDef(FS.Values['NoOwner'], False);
    chkCreateDB.Checked := StrToBoolDef(FS.Values['CreateDB'], False);
    chkDropDB.Checked := StrToBoolDef(FS.Values['DropDB'], False);
    chkIfExists.Checked := StrToBoolDef(FS.Values['IfExists'], False);
    chkDisableTriggers.Checked := StrToBoolDef(FS.Values['DisableTriggers'], False);
    chkUseInserts.Checked := StrToBoolDef(FS.Values['UseInsert'], False);
    chkUseColumnInserts.Checked := StrToBoolDef(FS.Values['UseColumnInsert'], False);
    chkUseSetSessionAuthorization.Checked := StrToBoolDef(FS.Values['UseSSAUTH'], False);
    chkNoTablespace.Checked := StrToBoolDef(FS.Values['NoTablespace'], True);
    edSuperUser.Text := FS.Values['Superuser'];
    chkRowSecurity.Checked := StrToBoolDef(FS.Values['RowSecutity'], False);

    slSelection.BeginUpdate;
    try
      Start := FS.IndexOfName('Selection');
      if Start > -1 then
        for i := Start to FS.Count - 1 do
        begin
          S := FS.ValueFromIndex[i];
          if S.length > 2 then
            CreateSelectionNode(S[1], S[2], S.Substring(2));
        end;
    finally
      slSelection.EndUpdate;
    end;

    pcOptions.ActivePage := tsConnect;
  finally
    FS.Free;
  end;
end;

procedure TfrmMain.pcOptionsChange(Sender: TObject);
begin
  if ((Sender as TPageControl).ActivePage <> tsSelection) then
    Exit;
  if slTables.Count = 0 then
    FillSelectionObjects(slTables);
end;

procedure TfrmMain.pgDumpAfterDump(Sender: TObject);
begin
  mmLog.Lines.Add('Dump finished: ' + DateTimeToStr(Now()));
end;

procedure TfrmMain.pgDumpBeforeDump(Sender: TObject);
begin
  mmLog.Clear;
  mmLog.Lines.Add('Dump started: ' + DateTimeToStr(Now()));
  OperationTime := GetTickCount();
end;

procedure TfrmMain.pgDumpLibraryLoad(Sender: TObject; var FileName: string);
begin
  FileName := cbLibs.Text;
end;

procedure TfrmMain.pgDumpLog(Sender: TObject; const LogMessage: string);
begin
  mmLog.Lines.Append(LogMessage + Format(' [%1.3f sec]', [(GetTickCount() - OperationTime) / 1000]));
  OperationTime := GetTickCount();
end;

procedure TfrmMain.ProcessParams;
var
  i: Integer;
  SL: TStringList;
  IsOK: boolean;
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
    if SL.IndexOf('--help') > -1 then
      aHelpExecute(nil);
    if SL.IndexOf('--execute') > -1 then
      if aExecute.Enabled then
      begin
        IsOK := aExecute.Execute;
        if IsOK and (SL.IndexOf('--exit-on-success') > -1) then
          Close;
        pcOptions.ActivePage := tsLog;
      end
      else
        pcOptions.ActivePage := tsConnect;
  finally
    SL.Free;
  end;
end;

procedure TfrmMain.rgFormatClick(Sender: TObject);
var
  Ext: string;
begin
  if edFilename.Text > '' then
  begin
    case rgFormat.ItemIndex of
      0:
        if StrToIntDef(edCompression.Text, 0) > 0 then
          Ext := '.gzip'
        else
          Ext := '.sql';
      1:
        Ext := '.tar';
      2:
        Ext := '.backup';
      3:
        Ext := '';
    end;
    edFilename.Text := ChangeFileExt(edFilename.Text, Ext);
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

var
  i: Integer;

begin
  FS := TStringList.Create();
  try
    WriteString('Host', edHost.Text);
    WriteString('User', edUser.Text);
    WriteString('Pass', edPassword.Text);
    WriteString('Port', edPort.Text);
    WriteString('Timeout', edTimeout.Text);
    WriteString('DBName', edDbName.Text);

    WriteString('Format', IntToStr(rgFormat.ItemIndex));
    WriteString('FileName', edFilename.Text);
    WriteString('Compression', edCompression.Text);
    WriteString('Encoding', edEncoding.Text);
    WriteString('Role', edRole.Text);
    WriteString('LockWaitTimeout', edLockWaitTimeout.Text);

    WriteBool('Blobs', chkBLOBs.Checked);
    WriteBool('Dollars', chkDisableDollars.Checked);
    WriteBool('Oids', chkWithOIDs.Checked);
    WriteBool('NoPriveleges', chkNoPriveleges.Checked);
    WriteBool('Verbose', chkVerbose.Checked);

    WriteBool('DataOnly', chkDataOnly.Checked);
    WriteBool('SchemaOnly', chkSchemaOnly.Checked);
    WriteBool('NoOwner', chkNoOwner.Checked);
    WriteBool('CreateDB', chkCreateDB.Checked);
    WriteBool('DropDB', chkDropDB.Checked);
    WriteBool('IfExists', chkIfExists.Checked);
    WriteBool('DisableTriggers', chkDisableTriggers.Checked);
    WriteBool('UseInsert', chkUseInserts.Checked);
    WriteBool('UseColumnInsert', chkUseColumnInserts.Checked);
    WriteBool('UseSSAUTH', chkUseSetSessionAuthorization.Checked);
    WriteBool('NoTablespace', chkNoTablespace.Checked);
    WriteString('Superuser', edSuperUser.Text);
    WriteBool('RowSecutity', chkRowSecurity.Checked);

    for i := 0 to slSelection.Count - 1 do
      WriteString('Selection', slSelection.ValueFromIndex[i] + char(slSelection.Objects[i]) + slSelection.Names[i]);

    FS.SaveToFile(FileName, TEncoding.UTF8);
  finally
    FS.Free;
  end;
end;

procedure TfrmMain.sdOutputShow(Sender: TObject);
begin
  case rgFormat.ItemIndex of
    1:
      sdOutput.FilterIndex := 2;
    0:
      if edCompression.Text > '0' then
        sdOutput.FilterIndex := 3
      else
        sdOutput.FilterIndex := 4;

    2:
      sdOutput.FilterIndex := 1;
  end;
end;

procedure TfrmMain.SelectionChanged(Sender: TObject);
begin
  sgSelection.RowCount := sgSelection.FixedRows + slSelection.Count + 1;
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

procedure TfrmMain.sgSelectionDblClick(Sender: TObject);
var
  K: Word;
begin
  K := VK_RETURN;
  sgSelectionKeyDown(nil, K, []);
end;

procedure TfrmMain.sgSelectionDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  SG: TDrawGrid;
  S: string;
  ImgIdx: Integer;
begin
  if (ARow = 0) or (ARow > slSelection.Count) then
    Exit;
  ImgIdx := 0;
  SG := Sender as TDrawGrid;
  Dec(ARow); // Strings starts with 0 in list, first row is fixed
  if ACol = ColIdxAction then
    if ARow > -1 then
      case slSelection.ValueFromIndex[ARow][1] of
        'I':
          begin
            S := 'Include';
            ImgIdx := imgInclude;
          end;
        'E':
          begin
            S := 'Exclude';
            ImgIdx := imgExclude;
          end;
        'X':
          begin
            S := 'Exclude Data';
            ImgIdx := imgExcludeData;
          end;
      end
    else
      S := 'Action'
  else if ARow > -1 then
  begin
    S := slSelection.Names[ARow];
    ImgIdx := ifthen(char(slSelection.Objects[ARow]) = 'S', imgSchema, imgTable);
  end
  else
    S := 'Object Name';

  if ImgIdx > -1 then
  begin
    imglMain.Draw(SG.Canvas, Rect.Left + 2, Rect.Top + ((Rect.Height - imglMain.Height) div 2), ImgIdx);
    Inc(Rect.Left, imglMain.Width + 3);
  end;
  SG.Canvas.TextRect(Rect, Rect.Left + 2, Rect.Top + ((Rect.Height - Canvas.TextHeight(S)) div 2), S)
end;

procedure TfrmMain.sgSelectionKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  R: TRect;
begin
  if Key <> VK_RETURN then
    Exit;
  R := sgSelection.CellRect(sgSelection.Col, sgSelection.Row);
  Inc(R.Top, sgSelection.DefaultRowHeight);
  if sgSelection.Col = ColIdxName then
    lbInplaceEditor.Items.Assign(slTables)
  else
    FillActions(lbInplaceEditor.Items);
  lbInplaceEditor.Left := R.Left + sgSelection.ExplicitLeft + sgSelection.GridLineWidth;
  lbInplaceEditor.Top := R.Top + sgSelection.ExplicitTop + sgSelection.GridLineWidth;
  lbInplaceEditor.Width := sgSelection.ColWidths[sgSelection.Col];
  lbInplaceEditor.ClientHeight := Min(lbInplaceEditor.ItemHeight * lbInplaceEditor.Items.Count,
    sgSelection.BoundsRect.Bottom - lbInplaceEditor.Top - 2);
  lbInplaceEditor.Show;
  lbInplaceEditor.SetFocus;
end;

procedure TfrmMain.UpdateCaption;
begin
  Caption := Format(SCaption, [pgDump.VersionAsStr]);
  LibVersion := pgDump.VersionAsInt;
end;

procedure TfrmMain.UpdateMRUList;
var
  OldPos: Integer;
begin
  OldPos := lbProfiles.Items.IndexOf(edProfileFile.Text);
  if OldPos > -1 then
    lbProfiles.Items.Move(OldPos, 0)
  else
    lbProfiles.Items.Insert(0, edProfileFile.Text);
end;

end.
