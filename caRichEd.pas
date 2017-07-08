unit caRichEd;

{$INCLUDE ca.inc}

{.$DEFINE RICHEDIT_VER_10}

interface

uses

  // Standard Delphi units;
  Windows,
  ActiveX,
  ComObj,
  CommCtrl,
  Messages,
  SysUtils,
  Classes,
  Controls,
  Forms,
  Graphics,
  StdCtrls,
  Dialogs,
  RichEdit,
  Menus,
  ComCtrls;

type

  TcaRichEditVersion = 1..3;

  TcaCharFormat2 = TCharFormat2A;

type
  TcaCustomRichEdit = class;

 //---------------------------------------------------------------------------
 // TcaTextAttributes
 //---------------------------------------------------------------------------

  TcaAttributeType = (atDefaultText, atSelected, atWord);
  TcaConsistentAttribute = (caBold, caColor, caFace, caItalic, caSize,
    caStrikeOut, caUnderline, caProtected, caOffset, caHidden, caLink,
    caBackColor, caDisabled, caWeight, caSubscript, caRevAuthor);
  TcaConsistentAttributes = set of TcaConsistentAttribute;
  TcaSubscriptStyle = (ssNone, ssSubscript, ssSuperscript);
  TcaUnderlineType = (utNone, utSolid, utWord, utDouble, utDotted, utWave);

  TcaTextAttributes = class(TPersistent)
  private
    RichEdit: TcaCustomRichEdit;
    FType: TcaAttributeType;
    procedure AssignFont(Font: TFont);
    procedure GetAttributes(var Format: TcaCharFormat2);
    function GetCharset: TFontCharset;
    procedure SetCharset(Value: TFontCharset);
    function GetSubscriptStyle: TcaSubscriptStyle;
    procedure SetSubscriptStyle(Value: TcaSubscriptStyle);
    function GetBackColor: TColor;
    function GetColor: TColor;
    function GetConsistentAttributes: TcaConsistentAttributes;
    function GetHeight: Integer;
    function GetHidden: Boolean;
    function GetDisabled: Boolean;
    function GetLink: Boolean;
    function GetName: TFontName;
    function GetOffset: Integer;
    function GetPitch: TFontPitch;
    function GetProtected: Boolean;
    function GetRevAuthorIndex: Byte;
    function GetSize: Integer;
    function GetStyle: TFontStyles;
    function GetUnderlineType: TcaUnderlineType;
    procedure SetAttributes(var Format: TcaCharFormat2);
    procedure SetBackColor(Value: TColor);
    procedure SetColor(Value: TColor);
    procedure SetDisabled(Value: Boolean);
    procedure SetHeight(Value: Integer);
    procedure SetHidden(Value: Boolean);
    procedure SetLink(Value: Boolean);
    procedure SetName(Value: TFontName);
    procedure SetOffset(Value: Integer);
    procedure SetPitch(Value: TFontPitch);
    procedure SetProtected(Value: Boolean);
    procedure SetRevAuthorIndex(Value: Byte);
    procedure SetSize(Value: Integer);
    procedure SetStyle(Value: TFontStyles);
    procedure SetUnderlineType(Value: TcaUnderlineType);
  protected
    procedure InitFormat(var Format: TcaCharFormat2);
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AOwner: TcaCustomRichEdit; AttributeType: TcaAttributeType);
    procedure Assign(Source: TPersistent); override;
    property Charset: TFontCharset read GetCharset write SetCharset;
    property BackColor: TColor read GetBackColor write SetBackColor;
    property Color: TColor read GetColor write SetColor;
    property ConsistentAttributes: TcaConsistentAttributes read GetConsistentAttributes;
    property Disabled: Boolean read GetDisabled write SetDisabled;
    property Hidden: Boolean read GetHidden write SetHidden;
    property Link: Boolean read GetLink write SetLink;
    property Name: TFontName read GetName write SetName;
    property Offset: Integer read GetOffset write SetOffset;
    property Pitch: TFontPitch read GetPitch write SetPitch;
    property Protected: Boolean read GetProtected write SetProtected;
    property RevAuthorIndex: Byte read GetRevAuthorIndex write SetRevAuthorIndex;
    property SubscriptStyle: TcaSubscriptStyle read GetSubscriptStyle write SetSubscriptStyle;
    property Size: Integer read GetSize write SetSize;
    property Style: TFontStyles read GetStyle write SetStyle;
    property Height: Integer read GetHeight write SetHeight;
    property UnderlineType: TcaUnderlineType read GetUnderlineType write SetUnderlineType;
  end;

 //---------------------------------------------------------------------------
 // TcaParaAttributes
 //---------------------------------------------------------------------------

  TcaNumbering = (nsNone, nsBullet, nsArabicNumbers, nsLoCaseLetter, nsUpCaseLetter, nsLoCaseRoman, nsUpCaseRoman);

  TcaNumberingStyle = (nsParenthesis, nsPeriod, nsEnclosed, nsSimple);

  TcaParaAlignment = (paLeftJustify, paRightJustify, paCenter, paJustify);

  TcaLineSpacingRule = (lsSingle, lsOneAndHalf, lsDouble, lsSpecifiedOrMore, lsSpecified, lsMultiple);

  TcaHeadingStyle = 0..9;

  TcaParaTableStyle = (tsNone, tsTableRow, tsTableCellEnd, tsTableCell);

  TcaParaAttributes = class(TPersistent)
  private
    RichEdit: TcaCustomRichEdit;
    procedure GetAttributes(var Paragraph: TParaFormat2);
    function GetAlignment: TcaParaAlignment;
    function GetFirstIndent: Longint;
    function GetHeadingStyle: TcaHeadingStyle;
    function GetLeftIndent: Longint;
    function GetRightIndent: Longint;
    function GetSpaceAfter: Longint;
    function GetSpaceBefore: Longint;
    function GetLineSpacing: Longint;
    function GetLineSpacingRule: TcaLineSpacingRule;
    function GetNumbering: TcaNumbering;
    function GetNumberingStyle: TcaNumberingStyle;
    function GetNumberingTab: Word;
    function GetTab(Index: Byte): Longint;
    function GetTabCount: Integer;
    function GetTableStyle: TcaParaTableStyle;
    procedure SetAlignment(Value: TcaParaAlignment);
    procedure SetAttributes(var Paragraph: TParaFormat2);
    procedure SetFirstIndent(Value: Longint);
    procedure SetHeadingStyle(Value: TcaHeadingStyle);
    procedure SetLeftIndent(Value: Longint);
    procedure SetRightIndent(Value: Longint);
    procedure SetSpaceAfter(Value: Longint);
    procedure SetSpaceBefore(Value: Longint);
    procedure SetLineSpacing(Value: Longint);
    procedure SetLineSpacingRule(Value: TcaLineSpacingRule);
    procedure SetNumbering(Value: TcaNumbering);
    procedure SetNumberingStyle(Value: TcaNumberingStyle);
    procedure SetNumberingTab(Value: Word);
    procedure SetTab(Index: Byte; Value: Longint);
    procedure SetTabCount(Value: Integer);
    procedure SetTableStyle(Value: TcaParaTableStyle);
  protected
    procedure InitPara(var Paragraph: TParaFormat2);
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AOwner: TcaCustomRichEdit);
    procedure Assign(Source: TPersistent); override;
    property Alignment: TcaParaAlignment read GetAlignment write SetAlignment;
    property FirstIndent: Longint read GetFirstIndent write SetFirstIndent;
    property HeadingStyle: TcaHeadingStyle read GetHeadingStyle write SetHeadingStyle;
    property LeftIndent: Longint read GetLeftIndent write SetLeftIndent;
    property LineSpacing: Longint read GetLineSpacing write SetLineSpacing;
    property LineSpacingRule: TcaLineSpacingRule read GetLineSpacingRule write SetLineSpacingRule;
    property Numbering: TcaNumbering read GetNumbering write SetNumbering;
    property NumberingStyle: TcaNumberingStyle read GetNumberingStyle write SetNumberingStyle;
    property NumberingTab: Word read GetNumberingTab write SetNumberingTab;
    property RightIndent: Longint read GetRightIndent write SetRightIndent;
    property SpaceAfter: Longint read GetSpaceAfter write SetSpaceAfter;
    property SpaceBefore: Longint read GetSpaceBefore write SetSpaceBefore;
    property Tab[Index: Byte]: Longint read GetTab write SetTab;
    property TabCount: Integer read GetTabCount write SetTabCount;
    property TableStyle: TcaParaTableStyle read GetTableStyle write SetTableStyle;
  end;

 //---------------------------------------------------------------------------
 // TcaOEMConversion
 //---------------------------------------------------------------------------

  TcaOEMConversion = class(TConversion)
  public
    function ConvertReadStream(Stream: TStream; Buffer: PChar; BufSize: Integer): Integer; override;
    function ConvertWriteStream(Stream: TStream; Buffer: PChar; BufSize: Integer): Integer; override;
  end;

 //---------------------------------------------------------------------------
 // TcaCustomRichEdit
 //---------------------------------------------------------------------------

  TcaUndoName = (unUnknown, unTyping, unDelete, unDragDrop, unCut, unPaste);

  TcaRichSearchType = (stWholeWord, stMatchCase, stBackward, stSetSelection);

  TcaRichSearchTypes = set of TcaRichSearchType;

  TcaRichSelection = (stText, stObject, stMultiChar, stMultiObject);

  TcaRichSelectionType = set of TcaRichSelection;

  TcaRichLangOption = (rlAutoKeyboard, rlAutoFont, rlImeCancelComplete, rlImeAlwaysSendNotify);

  TcaRichLangOptions = set of TcaRichLangOption;

  TcaRichStreamFormat = (sfDefault, sfRichText, sfPlainText);

  TcaRichStreamMode = (smSelection, smPlainRtf, smNoObjects, smUnicode);

  TcaRichStreamModes = set of TcaRichStreamMode;

  TcaRichEditURLClickEvent = procedure(Sender: TObject; const URLText: string; Button: TMouseButton) of object;

  TcaRichEditProtectChangeEx = procedure(Sender: TObject; const Message: TMessage; StartPos, EndPos: Integer; var AllowChange: Boolean) of object;

  TcaRichEditFindErrorEvent = procedure(Sender: TObject; const FindText: string) of object;

  TcaRichEditFindCloseEvent = procedure(Sender: TObject; Dialog: TFindDialog) of object;

 //---------------------------------------------------------------------------
 // TcaRichConversionFormat
 //---------------------------------------------------------------------------

  PcaRichConversionFormat = ^TcaRichConversionFormat;
  TcaRichConversionFormat = record
    ConversionClass: TConversionClass;
    Extension: string;
    PlainText: Boolean;
    Next: PcaRichConversionFormat;
  end;

  TcaCustomRichEdit = class(TCustomMemo)
  private
    FAllowInPlace: Boolean;
    FAllowObjects: Boolean;
    FAutoURLDetect: Boolean;
    FAutoVerbMenu: Boolean;
    FCallback: TObject;
    FClickBtn: TMouseButton;
    FClickRange: TCharRange;
    FDefAttributes: TcaTextAttributes;
    FDefaultConverter: TConversionClass;
    FFindDialog: TFindDialog;
    FHideScrollBars: Boolean;
    FHideSelection: Boolean;
    FLangOptions: TcaRichLangOptions;
    FLastFind: TFindDialog;
    FLinesUpdating: Boolean;
    FMemStream: TMemoryStream;
    FModified: Boolean;
    FOldParaAlignment: TcaParaAlignment;
    FOnCloseFindDialog: TcaRichEditFindCloseEvent;
    FOnProtectChange: TRichEditProtectChange;
    FOnProtectChangeEx: TcaRichEditProtectChangeEx;
    FOnResizeRequest: TRichEditResizeEvent;
    FOnSaveClipboard: TRichEditSaveClipboard;
    FOnSelChange: TNotifyEvent;
    FOnTextChanged: TNotifyEvent;
    FOnTextNotFound: TcaRichEditFindErrorEvent;
    FOnURLClick: TcaRichEditURLClickEvent;
    FPageRect: TRect;
    FParagraph: TcaParaAttributes;
    FPlainText: Boolean;
    FPopupVerbMenu: TPopupMenu;
    FReplaceDialog: TReplaceDialog;
    FRichEditOle: IUnknown;
    FRichEditStrings: TStrings;
    FScreenLogPixels: Integer;
    FSelAttributes: TcaTextAttributes;
    FSelectionBar: Boolean;
    FTitle: string;
    FUndoLimit: Integer;
    FWordAttributes: TcaTextAttributes;
    FWordSelection: Boolean;
    // Property methods
    function GetAutoURLDetect: Boolean;
    function GetCanFindNext: Boolean;
    function GetCanPaste: Boolean;
    function GetCanRedo: Boolean;
    function GetCanUndo: Boolean;
    function GetLangOptions: TcaRichLangOptions;
    function GetRedoName: TcaUndoName;
    function GetSelectionType: TcaRichSelectionType;
    function GetStreamFormat: TcaRichStreamFormat;
    function GetStreamMode: TcaRichStreamModes;
    function GetUndoName: TcaUndoName;
    function GetWordSelection: Boolean;
    procedure SetAllowObjects(Value: Boolean);
    procedure SetAutoURLDetect(Value: Boolean);
    procedure SetDefAttributes(Value: TcaTextAttributes);
    procedure SetHideScrollBars(Value: Boolean);
    procedure SetHideSelection(Value: Boolean);
    procedure SetLangOptions(Value: TcaRichLangOptions);
    procedure SetRichEditStrings(Value: TStrings);
    procedure SetSelAttributes(Value: TcaTextAttributes);
    procedure SetSelectionBar(Value: Boolean);
    procedure SetStreamFormat(Value: TcaRichStreamFormat);
    procedure SetStreamMode(Value: TcaRichStreamModes);
    procedure SetTitle(const Value: string);
    procedure SetUndoLimit(Value: Integer);
    procedure SetWordAttributes(Value: TcaTextAttributes);
    procedure SetWordSelection(Value: Boolean);
    // Private methods
    function FindEditText(Dialog: TFindDialog; AdjustPos, Events: Boolean): Boolean;
    procedure AdjustFindDialogPosition(Dialog: TFindDialog);
    procedure CloseObjects;
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMDocWindowActivate(var Message: TMessage); message CM_DOCWINDOWACTIVATE;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMUIDeactivate(var Message: TMessage); message CM_UIDEACTIVATE;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure EMReplaceSel(var Message: TMessage); message EM_REPLACESEL;
    procedure FindDialogClose(Sender: TObject);
    procedure FindDialogFind(Sender: TObject);
    procedure ObjectPropsClick(Sender: TObject);
    procedure PopupVerbClick(Sender: TObject);
    procedure ReplaceDialogReplace(Sender: TObject);
    procedure SetUIActive(Active: Boolean);
    procedure SetupFindDialog(Dialog: TFindDialog; const SearchStr, ReplaceStr: string);
    procedure UpdateHostNames;
    procedure UpdateTextModes(Plain: Boolean);
    procedure WMDestroy(var Msg: TWMDestroy); message WM_DESTROY;
    procedure WMMouseMove(var Message: TMessage); message WM_MOUSEMOVE;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMRButtonUp(var Message: TMessage); message WM_RBUTTONUP;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure WMSetFont(var Message: TWMSetFont); message WM_SETFONT;
  protected
    function GetPopupMenu: TPopupMenu; override;
    function GetSelLength: Integer; override;
    function GetSelStart: Integer; override;
    function GetSelText: string; override;
    function ProtectChange(const Message: TMessage; StartPos, EndPos: Integer): Boolean; dynamic;
    function SaveClipboard(NumObj, NumChars: Integer): Boolean; dynamic;
    procedure CloseFindDialog(Dialog: TFindDialog); virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure DoSetMaxLength(Value: Integer); override;
    procedure DoTextChanged; virtual;
    procedure RequestSize(const Rect: TRect); virtual;
    procedure SelectionChange; dynamic;
    procedure SetPlainText(Value: Boolean); virtual;
    procedure SetSelLength(Value: Integer); override;
    procedure SetSelStart(Value: Integer); override;
    procedure TextNotFound(Dialog: TFindDialog); virtual;
    procedure URLClick(const URLText: string; Button: TMouseButton); dynamic;
    // Properties
    property AllowInPlace: Boolean read FAllowInPlace write FAllowInPlace default True;
    property AllowObjects: Boolean read FAllowObjects write SetAllowObjects default True;
    property AutoURLDetect: Boolean read GetAutoURLDetect write SetAutoURLDetect default True;
    property AutoVerbMenu: Boolean read FAutoVerbMenu write FAutoVerbMenu default True;
    property HideScrollBars: Boolean read FHideScrollBars write SetHideScrollBars default True;
    property HideSelection: Boolean read FHideSelection write SetHideSelection default True;
    property LangOptions: TcaRichLangOptions read GetLangOptions write SetLangOptions default [rlAutoFont];
    property Lines: TStrings read FRichEditStrings write SetRichEditStrings;
    property OnCloseFindDialog: TcaRichEditFindCloseEvent read FOnCloseFindDialog write FOnCloseFindDialog;
    property OnProtectChange: TRichEditProtectChange read FOnProtectChange write FOnProtectChange; // obsolete
    property OnProtectChangeEx: TcaRichEditProtectChangeEx read FOnProtectChangeEx write FOnProtectChangeEx;
    property OnResizeRequest: TRichEditResizeEvent read FOnResizeRequest write FOnResizeRequest;
    property OnSaveClipboard: TRichEditSaveClipboard read FOnSaveClipboard write FOnSaveClipboard;
    property OnSelectionChange: TNotifyEvent read FOnSelChange write FOnSelChange;
    property OnTextNotFound: TcaRichEditFindErrorEvent read FOnTextNotFound write FOnTextNotFound;
    property OnTextChanged: TNotifyEvent read FOnTextChanged write FOnTextChanged;
    property OnURLClick: TcaRichEditURLClickEvent read FOnURLClick write FOnURLClick;
    property PlainText: Boolean read FPlainText write SetPlainText default False;
    property ScrollBars default ssBoth;
    property SelectionBar: Boolean read FSelectionBar write SetSelectionBar default True;
    property StreamFormat: TcaRichStreamFormat read GetStreamFormat write SetStreamFormat default sfDefault;
    property StreamMode: TcaRichStreamModes read GetStreamMode write SetStreamMode default [];
    property TabStop default True;
    property Title: string read FTitle write SetTitle;
    property UndoLimit: Integer read FUndoLimit write SetUndoLimit default 100;
    property WordSelection: Boolean read GetWordSelection write SetWordSelection default True;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class procedure RegisterConversionFormat(const AExtension: string; APlainText: Boolean;
      AConversionClass: TConversionClass);
    function FindDialog(const SearchStr: string): TFindDialog;
    function FindNext: Boolean;
    function FindText(const SearchStr: string; StartPos, Length: Integer; Options: TcaRichSearchTypes): Integer;
    function GetCaretPos: TPoint; override; { TODO : ? }
    function GetCharPos(CharIndex: Integer): TPoint;
    function GetLineIndex(LineNo: Integer): Integer;
    function GetLineLength(CharIndex: Integer): Integer;
    function GetSelection: TCharRange;
    function GetSelTextBuf(Buffer: PChar; BufSize: Integer): Integer; override;
    function GetTextRange(StartPos, EndPos: Longint): string;
    function InsertObjectDialog: Boolean;
    function LineFromChar(CharIndex: Integer): Integer;
    function ObjectPropertiesDialog: Boolean;
    function PasteSpecialDialog: Boolean;
    function ReplaceDialog(const SearchStr, ReplaceStr: string): TReplaceDialog;
    function WordAtCursor: string;
    procedure Clear; override;
    procedure ClearUndo;
    procedure Print(const Caption: string); virtual;
    procedure Redo;
    procedure SetSelection(StartPos, EndPos: Longint; ScrollCaret: Boolean);
    procedure StopGroupTyping;
    procedure Undo;
    // Properties
    property CanFindNext: Boolean read GetCanFindNext;
    property CanPaste: Boolean read GetCanPaste;
    property CanRedo: Boolean read GetCanRedo;
    property CanUndo: Boolean read GetCanUndo;
    property CaretPos: TPoint read GetCaretPos;
    property DefAttributes: TcaTextAttributes read FDefAttributes write SetDefAttributes;
    property DefaultConverter: TConversionClass read FDefaultConverter write FDefaultConverter;
    property PageRect: TRect read FPageRect write FPageRect;
    property Paragraph: TcaParaAttributes read FParagraph;
    property RedoName: TcaUndoName read GetRedoName;
    property SelAttributes: TcaTextAttributes read FSelAttributes write SetSelAttributes;
    property SelectionType: TcaRichSelectionType read GetSelectionType;
    property UndoName: TcaUndoName read GetUndoName;
    property WordAttributes: TcaTextAttributes read FWordAttributes write SetWordAttributes;
  end;

  TcaRichEdit = class(TcaCustomRichEdit)
  published
    property Align;
    property Alignment;
    property AllowInPlace;
    property AllowObjects;
    property Anchors;
    property AutoURLDetect;
    property AutoVerbMenu;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideScrollBars;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property LangOptions;
    property Lines;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PlainText;
    property PopupMenu;
    property ReadOnly;
    property ScrollBars;
    property SelectionBar;
    property ShowHint;
    property StreamFormat;
    property StreamMode;
    property TabOrder;
    property TabStop;
    property Title;
    property UndoLimit;
    property Visible;
    property WantReturns;
    property WantTabs;
    property WordSelection;
    property WordWrap;
    // Event properties
    property OnChange;
    property OnCloseFindDialog;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnProtectChange; // obsolete
    property OnProtectChangeEx;
    property OnResizeRequest;
    property OnSaveClipboard;
    property OnSelectionChange;
    property OnStartDock;
    property OnStartDrag;
    property OnTextChanged; 
    property OnTextNotFound;
    property OnURLClick;
  end;

var
  RichEditVersion: TcaRichEditVersion;

implementation

uses

  // Standard Delphi units
  Printers,
  ComStrs,
  OleConst,
  OleDlg,
  OleCtnrs,
  Math;

const

  RTFConversionFormat: TcaRichConversionFormat = (
    ConversionClass: TConversion;
    Extension: 'rtf';
    PlainText: False;
    Next: nil);

  TextConversionFormat: TcaRichConversionFormat = (
    ConversionClass: TConversion;
    Extension: 'txt';
    PlainText: True;
    Next: @RTFConversionFormat);

var
  ConversionFormatList: PcaRichConversionFormat = @TextConversionFormat;

const
  RichEdit10ModuleName = 'RICHED32.DLL';
  RichEdit20ModuleName = 'RICHED20.DLL';

const
  FT_DOWN = 1;

type
  PcaENLink = ^TENLink;
  PcaENOleOpFailed = ^TENOleOpFailed;
  TcaFindTextEx = TFindTextExA;

  TcaTextRangeA = record
    chrg: TCharRange;
    lpstrText: PAnsiChar;
  end;
  TcaTextRangeW = record
    chrg: TCharRange;
    lpstrText: PWideChar;
  end;
  TcaTextRange = TcaTextRangeA;

function ResStr(const Ident: string): string;
begin
  Result := Ident;
end;

 //---------------------------------------------------------------------------
 // TcaTextAttributes
 //---------------------------------------------------------------------------

const
  AttrFlags: array[TcaAttributeType] of Word = (0, SCF_SELECTION,
    SCF_WORD or SCF_SELECTION);

constructor TcaTextAttributes.Create(AOwner: TcaCustomRichEdit;
  AttributeType: TcaAttributeType);
begin
  inherited Create;
  RichEdit := AOwner;
  FType := AttributeType;
end;

procedure TcaTextAttributes.InitFormat(var Format: TcaCharFormat2);
begin
  FillChar(Format, SizeOf(Format), 0);
  if RichEditVersion >= 2 then
    Format.cbSize := SizeOf(Format)
  else
    Format.cbSize := SizeOf(TCharFormat);
end;

function TcaTextAttributes.GetConsistentAttributes: TcaConsistentAttributes;
var
  Format: TcaCharFormat2;
begin
  Result := [];
  if RichEdit.HandleAllocated and (FType <> atDefaultText) then
    begin
      InitFormat(Format);
      SendMessage(RichEdit.Handle, EM_GETCHARFORMAT,
        AttrFlags[FType], LPARAM(@Format));
      with Format do
        begin
          if (dwMask and CFM_BOLD) <> 0 then Include(Result, caBold);
          if (dwMask and CFM_COLOR) <> 0 then Include(Result, caColor);
          if (dwMask and CFM_FACE) <> 0 then Include(Result, caFace);
          if (dwMask and CFM_ITALIC) <> 0 then Include(Result, caItalic);
          if (dwMask and CFM_SIZE) <> 0 then Include(Result, caSize);
          if (dwMask and CFM_STRIKEOUT) <> 0 then Include(Result, caStrikeOut);
          if (dwMask and CFM_UNDERLINE) <> 0 then Include(Result, caUnderline);
          if (dwMask and CFM_PROTECTED) <> 0 then Include(Result, caProtected);
          if (dwMask and CFM_OFFSET) <> 0 then Include(Result, caOffset);
          if (dwMask and CFM_HIDDEN) <> 0 then Include(result, caHidden);
          if RichEditVersion >= 2 then
            begin
              if (dwMask and CFM_LINK) <> 0 then Include(Result, caLink);
              if (dwMask and CFM_BACKCOLOR) <> 0 then Include(Result, caBackColor);
              if (dwMask and CFM_DISABLED) <> 0 then Include(Result, caDisabled);
              if (dwMask and CFM_WEIGHT) <> 0 then Include(Result, caWeight);
              if (dwMask and CFM_SUBSCRIPT) <> 0 then Include(Result, caSubscript);
              if (dwMask and CFM_REVAUTHOR) <> 0 then Include(Result, caRevAuthor);
            end;
        end;
    end;
end;

procedure TcaTextAttributes.GetAttributes(var Format: TcaCharFormat2);
begin
  InitFormat(Format);
  if RichEdit.HandleAllocated then
    SendMessage(RichEdit.Handle, EM_GETCHARFORMAT, AttrFlags[FType],
      LPARAM(@Format));
end;

procedure TcaTextAttributes.SetAttributes(var Format: TcaCharFormat2);
begin
  if RichEdit.HandleAllocated then
    SendMessage(RichEdit.Handle, EM_SETCHARFORMAT, AttrFlags[FType],
      LPARAM(@Format));
end;

function TcaTextAttributes.GetCharset: TFontCharset;
var
  Format: TcaCharFormat2;
begin
  GetAttributes(Format);
  Result := Format.bCharset;
end;

procedure TcaTextAttributes.SetCharset(Value: TFontCharset);
var
  Format: TcaCharFormat2;
begin
  InitFormat(Format);
  with Format do
    begin
      dwMask := CFM_CHARSET;
      bCharSet := Value;
    end;
  SetAttributes(Format);
end;

function TcaTextAttributes.GetProtected: Boolean;
var
  Format: TcaCharFormat2;
begin
  GetAttributes(Format);
  with Format do
    Result := (dwEffects and CFE_PROTECTED) <> 0;
end;

procedure TcaTextAttributes.SetProtected(Value: Boolean);
var
  Format: TcaCharFormat2;
begin
  InitFormat(Format);
  with Format do
    begin
      dwMask := CFM_PROTECTED;
      if Value then dwEffects := CFE_PROTECTED;
    end;
  SetAttributes(Format);
end;

function TcaTextAttributes.GetLink: Boolean;
var
  Format: TcaCharFormat2;
begin
  Result := False;
  if RichEditVersion < 2 then Exit;
  GetAttributes(Format);
  with Format do
    Result := (dwEffects and CFE_LINK) <> 0;
end;

procedure TcaTextAttributes.SetLink(Value: Boolean);
var
  Format: TcaCharFormat2;
begin
  if RichEditVersion < 2 then Exit;
  InitFormat(Format);
  with Format do
    begin
      dwMask := CFM_LINK;
      if Value then dwEffects := CFE_LINK;
    end;
  SetAttributes(Format);
end;

function TcaTextAttributes.GetRevAuthorIndex: Byte;
var
  Format: TcaCharFormat2;
begin
  GetAttributes(Format);
  Result := Format.bRevAuthor;
end;

procedure TcaTextAttributes.SetRevAuthorIndex(Value: Byte);
var
  Format: TcaCharFormat2;
begin
  if RichEditVersion < 2 then Exit;
  InitFormat(Format);
  with Format do
    begin
      dwMask := CFM_REVAUTHOR;
      bRevAuthor := Value;
    end;
  SetAttributes(Format);
end;

function TcaTextAttributes.GetHidden: Boolean;
var
  Format: TcaCharFormat2;
begin
  Result := False;
  if RichEditVersion < 2 then Exit;
  GetAttributes(Format);
  Result := Format.dwEffects and CFE_HIDDEN <> 0;
end;

procedure TcaTextAttributes.SetHidden(Value: Boolean);
var
  Format: TcaCharFormat2;
begin
  if RichEditVersion < 2 then Exit;
  InitFormat(Format);
  with Format do
    begin
      dwMask := CFM_HIDDEN;
      if Value then dwEffects := CFE_HIDDEN;
    end;
  SetAttributes(Format);
end;

function TcaTextAttributes.GetDisabled: Boolean;
var
  Format: TcaCharFormat2;
begin
  Result := False;
  if RichEditVersion < 2 then Exit;
  GetAttributes(Format);
  Result := Format.dwEffects and CFE_DISABLED <> 0;
end;

procedure TcaTextAttributes.SetDisabled(Value: Boolean);
var
  Format: TcaCharFormat2;
begin
  if RichEditVersion < 2 then Exit;
  InitFormat(Format);
  with Format do
    begin
      dwMask := CFM_DISABLED;
      if Value then dwEffects := CFE_DISABLED;
    end;
  SetAttributes(Format);
end;

function TcaTextAttributes.GetColor: TColor;
var
  Format: TcaCharFormat2;
begin
  GetAttributes(Format);
  with Format do
    if (dwEffects and CFE_AUTOCOLOR) <> 0 then
      Result := clWindowText
    else
      Result := crTextColor;
end;

procedure TcaTextAttributes.SetColor(Value: TColor);
var
  Format: TcaCharFormat2;
begin
  InitFormat(Format);
  with Format do
    begin
      dwMask := CFM_COLOR;
      if (Value = clWindowText) or (Value = clDefault) then
        dwEffects := CFE_AUTOCOLOR
      else
        crTextColor := ColorToRGB(Value);
    end;
  SetAttributes(Format);
end;

function TcaTextAttributes.GetBackColor: TColor;
var
  Format: TcaCharFormat2;
begin
  if RichEditVersion < 2 then
    begin
      Result := clWindow;
      Exit;
    end;
  GetAttributes(Format);
  with Format do
    if (dwEffects and CFE_AUTOBACKCOLOR) <> 0 then
      Result := clWindow
    else
      Result := crBackColor;
end;

procedure TcaTextAttributes.SetBackColor(Value: TColor);
var
  Format: TcaCharFormat2;
begin
  if RichEditVersion < 2 then Exit;
  InitFormat(Format);
  with Format do
    begin
      dwMask := CFM_BACKCOLOR;
      if (Value = clWindow) or (Value = clDefault) then
        dwEffects := CFE_AUTOBACKCOLOR
      else
        crBackColor := ColorToRGB(Value);
    end;
  SetAttributes(Format);
end;

function TcaTextAttributes.GetName: TFontName;
var
  Format: TcaCharFormat2;
begin
  GetAttributes(Format);
  Result := Format.szFaceName;
end;

procedure TcaTextAttributes.SetName(Value: TFontName);
var
  Format: TcaCharFormat2;
begin
  InitFormat(Format);
  with Format do
    begin
      dwMask := CFM_FACE;
      StrPLCopy(szFaceName, Value, SizeOf(szFaceName));
    end;
  SetAttributes(Format);
end;

function TcaTextAttributes.GetStyle: TFontStyles;
var
  Format: TcaCharFormat2;
begin
  Result := [];
  GetAttributes(Format);
  with Format do
    begin
      if (dwEffects and CFE_BOLD) <> 0 then Include(Result, fsBold);
      if (dwEffects and CFE_ITALIC) <> 0 then Include(Result, fsItalic);
      if (dwEffects and CFE_UNDERLINE) <> 0 then Include(Result, fsUnderline);
      if (dwEffects and CFE_STRIKEOUT) <> 0 then Include(Result, fsStrikeOut);
    end;
end;

procedure TcaTextAttributes.SetStyle(Value: TFontStyles);
var
  Format: TcaCharFormat2;
begin
  InitFormat(Format);
  with Format do
    begin
      dwMask := CFM_BOLD or CFM_ITALIC or CFM_UNDERLINE or CFM_STRIKEOUT;
      if fsBold in Value then dwEffects := dwEffects or CFE_BOLD;
      if fsItalic in Value then dwEffects := dwEffects or CFE_ITALIC;
      if fsUnderline in Value then dwEffects := dwEffects or CFE_UNDERLINE;
      if fsStrikeOut in Value then dwEffects := dwEffects or CFE_STRIKEOUT;
    end;
  SetAttributes(Format);
end;

function TcaTextAttributes.GetUnderlineType: TcaUnderlineType;
var
  Format: TcaCharFormat2;
begin
  Result := utNone;
  if RichEditVersion < 2 then Exit;
  GetAttributes(Format);
  with Format do
    begin
      if (dwEffects and CFE_UNDERLINE <> 0) and
        (dwMask and CFM_UNDERLINETYPE = CFM_UNDERLINETYPE) then
        Result := TcaUnderlineType(bUnderlineType);
    end;
end;

procedure TcaTextAttributes.SetUnderlineType(Value: TcaUnderlineType);
var
  Format: TcaCharFormat2;
begin
  if RichEditVersion < 2 then Exit;
  InitFormat(Format);
  with Format do
    begin
      dwMask := CFM_UNDERLINETYPE or CFM_UNDERLINE;
      bUnderlineType := Ord(Value);
      if Value <> utNone then dwEffects := dwEffects or CFE_UNDERLINE;
    end;
  SetAttributes(Format);
end;

function TcaTextAttributes.GetOffset: Integer;
var
  Format: TcaCharFormat2;
begin
  GetAttributes(Format);
  Result := Format.yOffset div 20;
end;

procedure TcaTextAttributes.SetOffset(Value: Integer);
var
  Format: TcaCharFormat2;
begin
  InitFormat(Format);
  with Format do
    begin
      dwMask := DWORD(CFM_OFFSET);
      yOffset := Value * 20;
    end;
  SetAttributes(Format);
end;

function TcaTextAttributes.GetSize: Integer;
var
  Format: TcaCharFormat2;
begin
  GetAttributes(Format);
  Result := Format.yHeight div 20;
end;

procedure TcaTextAttributes.SetSize(Value: Integer);
var
  Format: TcaCharFormat2;
begin
  InitFormat(Format);
  with Format do
    begin
      dwMask := DWORD(CFM_SIZE);
      yHeight := Value * 20;
    end;
  SetAttributes(Format);
end;

function TcaTextAttributes.GetHeight: Integer;
begin
  Result := MulDiv(Size, RichEdit.FScreenLogPixels, 72);
end;

procedure TcaTextAttributes.SetHeight(Value: Integer);
begin
  Size := MulDiv(Value, 72, RichEdit.FScreenLogPixels);
end;

function TcaTextAttributes.GetPitch: TFontPitch;
var
  Format: TcaCharFormat2;
begin
  GetAttributes(Format);
  case (Format.bPitchAndFamily and $03) of
    DEFAULT_PITCH: Result := fpDefault;
    VARIABLE_PITCH: Result := fpVariable;
    FIXED_PITCH: Result := fpFixed;
  else
    Result := fpDefault;
  end;
end;

procedure TcaTextAttributes.SetPitch(Value: TFontPitch);
var
  Format: TcaCharFormat2;
begin
  InitFormat(Format);
  with Format do
    begin
      case Value of
        fpVariable: bPitchAndFamily := VARIABLE_PITCH;
        fpFixed: bPitchAndFamily := FIXED_PITCH;
      else
        bPitchAndFamily := DEFAULT_PITCH;
      end;
    end;
  SetAttributes(Format);
end;

function TcaTextAttributes.GetSubscriptStyle: TcaSubscriptStyle;
var
  Format: TcaCharFormat2;
begin
  Result := ssNone;
  if RichEditVersion < 2 then Exit;
  GetAttributes(Format);
  with Format do
    begin
      if (dwEffects and CFE_SUBSCRIPT) <> 0 then
        Result := ssSubscript
      else if (dwEffects and CFE_SUPERSCRIPT) <> 0 then
        Result := ssSuperscript;
    end;
end;

procedure TcaTextAttributes.SetSubscriptStyle(Value: TcaSubscriptStyle);
var
  Format: TcaCharFormat2;
begin
  if RichEditVersion < 2 then Exit;
  InitFormat(Format);
  with Format do
    begin
      dwMask := DWORD(CFM_SUBSCRIPT);
      case Value of
        ssSubscript: dwEffects := CFE_SUBSCRIPT;
        ssSuperscript: dwEffects := CFE_SUPERSCRIPT;
      end;
    end;
  SetAttributes(Format);
end;

procedure TcaTextAttributes.AssignFont(Font: TFont);
var
  LogFont: TLogFont;
  Format: TcaCharFormat2;
begin
  InitFormat(Format);
  with Format do
    begin
      case Font.Pitch of
        fpVariable: bPitchAndFamily := VARIABLE_PITCH;
        fpFixed: bPitchAndFamily := FIXED_PITCH;
      else
        bPitchAndFamily := DEFAULT_PITCH;
      end;
      dwMask := dwMask or CFM_SIZE or CFM_BOLD or CFM_ITALIC or
        CFM_UNDERLINE or CFM_STRIKEOUT or CFM_FACE or CFM_COLOR;
      yHeight := Font.Size * 20;
      if fsBold in Font.Style then dwEffects := dwEffects or CFE_BOLD;
      if fsItalic in Font.Style then dwEffects := dwEffects or CFE_ITALIC;
      if fsUnderline in Font.Style then dwEffects := dwEffects or CFE_UNDERLINE;
      if fsStrikeOut in Font.Style then dwEffects := dwEffects or CFE_STRIKEOUT;
      StrPLCopy(szFaceName, Font.Name, SizeOf(szFaceName));
      if (Font.Color = clWindowText) or (Font.Color = clDefault) then
        dwEffects := CFE_AUTOCOLOR
      else
        crTextColor := ColorToRGB(Font.Color);
      dwMask := dwMask or CFM_CHARSET;
      bCharSet := Font.Charset;
      if GetObject(Font.Handle, SizeOf(LogFont), @LogFont) <> 0 then
        begin
          dwMask := dwMask or DWORD(CFM_WEIGHT);
          wWeight := Word(LogFont.lfWeight);
        end;
    end;
  SetAttributes(Format);
end;

procedure TcaTextAttributes.Assign(Source: TPersistent);
var
  Format: TcaCharFormat2;
begin
  if Source is TFont then
    AssignFont(TFont(Source))
  else if Source is TTextAttributes then
    begin
      Name := TTextAttributes(Source).Name;
      Charset := TTextAttributes(Source).Charset;
      Style := TTextAttributes(Source).Style;
      Pitch := TTextAttributes(Source).Pitch;
      Color := TTextAttributes(Source).Color;
    end
  else if Source is TcaTextAttributes then
    begin
      TcaTextAttributes(Source).GetAttributes(Format);
      SetAttributes(Format);
    end
  else
    inherited Assign(Source);
end;

procedure TcaTextAttributes.AssignTo(Dest: TPersistent);
begin
  if Dest is TFont then
    begin
      TFont(Dest).Color := Color;
      TFont(Dest).Name := Name;
      TFont(Dest).Charset := Charset;
      TFont(Dest).Style := Style;
      TFont(Dest).Size := Size;
      TFont(Dest).Pitch := Pitch;
    end
  else if Dest is TTextAttributes then
    begin
      TTextAttributes(Dest).Color := Color;
      TTextAttributes(Dest).Name := Name;
      TTextAttributes(Dest).Charset := Charset;
      TTextAttributes(Dest).Style := Style;
      TTextAttributes(Dest).Pitch := Pitch;
    end
  else
    inherited AssignTo(Dest);
end;

 //---------------------------------------------------------------------------
 // TcaParaAttributes
 //---------------------------------------------------------------------------

constructor TcaParaAttributes.Create(AOwner: TcaCustomRichEdit);
begin
  inherited Create;
  RichEdit := AOwner;
end;

procedure TcaParaAttributes.InitPara(var Paragraph: TParaFormat2);
begin
  FillChar(Paragraph, SizeOf(Paragraph), 0);
  if RichEditVersion >= 2 then
    Paragraph.cbSize := SizeOf(Paragraph)
  else
    Paragraph.cbSize := SizeOf(TParaFormat);
end;

procedure TcaParaAttributes.GetAttributes(var Paragraph: TParaFormat2);
begin
  InitPara(Paragraph);
  if RichEdit.HandleAllocated then
    SendMessage(RichEdit.Handle, EM_GETPARAFORMAT, 0, LPARAM(@Paragraph));
end;

procedure TcaParaAttributes.SetAttributes(var Paragraph: TParaFormat2);
begin
  RichEdit.HandleNeeded;
  if RichEdit.HandleAllocated then
    begin
      if RichEdit.UseRightToLeftAlignment then
        if Paragraph.wAlignment = PFA_LEFT then
          Paragraph.wAlignment := PFA_RIGHT
        else if Paragraph.wAlignment = PFA_RIGHT then
          Paragraph.wAlignment := PFA_LEFT;
      SendMessage(RichEdit.Handle, EM_SETPARAFORMAT, 0, LPARAM(@Paragraph));
    end;
end;

function TcaParaAttributes.GetAlignment: TcaParaAlignment;
var
  Paragraph: TParaFormat2;
begin
  GetAttributes(Paragraph);
  Result := TcaParaAlignment(Paragraph.wAlignment - 1);
end;

procedure TcaParaAttributes.SetAlignment(Value: TcaParaAlignment);
var
  Paragraph: TParaFormat2;
begin
  InitPara(Paragraph);
  with Paragraph do
    begin
      dwMask := PFM_ALIGNMENT;
      wAlignment := Ord(Value) + 1;
    end;
  SetAttributes(Paragraph);
end;

function TcaParaAttributes.GetNumbering: TcaNumbering;
var
  Paragraph: TParaFormat2;
begin
  GetAttributes(Paragraph);
  Result := TcaNumbering(Paragraph.wNumbering);
  if RichEditVersion = 1 then
    if Result <> nsNone then Result := nsBullet;
end;

procedure TcaParaAttributes.SetNumbering(Value: TcaNumbering);
var
  Paragraph: TParaFormat2;
begin
  if RichEditVersion = 1 then
    if Value <> nsNone then Value := TcaNumbering(PFN_BULLET);
  case Value of
    nsNone: LeftIndent := 0;
  else if LeftIndent < 10 then
    LeftIndent := 10;
  end;
  InitPara(Paragraph);
  with Paragraph do
    begin
      dwMask := PFM_NUMBERING;
      wNumbering := Ord(Value);
    end;
  SetAttributes(Paragraph);
end;

function TcaParaAttributes.GetNumberingStyle: TcaNumberingStyle;
var
  Paragraph: TParaFormat2;
begin
  if RichEditVersion < 2 then
    Result := nsSimple
  else
    begin
      GetAttributes(Paragraph);
      Result := TcaNumberingStyle(Paragraph.wNumberingStyle);
    end;
end;

procedure TcaParaAttributes.SetNumberingStyle(Value: TcaNumberingStyle);
var
  Paragraph: TParaFormat2;
begin
  if RichEditVersion < 2 then Exit;
  InitPara(Paragraph);
  with Paragraph do
    begin
      dwMask := PFM_NUMBERINGSTYLE;
      wNumberingStyle := Ord(Value);
    end;
  SetAttributes(Paragraph);
end;

function TcaParaAttributes.GetNumberingTab: Word;
var
  Paragraph: TParaFormat2;
begin
  GetAttributes(Paragraph);
  Result := Paragraph.wNumberingTab div 20;
end;

procedure TcaParaAttributes.SetNumberingTab(Value: Word);
var
  Paragraph: TParaFormat2;
begin
  if RichEditVersion < 2 then Exit;
  InitPara(Paragraph);
  with Paragraph do
    begin
      dwMask := PFM_NUMBERINGTAB;
      wNumberingTab := Value * 20;
    end;
  SetAttributes(Paragraph);
end;

function TcaParaAttributes.GetFirstIndent: Longint;
var
  Paragraph: TParaFormat2;
begin
  GetAttributes(Paragraph);
  Result := Paragraph.dxStartIndent div 20;
end;

procedure TcaParaAttributes.SetFirstIndent(Value: Longint);
var
  Paragraph: TParaFormat2;
begin
  InitPara(Paragraph);
  with Paragraph do
    begin
      dwMask := PFM_STARTINDENT;
      dxStartIndent := Value * 20;
    end;
  SetAttributes(Paragraph);
end;

function TcaParaAttributes.GetHeadingStyle: TcaHeadingStyle;
var
  Paragraph: TParaFormat2;
begin
  if RichEditVersion < 3 then
    Result := 0
  else
    begin
      GetAttributes(Paragraph);
      Result := Paragraph.sStyle;
    end;
end;

procedure TcaParaAttributes.SetHeadingStyle(Value: TcaHeadingStyle);
var
  Paragraph: TParaFormat2;
begin
  if RichEditVersion < 3 then Exit;
  InitPara(Paragraph);
  with Paragraph do
    begin
      dwMask := PFM_STYLE;
      sStyle := Value;
    end;
  SetAttributes(Paragraph);
end;

function TcaParaAttributes.GetLeftIndent: Longint;
var
  Paragraph: TParaFormat2;
begin
  GetAttributes(Paragraph);
  Result := Paragraph.dxOffset div 20;
end;

procedure TcaParaAttributes.SetLeftIndent(Value: Longint);
var
  Paragraph: TParaFormat2;
begin
  InitPara(Paragraph);
  with Paragraph do
    begin
      dwMask := PFM_OFFSET;
      dxOffset := Value * 20;
    end;
  SetAttributes(Paragraph);
end;

function TcaParaAttributes.GetRightIndent: Longint;
var
  Paragraph: TParaFormat2;
begin
  GetAttributes(Paragraph);
  Result := Paragraph.dxRightIndent div 20;
end;

procedure TcaParaAttributes.SetRightIndent(Value: Longint);
var
  Paragraph: TParaFormat2;
begin
  InitPara(Paragraph);
  with Paragraph do
    begin
      dwMask := PFM_RIGHTINDENT;
      dxRightIndent := Value * 20;
    end;
  SetAttributes(Paragraph);
end;

function TcaParaAttributes.GetSpaceAfter: Longint;
var
  Paragraph: TParaFormat2;
begin
  GetAttributes(Paragraph);
  Result := Paragraph.dySpaceAfter div 20;
end;

procedure TcaParaAttributes.SetSpaceAfter(Value: Longint);
var
  Paragraph: TParaFormat2;
begin
  if RichEditVersion < 2 then Exit;
  InitPara(Paragraph);
  with Paragraph do
    begin
      dwMask := PFM_SPACEAFTER;
      dySpaceAfter := Value * 20;
    end;
  SetAttributes(Paragraph);
end;

function TcaParaAttributes.GetSpaceBefore: Longint;
var
  Paragraph: TParaFormat2;
begin
  GetAttributes(Paragraph);
  Result := Paragraph.dySpaceBefore div 20;
end;

procedure TcaParaAttributes.SetSpaceBefore(Value: Longint);
var
  Paragraph: TParaFormat2;
begin
  if RichEditVersion < 2 then Exit;
  InitPara(Paragraph);
  with Paragraph do
    begin
      dwMask := PFM_SPACEBEFORE;
      dySpaceBefore := Value * 20;
    end;
  SetAttributes(Paragraph);
end;

function TcaParaAttributes.GetLineSpacing: Longint;
var
  Paragraph: TParaFormat2;
begin
  GetAttributes(Paragraph);
  Result := Paragraph.dyLineSpacing div 20;
end;

procedure TcaParaAttributes.SetLineSpacing(Value: Longint);
var
  Paragraph: TParaFormat2;
begin
  if RichEditVersion < 2 then Exit;
  GetAttributes(Paragraph);
  with Paragraph do
    begin
      dwMask := PFM_LINESPACING;
      dyLineSpacing := Value * 20;
    end;
  SetAttributes(Paragraph);
end;

function TcaParaAttributes.GetLineSpacingRule: TcaLineSpacingRule;
var
  Paragraph: TParaFormat2;
begin
  GetAttributes(Paragraph);
  Result := TcaLineSpacingRule(Paragraph.bLineSpacingRule);
end;

procedure TcaParaAttributes.SetLineSpacingRule(Value: TcaLineSpacingRule);
var
  Paragraph: TParaFormat2;
begin
  if RichEditVersion < 2 then Exit;
  GetAttributes(Paragraph);
  with Paragraph do
    begin
      dwMask := PFM_LINESPACING;
      bLineSpacingRule := Ord(Value);
    end;
  SetAttributes(Paragraph);
end;

function TcaParaAttributes.GetTab(Index: Byte): Longint;
var
  Paragraph: TParaFormat2;
begin
  GetAttributes(Paragraph);
  Result := Paragraph.rgxTabs[Index] div 20;
end;

procedure TcaParaAttributes.SetTab(Index: Byte; Value: Longint);
var
  Paragraph: TParaFormat2;
begin
  GetAttributes(Paragraph);
  with Paragraph do
    begin
      rgxTabs[Index] := Value * 20;
      dwMask := PFM_TABSTOPS;
      if cTabCount < Index then cTabCount := Index;
      SetAttributes(Paragraph);
    end;
end;

function TcaParaAttributes.GetTabCount: Integer;
var
  Paragraph: TParaFormat2;
begin
  GetAttributes(Paragraph);
  Result := Paragraph.cTabCount;
end;

procedure TcaParaAttributes.SetTabCount(Value: Integer);
var
  Paragraph: TParaFormat2;
begin
  GetAttributes(Paragraph);
  with Paragraph do
    begin
      dwMask := PFM_TABSTOPS;
      cTabCount := Value;
      SetAttributes(Paragraph);
    end;
end;

function TcaParaAttributes.GetTableStyle: TcaParaTableStyle;
var
  Paragraph: TParaFormat2;
begin
  Result := tsNone;
  if RichEditVersion < 2 then Exit;
  GetAttributes(Paragraph);
  with Paragraph do
    begin
      if (wReserved and PFE_TABLEROW) <> 0 then
        Result := tsTableRow
      else if (wReserved and PFE_TABLECELLEND) <> 0 then
        Result := tsTableCellEnd
      else if (wReserved and PFE_TABLECELL) <> 0 then
        Result := tsTableCell;
    end;
end;

procedure TcaParaAttributes.SetTableStyle(Value: TcaParaTableStyle);
var
  Paragraph: TParaFormat2;
begin
  if RichEditVersion < 2 then Exit;
  InitPara(Paragraph);
  with Paragraph do
    begin
      dwMask := PFM_TABLE;
      case Value of
        tsTableRow: wReserved := PFE_TABLEROW;
        tsTableCellEnd: wReserved := PFE_TABLECELLEND;
        tsTableCell: wReserved := PFE_TABLECELL;
      end;
    end;
  SetAttributes(Paragraph);
end;

procedure TcaParaAttributes.AssignTo(Dest: TPersistent);
var
  I: Integer;
begin
  if Dest is TParaAttributes then
    begin
      with TParaAttributes(Dest) do
        begin
          if Self.Alignment = paJustify then
            Alignment := taLeftJustify
          else
            Alignment := TAlignment(Self.Alignment);
          FirstIndent := Self.FirstIndent;
          LeftIndent := Self.LeftIndent;
          RightIndent := Self.RightIndent;
          if Self.Numbering <> nsNone then
            Numbering := TNumberingStyle(nsBullet)
          else
            Numbering := TNumberingStyle(nsNone);
          for I := 0 to MAX_TAB_STOPS - 1 do
            Tab[I] := Self.Tab[I];
        end;
    end
  else
    inherited AssignTo(Dest);
end;

procedure TcaParaAttributes.Assign(Source: TPersistent);
var
  I: Integer;
  Paragraph: TParaFormat2;
begin
  if Source is TParaAttributes then
    begin
      Alignment := TcaParaAlignment(TParaAttributes(Source).Alignment);
      FirstIndent := TParaAttributes(Source).FirstIndent;
      LeftIndent := TParaAttributes(Source).LeftIndent;
      RightIndent := TParaAttributes(Source).RightIndent;
      Numbering := TcaNumbering(TParaAttributes(Source).Numbering);
      for I := 0 to MAX_TAB_STOPS - 1 do
        Tab[I] := TParaAttributes(Source).Tab[I];
    end
  else if Source is TcaParaAttributes then
    begin
      TcaParaAttributes(Source).GetAttributes(Paragraph);
      SetAttributes(Paragraph);
    end
  else
    inherited Assign(Source);
end;

 //---------------------------------------------------------------------------
 // OLE utility routines
 //---------------------------------------------------------------------------

function WStrLen(Str: PWideChar): Integer;
begin
  Result := 0;
  while Str[Result] <> #0 do
    Inc(Result);
end;

procedure ReleaseObject(var Obj);
begin
  if IUnknown(Obj) <> nil then
    begin
      IUnknown(Obj) := nil;
    end;
end;

procedure CreateStorage(var Storage: IStorage);
var
  LockBytes: ILockBytes;
begin
  OleCheck(CreateILockBytesOnHGlobal(0, True, LockBytes));
  try
    OleCheck(StgCreateDocfileOnILockBytes(LockBytes, STGM_READWRITE
      or STGM_SHARE_EXCLUSIVE or STGM_CREATE, 0, Storage));
  finally
    ReleaseObject(LockBytes);
  end;
end;

procedure DestroyMetaPict(MetaPict: HGlobal);
begin
  if MetaPict <> 0 then
    begin
      DeleteMetaFile(PMetaFilePict(GlobalLock(MetaPict))^.hMF);
      GlobalUnlock(MetaPict);
      GlobalFree(MetaPict);
    end;
end;

function OleSetDrawAspect(OleObject: IOleObject; Iconic: Boolean;
  IconMetaPict: HGlobal; var DrawAspect: Longint): HResult;
var
  OleCache: IOleCache;
  EnumStatData: IEnumStatData;
  OldAspect, AdviseFlags, Connection: Longint;
  TempMetaPict: HGlobal;
  FormatEtc: TFormatEtc;
  Medium: TStgMedium;
  ClassID: TCLSID;
  StatData: TStatData;
begin
  Result := S_OK;
  OldAspect := DrawAspect;
  if Iconic then
    begin
      DrawAspect := DVASPECT_ICON;
      AdviseFlags := ADVF_NODATA;
    end
  else
    begin
      DrawAspect := DVASPECT_CONTENT;
      AdviseFlags := ADVF_PRIMEFIRST;
    end;
  if (DrawAspect <> OldAspect) or (DrawAspect = DVASPECT_ICON) then
    begin
      Result := OleObject.QueryInterface(IOleCache, OleCache);
      if Succeeded(Result) then
      try
        if DrawAspect <> OldAspect then
          begin
            // Setup new cache with the new aspect
            FillChar(FormatEtc, SizeOf(FormatEtc), 0);
            FormatEtc.dwAspect := DrawAspect;
            FormatEtc.lIndex := -1;
            Result := OleCache.Cache(FormatEtc, AdviseFlags, Connection);
          end;
        if Succeeded(Result) and (DrawAspect = DVASPECT_ICON) then
          begin
            TempMetaPict := 0;
            if IconMetaPict = 0 then
              begin
                if Succeeded(OleObject.GetUserClassID(ClassID)) then
                  begin
                    TempMetaPict := OleGetIconOfClass(ClassID, nil, True);
                    IconMetaPict := TempMetaPict;
                  end;
              end;
            try
              FormatEtc.cfFormat := CF_METAFILEPICT;
              FormatEtc.ptd := nil;
              FormatEtc.dwAspect := DVASPECT_ICON;
              FormatEtc.lIndex := -1;
              FormatEtc.tymed := TYMED_MFPICT;
              Medium.tymed := TYMED_MFPICT;
              Medium.hMetaFilePict := IconMetaPict;
              Medium.unkForRelease := nil;
              Result := OleCache.SetData(FormatEtc, Medium, False);
            finally
              DestroyMetaPict(TempMetaPict);
            end;
          end;
        if Succeeded(Result) and (DrawAspect <> OldAspect) then
          begin
            // remove any existing caches that are set up for the old display aspect
            OleCache.EnumCache(EnumStatData);
            if EnumStatData <> nil then
            try
              while EnumStatData.Next(1, StatData, nil) = 0 do
                if StatData.formatetc.dwAspect = OldAspect then
                  OleCache.Uncache(StatData.dwConnection);
            finally
              ReleaseObject(EnumStatData);
            end;
          end;
      finally
        ReleaseObject(OleCache);
      end;
      if Succeeded(Result) and (DrawAspect <> DVASPECT_ICON) then
        OleObject.Update;
    end;
end;

function GetIconMetaPict(OleObject: IOleObject; DrawAspect: Longint): HGlobal;
var
  DataObject: IDataObject;
  FormatEtc: TFormatEtc;
  Medium: TStgMedium;
  ClassID: TCLSID;
begin
  Result := 0;
  if DrawAspect = DVASPECT_ICON then
    begin
      OleObject.QueryInterface(IDataObject, DataObject);
      if DataObject <> nil then
        begin
          FormatEtc.cfFormat := CF_METAFILEPICT;
          FormatEtc.ptd := nil;
          FormatEtc.dwAspect := DVASPECT_ICON;
          FormatEtc.lIndex := -1;
          FormatEtc.tymed := TYMED_MFPICT;
          if Succeeded(DataObject.GetData(FormatEtc, Medium)) then
            Result := Medium.hMetaFilePict;
          ReleaseObject(DataObject);
        end;
    end;
  if Result = 0 then
    begin
      OleCheck(OleObject.GetUserClassID(ClassID));
      Result := OleGetIconOfClass(ClassID, nil, True);
    end;
end;

 // Return the first piece of a moniker

function OleStdGetFirstMoniker(Moniker: IMoniker): IMoniker;
var
  Mksys: Longint;
  EnumMoniker: IEnumMoniker;
begin
  Result := nil;
  if Moniker <> nil then
    begin
      if (Moniker.IsSystemMoniker(Mksys) = 0) and
        (Mksys = MKSYS_GENERICCOMPOSITE) then
        begin
          if Moniker.Enum(True, EnumMoniker) <> 0 then Exit;
          EnumMoniker.Next(1, Result, nil);
          ReleaseObject(EnumMoniker);
        end
      else
        begin
          Result := Moniker;
        end;
    end;
end;

 // Return length of file moniker piece of the given moniker

function OleStdGetLenFilePrefixOfMoniker(Moniker: IMoniker): Integer;
var
  MkFirst: IMoniker;
  BindCtx: IBindCtx;
  Mksys: Longint;
  P: PWideChar;
begin
  Result := 0;
  if Moniker <> nil then
    begin
      MkFirst := OleStdGetFirstMoniker(Moniker);
      if MkFirst <> nil then
        begin
          if (MkFirst.IsSystemMoniker(Mksys) = 0) and
            (Mksys = MKSYS_FILEMONIKER) then
            begin
              if CreateBindCtx(0, BindCtx) = 0 then
                begin
                  if (MkFirst.GetDisplayName(BindCtx, nil, P) = 0) and (P <> nil) then
                    begin
                      Result := WStrLen(P);
                      CoTaskMemFree(P);
                    end;
                  ReleaseObject(BindCtx);
                end;
            end;
          ReleaseObject(MkFirst);
        end;
    end;
end;

function CoAllocCStr(const S: string): PChar;
begin
  Result := StrCopy(CoTaskMemAlloc(Length(S) + 1), PChar(S));
end;

function WStrToString(P: PWideChar): string;
begin
  Result := '';
  if P <> nil then
    begin
      Result := WideCharToString(P);
      CoTaskMemFree(P);
    end;
end;

function GetFullNameStr(OleObject: IOleObject): string;
var
  P: PWideChar;
begin
  OleObject.GetUserType(USERCLASSTYPE_FULL, P);
  Result := WStrToString(P);
end;

function GetShortNameStr(OleObject: IOleObject): string;
var
  P: PWideChar;
begin
  OleObject.GetUserType(USERCLASSTYPE_SHORT, P);
  Result := WStrToString(P);
end;

function GetDisplayNameStr(OleLink: IOleLink): string;
var
  P: PWideChar;
begin
  OleLink.GetSourceDisplayName(P);
  Result := WStrToString(P);
end;

function GetVCLFrameForm(Form: TCustomForm): IVCLFrameForm;
begin
  if Form.OleFormObject = nil then TOleForm.Create(Form);
  Result := Form.OleFormObject as IVCLFrameForm;
end;

function IsFormMDIChild(Form: TCustomForm): Boolean;
begin
  Result := (Form is TForm) and (TForm(Form).FormStyle = fsMDIChild);
end;

 // Clipboard formats

var
  CFEmbeddedObject: Integer;
  CFLinkSource: Integer;
  CFRtf: Integer;
  CFRtfNoObjs: Integer;

const
  CF_EMBEDDEDOBJECT = 'Embedded Object';
  CF_LINKSOURCE = 'Link Source';

 //---------------------------------------------------------------------------
 // OLE Extensions to the Rich Text Editor
 // Converted from RICHOLE.H
 //---------------------------------------------------------------------------

 //---------------------------------------------------------------------------
 // Structure passed to GetObject and InsertObject
 //---------------------------------------------------------------------------

type
  _caReObject = record
    cbStruct: DWORD;            // Size of structure
    cp: ULONG;                  // Character position of object
    clsid: TCLSID;              // Class ID of object
    poleobj: IOleObject;        // OLE object interface
    pstg: IStorage;             // Associated storage interface
    polesite: IOleClientSite;   // Associated client site interface
    sizel: TSize;               // Size of object (may be 0,0)
    dvAspect: Longint;          // Display aspect to use
    dwFlags: DWORD;             // Object status flags
    dwUser: DWORD;              // Dword for user's use
  end;
  TcaReObject = _caReObject;

const

 // Flags to specify which interfaces should be returned in the structure above

  REO_GETOBJ_NO_INTERFACES = $00000000;
  REO_GETOBJ_POLEOBJ = $00000001;
  REO_GETOBJ_PSTG = $00000002;
  REO_GETOBJ_POLESITE = $00000004;
  REO_GETOBJ_ALL_INTERFACES = $00000007;

 // Place object at selection

  REO_CP_SELECTION = ULONG(-1);

 // Use character position to specify object instead of index

  REO_IOB_SELECTION = ULONG(-1);
  REO_IOB_USE_CP = ULONG(-2);

 // Object flags

  REO_NULL = $00000000;             // No flags
  REO_READWRITEMASK = $0000003F;    // Mask out RO bits
  REO_DONTNEEDPALETTE = $00000020;  // Object doesn't need palette
  REO_BLANK = $00000010;            // Object is blank
  REO_DYNAMICSIZE = $00000008;      // Object defines size always
  REO_INVERTEDSELECT = $00000004;   // Object drawn all inverted if sel
  REO_BELOWBASELINE = $00000002;    // Object sits below the baseline
  REO_RESIZABLE = $00000001;        // Object may be resized
  REO_LINK = $80000000;             // Object is a link (RO)
  REO_STATIC = $40000000;           // Object is static (RO)
  REO_SELECTED = $08000000;         // Object selected (RO)
  REO_OPEN = $04000000;             // Object open in its server (RO)
  REO_INPLACEACTIVE = $02000000;    // Object in place active (RO)
  REO_HILITED = $01000000;          // Object is to be hilited (RO)
  REO_LINKAVAILABLE = $00800000;    // Link believed available (RO)
  REO_GETMETAFILE = $00400000;      // Object requires metafile (RO)
  REO_SPEEDWAGON = $FFFFFFFF;       // Sorry, couldn't resist <g>

 // Flags for IRichEditOle.GetClipboardData,
 // IcaRichEditOleCallback.GetClipboardData and
 // IcaRichEditOleCallback.QueryAcceptData

  RECO_PASTE = $00000000;           // paste from clipboard
  RECO_DROP = $00000001;            // drop
  RECO_COPY = $00000002;            // copy to the clipboard
  RECO_CUT = $00000003;             // cut to the clipboard
  RECO_DRAG = $00000004;            // drag

 // RichEdit GUIDs

//  IID_IcaRichEditOle: TGUID = (
//    D1: $00020D00; D2: $0000; D3: $0000; D4: ($C0, $00, $00, $00, $00, $00, $00, $46));
//  IID_IcaRichEditOleCallback: TGUID = (
//    D1: $00020D03; D2: $0000; D3: $0000; D4: ($C0, $00, $00, $00, $00, $00, $00, $46));

type

 //---------------------------------------------------------------------------
 // IcaRichEditOle
 //
 // Purpose:
 // Interface used by the client of RichEdit to perform OLE-related operations.
 // The methods herein may just want to be regular Windows messages.
 //---------------------------------------------------------------------------

  IcaRichEditOle = interface(IUnknown)
    ['{00020d00-0000-0000-c000-000000000046}']
    function GetClientSite(out clientSite: IOleClientSite): HResult; stdcall;
    function GetObjectCount: HResult; stdcall;
    function GetLinkCount: HResult; stdcall;
    function GetObject(iob: Longint; out reobject: TcaReObject; dwFlags: DWORD): HResult; stdcall;
    function InsertObject(var reobject: TcaReObject): HResult; stdcall;
    function ConvertObject(iob: Longint; rclsidNew: TIID; lpstrUserTypeNew: LPCSTR): HResult; stdcall;
    function ActivateAs(rclsid: TIID; rclsidAs: TIID): HResult; stdcall;
    function SetHostNames(lpstrContainerApp: LPCSTR; lpstrContainerObj: LPCSTR): HResult; stdcall;
    function SetLinkAvailable(iob: Longint; fAvailable: BOOL): HResult; stdcall;
    function SetDvaspect(iob: Longint; dvaspect: DWORD): HResult; stdcall;
    function HandsOffStorage(iob: Longint): HResult; stdcall;
    function SaveCompleted(iob: Longint; const stg: IStorage): HResult; stdcall;
    function InPlaceDeactivate: HResult; stdcall;
    function ContextSensitiveHelp(fEnterMode: BOOL): HResult; stdcall;
    function GetClipboardData(var chrg: TCharRange; reco: DWORD; out dataobj: IDataObject): HResult; stdcall;
    function ImportDataObject(dataobj: IDataObject; cf: TClipFormat; hMetaPict: HGLOBAL): HResult; stdcall;
  end;

 //---------------------------------------------------------------------------
 // IcaRichEditOleCallback
 //
 // Purpose:
 // Interface used by the RichEdit to get OLE-related stuff from the
 // application using RichEdit.
 //---------------------------------------------------------------------------

  IcaRichEditOleCallback = interface(IUnknown)
    ['{00020d03-0000-0000-c000-000000000046}']
    function GetNewStorage(out stg: IStorage): HResult; stdcall;
    function GetInPlaceContext(out Frame: IOleInPlaceFrame; out Doc: IOleInPlaceUIWindow; lpFrameInfo: POleInPlaceFrameInfo): HResult; stdcall;
    function ShowContainerUI(fShow: BOOL): HResult; stdcall;
    function QueryInsertObject(const clsid: TCLSID; const stg: IStorage; cp: Longint): HResult; stdcall;
    function DeleteObject(const oleobj: IOleObject): HResult; stdcall;
    function QueryAcceptData(const dataobj: IDataObject; var cfFormat: TClipFormat; reco: DWORD; fReally: BOOL; hMetaPict: HGLOBAL): HResult; stdcall;
    function ContextSensitiveHelp(fEnterMode: BOOL): HResult; stdcall;
    function GetClipboardData(const chrg: TCharRange; reco: DWORD; out dataobj: IDataObject): HResult; stdcall;
    function GetDragDropEffect(fDrag: BOOL; grfKeyState: DWORD; var dwEffect: DWORD): HResult; stdcall;
    function GetContextMenu(seltype: Word; const oleobj: IOleObject; const chrg: TCharRange; out menu: HMENU): HResult; stdcall;
  end;

 //---------------------------------------------------------------------------
 // TcaRichEditOleCallback
 //---------------------------------------------------------------------------

  TcaRichEditOleCallback = class(TObject, IUnknown, IcaRichEditOleCallback)
  private
    FDocForm: IVCLFrameForm;
    FFrameForm: IVCLFrameForm;
    FAccelTable: HAccel;
    FAccelCount: Integer;
    FAutoScroll: Boolean;
    procedure CreateAccelTable;
    procedure DestroyAccelTable;
    procedure AssignFrame;
  private
    FRefCount: Longint;
    FRichEdit: TcaCustomRichEdit;
  public
    constructor Create(RichEdit: TcaCustomRichEdit);
    destructor Destroy; override;
    function QueryInterface(const iid: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Longint; stdcall;
    function _Release: Longint; stdcall;
    function GetNewStorage(out stg: IStorage): HResult; stdcall;
    function GetInPlaceContext(out Frame: IOleInPlaceFrame; out Doc: IOleInPlaceUIWindow; lpFrameInfo: POleInPlaceFrameInfo): HResult; stdcall;
    function GetClipboardData(const chrg: TCharRange; reco: DWORD; out dataobj: IDataObject): HResult; stdcall;
    function GetContextMenu(seltype: Word; const oleobj: IOleObject; const chrg: TCharRange; out menu: HMENU): HResult; stdcall;
    function ShowContainerUI(fShow: BOOL): HResult; stdcall;
    function QueryInsertObject(const clsid: TCLSID; const stg: IStorage; cp: Longint): HResult; stdcall;
    function DeleteObject(const oleobj: IOleObject): HResult; stdcall;
    function QueryAcceptData(const dataobj: IDataObject; var cfFormat: TClipFormat; reco: DWORD; fReally: BOOL; hMetaPict: HGLOBAL): HResult; stdcall;
    function ContextSensitiveHelp(fEnterMode: BOOL): HResult; stdcall;
    function GetDragDropEffect(fDrag: BOOL; grfKeyState: DWORD; var dwEffect: DWORD): HResult; stdcall;
  end;

constructor TcaRichEditOleCallback.Create(RichEdit: TcaCustomRichEdit);
begin
  inherited Create;
  FRichEdit := RichEdit;
end;

destructor TcaRichEditOleCallback.Destroy;
begin
  DestroyAccelTable;
  FFrameForm := nil;
  FDocForm := nil;
  inherited Destroy;
end;

function TcaRichEditOleCallback.QueryInterface(const iid: TGUID; out Obj): HResult;
begin
  if GetInterface(iid, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TcaRichEditOleCallback._AddRef: Longint;
begin
  Inc(FRefCount);
  Result := FRefCount;
end;

function TcaRichEditOleCallback._Release: Longint;
begin
  Dec(FRefCount);
  Result := FRefCount;
end;

procedure TcaRichEditOleCallback.CreateAccelTable;
var
  Menu: TMainMenu;
begin
  if (FAccelTable = 0) and Assigned(FFrameForm) then
    begin
      Menu := FFrameForm.Form.Menu;
      if Menu <> nil then
        Menu.GetOle2AcceleratorTable(FAccelTable, FAccelCount, [0, 2, 4]);
    end;
end;

procedure TcaRichEditOleCallback.DestroyAccelTable;
begin
  if FAccelTable <> 0 then
    begin
      DestroyAcceleratorTable(FAccelTable);
      FAccelTable := 0;
      FAccelCount := 0;
    end;
end;

procedure TcaRichEditOleCallback.AssignFrame;
begin
  if (GetParentForm(FRichEdit) <> nil) and not Assigned(FFrameForm) and
    FRichEdit.AllowInPlace then
    begin
      FDocForm := GetVCLFrameForm(ValidParentForm(FRichEdit));
      FFrameForm := FDocForm;
      if IsFormMDIChild(FDocForm.Form) then
        FFrameForm := GetVCLFrameForm(Application.MainForm);
    end;
end;

function TcaRichEditOleCallback.GetNewStorage(out stg: IStorage): HResult;
begin
  try
    CreateStorage(stg);
    Result := S_OK;
  except
    Result := E_OUTOFMEMORY;
  end;
end;

function TcaRichEditOleCallback.GetInPlaceContext(out Frame: IOleInPlaceFrame; out Doc: IOleInPlaceUIWindow;
  lpFrameInfo: POleInPlaceFrameInfo): HResult;
begin
  AssignFrame;
  if Assigned(FFrameForm) and FRichEdit.AllowInPlace then
    begin
      Frame := FFrameForm;
      Doc := FDocForm;
      CreateAccelTable;
      with lpFrameInfo^ do
        begin
          fMDIApp := False;
          FFrameForm.GetWindow(hWndFrame);
          hAccel := FAccelTable;
          cAccelEntries := FAccelCount;
        end;
      Result := S_OK;
    end
  else
    Result := E_NOTIMPL;
end;

function TcaRichEditOleCallback.QueryInsertObject(const clsid: TCLSID; const stg: IStorage;
  cp: Longint): HResult;
begin
  Result := NOERROR;
end;

function TcaRichEditOleCallback.DeleteObject(const oleobj: IOleObject): HResult;
begin
  if Assigned(oleobj) then oleobj.Close(OLECLOSE_NOSAVE);
  Result := NOERROR;
end;

function TcaRichEditOleCallback.QueryAcceptData(const dataobj: IDataObject;
  var cfFormat: TClipFormat; reco: DWORD; fReally: BOOL;
  hMetaPict: HGLOBAL): HResult;
begin
  Result := S_OK;
end;

function TcaRichEditOleCallback.ContextSensitiveHelp(fEnterMode: BOOL): HResult;
begin
  Result := NOERROR;
end;

function TcaRichEditOleCallback.GetClipboardData(const chrg: TCharRange; reco: DWORD; out dataobj: IDataObject): HResult;
begin
  Result := E_NOTIMPL;
end;

function TcaRichEditOleCallback.GetDragDropEffect(fDrag: BOOL; grfKeyState: DWORD;
  var dwEffect: DWORD): HResult;
begin
  Result := E_NOTIMPL;
end;

function TcaRichEditOleCallback.GetContextMenu(seltype: Word; const oleobj: IOleObject; const chrg: TCharRange; out menu: HMENU): HResult;
begin
  Result := E_NOTIMPL;
end;

function TcaRichEditOleCallback.ShowContainerUI(fShow: BOOL): HResult;
begin
  if not fShow then AssignFrame;
  if Assigned(FFrameForm) then
    begin
      if fShow then
        begin
          FFrameForm.SetMenu(0, 0, 0);
          FFrameForm.ClearBorderSpace;
          FRichEdit.SetUIActive(False);
          DestroyAccelTable;
          TForm(FFrameForm.Form).AutoScroll := FAutoScroll;
          FFrameForm := nil;
          FDocForm := nil;
        end
      else
        begin
          FAutoScroll := TForm(FFrameForm.Form).AutoScroll;
          TForm(FFrameForm.Form).AutoScroll := False;
          FRichEdit.SetUIActive(True);
        end;
      Result := S_OK;
    end
  else
    Result := E_NOTIMPL;
end;

 //---------------------------------------------------------------------------
 // TcaOleUIObjInfo
 //
 // helper interface for Object Properties dialog
 //---------------------------------------------------------------------------

type
  TcaOleUIObjInfo = class(TInterfacedObject, IOleUIObjInfo)
  private
    FRichEdit: TcaCustomRichEdit;
    FReObject: TcaReObject;
  public
    constructor Create(RichEdit: TcaCustomRichEdit; ReObject: TcaReObject);
    function GetObjectInfo(dwObject: Longint; var dwObjSize: Longint; var lpszLabel: PChar; var lpszType: PChar; var lpszShortType: PChar; var lpszLocation: PChar): HResult; stdcall;
    function GetConvertInfo(dwObject: Longint; var ClassID: TCLSID; var wFormat: Word; var ConvertDefaultClassID: TCLSID; var lpClsidExclude: PCLSID; var cClsidExclude: Longint): HResult; stdcall;
    function ConvertObject(dwObject: Longint; const clsidNew: TCLSID): HResult; stdcall;
    function GetViewInfo(dwObject: Longint; var hMetaPict: HGlobal; var dvAspect: Longint; var nCurrentScale: Integer): HResult; stdcall;
    function SetViewInfo(dwObject: Longint; hMetaPict: HGlobal; dvAspect: Longint; nCurrentScale: Integer; bRelativeToOrig: BOOL): HResult; stdcall;
  end;

constructor TcaOleUIObjInfo.Create(RichEdit: TcaCustomRichEdit;
  ReObject: TcaReObject);
begin
  inherited Create;
  FRichEdit := RichEdit;
  FReObject := ReObject;
end;

function TcaOleUIObjInfo.GetObjectInfo(dwObject: Longint;
  var dwObjSize: Longint; var lpszLabel: PChar;
  var lpszType: PChar; var lpszShortType: PChar;
  var lpszLocation: PChar): HResult;
begin
  if @dwObjSize <> nil then dwObjSize := -1;
  if @lpszLabel <> nil then lpszLabel := CoAllocCStr(GetFullNameStr(FReObject.poleobj));
  if @lpszType <> nil then lpszType := CoAllocCStr(GetFullNameStr(FReObject.poleobj));
  if @lpszShortType <> nil then lpszShortType := CoAllocCStr(GetShortNameStr(FReObject.poleobj));
  if (@lpszLocation <> nil) then
    begin
      if Trim(FRichEdit.Title) <> '' then
        lpszLocation := CoAllocCStr(Format('%s - %s',
          [FRichEdit.Title, Application.Title]))
      else
        lpszLocation := CoAllocCStr(Application.Title);
    end;
  Result := S_OK;
end;

function TcaOleUIObjInfo.GetConvertInfo(dwObject: Longint; var ClassID: TCLSID;
  var wFormat: Word; var ConvertDefaultClassID: TCLSID;
  var lpClsidExclude: PCLSID; var cClsidExclude: Longint): HResult;
begin
  FReObject.poleobj.GetUserClassID(ClassID);
  Result := S_OK;
end;

function TcaOleUIObjInfo.ConvertObject(dwObject: Longint;
  const clsidNew: TCLSID): HResult;
begin
  Result := E_NOTIMPL;
end;

function TcaOleUIObjInfo.GetViewInfo(dwObject: Longint; var hMetaPict: HGlobal;
  var dvAspect: Longint; var nCurrentScale: Integer): HResult;
begin
  if @hMetaPict <> nil then
    hMetaPict := GetIconMetaPict(FReObject.poleobj, FReObject.dvAspect);
  if @dvAspect <> nil then dvAspect := FReObject.dvAspect;
  if @nCurrentScale <> nil then nCurrentScale := 0;
  Result := S_OK;
end;

function TcaOleUIObjInfo.SetViewInfo(dwObject: Longint; hMetaPict: HGlobal;
  dvAspect: Longint; nCurrentScale: Integer;
  bRelativeToOrig: BOOL): HResult;
var
  Iconic: Boolean;
begin
  if Assigned(FRichEdit.FRichEditOle) then
    begin
      case dvAspect of
        DVASPECT_CONTENT:
          Iconic := False;
        DVASPECT_ICON:
          Iconic := True;
      else
        Iconic := FReObject.dvAspect = DVASPECT_ICON;
      end;
      IcaRichEditOle(FRichEdit.FRichEditOle).InPlaceDeactivate;
      Result := OleSetDrawAspect(FReObject.poleobj, Iconic, hMetaPict,
        FReObject.dvAspect);
      if Succeeded(Result) then
        IcaRichEditOle(FRichEdit.FRichEditOle).SetDvaspect(
          Longint(REO_IOB_SELECTION), FReObject.dvAspect);
    end
  else
    Result := E_NOTIMPL;
end;

 //---------------------------------------------------------------------------
 // TcaOleUILinkInfo
 //
 // helper interface for Object Properties dialog
 //---------------------------------------------------------------------------

type
  TcaOleUILinkInfo = class(TInterfacedObject, IOleUILinkInfo)
  private
    FReObject: TcaReObject;
    FRichEdit: TcaCustomRichEdit;
    FOleLink: IOleLink;
  public
    constructor Create(RichEdit: TcaCustomRichEdit; ReObject: TcaReObject);
    function GetNextLink(dwLink: Longint): Longint; stdcall;
    function SetLinkUpdateOptions(dwLink: Longint; dwUpdateOpt: Longint): HResult; stdcall;
    function GetLinkUpdateOptions(dwLink: Longint; var dwUpdateOpt: Longint): HResult; stdcall;
    function SetLinkSource(dwLink: Longint; pszDisplayName: PChar;
      lenFileName: Longint; var chEaten: Longint;
      fValidateSource: BOOL): HResult; stdcall;
    function GetLinkSource(dwLink: Longint; var pszDisplayName: PChar;
      var lenFileName: Longint; var pszFullLinkType: PChar;
      var pszShortLinkType: PChar; var fSourceAvailable: BOOL;
      var fIsSelected: BOOL): HResult; stdcall;
    function OpenLinkSource(dwLink: Longint): HResult; stdcall;
    function UpdateLink(dwLink: Longint; fErrorMessage: BOOL; fErrorAction: BOOL): HResult; stdcall;
    function CancelLink(dwLink: Longint): HResult; stdcall;
    function GetLastUpdate(dwLink: Longint; var LastUpdate: TFileTime): HResult; stdcall;
  end;

procedure LinkError(const Ident: string);
begin
  Application.MessageBox(PChar(Ident), PChar(SLinkProperties),
    MB_OK or MB_ICONSTOP);
end;

constructor TcaOleUILinkInfo.Create(RichEdit: TcaCustomRichEdit;
  ReObject: TcaReObject);
begin
  inherited Create;
  FReObject := ReObject;
  FRichEdit := RichEdit;
  OleCheck(FReObject.poleobj.QueryInterface(IOleLink, FOleLink));
end;

function TcaOleUILinkInfo.GetNextLink(dwLink: Longint): Longint;
begin
  if dwLink = 0 then
    Result := Longint(FRichEdit)
  else
    Result := 0;
end;

function TcaOleUILinkInfo.SetLinkUpdateOptions(dwLink: Longint;
  dwUpdateOpt: Longint): HResult;
begin
  Result := FOleLink.SetUpdateOptions(dwUpdateOpt);
  if Succeeded(Result) then FRichEdit.Modified := True;
end;

function TcaOleUILinkInfo.GetLinkUpdateOptions(dwLink: Longint;
  var dwUpdateOpt: Longint): HResult;
begin
  Result := FOleLink.GetUpdateOptions(dwUpdateOpt);
end;

function TcaOleUILinkInfo.SetLinkSource(dwLink: Longint; pszDisplayName: PChar;
  lenFileName: Longint; var chEaten: Longint;
  fValidateSource: BOOL): HResult;
var
  DisplayName: string;
  Buffer: array[0..255] of WideChar;
begin
  Result := E_FAIL;
  if fValidateSource then
    begin
      DisplayName := pszDisplayName;
      if Succeeded(FOleLink.SetSourceDisplayName(StringToWideChar(DisplayName,
        Buffer, SizeOf(Buffer) div 2))) then
        begin
          chEaten := Length(DisplayName);
          try
            OleCheck(FReObject.poleobj.Update);
          except
            Application.HandleException(FRichEdit);
          end;
          Result := S_OK;
        end;
    end
  else
    LinkError(SInvalidLinkSource);
end;

function TcaOleUILinkInfo.GetLinkSource(dwLink: Longint; var pszDisplayName: PChar;
  var lenFileName: Longint; var pszFullLinkType: PChar;
  var pszShortLinkType: PChar; var fSourceAvailable: BOOL;
  var fIsSelected: BOOL): HResult;
var
  Moniker: IMoniker;
begin
  if @pszDisplayName <> nil then
    pszDisplayName := CoAllocCStr(GetDisplayNameStr(FOleLink));
  if @lenFileName <> nil then
    begin
      lenFileName := 0;
      FOleLink.GetSourceMoniker(Moniker);
      if Moniker <> nil then
        begin
          lenFileName := OleStdGetLenFilePrefixOfMoniker(Moniker);
          ReleaseObject(Moniker);
        end;
    end;
  if @pszFullLinkType <> nil then
    pszFullLinkType := CoAllocCStr(GetFullNameStr(FReObject.poleobj));
  if @pszShortLinkType <> nil then
    pszShortLinkType := CoAllocCStr(GetShortNameStr(FReObject.poleobj));
  Result := S_OK;
end;

function TcaOleUILinkInfo.OpenLinkSource(dwLink: Longint): HResult;
begin
  try
    OleCheck(FReObject.poleobj.DoVerb(OLEIVERB_SHOW, nil, FReObject.polesite,
      0, FRichEdit.Handle, FRichEdit.ClientRect));
  except
    Application.HandleException(FRichEdit);
  end;
  Result := S_OK;
end;

function TcaOleUILinkInfo.UpdateLink(dwLink: Longint; fErrorMessage: BOOL;
  fErrorAction: BOOL): HResult;
begin
  try
    OleCheck(FReObject.poleobj.Update);
  except
    Application.HandleException(FRichEdit);
  end;
  Result := S_OK;
end;

function TcaOleUILinkInfo.CancelLink(dwLink: Longint): HResult;
begin
  LinkError(SCannotBreakLink);
  Result := E_NOTIMPL;
end;

function TcaOleUILinkInfo.GetLastUpdate(dwLink: Longint;
  var LastUpdate: TFileTime): HResult;
begin
  Result := S_OK;
end;

 // Get RichEdit OLE interface

function GetRichEditOle(Wnd: HWnd; var RichEditOle): Boolean;
begin
  Result := SendMessage(Wnd, EM_GETOLEINTERFACE, 0, Longint(@RichEditOle)) <> 0;
end;

 //---------------------------------------------------------------------------
 // TcaRichEditStrings
 //---------------------------------------------------------------------------

const
  ReadError   = $0001;
  WriteError  = $0002;
  NoError     = $0000;

type
  TcaRichEditStrings = class(TStrings)
  private
    RichEdit: TcaCustomRichEdit;
    FFormat: TcaRichStreamFormat;
    FMode: TcaRichStreamModes;
    FConverter: TConversion;
    procedure EnableChange(const Value: Boolean);
  protected
    function Get(Index: Integer): string; override;
    function GetCount: Integer; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure SetUpdateState(Updating: Boolean); override;
    procedure SetTextStr(const Value: string); override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    procedure AddStrings(Strings: TStrings); override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
    procedure LoadFromFile(const FileName: string); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToFile(const FileName: string); override;
    procedure SaveToStream(Stream: TStream); override;
    property Format: TcaRichStreamFormat read FFormat write FFormat;
    property Mode: TcaRichStreamModes read FMode write FMode;
  end;

destructor TcaRichEditStrings.Destroy;
begin
  FConverter.Free;
  inherited Destroy;
end;

procedure TcaRichEditStrings.AddStrings(Strings: TStrings);
var
  SelChange: TNotifyEvent;
begin
  SelChange := RichEdit.OnSelectionChange;
  RichEdit.OnSelectionChange := nil;
  try
    inherited AddStrings(Strings);
  finally
    RichEdit.OnSelectionChange := SelChange;
  end;
end;

function TcaRichEditStrings.GetCount: Integer;
begin
  with RichEdit do
    begin
      Result := SendMessage(Handle, EM_GETLINECOUNT, 0, 0);
      if GetLineLength(GetLineIndex(Result - 1)) = 0 then Dec(Result);
    end;
end;

function TcaRichEditStrings.Get(Index: Integer): string;
var
  Text: array[0..4095] of Char;
  L: Integer;
begin
  Word((@Text)^) := SizeOf(Text);
  L := SendMessage(RichEdit.Handle, EM_GETLINE, Index, Longint(@Text));
  if (Text[L - 2] = #13) and (Text[L - 1] = #10) then
    Dec(L, 2)
  else if (RichEditVersion >= 2) and (Text[L - 1] = #13) then
    Dec(L);
  SetString(Result, Text, L);
end;

procedure TcaRichEditStrings.Put(Index: Integer; const S: string);
var
  Selection: TCharRange;
begin
  if Index >= 0 then
    begin
      Selection.cpMin := RichEdit.GetLineIndex(Index);
      if Selection.cpMin <> -1 then
        begin
          Selection.cpMax := Selection.cpMin +
            RichEdit.GetLineLength(Selection.cpMin);
          SendMessage(RichEdit.Handle, EM_EXSETSEL, 0, Longint(@Selection));
          RichEdit.FLinesUpdating := True;
          try
            SendMessage(RichEdit.Handle, EM_REPLACESEL, 0, Longint(PChar(S)));
          finally
            RichEdit.FLinesUpdating := False;
          end;
        end;
    end;
end;

procedure TcaRichEditStrings.Insert(Index: Integer; const S: string);
var
  L: Integer;
  Selection: TCharRange;
  Fmt: PChar;
  Str: string;
begin
  if Index >= 0 then
    begin
      Selection.cpMin := RichEdit.GetLineIndex(Index);
      if Selection.cpMin >= 0 then
        begin
          if RichEditVersion = 1 then
            Fmt := '%s'#13#10
          else
            Fmt := '%s'#13;
        end
      else
        begin
          Selection.cpMin := RichEdit.GetLineIndex(Index - 1);
          if Selection.cpMin < 0 then Exit;
          L := RichEdit.GetLineLength(Selection.cpMin);
          if L = 0 then Exit;
          Inc(Selection.cpMin, L);
          if RichEditVersion = 1 then
            Fmt := #13#10'%s'
          else
            Fmt := #13'%s';
        end;
      Selection.cpMax := Selection.cpMin;
      SendMessage(RichEdit.Handle, EM_EXSETSEL, 0, Longint(@Selection));
      Str := SysUtils.Format(Fmt, [S]);
      RichEdit.FLinesUpdating := True;
      try
        SendMessage(RichEdit.Handle, EM_REPLACESEL, 0, Longint(PChar(Str)));
      finally
        RichEdit.FLinesUpdating := False;
      end;
      if RichEditVersion = 1 then
        if RichEdit.SelStart <> (Selection.cpMax + Length(Str)) then
          raise EOutOfResources.Create(ResStr(sRichEditInsertError));
    end;
end;

procedure TcaRichEditStrings.Delete(Index: Integer);
const
  Empty: PChar = '';
var
  Selection: TCharRange;
begin
  if Index < 0 then Exit;
  Selection.cpMin := RichEdit.GetLineIndex(Index);
  if Selection.cpMin <> -1 then
    begin
      Selection.cpMax := RichEdit.GetLineIndex(Index + 1);
      if Selection.cpMax = -1 then
        Selection.cpMax := Selection.cpMin +
          RichEdit.GetLineLength(Selection.cpMin);
      SendMessage(RichEdit.Handle, EM_EXSETSEL, 0, Longint(@Selection));
      RichEdit.FLinesUpdating := True;
      try
        SendMessage(RichEdit.Handle, EM_REPLACESEL, 0, Longint(Empty));
      finally
        RichEdit.FLinesUpdating := False;
      end;
    end;
end;

procedure TcaRichEditStrings.Clear;
begin
  RichEdit.Clear;
end;

procedure TcaRichEditStrings.SetUpdateState(Updating: Boolean);
begin
  if RichEdit.Showing then
    SendMessage(RichEdit.Handle, WM_SETREDRAW, Ord(not Updating), 0);
  if not Updating then
    begin
      RichEdit.Refresh;
      RichEdit.Perform(CM_TEXTCHANGED, 0, 0);
    end;
end;

procedure TcaRichEditStrings.EnableChange(const Value: Boolean);
var
  EventMask: Longint;
begin
  with RichEdit do
    begin
      EventMask := SendMessage(Handle, EM_GETEVENTMASK, 0, 0);
      if Value then
        EventMask := EventMask or ENM_CHANGE
      else
        EventMask := EventMask and not ENM_CHANGE;
      SendMessage(Handle, EM_SETEVENTMASK, 0, EventMask);
    end;
end;

procedure TcaRichEditStrings.SetTextStr(const Value: string);
begin
  EnableChange(False);
  try
    inherited SetTextStr(Value);
  finally
    EnableChange(True);
  end;
end;

function AdjustLineBreaks(Dest, Source: PChar): Integer; assembler;
asm
        PUSH    ESI
        PUSH    EDI
        MOV     EDI,EAX
        MOV     ESI,EDX
        MOV     EDX,EAX
        CLD
@@1:    LODSB
@@2:    OR      AL,AL
        JE      @@4
        CMP     AL,0AH
        JE      @@3
        STOSB
        CMP     AL,0DH
        JNE     @@1
        MOV     AL,0AH
        STOSB
        LODSB
        CMP     AL,0AH
        JE      @@1
        JMP     @@2
@@3:    MOV     EAX,0A0DH
        STOSW
        JMP     @@1
@@4:    STOSB
        LEA     EAX,[EDI-1]
        SUB     EAX,EDX
        POP     EDI
        POP     ESI
end;

function StreamSave(dwCookie: Longint; pbBuff: PByte;
  cb: Longint; var pcb: Longint): Longint; stdcall;
var
  StreamInfo: PRichEditStreamInfo;
begin
  Result := NoError;
  StreamInfo := PRichEditStreamInfo(Pointer(dwCookie));
  try
    pcb := 0;
    if StreamInfo^.Converter <> nil then
      pcb := StreamInfo^.Converter.ConvertWriteStream(StreamInfo^.Stream, PChar(pbBuff), cb);
  except
    Result := WriteError;
  end;
end;

function StreamLoad(dwCookie: Longint; pbBuff: PByte;
  cb: Longint; var pcb: Longint): Longint; stdcall;
var
  Buffer, pBuff: PChar;
  StreamInfo: PRichEditStreamInfo;
begin
  Result := NoError;
  StreamInfo := PRichEditStreamInfo(Pointer(dwCookie));
  Buffer := StrAlloc(cb + 1);
  try
    cb := cb div 2;
    pcb := 0;
    pBuff := Buffer + cb;
    try
      if StreamInfo^.Converter <> nil then
        pcb := StreamInfo^.Converter.ConvertReadStream(StreamInfo^.Stream, pBuff, cb);
      if pcb > 0 then
        begin
          pBuff[pcb] := #0;
          if pBuff[pcb - 1] = #13 then pBuff[pcb - 1] := #0;
          pcb := AdjustLineBreaks(Buffer, pBuff);
          Move(Buffer^, pbBuff^, pcb);
        end;
    except
      Result := ReadError;
    end;
  finally
    StrDispose(Buffer);
  end;
end;

procedure TcaRichEditStrings.LoadFromStream(Stream: TStream);
var
  EditStream: TEditStream;
  Position: Longint;
  TextType: Longint;
  StreamInfo: TRichEditStreamInfo;
  Converter: TConversion;
begin
  StreamInfo.Stream := Stream;
  if FConverter <> nil then
    Converter := FConverter
  else
    Converter := RichEdit.DefaultConverter.Create;
  StreamInfo.Converter := Converter;
  try
    with EditStream do
      begin
        dwCookie := Longint(Pointer(@StreamInfo));
        pfnCallBack := @StreamLoad;
        dwError := 0;
      end;
    Position := Stream.Position;
    case FFormat of
      sfDefault:
        if RichEdit.PlainText then
          TextType := SF_TEXT
        else
          TextType := SF_RTF;
      sfRichText: TextType := SF_RTF;
    else // sfPlainText
      TextType := SF_TEXT;
    end;
    if TextType = SF_RTF then
      begin
        if smPlainRtf in Mode then TextType := TextType or SFF_PLAINRTF;
      end;
    if TextType = SF_TEXT then
      begin
        if (smUnicode in Mode) and (RichEditVersion > 1) then
          TextType := TextType or SF_UNICODE;
      end;
    if smSelection in Mode then TextType := TextType or SFF_SELECTION;
    SendMessage(RichEdit.Handle, EM_STREAMIN, TextType, Longint(@EditStream));
    if (EditStream.dwError <> 0) then
      begin
        Stream.Position := Position;
        if (TextType and SF_RTF = SF_RTF) then
          TextType := SF_TEXT
        else
          TextType := SF_RTF;
        SendMessage(RichEdit.Handle, EM_STREAMIN, TextType, Longint(@EditStream));
        if EditStream.dwError <> 0 then
          raise EOutOfResources.Create(ResStr(sRichEditLoadFail));
      end;
    RichEdit.SetSelection(0, 0, True);
  finally
    if FConverter = nil then Converter.Free;
  end;
end;

procedure TcaRichEditStrings.SaveToStream(Stream: TStream);
var
  EditStream: TEditStream;
  TextType: Longint;
  StreamInfo: TRichEditStreamInfo;
  Converter: TConversion;
begin
  if FConverter <> nil then
    Converter := FConverter
  else
    Converter := RichEdit.DefaultConverter.Create;
  StreamInfo.Stream := Stream;
  StreamInfo.Converter := Converter;
  try
    with EditStream do
      begin
        dwCookie := Longint(Pointer(@StreamInfo));
        pfnCallBack := @StreamSave;
        dwError := 0;
      end;
    case FFormat of
      sfDefault:
        if RichEdit.PlainText then
          TextType := SF_TEXT
        else
          TextType := SF_RTF;
      sfRichText: TextType := SF_RTF;
    else  // sfPlainText
      TextType := SF_TEXT;
    end;
    if TextType = SF_RTF then
      begin
        if smNoObjects in Mode then TextType := SF_RTFNOOBJS;
        if smPlainRtf in Mode then TextType := TextType or SFF_PLAINRTF;
      end
    else if TextType = SF_TEXT then
      begin
        if (smUnicode in Mode) and (RichEditVersion > 1) then
          TextType := TextType or SF_UNICODE;
      end;
    if smSelection in Mode then TextType := TextType or SFF_SELECTION;
    SendMessage(RichEdit.Handle, EM_STREAMOUT, TextType, Longint(@EditStream));
    if EditStream.dwError <> 0 then
      raise EOutOfResources.Create(ResStr(sRichEditSaveFail));
  finally
    if FConverter = nil then Converter.Free;
  end;
end;

procedure TcaRichEditStrings.LoadFromFile(const FileName: string);
var
  Ext: string;
  Convert: PcaRichConversionFormat;
  SaveFormat: TcaRichStreamFormat;
begin
  Ext := LowerCase(ExtractFileExt(Filename));
  System.Delete(Ext, 1, 1);
  Convert := ConversionFormatList;
  while Convert <> nil do
    with Convert^ do
      if Extension <> Ext then
        Convert := Next
      else
        Break;
  if (FConverter = nil) and (Convert <> nil) then
    FConverter := Convert^.ConversionClass.Create;
  try
    SaveFormat := Format;
    try
      if Convert <> nil then
        begin
          if Convert^.PlainText then
            FFormat := sfPlainText
          else
            FFormat := sfRichText;
        end;
      inherited LoadFromFile(FileName);
    finally
      FFormat := SaveFormat;
    end;
  except
    FConverter.Free;
    FConverter := nil;
    raise;
  end;
end;

procedure TcaRichEditStrings.SaveToFile(const FileName: string);
var
  Ext: string;
  Convert: PcaRichConversionFormat;
  SaveFormat: TcaRichStreamFormat;
begin
  Ext := LowerCase(ExtractFileExt(Filename));
  System.Delete(Ext, 1, 1);
  Convert := ConversionFormatList;
  while Convert <> nil do
    with Convert^ do
      if Extension <> Ext then
        Convert := Next
      else
        Break;
  if (FConverter = nil) and (Convert <> nil) then
    FConverter := Convert^.ConversionClass.Create;
  try
    SaveFormat := Format;
    try
      if Convert <> nil then
        begin
          if Convert^.PlainText then
            FFormat := sfPlainText
          else
            FFormat := sfRichText;
        end;
      inherited SaveToFile(FileName);
    finally
      FFormat := SaveFormat;
    end;
  except
    FConverter.Free;
    FConverter := nil;
    raise;
  end;
end;

 //---------------------------------------------------------------------------
 // TcaOEMConversion
 //---------------------------------------------------------------------------

function TcaOEMConversion.ConvertReadStream(Stream: TStream; Buffer: PChar;
  BufSize: Integer): Integer;
var
  Mem: TMemoryStream;
begin
  Mem := TMemoryStream.Create;
  try
    Mem.SetSize(BufSize);
    Result := inherited ConvertReadStream(Stream, PChar(Mem.Memory), BufSize);
    OemToCharBuff(PChar(Mem.Memory), Buffer, Result);
  finally
    Mem.Free;
  end;
end;

function TcaOEMConversion.ConvertWriteStream(Stream: TStream; Buffer: PChar;
  BufSize: Integer): Integer;
var
  Mem: TMemoryStream;
begin
  Mem := TMemoryStream.Create;
  try
    Mem.SetSize(BufSize);
    CharToOemBuff(Buffer, PChar(Mem.Memory), BufSize);
    Result := inherited ConvertWriteStream(Stream, PChar(Mem.Memory), BufSize);
  finally
    Mem.Free;
  end;
end;

 //---------------------------------------------------------------------------
 // TcaCustomRichEdit
 //---------------------------------------------------------------------------

constructor TcaCustomRichEdit.Create(AOwner: TComponent);
var
  DC: HDC;
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];
  FSelAttributes := TcaTextAttributes.Create(Self, atSelected);
  FDefAttributes := TcaTextAttributes.Create(Self, atDefaultText);
  FWordAttributes := TcaTextAttributes.Create(Self, atWord);
  FParagraph := TcaParaAttributes.Create(Self);
  FRichEditStrings := TcaRichEditStrings.Create;
  TcaRichEditStrings(FRichEditStrings).RichEdit := Self;
  TabStop := True;
  Width := 185;
  Height := 89;
  AutoSize := False;
  DoubleBuffered := False;
  FAllowObjects := True;
  FAllowInPlace := True;
  FAutoVerbMenu := True;
  FHideSelection := True;
  FHideScrollBars := True;
  ScrollBars := ssBoth;
  FSelectionBar := True;
  FLangOptions := [rlAutoFont];
  DC := GetDC(0);
  FScreenLogPixels := GetDeviceCaps(DC, LOGPIXELSY);
  ReleaseDC(0, DC);
  DefaultConverter := TConversion;
  FOldParaAlignment := TcaParaAlignment(Alignment);
  FUndoLimit := 100;
  FAutoURLDetect := True;
  FWordSelection := True;
  with FClickRange do
    begin
      cpMin := -1;
      cpMax := -1;
    end;
  FCallback := TcaRichEditOleCallback.Create(Self);
  Perform(CM_PARENTBIDIMODECHANGED, 0, 0);
end;

destructor TcaCustomRichEdit.Destroy;
begin
  FLastFind := nil;
  FSelAttributes.Free;
  FDefAttributes.Free;
  FWordAttributes.Free;
  FParagraph.Free;
  FRichEditStrings.Free;
  FMemStream.Free;
  FPopupVerbMenu.Free;
  FFindDialog.Free;
  FReplaceDialog.Free;
  inherited Destroy;
  // be sure that callback object is destroyed after inherited Destroy
  TcaRichEditOleCallback(FCallback).Free;
end;

procedure TcaCustomRichEdit.Clear;
begin
  CloseObjects;
  inherited Clear;
  Modified := False;
end;

procedure TcaCustomRichEdit.CreateParams(var Params: TCreateParams);
const
  HideScrollBars: array[Boolean] of DWORD = (ES_DISABLENOSCROLL, 0);
  HideSelections: array[Boolean] of DWORD = (ES_NOHIDESEL, 0);
  WordWraps: array[Boolean] of DWORD = (0, ES_AUTOHSCROLL);
  SelectionBars: array[Boolean] of DWORD = (0, ES_SELECTIONBAR);
begin
  inherited CreateParams(Params);
  case RichEditVersion of
    1: CreateSubClass(Params, RICHEDIT_CLASS10A);
  else
    CreateSubClass(Params, RICHEDIT_CLASS);
  end;
  with Params do
    begin
      Style := (Style and not (WS_HSCROLL or WS_VSCROLL)) or ES_SAVESEL or
        (WS_CLIPSIBLINGS or WS_CLIPCHILDREN);
      // NOTE: WS_CLIPCHILDREN and WS_CLIPSIBLINGS are essential otherwise
      // once the object is inserted you see some painting problems.
      Style := Style and not (WS_HSCROLL or WS_VSCROLL);
      if ScrollBars in [ssVertical, ssBoth] then
        Style := Style or WS_VSCROLL;
      if (ScrollBars in [ssHorizontal, ssBoth]) and not WordWrap then
        Style := Style or WS_HSCROLL;
      Style := Style or HideScrollBars[FHideScrollBars] or
        SelectionBars[FSelectionBar] or HideSelections[FHideSelection] and
        not WordWraps[WordWrap];
      WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
    end;
end;

procedure TcaCustomRichEdit.CreateWnd;
var
  StreamFmt: TcaRichStreamFormat;
  Mode: TcaRichStreamModes;
  DesignMode: Boolean;
  Mask: Longint;
begin
  StreamFmt := TcaRichEditStrings(Lines).Format;
  Mode := TcaRichEditStrings(Lines).Mode;
  inherited CreateWnd;
  if (SysLocale.FarEast) and not (SysLocale.PriLangID = LANG_JAPANESE) then
    Font.Charset := GetDefFontCharSet;
  Mask := ENM_CHANGE or ENM_SELCHANGE or ENM_REQUESTRESIZE or ENM_PROTECTED;
  if RichEditVersion >= 2 then Mask := Mask or ENM_LINK;
  SendMessage(Handle, EM_SETEVENTMASK, 0, Mask);
  SendMessage(Handle, EM_SETBKGNDCOLOR, 0, ColorToRGB(Color));
  DoSetMaxLength(MaxLength);
  SetWordSelection(FWordSelection);
  if RichEditVersion >= 2 then
    begin
      SendMessage(Handle, EM_AUTOURLDETECT, Longint(FAutoURLDetect), 0);
      FUndoLimit := SendMessage(Handle, EM_SETUNDOLIMIT, FUndoLimit, 0);
      UpdateTextModes(PlainText);
      SetLangOptions(FLangOptions);
    end;
  if FAllowObjects then
    begin
      SendMessage(Handle, EM_SETOLECALLBACK, 0,
        LPARAM(TcaRichEditOleCallback(FCallback) as IcaRichEditOleCallback));
      GetRichEditOle(Handle, FRichEditOle);
      UpdateHostNames;
    end;
  if FMemStream <> nil then
    begin
      FMemStream.ReadBuffer(DesignMode, SizeOf(DesignMode));
      if DesignMode then
        begin
          TcaRichEditStrings(Lines).Format := sfPlainText;
          TcaRichEditStrings(Lines).Mode := [];
        end;
      try
        Lines.LoadFromStream(FMemStream);
        FMemStream.Free;
        FMemStream := nil;
      finally
        TcaRichEditStrings(Lines).Format := StreamFmt;
        TcaRichEditStrings(Lines).Mode := Mode;
      end;
    end;
  if RichEditVersion < 2 then
    SendMessage(Handle, WM_SETFONT, 0, 0);
  Modified := FModified;
end;

procedure TcaCustomRichEdit.DestroyWnd;
var
  StreamFmt: TcaRichStreamFormat;
  Mode: TcaRichStreamModes;
  DesignMode: Boolean;
begin
  FModified := Modified;
  FMemStream := TMemoryStream.Create;
  StreamFmt := TcaRichEditStrings(Lines).Format;
  Mode := TcaRichEditStrings(Lines).Mode;
  DesignMode := (csDesigning in ComponentState);
  FMemStream.WriteBuffer(DesignMode, SizeOf(DesignMode));
  if DesignMode then
    begin
      TcaRichEditStrings(Lines).Format := sfPlainText;
      TcaRichEditStrings(Lines).Mode := [];
    end;
  try
    Lines.SaveToStream(FMemStream);
    FMemStream.Position := 0;
  finally
    TcaRichEditStrings(Lines).Format := StreamFmt;
    TcaRichEditStrings(Lines).Mode := Mode;
  end;
  inherited DestroyWnd;
end;

procedure TcaCustomRichEdit.SetAllowObjects(Value: Boolean);
begin
  if FAllowObjects <> Value then
    begin
      FAllowObjects := Value;
      RecreateWnd;
    end;
end;

procedure TcaCustomRichEdit.UpdateHostNames;
var
  AppName: string;
begin
  if HandleAllocated and Assigned(FRichEditOle) then
    begin
      AppName := Application.Title;
      if Trim(AppName) = '' then
        AppName := ExtractFileName(Application.ExeName);
      if Trim(Title) = '' then
        IcaRichEditOle(FRichEditOle).SetHostNames(PChar(AppName), PChar(AppName))
      else
        IcaRichEditOle(FRichEditOle).SetHostNames(PChar(AppName), PChar(Title));
    end;
end;

procedure TcaCustomRichEdit.SetTitle(const Value: string);
begin
  if FTitle <> Value then
    begin
      FTitle := Value;
      UpdateHostNames;
    end;
end;

function TcaCustomRichEdit.GetPopupMenu: TPopupMenu;
var
  EnumOleVerb: IEnumOleVerb;
  OleVerb: TOleVerb;
  Item: TMenuItem;
  ReObject: TcaReObject;
begin
  FPopupVerbMenu.Free;
  FPopupVerbMenu := nil;
  Result := inherited GetPopupMenu;
  if FAutoVerbMenu and (SelectionType = [stObject]) and
    Assigned(FRichEditOle) then
    begin
      FillChar(ReObject, SizeOf(ReObject), 0);
      ReObject.cbStruct := SizeOf(ReObject);
      if Succeeded(IcaRichEditOle(FRichEditOle).GetObject(
        Longint(REO_IOB_SELECTION), ReObject, REO_GETOBJ_POLEOBJ)) then
      try
        if Assigned(ReObject.poleobj) and
          (ReObject.dwFlags and REO_INPLACEACTIVE = 0) then
          begin
            FPopupVerbMenu := TPopupMenu.Create(Self);
            if ReObject.poleobj.EnumVerbs(EnumOleVerb) = 0 then
            try
              while (EnumOleVerb.Next(1, OleVerb, nil) = 0) and
                (OleVerb.lVerb >= 0) and
                (OleVerb.grfAttribs and OLEVERBATTRIB_ONCONTAINERMENU <> 0) do
                begin
                  Item := TMenuItem.Create(FPopupVerbMenu);
                  Item.Caption := WideCharToString(OleVerb.lpszVerbName);
                  Item.Tag := OleVerb.lVerb;
                  Item.Default := (OleVerb.lVerb = OLEIVERB_PRIMARY);
                  Item.OnClick := PopupVerbClick;
                  FPopupVerbMenu.Items.Add(Item);
                end;
            finally
              ReleaseObject(EnumOleVerb);
            end;
            if (Result <> nil) and (Result.Items.Count > 0) then
              begin
                Item := TMenuItem.Create(FPopupVerbMenu);
                Item.Caption := '-';
                Result.Items.Add(Item);
                Item := TMenuItem.Create(FPopupVerbMenu);
                Item.Caption := Format(ResStr(SPropDlgCaption),
                  [GetFullNameStr(ReObject.poleobj)]);
                Item.OnClick := ObjectPropsClick;
                Result.Items.Add(Item);
                if FPopupVerbMenu.Items.Count > 0 then
                  begin
                    FPopupVerbMenu.Items.Caption := GetFullNameStr(ReObject.poleobj);
                    Result.Items.Add(FPopupVerbMenu.Items);
                  end;
              end
            else if FPopupVerbMenu.Items.Count > 0 then
              begin
                Item := TMenuItem.Create(FPopupVerbMenu);
                Item.Caption := Format(ResStr(SPropDlgCaption),
                  [GetFullNameStr(ReObject.poleobj)]);
                Item.OnClick := ObjectPropsClick;
                FPopupVerbMenu.Items.Insert(0, Item);
                Result := FPopupVerbMenu;
              end;
          end;
      finally
        ReleaseObject(ReObject.poleobj);
      end;
    end;
end;

procedure TcaCustomRichEdit.PopupVerbClick(Sender: TObject);
var
  ReObject: TcaReObject;
begin
  if Assigned(FRichEditOle) then
    begin
      FillChar(ReObject, SizeOf(ReObject), 0);
      ReObject.cbStruct := SizeOf(ReObject);
      if Succeeded(IcaRichEditOle(FRichEditOle).GetObject(
        Longint(REO_IOB_SELECTION), ReObject, REO_GETOBJ_POLEOBJ or
        REO_GETOBJ_POLESITE)) then
      try
        if ReObject.dwFlags and REO_INPLACEACTIVE = 0 then
          OleCheck(ReObject.poleobj.DoVerb((Sender as TMenuItem).Tag, nil,
            ReObject.polesite, 0, Handle, ClientRect));
      finally
        ReleaseObject(ReObject.polesite);
        ReleaseObject(ReObject.poleobj);
      end;
    end;
end;

procedure TcaCustomRichEdit.ObjectPropsClick(Sender: TObject);
begin
  ObjectPropertiesDialog;
end;

procedure TcaCustomRichEdit.WMSetFont(var Message: TWMSetFont);
begin
  FDefAttributes.Assign(Font);
end;

procedure TcaCustomRichEdit.CMFontChanged(var Message: TMessage);
begin
  inherited;
  FDefAttributes.Assign(Font);
end;

procedure TcaCustomRichEdit.CreateWindowHandle(const Params: TCreateParams);
var
  Bounds: TRect;
begin
  Bounds := BoundsRect;
  inherited CreateWindowHandle(Params);
  if HandleAllocated then BoundsRect := Bounds;
end;

procedure TcaCustomRichEdit.DoSetMaxLength(Value: Integer);
begin
  // The rich edit control's default maximum amount of text is 32K
  // Let's set it at 16M by default
  if Value = 0 then Value := $FFFFFF;
  SendMessage(Handle, EM_EXLIMITTEXT, 0, Value);
end;

procedure TcaCustomRichEdit.DoTextChanged;
begin
  if Assigned(FOnTextChanged) then FOnTextChanged(Self);    
end;

function TcaCustomRichEdit.GetCaretPos: TPoint;
var
  CharRange: TCharRange;
begin
  SendMessage(Handle, EM_EXGETSEL, 0, Longint(@CharRange));
  Result.X := CharRange.cpMax;
  Result.Y := LineFromChar(Result.X);
  Dec(Result.X, GetLineIndex(-1));
end;

function TcaCustomRichEdit.GetSelLength: Integer;
begin
  with GetSelection do
    Result := cpMax - cpMin;
end;

function TcaCustomRichEdit.GetSelStart: Integer;
begin
  Result := GetSelection.cpMin;
end;

function TcaCustomRichEdit.GetSelText: string;
begin
  with GetSelection do
    Result := GetTextRange(cpMin, cpMax);
end;

function TcaCustomRichEdit.GetSelTextBuf(Buffer: PChar; BufSize: Integer): Integer;
var
  S: string;
begin
  S := SelText;
  Result := Length(S);
  if BufSize < Length(S) then Result := BufSize;
  StrPLCopy(Buffer, S, Result);
end;

procedure TcaCustomRichEdit.CMBiDiModeChanged(var Message: TMessage);
var
  AParagraph: TParaFormat2;
begin
  HandleNeeded; // we REALLY need the handle for BiDi
  inherited;
  Paragraph.GetAttributes(AParagraph);
  AParagraph.dwMask := PFM_ALIGNMENT;
  AParagraph.wAlignment := Ord(Alignment) + 1;
  Paragraph.SetAttributes(AParagraph);
end;

procedure TcaCustomRichEdit.SetHideScrollBars(Value: Boolean);
begin
  if HideScrollBars <> Value then
    begin
      FHideScrollBars := Value;
      RecreateWnd;
    end;
end;

procedure TcaCustomRichEdit.SetSelectionBar(Value: Boolean);
begin
  if FSelectionBar <> Value then
    begin
      FSelectionBar := Value;
      RecreateWnd;
    end;
end;

procedure TcaCustomRichEdit.SetHideSelection(Value: Boolean);
begin
  if HideSelection <> Value then
    begin
      FHideSelection := Value;
      SendMessage(Handle, EM_HIDESELECTION, Ord(HideSelection), LPARAM(True));
    end;
end;

function TcaCustomRichEdit.GetAutoURLDetect: Boolean;
begin
  Result := FAutoURLDetect;
  if HandleAllocated and not (csDesigning in ComponentState) then
    begin
      if RichEditVersion >= 2 then
        Result := Boolean(SendMessage(Handle, EM_GETAUTOURLDETECT, 0, 0));
    end;
end;

procedure TcaCustomRichEdit.SetAutoURLDetect(Value: Boolean);
begin
  if Value <> FAutoURLDetect then
    begin
      FAutoURLDetect := Value;
      if HandleAllocated and (RichEditVersion >= 2) then
        SendMessage(Handle, EM_AUTOURLDETECT, Longint(FAutoURLDetect), 0);
    end;
end;

function TcaCustomRichEdit.GetWordSelection: Boolean;
begin
  Result := FWordSelection;
  if HandleAllocated then
    Result := (SendMessage(Handle, EM_GETOPTIONS, 0, 0) and
      ECO_AUTOWORDSELECTION) <> 0;
end;

procedure TcaCustomRichEdit.SetWordSelection(Value: Boolean);
var
  Options: LPARAM;
begin
  FWordSelection := Value;
  if HandleAllocated then
    begin
      Options := SendMessage(Handle, EM_GETOPTIONS, 0, 0);
      if Value then
        Options := Options or ECO_AUTOWORDSELECTION
      else
        Options := Options and not ECO_AUTOWORDSELECTION;
      SendMessage(Handle, EM_SETOPTIONS, ECOOP_SET, Options);
    end;
end;

const
  RichLangOptions: array[TcaRichLangOption] of DWORD = (IMF_AUTOKEYBOARD,
    IMF_AUTOFONT, IMF_IMECANCELCOMPLETE, IMF_IMEALWAYSSENDNOTIFY);

function TcaCustomRichEdit.GetLangOptions: TcaRichLangOptions;
var
  Flags: Longint;
  I: TcaRichLangOption;
begin
  Result := FLangOptions;
  if HandleAllocated and not (csDesigning in ComponentState) and
    (RichEditVersion >= 2) then
    begin
      Result := [];
      Flags := SendMessage(Handle, EM_GETLANGOPTIONS, 0, 0);
      for I := Low(TcaRichLangOption) to High(TcaRichLangOption) do
        if Flags and RichLangOptions[I] <> 0 then Include(Result, I);
    end;
end;

procedure TcaCustomRichEdit.SetLangOptions(Value: TcaRichLangOptions);
var
  Flags: DWORD;
  I: TcaRichLangOption;
begin
  FLangOptions := Value;
  if HandleAllocated and (RichEditVersion >= 2) then
    begin
      Flags := 0;
      for I := Low(TcaRichLangOption) to High(TcaRichLangOption) do
        if I in Value then Flags := Flags or RichLangOptions[I];
      SendMessage(Handle, EM_SETLANGOPTIONS, 0, LPARAM(Flags));
    end;
end;

procedure TcaCustomRichEdit.SetSelAttributes(Value: TcaTextAttributes);
begin
  FSelAttributes.Assign(Value);
end;

function TcaCustomRichEdit.GetCanRedo: Boolean;
begin
  Result := False;
  if HandleAllocated and (RichEditVersion >= 2) then
    Result := SendMessage(Handle, EM_CANREDO, 0, 0) <> 0;
end;

function TcaCustomRichEdit.GetCanPaste: Boolean;
begin
  Result := False;
  if HandleAllocated then
    Result := SendMessage(Handle, EM_CANPASTE, 0, 0) <> 0;
end;

function TcaCustomRichEdit.GetCanUndo: Boolean;
begin
  Result := False;
  if HandleAllocated then
    Result := SendMessage(Handle, EM_CANUNDO, 0, 0) <> 0;
end;

function TcaCustomRichEdit.GetRedoName: TcaUndoName;
begin
  Result := unUnknown;
  if (RichEditVersion >= 2) and HandleAllocated then
    Result := TcaUndoName(SendMessage(Handle, EM_GETREDONAME, 0, 0));
end;

function TcaCustomRichEdit.GetUndoName: TcaUndoName;
begin
  Result := unUnknown;
  if (RichEditVersion >= 2) and HandleAllocated then
    Result := TcaUndoName(SendMessage(Handle, EM_GETUNDONAME, 0, 0));
end;

function TcaCustomRichEdit.GetSelectionType: TcaRichSelectionType;
const
  SelTypes: array[TcaRichSelection] of Integer = (
    SEL_TEXT, SEL_OBJECT, SEL_MULTICHAR, SEL_MULTIOBJECT);
var
  Selection: Integer;
  I: TcaRichSelection;
begin
  Result := [];
  if HandleAllocated then
    begin
      Selection := SendMessage(Handle, EM_SELECTIONTYPE, 0, 0);
      for I := Low(TcaRichSelection) to High(TcaRichSelection) do
        if SelTypes[I] and Selection <> 0 then Include(Result, I);
    end;
end;

function TcaCustomRichEdit.GetSelection: TCharRange;
begin
  SendMessage(Handle, EM_EXGETSEL, 0, Longint(@Result));
end;

procedure TcaCustomRichEdit.SetSelection(StartPos, EndPos: Longint;
  ScrollCaret: Boolean);
var
  CharRange: TCharRange;
begin
  with CharRange do
    begin
      cpMin := StartPos;
      cpMax := EndPos;
    end;
  SendMessage(Handle, EM_EXSETSEL, 0, Longint(@CharRange));
  if ScrollCaret then SendMessage(Handle, EM_SCROLLCARET, 0, 0);
end;

procedure TcaCustomRichEdit.SetSelLength(Value: Integer);
begin
  with GetSelection do
    SetSelection(cpMin, cpMin + Value, True);
end;

procedure TcaCustomRichEdit.SetSelStart(Value: Integer);
begin
  SetSelection(Value, Value, False);
end;

function TcaCustomRichEdit.GetCharPos(CharIndex: Integer): TPoint;
var
  Res: Longint;
begin
  FillChar(Result, SizeOf(Result), 0);
  if HandleAllocated then
    begin
      if RichEditVersion = 2 then
        begin
          Res := SendMessage(Handle, Messages.EM_POSFROMCHAR, CharIndex, 0);
          Result.X := LoWord(Res);
          Result.Y := HiWord(Res);
        end
      else  // RichEdit 1.0 and 3.0
        SendMessage(Handle, Messages.EM_POSFROMCHAR, WPARAM(@Result), CharIndex);
    end;
end;

function TcaCustomRichEdit.GetTextRange(StartPos, EndPos: Longint): string;
var
  TextRange: TcaTextRange;
begin
  SetLength(Result, EndPos - StartPos + 1);
  TextRange.chrg.cpMin := StartPos;
  TextRange.chrg.cpMax := EndPos;
  TextRange.lpstrText := PAnsiChar(Result);
  SetLength(Result, SendMessage(Handle, EM_GETTEXTRANGE, 0, Longint(@TextRange)));
end;

function TcaCustomRichEdit.WordAtCursor: string;
var
  Range: TCharRange;
begin
  Result := '';
  if HandleAllocated then
    begin
      Range.cpMax := SelStart;
      if Range.cpMax = 0 then
        Range.cpMin := 0
      else if SendMessage(Handle, EM_FINDWORDBREAK, WB_ISDELIMITER, Range.cpMax) <> 0 then
        Range.cpMin := SendMessage(Handle, EM_FINDWORDBREAK, WB_MOVEWORDLEFT, Range.cpMax)
      else
        Range.cpMin := SendMessage(Handle, EM_FINDWORDBREAK, WB_LEFT, Range.cpMax);
      while SendMessage(Handle, EM_FINDWORDBREAK, WB_ISDELIMITER, Range.cpMin) <> 0 do
        Inc(Range.cpMin);
      Range.cpMax := SendMessage(Handle, EM_FINDWORDBREAK, WB_RIGHTBREAK, Range.cpMax);
      Result := Trim(GetTextRange(Range.cpMin, Range.cpMax));
    end;
end;

function TcaCustomRichEdit.LineFromChar(CharIndex: Integer): Integer;
begin
  Result := SendMessage(Handle, EM_EXLINEFROMCHAR, 0, CharIndex);
end;

function TcaCustomRichEdit.GetLineIndex(LineNo: Integer): Integer;
begin
  Result := SendMessage(Handle, EM_LINEINDEX, LineNo, 0);
end;

function TcaCustomRichEdit.GetLineLength(CharIndex: Integer): Integer;
begin
  Result := SendMessage(Handle, EM_LINELENGTH, CharIndex, 0);
end;

procedure TcaCustomRichEdit.SetUndoLimit(Value: Integer);
begin
  if (Value <> FUndoLimit) then
    begin
      FUndoLimit := Value;
      if (RichEditVersion >= 2) and HandleAllocated then
        FUndoLimit := SendMessage(Handle, EM_SETUNDOLIMIT, Value, 0);
    end;
end;

procedure TcaCustomRichEdit.SetDefAttributes(Value: TcaTextAttributes);
begin
  FDefAttributes.Assign(Value);
end;

procedure TcaCustomRichEdit.SetWordAttributes(Value: TcaTextAttributes);
begin
  FWordAttributes.Assign(Value);
end;

function TcaCustomRichEdit.GetStreamFormat: TcaRichStreamFormat;
begin
  Result := TcaRichEditStrings(Lines).Format;
end;

function TcaCustomRichEdit.GetStreamMode: TcaRichStreamModes;
begin
  Result := TcaRichEditStrings(Lines).Mode;
end;

procedure TcaCustomRichEdit.SetStreamFormat(Value: TcaRichStreamFormat);
begin
  TcaRichEditStrings(Lines).Format := Value;
end;

procedure TcaCustomRichEdit.SetStreamMode(Value: TcaRichStreamModes);
begin
  TcaRichEditStrings(Lines).Mode := Value;
end;

procedure TcaCustomRichEdit.SetPlainText(Value: Boolean);
var
  MemStream: TStream;
  StreamFmt: TcaRichStreamFormat;
  Mode: TcaRichStreamModes;
begin
  if PlainText <> Value then
    begin
      if HandleAllocated and (RichEditVersion >= 2) then
        begin
          MemStream := TMemoryStream.Create;
          try
            StreamFmt := TcaRichEditStrings(Lines).Format;
            Mode := TcaRichEditStrings(Lines).Mode;
            try
              if (csDesigning in ComponentState) or Value then
                TcaRichEditStrings(Lines).Format := sfPlainText
              else
                TcaRichEditStrings(Lines).Format := sfRichText;
              TcaRichEditStrings(Lines).Mode := [];
              Lines.SaveToStream(MemStream);
              MemStream.Position := 0;
              TcaRichEditStrings(Lines).EnableChange(False);
              try
                SendMessage(Handle, WM_SETTEXT, 0, 0);
                UpdateTextModes(Value);
                FPlainText := Value;
              finally
                TcaRichEditStrings(Lines).EnableChange(True);
              end;
              Lines.LoadFromStream(MemStream);
            finally
              TcaRichEditStrings(Lines).Format := StreamFmt;
              TcaRichEditStrings(Lines).Mode := Mode;
            end;
          finally
            MemStream.Free;
          end;
        end;
      FPlainText := Value;
    end;
end;

procedure TcaCustomRichEdit.UpdateTextModes(Plain: Boolean);
const
  TextModes: array[Boolean] of DWORD = (TM_RICHTEXT, TM_PLAINTEXT);
  UndoModes: array[Boolean] of DWORD = (TM_SINGLELEVELUNDO, TM_MULTILEVELUNDO);
begin
  if (RichEditVersion >= 2) and HandleAllocated then
    begin
      SendMessage(Handle, EM_SETTEXTMODE, TextModes[Plain] or
        UndoModes[FUndoLimit > 1], 0);
    end;
end;

procedure TcaCustomRichEdit.CMColorChanged(var Message: TMessage);
begin
  inherited;
  SendMessage(Handle, EM_SETBKGNDCOLOR, 0, ColorToRGB(Color))
end;

procedure TcaCustomRichEdit.EMReplaceSel(var Message: TMessage);
var
  CharRange: TCharRange;
begin
  Perform(EM_EXGETSEL, 0, Longint(@CharRange));
  with CharRange do
    cpMax := cpMin + Integer(StrLen(PChar(Message.lParam)));
  if (FUndoLimit > 1) and (RichEditVersion >= 2) and not FLinesUpdating then
    Message.wParam := 1; // allow Undo
  inherited;
  if not FLinesUpdating then
    begin
      Perform(EM_EXSETSEL, 0, Longint(@CharRange));
      Perform(EM_SCROLLCARET, 0, 0);
    end;
end;

procedure TcaCustomRichEdit.SetRichEditStrings(Value: TStrings);
begin
  FRichEditStrings.Assign(Value);
end;

procedure TcaCustomRichEdit.CloseObjects;
var
  I: Integer;
  ReObject: TcaReObject;
begin
  if Assigned(FRichEditOle) then
    begin
      FillChar(ReObject, SizeOf(ReObject), 0);
      ReObject.cbStruct := SizeOf(ReObject);
      with IcaRichEditOle(FRichEditOle) do
        begin
          for I := GetObjectCount - 1 downto 0 do
            if Succeeded(GetObject(I, ReObject, REO_GETOBJ_POLEOBJ)) then
              begin
                if ReObject.dwFlags and REO_INPLACEACTIVE <> 0 then
                  IcaRichEditOle(FRichEditOle).InPlaceDeactivate;
                ReObject.poleobj.Close(OLECLOSE_NOSAVE);
                ReleaseObject(ReObject.poleobj);
              end;
        end;
    end;
end;

function TcaCustomRichEdit.PasteSpecialDialog: Boolean;

  procedure SetPasteEntry(var Entry: TOleUIPasteEntry; Format: TClipFormat;
    tymed: DWORD; const FormatName, ResultText: string; Flags: DWORD);
  begin
    with Entry do
      begin
        fmtetc.cfFormat := Format;
        fmtetc.dwAspect := DVASPECT_CONTENT;
        fmtetc.lIndex := -1;
        fmtetc.tymed := tymed;
        if FormatName <> '' then
          lpstrFormatName := PChar(FormatName)
        else
          lpstrFormatName := '%s';
        if ResultText <> '' then
          lpstrResultText := PChar(ResultText)
        else
          lpstrResultText := '%s';
        dwFlags := Flags;
      end;
  end;

const
  PasteFormatCount = 6;
var
  Data: TOleUIPasteSpecial;
  PasteFormats: array[0..PasteFormatCount - 1] of TOleUIPasteEntry;
  Format: Integer;
  OleClientSite: IOleClientSite;
  Storage: IStorage;
  OleObject: IOleObject;
  ReObject: TcaReObject;
  Selection: TCharRange;
begin
  Result := False;
  if not CanPaste or not Assigned(FRichEditOle) then Exit;
  FillChar(Data, SizeOf(Data), 0);
  FillChar(PasteFormats, SizeOf(PasteFormats), 0);
  with Data do
    begin
      cbStruct := SizeOf(Data);
      hWndOwner := Handle;
      arrPasteEntries := @PasteFormats;
      cPasteEntries := PasteFormatCount;
      arrLinkTypes := @CFLinkSource;
      cLinkTypes := 1;
      dwFlags := PSF_SELECTPASTE;
    end;
  SetPasteEntry(PasteFormats[0], CFEmbeddedObject, TYMED_ISTORAGE, '', '',
    OLEUIPASTE_PASTE or OLEUIPASTE_ENABLEICON);
  SetPasteEntry(PasteFormats[1], CFLinkSource, TYMED_ISTREAM, '', '',
    OLEUIPASTE_LINKTYPE1 or OLEUIPASTE_ENABLEICON);
  SetPasteEntry(PasteFormats[2], CFRtf, TYMED_ISTORAGE,
    CF_RTF, CF_RTF, OLEUIPASTE_PASTE);
  SetPasteEntry(PasteFormats[3], CFRtfNoObjs, TYMED_ISTORAGE,
    CF_RTFNOOBJS, CF_RTFNOOBJS, OLEUIPASTE_PASTE);
  SetPasteEntry(PasteFormats[4], CF_TEXT, TYMED_HGLOBAL,
    'Unformatted text', 'text without any formatting', OLEUIPASTE_PASTE);
  SetPasteEntry(PasteFormats[5], CF_BITMAP, TYMED_GDI,
    'Windows Bitmap', 'bitmap image', OLEUIPASTE_PASTE);
  try
    if OleUIPasteSpecial(Data) = OLEUI_OK then
      begin
        Result := True;
        if Data.nSelectedIndex in [0, 1] then
          begin
            // CFEmbeddedObject, CFLinkSource
            FillChar(ReObject, SizeOf(TcaReObject), 0);
            IcaRichEditOle(FRichEditOle).GetClientSite(OleClientSite);
            Storage := nil;
            try
              CreateStorage(Storage);
              case Data.nSelectedIndex of
                0: OleCheck(OleCreateFromData(Data.lpSrcDataObj, IOleObject, OLERENDER_DRAW, nil, OleClientSite, Storage, OleObject));
                1: OleCheck(OleCreateLinkFromData(Data.lpSrcDataObj, IOleObject, OLERENDER_DRAW, nil, OleClientSite, Storage, OleObject));
              end;
              try
                with ReObject do
                  begin
                    cbStruct := SizeOf(TcaReObject);
                    cp := REO_CP_SELECTION;
                    poleobj := OleObject;
                    OleObject.GetUserClassID(clsid);
                    pstg := Storage;
                    polesite := OleClientSite;
                    dvAspect := DVASPECT_CONTENT;
                    dwFlags := REO_RESIZABLE;
                    OleCheck(OleSetDrawAspect(OleObject,
                      Data.dwFlags and PSF_CHECKDISPLAYASICON <> 0,
                      Data.hMetaPict, dvAspect));
                  end;
                SendMessage(Handle, EM_EXGETSEL, 0, Longint(@Selection));
                Selection.cpMax := Selection.cpMin + 1;
                OleCheck(IcaRichEditOle(FRichEditOle).InsertObject(ReObject));
                SendMessage(Handle, EM_EXSETSEL, 0, Longint(@Selection));
                IcaRichEditOle(FRichEditOle).SetDvaspect(
                  Longint(REO_IOB_SELECTION), ReObject.dvAspect);
              finally
                ReleaseObject(OleObject);
              end;
            finally
              ReleaseObject(OleClientSite);
              ReleaseObject(Storage);
            end;
          end
        else
          begin
            Format := PasteFormats[Data.nSelectedIndex].fmtetc.cfFormat;
            OleCheck(IcaRichEditOle(FRichEditOle).ImportDataObject(
              Data.lpSrcDataObj, Format, Data.hMetaPict));
          end;
        SendMessage(Handle, EM_SCROLLCARET, 0, 0);
      end;
  finally
    DestroyMetaPict(Data.hMetaPict);
    ReleaseObject(Data.lpSrcDataObj);
  end;
end;

function TcaCustomRichEdit.InsertObjectDialog: Boolean;
var
  Data: TOleUIInsertObject;
  NameBuffer: array[0..255] of Char;
  OleClientSite: IOleClientSite;
  Storage: IStorage;
  OleObject: IOleObject;
  ReObject: TcaReObject;
  IsNewObject: Boolean;
  Selection: TCharRange;
begin
  FillChar(Data, SizeOf(Data), 0);
  FillChar(NameBuffer, SizeOf(NameBuffer), 0);
  FillChar(ReObject, SizeOf(TcaReObject), 0);
  if Assigned(FRichEditOle) then
    begin
      IcaRichEditOle(FRichEditOle).GetClientSite(OleClientSite);
      Storage := nil;
      try
        CreateStorage(Storage);
        with Data do
          begin
            cbStruct := SizeOf(Data);
            dwFlags := IOF_SELECTCREATENEW or IOF_VERIFYSERVERSEXIST or
              IOF_CREATENEWOBJECT or IOF_CREATEFILEOBJECT or IOF_CREATELINKOBJECT;
            hWndOwner := Handle;
            lpszFile := NameBuffer;
            cchFile := SizeOf(NameBuffer);
            iid := IOleObject;
            oleRender := OLERENDER_DRAW;
            lpIOleClientSite := OleClientSite;
            lpIStorage := Storage;
            ppvObj := @OleObject;
          end;
        try
          Result := OleUIInsertObject(Data) = OLEUI_OK;
          if Result then
          try
            IsNewObject := Data.dwFlags and IOF_SELECTCREATENEW = IOF_SELECTCREATENEW;
            with ReObject do
              begin
                cbStruct := SizeOf(TcaReObject);
                cp := REO_CP_SELECTION;
                clsid := Data.clsid;
                poleobj := OleObject;
                pstg := Storage;
                polesite := OleClientSite;
                dvAspect := DVASPECT_CONTENT;
                dwFlags := REO_RESIZABLE;
                if IsNewObject then dwFlags := dwFlags or REO_BLANK;
                OleCheck(OleSetDrawAspect(OleObject,
                  Data.dwFlags and IOF_CHECKDISPLAYASICON <> 0,
                  Data.hMetaPict, dvAspect));
              end;
            SendMessage(Handle, EM_EXGETSEL, 0, Longint(@Selection));
            Selection.cpMax := Selection.cpMin + 1;
            OleCheck(IcaRichEditOle(FRichEditOle).InsertObject(ReObject));
            SendMessage(Handle, EM_EXSETSEL, 0, Longint(@Selection));
            SendMessage(Handle, EM_SCROLLCARET, 0, 0);
            IcaRichEditOle(FRichEditOle).SetDvaspect(
              Longint(REO_IOB_SELECTION), ReObject.dvAspect);
            if IsNewObject then
              OleObject.DoVerb(OLEIVERB_SHOW, nil,
                OleClientSite, 0, Handle, ClientRect);
          finally
            ReleaseObject(OleObject);
          end;
        finally
          DestroyMetaPict(Data.hMetaPict);
        end;
      finally
        ReleaseObject(OleClientSite);
        ReleaseObject(Storage);
      end;
    end
  else
    Result := False;
end;

function TcaCustomRichEdit.ObjectPropertiesDialog: Boolean;
var
  ObjectProps: TOleUIObjectProps;
  PropSheet: TPropSheetHeader;
  GeneralProps: TOleUIGnrlProps;
  ViewProps: TOleUIViewProps;
  LinkProps: TOleUILinkProps;
  DialogCaption: string;
  ReObject: TcaReObject;
begin
  Result := False;
  if not Assigned(FRichEditOle) or (SelectionType <> [stObject]) then Exit;
  FillChar(ObjectProps, SizeOf(ObjectProps), 0);
  FillChar(PropSheet, SizeOf(PropSheet), 0);
  FillChar(GeneralProps, SizeOf(GeneralProps), 0);
  FillChar(ViewProps, SizeOf(ViewProps), 0);
  FillChar(LinkProps, SizeOf(LinkProps), 0);
  FillChar(ReObject, SizeOf(ReObject), 0);
  ReObject.cbStruct := SizeOf(ReObject);
  if Succeeded(IcaRichEditOle(FRichEditOle).GetObject(Longint(REO_IOB_SELECTION),
    ReObject, REO_GETOBJ_POLEOBJ or REO_GETOBJ_POLESITE)) then
  try
    if ReObject.dwFlags and REO_INPLACEACTIVE = 0 then
      begin
        ObjectProps.cbStruct := SizeOf(ObjectProps);
        ObjectProps.dwFlags := OPF_DISABLECONVERT;
        ObjectProps.lpPS := @PropSheet;
        ObjectProps.lpObjInfo := TcaOleUIObjInfo.Create(Self, ReObject);
        if (ReObject.dwFlags and REO_LINK) <> 0 then
          begin
            ObjectProps.dwFlags := ObjectProps.dwFlags or OPF_OBJECTISLINK;
            ObjectProps.lpLinkInfo := TcaOleUILinkInfo.Create(Self, ReObject);
          end;
        ObjectProps.lpGP := @GeneralProps;
        ObjectProps.lpVP := @ViewProps;
        ObjectProps.lpLP := @LinkProps;
        PropSheet.dwSize := SizeOf(PropSheet);
        PropSheet.hWndParent := Handle;
        PropSheet.hInstance := MainInstance;
        DialogCaption := Format(ResStr(SPropDlgCaption), [GetFullNameStr(ReObject.poleobj)]);
        PropSheet.pszCaption := PChar(DialogCaption);
        GeneralProps.cbStruct := SizeOf(GeneralProps);
        ViewProps.cbStruct := SizeOf(ViewProps);
        ViewProps.dwFlags := VPF_DISABLESCALE;
        LinkProps.cbStruct := SizeOf(LinkProps);
        LinkProps.dwFlags := ELF_DISABLECANCELLINK;
        Result := OleUIObjectProperties(ObjectProps) = OLEUI_OK;
      end;
  finally
  end;
end;

procedure TcaCustomRichEdit.Print(const Caption: string);
var
  Range: TFormatRange;
  LastChar, MaxLen, LogX, LogY, OldMap: Integer;
  SaveRect: TRect;
  TextLenEx: TGetTextLengthEx;
begin
  FillChar(Range, SizeOf(TFormatRange), 0);
  with Printer, Range do
    begin
      Title := Caption;
      BeginDoc;
      hdc := Handle;
      hdcTarget := hdc;
      LogX := GetDeviceCaps(Handle, LOGPIXELSX);
      LogY := GetDeviceCaps(Handle, LOGPIXELSY);
      if IsRectEmpty(PageRect) then
        begin
          rc.right := PageWidth * 1440 div LogX;
          rc.bottom := PageHeight * 1440 div LogY;
        end
      else
        begin
          rc.left := PageRect.Left * 1440 div LogX;
          rc.top := PageRect.Top * 1440 div LogY;
          rc.right := PageRect.Right * 1440 div LogX;
          rc.bottom := PageRect.Bottom * 1440 div LogY;
        end;
      rcPage := rc;
      SaveRect := rc;
      LastChar := 0;
      if RichEditVersion >= 2 then
        begin
          with TextLenEx do
            begin
              flags := GTL_DEFAULT;
              codepage := CP_ACP;
            end;
          MaxLen := Perform(EM_GETTEXTLENGTHEX, WParam(@TextLenEx), 0);
        end
      else
        MaxLen := GetTextLen;
      chrg.cpMax := -1;
      // ensure printer DC is in text map mode
      OldMap := SetMapMode(hdc, MM_TEXT);
      SendMessage(Self.Handle, EM_FORMATRANGE, 0, 0);   // flush buffer
      try
        repeat
          rc := SaveRect;
          chrg.cpMin := LastChar;
          LastChar := SendMessage(Self.Handle, EM_FORMATRANGE, 1, Longint(@Range));
          if (LastChar < MaxLen) and (LastChar <> -1) then NewPage;
        until (LastChar >= MaxLen) or (LastChar = -1);
        EndDoc;
      finally
        SendMessage(Self.Handle, EM_FORMATRANGE, 0, 0);   // flush buffer
        SetMapMode(hdc, OldMap); 
      end;
    end;
end;

var
  Painting: Boolean = False;

procedure TcaCustomRichEdit.WMPaint(var Message: TWMPaint);
var
  R, R1: TRect;
begin
  if RichEditVersion >= 2 then
    inherited
  else
    begin
      if GetUpdateRect(Handle, R, True) then
        begin
          with ClientRect do
            R1 := Rect(Right - 3, Top, Right, Bottom);
          if IntersectRect(R, R, R1) then InvalidateRect(Handle, @R1, True);
        end;
      if Painting then
        Invalidate
      else
        begin
          Painting := True;
          try
            inherited;
          finally
            Painting := False;
          end;
        end;
    end;
end;

procedure TcaCustomRichEdit.WMDestroy(var Msg: TWMDestroy);
begin
  CloseObjects;
  ReleaseObject(FRichEditOle);
  inherited;
end;

procedure TcaCustomRichEdit.WMMouseMove(var Message: TMessage);
begin
  inherited;
end;

procedure TcaCustomRichEdit.WMSetCursor(var Message: TWMSetCursor);
begin
  inherited;
end;

procedure TcaCustomRichEdit.WMRButtonUp(var Message: TMessage);
begin
  // RichEd20 does not pass the WM_RBUTTONUP message to defwndproc,
  // so we get no WM_CONTEXTMENU message. Simulate message here.  
  if Win32MajorVersion < 5 then
    Perform(WM_CONTEXTMENU, Handle, LParam(PointToSmallPoint(
      ClientToScreen(SmallPointToPoint(TWMMouse(Message).Pos)))));
  inherited;
end;

procedure TcaCustomRichEdit.CNNotify(var Message: TWMNotify);
var
  AMsg: TMessage;
begin
  with Message do
    case NMHdr^.code of
      EN_SELCHANGE: SelectionChange;
      EN_REQUESTRESIZE: RequestSize(PReqSize(NMHdr)^.rc);
      EN_SAVECLIPBOARD:
        with PENSaveClipboard(NMHdr)^ do
          if not SaveClipboard(cObjectCount, cch) then Result := 1;
      EN_PROTECTED:
        with PENProtected(NMHdr)^ do
          begin
            AMsg.Msg := Msg;
            AMsg.WParam := WParam;
            AMsg.LParam := LParam;
            AMsg.Result := 0;
            if not ProtectChange(AMsg, chrg.cpMin, chrg.cpMax) then
              Result := 1;
          end;
      EN_LINK:
        with PcaENLink(NMHdr)^ do
          begin
            case Msg of
              WM_RBUTTONDOWN:
                begin
                  FClickRange := chrg;
                  FClickBtn := mbRight;
                end;
              WM_RBUTTONUP:
                begin
                  if (FClickBtn = mbRight) and (FClickRange.cpMin = chrg.cpMin) and
                    (FClickRange.cpMax = chrg.cpMax) then
                    URLClick(GetTextRange(chrg.cpMin, chrg.cpMax), mbRight);
                  with FClickRange do
                    begin
                      cpMin := -1;
                      cpMax := -1;
                    end;
                end;
              WM_LBUTTONDOWN:
                begin
                  FClickRange := chrg;
                  FClickBtn := mbLeft;
                end;
              WM_LBUTTONUP:
                begin
                  if (FClickBtn = mbLeft) and (FClickRange.cpMin = chrg.cpMin) and
                    (FClickRange.cpMax = chrg.cpMax) then
                    URLClick(GetTextRange(chrg.cpMin, chrg.cpMax), mbLeft);
                  with FClickRange do
                    begin
                      cpMin := -1;
                      cpMax := -1;
                    end;
                end;
            end;
          end;
      EN_STOPNOUNDO:
        begin
          // cannot allocate enough memory to maintain the undo state
        end;
    end;
end;

function TcaCustomRichEdit.SaveClipboard(NumObj, NumChars: Integer): Boolean;
begin
  Result := True;
  if Assigned(OnSaveClipboard) then
    OnSaveClipboard(Self, NumObj, NumChars, Result);
end;

function TcaCustomRichEdit.ProtectChange(const Message: TMessage; StartPos,
  EndPos: Integer): Boolean;
begin
  Result := False;
  if Assigned(OnProtectChangeEx) then
    OnProtectChangeEx(Self, Message, StartPos, EndPos, Result)
  else if Assigned(OnProtectChange) then
    OnProtectChange(Self, StartPos, EndPos, Result);
end;

procedure TcaCustomRichEdit.SelectionChange;
begin
  if Assigned(OnSelectionChange) then OnSelectionChange(Self);
end;

procedure TcaCustomRichEdit.RequestSize(const Rect: TRect);
begin
  if Assigned(OnResizeRequest) then OnResizeRequest(Self, Rect);
end;

procedure TcaCustomRichEdit.URLClick(const URLText: string; Button: TMouseButton);
begin
  if Assigned(OnURLClick) then OnURLClick(Self, URLText, Button);
end;

function TcaCustomRichEdit.FindText(const SearchStr: string;
  StartPos, Length: Integer; Options: TcaRichSearchTypes): Integer;
var
  Find: TcaFindTextEx;
  Flags: Integer;
begin
  with Find.chrg do
    begin
      cpMin := StartPos;
      cpMax := cpMin + Abs(Length);
    end;
  if RichEditVersion >= 2 then
    begin
      if not (stBackward in Options) then
        Flags := FT_DOWN
      else
        Flags := 0;
    end
  else
    begin
      Options := Options - [stBackward];
      Flags := 0;
    end;
  if stWholeWord in Options then Flags := Flags or FT_WHOLEWORD;
  if stMatchCase in Options then Flags := Flags or FT_MATCHCASE;
  Find.lpstrText := PChar(SearchStr);
  Result := SendMessage(Handle, EM_FINDTEXTEX, Flags, Longint(@Find));
  if (Result >= 0) and (stSetSelection in Options) then
    begin
      SendMessage(Handle, EM_EXSETSEL, 0, Longint(@Find.chrgText));
      SendMessage(Handle, EM_SCROLLCARET, 0, 0);
    end;
end;

procedure TcaCustomRichEdit.ClearUndo;
begin
  SendMessage(Handle, EM_EMPTYUNDOBUFFER, 0, 0);
end;

procedure TcaCustomRichEdit.Redo;
begin
  SendMessage(Handle, EM_REDO, 0, 0);
end;

procedure TcaCustomRichEdit.Undo;
begin
  SendMessage(Handle, WM_UNDO, 0, 0);
end;

procedure TcaCustomRichEdit.StopGroupTyping;
begin
  if (RichEditVersion >= 2) and HandleAllocated then
    SendMessage(Handle, EM_STOPGROUPTYPING, 0, 0);
end;

procedure TcaCustomRichEdit.SetUIActive(Active: Boolean);
var
  Form: TCustomForm;
begin
  try
    Form := GetParentForm(Self);
    if Form <> nil then
      if Active then
        begin
          if (Form.ActiveOleControl <> nil) and
            (Form.ActiveOleControl <> Self) then
            Form.ActiveOleControl.Perform(CM_UIDEACTIVATE, 0, 0);
          Form.ActiveOleControl := Self;
          if AllowInPlace and CanFocus then SetFocus;
        end
      else
        begin
          if Form.ActiveOleControl = Self then Form.ActiveOleControl := nil;
          if (Form.ActiveControl = Self) and AllowInPlace then
            begin
              Windows.SetFocus(Handle);
              SelectionChange;
            end;
        end;
  except
    Application.HandleException(Self);
  end;
end;

procedure TcaCustomRichEdit.CMDocWindowActivate(var Message: TMessage);
begin
  if Assigned(FCallback) then
    with TcaRichEditOleCallback(FCallback) do
      if Assigned(FDocForm) and IsFormMDIChild(FDocForm.Form) then
        begin
          if Message.WParam = 0 then
            begin
              FFrameForm.SetMenu(0, 0, 0);
              FFrameForm.ClearBorderSpace;
            end;
        end;
end;

procedure TcaCustomRichEdit.CMUIDeactivate(var Message: TMessage);
begin
  if (GetParentForm(Self) <> nil) and Assigned(FRichEditOle) and
    (GetParentForm(Self).ActiveOleControl = Self) then ;
end;

procedure TcaCustomRichEdit.CMTextChanged(var Message: TMessage); 
begin
  inherited;
  DoTextChanged;
end;

 //---------------------------------------------------------------------------
 // Find & Replace Dialogs
 //---------------------------------------------------------------------------

procedure TcaCustomRichEdit.SetupFindDialog(Dialog: TFindDialog; const SearchStr, ReplaceStr: string);
begin
  with Dialog do
    begin
      if SearchStr <> '' then FindText := SearchStr;
      if RichEditVersion = 1 then
        Options := Options + [frHideUpDown, frDown];
      OnFind := FindDialogFind;
      OnClose := FindDialogClose;
    end;
  if Dialog is TReplaceDialog then
    with TReplaceDialog(Dialog) do
      begin
        if ReplaceStr <> '' then ReplaceText := ReplaceStr;
        OnReplace := ReplaceDialogReplace;
      end;
end;

function TcaCustomRichEdit.FindDialog(const SearchStr: string): TFindDialog;
begin
  if FFindDialog = nil then
    begin
      FFindDialog := TFindDialog.Create(Self);
      if FReplaceDialog <> nil then
        FFindDialog.FindText := FReplaceDialog.FindText;
    end;
  Result := FFindDialog;
  SetupFindDialog(FFindDialog, SearchStr, '');
  FFindDialog.Execute;
end;

function TcaCustomRichEdit.ReplaceDialog(const SearchStr,
  ReplaceStr: string): TReplaceDialog;
begin
  if FReplaceDialog = nil then
    begin
      FReplaceDialog := TReplaceDialog.Create(Self);
      if FFindDialog <> nil then
        FReplaceDialog.FindText := FFindDialog.FindText;
    end;
  Result := FReplaceDialog;
  SetupFindDialog(FReplaceDialog, SearchStr, ReplaceStr);
  FReplaceDialog.Execute;
end;

function TcaCustomRichEdit.GetCanFindNext: Boolean;
begin
  Result := HandleAllocated and (FLastFind <> nil) and
    (FLastFind.FindText <> '');
end;

function TcaCustomRichEdit.FindNext: Boolean;
begin
  if CanFindNext then
    Result := FindEditText(FLastFind, False, True)
  else
    Result := False;
end;

procedure TcaCustomRichEdit.AdjustFindDialogPosition(Dialog: TFindDialog);
var
  TextRect, R: TRect;
begin
  if Dialog.Handle = 0 then Exit;
  with TextRect do
    begin
      TopLeft := ClientToScreen(GetCharPos(SelStart));
      BottomRight := ClientToScreen(GetCharPos(SelStart + SelLength));
      Inc(Bottom, 20);
    end;
  with Dialog do
    begin
      GetWindowRect(Handle, R);
      if PtInRect(R, TextRect.TopLeft) or PtInRect(R, TextRect.BottomRight) then
        begin
          if TextRect.Top > R.Bottom - R.Top + 20 then
            OffsetRect(R, 0, TextRect.Top - R.Bottom - 20)
          else
            begin
              if TextRect.Top + R.Bottom - R.Top < GetSystemMetrics(SM_CYSCREEN) then
                OffsetRect(R, 0, 40 + TextRect.Top - R.Top);
            end;
          Position := R.TopLeft;
        end;
    end;
end;

function TcaCustomRichEdit.FindEditText(Dialog: TFindDialog; AdjustPos, Events: Boolean): Boolean;
var
  Length, StartPos: Integer;
  SrchOptions: TcaRichSearchTypes;
begin
  with TFindDialog(Dialog) do
    begin
      SrchOptions := [stSetSelection];
      if frDown in Options then
        begin
          StartPos := Max(SelStart, SelStart + SelLength);
          Length := System.Length(Text) - StartPos + 1;
        end
      else
        begin
          SrchOptions := SrchOptions + [stBackward];
          StartPos := Min(SelStart, SelStart + SelLength);
          Length := StartPos + 1;
        end;
      if frMatchCase in Options then
        SrchOptions := SrchOptions + [stMatchCase];
      if frWholeWord in Options then
        SrchOptions := SrchOptions + [stWholeWord];
      Result := Self.FindText(FindText, StartPos, Length, SrchOptions) >= 0;
      if FindText <> '' then FLastFind := Dialog;
      if Result then
        begin
          if AdjustPos then AdjustFindDialogPosition(Dialog);
        end
      else if Events then
        TextNotFound(Dialog);
    end;
end;

procedure TcaCustomRichEdit.TextNotFound(Dialog: TFindDialog);
begin
  with Dialog do
    if Assigned(FOnTextNotFound) then FOnTextNotFound(Self, FindText);
end;

procedure TcaCustomRichEdit.FindDialogFind(Sender: TObject);
begin
  FindEditText(TFindDialog(Sender), True, True);
end;

procedure TcaCustomRichEdit.ReplaceDialogReplace(Sender: TObject);
var
  Cnt: Integer;
  SaveSelChange: TNotifyEvent;
begin
  with TReplaceDialog(Sender) do
    begin
      if (frReplaceAll in Options) then
        begin
          Cnt := 0;
          SaveSelChange := FOnSelChange;
          TcaRichEditStrings(Lines).EnableChange(False);
          try
            FOnSelChange := nil;
            while FindEditText(TFindDialog(Sender), False, False) do
              begin
                SelText := ReplaceText;
                Inc(Cnt);
              end;
            if Cnt = 0 then
              TextNotFound(TFindDialog(Sender))
            else
              AdjustFindDialogPosition(TFindDialog(Sender));
          finally
            TcaRichEditStrings(Lines).EnableChange(True);
            FOnSelChange := SaveSelChange;
            if Cnt > 0 then
              begin
                Change;
                SelectionChange;
              end;
          end;
        end
      else if (frReplace in Options) then
        begin
          if FindEditText(TFindDialog(Sender), True, True) then
            SelText := ReplaceText;
        end;
    end;
end;

procedure TcaCustomRichEdit.FindDialogClose(Sender: TObject);
begin
  CloseFindDialog(Sender as TFindDialog);
end;

procedure TcaCustomRichEdit.CloseFindDialog(Dialog: TFindDialog);
begin
  if Assigned(FOnCloseFindDialog) then FOnCloseFindDialog(Self, Dialog);
end;

 //---------------------------------------------------------------------------
 // Conversion formats
 //---------------------------------------------------------------------------

procedure AppendConversionFormat(const Ext: string; Plain: Boolean;
  AClass: TConversionClass);
var
  NewRec: PcaRichConversionFormat;
begin
  New(NewRec);
  with NewRec^ do
    begin
      Extension := AnsiLowerCaseFileName(Ext);
      PlainText := Plain;
      ConversionClass := AClass;
      Next := ConversionFormatList;
    end;
  ConversionFormatList := NewRec;
end;

class procedure TcaCustomRichEdit.RegisterConversionFormat(const AExtension: string;
  APlainText: Boolean; AConversionClass: TConversionClass);
begin
  AppendConversionFormat(AExtension, APlainText, AConversionClass);
end;

 //---------------------------------------------------------------------------
 // Initialization part
 //---------------------------------------------------------------------------

var
  OldError: Longint;
  FLibHandle: THandle;
  Ver: TOsVersionInfo;

initialization
  RichEditVersion := 1;
  OldError := SetErrorMode(SEM_NOOPENFILEERRORBOX);
  try
{$IFNDEF RICHEDIT_VER_10}
    FLibHandle := LoadLibrary(RichEdit20ModuleName);
    if (FLibHandle > 0) and (FLibHandle < HINSTANCE_ERROR) then FLibHandle := 0;
{$ELSE}
    FLibHandle := 0;
{$ENDIF}
    if FLibHandle = 0 then
      begin
        FLibHandle := LoadLibrary(RichEdit10ModuleName);
        if (FLibHandle > 0) and (FLibHandle < HINSTANCE_ERROR) then FLibHandle := 0;
      end
    else
      begin
        RichEditVersion := 2;
        Ver.dwOSVersionInfoSize := SizeOf(Ver);
        GetVersionEx(Ver);
        with Ver do
          begin
            if (dwPlatformId = VER_PLATFORM_WIN32_NT) and
              (dwMajorVersion >= 5) then
              RichEditVersion := 3;
          end;
      end;
  finally
    SetErrorMode(OldError);
  end;
  CFEmbeddedObject := RegisterClipboardFormat(CF_EMBEDDEDOBJECT);
  CFLinkSource := RegisterClipboardFormat(CF_LINKSOURCE);
  CFRtf := RegisterClipboardFormat(CF_RTF);
  CFRtfNoObjs := RegisterClipboardFormat(CF_RTFNOOBJS);
finalization
  if FLibHandle <> 0 then FreeLibrary(FLibHandle);
end.

