unit caIntf;

interface

uses

  // Standard Delphi units 
  Classes,
  Sysutils,
  // sIntfInfo,
  // Invoker,
  TypInfo,

  // ca units 
  caClasses,
  caLog;

type

  //----------------------------------------------------------------------------
  // TcaIntfMethod                                                              
  //----------------------------------------------------------------------------

  TcaIntfMethod = class(TObject)
  private
  protected
  public
    // constructor Create(const AMetaData: TIntfMetaData);
  end;

  //----------------------------------------------------------------------------
  // IcaIntfInfo                                                                
  //----------------------------------------------------------------------------

  IcaIntfInfo = interface
  ['{9DB26BE5-1EFA-4CDD-944E-715A7AFDF1F9}']
    // Interface methods 
    procedure GetMethodsAsStrings(AStrings: TStrings);
  end;

  //----------------------------------------------------------------------------
  // TcaIntfInfo                                                                
  //----------------------------------------------------------------------------

  TcaIntfInfo = class(TInterfacedObject, IcaIntfInfo)
  private
    // Private fields 
//    FIntf: IUnknown;
//    FMetaData: TIntfMetaData;
  protected
  public
    // Create/Destroy 
    // constructor Create(const AIntf: IUnknown; const ATypeInfo: PTypeInfo);
    // Interface methods - IcaIntfInfo 
    procedure GetMethodsAsStrings(AStrings: TStrings);
  end;

implementation

  //----------------------------------------------------------------------------
  // TcaIntfMethod                                                              
  //----------------------------------------------------------------------------

//  constructor TcaIntfMethod.Create(const AMetaData: TIntfMetaData);
//  begin
//
//  end;

  // Create/Destroy 

//  constructor TcaIntfInfo.Create(const AIntf: IUnknown; const ATypeInfo: PTypeInfo);
//  begin
//    inherited Create;
//    FIntf := AIntf;
//    GetIntfMetaData(ATypeInfo, FMetaData);
//  end;

  // Public methods 

procedure TcaIntfInfo.GetMethodsAsStrings(AStrings: TStrings);
//var
//  Index: Integer;
//  Method: TIntfMethEntry;
//  Methods: TIntfMethEntryArray;
begin
//  AStrings.Clear;
//  Methods := FMetaData.MDA;
//  for Index := Low(Methods) to High(Methods) do
//    begin
//      Method := Methods[Index];
//      AStrings.Add(Method.Name);
//    end;
end;

end.
