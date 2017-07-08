unit caDict;

{$INCLUDE ca.inc}

interface

uses
  // Standard Delphi units
  SysUtils,
  Classes,
  Math,

  // ca units
  caClasses,
  caPrimes,
  caUtils;

type

 //---------------------------------------------------------------------------
 // TcaDictKey
 //---------------------------------------------------------------------------

  TcaDictKey = class(TObject)
  protected
    // Virtual methods
    function GetAsString: String; virtual; abstract;
  public
    // Properties
    property AsString: String read GetAsString;
  end;

 //---------------------------------------------------------------------------
 // IcaDict
 //---------------------------------------------------------------------------

  IcaDict = interface
  ['{11A4441E-7241-4C09-8A7F-99ACE25178EC}']

  end;

 //---------------------------------------------------------------------------
 // TcaDict
 //---------------------------------------------------------------------------

  TcaDict = class(TcaInterfacedPersistent, IcaDict)
  private
  protected
  public
    // function Get(AKey: TcaDictKey):
  end;

implementation

end.
