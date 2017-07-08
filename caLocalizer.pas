unit caLocalizer;

interface

uses

  // Standard Delphi units 
  SysUtils,
  Classes,
  Controls,
  Contnrs,
  TypInfo,
  Forms,
  Dialogs,

  // ca units 
  caClasses,
  caUtils,
  caForms;

type

  //---------------------------------------------------------------------------
  // TcaLocalizer                                                              
  //---------------------------------------------------------------------------

  TcaLocalizer = class(TComponent)
  private
    // Private fields 
    // Property methods 
    // Private methods 
  protected
    // Protected virtual methods 
  public
    // Create/Destroy 
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Public methods 
  published
    // Properties 
  end;

implementation

uses caLocalizerForm;

  //---------------------------------------------------------------------------
  // TcaLocalizer                                                              
  //---------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaLocalizer.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TcaLocalizer.Destroy;
begin
  inherited;
end;

  // Public methods 


  // Protected virtual methods 


  // Private methods 


  // Property methods 

end.
