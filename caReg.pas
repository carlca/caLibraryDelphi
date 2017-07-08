{ This file is part of the caLibrary (for Delphi 7) package

  Copyright (C) 1999-2017 - Carl Caulkett - carl.caulkett@gmail.com

  MODIFIED LGPL Licence - this is the same licence as that used by the Free Pascal Compiler (FPC)
  A copy of the full licence can be found in the file Licence.md in the same folder as this file.

  This library is free software; you can redistribute it and/or modify it under the terms of the GNU Library General Public
  License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version
  with the following modification:

  As a special exception, the copyright holders of this library give you permission to link this library with independent
  modules to produce an executable, regardless of the license terms of these independent modules, and to copy and distribute the
  resulting executable under terms of your choice, provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a module which is not derived from or based on this
  library. If you modify this library, you may extend this exception to your version of the library, but you are not obligated
  to do so. If you do not wish to do so, delete this exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License for more details.

  You should have received a copy of the GNU Library General Public License along with this library; if not, write to the Free
  Software Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}


unit caReg;

{$INCLUDE ca.inc}

interface

procedure Register;

implementation

{$R CACONTROLS.DCR}
{$R CAMENU.DCR}

uses
  Classes,
  {$IFDEF D7_UP}
  DesignIntf,
  DesignEditors,
  VCLEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF}
  ca8087,
  caActionMgr,
  caActionMgrDsgn,
  caActionMgrForm,
  caButtons,
  caChangeParent,
  caChart,
  caChartLegendListBox,
  caCheckBox,
  caClasses,
  caColorPicker,
  caControls,
  caDiagram,
  caDirListView,
  caEdit,
  caEditListBox,
  caFlyout,
  caFont,
  caFormInitializer,
  caFormHook,
  caForms,
  caFormScaler,
  caKeyMonitor,
  caLabel,
  caListView,
  caLocalizer,
  caLog,
  caMarkup,
  // caMemMapImage,
  // caMemMapImageDsgn,
  caMenu,
  caMenuBar,
  caMenuDsgn,
  caNotifyIcon,
  caPageControl,
  caPopupMenu,
  caProgress,
  caRadioButton,
  caRichEd,
  caSizeBar,
  caSizeMove,  
  caSizeMovePanel,
  caSystemMenu,
  caTextStore,
  caTextStoreDsgn,
  caTranslate,
  caTransparentWnd,
  caTrayIcon,
  caTreeView,
  caXMLData,
  caXPControls;

procedure Register;
begin
  RegisterComponents('ca', [
    Tca8087,
    TcaActionManager,
    TcaChangeParent,
    TcaChart,
    TcaChartLegendListBox,
    TcaCheckbox,
    TcaColorPicker,
    TcaComboBox,
    TcaDFMFile,
    TcaDiagram,
    // TcaDiagramElement,
    TcaDiagramController,
    TcaDiagramBox,
    TcaDiagramLineNode,
    TcaDirListView,
    TcaEdit,
    TcaEditListBox,
    TcaFlyout,
    TcaFontController,
    TcaFormInitializer,
    TcaFormHook,
    TcaFormPanel,
    TcaFormScaler,
    TcaGraphicBox,
    TcaKeyMonitor,
    TcaLabel,
    TcaListView,
    TcaLocalizer,
    TcaLogComponent,
    TcaMarkupViewer,
    // TcaMemMapImage,
    TcaMenu,
    TcaMenuBar,
    TcaMenuPage,
    TcaMenuItemButton,
    TcaNotifyIcon,
    TcaPageControl,
    TcaPanel,
    TcaPopupMenu,
    TcaProgressBar,
    TcaRadioButton,
    TcaRepeatSpeedButton,
    TcaRichEdit,
    TcaSizeBar,
    TcaSizeMoveController,
    TcaSizeMovePanel,
    TcaSpacer,
    TcaSpeedButton,
    TcaSplitter,
    TcaSystemMenu,
    TcaTextStore,
    TcaTransparentWnd,
    TcaTrayIcon,
    TcaTreeView,
    TcaXMLDataset,
    TcaXPButton,
    TcaXPCheckbox,
    TcaXPMutex
  ]);
  RegisterNoIcon([TcaTextItem]);
  RegisterComponentEditor(TcaMenu, TcaMenuCompEd);
  RegisterComponentEditor(TcaMenuPage, TcaMenuCompEd);
  RegisterComponentEditor(TcaActionManager, TcaActionMgrCompEd);
  RegisterComponentEditor(TcaTextStore, TcaTextStoreComponentEditor);
  // RegisterPropertyEditor(TypeInfo(TcaBMPFilename), nil, '', TcaBMPFilenameProperty);
  {$IFDEF D7_UP}
  RegisterPropertyInCategory('Visual', TcaPageControl, 'Mode');
  {$ELSE}
  RegisterPropertyInCategory(TVisualCategory, TcaPageControl, 'Mode');
  {$ENDIF}
end;

end.

