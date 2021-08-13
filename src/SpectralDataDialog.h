/**********************************************************************

  Audacity: A Digital Audio Editor

  SpectralDataDialog.h

  Edward Hui

**********************************************************************/

#ifndef __AUDACITY_SPECTRAL_DATA_DIALOG__
#define __AUDACITY_SPECTRAL_DATA_DIALOG__

#include "ClientData.h"
#include "Prefs.h"
#include "widgets/wxPanelWrapper.h" // to inherit
#include "commands/CommandManagerWindowClasses.h"

class wxButton;
class wxCheckBox;
class wxListCtrl;
class wxListEvent;
class wxSpinCtrl;
class wxTextCtrl;
class AudacityProject;
class ShuttleGui;
class UndoManager;

class SpectralDataDialog final : public wxDialogWrapper,
                                 public PrefsListener,
                                 public ClientData::Base,
                                 public TopLevelKeystrokeHandlingWindow
{

public:
   SpectralDataDialog(AudacityProject * parent);

   void UpdateDisplay(wxEvent &e);

   bool Show( bool show = true ) override;

private:
   void Populate(ShuttleGui & S);

   void OnAudioIO(wxCommandEvent & evt);
   void DoUpdate();

   void OnShow(wxShowEvent &event);
   void OnCloseWindow(wxCloseEvent &event);
   void OnApply(wxCommandEvent &event);
   void OnBrushSizeSlider(wxCommandEvent &event);
   void OnCheckSmartSelection(wxCommandEvent &event);
   void OnCheckOvertones(wxCommandEvent &event);

   // PrefsListener implementation
   void UpdatePrefs() override;

   AudacityProject   *mProject;
   wxButton          *mApplyBtn;

   int               mSelected;
   bool              mAudioIOBusy;

public:
DECLARE_EVENT_TABLE()
};

class AUDACITY_DLL_API SpectralDataDialogWorker final
      : public ClientData::Base{
public:
   explicit SpectralDataDialogWorker( AudacityProject &project );
   static SpectralDataDialogWorker &Get(AudacityProject &project);

   void OnToolChanged(wxCommandEvent &evt);

private:
   AudacityProject *mProject;
};
#endif
