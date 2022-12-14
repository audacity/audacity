/**********************************************************************

  Audacity: A Digital Audio Editor


  EditToolbar.h

  Dominic Mazzoni
  Shane T. Mueller
  Leland Lucius

**********************************************************************/

#ifndef __AUDACITY_EDIT_TOOLBAR__
#define __AUDACITY_EDIT_TOOLBAR__

#include <wx/defs.h>

#include "ToolBar.h"
#include "ToolBarButtons.h"

class wxCommandEvent;
class wxDC;
class wxGridSizer;
class wxImage;
class wxWindow;

class AButton;

enum {
   ETBZoomInID,
   ETBZoomOutID,
#ifdef EXPERIMENTAL_ZOOM_TOGGLE_BUTTON
   ETBZoomToggleID,
#endif

   ETBZoomSelID,
   ETBZoomFitID,

   ETBTrimID,
   ETBSilenceID,

#ifdef EXPERIMENTAL_SYNC_LOCK
   //Undefined, so no sync-lock on/off button.
   //#define OPTION_SYNC_LOCK_BUTTON
#endif

#ifdef OPTION_SYNC_LOCK_BUTTON
   ETBSyncLockID,
#endif

   ETBUndoID,
   ETBRedoID,

   ETBNumButtons
};

class EditToolBar final : public ToolBar {

 public:

   EditToolBar( AudacityProject &project );
   virtual ~EditToolBar();

   void Create(wxWindow *parent) override;

   void OnButton(wxCommandEvent & event);

   void Populate() override;
   void Repaint(wxDC * WXUNUSED(dc)) override {};
   void EnableDisableButtons() override;
   void UpdatePrefs() override;

 private:

   void AddButton(
      teBmps eEnabledUp, teBmps eEnabledDown, teBmps eDisabled,
      int firstToolBarId, int thisButtonId,
      const TranslatableString &label, bool toggle = false);

   void AddSeparator();

   void RegenerateTooltips() override;
   void ForAllButtons(int Action);

   AButton *mButtons[ETBNumButtons];

   wxGridSizer* mToolSizer;

 public:

   DECLARE_CLASS(EditToolBar)
   DECLARE_EVENT_TABLE()
};

#endif

