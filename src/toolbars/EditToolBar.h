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
#include "../Theme.h"
#include "../Experimental.h"

class wxCommandEvent;
class wxDC;
class wxImage;
class wxWindow;

class AButton;

enum {
   ETBCutID,
   ETBCopyID,
   ETBPasteID,
   ETBTrimID,
   ETBSilenceID,

   ETBUndoID,
   ETBRedoID,

#ifdef EXPERIMENTAL_SYNC_LOCK
   ETBSyncLockID,
#endif

   ETBZoomInID,
   ETBZoomOutID,

   #if 0 // Disabled for version 1.2.0 since it doesn't work quite right...
   ETBZoomToggleID,
   #endif

   ETBZoomSelID,
   ETBZoomFitID,

#if defined(EXPERIMENTAL_EFFECTS_RACK)
   ETBEffectsID,
#endif

   ETBNumButtons
};

class EditToolBar final : public ToolBar {

 public:

   EditToolBar();
   virtual ~EditToolBar();

   void Create(wxWindow *parent);

   void OnButton(wxCommandEvent & event);

   void Populate();
   void Repaint(wxDC * WXUNUSED(dc)) {};
   void EnableDisableButtons();
   void UpdatePrefs();

 private:

   AButton *AddButton(teBmps eEnabledUp, teBmps eEnabledDown, teBmps eDisabled,
      int id, const wxChar *label, bool toggle = false);

   void AddSeparator();

   void MakeButtons();

   void RegenerateTooltips() override;

   AButton *mButtons[ETBNumButtons];

   wxImage *upImage;
   wxImage *downImage;
   wxImage *hiliteImage;

 public:

   DECLARE_CLASS(EditToolBar)
   DECLARE_EVENT_TABLE()
};

#endif

