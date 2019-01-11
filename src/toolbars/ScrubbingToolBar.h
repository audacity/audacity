/**********************************************************************

 Audacity: A Digital Audio Editor


 ScrubbingToolbar.h
 
 Paul Licameli

 **********************************************************************/

#ifndef __AUDACITY_SCRUBBING_TOOLBAR__
#define __AUDACITY_SCRUBBING_TOOLBAR__



#include <wx/defs.h>

#include "ToolBar.h"
#include "../Theme.h"

class wxCommandEvent;
class wxDC;
class wxImage;
class wxWindow;

class AButton;

enum {
   STBScrubID,
   STBSeekID,
   STBRulerID,

   STBNumButtons,
   STBFirstButton = STBScrubID
};

class ScrubbingToolBar final : public ToolBar {

public:

   ScrubbingToolBar();
   virtual ~ScrubbingToolBar();

   void Create(wxWindow *parent) override;

   void OnButton(wxCommandEvent & event);

   void Populate() override;
   void Repaint(wxDC * WXUNUSED(dc)) override {};
   void EnableDisableButtons() override;
   void UpdatePrefs() override;

   void RegenerateTooltips() override;

private:

   static AButton *AddButton(
      ScrubbingToolBar *pBar,
      teBmps eEnabledUp, teBmps eEnabledDown, teBmps eDisabled,
      int id, const wxChar *label, bool toggle = false);

   void MakeButtons();

   AButton *mButtons[STBNumButtons];

   wxImage *upImage;
   wxImage *downImage;
   wxImage *hiliteImage;

public:

   DECLARE_CLASS(ScrubbingToolBar)
   DECLARE_EVENT_TABLE()
};

#endif
