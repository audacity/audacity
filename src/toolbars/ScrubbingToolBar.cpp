/**********************************************************************

 Audacity: A Digital Audio Editor

 ScrubbingToolBar.cpp

 Paul Licameli

 See ScrubbingToolBar.h for details

 *******************************************************************/

// Much of this is imitative of EditToolBar.  Should there be a common base
// class?

#include "../Audacity.h"
#include "ScrubbingToolBar.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#include <wx/setup.h> // for wxUSE_* macros

#ifndef WX_PRECOMP
#include <wx/event.h>
#include <wx/image.h>
#include <wx/intl.h>
#include <wx/sizer.h>
#include <wx/tooltip.h>
#endif

#include "../AdornedRulerPanel.h"
#include "../AllThemeResources.h"
#include "../AudioIO.h"
#include "../ImageManipulation.h"
#include "../Internat.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../Theme.h"
#include "../Track.h"
#include "../UndoManager.h"
#include "../widgets/AButton.h"
#include "../tracks/ui/Scrubbing.h"
#include "../commands/CommandContext.h"

IMPLEMENT_CLASS(ScrubbingToolBar, ToolBar);

//const int BUTTON_WIDTH = 27;
//const int SEPARATOR_WIDTH = 14;

////////////////////////////////////////////////////////////
/// Methods for ScrubbingToolBar
////////////////////////////////////////////////////////////

BEGIN_EVENT_TABLE( ScrubbingToolBar, ToolBar )
EVT_COMMAND_RANGE( STBFirstButton,
                  STBFirstButton + STBNumButtons - 1,
                  wxEVT_COMMAND_BUTTON_CLICKED,
                  ScrubbingToolBar::OnButton )
END_EVENT_TABLE()

//Standard contructor
ScrubbingToolBar::ScrubbingToolBar()
: ToolBar(ScrubbingBarID, _("Scrub"), wxT("Scrub"))
{
}

ScrubbingToolBar::~ScrubbingToolBar()
{
}

void ScrubbingToolBar::Create(wxWindow * parent)
{
   ToolBar::Create(parent);
}

/// This is a convenience function that allows for button creation in
/// MakeButtons() with fewer arguments
/// Very similar to code in ControlToolBar...
AButton *ScrubbingToolBar::AddButton
(ScrubbingToolBar *pBar,
 teBmps eEnabledUp, teBmps eEnabledDown, teBmps eDisabled,
 int id,
 const wxChar *label,
 bool toggle)
{
   AButton *&r = pBar->mButtons[id];

   r = ToolBar::MakeButton
   (pBar,
    bmpRecoloredUpSmall, bmpRecoloredDownSmall, bmpRecoloredUpHiliteSmall, bmpRecoloredHiliteSmall,
    eEnabledUp, eEnabledDown, eDisabled,
    wxWindowID(id),
    wxDefaultPosition,
    toggle,
    theTheme.ImageSize( bmpRecoloredUpSmall ));

   r->SetLabel(label);
   // JKC: Unlike ControlToolBar, does not have a focus rect.  Shouldn't it?
   // r->SetFocusRect( r->GetRect().Deflate( 4, 4 ) );

   pBar->Add( r, 0, wxALIGN_CENTER );

   return r;
}

void ScrubbingToolBar::Populate()
{
   SetBackgroundColour( theTheme.Colour( clrMedium  ) );
   MakeButtonBackgroundsSmall();

   /* Buttons */
   AddButton(this, bmpScrub, bmpScrub, bmpScrubDisabled, STBScrubID,
             _("Scrub"), true);
   AddButton(this, bmpSeek, bmpSeek, bmpSeekDisabled, STBSeekID,
             _("Seek"), true);
   AddButton(this, bmpToggleScrubRuler, bmpToggleScrubRuler, bmpToggleScrubRuler,
             STBRulerID,
             _("Scrub Ruler"), true);


   RegenerateTooltips();
}

void ScrubbingToolBar::UpdatePrefs()
{
   RegenerateTooltips();

   // Set label to pull in language change
   SetLabel(_("Scrubbing"));

   // Give base class a chance
   ToolBar::UpdatePrefs();
}

void ScrubbingToolBar::RegenerateTooltips()
{
#if wxUSE_TOOLTIPS
   auto fn = [&]
   (AButton &button, const wxString &label, const CommandID &cmd)
   {
      TranslatedInternalString command{ cmd, label };
      ToolBar::SetButtonToolTip( button, &command, 1u );
   };

   auto project = GetActiveProject();
   if (project) {
      auto &scrubber = project->GetScrubber();

      const auto scrubButton = mButtons[STBScrubID];
      const auto seekButton = mButtons[STBSeekID];

      wxString label;
      label = (
               scrubber.Scrubs()
               /* i18n-hint: These commands assist the user in finding a sound by ear. ...
                "Scrubbing" is variable-speed playback, ...
                "Seeking" is normal speed playback but with skips
                */
               ? _("Stop Scrubbing")
               : _("Start Scrubbing")
               );
      fn(*scrubButton, label, wxT("Scrub"));

      label = (
               scrubber.Seeks()
               /* i18n-hint: These commands assist the user in finding a sound by ear. ...
                "Scrubbing" is variable-speed playback, ...
                "Seeking" is normal speed playback but with skips
                */
               ? _("Stop Seeking")
               : _("Start Seeking")
               );
      fn(*seekButton, label, wxT("Seek"));

      label = (
               project->GetRulerPanel()->ShowingScrubRuler()
               ? _("Hide Scrub Ruler")
               : _("Show Scrub Ruler")
               );
      fn(*mButtons[STBRulerID], label, wxT("ToggleScrubRuler"));
   }
#endif
}

void ScrubbingToolBar::OnButton(wxCommandEvent &event)
{
   AudacityProject *p = GetActiveProject();
   if (!p) return;
   auto &scrubber = p->GetScrubber();

   int id = event.GetId();

   switch (id) {
      case STBScrubID:
         scrubber.OnScrub(*p);
         break;
      case STBSeekID:
         scrubber.OnSeek(*p);
         break;
      case STBRulerID:
         scrubber.OnToggleScrubRuler(*p);
         break;
      default:
         wxASSERT(false);
   }

   EnableDisableButtons();
}

void ScrubbingToolBar::EnableDisableButtons()
{
   const auto scrubButton = mButtons[STBScrubID];
   scrubButton->SetEnabled(true);
   const auto seekButton = mButtons[STBSeekID];
   seekButton->SetEnabled(true);

   AudacityProject *p = GetActiveProject();
   if (!p) return;

   auto &scrubber = p->GetScrubber();
   const auto canScrub = scrubber.CanScrub();

   if (scrubber.Scrubs()) {
      scrubButton->PushDown();
      scrubButton->Enable();
   }
   else {
      scrubButton->PopUp();
      if (canScrub)
         scrubButton->Enable();
      else
         scrubButton->Disable();
   }

   if (scrubber.Seeks()) {
      seekButton->PushDown();
      seekButton->Enable();
   }
   else {
      seekButton->PopUp();
      if (canScrub)
         seekButton->Enable();
      else
         seekButton->Disable();
   }

   const auto barButton = mButtons[STBRulerID];
   barButton->Enable();
   if (p->GetRulerPanel()->ShowingScrubRuler())
      barButton->PushDown();
   else
      barButton->PopUp();
   RegenerateTooltips();
   scrubber.CheckMenuItems();
}
