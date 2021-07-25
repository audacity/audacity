/**********************************************************************

 Audacity: A Digital Audio Editor

 ScrubbingToolBar.cpp

 Paul Licameli

 See ScrubbingToolBar.h for details

 *******************************************************************/

// Much of this is imitative of EditToolBar.  Should there be a common base
// class?


#include "ScrubbingToolBar.h"
#include "ToolManager.h"

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
#include "../ImageManipulation.h"
#include "Prefs.h"
#include "../UndoManager.h"
#include "../widgets/AButton.h"
#include "../tracks/ui/Scrubbing.h"

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
EVT_IDLE( ScrubbingToolBar::OnIdle )
END_EVENT_TABLE()

//Standard constructor
ScrubbingToolBar::ScrubbingToolBar( AudacityProject &project )
: ToolBar(project, ScrubbingBarID, XO("Scrub"), wxT("Scrub"))
{
}

ScrubbingToolBar::~ScrubbingToolBar()
{
}

ScrubbingToolBar &ScrubbingToolBar::Get( AudacityProject &project )
{
   auto &toolManager = ToolManager::Get( project );
   return *static_cast<ScrubbingToolBar*>( toolManager.GetToolBar(ScrubbingBarID) );
}

const ScrubbingToolBar &ScrubbingToolBar::Get( const AudacityProject &project )
{
   return Get( const_cast<AudacityProject&>( project )) ;
}

void ScrubbingToolBar::Create(wxWindow * parent)
{
   ToolBar::Create(parent);
   UpdatePrefs();
}

/// This is a convenience function that allows for button creation in
/// MakeButtons() with fewer arguments
/// Very similar to code in ControlToolBar...
AButton *ScrubbingToolBar::AddButton
(ScrubbingToolBar *pBar,
 teBmps eEnabledUp, teBmps eEnabledDown, teBmps eDisabled,
 int id,
 const TranslatableString &label,
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
             XO("Scrub"), true);
   AddButton(this, bmpSeek, bmpSeek, bmpSeekDisabled, STBSeekID,
             XO("Seek"), true);
   AddButton(this, bmpToggleScrubRuler, bmpToggleScrubRuler, bmpToggleScrubRuler,
             STBRulerID,
             XO("Scrub Ruler"), true);


   RegenerateTooltips();
}

void ScrubbingToolBar::UpdatePrefs()
{
   RegenerateTooltips();

   // Set label to pull in language change
   SetLabel(XO("Scrubbing"));

   // Give base class a chance
   ToolBar::UpdatePrefs();
}

void ScrubbingToolBar::RegenerateTooltips()
{
   DoRegenerateTooltips( true );
}

void ScrubbingToolBar::DoRegenerateTooltips( bool force )
{
#if wxUSE_TOOLTIPS
   auto fn = [&](
      AButton &button, const TranslatableString &label, const CommandID &cmd)
   {
      ComponentInterfaceSymbol command{ cmd, label };
      ToolBar::SetButtonToolTip( mProject, button, &command, 1u );
   };

   auto project = &mProject;
   if (project) {
      auto &scrubber = Scrubber::Get( *project );

      const auto scrubButton = mButtons[STBScrubID];
      const auto seekButton = mButtons[STBSeekID];

      TranslatableString label;
      bool scrubs = scrubber.Scrubs();
      if (force || mLastScrub != scrubs) {
         label = (
                  scrubs
                  /* i18n-hint: These commands assist the user in finding a sound by ear. ...
                   "Scrubbing" is variable-speed playback, ...
                   "Seeking" is normal speed playback but with skips
                   */
                  ? XO("Stop Scrubbing")
                  /* i18n-hint: These commands assist the user in finding a sound by ear. ...
                   "Scrubbing" is variable-speed playback, ...
                   "Seeking" is normal speed playback but with skips
                   */
                  : XO("Start Scrubbing")
                  );
         fn(*scrubButton, label, wxT("Scrub"));
      }
      mLastScrub = scrubs;

      bool seeks = scrubber.Seeks();
      if (force || mLastSeek != seeks) {
         label = (
                  seeks
                  /* i18n-hint: These commands assist the user in finding a sound by ear. ...
                   "Scrubbing" is variable-speed playback, ...
                   "Seeking" is normal speed playback but with skips
                   */
                  ? XO("Stop Seeking")
                  /* i18n-hint: These commands assist the user in finding a sound by ear. ...
                   "Scrubbing" is variable-speed playback, ...
                   "Seeking" is normal speed playback but with skips
                   */
                  : XO("Start Seeking")
                  );
         fn(*seekButton, label, wxT("Seek"));
      }
      mLastSeek = seeks;

      bool showingRuler = AdornedRulerPanel::Get( *project ).ShowingScrubRuler();
      if (force || mLastRuler != showingRuler) {
         label = (
                  showingRuler
                  ? XO("Hide Scrub Ruler")
                  : XO("Show Scrub Ruler")
                  );
         fn(*mButtons[STBRulerID], label, wxT("ToggleScrubRuler"));
      }
      mLastRuler = showingRuler;
   }
#endif
}

void ScrubbingToolBar::OnButton(wxCommandEvent &event)
{
   AudacityProject *p = &mProject;
   if (!p) return;
   auto &scrubber = Scrubber::Get( *p );

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
   const auto seekButton = mButtons[STBSeekID];

   AudacityProject *p = &mProject;

   auto &scrubber = Scrubber::Get( *p );
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
   if (AdornedRulerPanel::Get( *p ).ShowingScrubRuler())
      barButton->PushDown();
   else
      barButton->PopUp();
   DoRegenerateTooltips( false );
   scrubber.CheckMenuItems();
}

void ScrubbingToolBar::OnIdle( wxIdleEvent &evt )
{
   evt.Skip();
   EnableDisableButtons();
}

static RegisteredToolbarFactory factory{ ScrubbingBarID,
   []( AudacityProject &project ){
      return ToolBar::Holder{ safenew ScrubbingToolBar{ project } }; }
};

namespace {
AttachedToolBarMenuItem sAttachment{
   /* i18n-hint: Clicking this menu item shows the toolbar
      that enables Scrub or Seek playback and Scrub Ruler */
   ScrubbingBarID, wxT("ShowScrubbingTB"), XXO("Scru&b Toolbar")
};
}

