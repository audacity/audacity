/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ShareAudioToolbar.cpp

  Dmitry Vedenko

**********************************************************************/
#include "ShareAudioToolbar.h"

#include <wx/sizer.h>
#include <wx/stattext.h>

#include <algorithm>

#if wxUSE_TOOLTIPS
#include <wx/tooltip.h>
#endif

#include "AColor.h"
#include "AllThemeResources.h"
#include "AudioIOBase.h"
#include "ExportUtils.h"
#include "PlayableTrack.h"
#include "Prefs.h"
#include "ProjectWindow.h"
#include "Theme.h"

#include "GetEffectsDialog.h"
#include "dialogs/ShareAudioDialog.h"
#include "toolbars/ToolManager.h"
#include "widgets/AButton.h"

IMPLEMENT_CLASS(audacity::cloud::ShareAudioToolbar, ToolBar);

namespace audacity::cloud
{
Identifier ShareAudioToolbar::ID()
{
   return wxT("Share Audio");
}

ShareAudioToolbar::ShareAudioToolbar(AudacityProject& project)
    : ToolBar(project, XO("Share Audio"), ID())
{
}

ShareAudioToolbar::~ShareAudioToolbar()
{
}

ShareAudioToolbar& ShareAudioToolbar::Get(AudacityProject& project)
{
   auto& toolManager = ToolManager::Get(project);
   return *static_cast<ShareAudioToolbar*>(
      toolManager.GetToolBar(ID()));
}

const ShareAudioToolbar& ShareAudioToolbar::Get(const AudacityProject& project)
{
   return Get(const_cast<AudacityProject&>(project));
}

void ShareAudioToolbar::Create(wxWindow* parent)
{
   ToolBar::Create(parent);

   // Simulate a size event to set initial meter placement/size
   wxSizeEvent event(GetSize(), GetId());
   event.SetEventObject(this);
   GetEventHandler()->ProcessEvent(event);
}

void ShareAudioToolbar::RegenerateTooltips()
{
#if wxUSE_TOOLTIPS
   for (long iWinID = ID_SHARE_AUDIO_BUTTON; iWinID < BUTTON_COUNT; iWinID++)
   {
      auto pCtrl = static_cast<AButton*>(this->FindWindowById(iWinID));
      if (!pCtrl) {
         assert(true);
         continue;
      }
      CommandID name;
      switch (iWinID)
      {
      case ID_SHARE_AUDIO_BUTTON:
         name = wxT("Share Audio");
         break;
      case ID_GET_EFFECTS_BUTTON:
         name = wxT("Get Effects");
         break;
      }

      std::vector<ComponentInterfaceSymbol> commands(
         1u, { name, Verbatim(pCtrl->GetLabel()) });

      ToolBar::SetButtonToolTip(
         mProject, *pCtrl, commands.data(), commands.size());
   }
#endif
}

void ShareAudioToolbar::Populate()
{
   MakeButtonBackgroundsSmall();
   SetBackgroundColour(theTheme.Colour(clrMedium));
   MakeShareAudioButton();
   MakeGetEffectsButton();

#if wxUSE_TOOLTIPS
   RegenerateTooltips();
   wxToolTip::Enable(true);
   wxToolTip::SetDelay(1000);
#endif

   // Set default order and mode
   ArrangeButtons();
}

void ShareAudioToolbar::Repaint(wxDC* dc)
{
   // this was used to add a bevel and can be removed
}

void ShareAudioToolbar::EnableDisableButtons()
{
   auto gAudioIO = AudioIOBase::Get();

   const bool audioStreamActive = gAudioIO &&
      gAudioIO->IsStreamActive() && !gAudioIO->IsMonitoring();

   bool hasAudio = false;

   for (const auto& track : TrackList::Get(mProject).Any<PlayableTrack>())
   {
      if (track->GetStartTime() != track->GetEndTime())
      {
         hasAudio = true;
         break;
      }
   }

   mShareAudioButton->SetEnabled(hasAudio && !audioStreamActive);
}

void ShareAudioToolbar::ReCreateButtons()
{
   ToolBar::ReCreateButtons();

   EnableDisableButtons();

   RegenerateTooltips();
}

AButton* ShareAudioToolbar::MakeButton(int id, const TranslatableString& label, const wxImage& icon)
{
   const auto height = 25;

   auto button = safenew AButton(this, id);
   button->SetLabel(label);
   button->SetButtonType(AButton::FrameTextHButton);
   button->SetImages(
      theTheme.Image(bmpRecoloredUpSmall),
      theTheme.Image(bmpRecoloredUpHiliteSmall),
      theTheme.Image(bmpRecoloredDownSmall),
      theTheme.Image(bmpRecoloredHiliteSmall),
      theTheme.Image(bmpRecoloredUpSmall));
   button->SetIcon(icon);
   button->SetForegroundColour(theTheme.Colour(clrTrackPanelText));

   button->SetMinSize(wxSize{-1, height});
   button->SetMaxSize(wxSize{-1, height});
   button->SetFrameMid(3);

   return button;
}


void ShareAudioToolbar::MakeShareAudioButton()
{
   mShareAudioButton = MakeButton(ID_SHARE_AUDIO_BUTTON, XO("Share Audio"), theTheme.Image(bmpShareAudio));

   mShareAudioButton->Bind(wxEVT_BUTTON, [this](auto) {
      audiocom::ShareAudioDialog dlg(
         mProject, AudiocomTrace::ShareAudioButton,
         &ProjectWindow::Get(mProject));
      dlg.ShowModal();

         mShareAudioButton->PopUp();
      });
}

void ShareAudioToolbar::MakeGetEffectsButton()
{
   mGetEffectsButton = MakeButton(ID_GET_EFFECTS_BUTTON, XO("Get Effects"), theTheme.Image(bmpPlug));

   mGetEffectsButton->Bind(wxEVT_BUTTON, [this](auto) {
      musehub::GetEffectsDialog dlg(&ProjectWindow::Get(mProject));
      dlg.ShowModal();

         mGetEffectsButton->PopUp();
      });

}

void ShareAudioToolbar::ArrangeButtons()
{

   wxGridSizer *sizer = new wxGridSizer(2, 1, toolbarSpacing, toolbarSpacing);
   Add(sizer, 0, wxALIGN_CENTRE | wxALL, toolbarSpacing);

   sizer->Add(mShareAudioButton, 0, wxEXPAND);
   sizer->Add(mGetEffectsButton, 0, wxEXPAND);

   // Layout the toolbar
   sizer->Layout();
   Layout();
}

static RegisteredToolbarFactory factory {
   [](AudacityProject& project)
   { return ToolBar::Holder { safenew ShareAudioToolbar { project } }; }
};

namespace
{
AttachedToolBarMenuItem sAttachment {
   /* i18n-hint: Clicking this menu item shows the toolbar
      that opens Share Audio dialog */
   ShareAudioToolbar::ID(), wxT("ShareAudioTB"), XXO("&Share Audio Toolbar")
};
}

} // namespace audacity::cloud
