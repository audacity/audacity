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
#include "AudioIOBase.h"
#include "AllThemeResources.h"
#include "Prefs.h"
#include "ProjectWindow.h"
#include "Theme.h"
#include "Track.h"

#include "audiocom/ShareAudioDialog.h"
#include "toolbars/ToolManager.h"
#include "widgets/AButton.h"

IMPLEMENT_CLASS(cloud::ShareAudioToolbar, ToolBar);

namespace cloud
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
      auto pCtrl = static_cast<AButton*>(this->FindWindow(iWinID));
      CommandID name;
      switch (iWinID)
      {
      case ID_SHARE_AUDIO_BUTTON:
         name = ID();
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
#ifndef USE_AQUA_THEME
   const auto s = mSizer->GetSize();
   const auto p = mSizer->GetPosition();

   wxRect bevelRect(p.x, p.y, s.GetWidth() - 1, s.GetHeight() - 1);
   AColor::Bevel(*dc, true, bevelRect);
#endif
}

void ShareAudioToolbar::EnableDisableButtons()
{
   auto gAudioIO = AudioIOBase::Get();

   const bool audioStreamActive = gAudioIO &&
      gAudioIO->IsStreamActive() && !gAudioIO->IsMonitoring();

   bool hasAudio = false;

   for (const auto& track : TrackList::Get(mProject).Leaders<PlayableTrack>())
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
   // ToolBar::ReCreateButtons() will get rid of its sizer and
   // since we've attached our sizer to it, ours will get deleted too
   // so clean ours up first.
   DestroySizer();

   ToolBar::ReCreateButtons();

   EnableDisableButtons();

   RegenerateTooltips();
}

void ShareAudioToolbar::MakeShareAudioButton()
{
   mShareAudioButton = safenew AButton(this, ID_SHARE_AUDIO_BUTTON);
   //i18n-hint: Share audio button text, keep as short as possible
   mShareAudioButton->SetLabel(XO("Share Audio"));
   mShareAudioButton->SetButtonType(AButton::FrameButton);
   mShareAudioButton->SetImages(
      theTheme.Image(bmpRecoloredUpSmall),
      theTheme.Image(bmpRecoloredUpHiliteSmall),
      theTheme.Image(bmpRecoloredDownSmall),
      theTheme.Image(bmpRecoloredHiliteSmall),
      theTheme.Image(bmpRecoloredUpSmall));
   mShareAudioButton->SetIcon(theTheme.Image(bmpShareAudio));
   mShareAudioButton->SetForegroundColour(theTheme.Colour(clrTrackPanelText));

   mShareAudioButton->Bind(
      wxEVT_BUTTON,
      [this](auto)
      {
         audiocom::ShareAudioDialog dlg(mProject, &ProjectWindow::Get(mProject));
         dlg.ShowModal();

         mShareAudioButton->PopUp();
      });
}

void ShareAudioToolbar::ArrangeButtons()
{
   // (Re)allocate the button sizer
   DestroySizer();

   Add((mSizer = safenew wxBoxSizer(wxHORIZONTAL)), 1, wxEXPAND);
   mSizer->Add(mShareAudioButton, 1, wxEXPAND);

   // Layout the sizer
   mSizer->Layout();

   // Layout the toolbar
   Layout();

   const auto height = 2 * toolbarSingle;
   SetMinSize({ std::max(76, GetSizer()->GetMinSize().GetWidth()), height });
   SetMaxSize({ -1, height });
}

void ShareAudioToolbar::DestroySizer()
{
   if (mSizer == nullptr)
      return;

   Detach(mSizer);

   std::unique_ptr<wxSizer> { mSizer }; // DELETE it
   mSizer = nullptr;
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

} // namespace cloud
