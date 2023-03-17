/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  TimeSignatureToolBar.cpp

  Dmitry Vedenko

*******************************************************************/

#include "TimeSignatureToolBar.h"

#include <algorithm>
#include <cassert>

#include <wx/sizer.h>
#include <wx/spinctrl.h>
#include <wx/combobox.h>

#include "ToolManager.h"


#include "widgets/BasicMenu.h"
#include "wxWidgetsWindowPlacement.h"

#include "Prefs.h"
#include "Project.h"
#include "../ProjectSettings.h"
#include "ViewInfo.h"

#include "AllThemeResources.h"

#include "ProjectTimeSignature.h"
#include "wxArrayStringEx.h"

#if wxUSE_ACCESSIBILITY
#include "WindowAccessible.h"
#endif

IMPLEMENT_CLASS(TimeSignatureToolBar, ToolBar);

BEGIN_EVENT_TABLE(TimeSignatureToolBar, ToolBar)
   EVT_SIZE(TimeSignatureToolBar::OnSize)
END_EVENT_TABLE()

Identifier TimeSignatureToolBar::ID()
{
   return wxT("TimeSignature");
}

TimeSignatureToolBar::TimeSignatureToolBar(AudacityProject& project)
    : ToolBar(project, XO("Time Signature"), ID())
    , mTimeSignatureChangedSubscription(ProjectTimeSignature::Get(mProject).Subscribe(
            [this](auto settings)
            {
               if (mTempoControl)
                  mTempoControl->SetValue(settings.newTempo);
               
               if (mUpperSignatureControl)
                  mUpperSignatureControl->SetValue(
                     settings.newUpperTimeSignature);

               if (mLowerSignatureControl)
                  mLowerSignatureControl->SetValue(
                     wxString::Format("%d", settings.newLowerTimeSignature));
            }))
{
}

TimeSignatureToolBar::~TimeSignatureToolBar() = default;

bool TimeSignatureToolBar::ShownByDefault() const
{
   return true;
}

ToolBar::DockID TimeSignatureToolBar::DefaultDockID() const
{
   return BotDockID;
}

TimeSignatureToolBar& TimeSignatureToolBar::Get(AudacityProject& project)
{
   auto &toolManager = ToolManager::Get( project );
   return *static_cast<TimeSignatureToolBar*>(toolManager.GetToolBar(ID()));
}

const TimeSignatureToolBar& TimeSignatureToolBar::Get(const AudacityProject& project)
{
   return Get( const_cast<AudacityProject&>( project )) ;
}

void TimeSignatureToolBar::Create(wxWindow* parent)
{
   ToolBar::Create(parent);
   UpdatePrefs();
}

void TimeSignatureToolBar::Populate()
{
   const auto size = wxSize(45, -1);
   
   SetBackgroundColour( theTheme.Colour( clrMedium  ) );

   auto sizer = safenew wxFlexGridSizer(2, 5, 1);
   Add(sizer, 0, wxALIGN_CENTER_VERTICAL | wxLEFT, 5);

   AddTitle(XO("Tempo"), sizer);
   AddTitle(XO("Time Signature"), sizer);

   mTempoControl = safenew wxSpinCtrl(
      this, wxID_ANY, {}, wxDefaultPosition, size, wxSP_ARROW_KEYS, 1,
      1000, ProjectTimeSignature::Get(mProject).GetTempo());

   mTempoControl->SetName(XO("Tempo").Translation());
   
   sizer->Add(mTempoControl, 0, wxEXPAND | wxRIGHT, 5);

   auto tempoSizer = safenew wxBoxSizer(wxHORIZONTAL);
   sizer->Add(tempoSizer, 0, wxEXPAND | wxRIGHT, 5);

   mUpperSignatureControl = safenew wxSpinCtrl(
      this, wxID_ANY, {}, wxDefaultPosition, size, wxSP_ARROW_KEYS, 1,
      128, ProjectTimeSignature::Get(mProject).GetUpperTimeSignature());

   mUpperSignatureControl->SetName(XO("Upper Time Signature").Translation());

   tempoSizer->Add(mUpperSignatureControl, 0, wxEXPAND | wxRIGHT, 5);

   AddTitle(
      Verbatim(L"/"), tempoSizer, wxALIGN_CENTER_VERTICAL | wxRIGHT, 5, 1.5);

   mLowerSignatureControl = safenew wxComboBox(
      this, wxID_ANY,
      wxString::Format(
         "%d", ProjectTimeSignature::Get(mProject).GetLowerTimeSignature()),
      wxDefaultPosition, size,
      wxArrayStringEx { L"1", L"2", L"4", L"8", L"16", L"32", L"64" },
      wxCB_READONLY);

   mLowerSignatureControl->SetName(XO("Lower Time Signature").Translation());

   tempoSizer->Add(mLowerSignatureControl, 0, wxEXPAND | wxRIGHT, 5);

   mTempoControl->Bind(
      wxEVT_SPINCTRL,
      [this](auto)
      {
         ProjectTimeSignature::Get(mProject).SetTempo(
            mTempoControl->GetValue());
      });

   mUpperSignatureControl->Bind(
      wxEVT_SPINCTRL,
      [this](auto)
      {
         ProjectTimeSignature::Get(mProject).SetUpperTimeSignature(
            mUpperSignatureControl->GetValue());
      });

   mLowerSignatureControl->Bind(
      wxEVT_COMBOBOX,
      [this](auto)
      {
         long value;
         if (mLowerSignatureControl->GetValue().ToLong(&value))
            ProjectTimeSignature::Get(mProject).SetLowerTimeSignature(value);
      });

#if wxUSE_ACCESSIBILITY
   mTempoControl->SetAccessible(safenew WindowAccessible(mTempoControl));
   mUpperSignatureControl->SetAccessible(
      safenew WindowAccessible(mUpperSignatureControl));
   mLowerSignatureControl->SetAccessible(
      safenew WindowAccessible(mLowerSignatureControl));
#endif
   
   RegenerateTooltips();
   Fit();
   Layout();
}

void TimeSignatureToolBar::UpdatePrefs()
{
   // Set label to pull in language change
   SetLabel(XO("Time Signature"));

   RegenerateTooltips();
   // Give base class a chance
   ToolBar::UpdatePrefs();
}

void TimeSignatureToolBar::RegenerateTooltips()
{
}

void TimeSignatureToolBar::OnSize(wxSizeEvent& evt)
{
   Refresh( true );

   evt.Skip();
}

void TimeSignatureToolBar::AddTitle(
   const TranslatableString& Title, wxSizer* pSizer, int flags, int border,
   double fontMultiplier)
{
   const auto translated = Title.Translation();
   
   auStaticText* pTitle = safenew auStaticText(this, translated);
   
   pTitle->SetBackgroundColour(theTheme.Colour(clrMedium));
   pTitle->SetForegroundColour(theTheme.Colour(clrTrackPanelText));
   pTitle->ScaleFont(fontMultiplier);

   pSizer->Add(pTitle, 0, flags, border);
}

static RegisteredToolbarFactory factory{
   []( AudacityProject &project ){
   return ToolBar::Holder { safenew TimeSignatureToolBar { project } };
} };

namespace {
AttachedToolBarMenuItem sAttachment{
   /* i18n-hint: Clicking this menu item shows the toolbar
      for selecting a time range of audio */
   TimeSignatureToolBar::ID(), wxT("ShowTimeSignatureTB"), XXO("Time Signature Toolbar")
};
}

