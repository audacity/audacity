/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  SnappingToolBar.cpp

  Dmitry Vedenko

*******************************************************************/

#include "SnappingToolBar.h"

#include <algorithm>
#include <cassert>

#include <wx/sizer.h>
#include <wx/checkbox.h>
#include <wx/combo.h>
#include <wx/menu.h>

#include "ToolManager.h"


#include "widgets/BasicMenu.h"
#include "wxWidgetsWindowPlacement.h"

#include "Prefs.h"
#include "Project.h"
#include "../ProjectSettings.h"
#include "ViewInfo.h"

#include "AllThemeResources.h"

#include "ProjectSnap.h"

#if wxUSE_ACCESSIBILITY
#include "WindowAccessible.h"
#endif

namespace
{
TranslatableString GetSnapToLabel(Identifier snapTo)
{
   auto item = SnapFunctionsRegistry::Find(snapTo);
   return item != nullptr ? item->label : XO("Unknown");
}

// Build a popup menu based on snap functions registry
struct PopupMenuBuilder final : public SnapRegistryVisitor
{
   template<typename Callback>
   PopupMenuBuilder(Callback callback)
       : menuStack { &menu }
       , snapModeUpdated { std::move(callback) }
   {
      SnapFunctionsRegistry::Visit(*this);
   }
   
   void BeginGroup(const SnapRegistryGroup& item) override
   {
      if (item.transparent)
         return;

      auto menu = safenew wxMenu;
      
      menuStack.back()->AppendSubMenu(menu, item.label.Translation());
      menuStack.push_back(menu);
   }
   
   void EndGroup(const SnapRegistryGroup& item) override
   {
      assert(!menuStack.empty());
      
      if (item.transparent)
      {
         menuStack.back()->AppendSeparator();
         return;
      }

      menuStack.pop_back();
   }

   void Visit(const SnapRegistryItem& item) override
   {
      auto menuItem = menuStack.back()->AppendCheckItem(wxID_ANY, item.label.Translation());

      if (ReadSnapTo() == item.name)
         menuItem->Check();

      menuStack.back()->Bind(
         wxEVT_MENU,
         [this, id = item.name](wxCommandEvent&) { snapModeUpdated(id); },
         menuItem->GetId());
   }

   wxMenu menu;
   std::vector<wxMenu*> menuStack;
   std::function<void(const Identifier& id)> snapModeUpdated;
};

// Build a linear list from all the items in the snap functions registry
struct SnapToListBuilder final : public SnapRegistryVisitor
{
   SnapToListBuilder()
   {
      SnapFunctionsRegistry::Visit(*this);
   }

   void BeginGroup(const SnapRegistryGroup& item) override
   {
   }

   void EndGroup(const SnapRegistryGroup& item) override
   {
   }

   void Visit(const SnapRegistryItem& item) override
   {
      List.push_back(item.name);
   }

   std::vector<Identifier> List;
};

/*
 * This class provides a hack to use popup menu instead of the dropdown list.
 * This allows to organize the list of items in a more user-friendly way.
 */
class SnapModePopup final : public wxComboPopup
{
public:
   explicit SnapModePopup(AudacityProject& project)
       : mProject { project }
       , mSnappingModeChangedSubscription(ProjectSnap::Get(project).Subscribe(
            [this](auto& msg) {
               UpdateCurrentIndex(msg.newSnapTo);
               
               auto comboCtrl = GetComboCtrl();
               
               comboCtrl->SetValue(
                  GetSnapToLabel(msg.newSnapTo).Translation());

               comboCtrl->SetName(GetComboCtrl()->GetValue());
            }))
   {
   }

   void Init () override
   {
      mSnapToList = std::move(SnapToListBuilder().List);

      UpdateCurrentIndex(ReadSnapTo());
   }
   
   bool Create(wxWindow* parent) override
   {
      mControl = safenew wxWindow(parent, wxID_ANY);

      // This call cannot happen in Init(), because the combobox is not yet in a valid
      // state. Doing a deferred call from Init() is unsafe,
      // as in some cases Audacity recreates the combobox multiple times before the next
      // event loop iteration.
      GetComboCtrl()->SetValue(GetStringValue());
      
      return mControl;
   }

   wxWindow* GetControl() override
   {
      return mControl;
   }

   wxString GetStringValue() const override
   {
      return GetSnapToLabel(ReadSnapTo()).Translation();
   }

   void OnPopup() override
   {
      PopupMenuBuilder menuBuilder { [this](const auto& id) {
         ProjectSnap::Get(mProject).SetSnapTo(id);
      } };

      BasicMenu::Handle { &menuBuilder.menu }.Popup(
         wxWidgetsWindowPlacement { GetComboCtrl() },
         { 0, GetComboCtrl()->GetSize().y });

      // Hide the combobox list after the menu was closed
      BasicUI::CallAfter([this] { Dismiss(); });
   }

   void SetStringValue(const wxString& value) override
   {
       wxComboPopup::SetStringValue(value);
   }

   bool FindItem(const wxString& item, wxString* trueItem = NULL) override
   {
      return wxComboPopup::FindItem(item, trueItem);
   }

   void OnComboKeyEvent(wxKeyEvent& event) override
   {
      const auto keyCode = event.GetKeyCode();

      if (keyCode == WXK_RETURN || keyCode == WXK_NUMPAD_ENTER)
      {
         GetComboCtrl()->ShowPopup();
         return;
      }

      int direction = 0;

      if (
         keyCode == WXK_UP || keyCode == WXK_NUMPAD_UP || keyCode == WXK_LEFT ||
         keyCode == WXK_NUMPAD_LEFT)
         direction = -1;
      else if (
         keyCode == WXK_DOWN || keyCode == WXK_NUMPAD_DOWN ||
         keyCode == WXK_RIGHT || keyCode == WXK_NUMPAD_RIGHT)
         direction = 1;
  
      if (direction == 0)
         return;

      const auto newIndex = std::clamp<ptrdiff_t>(
         mCurrentIndex + direction, 0, mSnapToList.size() - 1);

      if (newIndex == mCurrentIndex)
         return;

      mCurrentIndex = newIndex;

      ProjectSnap::Get(mProject).SetSnapTo(mSnapToList[mCurrentIndex]);
   }

   void OnComboCharEvent(wxKeyEvent& event) override
   {
      // Consume the event to prevent editing
   }

   void UpdateCurrentIndex(const Identifier& identifier)
   {
      if (
         mCurrentIndex < mSnapToList.size() &&
         mSnapToList[mCurrentIndex] == identifier)
         return;

      mCurrentIndex = static_cast<size_t>(std::distance(
         mSnapToList.begin(),
         std::find(mSnapToList.begin(), mSnapToList.end(), identifier)));
   }

private:
   AudacityProject& mProject;
   wxWeakRef<wxWindow> mControl;

   std::vector<Identifier> mSnapToList;
   std::ptrdiff_t mCurrentIndex { -1 };

   Observer::Subscription mSnappingModeChangedSubscription;
};
}

IMPLEMENT_CLASS(SnappingToolBar, ToolBar);

BEGIN_EVENT_TABLE(SnappingToolBar, ToolBar)
   EVT_SIZE(SnappingToolBar::OnSize)
END_EVENT_TABLE()

Identifier SnappingToolBar::ID()
{
   return wxT("Snapping");
}

SnappingToolBar::SnappingToolBar(AudacityProject& project)
    : ToolBar(project, XO("Snapping"), ID())
    , mSnappingModeChangedSubscription(ProjectSnap::Get(mProject).Subscribe(
         [this](auto settings)
         {
            if (mSnapModeCheckBox)
               mSnapModeCheckBox->SetValue(
                  settings.newSnapMode != SnapMode::SNAP_OFF);

            if (mSnapToCombo)
               mSnapToCombo->Enable(settings.newSnapMode != SnapMode::SNAP_OFF);
         }))
{
}

SnappingToolBar::~SnappingToolBar()
{
}

bool SnappingToolBar::ShownByDefault() const
{
   return true;
}

ToolBar::DockID SnappingToolBar::DefaultDockID() const
{
   return BotDockID;
}

SnappingToolBar& SnappingToolBar::Get(AudacityProject& project)
{
   auto &toolManager = ToolManager::Get( project );
   return *static_cast<SnappingToolBar*>(toolManager.GetToolBar(ID()));
}

const SnappingToolBar& SnappingToolBar::Get(const AudacityProject& project)
{
   return Get( const_cast<AudacityProject&>( project )) ;
}

void SnappingToolBar::Create(wxWindow* parent)
{
   ToolBar::Create(parent);
   UpdatePrefs();
}

void SnappingToolBar::Populate()
{
   SetBackgroundColour( theTheme.Colour( clrMedium  ) );

   auto sizer = safenew wxFlexGridSizer(1, 1, 1);
   Add(sizer, 0, wxALIGN_CENTER_VERTICAL | wxLEFT, 5);

   mSnapModeCheckBox =
      safenew wxCheckBox(this, wxID_ANY, XO("Snap").Translation());
   mSnapModeCheckBox->SetName(XO("Snap").Translation());
#if wxUSE_ACCESSIBILITY
   // so that name can be set on a standard control
   mSnapModeCheckBox->SetAccessible(
      safenew WindowAccessible(mSnapModeCheckBox));
#endif

   sizer->Add(mSnapModeCheckBox, 0, wxBOTTOM | wxRIGHT | wxEXPAND, 5);

   const bool snapEnabled =
      ProjectSnap::Get(mProject).GetSnapMode() != SnapMode::SNAP_OFF;

   mSnapModeCheckBox->SetValue(snapEnabled);

   mSnapToCombo = safenew wxComboCtrl(
      this, wxID_ANY, {}, wxDefaultPosition, wxDefaultSize /*, wxCB_READONLY*/);
#if wxUSE_ACCESSIBILITY
   // so that name can be set on a standard control
   mSnapToCombo->SetAccessible(safenew WindowAccessible(mSnapToCombo));
#endif

   //mSnapToCombo->SetEditable(false);
   mSnapToCombo->SetPopupControl(safenew SnapModePopup(mProject));
   mSnapToCombo->SetName(mSnapToCombo->GetValue());
   mSnapToCombo->Enable(snapEnabled);
   mSnapToCombo->SetMinSize(wxSize(150, -1));
   
   sizer->Add(mSnapToCombo, 1, wxRIGHT | wxEXPAND, 5);

   mSnapModeCheckBox->Bind(
      wxEVT_CHECKBOX, [this](auto&) { OnSnapModeChanged(); });

   mSnapModeCheckBox->Bind(
      wxEVT_CHAR_HOOK,
      [this](auto& evt)
      {         
         const auto keyCode = evt.GetKeyCode();

         if (keyCode != WXK_NUMPAD_ENTER && keyCode != WXK_RETURN)
         {
            evt.Skip();
            return;
         }

         mSnapModeCheckBox->SetValue(!mSnapModeCheckBox->GetValue());

         OnSnapModeChanged();
      });

   RegenerateTooltips();
   Fit();
   Layout();
}

void SnappingToolBar::UpdatePrefs()
{
   // Set label to pull in language change
   SetLabel(XO("Snapping"));

   RegenerateTooltips();
   // Give base class a chance
   ToolBar::UpdatePrefs();
}

void SnappingToolBar::RegenerateTooltips()
{
}

void SnappingToolBar::OnSize(wxSizeEvent& evt)
{
   Refresh( true );

   evt.Skip();
}

void SnappingToolBar::OnSnapModeChanged()
{
   const bool snapEnabled = mSnapModeCheckBox->GetValue();

   ProjectSnap::Get(mProject).SetSnapMode(
      snapEnabled ? SnapMode::SNAP_NEAREST : SnapMode::SNAP_OFF);

   mSnapToCombo->Enable(snapEnabled);
}

static RegisteredToolbarFactory factory{
   []( AudacityProject &project ){
   return ToolBar::Holder { safenew SnappingToolBar { project } };
} };

namespace {
AttachedToolBarMenuItem sAttachment{
   /* i18n-hint: Clicking this menu item shows the toolbar
      for selecting a time range of audio */
   SnappingToolBar::ID(), wxT("ShowSnappingTB"), XXO("&Snapping Toolbar")
};
}

