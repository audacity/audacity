/**********************************************************************

Audacity: A Digital Audio Editor

CommonTrackControls.cpp

Paul Licameli split from TrackControls.cpp

**********************************************************************/

#include "CommonTrackControls.h"

#include "TrackButtonHandles.h"
#include "TrackSelectHandle.h"
#include "../../RefreshCode.h"
#include "../../Project.h"
#include "../../ProjectHistory.h"
#include "../../TrackInfo.h"
#include "../../TrackPanelMouseEvent.h"
#include "../../TrackUtilities.h"
#include <wx/textdlg.h>
#include "../../commands/CommandType.h"
#include "../../commands/CommandManager.h"
#include "../../ShuttleGui.h"
#include "../../Track.h"
#include "../../widgets/PopupMenuTable.h"

#include <wx/frame.h>

std::vector<UIHandlePtr> CommonTrackControls::HitTest
(const TrackPanelMouseState &st,
 const AudacityProject *WXUNUSED(project))
{
   // Hits are mutually exclusive, results single

   const wxMouseState &state = st.state;
   const wxRect &rect = st.rect;
   UIHandlePtr result;
   std::vector<UIHandlePtr> results;

   auto sThis = shared_from_this();

   if (NULL != (result = CloseButtonHandle::HitTest(
      mCloseHandle, state, rect, this)))
      results.push_back(result);

   if (NULL != (result = MenuButtonHandle::HitTest(
      mMenuHandle, state, rect, sThis)))
      results.push_back(result);

   if (NULL != (result = MinimizeButtonHandle::HitTest(
      mMinimizeHandle, state, rect, this)))
      results.push_back(result);

   if (NULL != (result = SelectButtonHandle::HitTest(
      mSelectButtonHandle, state, rect, this)))
      results.push_back(result);

   if (results.empty()) {
      if (NULL != (result = TrackSelectHandle::HitAnywhere(
         mSelectHandle, FindTrack())))
         results.push_back(result);
   }

   return results;
}

enum
{
   OnSetNameID = 2000,
   OnMoveUpID,
   OnMoveDownID,
   OnMoveTopID,
   OnMoveBottomID,
};

class TrackMenuTable : public PopupMenuTable
{
   TrackMenuTable() : mpData(NULL) {}
   DECLARE_POPUP_MENU(TrackMenuTable);

public:
   static TrackMenuTable &Instance();

private:
   void OnSetName(wxCommandEvent &);
   void OnMoveTrack(wxCommandEvent &event);

   void InitMenu(Menu *pMenu, void *pUserData) override;

   void DestroyMenu() override
   {
      mpData = nullptr;
   }

   CommonTrackControls::InitMenuData *mpData;
};

TrackMenuTable &TrackMenuTable::Instance()
{
   static TrackMenuTable instance;
   return instance;
}

void TrackMenuTable::InitMenu(Menu *pMenu, void *pUserData)
{
   mpData = static_cast<CommonTrackControls::InitMenuData*>(pUserData);
   Track *const pTrack = mpData->pTrack;

   const auto &tracks = TrackList::Get( *GetActiveProject() );

   pMenu->Enable(OnMoveUpID, tracks.CanMoveUp(pTrack));
   pMenu->Enable(OnMoveDownID, tracks.CanMoveDown(pTrack));
   pMenu->Enable(OnMoveTopID, tracks.CanMoveUp(pTrack));
   pMenu->Enable(OnMoveBottomID, tracks.CanMoveDown(pTrack));
}

BEGIN_POPUP_MENU(TrackMenuTable)
   POPUP_MENU_ITEM(OnSetNameID, _("&Name..."), OnSetName)
   POPUP_MENU_SEPARATOR()
   POPUP_MENU_ITEM(
      // It is not correct to use NormalizedKeyString::Display here --
      // wxWidgets will apply its equivalent to the key names passed to menu
      // functions.
      OnMoveUpID,
      _("Move Track &Up") + wxT("\t") +
         (CommandManager::Get( *GetActiveProject() ).
          // using GET to compose menu item name for wxWidgets
          GetKeyFromName(wxT("TrackMoveUp")).GET()),
      OnMoveTrack)
   POPUP_MENU_ITEM(
      OnMoveDownID,
      _("Move Track &Down") + wxT("\t") +
         (CommandManager::Get( *GetActiveProject() ).
          // using GET to compose menu item name for wxWidgets
          GetKeyFromName(wxT("TrackMoveDown")).GET()),
      OnMoveTrack)
   POPUP_MENU_ITEM(
      OnMoveTopID,
      _("Move Track to &Top") + wxT("\t") +
         (CommandManager::Get( *GetActiveProject() ).
          // using GET to compose menu item name for wxWidgets
          GetKeyFromName(wxT("TrackMoveTop")).GET()),
      OnMoveTrack)
   POPUP_MENU_ITEM(
      OnMoveBottomID,
      _("Move Track to &Bottom") + wxT("\t") +
         (CommandManager::Get( *GetActiveProject() ).
          // using GET to compose menu item name for wxWidgets
          GetKeyFromName(wxT("TrackMoveBottom")).GET()),
      OnMoveTrack)
END_POPUP_MENU()




#define SET_TRACK_NAME_PLUGIN_SYMBOL ComponentInterfaceSymbol{ XO("Set Track Name") }

// An example of using an AudacityCommand simply to create a dialog.
// We can add additional functions later, if we want to make it
// available to scripting.
// However there is no reason to, as SetTrackStatus is already provided.
class SetTrackNameCommand : public AudacityCommand
{
public:
   // ComponentInterface overrides
   ComponentInterfaceSymbol GetSymbol() override
   {return SET_TRACK_NAME_PLUGIN_SYMBOL;};
   //wxString GetDescription() override {return _("Sets the track name.");};
   //bool DefineParams( ShuttleParams & S ) override;
   void PopulateOrExchange(ShuttleGui & S) override;
   //bool Apply(const CommandContext & context) override;

   // Provide an override, if we want the help button.
   // wxString ManualPage() override {return wxT("");};
public:
   wxString mName;
};

void SetTrackNameCommand::PopulateOrExchange(ShuttleGui & S)
{
   S.AddSpace(0, 5);

   S.StartMultiColumn(2, wxALIGN_CENTER);
   {
      S.TieTextBox(_("Name:"),mName,60);
   }
   S.EndMultiColumn();
}

void TrackMenuTable::OnSetName(wxCommandEvent &)
{
   Track *const pTrack = mpData->pTrack;
   if (pTrack)
   {
      AudacityProject *const proj = ::GetActiveProject();
      const wxString oldName = pTrack->GetName();

      SetTrackNameCommand Command;
      Command.mName = oldName;
      // Bug 1837 : We need an OK/Cancel result if we are to enter a blank string.
      bool bResult = Command.PromptUser( &GetProjectFrame( *proj ) );
      if (bResult) 
      {
         wxString newName = Command.mName;
         for (auto channel : TrackList::Channels(pTrack))
            channel->SetName(newName);

         ProjectHistory::Get( *proj )
            .PushState(wxString::Format(_("Renamed '%s' to '%s'"),
            oldName,
            newName),
            _("Name Change"));

         mpData->result = RefreshCode::RefreshAll;
      }
   }
}

void TrackMenuTable::OnMoveTrack(wxCommandEvent &event)
{
   AudacityProject *const project = GetActiveProject();
   TrackUtilities::MoveChoice choice;
   switch (event.GetId()) {
   default:
      wxASSERT(false);
   case OnMoveUpID:
      choice = TrackUtilities::OnMoveUpID; break;
   case OnMoveDownID:
      choice = TrackUtilities::OnMoveDownID; break;
   case OnMoveTopID:
      choice = TrackUtilities::OnMoveTopID; break;
   case OnMoveBottomID:
      choice = TrackUtilities::OnMoveBottomID; break;
   }

   TrackUtilities::DoMoveTrack(*project, mpData->pTrack, choice);

   // MoveTrack already refreshed TrackPanel, which means repaint will happen.
   // This is a harmless redundancy:
   mpData->result = RefreshCode::RefreshAll;
}

unsigned CommonTrackControls::DoContextMenu
   (const wxRect &rect, wxWindow *pParent, wxPoint *)
{
   wxRect buttonRect;
   TrackInfo::GetTitleBarRect(rect, buttonRect);

   auto track = FindTrack();
   if (!track)
      return RefreshCode::RefreshNone;

   InitMenuData data{ track.get(), pParent, RefreshCode::RefreshNone };

   const auto pTable = &TrackMenuTable::Instance();
   auto pMenu = PopupMenuTable::BuildMenu(pParent, pTable, &data);

   PopupMenuTable *const pExtension = GetMenuExtension(track.get());
   if (pExtension)
      pMenu->Extend(pExtension);

   pParent->PopupMenu
      (pMenu.get(), buttonRect.x + 1, buttonRect.y + buttonRect.height + 1);

   return data.result;
}
