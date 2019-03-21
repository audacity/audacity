/**********************************************************************

Audacity: A Digital Audio Editor

TrackControls.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../Audacity.h"
#include "TrackControls.h"

#include "TrackButtonHandles.h"
#include "TrackSelectHandle.h"
#include "../../HitTestResult.h"
#include "../../RefreshCode.h"
#include "../../Menus.h"
#include "../../Project.h"
#include "../../TrackPanel.h" // for TrackInfo
#include "../../TrackPanelMouseEvent.h"
#include "../../Track.h"
#include <wx/textdlg.h>
#include "../../commands/CommandType.h"
#include "../../commands/Command.h"
#include "../../commands/CommandManager.h"
#include "../../ShuttleGui.h"


TrackControls::TrackControls( std::shared_ptr<Track> pTrack )
   : mwTrack{ pTrack }
{
}

TrackControls::~TrackControls()
{
}

std::shared_ptr<Track> TrackControls::DoFindTrack()
{
   return mwTrack.lock();
}

std::vector<UIHandlePtr> TrackControls::HitTest
(const TrackPanelMouseState &st,
 const AudacityProject *WXUNUSED(project))
{
   // Hits are mutually exclusive, results single

   const wxMouseState &state = st.state;
   const wxRect &rect = st.rect;
   UIHandlePtr result;
   std::vector<UIHandlePtr> results;

   auto pTrack = FindTrack();
   // shared pointer to this:
   auto sThis = pTrack->GetTrackControl();

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
         mSelectHandle, pTrack)))
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

   TrackControls::InitMenuData *mpData;
};

TrackMenuTable &TrackMenuTable::Instance()
{
   static TrackMenuTable instance;
   return instance;
}

void TrackMenuTable::InitMenu(Menu *pMenu, void *pUserData)
{
   mpData = static_cast<TrackControls::InitMenuData*>(pUserData);
   Track *const pTrack = mpData->pTrack;

   TrackList *const tracks = GetActiveProject()->GetTracks();

   pMenu->Enable(OnMoveUpID, tracks->CanMoveUp(pTrack));
   pMenu->Enable(OnMoveDownID, tracks->CanMoveDown(pTrack));
   pMenu->Enable(OnMoveTopID, tracks->CanMoveUp(pTrack));
   pMenu->Enable(OnMoveBottomID, tracks->CanMoveDown(pTrack));
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
         (GetActiveProject()->GetCommandManager()->
          GetKeyFromName(wxT("TrackMoveUp")).Raw()),
      OnMoveTrack)
   POPUP_MENU_ITEM(
      OnMoveDownID,
      _("Move Track &Down") + wxT("\t") +
         (GetActiveProject()->GetCommandManager()->
          GetKeyFromName(wxT("TrackMoveDown")).Raw()),
      OnMoveTrack)
   POPUP_MENU_ITEM(
      OnMoveTopID,
      _("Move Track to &Top") + wxT("\t") +
         (GetActiveProject()->GetCommandManager()->
          GetKeyFromName(wxT("TrackMoveTop")).Raw()),
      OnMoveTrack)
   POPUP_MENU_ITEM(
      OnMoveBottomID,
      _("Move Track to &Bottom") + wxT("\t") +
         (GetActiveProject()->GetCommandManager()->
          GetKeyFromName(wxT("TrackMoveBottom")).Raw()),
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
      S.TieTextBox(_("Name:"),mName);
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
      bool bResult = Command.PromptUser( proj );
      if (bResult) 
      {
         wxString newName = Command.mName;
         for (auto channel : TrackList::Channels(pTrack))
            channel->SetName(newName);

         proj->PushState(wxString::Format(_("Renamed '%s' to '%s'"),
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
   TrackActions::MoveChoice choice;
   switch (event.GetId()) {
   default:
      wxASSERT(false);
   case OnMoveUpID:
      choice = TrackActions::OnMoveUpID; break;
   case OnMoveDownID:
      choice = TrackActions::OnMoveDownID; break;
   case OnMoveTopID:
      choice = TrackActions::OnMoveTopID; break;
   case OnMoveBottomID:
      choice = TrackActions::OnMoveBottomID; break;
   }

   TrackActions::DoMoveTrack(*project, mpData->pTrack, choice);

   // MoveTrack already refreshed TrackPanel, which means repaint will happen.
   // This is a harmless redundancy:
   mpData->result = RefreshCode::RefreshAll;
}

unsigned TrackControls::DoContextMenu
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
