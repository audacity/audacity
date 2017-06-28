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
#include "../../MixerBoard.h"
#include "../../Project.h"
#include "../../TrackPanel.h"
#include "../../TrackPanelMouseEvent.h"
#include "../../WaveTrack.h"
#include <wx/textdlg.h>

int TrackControls::gCaptureState;

TrackControls::TrackControls( std::shared_ptr<Track> pTrack )
   : mwTrack{ pTrack }
{
}

TrackControls::~TrackControls()
{
}

std::shared_ptr<Track> TrackControls::FindTrack()
{
   return mwTrack.lock();
}

HitTestResult TrackControls::HitTest
(const TrackPanelMouseEvent &evt,
 const AudacityProject *project)
{
   const wxMouseEvent &event = evt.event;
   const wxRect &rect = evt.rect;
   HitTestResult result;

   if (NULL != (result = CloseButtonHandle::HitTest(event, rect)).handle)
      return result;

   if (NULL != (result = MenuButtonHandle::HitTest(event, rect,
         this->FindTrack()->GetTrackControl())).handle)
      return result;

   if (NULL != (result = MinimizeButtonHandle::HitTest(event, rect)).handle)
      return result;

   return TrackSelectHandle::HitAnywhere
      (project->GetTrackPanel()->GetTrackCount());
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
      // It is not correct to use KeyStringDisplay here -- wxWidgets will apply
      // its equivalent to the key names passed to menu functions.
      OnMoveUpID,
      _("Move Track &Up") + wxT("\t") +
         (GetActiveProject()->GetCommandManager()->
          GetKeyFromName(wxT("TrackMoveUp"))),
      OnMoveTrack)
   POPUP_MENU_ITEM(
      OnMoveDownID,
      _("Move Track &Down") + wxT("\t") +
         (GetActiveProject()->GetCommandManager()->
          GetKeyFromName(wxT("TrackMoveDown"))),
      OnMoveTrack)
   POPUP_MENU_ITEM(
      OnMoveTopID,
      _("Move Track to &Top") + wxT("\t") +
         (GetActiveProject()->GetCommandManager()->
          GetKeyFromName(wxT("TrackMoveTop"))),
      OnMoveTrack)
   POPUP_MENU_ITEM(
      OnMoveBottomID,
      _("Move Track to &Bottom") + wxT("\t") +
         (GetActiveProject()->GetCommandManager()->
          GetKeyFromName(wxT("TrackMoveBottom"))),
      OnMoveTrack)
END_POPUP_MENU()

void TrackMenuTable::OnSetName(wxCommandEvent &)
{
   Track *const pTrack = mpData->pTrack;
   if (pTrack)
   {
      AudacityProject *const proj = ::GetActiveProject();
      const wxString oldName = pTrack->GetName();
      const wxString newName =
         wxGetTextFromUser(_("Change track name to:"),
         _("Track Name"), oldName);
      if (newName != wxT("")) // wxGetTextFromUser returns empty string on Cancel.
      {
         pTrack->SetName(newName);
         // if we have a linked channel this name should change as well
         // (otherwise sort by name and time will crash).
         if (pTrack->GetLinked())
            pTrack->GetLink()->SetName(newName);

         MixerBoard *const pMixerBoard = proj->GetMixerBoard();
         auto pt = dynamic_cast<PlayableTrack*>(pTrack);
         if (pt && pMixerBoard)
            pMixerBoard->UpdateName(pt);

         proj->PushState(wxString::Format(_("Renamed '%s' to '%s'"),
            oldName.c_str(),
            newName.c_str()),
            _("Name Change"));

         mpData->result = RefreshCode::RefreshAll;
      }
   }
}

void TrackMenuTable::OnMoveTrack(wxCommandEvent &event)
{
   AudacityProject *const project = GetActiveProject();
   AudacityProject::MoveChoice choice;
   switch (event.GetId()) {
   default:
      wxASSERT(false);
   case OnMoveUpID:
      choice = AudacityProject::OnMoveUpID; break;
   case OnMoveDownID:
      choice = AudacityProject::OnMoveDownID; break;
   case OnMoveTopID:
      choice = AudacityProject::OnMoveTopID; break;
   case OnMoveBottomID:
      choice = AudacityProject::OnMoveBottomID; break;
   }

   project->MoveTrack(mpData->pTrack, choice);

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
