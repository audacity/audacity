/**********************************************************************

Audacity: A Digital Audio Editor

NoteTrackControls.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../../../Audacity.h" // for USE_* macros
#ifdef USE_MIDI
#include "NoteTrackControls.h"

#include "../../../../Experimental.h"

#include "NoteTrackButtonHandle.h"

#include "../../ui/PlayableTrackButtonHandles.h"
#include "NoteTrackSliderHandles.h"

#include "../../../../HitTestResult.h"
#include "../../../../TrackPanelMouseEvent.h"
#include "../../../../NoteTrack.h"
#include "../../../../widgets/PopupMenuTable.h"
#include "../../../../Project.h"
#include "../../../../RefreshCode.h"

///////////////////////////////////////////////////////////////////////////////
NoteTrackControls::~NoteTrackControls()
{
}

std::vector<UIHandlePtr> NoteTrackControls::HitTest
(const TrackPanelMouseState & st,
 const AudacityProject *pProject)
{
   // Hits are mutually exclusive, results single
   std::vector<UIHandlePtr> results;
   const wxMouseState &state = st.state;
   const wxRect &rect = st.rect;
   if (state.ButtonIsDown(wxMOUSE_BTN_ANY)) {
      auto track = std::static_pointer_cast<NoteTrack>(FindTrack());
      auto result = [&]{
         UIHandlePtr result;
         if (NULL != (result = MuteButtonHandle::HitTest(
            mMuteHandle, state, rect, pProject, track)))
            return result;

         if (NULL != (result = SoloButtonHandle::HitTest(
            mSoloHandle, state, rect, pProject, track)))
            return result;
#ifdef EXPERIMENTAL_MIDI_OUT
         if (NULL != (result = VelocitySliderHandle::HitTest(
            mVelocityHandle, state, rect, track)))
            return result;
         if (NULL != (result = NoteTrackButtonHandle::HitTest(
            mClickHandle, state, rect, track)))
            return result;
#endif
         return result;
      }();
      if (result) {
         results.push_back(result);
         return results;
      }
   }

   return TrackControls::HitTest(st, pProject);
}

class NoteTrackMenuTable : public PopupMenuTable
{
   NoteTrackMenuTable() : mpData(NULL) {}
   DECLARE_POPUP_MENU(NoteTrackMenuTable);

public:
   static NoteTrackMenuTable &Instance();

private:
   void InitMenu(Menu*, void *pUserData) override
   {
      mpData = static_cast<TrackControls::InitMenuData*>(pUserData);
   }

   void DestroyMenu() override
   {
      mpData = nullptr;
   }

   TrackControls::InitMenuData *mpData;

   void OnChangeOctave(wxCommandEvent &);
};

NoteTrackMenuTable &NoteTrackMenuTable::Instance()
{
   static NoteTrackMenuTable instance;
   return instance;
}

enum {
   OnUpOctaveID = 30000,
   OnDownOctaveID,
};

/// This only applies to MIDI tracks.  Presumably, it shifts the
/// whole sequence by an octave.
void NoteTrackMenuTable::OnChangeOctave(wxCommandEvent &event)
{
   NoteTrack *const pTrack = static_cast<NoteTrack*>(mpData->pTrack);

   wxASSERT(event.GetId() == OnUpOctaveID
      || event.GetId() == OnDownOctaveID);

   const bool bDown = (OnDownOctaveID == event.GetId());
   pTrack->SetBottomNote
      (pTrack->GetBottomNote() + ((bDown) ? -12 : 12));

   AudacityProject *const project = ::GetActiveProject();
   project->ModifyState(true);
   mpData->result = RefreshCode::RefreshAll;
}

BEGIN_POPUP_MENU(NoteTrackMenuTable)
   POPUP_MENU_SEPARATOR()
   POPUP_MENU_ITEM(OnUpOctaveID, _("Up &Octave"), OnChangeOctave)
   POPUP_MENU_ITEM(OnDownOctaveID, _("Down Octa&ve"), OnChangeOctave)
END_POPUP_MENU()

PopupMenuTable *NoteTrackControls::GetMenuExtension(Track *)
{
#if defined(USE_MIDI)
   return &NoteTrackMenuTable::Instance();
#else
   return NULL;
#endif
}

#endif
