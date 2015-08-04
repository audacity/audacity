/**********************************************************************

Audacity: A Digital Audio Editor

NoteTrackControls.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../../../Audacity.h"

#ifdef USE_MIDI

#include "NoteTrackControls.h"
#include "../../ui/PlayableTrackButtonHandles.h"
#include "NoteTrackSliderHandles.h"

#include "../../../../HitTestResult.h"
#include "../../../../Track.h"
#include "../../../../TrackPanelMouseEvent.h"
#include "../../../../NoteTrack.h"
#include "../../../../widgets/PopupMenuTable.h"
#include "../../../../Project.h"
#include "../../../../RefreshCode.h"

NoteTrackControls::NoteTrackControls()
{
}

NoteTrackControls &NoteTrackControls::Instance()
{
   static NoteTrackControls instance;
   return instance;
}

NoteTrackControls::~NoteTrackControls()
{
}

HitTestResult NoteTrackControls::HitTest
(const TrackPanelMouseEvent & evt,
 const AudacityProject *pProject)
{
   const wxMouseEvent &event = evt.event;
   const wxRect &rect = evt.rect;
   if (event.Button(wxMOUSE_BTN_LEFT)) {
      if (mpTrack->GetKind() == Track::Note) {
         auto track = GetTrack();
         HitTestResult result;
         if (NULL !=
             (result = MuteButtonHandle::HitTest
                 (event, rect, pProject, track)).handle)
            return result;

         if (NULL !=
             (result = SoloButtonHandle::HitTest
                 (event, rect, pProject, track)).handle)
            return result;
#ifdef EXPERIMENTAL_MIDI_OUT
         if (NULL != (result =
             VelocitySliderHandle::HitTest(event, rect, pProject, mpTrack)).handle)
            return result;
#endif
      }
   }

   return TrackControls::HitTest(evt, pProject);
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
   wxASSERT(pTrack->GetKind() == Track::Note);

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