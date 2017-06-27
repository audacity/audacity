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
#include "../../../../TrackPanel.h"
#include "../../../../TrackPanelMouseEvent.h"
#include "../../../../UIHandle.h"

///////////////////////////////////////////////////////////////////////////////
// TODO: do appearance changes as in ButtonHandle, or even, inherit from that
class NoteTrackClickHandle : public UIHandle
{
   NoteTrackClickHandle(const NoteTrackClickHandle&);
   NoteTrackClickHandle &operator=(const NoteTrackClickHandle&);
   NoteTrackClickHandle();
   virtual ~NoteTrackClickHandle();
   static NoteTrackClickHandle& Instance();

public:
   static HitTestResult HitTest
      (const wxMouseEvent &event, const wxRect &rect,
       const std::shared_ptr<NoteTrack> &pTrack);

protected:
   virtual Result Click
      (const TrackPanelMouseEvent &event, AudacityProject *pProject);

   virtual Result Drag
      (const TrackPanelMouseEvent &event, AudacityProject *pProject);

   virtual HitTestPreview Preview
      (const TrackPanelMouseEvent &event, const AudacityProject *pProject);

   virtual Result Release
      (const TrackPanelMouseEvent &event, AudacityProject *pProject,
       wxWindow *pParent);

   virtual Result Cancel(AudacityProject *pProject);

   std::weak_ptr<NoteTrack> mpTrack;
   wxRect mRect{};
};

NoteTrackClickHandle::NoteTrackClickHandle()
{
}

NoteTrackClickHandle::~NoteTrackClickHandle()
{
}

NoteTrackClickHandle &NoteTrackClickHandle::Instance()
{
   static NoteTrackClickHandle instance;
   return instance;
}

HitTestResult NoteTrackClickHandle::HitTest
   (const wxMouseEvent &event, const wxRect &rect,
    const std::shared_ptr<NoteTrack> &pTrack)
{
   wxRect midiRect;
   TrackInfo::GetMidiControlsRect(rect, midiRect);
   if ( TrackInfo::HideTopItem( rect, midiRect ) )
      return {};
   if (pTrack->GetKind() == Track::Note &&
       midiRect.Contains(event.m_x, event.m_y)) {
         Instance().mpTrack = pTrack;
         Instance().mRect = midiRect;
         return {
            HitTestPreview(),
            &Instance()
         };
   }
   else
      return {};
}

UIHandle::Result NoteTrackClickHandle::Click
(const TrackPanelMouseEvent &, AudacityProject *)
{
   return RefreshCode::RefreshNone;
}

UIHandle::Result NoteTrackClickHandle::Drag
(const TrackPanelMouseEvent &, AudacityProject *)
{
   return RefreshCode::RefreshNone;
}

HitTestPreview NoteTrackClickHandle::Preview
(const TrackPanelMouseEvent &, const AudacityProject *)
{
   // No special message or cursor
   return {};
}

UIHandle::Result NoteTrackClickHandle::Release
(const TrackPanelMouseEvent &evt, AudacityProject *pProject, wxWindow *)
{
   using namespace RefreshCode;

   auto pTrack = mpTrack.lock();
   if (!pTrack)
      return Cancelled;

   const wxMouseEvent &event = evt.event;
   if (pTrack->LabelClick(mRect, event.m_x, event.m_y,
      event.Button(wxMOUSE_BTN_RIGHT))) {
      // No undo items needed??
      pProject->ModifyState(false);
      return RefreshAll;
   }
   return RefreshNone;
}

UIHandle::Result NoteTrackClickHandle::Cancel(AudacityProject *)
{
   return RefreshCode::RefreshNone;
}

///////////////////////////////////////////////////////////////////////////////
NoteTrackControls::~NoteTrackControls()
{
}

HitTestResult NoteTrackControls::HitTest
(const TrackPanelMouseEvent & evt,
 const AudacityProject *pProject)
{
   const wxMouseEvent &event = evt.event;
   const wxRect &rect = evt.rect;
   if (event.ButtonDown() || event.ButtonDClick()) {
      auto track = std::static_pointer_cast<NoteTrack>(FindTrack());
      if (track && track->GetKind() == Track::Note) {
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
             VelocitySliderHandle::HitTest(event, rect, pProject, track)).handle)
            return result;
         if (NULL != (result =
            NoteTrackClickHandle::HitTest(event, rect, track)).handle)
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