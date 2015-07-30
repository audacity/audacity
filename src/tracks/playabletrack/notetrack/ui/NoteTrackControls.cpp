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

   void InitMenu(Menu*, void *pUserData) override
   {
      mpData = static_cast<TrackControls::InitMenuData*>(pUserData);
   }

   void DestroyMenu() override
   {
      mpData = nullptr;
   }

   TrackControls::InitMenuData *mpData;
};

NoteTrackMenuTable &NoteTrackMenuTable::Instance()
{
   static NoteTrackMenuTable instance;
   return instance;
}

BEGIN_POPUP_MENU(NoteTrackMenuTable)
END_POPUP_MENU()

PopupMenuTable *NoteTrackControls::GetMenuExtension(Track *)
{
   return &NoteTrackMenuTable::Instance();
}

#endif