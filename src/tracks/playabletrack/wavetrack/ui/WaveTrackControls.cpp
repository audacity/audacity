/**********************************************************************

Audacity: A Digital Audio Editor

WaveTrackControls.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../../../Audacity.h"
#include "WaveTrackControls.h"
#include "../../ui/PlayableTrackButtonHandles.h"
#include "WaveTrackSliderHandles.h"

#include "../../../../HitTestResult.h"
#include "../../../../WaveTrack.h"
#include "../../../../TrackPanel.h"
#include "../../../../TrackPanelMouseEvent.h"
#include "../../../../widgets/PopupMenuTable.h"

WaveTrackControls::WaveTrackControls()
{
}

WaveTrackControls &WaveTrackControls::Instance()
{
   static WaveTrackControls instance;
   return instance;
}

WaveTrackControls::~WaveTrackControls()
{
}


HitTestResult WaveTrackControls::HitTest
(const TrackPanelMouseEvent & evt,
 const AudacityProject *pProject)
{
   const wxMouseEvent &event = evt.event;
   const wxRect &rect = evt.rect;
   if (event.Button(wxMOUSE_BTN_LEFT)) {
      if (mpTrack->GetKind() == Track::Wave) {
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

         if (NULL != (result =
            GainSliderHandle::HitTest(event, rect, pProject, mpTrack)).handle)
            return result;

         if (NULL != (result =
            PanSliderHandle::HitTest(event, rect, pProject, mpTrack)).handle)
            return result;
      }
   }

   return TrackControls::HitTest(evt, pProject);
}

class WaveTrackMenuTable : public PopupMenuTable
{
   WaveTrackMenuTable() : mpData(NULL) {}
   DECLARE_POPUP_MENU(WaveTrackMenuTable);

public:
   static WaveTrackMenuTable &Instance();

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

WaveTrackMenuTable &WaveTrackMenuTable::Instance()
{
   static WaveTrackMenuTable instance;
   return instance;
}

BEGIN_POPUP_MENU(WaveTrackMenuTable)
END_POPUP_MENU()

PopupMenuTable *WaveTrackControls::GetMenuExtension(Track*)
{
   return &WaveTrackMenuTable::Instance();
}
