/**********************************************************************

Audacity: A Digital Audio Editor

SliderHandle.cpp

Paul Licameli

**********************************************************************/

#include "../../Audacity.h"
#include "SliderHandle.h"
#include "../../widgets/ASlider.h"
#include "../../HitTestResult.h"
#include "../../Project.h"
#include "../../RefreshCode.h"
#include "../../TrackPanelMouseEvent.h"

SliderHandle::SliderHandle()
{
}

SliderHandle::~SliderHandle()
{
}

HitTestPreview SliderHandle::HitPreview()
{
   // No special message or cursor
   return {};
}

UIHandle::Result SliderHandle::Click
(const TrackPanelMouseEvent &evt, AudacityProject *pProject)
{
   wxMouseEvent &event = evt.event;
   using namespace RefreshCode;
   if (!event.Button(wxMOUSE_BTN_LEFT))
      return Cancelled;

   // Come here for left click or double click
   mStartingValue = GetValue();
   GetSlider( pProject )->Set(mStartingValue);
   GetSlider( pProject )->OnMouseEvent(event);

   if (event.ButtonDClick())
      // Just did a modal dialog in OnMouseEvent
      // Do not start a drag
      return RefreshCell | Cancelled;
   else
      return RefreshCell;
}

UIHandle::Result SliderHandle::Drag
(const TrackPanelMouseEvent &evt, AudacityProject *pProject)
{
   wxMouseEvent &event = evt.event;
   using namespace RefreshCode;
   GetSlider( pProject )->OnMouseEvent(event);
   const float newValue = GetSlider( pProject )->Get();

   // Make a non-permanent change to the project data:
   return RefreshCell | SetValue(pProject, newValue);
}

HitTestPreview SliderHandle::Preview
(const TrackPanelMouseState &, const AudacityProject *)
{
   // No special message or cursor
   return {};
}

UIHandle::Result SliderHandle::Release
(const TrackPanelMouseEvent &evt, AudacityProject *pProject,
 wxWindow *)
{
   using namespace RefreshCode;
   wxMouseEvent &event = evt.event;
   GetSlider( pProject )->OnMouseEvent(event);
   const float newValue = GetSlider( pProject )->Get();

   Result result = RefreshCell;

   // Commit changes to the project data:
   result |= SetValue(pProject, newValue);
   result |= CommitChanges(event, pProject);

   mpTrack.reset();
   return result;
}

UIHandle::Result SliderHandle::Cancel(AudacityProject *pProject)
{
   wxMouseEvent event(wxEVT_LEFT_UP);
   GetSlider( pProject )->OnMouseEvent(event);

   // Undo un-committed changes to project data:
   auto result = SetValue(pProject, mStartingValue);
   mpTrack.reset();
   return RefreshCode::RefreshCell | result;
}

LWSlider *SliderHandle::GetSlider( AudacityProject *pProject )
{
   auto pTrack = pProject->GetTracks()->Lock(mpTrack);
   return mSliderFn( pProject, mRect, pTrack.get() );
}
