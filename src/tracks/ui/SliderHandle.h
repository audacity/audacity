/**********************************************************************

Audacity: A Digital Audio Editor

SliderHandle.h

Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_SLIDER_HANDLE__
#define __AUDACITY_SLIDER_HANDLE__

#include "../../MemoryX.h"
#include "../../UIHandle.h"
#include <wx/gdicmn.h>

class wxMouseEvent;
class LWSlider;
class Track;

class SliderHandle /* not final */ : public UIHandle
{
   SliderHandle(const SliderHandle&) = delete;

public:
   using SliderFn = LWSlider *(*)( AudacityProject*, const wxRect&, Track* );

   explicit SliderHandle
      ( SliderFn sliderFn, const wxRect &rect,
        const std::shared_ptr<Track> &pTrack );

   SliderHandle &operator=(const SliderHandle&) = default;

   std::shared_ptr<Track> GetTrack() const { return mpTrack.lock(); }
   bool IsClicked() const { return mIsClicked; }

protected:
   virtual ~SliderHandle();

   // These new abstract virtuals simplify the duties of further subclasses.
   // This class will decide whether to refresh the clicked cell for slider state
   // change.
   // Subclass can decide to refresh other things and the results will be ORed.
   virtual float GetValue() = 0;
   virtual Result SetValue(AudacityProject *pProject, float newValue) = 0;
   virtual Result CommitChanges
      (const wxMouseEvent &event, AudacityProject *pProject) = 0;

   Result Click
      (const TrackPanelMouseEvent &event, AudacityProject *pProject) override;

   Result Drag
      (const TrackPanelMouseEvent &event, AudacityProject *pProject) override;

   HitTestPreview Preview
      (const TrackPanelMouseState &state, const AudacityProject *pProject)
      override;

   Result Release
      (const TrackPanelMouseEvent &event, AudacityProject *pProject,
       wxWindow *pParent) override;

   Result Cancel(AudacityProject *pProject) override;

   // Derived class is expected to set these two before Click():
   std::weak_ptr<Track> mpTrack;
   wxRect mRect{};
   SliderFn mSliderFn;
   LWSlider *GetSlider( AudacityProject *pProject );

   float mStartingValue {};

   bool mIsClicked{};
};

#endif
