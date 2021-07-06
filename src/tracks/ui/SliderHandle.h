/**********************************************************************

Sneedacity: A Digital Audio Editor

SliderHandle.h

Paul Licameli

**********************************************************************/

#ifndef __SNEEDACITY_SLIDER_HANDLE__
#define __SNEEDACITY_SLIDER_HANDLE__

#include "../../UIHandle.h"

class wxMouseEvent;
class wxMouseState;
class LWSlider;
class Track;
class TranslatableString;

class SNEEDACITY_DLL_API SliderHandle /* not final */ : public UIHandle
{
   SliderHandle(const SliderHandle&) = delete;

public:
   using SliderFn = LWSlider *(*)( SneedacityProject*, const wxRect&, Track* );

   explicit SliderHandle
      ( SliderFn sliderFn, const wxRect &rect,
        const std::shared_ptr<Track> &pTrack );

   SliderHandle &operator=(const SliderHandle&) = default;

   std::shared_ptr<Track> GetTrack() const { return mpTrack.lock(); }
   bool IsClicked() const { return mIsClicked; }

protected:
   virtual ~SliderHandle();

   // These NEW abstract virtuals simplify the duties of further subclasses.
   // This class will decide whether to refresh the clicked cell for slider state
   // change.
   // Subclass can decide to refresh other things and the results will be ORed.
   virtual float GetValue() = 0;
   virtual Result SetValue(SneedacityProject *pProject, float newValue) = 0;
   virtual Result CommitChanges
      (const wxMouseEvent &event, SneedacityProject *pProject) = 0;

   // Define a message for the status bar and tooltip.
   virtual TranslatableString Tip(
      const wxMouseState &state, SneedacityProject &project) const = 0;
 
   void Enter(bool forward, SneedacityProject *) override;

   Result Click
      (const TrackPanelMouseEvent &event, SneedacityProject *pProject)
      final override;

   Result Drag
      (const TrackPanelMouseEvent &event, SneedacityProject *pProject)
      final override;

   HitTestPreview Preview
      (const TrackPanelMouseState &state, SneedacityProject *pProject)
      final override;

   Result Release
      (const TrackPanelMouseEvent &event, SneedacityProject *pProject,
       wxWindow *pParent) final override;

   Result Cancel(SneedacityProject *pProject) final override;

   // Derived class is expected to set these two before Click():
   std::weak_ptr<Track> mpTrack;
   wxRect mRect{};
   SliderFn mSliderFn;
   LWSlider *GetSlider( SneedacityProject *pProject );

   float mStartingValue {};

   bool mIsClicked{};
};

#endif
