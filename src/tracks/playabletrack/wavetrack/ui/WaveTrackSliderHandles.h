/**********************************************************************

Sneedacity: A Digital Audio Editor

WavelTrackSliderHandles.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __SNEEDACITY_WAVE_TRACK_SLIDER_HANDLES__
#define __SNEEDACITY_WAVE_TRACK_SLIDER_HANDLES__

#include "../../../ui/SliderHandle.h"

class wxMouseState;
class WaveTrack;

class GainSliderHandle final : public SliderHandle
{
   GainSliderHandle(const GainSliderHandle&) = delete;

   std::shared_ptr<WaveTrack> GetWaveTrack() const;

public:
   explicit GainSliderHandle
      ( SliderFn sliderFn, const wxRect &rect,
        const std::shared_ptr<Track> &pTrack );

   GainSliderHandle &operator=(const GainSliderHandle&) = default;

   virtual ~GainSliderHandle();

protected:
   float GetValue() override;
   Result SetValue
      (SneedacityProject *pProject, float newValue) override;
   Result CommitChanges
      (const wxMouseEvent &event, SneedacityProject *pProject) override;

   TranslatableString Tip(
      const wxMouseState &state, SneedacityProject &) const override;

   bool StopsOnKeystroke () override { return true; }

public:
   static UIHandlePtr HitTest
      (std::weak_ptr<GainSliderHandle> &holder,
       const wxMouseState &state, const wxRect &rect,
       const std::shared_ptr<Track> &pTrack);
};

////////////////////////////////////////////////////////////////////////////////

class PanSliderHandle final : public SliderHandle
{
   PanSliderHandle(const PanSliderHandle&) = delete;

   std::shared_ptr<WaveTrack> GetWaveTrack() const;

public:
   explicit PanSliderHandle
      ( SliderFn sliderFn, const wxRect &rect,
        const std::shared_ptr<Track> &pTrack );

   PanSliderHandle &operator=(const PanSliderHandle&) = default;

   virtual ~PanSliderHandle();

protected:
   float GetValue() override;
   Result SetValue(SneedacityProject *pProject, float newValue) override;
   Result CommitChanges
      (const wxMouseEvent &event, SneedacityProject *pProject) override;

   TranslatableString Tip(
      const wxMouseState &state, SneedacityProject &) const override;

   bool StopsOnKeystroke () override { return true; }

public:
   static UIHandlePtr HitTest
      (std::weak_ptr<PanSliderHandle> &holder,
       const wxMouseState &state, const wxRect &rect,
       const std::shared_ptr<Track> &pTrack);
};

#endif
