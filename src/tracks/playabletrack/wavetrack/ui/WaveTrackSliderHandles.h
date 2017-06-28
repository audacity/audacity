/**********************************************************************

Audacity: A Digital Audio Editor

WavelTrackSliderHandles.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_WAVE_TRACK_SLIDER_HANDLES__
#define __AUDACITY_WAVE_TRACK_SLIDER_HANDLES__

#include "../../../ui/SliderHandle.h"

class WaveTrack;

struct HitTestResult;

class GainSliderHandle final : public SliderHandle
{
   GainSliderHandle(const GainSliderHandle&) = delete;
   GainSliderHandle &operator=(const GainSliderHandle&) = delete;

   GainSliderHandle();
   virtual ~GainSliderHandle();
   static GainSliderHandle& Instance();

   std::shared_ptr<WaveTrack> GetWaveTrack();

protected:
   float GetValue() override;
   Result SetValue
      (AudacityProject *pProject, float newValue) override;
   Result CommitChanges
      (const wxMouseEvent &event, AudacityProject *pProject) override;

   bool StopsOnKeystroke () override { return true; }

public:
   static HitTestResult HitTest
      (const wxMouseEvent &event, const wxRect &rect,
       const AudacityProject *pProject, const std::shared_ptr<Track> &pTrack);
};

////////////////////////////////////////////////////////////////////////////////

class PanSliderHandle final : public SliderHandle
{
   PanSliderHandle(const PanSliderHandle&) = delete;
   PanSliderHandle &operator=(const PanSliderHandle&) = delete;

   PanSliderHandle();
   virtual ~PanSliderHandle();
   static PanSliderHandle& Instance();

   std::shared_ptr<WaveTrack> GetWaveTrack();

protected:
   float GetValue() override;
   Result SetValue(AudacityProject *pProject, float newValue) override;
   Result CommitChanges
      (const wxMouseEvent &event, AudacityProject *pProject) override;

   bool StopsOnKeystroke () override { return true; }

public:
   static HitTestResult HitTest
      (const wxMouseEvent &event, const wxRect &rect,
       const AudacityProject *pProject, const std::shared_ptr<Track> &pTrack);
};

#endif
