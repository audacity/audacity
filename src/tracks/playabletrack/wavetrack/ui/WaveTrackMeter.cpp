/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file WaveTrackMeter.cpp
 
 Paul Licameli
 
 **********************************************************************/
#include "WaveTrackMeter.h"

#include "ActiveProject.h"
#include "AllThemeResources.h"
#include "Decibels.h"
#include "PeakAndRmsMeter.h"
#include "../../../../widgets/MeterPainter.h"
#include "TrackPanel.h"
#include "TransportUtilities.h"
#include "WaveTrack.h"

#include <wx/dc.h>
#include <wx/timer.h>

namespace {
constexpr long mMeterRefreshRate{ 30 };

struct WaveTrackRmsMeter
   : PeakAndRmsMeter
   , wxTimer
{
   using PeakAndRmsMeter::mStats;

   explicit WaveTrackRmsMeter(int dbRange, size_t nChannels)
      : PeakAndRmsMeter(dbRange)
   {
      mMeterDisabled = false;
      mNumBars = nChannels;
      mDB = true;
      Reset(44100.0, true);
   }
   ~WaveTrackRmsMeter() override = default;
   void Reset(double sampleRate, bool resetClipping) override {
      // wxTimers seem to be a little unreliable - sometimes they stop for
      // no good reason, so this "primes" it every now and then...
      wxTimer::Stop();

      // While it's stopped, empty the queue
      PeakAndRmsMeter::Reset(sampleRate, resetClipping);

      wxTimer::Start(1000 / mMeterRefreshRate);
   }
   void Notify() override {
      mNumChanges = 0;
      PeakAndRmsMeter::Poll();
      if (mNumChanges) {
         if (auto pProject = GetActiveProject().lock())
            TrackPanel::Get(*pProject).Refresh();
      }
   }
   void Receive(double, const MeterUpdateMsg &) override {
      ++mNumChanges;
   }

   unsigned mNumChanges{};
};

int ChooseBgColor(bool selected) {
   return selected ? clrTrackInfoSelected : clrTrackInfo;
}

const AttachedTrackObjects::RegisteredFactory key{
   [](Track &track){ return std::make_shared<WaveTrackMeter>(); } };

TransportUtilities::GetTrackMeters::WrapperScope scope {
   [](const auto &prevFn){ return [prevFn](WaveTrack &track){
      MeterPtrs results;
      if (prevFn)
         results = prevFn(track);
      results.push_back(WaveTrackMeter::Get(track).weak_from_this());
      return results;
   }; }
};
}

struct WaveTrackMeter::Impl{
   Impl(int dbRange, size_t nChannels)
      : mPainter{ true, true, false, ChooseBgColor(false) }
      , mpMeter{ std::make_unique<WaveTrackRmsMeter>(dbRange, nChannels) }
   {}

   MeterPainter mPainter;
   MeterBar mBar[kMaxMeterBars]{};
   std::unique_ptr<WaveTrackRmsMeter> mpMeter;
};

WaveTrackMeter &WaveTrackMeter::Get(WaveTrack &track)
{
   return track.AttachedTrackObjects::Get<WaveTrackMeter>(key);
}

WaveTrackMeter::WaveTrackMeter()
   : mpImpl{ std::make_unique<Impl>(DecibelScaleCutoff.Read(), 2) }
{
}

WaveTrackMeter::~WaveTrackMeter() = default;

void WaveTrackMeter::Draw(wxDC &dc, wxRect rect, bool selected) {
   auto &painter = mpImpl->mPainter;
   auto &bar = mpImpl->mBar;
   auto &meter = *mpImpl->mpMeter;
   const auto [_, __, width, height] = rect;
   if (!(mLastWidth == width && mLastHeight == height &&
      mLastSelected == selected))
   {
      // Recreate the bitmap in the painter for changed dimensions or
      // selectedness
      painter.SetBackgroundColor(ChooseBgColor(selected));
      const auto clip = painter.GetClip();
      auto rect0 = wxRect{ 0, 0, width, height };
      rect0.width -= (1 + MeterBar::gap);
      rect0.width /= 2;
      auto rect1 = rect0;
      rect1.x += rect0.width + 1 + MeterBar::gap;
      bar[1].SetRectangles(rect1, true, clip);
      bar[0].SetRectangles(rect0, true, clip);

      painter.AllocateBitmap(dc, width, height);
      for (unsigned int i = 0; i < 2; ++i)
         painter.FillBitmap(bar[i], false, meter.GetDBRange());

      mLastWidth = width;
      mLastHeight = height;
      mLastSelected = selected;
   }

   // Copy predrawn bitmap to the dest DC
   dc.DrawBitmap(*painter.GetBitmap(), rect.x, rect.y);

   {
      const auto origin = dc.GetLogicalOrigin();
      Finally Do{ [&]{ dc.SetLogicalOrigin(origin.x, origin.y); } };
      auto newOrigin = origin - wxPoint{ rect.x, rect.y };
      dc.SetLogicalOrigin(newOrigin.x, newOrigin.y);

      for (unsigned int i = 0; i < 2; ++i)
         painter.DrawMeterBar(dc, meter.IsDisabled(), bar[i],
            meter.mStats[i]);
   }
}

void WaveTrackMeter::Update(unsigned numChannels,
   unsigned long numFrames, const float *sampleData, bool interleaved)
{
   mpImpl->mpMeter->Update(numChannels, numFrames, sampleData, interleaved);
}

bool WaveTrackMeter::IsDisabled() const
{
   return mpImpl->mpMeter->IsDisabled();
}

void WaveTrackMeter::Clear()
{
   mpImpl->mpMeter->Clear();
}

void WaveTrackMeter::Reset(double sampleRate, bool resetClipping)
{
   mpImpl->mpMeter->Reset(sampleRate, resetClipping);
}
