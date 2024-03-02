/**********************************************************************

  Audacity: A Digital Audio Editor

  WaveTrack.cpp

  Dominic Mazzoni

*******************************************************************//**

\class WaveTrack
\brief A Track that contains audio waveform data.

*//****************************************************************//**

\class WaveTrack::Location
\brief Used only by WaveTrack, a special way to hold location that
can accommodate merged regions.

*//****************************************************************/

/*!
@class WaveTrackFactory
@brief Used to create or clone a WaveTrack, with appropriate context
from the project that will own the track.
*/


#include "WaveTrack.h"

#include "WaveClip.h"

#include <wx/defs.h>
#include <wx/debug.h>
#include <wx/log.h>

#include <algorithm>
#include <float.h>
#include <math.h>
#include <numeric>
#include <optional>
#include <type_traits>
#include <unordered_set>

#include "float_cast.h"

#include "AudioSegmentSampleView.h"
#include "ChannelAttachments.h"
#include "ClipTimeAndPitchSource.h"
#include "Envelope.h"
#include "Sequence.h"
#include "StaffPadTimeAndPitch.h"

#include "TempoChange.h"
#include "Project.h"
#include "ProjectRate.h"
#include "SampleBlock.h"

#include "BasicUI.h"
#include "Prefs.h"
#include "QualitySettings.h"
#include "SyncLock.h"
#include "TimeWarper.h"


#include "InconsistencyException.h"

#include "ProjectFormatExtensionsRegistry.h"

#include <cmath>

using std::max;

WaveChannelInterval::WaveChannelInterval(WaveClipHolder pWideClip,
   WaveClipHolder pNarrowClip, size_t iChannel
)  : mpWideClip{ move(pWideClip) }
   , mpNarrowClip{ move(pNarrowClip) }
   , miChannel{ iChannel }
{
   assert(mpWideClip != nullptr);
   assert(mpNarrowClip != nullptr);
}

WaveChannelInterval::~WaveChannelInterval() = default;

bool WaveChannelInterval::Intersects(double t0, double t1) const
{
   return GetNarrowClip().IntersectsPlayRegion(t0, t1);
}

double WaveChannelInterval::Start() const
{
   return GetNarrowClip().GetPlayStartTime();
}

double WaveChannelInterval::End() const
{
   return GetNarrowClip().GetPlayEndTime();
}

AudioSegmentSampleView
WaveChannelInterval::GetSampleView(double t0, double t1, bool mayThrow) const
{
   constexpr auto iChannel = 0u;
   // TODO wide wave tracks: use the real channel number.
   return GetNarrowClip().GetSampleView(iChannel, t0, t1, mayThrow);
}

const Envelope &WaveChannelInterval::GetEnvelope() const
{
   // Always the left clip's envelope
   return *GetWideClip().GetEnvelope();
}

Envelope &WaveChannelInterval::GetEnvelope()
{
   // Always the left clip's envelope
   return *GetWideClip().GetEnvelope();
}

sampleCount WaveChannelInterval::GetVisibleSampleCount() const
{
   return GetNarrowClip().GetVisibleSampleCount();
}

int WaveChannelInterval::GetRate() const
{
   return GetNarrowClip().GetRate();
}

double WaveChannelInterval::GetPlayStartTime() const
{
   return GetNarrowClip().GetPlayStartTime();
}

double WaveChannelInterval::GetPlayEndTime() const
{
   return GetNarrowClip().GetPlayEndTime();
}

double WaveChannelInterval::GetPlayDuration() const
{
   return GetPlayEndTime() - GetPlayStartTime();
}

bool WaveChannelInterval::WithinPlayRegion(double t) const
{
   return GetNarrowClip().WithinPlayRegion(t);
}

sampleCount WaveChannelInterval::TimeToSamples(double time) const
{
   return GetNarrowClip().TimeToSamples(time);
}

double WaveChannelInterval::SamplesToTime(sampleCount s) const noexcept
{
   return GetNarrowClip().SamplesToTime(s);
}

double WaveChannelInterval::GetStretchRatio() const
{
   return GetNarrowClip().GetStretchRatio();
}

bool WaveChannelInterval::HasPitchOrSpeed() const
{
   return GetNarrowClip().HasPitchOrSpeed();
}

double WaveChannelInterval::GetTrimLeft() const
{
   return GetNarrowClip().GetTrimLeft();
}

double WaveChannelInterval::GetTrimRight() const
{
   return GetNarrowClip().GetTrimRight();
}

bool WaveChannelInterval::GetSamples(samplePtr buffer, sampleFormat format,
   sampleCount start, size_t len, bool mayThrow) const
{
   // Not just a pass-through, but supply the first argument
   // TODO wide wave tracks -- pass miChannel not 0
   return GetNarrowClip().GetSamples(0, buffer, format, start, len, mayThrow);
}

AudioSegmentSampleView WaveChannelInterval::GetSampleView(
   sampleCount start, size_t length, bool mayThrow) const
{
   // Not just a pass-through, but supply the first argument
   // TODO wide wave tracks -- pass miChannel not 0
   return GetNarrowClip().GetSampleView(0, start, length, mayThrow);
}

const Sequence &WaveChannelInterval::GetSequence() const
{
   // TODO wide wave tracks -- use miChannel
   const auto pSequence = GetNarrowClip().GetSequence(0);
   // Assume sufficiently wide clip
   assert(pSequence);
   return *pSequence;
}

constSamplePtr WaveChannelInterval::GetAppendBuffer() const
{
   // TODO wide wave tracks -- use miChannel
   return GetNarrowClip().GetAppendBuffer(0);
}

size_t WaveChannelInterval::GetAppendBufferLen() const
{
   // TODO wide wave tracks -- use miChannel
   return GetNarrowClip().GetAppendBufferLen(0);
}

BlockArray *WaveChannelInterval::GetSequenceBlockArray()
{
   return GetNarrowClip().GetSequenceBlockArray(
      0
      // TODO wide wave tracks -- miChannel
   );
}

std::pair<float, float>
WaveChannelInterval::GetMinMax(double t0, double t1, bool mayThrow) const
{
   return GetNarrowClip().GetMinMax(
      // TODO wide wave tracks -- miChannel
      0, t0, t1, mayThrow);
}

float WaveChannelInterval::GetRMS(double t0, double t1, bool mayThrow) const
{
   return GetNarrowClip().GetRMS(
      // TODO wide wave tracks -- miChannel
      0, t0, t1, mayThrow);
}

sampleCount WaveChannelInterval::GetPlayStartSample() const
{
   return GetNarrowClip().GetPlayStartSample();
}

sampleCount WaveChannelInterval::GetPlayEndSample() const
{
   return GetNarrowClip().GetPlayEndSample();
}

void WaveChannelInterval::SetSamples(constSamplePtr buffer, sampleFormat format,
   sampleCount start, size_t len, sampleFormat effectiveFormat)
{
   return GetNarrowClip().SetSamples(
      // TODO wide wave tracks -- miChannel
      0, buffer, format, start, len, effectiveFormat);
}

void WaveChannelInterval::WriteXML(XMLWriter &xmlFile) const
{
   // TODO wide wave tracks -- use miChannel
   GetNarrowClip().WriteXML(0, xmlFile);
}

WaveTrack::Interval::Interval(const ChannelGroup &group,
   const std::shared_ptr<WaveClip> &pClip,
   const std::shared_ptr<WaveClip> &pClip1
)  : WideChannelGroupInterval{ group }
   , mpClip{ pClip }
   , mpClip1{ pClip1 }
{
}

WaveTrack::Interval::Interval(
   const ChannelGroup& group, size_t width,
   const SampleBlockFactoryPtr& factory, int rate, sampleFormat format)
    : Interval(
         group, std::make_shared<WaveClip>(1, factory, format, rate),
         width == 2 ?
            std::make_shared<WaveClip>(1, factory, format, rate) :
            nullptr)
{
}

WaveTrack::Interval::~Interval() = default;

double WaveTrack::Interval::Start() const
{
   return mpClip->GetPlayStartTime();
}

double WaveTrack::Interval::End() const
{
   return mpClip->GetPlayEndTime();
}

size_t WaveTrack::Interval::GetBestBlockSize(sampleCount start) const
{
   return GetClip(0)->GetSequence(0)->GetBestBlockSize(start);
}

size_t WaveTrack::Interval::GetMaxBlockSize() const
{
   auto result = GetClip(0)->GetSequence(0)->GetMaxBlockSize();
   if (NChannels() > 1)
      result =
         std::max(result, GetClip(1)->GetSequence(0)->GetMaxBlockSize());
   return result;
}

sampleCount WaveTrack::Interval::GetSequenceStartSample() const
{
   return GetClip(0)->GetSequenceStartSample();
}

bool WaveTrack::Interval::EqualSequenceLengthInvariant() const
{
   if (NChannels() < 2)
      return true;
   const auto &pClip0 = GetClip(0);
   const auto &pClip1 = GetClip(1);
   return
      pClip0->GetSequenceStartTime() == pClip1->GetSequenceStartTime()
   &&
      pClip0->GetSequenceEndTime() == pClip1->GetSequenceEndTime()
   &&
      pClip0->GetPlayStartTime() == pClip1->GetPlayStartTime()
   &&
      pClip0->GetPlayEndTime() == pClip1->GetPlayEndTime()
   ;
}

void WaveTrack::Interval::Append(
   constSamplePtr buffer[], sampleFormat format, size_t len)
{
   for (unsigned channel = 0; channel < NChannels(); ++channel)
      GetClip(channel)->AppendNewBlock(buffer[channel], format, len);
}

void WaveTrack::Interval::Flush()
{
   ForEachClip([](auto& clip) { clip.Flush(); });
}

void WaveTrack::Interval::RepairChannels()
{
   ForEachClip([](auto& clip) { clip.RepairChannels(); });
}

void WaveTrack::Interval::Clear(double t0, double t1)
{
   ForEachClip([&](auto& clip) { clip.Clear(t0, t1); });
}

void WaveTrack::Interval::TrimLeftTo(double t)
{
   for(unsigned channel = 0; channel < NChannels(); ++channel)
      GetClip(channel)->TrimLeftTo(t);
}

void WaveTrack::Interval::TrimRightTo(double t)
{
   for(unsigned channel = 0; channel < NChannels(); ++channel)
      GetClip(channel)->TrimRightTo(t);
}

void WaveTrack::Interval::TrimQuarternotesFromRight(double quarters)
{
   ForEachClip(
      [quarters](auto& clip) { clip.TrimQuarternotesFromRight(quarters); });
}

void WaveTrack::Interval::SetTrimLeft(double t)
{
   for(unsigned channel = 0; channel < NChannels(); ++channel)
      GetClip(channel)->SetTrimLeft(t);
}

void WaveTrack::Interval::SetTrimRight(double t)
{
   for(unsigned channel = 0; channel < NChannels(); ++channel)
      GetClip(channel)->SetTrimRight(t);
}

void WaveTrack::Interval::TrimLeft(double deltaTime)
{
   ForEachClip([&](auto &clip) { clip.TrimLeft(deltaTime); });
}

void WaveTrack::Interval::TrimRight(double deltaTime)
{
   ForEachClip([&](auto &clip) { clip.TrimRight(deltaTime); });
}

void WaveTrack::Interval::ClearLeft(double t)
{
   for(unsigned channel = 0; channel < NChannels(); ++channel)
      GetClip(channel)->ClearLeft(t);
}

void WaveTrack::Interval::ClearRight(double t)
{
   for(unsigned channel = 0; channel < NChannels(); ++channel)
      GetClip(channel)->ClearRight(t);
}

void WaveTrack::Interval::ClearAndAddCutLine(double t0, double t1)
{
   ForEachClip(
      [&](auto& clip) { clip.ClearAndAddCutLine(t0, t1); });
}

void WaveTrack::Interval::AddCutLine(Interval &interval)
{
   assert(NChannels() == interval.NChannels());
   size_t ii = 0;
   ForEachClip([&](auto &clip) { clip.AddCutLine(interval.GetClip(ii++)); });
}

void WaveTrack::Interval::StretchLeftTo(double t)
{
   for(unsigned channel = 0; channel < NChannels(); ++channel)
      GetClip(channel)->StretchLeftTo(t);
}

void WaveTrack::Interval::StretchRightTo(double t)
{
   for(unsigned channel = 0; channel < NChannels(); ++channel)
      GetClip(channel)->StretchRightTo(t);
}

void WaveTrack::Interval::StretchBy(double ratio)
{
   for (unsigned channel = 0; channel < NChannels(); ++channel)
      GetClip(channel)->StretchBy(ratio);
}

bool WaveTrack::Interval::SetCentShift(int cents)
{
   for (unsigned channel = 0; channel < NChannels(); ++channel)
      if (!GetClip(channel)->SetCentShift(cents))
         return false;
   return true;
}

WaveTrack::IntervalHolder WaveTrack::Interval::GetRenderedCopy(
   const std::function<void(double)>& reportProgress, const ChannelGroup& group,
   const SampleBlockFactoryPtr& factory, sampleFormat format)
{
   if (!HasPitchOrSpeed())
      return std::make_shared<Interval>(group, mpClip, mpClip1);

   const auto dst = std::make_shared<Interval>(
      group, NChannels(), factory, mpClip->GetRate(), format);

   const auto originalPlayStartTime = GetPlayStartTime();
   const auto originalPlayEndTime = GetPlayEndTime();
   const auto stretchRatio = GetStretchRatio();

   auto success = false;
   Finally Do { [&] {
      if (!success)
      {
         TrimLeftTo(originalPlayStartTime);
         TrimRightTo(originalPlayEndTime);
      }
   } };

   // Leave 1 second of raw, unstretched audio before and after visible region
   // to give the algorithm a chance to be in a steady state when reaching the
   // play boundaries.
   const auto tmpPlayStartTime =
      std::max(GetSequenceStartTime(), originalPlayStartTime - stretchRatio);
   const auto tmpPlayEndTime =
      std::min(GetSequenceEndTime(), originalPlayEndTime + stretchRatio);
   TrimLeftTo(tmpPlayStartTime);
   TrimRightTo(tmpPlayEndTime);

   constexpr auto sourceDurationToDiscard = 0.;
   constexpr auto blockSize = 1024;
   const auto numChannels = NChannels();
   ClipTimeAndPitchSource stretcherSource { *this, sourceDurationToDiscard,
                                            PlaybackDirection::forward };
   TimeAndPitchInterface::Parameters params;
   params.timeRatio = stretchRatio;
   params.pitchRatio = std::pow(2., mpClip->GetCentShift() / 1200.);
   StaffPadTimeAndPitch stretcher { mpClip->GetRate(), numChannels,
                                    stretcherSource, std::move(params) };

   // Post-rendering sample counts, i.e., stretched units
   const auto totalNumOutSamples =
      sampleCount { GetVisibleSampleCount().as_double() *
                    stretchRatio };

   sampleCount numOutSamples { 0 };
   AudioContainer container(blockSize, numChannels);

   while (numOutSamples < totalNumOutSamples)
   {
      const auto numSamplesToGet =
         limitSampleBufferSize(blockSize, totalNumOutSamples - numOutSamples);
      stretcher.GetSamples(container.Get(), numSamplesToGet);
      constSamplePtr data[2];
      data[0] = reinterpret_cast<constSamplePtr>(container.Get()[0]);
      if (NChannels() == 2)
         data[1] = reinterpret_cast<constSamplePtr>(container.Get()[1]);
      dst->Append(data, floatSample, numSamplesToGet);
      numOutSamples += numSamplesToGet;
      if (reportProgress)
         reportProgress(
            numOutSamples.as_double() / totalNumOutSamples.as_double());
   }
   dst->Flush();

   // Now we're all like `this` except unstretched. We can clear leading and
   // trailing, stretching transient parts.
   dst->SetPlayStartTime(tmpPlayStartTime);
   dst->ClearLeft(originalPlayStartTime);
   dst->ClearRight(originalPlayEndTime);

   // We don't preserve cutlines but the relevant part of the envelope.
   Envelope dstEnvelope = GetEnvelope();
   const auto samplePeriod = 1. / mpClip->GetRate();
   dstEnvelope.CollapseRegion(
      originalPlayEndTime, GetSequenceEndTime() + samplePeriod, samplePeriod);
   dstEnvelope.CollapseRegion(0, originalPlayStartTime, samplePeriod);
   dstEnvelope.SetOffset(originalPlayStartTime);
   dst->SetEnvelope(dstEnvelope);

   success = true;

   assert(!dst->HasPitchOrSpeed());
   return dst;
}

bool WaveTrack::Interval::HasPitchOrSpeed() const
{
   // Assuming equal pitch and speed on both channels
   return GetClip(0u)->HasPitchOrSpeed();
}

bool WaveTrack::Interval::HasEqualPitchAndSpeed(const Interval& other) const
{
   // Assuming equal pitch and speed on both channels
   return GetClip(0u)->HasEqualPitchAndSpeed(*other.GetClip(0u));
}

void WaveTrack::Interval::OnProjectTempoChange(
   const std::optional<double>& oldTempo, double newTempo)
{
   ForEachClip([&](WaveClip &clip){
      clip.OnProjectTempoChange(oldTempo, newTempo); });
}

/** Insert silence at the end, and causes the envelope to ramp
    linearly to the given value */
void WaveTrack::Interval::AppendSilence(double len, double envelopeValue)
{
   ForEachClip([&](WaveClip &clip){ clip.AppendSilence(len, envelopeValue); });
}

bool WaveTrack::Interval::Paste(double t0, const Interval &src)
{
   bool result = true;
   ForCorrespondingClips(*this, src,
   [&](WaveClip &dstClip, const WaveClip &srcClip){
      result = result && dstClip.Paste(t0, srcClip);
   });
   return result;
}

/** Insert silence - note that this is an efficient operation for large
 * amounts of silence */
void WaveTrack::Interval::InsertSilence(
   double t, double len, double *pEnvelopeValue)
{
   ForEachClip([&](auto& clip) { clip.InsertSilence(t, len, pEnvelopeValue); });
}

void WaveTrack::Interval::ShiftBy(double delta) noexcept
{
   ForEachClip([&](auto& clip) { clip.ShiftBy(delta); });
}

sampleCount WaveTrack::Interval::GetSequenceSamplesCount() const
{
   sampleCount result{};
   ForEachClip([&](auto& clip) { result += clip.GetSequenceSamplesCount(); });
   return result;
}

size_t WaveTrack::Interval::CountBlocks() const
{
   size_t result{};
   ForEachClip([&](const WaveClip &clip){
      const auto width = clip.GetWidth();
      for (size_t ii = 0; ii < width; ++ii)
         result += clip.GetSequenceBlockArray(ii)->size(); });
   return result;
}

void WaveTrack::Interval::ConvertToSampleFormat(sampleFormat format,
   const std::function<void(size_t)> & progressReport)
{
   // TODO fix progress denominator?
   ForEachClip([&](auto& clip) {
      clip.ConvertToSampleFormat(format, progressReport); });
}

//! Silences the 'length' amount of samples starting from 'offset'
//! (relative to the play start)
void WaveTrack::Interval::SetSilence(sampleCount offset, sampleCount length)
{
   ForEachClip([&](auto& clip) { clip.SetSilence(offset, length); });
}

void WaveTrack::Interval::CloseLock() noexcept
{
   ForEachClip(std::mem_fn(&WaveClip::CloseLock));
}

SampleFormats WaveTrack::Interval::GetSampleFormats() const
{
   return GetClip(0)->GetSampleFormats();
}

bool WaveTrack::Interval::RemoveCutLine(double cutLinePosition)
{
   bool result = false;
   ForEachClip([&](auto& clip) {
      result = clip.RemoveCutLine(cutLinePosition) || result; });
   return true;
}

auto WaveTrack::Interval::GetCutLines(WaveTrack &track) -> IntervalHolders
{
   if (!mpClip) {
      assert(false);
      return {};
   }
   auto &cutLines0 = mpClip->GetCutLines();
   size_t nCutLines0 = cutLines0.size();
   auto *pCutLines1 = mpClip1 ? &mpClip1->GetCutLines() : nullptr;
   auto nCutLines1 = pCutLines1 ? pCutLines1->size() : 0;

   std::vector<IntervalHolder> result;
   result.reserve(nCutLines0);
   for (size_t ii = 0; ii < nCutLines0; ++ii) {
      auto pClip0 = cutLines0[ii];
      auto pClip1 = ii < nCutLines1 ? (*pCutLines1)[ii] : nullptr;
      auto pInterval = std::make_shared<Interval>(track, pClip0, pClip1);
      result.emplace_back(move(pInterval));
   }
   return result;
}

auto WaveTrack::Interval::GetCutLines(const WaveTrack &track) const
   -> IntervalConstHolders
{
   auto results =
      const_cast<Interval&>(*this).GetCutLines(const_cast<WaveTrack&>(track));
   return { results.begin(), results.end() };
}

void WaveTrack::Interval::Resample(int rate, BasicUI::ProgressDialog *progress)
{
   // TODO Progress denominator?
   ForEachClip([&](auto& clip) { clip.Resample(rate, progress); });
}

void WaveTrack::Interval::SetName(const wxString& name)
{
   ForEachClip([&](auto& clip) { clip.SetName(name); });
}

const wxString& WaveTrack::Interval::GetName() const
{
   //TODO wide wave tracks:  assuming that all 'narrow' clips share common name
   return mpClip->GetName();
}

int WaveTrack::Interval::GetRate() const
{
   return mpClip->GetRate();
}

sampleCount WaveTrack::Interval::GetVisibleSampleCount() const
{
   return mpClip->GetVisibleSampleCount();
}

size_t WaveTrack::Interval::NumCutLines() const
{
   return mpClip->NumCutLines();
}

void WaveTrack::Interval::SetPlayStartTime(double time)
{
   ForEachClip([&](auto& clip) { clip.SetPlayStartTime(time); });
}

double WaveTrack::Interval::GetPlayStartTime() const
{
   //TODO wide wave tracks:  assuming that all 'narrow' clips share common beginning
   return mpClip->GetPlayStartTime();
}

double WaveTrack::Interval::GetPlayEndTime() const
{
   // TODO wide wave tracks:  assuming that all 'narrow' clips share common
   // beginning
   return mpClip->GetPlayEndTime();
}

sampleCount WaveTrack::Interval::GetPlayStartSample() const
{
   return mpClip->GetPlayStartSample();
}

sampleCount WaveTrack::Interval::GetPlayEndSample() const
{
   return mpClip->GetPlayEndSample();
}

bool WaveTrack::Interval::SplitsPlayRegion(double t) const
{
   return mpClip->SplitsPlayRegion(t);
}

bool WaveTrack::Interval::WithinPlayRegion(double t) const
{
   return mpClip->WithinPlayRegion(t);
}

bool WaveTrack::Interval::BeforePlayRegion(double t) const
{
   return mpClip->BeforePlayRegion(t);
}

bool WaveTrack::Interval::AtOrBeforePlayRegion(double t) const
{
   return mpClip->AtOrBeforePlayRegion(t);
}

bool WaveTrack::Interval::AfterPlayRegion(double t) const
{
   return mpClip->AfterPlayRegion(t);
}

bool WaveTrack::Interval::EntirelyWithinPlayRegion(double t0, double t1) const
{
   return mpClip->EntirelyWithinPlayRegion(t0, t1);
}

bool WaveTrack::Interval::PartlyWithinPlayRegion(double t0, double t1) const
{
   return mpClip->PartlyWithinPlayRegion(t0, t1);
}

bool WaveTrack::Interval::IntersectsPlayRegion(double t0, double t1) const
{
   return mpClip->IntersectsPlayRegion(t0, t1);
}

bool WaveTrack::Interval::CoversEntirePlayRegion(double t0, double t1) const
{
   return mpClip->CoversEntirePlayRegion(t0, t1);
}

double WaveTrack::Interval::GetStretchRatio() const
{
   //TODO wide wave tracks:  assuming that all 'narrow' clips share common stretch ratio
   return mpClip->GetStretchRatio();
}

int WaveTrack::Interval::GetCentShift() const
{
   return mpClip->GetCentShift();
}

void WaveTrack::Interval::SetRawAudioTempo(double tempo)
{
   ForEachClip([&](auto& clip) { clip.SetRawAudioTempo(tempo); });
}

sampleCount WaveTrack::Interval::TimeToSamples(double time) const
{
   return mpClip->TimeToSamples(time);
}

double WaveTrack::Interval::SamplesToTime(sampleCount s) const
{
   return mpClip->SamplesToTime(s);
}

double WaveTrack::Interval::GetSequenceStartTime() const
{
   return mpClip->GetSequenceStartTime();
}

void WaveTrack::Interval::SetSequenceStartTime(double t)
{
   ForEachClip([t](auto& clip) { clip.SetSequenceStartTime(t); });
}

double WaveTrack::Interval::GetSequenceEndTime() const
{
   return mpClip->GetSequenceEndTime();
}

double WaveTrack::Interval::GetTrimLeft() const
{
   //TODO wide wave tracks:  assuming that all 'narrow' clips share common trims
   return mpClip->GetTrimLeft();
}

double WaveTrack::Interval::GetTrimRight() const
{
   //TODO wide wave tracks:  assuming that all 'narrow' clips share common trims
   return mpClip->GetTrimRight();
}

AudioSegmentSampleView WaveTrack::Interval::GetSampleView(
   size_t ii, sampleCount start, size_t len, bool mayThrow) const
{
   return GetClip(ii)->GetSampleView(0u, start, len, mayThrow);
}

size_t WaveTrack::Interval::GetWidth() const
{
   return NChannels();
}

Observer::Subscription
WaveTrack::Interval::SubscribeToCentShiftChange(std::function<void(int)> cb)
const
{
   // On purpose set the publisher on the left channel only. This is not a clip
   // property that is saved to disk, and else we'll get two callbacks for the
   // same event.
   return mpClip->SubscribeToCentShiftChange(std::move(cb));
}

bool WaveTrack::Interval::IsPlaceholder() const
{
   return mpClip->GetIsPlaceholder();
}

void WaveTrack::Interval::SetIsPlaceholder(bool val)
{
   ForEachClip([=](auto &clip){ clip.SetIsPlaceholder(val); });
}

Envelope& WaveTrack::Interval::GetEnvelope()
{
   return *mpClip->GetEnvelope();
}

const Envelope& WaveTrack::Interval::GetEnvelope() const
{
   return *mpClip->GetEnvelope();
}


bool WaveTrack::Interval::FindCutLine(double cutLinePosition,
   double* cutLineStart, double *cutLineEnd) const
{
   return mpClip->FindCutLine(cutLinePosition, cutLineStart, cutLineEnd);
}

void WaveTrack::Interval::ExpandCutLine(double cutlinePosition)
{
   // TODO stronger exception safety guarantee in case the second expansion
   // throws
   ForEachClip([=](auto &clip){ clip.ExpandCutLine(cutlinePosition); });
   assert(EqualSequenceLengthInvariant());
}

void WaveTrack::Interval::SetRate(int rate)
{
   ForEachClip([=](auto &clip){ clip.SetRate(rate); });
}

void WaveTrack::Interval::SetEnvelope(const Envelope& envelope)
{
   mpClip->SetEnvelope(std::make_unique<Envelope>(envelope));
}

std::shared_ptr<ChannelInterval>
WaveTrack::Interval::DoGetChannel(size_t iChannel)
{
   if (iChannel < NChannels()) {
      // TODO wide wave tracks: there will be only one, wide clip
      const auto pClip = (iChannel == 0 ? mpClip : mpClip1);
      return std::make_shared<WaveChannelInterval>(mpClip, pClip, iChannel);
   }
   return {};
}

std::shared_ptr<const WaveTrack::Interval>
WaveTrack::GetNextInterval(const Interval& interval, PlaybackDirection searchDirection) const
{
   std::shared_ptr<const Interval> result;
   auto bestMatchTime = searchDirection == PlaybackDirection::forward
      ? std::numeric_limits<double>::max()
      : std::numeric_limits<double>::lowest();

   for (const auto &other : Intervals()) {
      if((searchDirection == PlaybackDirection::forward &&
         (other->Start() > interval.Start() && other->Start() < bestMatchTime))
         ||
         (searchDirection == PlaybackDirection::backward &&
         (other->Start() < interval.Start() && other->Start() > bestMatchTime)))
      {
         result = other;
         bestMatchTime = other->Start();
      }
   }
   return result;
}

WaveTrack::IntervalHolder WaveTrack::GetNextInterval(
   const Interval& interval, PlaybackDirection searchDirection)
{
   return std::const_pointer_cast<Interval>(
      std::as_const(*this).GetNextInterval(interval, searchDirection));
}

WaveTrack::IntervalHolder WaveTrack::GetIntervalAtTime(double t)
{
   IntervalHolder result;
   for (const auto &interval : Intervals())
      if (interval->WithinPlayRegion(t))
         return interval;
   return nullptr;
}

namespace {
struct WaveTrackData : ClientData::Cloneable<> {
   WaveTrackData() = default;
   WaveTrackData(const WaveTrackData &);
   WaveTrackData& operator=(const WaveTrackData &) = delete;
   ~WaveTrackData() override;
   std::unique_ptr<ClientData::Cloneable<>> Clone() const override;

   static WaveTrackData &Get(WaveTrack &track);
   static const WaveTrackData &Get(const WaveTrack &track);

   double GetOrigin() const;
   void SetOrigin(double origin);

   sampleFormat GetSampleFormat() const;
   void SetSampleFormat(sampleFormat format);

   float GetGain() const;
   void SetGain(float value);
   float GetPan() const;
   void SetPan(float value);

   int GetRate() const;
   void SetRate(int value);

private:
   //! Atomic because it may be read by worker threads in playback
   std::atomic<float> mGain{ 1.0f };
   //! Atomic because it may be read by worker threads in playback
   std::atomic<float> mPan{ 0.0f };

   int mRate{ 44100 };
   double mOrigin{ 0.0 };
   sampleFormat mFormat { floatSample };
};

static const ChannelGroup::Attachments::RegisteredFactory
waveTrackDataFactory{
   [](auto &) { return std::make_unique<WaveTrackData>(); } };

//! Copy can't be generated by default because of atomic members
WaveTrackData::WaveTrackData(const WaveTrackData &other) {
   SetGain(other.GetGain());
   SetPan(other.GetPan());
   mRate = other.mRate;
   mOrigin = other.mOrigin;
   mFormat = other.mFormat;
}

WaveTrackData::~WaveTrackData() = default;

std::unique_ptr<ClientData::Cloneable<>> WaveTrackData::Clone() const {
   return std::make_unique<WaveTrackData>(*this);
}

WaveTrackData &WaveTrackData::Get(WaveTrack &track) {
   return track.Attachments::Get<WaveTrackData>(waveTrackDataFactory);
}

const WaveTrackData &WaveTrackData::Get(const WaveTrack &track)
{
   return Get(const_cast<WaveTrack &>(track));
}

double WaveTrackData::GetOrigin() const
{
   return mOrigin;
}
void WaveTrackData::SetOrigin(double origin)
{
   mOrigin = origin;
}

sampleFormat WaveTrackData::GetSampleFormat() const
{
   return mFormat;
}

void WaveTrackData::SetSampleFormat(sampleFormat format)
{
   mFormat = format;
}

float WaveTrackData::GetGain() const
{
   return mGain.load(std::memory_order_relaxed);
}

void WaveTrackData::SetGain(float value)
{
   mGain.store(value, std::memory_order_relaxed);
}

float WaveTrackData::GetPan() const
{
   return mPan.load(std::memory_order_relaxed);
}

void WaveTrackData::SetPan(float value)
{
   mPan.store(value, std::memory_order_relaxed);
}

int WaveTrackData::GetRate() const
{
   return mRate;
}

void WaveTrackData::SetRate(int value)
{
   mRate = value;
}

namespace {
bool AreAligned(const WaveClipPointers& a, const WaveClipPointers& b)
{
   if (a.size() != b.size())
      return false;

   const auto compare = [](const WaveClip* a, const WaveClip* b) {
      // clips are aligned if both sequence start/end
      // points and play start/end points of the first clip match
      // the corresponding points of the other clip
      return a->GetPlayStartTime() == b->GetPlayStartTime() &&
         a->GetSequenceStartTime() == b->GetSequenceStartTime() &&
         a->GetPlayEndTime() == b->GetPlayEndTime() &&
         a->GetSequenceEndTime() == b->GetSequenceEndTime();
   };

   return std::mismatch(a.begin(), a.end(), b.begin(), compare).first == a.end();
}
}

//Handles possible future file values
Track::LinkType ToLinkType(int value)
{
   if (value < 0)
      return Track::LinkType::None;
   else if (value > 3)
      return Track::LinkType::Group;
   return static_cast<Track::LinkType>(value);
}

}

double WaveTrack::ProjectNyquistFrequency(const AudacityProject &project)
{
   auto &tracks = TrackList::Get(project);
   return std::max(ProjectRate::Get(project).GetRate(),
      tracks.Any<const WaveTrack>().max(&WaveTrack::GetRate))
      / 2.0;
}

static auto DefaultName = XO("Audio");

WaveChannel::WaveChannel(WaveTrack &owner)
   : mOwner{ owner }
{
}

WaveChannel::WaveChannel(WaveTrack &owner, WaveChannel &&other)
   : mOwner{ owner }
   , mClips{ move(other.mClips) }
{
}

WaveChannel &WaveChannel::operator =(WaveChannel &&other)
{
   mClips = move(other.mClips);
   return *this;
}

void WaveChannel::Swap(WaveChannel &other)
{
   mClips.swap(other.mClips);
}

WaveChannel::~WaveChannel() = default;

wxString WaveTrack::GetDefaultAudioTrackNamePreference()
{
   const auto name = AudioTrackNameSetting.ReadWithDefault(L"");

   if (name.empty() || ( name == DefaultName.MSGID() ))
      // When nothing was specified,
      // the default-default is whatever translation of...
      /* i18n-hint: The default name for an audio track. */
      return DefaultName.Translation();
   else
      return name;
}

static ProjectFileIORegistry::ObjectReaderEntry readerEntry{
   "wavetrack",
   WaveTrack::New
};

std::shared_ptr<WaveTrack> WaveTrackFactory::Create()
{
   return Create(QualitySettings::SampleFormatChoice(), mRate.GetRate());
}

std::shared_ptr<WaveTrack> WaveTrackFactory::DoCreate(size_t nChannels,
   sampleFormat format, double rate)
{
   auto result = std::make_shared<WaveTrack>(
      WaveTrack::CreateToken{}, mpFactory, format, rate);
   // Set the number of channels correctly before building all channel
   // attachments
   if (nChannels > 1)
      result->CreateRight();
   // Only after make_shared returns, can weak_from_this be used, which
   // attached object factories may need
   result->AttachedTrackObjects::BuildAll();
   return result;
}

std::shared_ptr<WaveTrack> WaveTrackFactory::Create(sampleFormat format, double rate)
{
   return DoCreate(1, format, rate);
}

WaveTrack::Holder WaveTrackFactory::Create(size_t nChannels)
{
   assert(nChannels > 0);
   assert(nChannels <= 2);
   return Create(nChannels, QualitySettings::SampleFormatChoice(), mRate.GetRate());
}

TrackListHolder WaveTrackFactory::CreateMany(size_t nChannels)
{
   return CreateMany(nChannels,
      QualitySettings::SampleFormatChoice(), mRate.GetRate());
}

void WaveTrack::MergeChannelAttachments(WaveTrack &&other)
{
   this->AttachedTrackObjects::ForCorresponding(other,
   [this](TrackAttachment *pLeft, TrackAttachment *pRight){
      // Precondition of callback from ClientData::Site
      assert(pLeft && pRight);
      const auto pLeftAttachments =
         dynamic_cast<ChannelAttachmentsBase *>(pLeft);
      const auto pRightAttachments =
         dynamic_cast<ChannelAttachmentsBase *>(pRight);
      // They should have come from the same factory of channel attachments
      assert((pLeftAttachments == nullptr) == (pRightAttachments == nullptr));
      if (pLeftAttachments) {
         // First fixup the back-pointers from channel views to their track
         pRightAttachments->Reparent(shared_from_this());
         // Then "steal" them
         pLeftAttachments->MakeStereo(shared_from_this(),
            std::move(*pRightAttachments));
      }
   });
}

//! Erase all attachments for a given index
void WaveTrack::EraseChannelAttachments(size_t ii)
{
   this->AttachedTrackObjects::ForEach(
   [this, ii](TrackAttachment &attachment){
      if (const auto pAttachments =
         dynamic_cast<ChannelAttachmentsBase *>(&attachment))
         pAttachments->Erase(shared_from_this(), ii);
   });
}

WaveTrack::Holder WaveTrackFactory::Create(size_t nChannels, sampleFormat format, double rate)
{
   assert(nChannels > 0);
   assert(nChannels <= 2);
   return CreateMany(nChannels, format, rate)->DetachFirst()
      ->SharedPointer<WaveTrack>();
}

TrackListHolder WaveTrackFactory::CreateMany(size_t nChannels, sampleFormat format, double rate)
{
   // There are some cases where more than two channels are requested
   if (nChannels == 2)
      return TrackList::Temporary(nullptr, DoCreate(nChannels, format, rate));
   auto result = TrackList::Temporary(nullptr);
   while (nChannels--)
      result->Add(DoCreate(1, format, rate));
   return result;
}

WaveTrack::Holder WaveTrackFactory::Create(size_t nChannels, const WaveTrack& proto)
{
   return proto.EmptyCopy(nChannels, mpFactory);
}

WaveTrack *WaveTrack::New( AudacityProject &project )
{
   auto &trackFactory = WaveTrackFactory::Get( project );
   auto &tracks = TrackList::Get( project );
   auto result = tracks.Add(trackFactory.Create());
   return result;
}

WaveTrack::WaveTrack(CreateToken&&, const SampleBlockFactoryPtr &pFactory,
   sampleFormat format, double rate )
   : mpFactory(pFactory)
   , mChannel(*this)
{
   WaveTrackData::Get(*this).SetSampleFormat(format);
   DoSetRate(static_cast<int>(rate));
}

auto WaveTrack::Create(
   const SampleBlockFactoryPtr &pFactory, sampleFormat format, double rate)
      -> Holder
{
   auto result =
      std::make_shared<WaveTrack>(CreateToken{}, pFactory, format, rate);
   // Only after make_shared returns, can weak_from_this be used, which
   // attached object factories may need
   // (but this is anyway just the factory for unit test purposes)
   result->AttachedTrackObjects::BuildAll();
   return result;
}

void WaveChannel::CopyClips(SampleBlockFactoryPtr pFactory,
   const WaveChannel &orig, bool backup)
{
   for (const auto &clip : orig.mClips)
      InsertClip(std::make_shared<WaveClip>(*clip, pFactory, true),
         false, backup, false);
}

size_t WaveTrack::GetWidth() const
{
   return 1;
}

size_t WaveChannel::NChannels() const
{
   return 1;
}

size_t WaveTrack::NChannels() const
{
   return mRightChannel.has_value() ? 2 : 1;
}

AudioGraph::ChannelType WaveChannel::GetChannelType() const
{
   if (GetTrack().Channels().size() == 1)
      return AudioGraph::MonoChannel;
   else if (GetChannelIndex() == 0)
      return AudioGraph::LeftChannel;
   else
      // TODO: more-than-two-channels
      return AudioGraph::RightChannel;
}

AudioGraph::ChannelType WaveTrack::GetChannelType() const
{
   // Not quite meaningful but preserving old behavior
   return (*Channels().begin())->WaveChannel::GetChannelType();
}

// Copy the track metadata but not the contents.
void WaveTrack::Init(const WaveTrack &orig)
{
   WritableSampleTrack::Init(orig);
   mpFactory = orig.mpFactory;
}

WaveTrack::~WaveTrack()
{
}

/*! @excsafety{No-fail} */
void WaveTrack::MoveTo(double origin)
{
   double delta = origin - GetStartTime();
   for (const auto &pInterval : Intervals())
      // assume No-fail-guarantee
      pInterval->ShiftBy(delta);
   WaveTrackData::Get(*this).SetOrigin(origin);
}

auto WaveTrack::DuplicateWithOtherTempo(double newTempo) const -> Holder
{
   const auto srcCopy = Duplicate();
   ::DoProjectTempoChange(*srcCopy, newTempo);
   return std::static_pointer_cast<WaveTrack>(srcCopy);
}

bool WaveTrack::LinkConsistencyFix(const bool doFix)
{
   // This implies satisfaction of the precondition of SetRate()
   assert(!doFix || IsLeader());

   const auto removeZeroClips = [](WaveClipHolders& clips) {
      // Check for zero-length clips and remove them
      for (auto it = clips.begin(); it != clips.end();)
      {
         if ((*it)->IsEmpty())
            it = clips.erase(it);
         else
            ++it;
      }
   };

   auto err = !WritableSampleTrack::LinkConsistencyFix(doFix);

   auto linkType = GetLinkType();
   if (linkType != LinkType::None) {
      auto next = *TrackList::Channels(this).first.advance(1);
      if (next == nullptr) {
         //next track is absent or not a wave track, fix and report error
         if (doFix) {
            wxLogWarning(L"Right track %s is expected to be a WaveTrack."
               "\n Removing link from left wave track %s.",
               next->GetName(), GetName());
            SetLinkType(LinkType::None);
         }
         err = true;
      }
      else if (doFix) {
         // non-error upgrades happen here
         if (!AreAligned(SortedClipArray(), next->SortedClipArray()) ||
             !RateConsistencyCheck() || !FormatConsistencyCheck())
         {
            SetLinkType(linkType = LinkType::None);
         }
         else
         {
            SetLinkType(linkType = LinkType::Aligned);
            //clean up zero clips only after alignment check has completed
            //this can't break alignment as there should be a "twin"
            //in the right channel which will also be removed, otherwise
            //track will be unlinked because AreAligned returned false
            removeZeroClips(NarrowClips());
            removeZeroClips(next->NarrowClips());
         }
      }
   }
   if (doFix) {
      // More non-error upgrading
      // Set the common channel group rate from the unzipped leader's rate
      if (mLegacyRate > 0)
      {
         WaveTrack *next{};
         if (linkType != LinkType::None)
            next = *TrackList::Channels(this).first.advance(1);
         SetRate(mLegacyRate);
         mLegacyRate = 0;
         if (next)
            next->mLegacyRate = 0;
         if (mLegacyFormat != undefinedSample)
            WaveTrackData::Get(*this).SetSampleFormat(mLegacyFormat);
         if (next && next->mLegacyFormat != undefinedSample)
            WaveTrackData::Get(*next).SetSampleFormat(mLegacyFormat);
      }
      if (linkType == LinkType::None)
         // Did not visit the other call to removeZeroClips, do it now
         removeZeroClips(NarrowClips());
      else
         // Make a real wide wave track from two deserialized narrow tracks
         ZipClips();
   }
   return !err;
}

static const Track::TypeInfo &typeInfo()
{
   static const Track::TypeInfo info{
      { "wave", "wave", XO("Wave Track") },
      true, &WritableSampleTrack::ClassTypeInfo() };
   return info;
}

auto WaveTrack::GetTypeInfo() const -> const TypeInfo &
{
   return typeInfo();
}

auto WaveTrack::ClassTypeInfo() -> const TypeInfo &
{
   return typeInfo();
}

Track::Holder WaveTrack::PasteInto(
   AudacityProject &project, TrackList &list) const
{
   auto &trackFactory = WaveTrackFactory::Get(project);
   auto &pSampleBlockFactory = trackFactory.GetSampleBlockFactory();
   auto pFirstTrack = WideEmptyCopy(pSampleBlockFactory);
   list.Add(pFirstTrack->SharedPointer());
   pFirstTrack->Paste(0.0, *this);
   return pFirstTrack->SharedPointer();
}

size_t WaveTrack::NIntervals() const
{
   return NarrowClips().size();
}

auto WaveTrack::GetWideClip(size_t iInterval) -> IntervalHolder
{
   return std::static_pointer_cast<Interval>(DoGetInterval(iInterval));
}

auto WaveTrack::GetWideClip(size_t iInterval) const -> IntervalConstHolder
{
   return const_cast<WaveTrack&>(*this).GetWideClip(iInterval);
}

std::shared_ptr<WideChannelGroupInterval>
WaveTrack::DoGetInterval(size_t iInterval)
{
   if (iInterval < NIntervals()) {
      WaveClipHolder pClip = NarrowClips()[iInterval],
         pClip1;
      if (NChannels() > 1) {
         auto &rightClips = RightClips();
         if (iInterval < rightClips.size())
            pClip1 = rightClips[iInterval];
      }
      return std::make_shared<Interval>(*this, pClip, pClip1);
   }
   return {};
}

bool WaveTrack::HasClipNamed(const wxString& name) const
{
   auto clips = Intervals();
   return std::any_of(clips.begin(), clips.end(),
      [&](const auto &pClip){ return pClip->GetName() == name; });
}

std::shared_ptr<::Channel> WaveTrack::DoGetChannel(size_t iChannel)
{
   auto nChannels = NChannels();
   if (iChannel >= nChannels)
      return {};
   // TODO: more-than-two-channels
   ::Channel &aliased = (iChannel == 0)
      ? mChannel
      : *mRightChannel;
   // Use aliasing constructor of std::shared_ptr
   return { shared_from_this(), &aliased };
}

ChannelGroup &WaveChannel::DoGetChannelGroup() const
{
   return mOwner;
}

WaveClipHolders &WaveTrack::NarrowClips()
{
   return mChannel.Clips();
}

const WaveClipHolders &WaveTrack::NarrowClips() const
{
   return mChannel.Clips();
}

WaveClipHolders &WaveTrack::RightClips()
{
   return mRightChannel->Clips();
}

const WaveClipHolders &WaveTrack::RightClips() const
{
   return mRightChannel->Clips();
}

Track::Holder WaveTrack::Clone(bool backup) const
{
   auto newTrack = EmptyCopy(NChannels());
   newTrack->mChannel.CopyClips(newTrack->mpFactory, this->mChannel, backup);
   if (mRightChannel) {
      assert(newTrack->mRightChannel.has_value());
      newTrack->mRightChannel
         ->CopyClips(newTrack->mpFactory, *this->mRightChannel, backup);
   }
   return newTrack;
}

wxString WaveTrack::MakeClipCopyName(const wxString& originalName) const
{
   auto name = originalName;
   for (auto i = 1;; ++i)
   {
      if (!HasClipNamed(name))
         return name;
      //i18n-hint Template for clip name generation on copy-paste
      name = XC("%s.%i", "clip name template").Format(originalName, i).Translation();
   }
}

wxString WaveTrack::MakeNewClipName() const
{
   auto name = GetName();
   for (auto i = 1;; ++i)
   {
      if (!HasClipNamed(name))
         return name;
      //i18n-hint Template for clip name generation on inserting new empty clip
      name = XC("%s %i", "clip name template").Format(GetName(), i).Translation();
   }
}

double WaveChannel::GetRate() const
{
   return GetTrack().GetRate();
}

double WaveTrack::GetRate() const
{
   return WaveTrackData::Get(*this).GetRate();
}

void WaveTrack::SetRate(double newRate)
{
   assert(newRate > 0);
   newRate = std::max( 1.0, newRate );
   DoSetRate(newRate);

   for (const auto &clip : Intervals())
      clip->SetRate(newRate);
}

void WaveTrack::DoSetRate(double newRate)
{
   auto &data = WaveTrackData::Get(*this);
   data.SetRate(static_cast<int>(newRate));
}

float WaveTrack::GetGain() const
{
   return WaveTrackData::Get(*this).GetGain();
}

void WaveTrack::DoSetGain(float value)
{
   WaveTrackData::Get(*this).SetGain(value);
}

void WaveTrack::SetGain(float newGain)
{
   if (GetGain() != newGain) {
      DoSetGain(newGain);
      Notify(true);
   }
}

float WaveTrack::GetPan() const
{
   return WaveTrackData::Get(*this).GetPan();
}

void WaveTrack::DoSetPan(float value)
{
   WaveTrackData::Get(*this).SetPan(value);
}

void WaveTrack::SetPan(float newPan)
{
   if (newPan > 1.0)
      newPan = 1.0;
   else if (newPan < -1.0)
      newPan = -1.0;

   if ( GetPan() != newPan ) {
      DoSetPan(newPan);
      Notify(true);
   }
}

float WaveChannel::GetChannelGain(int channel) const
{
   return GetTrack().GetChannelGain(channel);
}

float WaveTrack::GetChannelGain(int channel) const
{
   // channel is not necessarily less than the channel group width but
   // a mono track might pan differently according to that
   float left = 1.0;
   float right = 1.0;

   const auto pan = GetPan();
   if (pan < 0)
      right = (pan + 1.0);
   else if (pan > 0)
      left = 1.0 - pan;

   const auto gain = GetGain();
   if ((channel % 2) == 0)
      return left * gain;
   else
      return right * gain;
}

sampleCount WaveTrack::GetVisibleSampleCount() const
{
    sampleCount result{ 0 };

    for (const auto& clip : Intervals())
        result += clip->GetVisibleSampleCount();

    return result;
}

sampleFormat WaveTrack::GetSampleFormat() const
{
   return WaveTrackData::Get(*this).GetSampleFormat();
}

/*! @excsafety{Weak} -- Might complete on only some clips */
void WaveTrack::ConvertToSampleFormat(sampleFormat format,
   const std::function<void(size_t)> & progressReport)
{
   for (const auto &pClip : Intervals())
      pClip->ConvertToSampleFormat(format, progressReport);
   WaveTrackData::Get(*this).SetSampleFormat(format);
}


bool WaveTrack::IsEmpty(double t0, double t1) const
{
   if (t0 > t1)
      return true;

   //wxPrintf("Searching for overlap in %.6f...%.6f\n", t0, t1);
   for (const auto &clip : Intervals())
   {
      if (clip->IntersectsPlayRegion(t0, t1)) {
         //wxPrintf("Overlapping clip: %.6f...%.6f\n",
         //       clip->GetStartTime(),
         //       clip->GetEndTime());
         // We found a clip that overlaps this region
         return false;
      }
   }
   //wxPrintf("No overlap found\n");

   // Otherwise, no clips overlap this region
   return true;
}

Track::Holder WaveTrack::Cut(double t0, double t1)
{
   if (t1 < t0)
      THROW_INCONSISTENCY_EXCEPTION;

   auto result = Copy(t0, t1);
   Clear(t0, t1);
   return result;
}

/*! @excsafety{Strong} */
auto WaveTrack::SplitCut(double t0, double t1) -> Holder
{
   if (t1 < t0)
      THROW_INCONSISTENCY_EXCEPTION;

   // SplitCut is the same as 'Copy', then 'SplitDelete'
   auto result = Copy(t0, t1);
   SplitDelete(t0, t1);
   return std::static_pointer_cast<WaveTrack>(result);
}

//Trim trims within a clip, rather than trimming everything.
//If a bound is outside a clip, it trims everything.
/*! @excsafety{Weak} */
void WaveTrack::Trim(double t0, double t1)
{
   bool inside0 = false;
   bool inside1 = false;

   for (const auto &clip : Intervals()) {
      if (t1 > clip->GetPlayStartTime() && t1 < clip->GetPlayEndTime()) {
         clip->SetTrimRight(
            clip->GetTrimRight() + clip->GetPlayEndTime() - t1);
         inside1 = true;
      }

      if (t0 > clip->GetPlayStartTime() && t0 < clip->GetPlayEndTime()) {
         clip->SetTrimLeft(
            clip->GetTrimLeft() + t0 - clip->GetPlayStartTime());
         inside0 = true;
      }
   }

   //if inside0 is false, then the left selector was between
   //clips, so DELETE everything to its left.
   if (const auto endTime = GetEndTime()
      ; !inside1 && t1 < endTime
   )
      Clear(t1, endTime);

   if (const auto startTime = GetStartTime()
      ; !inside0 && t0 > startTime
   )
      SplitDelete(startTime, t0);
}

WaveTrack::Holder WaveTrack::EmptyCopy(size_t nChannels,
   const SampleBlockFactoryPtr &pFactory) const
{
   const auto rate = GetRate();
   auto result = std::make_shared<WaveTrack>(CreateToken{},
      pFactory, GetSampleFormat(), rate);
   if (nChannels > 1)
      result->CreateRight();
   result->Init(*this);
   // Copy state rather than BuildAll()
   Track::CopyAttachments(*result, *this, true /* deep copy */);
   // The previous line might have destroyed the rate information stored in
   // channel group data.  The copy is not yet in a TrackList.  Reassign rate
   // in case the track needs to make WaveClips before it is properly joined
   // with the opposite channel in a TrackList.
   // TODO wide wave tracks -- all of the comment above will be irrelevant!
   result->DoSetRate(rate);
   result->mpFactory = pFactory ? pFactory : mpFactory;
   WaveTrackData::Get(*result).SetOrigin(0);
   return result;
}

auto WaveTrack::WideEmptyCopy(
   const SampleBlockFactoryPtr &pFactory) const -> Holder
{
   return EmptyCopy(NChannels(), pFactory);
}

void WaveTrack::MakeMono()
{
   mRightChannel.reset();
   EraseChannelAttachments(1);
}

auto WaveTrack::MonoToStereo() -> Holder
{
   assert(!GetOwner());
   mRightChannel.reset();
   EraseChannelAttachments(1);

   // Make temporary new mono track
   auto newTrack = Duplicate();

   // Make a list
   auto result = TrackList::Temporary(nullptr, shared_from_this());
   result->Add(newTrack);
   // Destroy the temporary track, widening this track to stereo
   ZipClips();

   return std::static_pointer_cast<WaveTrack>(result->DetachFirst());
}

auto WaveTrack::SplitChannels() -> std::vector<Holder>
{
   std::vector<Holder> result{ SharedPointer<WaveTrack>() };
   if (NChannels() > 1) {
      auto pOwner = GetOwner();
      assert(pOwner); // pre
      CopyClipEnvelopes();
      auto pNewTrack = result.emplace_back(EmptyCopy(1));
      pNewTrack->mChannel = std::move(*this->mRightChannel);
      this->mRightChannel.reset();
      auto iter = pOwner->Find(this);
      pOwner->Insert(*++iter, pNewTrack);
      // Fix up the channel attachments to avoid waste of space
      result[0]->EraseChannelAttachments(1);
      result[1]->EraseChannelAttachments(0);
   }
   return result;
}

void WaveTrack::SwapChannels()
{
   assert(NChannels() == 2);
   CopyClipEnvelopes();
   mChannel.Swap(*mRightChannel);
   this->AttachedTrackObjects::ForEach([this](TrackAttachment &attachment){
      if (const auto pAttachments =
         dynamic_cast<ChannelAttachmentsBase *>(&attachment)) {
         pAttachments->SwapChannels(shared_from_this());
      }
   });
}

Track::Holder WaveTrack::Copy(double t0, double t1, bool forClipboard) const
{
   if (t1 < t0)
      THROW_INCONSISTENCY_EXCEPTION;

   auto newTrack = EmptyCopy(NChannels());
   const auto endTime = std::max(GetEndTime(), t1);
   WaveChannel::CopyOne(newTrack->mChannel, this->mChannel,
      t0, t1, endTime, forClipboard);
   if (this->mRightChannel) {
      assert(newTrack->mRightChannel);
      WaveChannel::CopyOne(*newTrack->mRightChannel, *this->mRightChannel,
         t0, t1, endTime, forClipboard);
   }
   return newTrack;
}

void WaveChannel::CopyOne(WaveChannel &newChannel,
   const WaveChannel &channel, double t0, double t1, double endTime,
   bool forClipboard)
{
   const auto &pFactory = channel.GetTrack().GetSampleBlockFactory();

   // PRL:  Why shouldn't cutlines be copied and pasted too?  I don't know,
   // but that was the old behavior.  But this function is also used by the
   // Duplicate command and I changed its behavior in that case.

   auto &clips = channel.Clips();
   auto &newClips = newChannel.Clips();
   for (const auto &clip : clips) {
      if(clip->IsEmpty())
         continue;

      if (t0 <= clip->GetPlayStartTime() && t1 >= clip->GetPlayEndTime()) {
         // Whole clip is in copy region
         //wxPrintf("copy: clip %i is in copy region\n", (int)clip);

         newChannel.InsertClip(
            std::make_shared<WaveClip>(*clip, pFactory, !forClipboard),
            false, false, false);
         WaveClip *const newClip = newClips.back().get();
         newClip->ShiftBy(-t0);
      }
      else if (clip->CountSamples(t0, t1) >= 1) {
         // Clip is affected by command
         //wxPrintf("copy: clip %i is affected by command\n", (int)clip);

         auto newClip = std::make_shared<WaveClip>(
            *clip, pFactory, !forClipboard, t0, t1);
         newClip->SetName(clip->GetName());

         newClip->ShiftBy(-t0);
         if (newClip->GetPlayStartTime() < 0)
            newClip->SetPlayStartTime(0);

         newChannel.InsertClip(std::move(newClip), false, false, false);
      }
   }

   // AWD, Oct 2009: If the selection ends in whitespace, create a
   // placeholder clip representing that whitespace
   // PRL:  Only if we want the track for pasting into other tracks.  Not if
   // it goes directly into a project as in the Duplicate command.
   if (forClipboard && endTime + 1.0 / newChannel.GetRate() < t1 - t0) {
      // TODO wide wave tracks -- match clip width of newTrack
      auto placeholder = std::make_shared<WaveClip>(1, pFactory,
         newChannel.GetSampleFormat(),
         static_cast<int>(newChannel.GetRate()));
      placeholder->SetIsPlaceholder(true);
      placeholder->InsertSilence(0, (t1 - t0) - newChannel.GetEndTime());
      placeholder->ShiftBy(newChannel.GetEndTime());
      newChannel.InsertClip(std::move(placeholder), true, false, false);
   }
}

/*! @excsafety{Strong} */
void WaveTrack::Clear(double t0, double t1)
{
   HandleClear(t0, t1, false, false);
}

/*! @excsafety{Strong} */
void WaveTrack::ClearAndAddCutLine(double t0, double t1)
{
   HandleClear(t0, t1, true, false);
}

namespace {

   //Internal structure, which is supposed to contain
   //data related to clip boundaries, and used during
   //ClearAndPaste to restore old splits after new
   //data is pasted
   struct SplitInfo
   {
      //Time, where boundary is located
      double time;
      //Contains trimmed data, which should be re-appended to
      //the clip to the left from the boundary, may be null
      WaveTrack::IntervalHolder left;
      //Contains trimmed data, which should be re-appended to
      //the clip to the right from the boundary, may be null
      WaveTrack::IntervalHolder right;
      //Contains clip name next to the left from the boundary,
      //if present, that needs to be re-assigned to the matching
      //clip after split
      std::optional<wxString> leftClipName;
      //Contains clip name next to the right from the boundary,
      //if present, that needs to be re-assigned to the matching
      //clip after split
      std::optional<wxString> rightClipName;
   };

}

//
// ClearAndPaste() is a specialized version of HandleClear()
// followed by Paste() and is used mostly by effects that
// can't replace track data directly using Get()/Set().
//
// HandleClear() removes any cut/split lines with the
// cleared range, but, in most cases, effects want to preserve
// the existing cut/split lines, trimmed data and clip names,
// so they are saved before the HandleClear()/Paste() and restored after.
// When pasted track has split lines with hidden data at same places
// as the target one, then only targets hidden data is preserved, and
// hidden data from pasted track is discarded.
//
// If the pasted track overlaps two or more clips, then it will
// be pasted with visible split lines.  Normally, effects do not
// want these extra lines, so they may be merged out.
//
/*! @excsafety{Weak} -- This WaveTrack remains destructible in case of AudacityException.
But some of its cutline clips may have been destroyed. */
void WaveTrack::ClearAndPaste(
   double t0,                      // Start of time to clear
   double t1,                      // End of time to clear
   const WaveTrack& src,           // What to paste
   bool preserve,                  // Whether to reinsert splits/cuts
   bool merge,                     // Whether to remove 'extra' splits
   const TimeWarper* effectWarper, // How does time change
   bool clearByTrimming)
{
   // Get a modifiable copy of `src` because it may come from another project
   // with different tempo, making boundary queries incorrect.
   const auto& tempo = GetProjectTempo(*this);
   if (!tempo.has_value())
      THROW_INCONSISTENCY_EXCEPTION;
   const auto copyHolder = src.DuplicateWithOtherTempo(*tempo);
   ClearAndPasteAtSameTempo(
      t0, t1, *copyHolder, preserve, merge, effectWarper, clearByTrimming);
}

void WaveTrack::ClearAndPasteAtSameTempo(
   double t0, double t1, const WaveTrack& src, bool preserve, bool merge,
   const TimeWarper* effectWarper, bool clearByTrimming)
{
   const auto srcNChannels = src.NChannels();
   assert(srcNChannels == NChannels());
   assert(
      GetProjectTempo(*this).has_value() &&
      GetProjectTempo(*this) == GetProjectTempo(src));

   t0 = SnapToSample(t0);
   t1 = SnapToSample(t1);

   const auto endTime = src.GetEndTime();
   double dur = std::min(t1 - t0, endTime);

   // If duration is 0, then it's just a plain paste
   if (dur == 0.0) {
      // use Weak-guarantee
      PasteWaveTrack(t0, src, merge);
      return;
   }

   auto &track = *this;

   std::vector<SplitInfo> splits;
   IntervalHolders cuts;

   //helper routine, that finds SplitInfo by time value,
   //or creates a new one if no one exists yet
   auto get_split = [&](double time) {
      auto it = std::find_if(splits.begin(), splits.end(),
         [time](const SplitInfo& split) { return split.time == time; });
      if(it == splits.end())
         it = splits.insert(
            splits.end(),
            { time, nullptr, nullptr, std::nullopt, std::nullopt }
         );
      return it;
   };

   // If provided time warper was NULL, use a default one that does nothing
   IdentityTimeWarper localWarper;
   const TimeWarper *warper = (effectWarper ? effectWarper : &localWarper);

   const auto roundTime = [&track](double t){
      return track.SnapToSample(t);
   };

   // Align to a sample
   t0 = roundTime(t0);
   t1 = roundTime(t1);

   // Save the cut/split lines whether preserving or not since merging
   // needs to know if a clip boundary is being crossed since Paste()
   // will add split lines around the pasted clip if so.
   for (const auto &&clip : track.Intervals()) {
      double st;

      // Remember clip boundaries as locations to split
      // we need to copy clips, trims and names, because the original ones
      // could be changed later during Clear/Paste routines
      st = roundTime(clip->GetPlayStartTime());
      if (st >= t0 && st <= t1) {
         auto it = get_split(st);
         if (clip->GetTrimLeft() != 0) {
            //keep only hidden left part
            it->right = track.CopyClip(*clip, false);
            it->right->SetTrimLeft(.0);
            it->right->ClearRight(clip->GetPlayStartTime());
         }
         it->rightClipName = clip->GetName();
      }

      st = roundTime(clip->GetPlayEndTime());
      if (st >= t0 && st <= t1) {
         auto it = get_split(st);
         if (clip->GetTrimRight() != 0) {
            //keep only hidden right part
            it->left = track.CopyClip(*clip, false);
            it->left->SetTrimRight(.0);
            it->left->ClearLeft(clip->GetPlayEndTime());
         }
         it->leftClipName = clip->GetName();
      }

      // Search for cut lines
      auto cutlines = clip->GetCutLines(track);
      for (auto &cut : cutlines) {
         const auto unrounded =
            clip->GetSequenceStartTime() + cut->GetSequenceStartTime();
         const double cs = roundTime(unrounded);

         // Remember cut point
         if (cs >= t0 && cs <= t1) {
            // Remember the absolute offset and add to our cuts array.
            cut->SetSequenceStartTime(cs);
            bool removed = clip->RemoveCutLine(unrounded);
            assert(removed);
            cuts.push_back(move(cut));
         }
      }
   }

   const auto tolerance = 2.0 / track.GetRate();

   // This is not a split-cut operation.
   constexpr auto split = false;

   // Now, clear the selection
   track.HandleClear(t0, t1, false, split, clearByTrimming);

   // And paste in the new data
   track.PasteWaveTrackAtSameTempo(t0, src, merge);

   // First, merge the new clip(s) in with the existing clips
   if (merge && splits.size() > 0) {
      {
         // Now t1 represents the absolute end of the pasted data.
         t1 = t0 + endTime;

         // Get a sorted array of the clips
         auto clips = track.SortedIntervalArray();

         // Scan the sorted clips for the first clip whose start time
         // exceeds the pasted regions end time.
         IntervalHolder prev;
         for (const auto clip : clips) {
            // Merge this clip and the previous clip if the end time
            // falls within it and this isn't the first clip in the track.
            if (fabs(t1 - clip->GetPlayStartTime()) < tolerance) {
               if (prev && clip->HasEqualPitchAndSpeed(*prev))
                  track.MergeClips(
                     track.GetClipIndex(*prev), track.GetClipIndex(*clip));
               break;
            }
            prev = clip;
         }
      }

      {
         // Refill the array since clips have changed.
         auto clips = track.SortedIntervalArray();

         // Scan the sorted clips to look for the start of the pasted
         // region.
         IntervalHolder prev;
         for (const auto clip : clips) {
            if (prev) {
               // It must be that clip is what was pasted and it begins where
               // prev ends.
               // use Weak-guarantee
               if (clip->HasEqualPitchAndSpeed(*prev))
                  track.MergeClips(
                     track.GetClipIndex(*prev), track.GetClipIndex(*clip));
               break;
            }
            if (fabs(t0 - clip->GetPlayEndTime()) < tolerance)
               // Merge this clip and the next clip if the start time
               // falls within it and this isn't the last clip in the track.
               prev = clip;
            else
               prev = nullptr;
         }
      }
   }

   // Restore cut/split lines
   if (preserve) {
      auto attachLeft = [](Interval& target, Interval& src) {
         // What this lambda does is restoring the left hidden data of `target`
         // that was cleared by `HandleClear`. Hence, `target` has no left
         // hidden data at this stage.
         assert(target.GetTrimLeft() == 0);
         if (target.GetTrimLeft() != 0)
            return;

         // `src` was created by copy from `target`, so they have equal width,
         // pitch and speed.
         assert(target.NChannels() == src.NChannels());
         assert(target.HasEqualPitchAndSpeed(src));

         auto trim = src.GetPlayEndTime() - src.GetPlayStartTime();
         auto success = target.Paste(target.GetPlayStartTime(), src);
         assert(success); // because of precondition above
         target.SetTrimLeft(trim);
         //Play start time needs to be adjusted after
         //prepending data to the sequence
         target.ShiftBy(-trim);
      };

      auto attachRight = [](Interval &target, Interval &src)
      {
         // See `attachLeft` for rationale behind these asserts.
         assert(target.GetTrimRight() == 0);
         if (target.GetTrimRight() != 0)
            return;
         assert(target.NChannels() == src.NChannels());
         assert(target.HasEqualPitchAndSpeed(src));

         auto trim = src.GetPlayEndTime() - src.GetPlayStartTime();
         auto success = target.Paste(target.GetPlayEndTime(), src);
         assert(success); // because of precondition above
         target.SetTrimRight(trim);
      };

      // Restore the split lines and trims, transforming the position appropriately
      for (const auto& split: splits) {
         auto at = roundTime(warper->Warp(split.time));
         for (const auto &&clip : track.Intervals()) {
            // Clips in split began as copies of a clip in the track,
            // therefore have the same width, satisfying preconditions to
            // attach
            if (clip->SplitsPlayRegion(at))//strictly inside
            {
               auto newClip = CopyClip(*clip, true);
               clip->ClearRight(at);
               newClip->ClearLeft(at);
               if (split.left)
                  // clip was cleared right
                  attachRight(*clip, *split.left);
               if (split.right)
                  // new clip was cleared left
                  attachLeft(*newClip, *split.right);
               track.InsertInterval(move(newClip), false);
               break;
            }
            else if (clip->GetPlayStartSample() ==
               track.TimeToLongSamples(at) && split.right) {
               // Satisfy the precondition of attachLeft first!
               const auto trim = clip->GetTrimLeft();
               const auto seqStartTime = clip->GetSequenceStartTime();
               clip->Clear(seqStartTime, seqStartTime + trim);
               // This clearing, although only removing the hidden part, moved
               // the clip leftwards. We don't want this in this case.
               clip->ShiftBy(trim);
               attachLeft(*clip, *split.right);
               break;
            }
            else if (clip->GetPlayEndSample() ==
               track.TimeToLongSamples(at) && split.left) {
               // Satisfy the precondition of attachRight first!
               clip->Clear(
                  clip->GetPlayEndTime(), clip->GetSequenceEndTime());
               attachRight(*clip, *split.left);
               break;
            }
         }
      }

      //Restore clip names
      for (const auto& split : splits)
      {
         auto s = track.TimeToLongSamples(warper->Warp(split.time));
         for (auto &&clip : track.Intervals()) {
            if (split.rightClipName.has_value() && clip->GetPlayStartSample() == s)
               clip->SetName(*split.rightClipName);
            else if (split.leftClipName.has_value() && clip->GetPlayEndSample() == s)
               clip->SetName(*split.leftClipName);
         }
      }

      // Restore the saved cut lines, also transforming if time altered
      for (const auto &&clip : track.Intervals()) {
         const double st = clip->GetPlayStartTime();
         const double et = clip->GetPlayEndTime();

         // Scan the cuts for any that live within this clip
         for (auto &cut : cuts) {
            if (!cut)
               continue;

            //cutlines in this array were orphaned previously
            double cs = cut->GetSequenceStartTime();

            // Offset the cut from the start of the clip and add it to
            // this clips cutlines.
            if (cs >= st && cs <= et) {
               cut->SetSequenceStartTime(warper->Warp(cs) - st);
               clip->AddCutLine(*cut);
               cut = {};
            }
         }
      }
   }
}

/*! @excsafety{Strong} */
void WaveTrack::SplitDelete(double t0, double t1)
{
   constexpr bool addCutLines = false;
   constexpr bool split = true;
   HandleClear(t0, t1, addCutLines, split);
}

namespace
{
   WaveClipHolders::const_iterator
      FindClip(const WaveClipHolders &list, const WaveClip *clip, int *distance = nullptr)
   {
      if (distance)
         *distance = 0;
      auto it = list.begin();
      for (const auto end = list.end(); it != end; ++it)
      {
         if (it->get() == clip)
            break;
         if (distance)
            ++*distance;
      }
      return it;
   }

   WaveClipHolders::iterator
      FindClip(WaveClipHolders &list, const WaveClip *clip, int *distance = nullptr)
   {
      if (distance)
         *distance = 0;
      auto it = list.begin();
      for (const auto end = list.end(); it != end; ++it)
      {
         if (it->get() == clip)
            break;
         if (distance)
            ++*distance;
      }
      return it;
   }
}

std::ptrdiff_t WaveTrack::FindWideClip(const Interval &clip)
{
   auto clips = Intervals();
   const auto begin = clips.begin();
   const auto pNarrowClip = clip.GetClip(0).get();
   const auto pred = [pNarrowClip](auto pClip){
      return pClip->GetClip(0).get() == pNarrowClip; };
   // Don't use pointer identity of intervals yet TODO wide wave clips
   auto iter = std::find_if(begin, clips.end(), pred);
   return std::distance(begin, iter);
}

void WaveTrack::RemoveWideClip(std::ptrdiff_t distance)
{
   auto &clips = NarrowClips();
   if (distance < clips.size())
      clips.erase(clips.begin() + distance);
   if (NChannels() > 1) {
      auto &rightClips = RightClips();
      if (distance < rightClips.size())
         rightClips.erase(rightClips.begin() + distance);
   }
}

/*! @excsafety{Strong} */
void WaveTrack::HandleClear(double t0, double t1, bool addCutLines,
   const bool split, const bool clearByTrimming)
{
   // For debugging, use an ASSERT so that we stop
   // closer to the problem.
   wxASSERT( t1 >= t0 );
   if (t1 < t0)
      THROW_INCONSISTENCY_EXCEPTION;

   t0 = SnapToSample(t0);
   t1 = SnapToSample(t1);

   IntervalHolders clipsToDelete;
   IntervalHolders clipsToAdd;

   // We only add cut lines when deleting in the middle of a single clip
   // The cut line code is not really prepared to handle other situations
   if (addCutLines)
      for (const auto &clip : Intervals())
         if (clip->PartlyWithinPlayRegion(t0, t1)) {
            addCutLines = false;
            break;
         }

   for (const auto &clip : Intervals()) {
      if (clip->CoversEntirePlayRegion(t0, t1))
         // Whole clip must be deleted - remember this
         clipsToDelete.push_back(clip);
      else if (clip->IntersectsPlayRegion(t0, t1)) {
         // Clip data is affected by command
         if (addCutLines) {
            // Don't modify this clip in place, because we want a strong
            // guarantee, and might modify another clip
            clipsToDelete.push_back(clip);
            auto newClip = CopyClip(*clip, true);
            newClip->ClearAndAddCutLine(t0, t1);
            clipsToAdd.push_back(move(newClip));
         }
         else {
            if (split || clearByTrimming) {
               // Three cases:

               if (clip->BeforePlayRegion(t0)) {
                  // Delete from the left edge

                  // Don't modify this clip in place, because we want a strong
                  // guarantee, and might modify another clip
                  clipsToDelete.push_back(clip);
                  auto newClip = CopyClip(*clip, true);
                  newClip->TrimLeft(t1 - clip->GetPlayStartTime());
                  if (!split)
                     // If this is not a split-cut, where things are left in
                     // place, we need to reposition the clip.
                     newClip->ShiftBy(t0 - t1);
                  clipsToAdd.push_back(move(newClip));
               }
               else if (clip->AfterPlayRegion(t1)) {
                  // Delete to right edge

                  // Don't modify this clip in place, because we want a strong
                  // guarantee, and might modify another clip
                  clipsToDelete.push_back(clip);
                  auto newClip = CopyClip(*clip, true);
                  newClip->TrimRight(clip->GetPlayEndTime() - t0);

                  clipsToAdd.push_back(move(newClip));
               }
               else {
                  // Delete in the middle of the clip...we actually create two
                  // NEW clips out of the left and right halves...

                  auto leftClip = CopyClip(*clip, true);
                  leftClip->TrimRight(clip->GetPlayEndTime() - t0);
                  clipsToAdd.push_back(move(leftClip));

                  auto rightClip = CopyClip(*clip, true);
                  rightClip->TrimLeft(t1 - clip->GetPlayStartTime());
                  if (!split)
                     // If this is not a split-cut, where things are left in
                     // place, we need to reposition the clip.
                     rightClip->ShiftBy(t0 - t1);
                  clipsToAdd.push_back(move(rightClip));

                  clipsToDelete.push_back(clip);
               }
            }
            else {
               // (We are not doing a split cut)

               // Don't modify this clip in place, because we want a strong
               // guarantee, and might modify another clip
               clipsToDelete.push_back(clip);
               auto newClip = CopyClip(*clip, true);

               // clip->Clear keeps points < t0 and >= t1 via Envelope::CollapseRegion
               newClip->Clear(t0,t1);

               clipsToAdd.push_back(move(newClip));
            }
         }
      }
   }

   // Only now, change the contents of this track
   // use No-fail-guarantee for the rest

   for (const auto &clip: clipsToDelete)
      RemoveInterval(clip);

   const auto moveClipsLeft = !split && GetEditClipsCanMove();
   if (moveClipsLeft)
      // Clip is "behind" the region -- offset it unless we're splitting
      // or we're using the "don't move other clips" mode
      for (const auto &clip : Intervals())
         if (clip->AtOrBeforePlayRegion(t1))
            clip->ShiftBy(-(t1 - t0));

   for (auto &clip: clipsToAdd)
      InsertInterval(move(clip), false);
}

void WaveTrack::SyncLockAdjust(double oldT1, double newT1)
{
   const auto endTime = GetEndTime();
   if (newT1 > oldT1 &&
      // JKC: This is a rare case where using >= rather than > on a float matters.
      // GetEndTime() looks through the clips and may give us EXACTLY the same
      // value as T1, when T1 was set to be at the end of one of those clips.
      oldT1 >= endTime)
         return;
   if (newT1 > oldT1) {
      // Insert space within the track

      // If track is empty at oldT1 insert whitespace; otherwise, silence
      if (IsEmpty(oldT1, oldT1)) {
         // Check if clips can move
         if (EditClipsCanMove.Read()) {
            const auto offset = newT1 - oldT1;
            const auto rate = GetRate();
            for (const auto &clip : Intervals())
               if (clip->GetPlayStartTime() > oldT1 - (1.0 / rate))
                  clip->ShiftBy(offset);
         }
         return;
      }
      else {
         // AWD: Could just use InsertSilence() on its own here, but it doesn't
         // follow EditClipCanMove rules (Paste() does it right)
         const auto duration = newT1 - oldT1;
         auto tmp = WideEmptyCopy(mpFactory);
         tmp->InsertSilence(0.0, duration);
         tmp->Flush();
         Paste(oldT1, *tmp);
      }
   }
   else if (newT1 < oldT1)
      Clear(newT1, oldT1);
}

void WaveTrack::PasteWaveTrack(double t0, const WaveTrack& other, bool merge)
{
   // Get a modifiable copy of `src` because it may come from another project
   // with different tempo, making boundary queries incorrect.
   const auto& tempo = GetProjectTempo(*this);
   if (!tempo.has_value())
      THROW_INCONSISTENCY_EXCEPTION;
   const auto copyHolder = other.DuplicateWithOtherTempo(*tempo);
   PasteWaveTrackAtSameTempo(t0, *copyHolder, merge);
}

void WaveTrack::PasteWaveTrackAtSameTempo(
   double t0, const WaveTrack& other, bool merge)
{
   const auto otherNChannels = other.NChannels();
   assert(otherNChannels == NChannels());
   assert(
      GetProjectTempo(*this).has_value() &&
      GetProjectTempo(*this) == GetProjectTempo(other));
   const auto startTime = other.GetStartTime();
   const auto endTime = other.GetEndTime();

   const auto insertDuration = endTime;
   auto &track = *this;
    //
    // Pasting is a bit complicated, because with the existence of multiclip mode,
    // we must guess the behaviour the user wants.
    //
    // Currently, two modes are implemented:
    //
    // - If a single clip should be pasted, and it should be pasted inside another
    //   clip, no NEW clips are generated. The audio is simply inserted.
    //   This resembles the old (pre-multiclip support) behaviour. However, if
    //   the clip is pasted outside of any clip, a NEW clip is generated. This is
    //   the only behaviour which is different to what was done before, but it
    //   shouldn't confuse users too much.
    //
    // - If multiple clips should be pasted, or a single clip that does not fill
    // the duration of the pasted track, these are always pasted as single
    // clips, and the current clip is split, when necessary. This may seem
    // strange at first, but it probably is better than trying to auto-merge
    // anything. The user can still merge the clips by hand (which should be a
    // simple command reachable by a hotkey or single mouse click).
    //

    if (other.GetNumClips() == 0)
        return;

    t0 = track.SnapToSample(t0);

    //wxPrintf("paste: we have at least one clip\n");

    const auto clipAtT0 = track.GetIntervalAtTime(t0);
    const auto otherFirstClip = other.GetLeftmostClip();
    const auto otherLastClip = other.GetRightmostClip();
    const auto pitchAndSpeedMatch =
       !clipAtT0 || (clipAtT0->HasEqualPitchAndSpeed(*otherFirstClip) &&
                     clipAtT0->HasEqualPitchAndSpeed(*otherLastClip));

    // `singleClipMode` will try to merge. Only allow this if clips on both ends
    // of the selection have equal stretch ratio.
    const bool singleClipMode =
       other.GetNumClips() == 1 &&
       std::abs(startTime) < track.LongSamplesToTime(1) * 0.5 &&
       pitchAndSpeedMatch && merge;

    const auto rate = track.GetRate();
    if (insertDuration != 0 && insertDuration < 1.0 / rate)
        // PRL:  I added this check to avoid violations of preconditions in other WaveClip and Sequence
        // methods, but allow the value 0 so I don't subvert the purpose of commit
        // 739422ba70ceb4be0bb1829b6feb0c5401de641e which causes append-recording always to make
        // a new clip.
        return;

    //wxPrintf("Check if we need to make room for the pasted data\n");

    auto pastingFromTempTrack = !other.GetOwner();
    bool editClipCanMove = GetEditClipsCanMove();

    const SimpleMessageBoxException notEnoughSpaceException {
       ExceptionType::BadUserAction,
       XO("There is not enough room available to paste the selection"),
       XO("Warning"), "Error:_Insufficient_space_in_track"
    };

    // Make room for the pasted data
    if (editClipCanMove) {
        if (!singleClipMode)
            // We need to insert multiple clips, so split the current clip and ...
            track.SplitAt(t0);

        //else if there is a clip at t0 insert new clip inside it and ...

        // ... move everything to the right
        for (const auto& clip : track.Intervals())
            if (clip->GetPlayStartTime() > t0 - (1.0 / rate))
                clip->ShiftBy(insertDuration);
    }
    else
    {
       if (!merge)
          track.SplitAt(t0);
       const auto clipAtT0 = track.GetClipAtTime(t0);
       const auto t = clipAtT0 ? clipAtT0->GetPlayEndTime() : t0;
       if (!track.IsEmpty(t, t + insertDuration))
          throw notEnoughSpaceException;
    }

    // See if the clipboard data is one clip only and if it should be merged. If
    // edit-clip-can-move mode is checked, merging happens only if the pasting
    // point splits a clip. If it isn't, merging also happens when the pasting
    // point is at the exact beginning of a clip.
    if (singleClipMode && merge) {
        // Single clip mode
        // wxPrintf("paste: checking for single clip mode!\n");

        IntervalHolder insideClip{};
        for (const auto& clip : track.Intervals()) {
            if (editClipCanMove) {
                if (clip->SplitsPlayRegion(t0)) {
                    //wxPrintf("t0=%.6f: inside clip is %.6f ... %.6f\n",
                    //       t0, clip->GetStartTime(), clip->GetEndTime());
                    insideClip = clip;
                    break;
                }
            }
            else {
                // If clips are immovable we also allow prepending to clips
                if (clip->WithinPlayRegion(t0))
                {
                    insideClip = clip;
                    break;
                }
            }
        }

        if (insideClip) {
            // Exhibit traditional behaviour
            //wxPrintf("paste: traditional behaviour\n");
            if (!editClipCanMove) {
                // We did not move other clips out of the way already, so
                // check if we can paste without having to move other clips
                for (const auto& clip : track.Intervals()) {
                    if (clip->GetPlayStartTime() > insideClip->GetPlayStartTime() &&
                        insideClip->GetPlayEndTime() + insertDuration >
                        clip->GetPlayStartTime())
                        // Strong-guarantee in case of this path
                        // not that it matters.
                        throw notEnoughSpaceException;
                }
            }
            if (auto pClip = other.GetWideClip(0)) {
               // This branch only gets executed in `singleClipMode` - we've
               // already made sure that stretch ratios are equal, satisfying
               // `WaveClip::Paste`'s precondition.
               bool success = insideClip->Paste(t0, *pClip);
               // TODO wide wave tracks -- prove success, or propagate failure,
               // or we might throw a MessageBoxException
               // (which would require a change in base class Track)
               // for now it would be quiet failure if clip widths mismatched
               // Can't yet assert(success);
            }
            return;
        }
        // Just fall through and exhibit NEW behaviour
    }

    // Insert NEW clips
    //wxPrintf("paste: multi clip mode!\n");

    if (!editClipCanMove &&
        !track.IsEmpty(t0, t0 + insertDuration - 1.0 / rate))
        // Strong-guarantee in case of this path
        // not that it matters.
        throw notEnoughSpaceException;

    for (const auto& clip : other.Intervals()) {
        // AWD Oct. 2009: Don't actually paste in placeholder clips
        if (!clip->IsPlaceholder()) {
            const auto name = (pastingFromTempTrack)
                //Clips from the tracks which aren't bound to any TrackList are
                //considered to be new entities, thus named using "new" name template
                ? track.MakeNewClipName()
                : track.MakeClipCopyName(clip->GetName());
            const auto oldPlayStart = clip->GetPlayStartTime();
            const auto newSequenceStart =
               (oldPlayStart + t0) - clip->GetTrimLeft();
            const auto newClip =
               CreateWideClip(newSequenceStart, name, clip.get());
            newClip->Resample(rate);
            track.InsertInterval(move(newClip), false);
        }
    }
}

bool WaveTrack::RateConsistencyCheck() const
{
   // The channels and all clips in them should have the same sample rate.
   std::optional<double> oRate;
   auto channels = TrackList::Channels(this);
   return std::all_of(channels.begin(), channels.end(),
      [&](const WaveTrack *pTrack){
         if (!pTrack)
            return false;

         const auto rate = pTrack->mLegacyRate;
         if (!oRate)
            oRate = rate;
         else if (*oRate != rate)
            return false;
         return true;
      });
}

bool WaveTrack::FormatConsistencyCheck() const
{
   const auto channels = TrackList::Channels(this);
   return std::all_of(channels.begin(), channels.end(),
      [&](const WaveTrack *pTrack){
         return pTrack && pTrack->mLegacyFormat == mLegacyFormat;
      });
}

bool WaveChannel::InsertClip(WaveClipHolder clip,
   bool newClip, bool backup, bool allowEmpty)
{
   if (!backup && !clip->GetIsPlaceholder() && !allowEmpty && clip->IsEmpty())
      return false;

   auto &clips = Clips();
   const auto& tempo = GetProjectTempo(GetTrack());
   if (tempo.has_value())
      clip->OnProjectTempoChange(std::nullopt, *tempo);
   clips.push_back(std::move(clip));
   GetTrack().Publish({ clips.back(),
      newClip ? WaveTrackMessage::New : WaveTrackMessage::Inserted });

   return true;
}

void WaveChannel::RemoveClip(size_t iClip)
{
   if (iClip < mClips.size())
      mClips.erase(mClips.begin() + iClip);
}

void WaveTrack::ApplyPitchAndSpeed(
   std::optional<TimeInterval> interval, ProgressReporter reportProgress)
{
   // Assert that the interval is reasonable, but this function will be no-op
   // anyway if not
   assert(!interval.has_value() ||
          interval->first <= interval->second);
   if (GetNumClips() == 0)
      return;
   const auto startTime =
      interval ? std::max(SnapToSample(interval->first), GetStartTime()) :
                 GetStartTime();
   const auto endTime =
      interval ? std::min(SnapToSample(interval->second), GetEndTime()) :
                 GetEndTime();
   if (startTime >= endTime)
      return;

   // Here we assume that left- and right clips are aligned.
   if (auto clipAtT0 = GetClipAtTime(startTime);
       clipAtT0 && clipAtT0->SplitsPlayRegion(startTime) &&
       clipAtT0->HasPitchOrSpeed())
      Split(startTime, startTime);
   if (auto clipAtT1 = GetClipAtTime(endTime);
       clipAtT1 && clipAtT1->SplitsPlayRegion(endTime) &&
       clipAtT1->HasPitchOrSpeed())
      Split(endTime, endTime);

   IntervalHolders srcIntervals;
   auto clip = GetIntervalAtTime(startTime);
   while (clip && clip->GetPlayStartTime() < endTime)
   {
      if (clip->HasPitchOrSpeed())
         srcIntervals.push_back(clip);
      clip = GetNextInterval(*clip, PlaybackDirection::forward);
   }

   ApplyPitchAndSpeedOnIntervals(srcIntervals, reportProgress);
}

/*! @excsafety{Weak} */
void WaveTrack::Paste(double t0, const Track &src)
{
   if (const auto other = dynamic_cast<const WaveTrack*>(&src))
   {
      // Currently `Paste` isn't used by code that wants the newer "don't merge
      // when copy/pasting" behaviour ...
      constexpr auto merge = true;
      PasteWaveTrack(t0, *other, merge);
   }
   else
      // THROW_INCONSISTENCY_EXCEPTION; // ?
      (void)0;// Empty if intentional.
}

void WaveTrack::Silence(double t0, double t1, ProgressReporter reportProgress)
{
   if (t1 < t0)
      THROW_INCONSISTENCY_EXCEPTION;

   ApplyPitchAndSpeed({ { t0, t1 } }, std::move(reportProgress));

   auto start = TimeToLongSamples(t0);
   auto end = TimeToLongSamples(t1);

   for (const auto &pClip : Intervals()) {
      auto clipStart = pClip->GetPlayStartSample();
      auto clipEnd = pClip->GetPlayEndSample();
      if (clipEnd > start && clipStart < end) {
         auto offset = std::max(start - clipStart, sampleCount(0));
         // Clip sample region and Get/Put sample region overlap
         auto length = std::min(end, clipEnd) - (clipStart + offset);
         pClip->SetSilence(offset, length);
      }
   }
}

/*! @excsafety{Strong} */
void WaveTrack::InsertSilence(double t, double len)
{
   // Nothing to do, if length is zero.
   // Fixes Bug 1626
   if (len == 0)
      return;
   if (len <= 0)
      THROW_INCONSISTENCY_EXCEPTION;

   auto &&clips = Intervals();
   if (clips.empty()) {
      // Special case if there is no clip yet
      // TODO wide wave tracks -- match clip width
      auto clip = CreateWideClip(0);
      clip->InsertSilence(0, len);
      // use No-fail-guarantee
      InsertInterval(move(clip), true);
   }
   else
   {
      // Assume at most one clip contains t
      const auto end = clips.end();
      const auto it = std::find_if(clips.begin(), end,
         [&](const IntervalHolder &clip) { return clip->SplitsPlayRegion(t); } );

      // use Strong-guarantee
      if (it != end)
         (*it)->InsertSilence(t, len);

      // use No-fail-guarantee
      for (const auto &&clip : clips)
         if (clip->BeforePlayRegion(t))
            clip->ShiftBy(len);
   }
}

//Performs the opposite of Join
//Analyses selected region for possible Joined clips and disjoins them
/*! @excsafety{Weak} */
void WaveTrack::Disjoin(double t0, double t1)
{
   auto minSamples = TimeToLongSamples(WAVETRACK_MERGE_POINT_TOLERANCE);
   const size_t maxAtOnce = 1048576;
   std::vector<float> buffer;
   std::vector<samplePtr> buffers;
   Regions regions;

   const size_t width = NChannels();

   for (const auto &interval : Intervals()) {
      double startTime = interval->Start();
      double endTime = interval->End();

      if (endTime < t0 || startTime > t1)
         continue;

      // Assume all clips will have the same width
      if (buffer.empty()) {
         buffer.resize(maxAtOnce * width);
         buffers.resize(width);
         auto pBuffer = buffer.data();
         for (size_t ii = 0; ii < width; ++ii, pBuffer += maxAtOnce)
            buffers[ii] = reinterpret_cast<samplePtr>(pBuffer);
      }

      const auto allZeroesAt = [&](size_t i) {
         auto pData = buffer.data() + i;
         for (size_t ii = 0; ii < width; ++ii, pData += maxAtOnce) {
            if (*pData != 0.0)
               return false;
         }
         return true;
      };

      // simply look for a sequence of zeroes (across all channels) and if the
      // sequence is longer than the minimum number, split-delete the region

      sampleCount seqStart = -1;
      auto start = interval->TimeToSamples(std::max(.0, t0 - startTime));
      auto end = interval->TimeToSamples(std::min(endTime, t1) - startTime);

      auto len = (end - start);
      for (decltype(len) done = 0; done < len; done += maxAtOnce) {
         auto numSamples = limitSampleBufferSize(maxAtOnce, len - done);

         auto bufferIt = buffers.begin();

         for (auto channel : interval->Channels())
            channel->GetSamples(
               *bufferIt++, floatSample, start + done, numSamples);

         for (decltype(numSamples) i = 0; i < numSamples; ++i) {
            auto curSamplePos = start + done + i;

            //start a NEW sequence
            if (seqStart == -1 && allZeroesAt(i))
               seqStart = curSamplePos;
            else if (curSamplePos == end - 1 || !allZeroesAt(i)) {
               if (seqStart != -1) {
                  decltype(end) seqEnd;

                  //consider the end case, where selection ends in zeroes
                  if (curSamplePos == end - 1 && allZeroesAt(i))
                     seqEnd = end;
                  else
                     seqEnd = curSamplePos;
                  if (seqEnd - seqStart + 1 > minSamples) {
                     regions.push_back(
                        Region(
                           startTime + interval->SamplesToTime(seqStart),
                           startTime + interval->SamplesToTime(seqEnd)
                        )
                     );
                  }
                  seqStart = -1;
               }
            }
         } // samples
      } // blocks
   } // finding regions

   for (const auto &region : regions)
      SplitDelete(region.start, region.end);
}

/*! @excsafety{Weak} */
void WaveTrack::Join(
   double t0, double t1, const ProgressReporter& reportProgress)
{
   // Merge all WaveClips overlapping selection into one
   const auto &intervals = Intervals();

   {
      IntervalHolders intervalsToJoin;
      for (const auto &interval : intervals)
         if (interval->IntersectsPlayRegion(t0, t1))
            intervalsToJoin.push_back(interval);
      if (intervalsToJoin.size() < 2u)
         return;
      if (std::any_of(
             intervalsToJoin.begin() + 1, intervalsToJoin.end(),
             [first = intervalsToJoin[0]](const auto& interval) {
                return !first->HasEqualPitchAndSpeed(*interval);
             }))
         ApplyPitchAndSpeedOnIntervals(intervalsToJoin, reportProgress);
   }

   IntervalHolders clipsToDelete;
   IntervalHolder newClip{};

   const auto rate = GetRate();
   for (const auto &clip: intervals) {
      if (clip->IntersectsPlayRegion(t0, t1)) {
         // Put in sorted order
         auto it = clipsToDelete.begin(), end = clipsToDelete.end();
         for (; it != end; ++it)
            if ((*it)->GetPlayStartTime() > clip->GetPlayStartTime())
               break;
         //wxPrintf("Insert clip %.6f at position %d\n", clip->GetStartTime(), i);
         clipsToDelete.insert(it, clip);
      }
   }

   //if there are no clips to delete, nothing to do
   if (clipsToDelete.empty())
      return;

   const auto firstToDelete = clipsToDelete[0].get();
   auto t = firstToDelete->GetPlayStartTime();
   //preserve left trim data if any
   newClip = CreateWideClip(
      firstToDelete->GetSequenceStartTime(),
      firstToDelete->GetName());

   for (const auto &clip : clipsToDelete) {
      // wxPrintf("t=%.6f adding clip (offset %.6f, %.6f ... %.6f)\n",
      //       t, clip->GetOffset(), clip->GetStartTime(),
      //       clip->GetEndTime());

      if (clip->GetPlayStartTime() - t > (1.0 / rate))
      {
         double addedSilence = (clip->GetPlayStartTime() - t);
         // wxPrintf("Adding %.6f seconds of silence\n");
         auto offset = clip->GetPlayStartTime();
         auto value = clip->GetEnvelope().GetValue(offset);
         newClip->AppendSilence(addedSilence, value);
         t += addedSilence;
      }

      // wxPrintf("Pasting at %.6f\n", t);
      bool success = newClip->Paste(t, *clip);
      assert(success); // promise of CreateClip

      t = newClip->GetPlayEndTime();

      RemoveWideClip(FindWideClip(*clip));
   }

   InsertInterval(move(newClip), false);
}

/*! @excsafety{Partial}
-- Some prefix (maybe none) of the buffer is appended,
and no content already flushed to disk is lost. */
bool WaveChannel::AppendBuffer(constSamplePtr buffer, sampleFormat format,
   size_t len, unsigned stride, sampleFormat effectiveFormat)
{
   const size_t iChannel = GetChannelIndex();
   return GetTrack()
      .Append(iChannel, buffer, format, len, stride, effectiveFormat);
}

/*! @excsafety{Partial}
-- Some prefix (maybe none) of the buffer is appended,
and no content already flushed to disk is lost. */
bool WaveChannel::Append(constSamplePtr buffer, sampleFormat format,
   size_t len)
{
   const size_t iChannel = GetChannelIndex();
   return GetTrack()
      .Append(iChannel, buffer, format, len, 1, widestSampleFormat);
}

/*! @excsafety{Partial}
-- Some prefix (maybe none) of the buffer is appended,
and no content already flushed to disk is lost. */
bool WaveTrack::Append(size_t iChannel,
   constSamplePtr buffer, sampleFormat format,
   size_t len, unsigned int stride, sampleFormat effectiveFormat)
{
   assert(iChannel < NChannels());
   // TODO wide wave tracks -- there will be only one clip, and its `Append`
   // (or an overload) must take iChannel
   auto pTrack = this;
   constSamplePtr buffers[]{ buffer };
   auto pClip = RightmostOrNewClip();
   auto iter = pClip->Channels().begin();
   std::advance(iter, iChannel);
   return (*iter)->
      GetClip().Append(buffers, format, len, stride, effectiveFormat);
}

size_t WaveTrack::GetBestBlockSize(sampleCount s) const
{
   auto bestBlockSize = GetMaxBlockSize();

   for (const auto &clip : Intervals()) {
      auto startSample = clip->GetPlayStartSample();
      auto endSample = clip->GetPlayEndSample();
      if (s >= startSample && s < endSample)
      {
         // ignore extra channels (this function will soon be removed)
         bestBlockSize =
            clip->GetBestBlockSize(s - clip->GetSequenceStartSample());
         break;
      }
   }

   return bestBlockSize;
}

size_t WaveTrack::GetMaxBlockSize() const
{
   const auto clips = Intervals();
   auto maxblocksize = std::accumulate(clips.begin(), clips.end(), size_t{},
   [](size_t acc, auto pClip){
      return std::max(acc, pClip->GetMaxBlockSize()); });

   if (maxblocksize == 0)
   {
      // We really need the maximum block size, so create a
      // temporary sequence to get it.
      maxblocksize =
         Sequence{ mpFactory, SampleFormats{GetSampleFormat(), GetSampleFormat()} }
            .GetMaxBlockSize();
   }

   wxASSERT(maxblocksize > 0);

   return maxblocksize;
}

size_t WaveTrack::GetIdealBlockSize()
{
   // ignore extra channels (this function will soon be removed)
   return (*NewestOrNewClip()->Channels().begin())->GetClip()
      .GetSequence(0)->GetIdealBlockSize();
}


// TODO restore a proper exception safety guarantee; comment below is false
// because failure might happen after only one channel is done
/*! @excsafety{Mixed} */
/*! @excsafety{No-fail} -- The rightmost clip will be in a flushed state. */
/*! @excsafety{Partial}
-- Some initial portion (maybe none) of the append buffer of the rightmost
clip gets appended; no previously saved contents are lost. */
void WaveTrack::Flush()
{
   if (NIntervals() == 0)
      return;
   // After appending, presumably.  Do this to the clip that gets appended.
   GetRightmostClip()->Flush();
}

void WaveTrack::RepairChannels()
{
   for (auto pInterval : Intervals())
      pInterval->RepairChannels();
}

void WaveTrack::SetLegacyFormat(sampleFormat format)
{
   mLegacyFormat = format;
}

void WaveTrack::CopyClipEnvelopes()
{
   if (NChannels() != 2)
      return;
   // Assume correspondence of clips
   const auto leftClips = NarrowClips();
   const auto rightClips = RightClips();
   auto it = begin(leftClips),
      last = end(leftClips);
   auto it2 = begin(rightClips),
      last2 = end(rightClips);
   for (; it != last; ++it, ++it2) {
      if (it2 == last2) {
         assert(false);
         break;
      }
      (*it2)->SetEnvelope(std::make_unique<Envelope>(*(*it)->GetEnvelope()));
   }
}

const ChannelGroup *WaveTrack::FindChannelGroup() const
{
   return this;
}

bool WaveTrack::GetMute() const
{
   return PlayableTrack::GetMute();
}

bool WaveTrack::GetSolo() const
{
   return PlayableTrack::GetSolo();
}

const char *WaveTrack::WaveTrack_tag = "wavetrack";

static constexpr auto Offset_attr = "offset";
static constexpr auto Rate_attr = "rate";
static constexpr auto Gain_attr = "gain";
static constexpr auto Pan_attr = "pan";
static constexpr auto Linked_attr = "linked";
static constexpr auto SampleFormat_attr = "sampleformat";
static constexpr auto Channel_attr = "channel"; // write-only!

bool WaveTrack::HandleXMLTag(const std::string_view& tag, const AttributesList &attrs)
{
   if (tag == WaveTrack_tag) {
      double dblValue;
      long nValue;

      for (const auto& pair : attrs)
      {
         const auto& attr = pair.first;
         const auto& value = pair.second;

         if (attr == Rate_attr)
         {
            // mRate is an int, but "rate" in the project file is a float.
            if (!value.TryGet(dblValue) ||
                  (dblValue < 1.0) || (dblValue > 1000000.0)) // allow a large range to be read
               return false;

            // Defer the setting of rate until LinkConsistencyFix
            mLegacyRate = lrint(dblValue);
         }
         else if (attr == Offset_attr && value.TryGet(dblValue))
         {
            // Offset is only relevant for legacy project files. The value
            // is cached until the actual WaveClip containing the legacy
            // track is created.
            mLegacyProjectFileOffset = dblValue;
         }
         else if (this->WritableSampleTrack::HandleXMLAttribute(attr, value))
         {}
         else if (this->Track::HandleCommonXMLAttribute(attr, value))
            ;
         else if (attr == Gain_attr && value.TryGet(dblValue))
            DoSetGain(dblValue);
         else if (attr == Pan_attr && value.TryGet(dblValue) &&
                  (dblValue >= -1.0) && (dblValue <= 1.0))
            DoSetPan(dblValue);
         else if (attr == Linked_attr && value.TryGet(nValue))
            SetLinkType(ToLinkType(nValue), false);
         else if (attr == SampleFormat_attr && value.TryGet(nValue) &&
                  Sequence::IsValidSampleFormat(nValue))
         {
            //Remember sample format until consistency check is performed.
            SetLegacyFormat(static_cast<sampleFormat>(nValue));
         }
      } // while
      return true;
   }

   return false;
}

void WaveTrack::HandleXMLEndTag(const std::string_view&  WXUNUSED(tag))
{
#if 0
   // In case we opened a pre-multiclip project, we need to
   // simulate closing the waveclip tag.
   NewestOrNewClip()->HandleXMLEndTag(WaveClip::WaveClip_tag);
#else
   // File compatibility breaks have intervened long since, and the line above
   // would now have undesirable side effects
#endif
}

XMLTagHandler *WaveTrack::HandleXMLChild(const std::string_view& tag)
{
   if (auto pChild = WaveTrackIORegistry::Get().CallObjectAccessor(tag, *this))
      // Deserialize any extra attached structures
      return pChild;

   const auto getClip = [this]() -> WaveClip & {
      return (*NewestOrNewClip()->Channels().begin())->GetClip();
   };

   //
   // This is legacy code (1.2 and previous) and is not called for new projects!
   if (tag == Sequence::Sequence_tag || tag == "envelope") {
      // This is a legacy project, so set the cached offset
      NewestOrNewClip()->SetSequenceStartTime(mLegacyProjectFileOffset);

      // Legacy project file tracks are imported as one single wave clip
      if (tag == Sequence::Sequence_tag)
         return getClip().GetSequence(0);
      else if (tag == "envelope")
         return getClip().GetEnvelope();
   }

   // JKC... for 1.1.0, one step better than what we had, but still badly broken.
   // If we see a waveblock at this level, we'd better generate a sequence.
   if (tag == Sequence::WaveBlock_tag) {
      // This is a legacy project, so set the cached offset
      NewestOrNewClip()->SetSequenceStartTime(mLegacyProjectFileOffset);
      auto pSeq = getClip().GetSequence(0);
      return pSeq;
   }

   // This is for the new file format (post-1.2)
   if (tag == WaveClip::WaveClip_tag) {
      // Make clips (which don't serialize the rate) consistent with channel rate,
      // though the consistency check of channels with each other remains to do.
      // Not all `WaveTrackData` fields are properly initialized by now,
      // use deserialization helpers.
      auto clip = std::make_shared<WaveClip>(1,
         mpFactory, mLegacyFormat, mLegacyRate);
      const auto xmlHandler = clip.get();
      auto &clips = NarrowClips();
      clips.push_back(std::move(clip));
      Publish({ clips.back(), WaveTrackMessage::Deserialized });
      return xmlHandler;
   }

   return nullptr;
}

void WaveTrack::WriteXML(XMLWriter &xmlFile) const
// may throw
{
   const auto channels = Channels();
   size_t iChannel = 0,
      nChannels = channels.size();
   for (const auto pChannel : channels)
      WriteOneXML(*pChannel, xmlFile, iChannel++, nChannels);
}

void WaveTrack::WriteOneXML(const WaveChannel &channel, XMLWriter &xmlFile,
   size_t iChannel, size_t nChannels)
// may throw
{
   // Track data has always been written using channel-major iteration.
   // Do it still this way for compatibility.

   // Some values don't vary independently in channels but have been written
   // redundantly for each channel.  Keep doing this in 3.4 and later in case
   // a project is opened in an earlier version.

   xmlFile.StartTag(WaveTrack_tag);
   auto &track = channel.GetTrack();

   // Name, selectedness, etc. are channel group properties
   track.Track::WriteCommonXMLAttributes(xmlFile);

   // Write the "channel" attribute so earlier versions can interpret stereo
   // tracks, but this version doesn't read it
   {
      enum ChannelType {
         LeftChannel = 0,
         RightChannel = 1,
         MonoChannel = 2
      };
      const auto channelType = (nChannels == 0)
         ? MonoChannel
         : (iChannel == 0)
            ? LeftChannel
            : RightChannel;
      xmlFile.WriteAttr(Channel_attr, channelType);
   }

   // The "linked" flag is used to define the beginning of a channel group
   // that isn't mono
   const auto linkType = static_cast<int>(
      (iChannel == 0) && (nChannels == 2)
         ? LinkType::Aligned
         : LinkType::None);
   xmlFile.WriteAttr(Linked_attr, linkType);

   // More channel group properties written redundantly
   track.WritableSampleTrack::WriteXMLAttributes(xmlFile);
   xmlFile.WriteAttr(Rate_attr, track.GetRate());
   xmlFile.WriteAttr(Gain_attr, static_cast<double>(track.GetGain()));
   xmlFile.WriteAttr(Pan_attr, static_cast<double>(track.GetPan()));
   xmlFile.WriteAttr(SampleFormat_attr, static_cast<long>(track.GetSampleFormat()));

   // Other persistent data specified elsewhere;
   // NOT written redundantly any more
   if (iChannel == 0)
      WaveTrackIORegistry::Get().CallWriters(track, xmlFile);

   for (const auto &clip : channel.Intervals())
      clip->WriteXML(xmlFile);

   xmlFile.EndTag(WaveTrack_tag);
}

std::optional<TranslatableString> WaveTrack::GetErrorOpening() const
{
   const auto width = NChannels();
   for (const auto &clip : Intervals())
      // TODO wide wave clip -- inner loop over GetSequence() argument
      for (size_t ii = 0; ii < width; ++ii)
         if (auto pClip = clip->GetClip(ii);
             pClip && pClip->GetSequence(0)->GetErrorOpening())
            return XO("A track has a corrupted sample sequence.");

   return {};
}

auto WaveTrack::GetLeftmostClip() -> IntervalHolder {
   auto clips = Intervals();
   if (clips.empty())
      return nullptr;
   const auto begin = clips.begin(),
      iter = std::min_element(begin, clips.end(),
          [](const auto& a, const auto b) {
             return a->GetPlayStartTime() < b->GetPlayStartTime();
          });
   return GetWideClip(std::distance(begin, iter));
}

auto WaveTrack::GetLeftmostClip() const -> IntervalConstHolder {
   return const_cast<WaveTrack&>(*this).GetLeftmostClip();
}

auto WaveTrack::GetRightmostClip() -> IntervalHolder {
   auto clips = Intervals();
   if (clips.empty())
      return nullptr;
   const auto begin = clips.begin(),
      iter = std::max_element(begin, clips.end(),
          [](const auto& a, const auto b) {
             return a->GetPlayEndTime() < b->GetPlayEndTime();
          });
   return GetWideClip(std::distance(begin, iter));
}

auto WaveTrack::GetRightmostClip() const -> IntervalConstHolder {
   return const_cast<WaveTrack&>(*this).GetRightmostClip();
}

ClipConstHolders WaveTrack::GetClipInterfaces() const
{
   auto clips = Intervals();
   return { clips.begin(), clips.end() };
}

double WaveChannel::GetStartTime() const
{
   return GetTrack().GetStartTime();
}

double WaveTrack::GetStartTime() const
{
   return ChannelGroup::GetStartTime();
}

double WaveChannel::GetEndTime() const
{
   return GetTrack().GetEndTime();
}

double WaveTrack::GetEndTime() const
{
   return ChannelGroup::GetEndTime();
}

bool WaveChannel::DoGet(size_t iChannel, size_t nBuffers,
   const samplePtr buffers[], sampleFormat format,
   sampleCount start, size_t len, bool backwards, fillFormat fill,
   bool mayThrow, sampleCount* pNumWithinClips) const
{
   assert(iChannel == 0);
   assert(nBuffers <= 1);
   return GetTrack().DoGet(GetChannelIndex(), std::min<size_t>(nBuffers, 1),
      buffers, format, start, len, backwards, fill, mayThrow, pNumWithinClips);
}

//
// Getting/setting samples.  The sample counts here are
// expressed relative to t=0.0 at the track's sample rate.
//

bool WaveTrack::DoGet(size_t iChannel, size_t nBuffers,
   const samplePtr buffers[], sampleFormat format,
   sampleCount start, size_t len, bool backwards, fillFormat fill,
   bool mayThrow, sampleCount* pNumWithinClips) const
{
   const auto nChannels = NChannels();
   assert(iChannel + nBuffers <= nChannels); // precondition
   return std::all_of(buffers, buffers + nBuffers, [&](samplePtr buffer) {
      const auto &clips = iChannel == 0 ? NarrowClips() : RightClips();
      ++iChannel;
      const auto result = GetOne(clips,
         buffer, format, start, len, backwards, fill, mayThrow,
         pNumWithinClips);
      return result;
   });
}

bool WaveTrack::GetOne(const WaveClipHolders &clips,
   samplePtr buffer, sampleFormat format, sampleCount start, size_t len,
   bool backwards, fillFormat fill, bool mayThrow,
   sampleCount* pNumWithinClips) const
{
   if (backwards)
      start -= len;
   // Simple optimization: When this buffer is completely contained within one clip,
   // don't clear anything (because we won't have to). Otherwise, just clear
   // everything to be on the safe side.
   bool doClear = true;
   bool result = true;
   sampleCount samplesCopied = 0;
   for (const auto &clip: clips)
   {
      if (start >= clip->GetPlayStartSample() && start+len <= clip->GetPlayEndSample())
      {
         doClear = false;
         break;
      }
   }
   if (doClear)
   {
      // Usually we fill in empty space with zero
      if (fill == FillFormat::fillZero)
         ClearSamples(buffer, format, 0, len);
      // but we don't have to.
      else if (fill == FillFormat::fillTwo)
      {
         wxASSERT( format==floatSample );
         float * pBuffer = (float*)buffer;
         for(size_t i=0;i<len;i++)
            pBuffer[i]=2.0f;
      }
      else
      {
         wxFAIL_MSG(wxT("Invalid fill format"));
      }
   }

   // Iterate the clips.  They are not necessarily sorted by time.
   for (const auto &clip: clips)
   {
      auto clipStart = clip->GetPlayStartSample();
      auto clipEnd = clip->GetPlayEndSample();

      if (clipEnd > start && clipStart < start+len)
      {
         if (clip->HasPitchOrSpeed())
            return false;

         // Clip sample region and Get/Put sample region overlap
         auto samplesToCopy =
            std::min( start+len - clipStart, clip->GetVisibleSampleCount() );
         auto startDelta = clipStart - start;
         decltype(startDelta) inclipDelta = 0;
         if (startDelta < 0)
         {
            inclipDelta = -startDelta; // make positive value
            samplesToCopy -= inclipDelta;
            // samplesToCopy is now either len or
            //    (clipEnd - clipStart) - (start - clipStart)
            //    == clipEnd - start > 0
            // samplesToCopy is not more than len
            //
            startDelta = 0;
            // startDelta is zero
         }
         else {
            // startDelta is nonnegative and less than len
            // samplesToCopy is positive and not more than len
         }

         if (!clip->GetSamples(0,
               (samplePtr)(((char*)buffer) +
                           startDelta.as_size_t() *
                           SAMPLE_SIZE(format)),
               format, inclipDelta, samplesToCopy.as_size_t(), mayThrow ))
            result = false;
         else
            samplesCopied += samplesToCopy;
      }
   }
   if( pNumWithinClips )
      *pNumWithinClips = samplesCopied;
   if (result == true && backwards)
      ReverseSamples(buffer, format, 0, len);
   return result;
}

ChannelGroupSampleView
WaveTrack::GetSampleView(double t0, double t1, bool mayThrow) const
{
   ChannelGroupSampleView result;
   for (const auto& channel : Channels()) {
      result.push_back(channel->GetSampleView(t0, t1, mayThrow));
   }
   return result;
}

ChannelSampleView
WaveChannel::GetSampleView(double t0, double t1, bool mayThrow) const
{
   std::vector<std::shared_ptr<const WaveChannelInterval>>
      intersectingIntervals;
   for (const auto &interval : Intervals())
      if (interval->Intersects(t0, t1))
         intersectingIntervals.push_back(interval);
   if (intersectingIntervals.empty())
      return { AudioSegmentSampleView {
         (TimeToLongSamples(t1) - TimeToLongSamples(t0)).as_size_t() } };
   std::sort(
      intersectingIntervals.begin(), intersectingIntervals.end(),
      [](const auto& a, const auto& b) { return a->Start() < b->Start(); });
   std::vector<AudioSegmentSampleView> segments;
   segments.reserve(2 * intersectingIntervals.size() + 1);
   for (auto i = 0u; i < intersectingIntervals.size();++i)
   {
      const auto& interval = intersectingIntervals[i];
      const auto intervalStartTime = interval->Start();
      if (t0 < intervalStartTime)
      {
         const auto numSamples = TimeToLongSamples(intervalStartTime - t0);
         segments.push_back(AudioSegmentSampleView{numSamples.as_size_t()});
         t0 = intervalStartTime;
      }
      const auto intervalT0 = t0 - intervalStartTime;
      const auto intervalT1 = std::min(t1, interval->End()) - intervalStartTime;
      if(intervalT1 > intervalT0)
      {
         auto newSegment =
            interval->GetSampleView(intervalT0, intervalT1, mayThrow);
         t0 += intervalT1 - intervalT0;
         segments.push_back(std::move(newSegment));
      }
      if (t0 == t1)
         break;
   }
   if (t0 < t1)
      segments.push_back(AudioSegmentSampleView {
         (TimeToLongSamples(t1) - TimeToLongSamples(t0)).as_size_t() });
   return segments;
}

/*! @excsafety{Weak} */
bool WaveChannel::Set(constSamplePtr buffer, sampleFormat format,
   sampleCount start, size_t len, sampleFormat effectiveFormat)
{
   for (const auto &clip: Intervals())
   {
      auto clipStart = clip->GetPlayStartSample();
      auto clipEnd = clip->GetPlayEndSample();

      if (clipEnd > start && clipStart < start+len)
      {
         // Test as also in WaveTrack::GetOne()
         if (clip->HasPitchOrSpeed())
            return false;

         // Clip sample region and Get/Put sample region overlap
         auto samplesToCopy =
            std::min( start+len - clipStart, clip->GetVisibleSampleCount() );
         auto startDelta = clipStart - start;
         decltype(startDelta) inclipDelta = 0;
         if (startDelta < 0)
         {
            inclipDelta = -startDelta; // make positive value
            samplesToCopy -= inclipDelta;
            // samplesToCopy is now either len or
            //    (clipEnd - clipStart) - (start - clipStart)
            //    == clipEnd - start > 0
            // samplesToCopy is not more than len
            //
            startDelta = 0;
            // startDelta is zero
         }
         else {
            // startDelta is nonnegative and less than len
            // samplesToCopy is positive and not more than len
         }

         clip->SetSamples(
            buffer + startDelta.as_size_t() * SAMPLE_SIZE(format),
            format, inclipDelta, samplesToCopy.as_size_t(), effectiveFormat );
      }
   }
   return true;
}

sampleFormat WaveChannel::WidestEffectiveFormat() const
{
   return GetTrack().WidestEffectiveFormat();
}

sampleFormat WaveTrack::WidestEffectiveFormat() const
{
   auto result = narrowestSampleFormat;
   for (const auto &pClip : Intervals())
      result = std::max(result, pClip->GetSampleFormats().Effective());
   return result;
}

bool WaveChannel::HasTrivialEnvelope() const
{
   return GetTrack().HasTrivialEnvelope();
}

bool WaveTrack::HasTrivialEnvelope() const
{
   auto pTrack = this;
   if (!pTrack)
      return false;
   auto clips = pTrack->Intervals();
   return std::all_of(clips.begin(), clips.end(),
      [](const auto &pClip){ return pClip->GetEnvelope().IsTrivial(); });
}

void WaveChannel::GetEnvelopeValues(
   double* buffer, size_t bufferLen, double t0, bool backwards) const
{
   return GetTrack().GetEnvelopeValues(buffer, bufferLen, t0, backwards);
}

void WaveTrack::GetEnvelopeValues(
   double* buffer, size_t bufferLen, double t0, bool backwards) const
{
   auto pTrack = this;
   if (!pTrack)
      return;

   if (backwards)
      t0 -= bufferLen / pTrack->GetRate();
   // The output buffer corresponds to an unbroken span of time which the callers expect
   // to be fully valid.  As clips are processed below, the output buffer is updated with
   // envelope values from any portion of a clip, start, end, middle, or none at all.
   // Since this does not guarantee that the entire buffer is filled with values we need
   // to initialize the entire buffer to a default value.
   //
   // This does mean that, in the cases where a usable clip is located, the buffer value will
   // be set twice.  Unfortunately, there is no easy way around this since the clips are not
   // stored in increasing time order.  If they were, we could just track the time as the
   // buffer is filled.
   for (decltype(bufferLen) i = 0; i < bufferLen; i++)
   {
      buffer[i] = 1.0;
   }

   double startTime = t0;
   const auto rate = pTrack->GetRate();
   auto tstep = 1.0 / rate;
   double endTime = t0 + tstep * bufferLen;
   for (const auto &clip: pTrack->Intervals())
   {
      // IF clip intersects startTime..endTime THEN...
      auto dClipStartTime = clip->GetPlayStartTime();
      auto dClipEndTime = clip->GetPlayEndTime();
      if ((dClipStartTime < endTime) && (dClipEndTime > startTime))
      {
         auto rbuf = buffer;
         auto rlen = bufferLen;
         auto rt0 = t0;

         if (rt0 < dClipStartTime)
         {
            // This is not more than the number of samples in
            // (endTime - startTime) which is bufferLen:
            auto nDiff = (sampleCount)floor((dClipStartTime - rt0) * rate + 0.5);
            auto snDiff = nDiff.as_size_t();
            rbuf += snDiff;
            wxASSERT(snDiff <= rlen);
            rlen -= snDiff;
            rt0 = dClipStartTime;
         }

         if (rt0 + rlen*tstep > dClipEndTime)
         {
            auto nClipLen = clip->GetPlayEndSample() - clip->GetPlayStartSample();

            if (nClipLen <= 0) // Testing for bug 641, this problem is consistently '== 0', but doesn't hurt to check <.
               return;

            // This check prevents problem cited in http://bugzilla.audacityteam.org/show_bug.cgi?id=528#c11,
            // Gale's cross_fade_out project, which was already corrupted by bug 528.
            // This conditional prevents the previous write past the buffer end, in clip->GetEnvelope() call.
            // Never increase rlen here.
            // PRL bug 827:  rewrote it again
            rlen = limitSampleBufferSize( rlen, nClipLen );
            rlen = std::min(rlen, size_t(floor(0.5 + (dClipEndTime - rt0) / tstep)));
         }
         // Samples are obtained for the purpose of rendering a wave track,
         // so quantize time
         clip->GetEnvelope().GetValues(rbuf, rlen, rt0, tstep);
      }
   }
   if (backwards)
      std::reverse(buffer, buffer + bufferLen);
}

// When the time is both the end of a clip and the start of the next clip, the
// latter clip is returned.
const WaveClip* WaveTrack::GetClipAtTime(double time) const
{
   const auto clips = SortedClipArray();
   auto p = std::find_if(
      clips.rbegin(), clips.rend(), [&](const WaveClip* const& clip) {
         return clip->WithinPlayRegion(time);
      });
   return p != clips.rend() ? *p : nullptr;
}

auto WaveTrack::CreateWideClip(double offset, const wxString& name,
   const Interval *pToCopy, bool copyCutlines) -> IntervalHolder
{
   WaveClipHolder holders[2];
   size_t iChannel = 0;
   if (pToCopy)
      pToCopy->ForEachClip([&](const WaveClip &clip){
         auto pNewClip =
            std::make_shared<WaveClip>(clip, mpFactory, copyCutlines);
         pNewClip->SetName(name);
         pNewClip->SetSequenceStartTime(offset);
         holders[iChannel++] = pNewClip;
      });
   else {
      holders[iChannel++] = CreateClip(*this, nullptr, offset, name);
      if (mRightChannel.has_value())
         holders[iChannel++] = CreateClip(*this, nullptr, offset, name);
   }

   return std::make_shared<Interval>(*this, holders[0], holders[1]);
}

auto WaveTrack::CopyClip(const Interval &toCopy, bool copyCutlines)
   -> IntervalHolder
{
   return CreateWideClip(toCopy.GetSequenceStartTime(),
      toCopy.GetName(), &toCopy, copyCutlines);
}

void WaveTrack::CreateRight()
{
   mRightChannel.emplace(*this);
}

auto WaveTrack::CreateClip(WaveTrack &track,
   WaveClipHolders *pClips, double offset, const wxString& name)
   -> WaveClipHolder
{
   // TODO wide wave tracks -- choose clip width correctly for the track
   auto clip = std::make_shared<WaveClip>(1,
      track.mpFactory, track.GetSampleFormat(), track.GetRate());
   clip->SetName(name);
   clip->SetSequenceStartTime(offset);

   const auto& tempo = GetProjectTempo(track);
   if (tempo.has_value())
      clip->OnProjectTempoChange(std::nullopt, *tempo);
   if (pClips) {
      pClips->push_back(clip);
      track.Publish({ clip, WaveTrackMessage::New });
   }
   // TODO wide wave tracks -- for now assertion is correct because widths are
   // always 1
   assert(clip->GetWidth() == track.GetWidth());
   return clip;
}

auto WaveTrack::NewestOrNewClip() -> IntervalHolder
{
   auto &clips = NarrowClips();
   WaveClipHolder newClips[2];
   const auto origin = WaveTrackData::Get(*this).GetOrigin();
   const auto name = MakeNewClipName();
   newClips[0] = clips.empty()
      ? CreateClip(*this, &clips, origin, name)
      : clips.back();
   if (NChannels() > 1) {
      auto &rightClips = RightClips();
      newClips[1] = rightClips.empty()
         ? CreateClip(*this, &rightClips, origin, name)
         : rightClips.back();
   }
   return std::make_shared<Interval>(*this, newClips[0], newClips[1]);
}

/*! @excsafety{No-fail} */
auto WaveTrack::RightmostOrNewClip() -> IntervalHolder
{
   auto &clips = NarrowClips();
   if (clips.empty()) {
      auto pInterval = CreateWideClip(
         WaveTrackData::Get(*this).GetOrigin(), MakeNewClipName());
      InsertInterval(pInterval, true, true);
      return pInterval;
   }
   else {
      WaveClipHolder newClips[2];
      size_t iChannel = 0;
      const auto makeClip = [&](auto &theClips){
         auto end = theClips.end();
         auto it = theClips.begin();
         it = max_element(it, end,
            [](const auto &pClip1, const auto &pClip2){
               return pClip1->GetPlayStartTime() < pClip2->GetPlayStartTime();
         });
         assert(it != theClips.end());
         newClips[iChannel++] = *it;
      };
      makeClip(clips);
      if (NChannels() > 1)
         makeClip(RightClips());
      return std::make_shared<Interval>(*this, newClips[0], newClips[1]);
   }
}

// For internal purposes only
int WaveTrack::GetClipIndex(const Interval &clip) const
{
   int result = 0;
   const auto &&clips = Intervals();
   const auto test = [&](const auto &otherClip){
      bool match0 = clip.GetClip(0) == otherClip->GetClip(0);
      bool match1 = clip.GetClip(1) == otherClip->GetClip(1);
      if (match0) {
         assert(match1);
         return true;
      }
      return false;
   };
   auto begin = clips.begin(),
      end = clips.end(),
      iter = std::find_if(begin, end, test);
   return std::distance(begin, iter);
}

int WaveTrack::GetNumClips() const
{
   return NarrowClips().size();
}

bool WaveTrack::CanOffsetClips(
   const std::vector<Interval*> &movingClips,
   double amount,
   double *allowedAmount /* = NULL */)
{
   if (allowedAmount)
      *allowedAmount = amount;

   const auto &moving = [&](Interval *clip){
      // linear search might be improved, but expecting few moving clips
      // compared with the fixed clips
      // Don't use pointer identity of WaveTrack::Interval
      // TODO wide wave clips -- maybe change that
      const auto pred = [narrowClip = clip->GetClip(0).get()](auto *pInterval){
         return pInterval->GetClip(0).get() == narrowClip;
      };
      return movingClips.end() !=
         std::find_if(movingClips.begin(), movingClips.end(), pred);
   };

   for (const auto &c: Intervals()) {
      if ( moving( c.get() ) )
         continue;
      for (const auto clip : movingClips) {
         if (c->GetPlayStartTime() < clip->GetPlayEndTime() + amount &&
            c->GetPlayEndTime() > clip->GetPlayStartTime() + amount)
         {
            if (!allowedAmount)
               return false; // clips overlap

            if (amount > 0)
            {
               if (c->GetPlayStartTime() - clip->GetPlayEndTime() < *allowedAmount)
                  *allowedAmount = c->GetPlayStartTime() - clip->GetPlayEndTime();
               if (*allowedAmount < 0)
                  *allowedAmount = 0;
            } else
            {
               if (c->GetPlayEndTime() - clip->GetPlayStartTime() > *allowedAmount)
                  *allowedAmount = c->GetPlayEndTime() - clip->GetPlayStartTime();
               if (*allowedAmount > 0)
                  *allowedAmount = 0;
            }
         }
      }
   }

   if (allowedAmount)
   {
      if (*allowedAmount == amount)
         return true;

      // Check if the NEW calculated amount would not violate
      // any other constraint
      if (!CanOffsetClips(movingClips, *allowedAmount, nullptr)) {
         *allowedAmount = 0; // play safe and don't allow anything
         return false;
      }
      else
         return true;
   } else
      return true;
}

bool WaveTrack::CanInsertClip(
   const Interval& candidateClip, double& slideBy, double tolerance) const
{
   const auto &clips = Intervals();
   if (clips.empty())
      return true;
   // Find clip in this that overlaps most with `clip`:
   const auto candidateClipStartTime = candidateClip.GetPlayStartTime();
   const auto candidateClipEndTime = candidateClip.GetPlayEndTime();
   const auto t0 = SnapToSample(candidateClipStartTime + slideBy);
   const auto t1 = SnapToSample(candidateClipEndTime + slideBy);
   std::vector<double> overlaps;
   std::transform(
      clips.begin(), clips.end(), std::back_inserter(overlaps),
      [&](const auto& pClip) {
         return pClip->IntersectsPlayRegion(t0, t1) ?
                   std::min(pClip->GetPlayEndTime(), t1) -
                      std::max(pClip->GetPlayStartTime(), t0) :
                   0.0;
      });
   const auto maxOverlap = std::max_element(overlaps.begin(), overlaps.end());
   if (*maxOverlap > tolerance)
      return false;
   auto iter = clips.begin();
   std::advance(iter, std::distance(overlaps.begin(), maxOverlap));
   const auto overlappedClip = *iter;
   const auto requiredOffset =  slideBy +
             *maxOverlap * (overlappedClip->GetPlayStartTime() < t0 ? 1 : -1);
   // Brute-force check to see if there's another clip that'd be in the way.
   if (std::any_of(
          clips.begin(), clips.end(),
          [&](const auto& pClip)
          {
             const auto result = pClip->IntersectsPlayRegion(
                SnapToSample(candidateClipStartTime + requiredOffset),
                SnapToSample(candidateClipEndTime + requiredOffset));
             return result;
          }))
      return false;
   slideBy = requiredOffset;
   return true;
}

/*! @excsafety{Weak} */
void WaveTrack::Split(double t0, double t1)
{
   SplitAt(t0);
   if (t0 != t1)
      SplitAt(t1);
}

/*! @excsafety{Weak} */
auto WaveTrack::SplitAt(double t) -> std::pair<IntervalHolder, IntervalHolder>
{
   for (const auto &&c : Intervals()) {
      if (c->SplitsPlayRegion(t)) {
         t = SnapToSample(t);
         auto newClip = CopyClip(*c, true);
         c->TrimRightTo(t);// put t on a sample
         newClip->TrimLeftTo(t);
         auto result = std::pair{ c, newClip };

         // This could invalidate the iterators for the loop!  But we return
         // at once so it's okay
         InsertInterval(move(newClip), false); // transfer ownership
         return result;
      }
   }
   return {};
}

// Can't promise strong exception safety for a pair of tracks together
bool WaveTrack::MergeClips(int clipidx1, int clipidx2)
{
   const auto clip1 = GetWideClip(clipidx1);
   const auto clip2 = GetWideClip(clipidx2);

   if (!clip1 || !clip2)
      return false; // Don't throw, just do nothing.

   if (!clip1->HasEqualPitchAndSpeed(*clip2))
      return false;

   // Append data from second clip to first clip
   // use Strong-guarantee
   bool success = clip1->Paste(clip1->GetPlayEndTime(), *clip2);
   assert(success);  // assuming clips of the same track must have same width

   // use No-fail-guarantee for the rest
   // Delete second clip
   RemoveInterval(clip2);
   return true;
}

void WaveTrack::ApplyPitchAndSpeedOnIntervals(
   const IntervalHolders& srcIntervals,
   const ProgressReporter& reportProgress)
{
   IntervalHolders dstIntervals;
   dstIntervals.reserve(srcIntervals.size());
   std::transform(
      srcIntervals.begin(), srcIntervals.end(),
      std::back_inserter(dstIntervals), [&](const IntervalHolder& interval) {
         return interval->GetRenderedCopy(
            reportProgress, *this, mpFactory, GetSampleFormat());
      });

   // If we reach this point it means that no error was thrown - we can replace
   // the source with the destination intervals.
   for (auto i = 0; i < srcIntervals.size(); ++i)
      ReplaceInterval(srcIntervals[i], dstIntervals[i]);
}

namespace {
bool ClipsAreUnique(const WaveClipHolders &clips)
{
   // This is used only in assertions
   using Set = std::unordered_set<WaveClipHolder>;
   return clips.size() == Set{ clips.begin(), clips.end() }.size();
}
}

void WaveTrack::InsertInterval(const IntervalHolder& interval,
   bool newClip, bool allowEmpty)
{
   auto channel = 0;
   for (const auto pChannel : Channels()) {
      const auto clip = interval->GetClip(channel);
      if (clip) {
         pChannel->InsertClip(clip, newClip, false, allowEmpty);
         // Detect errors resulting in duplicate shared pointers to clips
         auto &clips = (channel == 0) ? mChannel.mClips : mRightChannel->mClips;
         assert(ClipsAreUnique(clips));
      }
      ++channel;
   }
}

void WaveTrack::RemoveInterval(const IntervalHolder& interval)
{
   const auto clips = Intervals();
   const auto begin = clips.begin();
   const auto pred = [pClip = interval->GetClip(0)](const auto &pInterval){
      return pInterval->GetClip(0) == pClip;
   };
   const auto iter = std::find_if(begin, clips.end(), pred);
   if (iter != clips.end()) {
      auto dist = std::distance(begin, iter);
      mChannel.RemoveClip(dist);
      if (NChannels() > 1)
         mRightChannel->RemoveClip(dist);
   }
}

void WaveTrack::ReplaceInterval(
   const IntervalHolder& oldOne, const IntervalHolder& newOne)
{
   assert(oldOne->NChannels() == newOne->NChannels());
   RemoveInterval(oldOne);
   InsertInterval(newOne, false);
   newOne->SetName(oldOne->GetName());
}

/*! @excsafety{Weak} -- Partial completion may leave clips at differing sample rates!
*/
void WaveTrack::Resample(int rate, BasicUI::ProgressDialog *progress)
{
   for (const auto &pClip : Intervals())
      pClip->Resample(rate, progress);
   DoSetRate(rate);
}

bool WaveTrack::SetFloats(const float *const *buffers,
   sampleCount start, size_t len, sampleFormat effectiveFormat)
{
   bool result = true;
   size_t ii = 0;
   for (const auto &pChannel : Channels()) {
      const auto buffer = buffers[ii++];
      assert(buffer); // precondition
      result = pChannel->SetFloats(buffer, start, len, effectiveFormat)
         && result;
   }
   return result;
}

namespace {
   template < typename Cont1, typename Cont2 >
   Cont1 FillSortedClipArray(const Cont2& mClips)
   {
      Cont1 clips;
      for (const auto &clip : mClips)
         clips.push_back(clip.get());
      std::sort(clips.begin(), clips.end(),
         [](const WaveClip *a, const WaveClip *b)
      { return a->GetPlayStartTime() < b->GetPlayStartTime(); });
      return clips;
   }
}

WaveClipPointers WaveTrack::SortedClipArray()
{
   return FillSortedClipArray<WaveClipPointers>(NarrowClips());
}

WaveClipConstPointers WaveTrack::SortedClipArray() const
{
   return FillSortedClipArray<WaveClipConstPointers>(NarrowClips());
}

auto WaveTrack::SortedIntervalArray() -> IntervalHolders
{
   const auto &intervals = Intervals();
   IntervalHolders result;
   copy(intervals.begin(), intervals.end(), back_inserter(result));
   sort(result.begin(), result.end(), [](const auto &pA, const auto &pB){
      return pA->GetPlayStartTime() < pB->GetPlayStartTime(); });
   return result;
}

auto WaveTrack::SortedIntervalArray() const -> IntervalConstHolders
{
   const auto &intervals = Intervals();
   IntervalConstHolders result;
   copy(intervals.begin(), intervals.end(), back_inserter(result));
   sort(result.begin(), result.end(), [](const auto &pA, const auto &pB){
      return pA->GetPlayStartTime() < pB->GetPlayStartTime(); });
   return result;
}

WaveClipHolders &WaveChannel::Clips()
{
   return mClips;
}

const WaveClipHolders &WaveChannel::Clips() const
{
   return const_cast<WaveChannel&>(*this).Clips();
}

void WaveTrack::ZipClips(bool mustAlign)
{
   const auto pOwner = GetOwner();
   assert(GetOwner()); // pre
   assert(NChannels() == 1); // pre

   // If deserializing, first un-link the track, so iterator finds the partner.
   SetLinkType(LinkType::None);

   auto iter = pOwner->Find(this);
   assert(this == *iter);
   ++iter;
   assert(iter != pOwner->end()); // pre
   auto pRight = dynamic_cast<WaveTrack*>(*iter);
   assert(pRight && pRight->NChannels() == 1); // pre

   //! Refuse if clips are not well aligned.
   if (mustAlign &&
      !AreAligned(this->SortedClipArray(), pRight->SortedClipArray()))
      return;

   // Still not actually "zipping" clips into single wide clip objects.
   // But there is now a real wide track object.
   mRightChannel.emplace(*this, std::move(pRight->mChannel));

   this->MergeChannelAttachments(std::move(*pRight));

   pOwner->Remove(*pRight);
}

static auto TrackFactoryFactory = []( AudacityProject &project ) {
   return std::make_shared< WaveTrackFactory >(
      ProjectRate::Get( project ),
      SampleBlockFactory::New( project ) );
};

static const AudacityProject::AttachedObjects::RegisteredFactory key2{
   TrackFactoryFactory
};

WaveTrackFactory &WaveTrackFactory::Get( AudacityProject &project )
{
   return project.AttachedObjects::Get< WaveTrackFactory >( key2 );
}

const WaveTrackFactory &WaveTrackFactory::Get( const AudacityProject &project )
{
   return Get( const_cast< AudacityProject & >( project ) );
}

WaveTrackFactory &WaveTrackFactory::Reset( AudacityProject &project )
{
   auto result = TrackFactoryFactory( project );
   project.AttachedObjects::Assign( key2, result );
   return *result;
}

void WaveTrackFactory::Destroy( AudacityProject &project )
{
   project.AttachedObjects::Assign( key2, nullptr );
}

StringSetting AudioTrackNameSetting{
   L"/GUI/TrackNames/DefaultTrackName",
   // Computed default value depends on chosen language
   []{ return DefaultName.Translation(); }
};

// Bug 825 is essentially that SyncLock requires EditClipsCanMove.
// SyncLock needs rethinking, but meanwhile this function
// fixes the issues of Bug 825 by allowing clips to move when in
// SyncLock.
bool GetEditClipsCanMove()
{
   bool mIsSyncLocked = SyncLockTracks.Read();
   if( mIsSyncLocked )
      return true;
   bool editClipsCanMove;
   return EditClipsCanMove.Read();
}

BoolSetting EditClipsCanMove{
   L"/GUI/EditClipCanMove",         false  };

DEFINE_XML_METHOD_REGISTRY( WaveTrackIORegistry );
