#include "StretchingSampleTrack.h"

#include "SilenceSegment.h"
#include "WaveClip.h"
#include "WaveClipSegment.h"

namespace
{
WaveClipHolders GetSortedWaveClips(const WaveTrack& track)
{
   auto clips = WaveClipHolders { track.GetClips() };
   std::sort(
      clips.begin(), clips.end(),
      [](const WaveClipHolder& a, const WaveClipHolder& b) {
         return a->GetPlayStartTime() < b->GetPlayStartTime();
      });
   return clips;
}
} // namespace

StretchingSampleTrack::StretchingSampleTrack(
   std::shared_ptr<const WaveTrack> waveTrack, double t0)
    : SampleTrack(*waveTrack, Track::ProtectedCreationArg {})
    , mWaveTrack(std::move(waveTrack))
    , mWaveClips(GetSortedWaveClips(*mWaveTrack))
{
   this->PlayableTrack::Init(*mWaveTrack);
   Reposition(t0);
}

void StretchingSampleTrack::Reposition(double t)
{
   mAudioSegments.clear();
   const auto firstClipToPlayIt = std::upper_bound(
      mWaveClips.begin(), mWaveClips.end(), t,
      [this](double t, const WaveClipHolder& clip) {
         return t < clip->GetPlayEndTime();
      });
   for (auto it = firstClipToPlayIt; it != mWaveClips.end(); ++it)
   {
      const auto& clip = *it;
      const auto clipStartTime = clip->GetPlayStartTime();
      if (clipStartTime > t)
      {
         const auto numSilenceSamples =
            sampleCount { (clipStartTime - t) * mWaveTrack->GetRate() + .5 };
         auto segment = std::make_shared<SilenceSegment>(numSilenceSamples);
         auto& processor = segment->GetProcessor();
         processor.SetOffsetFromPlayStartTime(0.0);
         mAudioSegments.push_back(std::move(segment));
      }
      auto segment = std::make_shared<WaveClipSegment>(clip);
      auto& processor = segment->GetProcessor();
      processor.SetOffsetFromPlayStartTime(t - clipStartTime);
      mAudioSegments.push_back(segment);
      t = clip->GetPlayEndTime();
   }
   mActiveAudioSegmentIt = mAudioSegments.begin();
}

Track::Intervals StretchingSampleTrack::GetIntervals()
{
   assert(false); // Unexpected call on non-const method.
   return {};
}

Track::ConstIntervals StretchingSampleTrack::GetIntervals() const
{
   return const_cast<const WaveTrack*>(mWaveTrack.get())->GetIntervals();
}

Track::ChannelType StretchingSampleTrack::GetChannel() const
{
   return mWaveTrack->GetChannel();
}

void StretchingSampleTrack::SetOffset(double o)
{
   assert(false); // Unexpected call on non-const method.
}

void StretchingSampleTrack::SetPan(float newPan)
{
   assert(false); // Unexpected call on non-const method.
}

void StretchingSampleTrack::SetPanFromChannelType()
{
}

void StretchingSampleTrack::SyncLockAdjust(double oldT1, double newT1)
{
   assert(false); // Unexpected call on non-const method.
}

bool StretchingSampleTrack::GetErrorOpening() const
{
   return mWaveTrack->GetErrorOpening();
}

Track::Holder StretchingSampleTrack::PasteInto(AudacityProject& project) const
{
   return mWaveTrack->PasteInto(project);
}

double StretchingSampleTrack::GetOffset() const
{
   return mWaveTrack->GetOffset();
}

Track::Holder StretchingSampleTrack::Cut(double t0, double t1)
{
   assert(false); // Unexpected call on non-const method.
   return nullptr;
}

Track::Holder
StretchingSampleTrack::Copy(double t0, double t1, bool forClipboard) const
{
   return mWaveTrack->Copy(t0, t1, forClipboard);
}

void StretchingSampleTrack::Clear(double t0, double t1)
{
   assert(false); // Unexpected call on non-const method.
}

void StretchingSampleTrack::Paste(double t0, const Track* src)
{
   assert(false); // Unexpected call on non-const method.
}

void StretchingSampleTrack::Silence(double t0, double t1)
{
   assert(false); // Unexpected call on non-const method.
}

void StretchingSampleTrack::InsertSilence(double t0, double t1)
{
   assert(false); // Unexpected call on non-const method.
}

Track::Holder StretchingSampleTrack::Clone() const
{
   return mWaveTrack->Clone();
}

double StretchingSampleTrack::GetStartTime() const
{
   return mWaveTrack->GetStartTime();
}

double StretchingSampleTrack::GetEndTime() const
{
   return mWaveTrack->GetEndTime();
}

bool StretchingSampleTrack::GetMute() const
{
   return mWaveTrack->GetMute();
}

bool StretchingSampleTrack::GetSolo() const
{
   return mWaveTrack->GetSolo();
}

bool StretchingSampleTrack::GetNotMute() const
{
   return mWaveTrack->GetNotMute();
}

bool StretchingSampleTrack::GetNotSolo() const
{
   return mWaveTrack->GetNotSolo();
}

sampleFormat StretchingSampleTrack::GetSampleFormat() const
{
   return mWaveTrack->GetSampleFormat();
}

Track::ChannelType StretchingSampleTrack::GetChannelIgnoringPan() const
{
   return mWaveTrack->GetChannelIgnoringPan();
}

double StretchingSampleTrack::GetRate() const
{
   return mWaveTrack->GetRate();
}

sampleFormat StretchingSampleTrack::WidestEffectiveFormat() const
{
   return mWaveTrack->WidestEffectiveFormat();
}

bool StretchingSampleTrack::HasTrivialEnvelope() const
{
   return mWaveTrack->HasTrivialEnvelope();
}

void StretchingSampleTrack::GetEnvelopeValues(
   double* buffer, size_t bufferLen, double t0) const
{
   return mWaveTrack->GetEnvelopeValues(buffer, bufferLen, t0);
}

float StretchingSampleTrack::GetChannelGain(int channel) const
{
   return mWaveTrack->GetChannelGain(channel);
}

size_t StretchingSampleTrack::GetBestBlockSize(sampleCount t) const
{
   return mWaveTrack->GetBestBlockSize(t);
}

size_t StretchingSampleTrack::GetMaxBlockSize() const
{
   return mWaveTrack->GetMaxBlockSize();
}

sampleCount StretchingSampleTrack::GetBlockStart(sampleCount t) const
{
   return mWaveTrack->GetBlockStart(t);
}

namespace
{
std::vector<float*>
GetOffsetBuffer(float* const* buffer, size_t numChannels, size_t offset)
{
   std::vector<float*> offsetBuffer(numChannels);
   for (auto i = 0u; i < numChannels; ++i)
   {
      offsetBuffer[i] = buffer[i] + offset;
   }
   return offsetBuffer;
}
} // namespace

bool StretchingSampleTrack::GetFloats(
   float* const* buffer, size_t numChannels, size_t samplesPerChannel)
{
   auto numProcessedSamples = 0u;
   while (numProcessedSamples < samplesPerChannel &&
          mActiveAudioSegmentIt != mAudioSegments.end())
   {
      const auto& segment = *mActiveAudioSegmentIt;
      AudioSegmentProcessor& processor = segment->GetProcessor();
      const auto offsetBuffer =
         GetOffsetBuffer(buffer, numChannels, numProcessedSamples);
      numProcessedSamples += processor.Process(
         offsetBuffer.data(), numChannels,
         samplesPerChannel - numProcessedSamples);
      if (!processor.SamplesRemaining())
      {
         ++mActiveAudioSegmentIt;
      }
   }
   if (numProcessedSamples == 0u)
   {
      return false;
   }
   const auto remaining = samplesPerChannel - numProcessedSamples;
   if (remaining > 0u)
   {
      const auto offsetBuffer =
         GetOffsetBuffer(buffer, numChannels, numProcessedSamples);
      for (auto i = 0u; i < numChannels; ++i)
      {
         std::fill(offsetBuffer[i], offsetBuffer[i] + remaining, 0.f);
      }
   }
   return true;
}

bool StretchingSampleTrack::Get(
   samplePtr buffer, sampleFormat format, sampleCount start, size_t len,
   fillFormat fill, bool mayThrow, sampleCount* pNumWithinClips) const
{
   assert(format == floatSample);
   if (format != floatSample)
   {
      THROW_INCONSISTENCY_EXCEPTION;
   }
   auto& mutableThis = const_cast<StretchingSampleTrack&>(*this);
   // Detect looping
   // todo(mhodgkinson) improve
   if (start < mLastStartRequest)
   {
      const auto t = start.as_double() / mWaveTrack->GetRate();
      mutableThis.Reposition(t);
   }
   mutableThis.mLastStartRequest = start;
   return mutableThis.GetFloats(
      reinterpret_cast<float* const*>(&buffer), 1u, len);
}

bool StretchingSampleTrack::HandleXMLTag(
   const std::string_view& tag, const AttributesList& attrs)
{
   assert(false); // Unexpected call on non-const method.
   return false;
}

void StretchingSampleTrack::HandleXMLEndTag(const std::string_view& tag)
{
   assert(false); // Unexpected call on non-const method.
}

XMLTagHandler*
StretchingSampleTrack::HandleXMLChild(const std::string_view& tag)
{
   assert(false); // Unexpected call on non-const method.
   return nullptr;
}

void StretchingSampleTrack::WriteXML(XMLWriter& xmlFile) const
{
   return mWaveTrack->WriteXML(xmlFile);
}
