#pragma once

#include "AudioSegment.h"
#include "WaveTrack.h"

#include <optional>

class STRETCHING_SAMPLE_TRACK_API StretchingSampleTrack final :
    public SampleTrack
{
public:
   StretchingSampleTrack(std::shared_ptr<const WaveTrack>, double t0);

   // Track
   ConstIntervals GetIntervals() const override;
   ChannelType GetChannel() const override;
   bool GetErrorOpening() const override;
   Holder PasteInto(AudacityProject& project) const override;
   double GetOffset() const override;
   Holder Copy(double t0, double t1, bool forClipboard) const override;
   Holder Clone() const override;
   double GetStartTime() const override;
   double GetEndTime() const override;

   // SampleTrack
   sampleFormat GetSampleFormat() const override;
   ChannelType GetChannelIgnoringPan() const override;
   double GetRate() const override;
   sampleFormat WidestEffectiveFormat() const override;
   bool HasTrivialEnvelope() const override;
   void GetEnvelopeValues(
      double* buffer, size_t bufferLen, double t0) const override;
   float GetChannelGain(int channel) const override;
   size_t GetBestBlockSize(sampleCount t) const override;
   size_t GetMaxBlockSize() const override;
   sampleCount GetBlockStart(sampleCount t) const override;
   bool Get(
      samplePtr buffer, sampleFormat format, sampleCount start, size_t len,
      fillFormat fill = fillZero, bool mayThrow = true,
      sampleCount* pNumWithinClips = nullptr) const override;

   // XMLTagHandler
   void WriteXML(XMLWriter& xmlFile) const override;

   // Forbidden because non-const ; this class is only meant for playback usage,
   // which manipulates a const WaveTrack.
private:
   Intervals GetIntervals() override;
   void SetOffset(double) override;
   void SetPan(float);
   void SetPanFromChannelType();
   void SyncLockAdjust(double, double) override;
   Holder Cut(double t0, double t1) override;
   void Clear(double t0, double t1) override;
   void Paste(double t0, const Track* src) override;
   void Silence(double t0, double t1) override;
   void InsertSilence(double t0, double t1) override;
   bool HandleXMLTag(
      const std::string_view& tag, const AttributesList& attrs) override;
   void HandleXMLEndTag(const std::string_view& tag) override;
   XMLTagHandler* HandleXMLChild(const std::string_view& tag) override;

   // Private methods
private:
   void Reposition(double t);
   bool GetFloats(
      float* const* buffer, size_t numChannels, size_t samplesPerChannel);

   const std::shared_ptr<const WaveTrack> mWaveTrack;
   const WaveClipHolders mWaveClips;
   using AudioSegmentVector = std::vector<std::shared_ptr<AudioSegment>>;
   AudioSegmentVector mAudioSegments;
   AudioSegmentVector::const_iterator mActiveAudioSegmentIt =
      mAudioSegments.end();
   sampleCount mLastStartRequest = 0u;
};
