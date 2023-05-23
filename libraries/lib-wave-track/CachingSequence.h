#pragma once

#include "Sequence.h"
#include "SequenceInterface.h"
#include "SequenceSampleCache.h"

class WAVE_TRACK_API CachingSequence final : public SequenceInterface
{
public:
   CachingSequence(
      const SampleBlockFactoryPtr& pFactory, SampleFormats formats);

   //! Use if orig lives in the same project
   CachingSequence(const CachingSequence& orig);

   //! Use if orig may be copies from another project
   CachingSequence(
      const CachingSequence& orig, const SampleBlockFactoryPtr& pFactory);

   ~CachingSequence() = default;

   sampleCount GetNumSamples() const override;

   bool Get(
      samplePtr buffer, sampleFormat format, sampleCount start, size_t len,
      bool mayThrow) const override;

   void SetSamples(
      constSamplePtr buffer, sampleFormat format, sampleCount start,
      sampleCount len, sampleFormat effectiveFormat) override;

   std::unique_ptr<SequenceInterface> Copy(
      const SampleBlockFactoryPtr& pFactory, sampleCount s0,
      sampleCount s1) const override;

   void Paste(sampleCount s0, const SequenceInterface* src) override;

   size_t GetIdealAppendLen() const override;

   bool Append(
      constSamplePtr buffer, sampleFormat format, size_t len, size_t stride,
      sampleFormat effectiveFormat) override;

   void Flush() override;

   SeqBlock::SampleBlockPtr AppendNewBlock(
      constSamplePtr buffer, sampleFormat format, size_t len) override;

   void AppendSharedBlock(const SeqBlock::SampleBlockPtr& pBlock) override;

   void Delete(sampleCount start, sampleCount len) override;

   void SetSilence(sampleCount s0, sampleCount len) override;

   void InsertSilence(sampleCount s0, sampleCount len) override;

   const SampleBlockFactoryPtr& GetFactory() const override;

   bool GetErrorOpening() const override;

   bool CloseLock() override;

   SampleFormats GetSampleFormats() const override;

   bool ConvertToSampleFormat(
      sampleFormat format,
      const std::function<void(size_t)>& progressReport = {}) override;

   std::pair<float, float>
   GetMinMax(sampleCount start, sampleCount len, bool mayThrow) const override;
   float
   GetRMS(sampleCount start, sampleCount len, bool mayThrow) const override;

   size_t GetMaxBlockSize() const override;

   const BlockArray& GetBlockArray() const override;

   size_t GetAppendBufferLen() const override;

   constSamplePtr GetAppendBuffer() const override;

   int FindBlock(sampleCount pos) const override;

   void ConsistencyCheck(
      const wxChar* whereStr, bool mayThrow = true) const override;

   bool HandleXMLTag(
      const std::string_view& tag, const AttributesList& attrs) override;

   void HandleXMLEndTag(const std::string_view& tag) override;

   XMLTagHandler* HandleXMLChild(const std::string_view& tag) override;

   void WriteXML(XMLWriter& xmlFile) const override;

private:
   CachingSequence(std::unique_ptr<Sequence>);

   const std::unique_ptr<Sequence> mSequence;
   std::unique_ptr<SequenceSampleCache> mCache;
};
