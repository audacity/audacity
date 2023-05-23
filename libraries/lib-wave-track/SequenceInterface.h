#pragma once

#include "SampleBlock.h"
#include "SampleCount.h"
#include "SampleFormat.h"
#include "XMLTagHandler.h"

// This is an internal data structure!  For advanced use only.
class SeqBlock
{
public:
   using SampleBlockPtr = std::shared_ptr<SampleBlock>;
   SampleBlockPtr sb;
   /// the sample in the global wavetrack that this block starts at.
   sampleCount start;

   SeqBlock()
       : sb {}
       , start(0)
   {
   }

   SeqBlock(const SampleBlockPtr& sb_, sampleCount start_)
       : sb(sb_)
       , start(start_)
   {
   }

   // Construct a SeqBlock with changed start, same file
   SeqBlock Plus(sampleCount delta) const
   {
      return SeqBlock(sb, start + delta);
   }
};

class BlockArray : public std::vector<SeqBlock>
{
};

using BlockPtrArray = std::vector<SeqBlock*>; // non-owning pointers

class WAVE_TRACK_API SequenceInterface : public XMLTagHandler
{
public:
   virtual ~SequenceInterface() = default;

   virtual sampleCount GetNumSamples() const = 0;

   virtual bool Get(
      samplePtr buffer, sampleFormat format, sampleCount start, size_t len,
      bool mayThrow) const = 0;

   virtual void SetSamples(
      constSamplePtr buffer, sampleFormat format, sampleCount start,
      sampleCount len, sampleFormat effectiveFormat) = 0;

   //  virtual std::unique_ptr<SequenceInterface> Clone() const = 0;

   virtual std::unique_ptr<SequenceInterface> Copy(
      const SampleBlockFactoryPtr& pFactory, sampleCount s0,
      sampleCount s1) const = 0;

   virtual void Paste(sampleCount s0, const SequenceInterface* src) = 0;

   virtual size_t GetIdealAppendLen() const = 0;

   virtual bool Append(
      constSamplePtr buffer, sampleFormat format, size_t len, size_t stride,
      sampleFormat effectiveFormat) = 0;

   virtual void Flush() = 0;

   virtual SeqBlock::SampleBlockPtr
   AppendNewBlock(constSamplePtr buffer, sampleFormat format, size_t len) = 0;

   virtual void AppendSharedBlock(const SeqBlock::SampleBlockPtr& pBlock) = 0;

   virtual void Delete(sampleCount start, sampleCount len) = 0;

   virtual void SetSilence(sampleCount s0, sampleCount len) = 0;

   virtual void InsertSilence(sampleCount s0, sampleCount len) = 0;

   virtual const SampleBlockFactoryPtr& GetFactory() const = 0;

   virtual bool GetErrorOpening() const = 0;

   virtual bool CloseLock() = 0;

   virtual SampleFormats GetSampleFormats() const = 0;

   virtual bool ConvertToSampleFormat(
      sampleFormat format,
      const std::function<void(size_t)>& progressReport = {}) = 0;

   virtual std::pair<float, float>
   GetMinMax(sampleCount start, sampleCount len, bool mayThrow) const = 0;
   virtual float
   GetRMS(sampleCount start, sampleCount len, bool mayThrow) const = 0;

   virtual size_t GetMaxBlockSize() const = 0;

   virtual const BlockArray& GetBlockArray() const = 0;

   virtual size_t GetAppendBufferLen() const = 0;

   virtual constSamplePtr GetAppendBuffer() const = 0;

   virtual int FindBlock(sampleCount pos) const = 0;

   virtual void
   ConsistencyCheck(const wxChar* whereStr, bool mayThrow = true) const = 0;

   virtual void WriteXML(XMLWriter& xmlFile) const = 0;
};
