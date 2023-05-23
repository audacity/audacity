#include "CachingSequence.h"
#include "Sequence.h"
#include "SequenceSampleCache.h"

using namespace std::placeholders;

CachingSequence::CachingSequence(
   const SampleBlockFactoryPtr& pFactory, SampleFormats formats)
    : mSequence(new Sequence(pFactory, formats))
{
}

CachingSequence::CachingSequence(
   const CachingSequence& orig, const SampleBlockFactoryPtr& pFactory)
    : mSequence(new Sequence(*orig.mSequence, pFactory))
{
}

CachingSequence::CachingSequence(const CachingSequence& orig)
    : mSequence(new Sequence(*orig.mSequence, orig.GetFactory()))
{
}

sampleCount CachingSequence::GetNumSamples() const
{
   return mSequence->GetNumSamples();
}

bool CachingSequence::Get(
   samplePtr buffer, sampleFormat format, sampleCount start, size_t len,
   bool mayThrow) const
{
   if (!mCache)
   {
      const_cast<std::unique_ptr<SequenceSampleCache>&>(mCache) =
         std::make_unique<SequenceSampleCache>(
            [this](
               samplePtr buffer, sampleFormat format, sampleCount start,
               size_t len, bool mayThrow) {
               return mSequence->Get(buffer, format, start, len, mayThrow);
            },
            mSequence->GetNumSamples(), mSequence->GetMaxBlockSize());
   }
   return mCache->Get(buffer, format, start, len, mayThrow);
}

void CachingSequence::SetSamples(
   constSamplePtr buffer, sampleFormat format, sampleCount start,
   sampleCount len, sampleFormat effectiveFormat)
{
   mCache.reset();
   return mSequence->SetSamples(buffer, format, start, len, effectiveFormat);
}

std::unique_ptr<SequenceInterface> CachingSequence::Copy(
   const SampleBlockFactoryPtr& pFactory, sampleCount s0, sampleCount s1) const
{
   return std::unique_ptr<CachingSequence>(
      new CachingSequence(std::unique_ptr<Sequence> { static_cast<Sequence*>(
         mSequence->Copy(pFactory, s0, s1).release()) }));
}

void CachingSequence::Paste(sampleCount s0, const SequenceInterface* src)
{
   mCache.reset();
   const Sequence* sequence = nullptr;
   if (auto cachingSequence = dynamic_cast<const CachingSequence*>(src))
   {
      sequence = cachingSequence->mSequence.get();
   }
   else if (auto straightSequence = dynamic_cast<const Sequence*>(src))
   {
      sequence = straightSequence;
   }
   else
   {
      assert(false);
   }
   return mSequence->Paste(s0, sequence);
}

size_t CachingSequence::GetIdealAppendLen() const
{
   return mSequence->GetIdealAppendLen();
}

bool CachingSequence::Append(
   constSamplePtr buffer, sampleFormat format, size_t len, size_t stride,
   sampleFormat effectiveFormat)
{
   mCache.reset();
   return mSequence->Append(buffer, format, len, stride, effectiveFormat);
}

void CachingSequence::Flush()
{
   mCache.reset();
   return mSequence->Flush();
}

SeqBlock::SampleBlockPtr CachingSequence::AppendNewBlock(
   constSamplePtr buffer, sampleFormat format, size_t len)
{
   mCache.reset();
   return mSequence->AppendNewBlock(buffer, format, len);
}

void CachingSequence::AppendSharedBlock(const SeqBlock::SampleBlockPtr& pBlock)
{
   mCache.reset();
   return mSequence->AppendSharedBlock(pBlock);
}

void CachingSequence::Delete(sampleCount start, sampleCount len)
{
   mCache.reset();
   return mSequence->Delete(start, len);
}

void CachingSequence::SetSilence(sampleCount s0, sampleCount len)
{
   mCache.reset();
   return mSequence->SetSilence(s0, len);
}

void CachingSequence::InsertSilence(sampleCount s0, sampleCount len)
{
   mCache.reset();
   return mSequence->InsertSilence(s0, len);
}

const SampleBlockFactoryPtr& CachingSequence::GetFactory() const
{
   return mSequence->GetFactory();
}

bool CachingSequence::GetErrorOpening() const
{
   return mSequence->GetErrorOpening();
}

bool CachingSequence::CloseLock()
{
   mCache.reset();
   return mSequence->CloseLock();
}

SampleFormats CachingSequence::GetSampleFormats() const
{
   return mSequence->GetSampleFormats();
}

bool CachingSequence::ConvertToSampleFormat(
   sampleFormat format, const std::function<void(size_t)>& progressReport)
{
   mCache.reset();
   return mSequence->ConvertToSampleFormat(format, progressReport);
}

std::pair<float, float> CachingSequence::GetMinMax(
   sampleCount start, sampleCount len, bool mayThrow) const
{
   return mSequence->GetMinMax(start, len, mayThrow);
}

float CachingSequence::GetRMS(
   sampleCount start, sampleCount len, bool mayThrow) const
{
   return mSequence->GetRMS(start, len, mayThrow);
}

size_t CachingSequence::GetMaxBlockSize() const
{
   return mSequence->GetMaxBlockSize();
}

const BlockArray& CachingSequence::GetBlockArray() const
{
   return mSequence->GetBlockArray();
}

size_t CachingSequence::GetAppendBufferLen() const
{
   return mSequence->GetAppendBufferLen();
}

constSamplePtr CachingSequence::GetAppendBuffer() const
{
   return mSequence->GetAppendBuffer();
}

int CachingSequence::FindBlock(sampleCount pos) const
{
   return mSequence->FindBlock(pos);
}

void CachingSequence::ConsistencyCheck(
   const wxChar* whereStr, bool mayThrow) const
{
   return mSequence->ConsistencyCheck(whereStr, mayThrow);
}

bool CachingSequence::HandleXMLTag(
   const std::string_view& tag, const AttributesList& attrs)
{
   mCache.reset();
   return mSequence->HandleXMLTag(tag, attrs);
}

void CachingSequence::HandleXMLEndTag(const std::string_view& tag)
{
   mCache.reset();
   return mSequence->HandleXMLEndTag(tag);
}

XMLTagHandler* CachingSequence::HandleXMLChild(const std::string_view& tag)
{
   mCache.reset();
   return mSequence->HandleXMLChild(tag);
}

void CachingSequence::WriteXML(XMLWriter& xmlFile) const
{
   return mSequence->WriteXML(xmlFile);
}

CachingSequence::CachingSequence(std::unique_ptr<Sequence> sequence)
    : mSequence(std::move(sequence))
{
}
