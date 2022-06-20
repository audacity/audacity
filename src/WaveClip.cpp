/**********************************************************************

  Audacity: A Digital Audio Editor

  WaveClip.cpp

  ?? Dominic Mazzoni
  ?? Markus Meyer

*******************************************************************//**

\class WaveClip
\brief This allows multiple clips to be a part of one WaveTrack.

*//*******************************************************************/

#include "WaveClip.h"



#include <math.h>
#include <vector>
#include <wx/log.h>

#include "BasicUI.h"
#include "Sequence.h"
#include "Prefs.h"
#include "Envelope.h"
#include "Resample.h"
#include "InconsistencyException.h"
#include "UserException.h"

#include "prefs/SpectrogramSettings.h"

#ifdef _OPENMP
#include <omp.h>
#endif

WaveClipListener::~WaveClipListener()
{
}

WaveClip::WaveClip(const SampleBlockFactoryPtr &factory,
                   sampleFormat format, int rate, int colourIndex)
{
   mRate = rate;
   mColourIndex = colourIndex;
   mSequence = std::make_unique<Sequence>(factory, format);

   mEnvelope = std::make_unique<Envelope>(true, 1e-7, 2.0, 1.0);
}

WaveClip::WaveClip(const WaveClip& orig,
                   const SampleBlockFactoryPtr &factory,
                   bool copyCutlines)
{
   // essentially a copy constructor - but you must pass in the
   // current sample block factory, because we might be copying
   // from one project to another

   mSequenceOffset = orig.mSequenceOffset;
   mTrimLeft = orig.mTrimLeft;
   mTrimRight = orig.mTrimRight;
   mRate = orig.mRate;
   mColourIndex = orig.mColourIndex;
   mSequence = std::make_unique<Sequence>(*orig.mSequence, factory);

   mEnvelope = std::make_unique<Envelope>(*orig.mEnvelope);

   mName = orig.mName;

   if ( copyCutlines )
      for (const auto &clip: orig.mCutLines)
         mCutLines.push_back
            ( std::make_unique<WaveClip>( *clip, factory, true ) );

   mIsPlaceholder = orig.GetIsPlaceholder();
}

WaveClip::WaveClip(const WaveClip& orig,
                   const SampleBlockFactoryPtr &factory,
                   bool copyCutlines,
                   double t0, double t1)
{
   // Copy only a range of the other WaveClip

   mSequenceOffset = orig.mSequenceOffset;
   mTrimLeft = orig.mTrimLeft + (t0 > orig.GetPlayStartTime()? t0 - orig.GetPlayStartTime() : 0);
   mTrimRight = orig.mTrimRight + (t1 < orig.GetPlayEndTime()? orig.GetPlayEndTime() - t1 : 0);
   
   mRate = orig.mRate;
   mColourIndex = orig.mColourIndex;

   mIsPlaceholder = orig.GetIsPlaceholder();

   auto s0 = orig.TimeToSequenceSamples(t0);
   auto s1 = orig.TimeToSequenceSamples(t1);

   mSequence = std::make_unique<Sequence>(*orig.mSequence, factory);

   mEnvelope = std::make_unique<Envelope>(*orig.mEnvelope);

   if ( copyCutlines )
      // Copy cutline clips that fall in the range
      for (const auto &ppClip : orig.mCutLines)
      {
         const WaveClip* clip = ppClip.get();
         double cutlinePosition = orig.GetSequenceStartTime() + clip->GetSequenceStartTime();
         if (cutlinePosition >= t0 && cutlinePosition <= t1)
         {
            auto newCutLine =
               std::make_unique< WaveClip >( *clip, factory, true );
            newCutLine->SetSequenceStartTime( cutlinePosition - t0 );
            mCutLines.push_back(std::move(newCutLine));
         }
      }
}


WaveClip::~WaveClip()
{
}

bool WaveClip::GetSamples(samplePtr buffer, sampleFormat format,
                   sampleCount start, size_t len, bool mayThrow) const
{
   return mSequence->Get(buffer, format, start + TimeToSamples(mTrimLeft), len, mayThrow);
}

/*! @excsafety{Strong} */
void WaveClip::SetSamples(constSamplePtr buffer, sampleFormat format,
                   sampleCount start, size_t len)
{
   // use Strong-guarantee
   mSequence->SetSamples(buffer, format, start + TimeToSamples(mTrimLeft), len);

   // use No-fail-guarantee
   MarkChanged();
}

BlockArray* WaveClip::GetSequenceBlockArray()
{
   return &mSequence->GetBlockArray();
}

const BlockArray* WaveClip::GetSequenceBlockArray() const
{
   return &mSequence->GetBlockArray();
}

void WaveClip::MarkChanged() // NOFAIL-GUARANTEE
{
   Caches::ForEach( std::mem_fn( &WaveClipListener::MarkChanged ) );
}

std::pair<float, float> WaveClip::GetMinMax(
   double t0, double t1, bool mayThrow) const
{
   if (t0 > t1) {
      if (mayThrow)
         THROW_INCONSISTENCY_EXCEPTION;
      return {
         0.f,  // harmless, but unused since Sequence::GetMinMax does not use these values
         0.f   // harmless, but unused since Sequence::GetMinMax does not use these values
      };
   }

   if (t0 == t1)
      return{ 0.f, 0.f };

   auto s0 = TimeToSequenceSamples(t0);
   auto s1 = TimeToSequenceSamples(t1);

   return mSequence->GetMinMax(s0, s1-s0, mayThrow);
}

float WaveClip::GetRMS(double t0, double t1, bool mayThrow) const
{
   if (t0 > t1) {
      if (mayThrow)
         THROW_INCONSISTENCY_EXCEPTION;
      return 0.f;
   }

   if (t0 == t1)
      return 0.f;

   auto s0 = TimeToSequenceSamples(t0);
   auto s1 = TimeToSequenceSamples(t1);

   return mSequence->GetRMS(s0, s1-s0, mayThrow);
}

void WaveClip::ConvertToSampleFormat(sampleFormat format,
   const std::function<void(size_t)> & progressReport)
{
   // Note:  it is not necessary to do this recursively to cutlines.
   // They get converted as needed when they are expanded.

   auto bChanged = mSequence->ConvertToSampleFormat(format, progressReport);
   if (bChanged)
      MarkChanged();
}

/*! @excsafety{No-fail} */
void WaveClip::UpdateEnvelopeTrackLen()
{
   auto len = (mSequence->GetNumSamples().as_double()) / mRate;
   if (len != mEnvelope->GetTrackLen())
      mEnvelope->SetTrackLen(len, 1.0 / GetRate());
}

/*! @excsafety{Strong} */
std::shared_ptr<SampleBlock> WaveClip::AppendNewBlock(
   samplePtr buffer, sampleFormat format, size_t len)
{
   return mSequence->AppendNewBlock( buffer, format, len );
}

/*! @excsafety{Strong} */
void WaveClip::AppendSharedBlock(const std::shared_ptr<SampleBlock> &pBlock)
{
   mSequence->AppendSharedBlock( pBlock );
}

/*! @excsafety{Partial}
 -- Some prefix (maybe none) of the buffer is appended,
and no content already flushed to disk is lost. */
bool WaveClip::Append(constSamplePtr buffer, sampleFormat format,
                      size_t len, unsigned int stride)
{
   //wxLogDebug(wxT("Append: len=%lli"), (long long) len);
   bool result = false;

   auto maxBlockSize = mSequence->GetMaxBlockSize();
   auto blockSize = mSequence->GetIdealAppendLen();
   sampleFormat seqFormat = mSequence->GetSampleFormat();

   if (!mAppendBuffer.ptr())
      mAppendBuffer.Allocate(maxBlockSize, seqFormat);

   auto cleanup = finally( [&] {
      // use No-fail-guarantee
      UpdateEnvelopeTrackLen();
      MarkChanged();
   } );

   for(;;) {
      if (mAppendBufferLen >= blockSize) {
         // flush some previously appended contents
         // use Strong-guarantee
         mSequence->Append(mAppendBuffer.ptr(), seqFormat, blockSize);
         result = true;

         // use No-fail-guarantee for rest of this "if"
         memmove(mAppendBuffer.ptr(),
                 mAppendBuffer.ptr() + blockSize * SAMPLE_SIZE(seqFormat),
                 (mAppendBufferLen - blockSize) * SAMPLE_SIZE(seqFormat));
         mAppendBufferLen -= blockSize;
         blockSize = mSequence->GetIdealAppendLen();
      }

      if (len == 0)
         break;

      // use No-fail-guarantee for rest of this "for"
      wxASSERT(mAppendBufferLen <= maxBlockSize);
      auto toCopy = std::min(len, maxBlockSize - mAppendBufferLen);

      CopySamples(buffer, format,
                  mAppendBuffer.ptr() + mAppendBufferLen * SAMPLE_SIZE(seqFormat),
                  seqFormat,
                  toCopy,
                  gHighQualityDither,
                  stride);

      mAppendBufferLen += toCopy;
      buffer += toCopy * SAMPLE_SIZE(format) * stride;
      len -= toCopy;
   }

   return result;
}

/*! @excsafety{Mixed} */
/*! @excsafety{No-fail} -- The clip will be in a flushed state. */
/*! @excsafety{Partial}
-- Some initial portion (maybe none) of the append buffer of the
clip gets appended; no previously flushed contents are lost. */
void WaveClip::Flush()
{
   //wxLogDebug(wxT("WaveClip::Flush"));
   //wxLogDebug(wxT("   mAppendBufferLen=%lli"), (long long) mAppendBufferLen);
   //wxLogDebug(wxT("   previous sample count %lli"), (long long) mSequence->GetNumSamples());

   if (mAppendBufferLen > 0) {

      auto cleanup = finally( [&] {
         // Blow away the append buffer even in case of failure.  May lose some
         // data but don't leave the track in an un-flushed state.

         // Use No-fail-guarantee of these steps.
         mAppendBufferLen = 0;
         UpdateEnvelopeTrackLen();
         MarkChanged();
      } );

      mSequence->Append(mAppendBuffer.ptr(), mSequence->GetSampleFormat(),
         mAppendBufferLen);
   }

   //wxLogDebug(wxT("now sample count %lli"), (long long) mSequence->GetNumSamples());
}

bool WaveClip::HandleXMLTag(const std::string_view& tag, const AttributesList &attrs)
{
   if (tag == "waveclip")
   {
      double dblValue;
      long longValue;
      for (auto pair : attrs)
      {
         auto attr = pair.first;
         auto value = pair.second;

         if (attr == "offset")
         {
            if (!value.TryGet(dblValue))
               return false;
            SetSequenceStartTime(dblValue);
         }
         else if (attr == "trimLeft")
         {
            if (!value.TryGet(dblValue))
               return false;
            SetTrimLeft(dblValue);
         }
         else if (attr == "trimRight")
         {
            if (!value.TryGet(dblValue))
               return false;
            SetTrimRight(dblValue);
         }
         else if (attr == "name")
         {
            if(value.IsStringView())
               SetName(value.ToWString());
         }
         else if (attr == "colorindex")
         {
            if (!value.TryGet(longValue))
               return false;
            SetColourIndex(longValue);
         }
      }
      return true;
   }

   return false;
}

void WaveClip::HandleXMLEndTag(const std::string_view& tag)
{
   if (tag == "waveclip")
      UpdateEnvelopeTrackLen();
}

XMLTagHandler *WaveClip::HandleXMLChild(const std::string_view& tag)
{
   if (tag == "sequence")
      return mSequence.get();
   else if (tag == "envelope")
      return mEnvelope.get();
   else if (tag == "waveclip")
   {
      // Nested wave clips are cut lines
      mCutLines.push_back(
         std::make_unique<WaveClip>(mSequence->GetFactory(),
            mSequence->GetSampleFormat(), mRate, 0 /*colourindex*/));
      return mCutLines.back().get();
   }
   else
      return NULL;
}

void WaveClip::WriteXML(XMLWriter &xmlFile) const
// may throw
{
   xmlFile.StartTag(wxT("waveclip"));
   xmlFile.WriteAttr(wxT("offset"), mSequenceOffset, 8);
   xmlFile.WriteAttr(wxT("trimLeft"), mTrimLeft, 8);
   xmlFile.WriteAttr(wxT("trimRight"), mTrimRight, 8);
   xmlFile.WriteAttr(wxT("name"), mName);
   xmlFile.WriteAttr(wxT("colorindex"), mColourIndex );

   mSequence->WriteXML(xmlFile);
   mEnvelope->WriteXML(xmlFile);

   for (const auto &clip: mCutLines)
      clip->WriteXML(xmlFile);

   xmlFile.EndTag(wxT("waveclip"));
}

/*! @excsafety{Strong} */
void WaveClip::Paste(double t0, const WaveClip* other)
{
   const bool clipNeedsResampling = other->mRate != mRate;
   const bool clipNeedsNewFormat =
      other->mSequence->GetSampleFormat() != mSequence->GetSampleFormat();
   std::unique_ptr<WaveClip> newClip;

   t0 = std::clamp(t0, GetPlayStartTime(), GetPlayEndTime());

   //seems like edge cases cannot happen, see WaveTrack::PasteWaveTrack
   if (t0 == GetPlayStartTime())
   {
       ClearSequence(GetSequenceStartTime(), t0);
       SetTrimLeft(other->GetTrimLeft());

       auto copy = std::make_unique<WaveClip>(*other, mSequence->GetFactory(), true);
       copy->ClearSequence(copy->GetPlayEndTime(), copy->GetSequenceEndTime());
       newClip = std::move(copy);
   }
   else if (t0 == GetPlayEndTime())
   {
       ClearSequence(GetPlayEndTime(), GetSequenceEndTime());
       SetTrimRight(other->GetTrimRight());

       auto copy = std::make_unique<WaveClip>(*other, mSequence->GetFactory(), true);
       copy->ClearSequence(copy->GetSequenceStartTime(), copy->GetPlayStartTime());
       newClip = std::move(copy);
   }
   else
   {
       newClip = std::make_unique<WaveClip>(*other, mSequence->GetFactory(), true,
           other->GetPlayStartTime(), other->GetPlayEndTime());
   }

   if (clipNeedsResampling || clipNeedsNewFormat)
   {
      auto copy = std::make_unique<WaveClip>(*newClip.get(), mSequence->GetFactory(), true);
      if (clipNeedsResampling)
         // The other clip's rate is different from ours, so resample
          copy->Resample(mRate);
      if (clipNeedsNewFormat)
         // Force sample formats to match.
          copy->ConvertToSampleFormat(mSequence->GetSampleFormat());
      newClip = std::move(copy);
   }

   // Paste cut lines contained in pasted clip
   WaveClipHolders newCutlines;
   for (const auto &cutline: newClip->mCutLines)
   {
      auto cutlineCopy = std::make_unique<WaveClip>(*cutline, mSequence->GetFactory(), 
         // Recursively copy cutlines of cutlines.  They don't need
         // their offsets adjusted.
         true);
      cutlineCopy->Offset(t0 - GetSequenceStartTime());
      newCutlines.push_back(std::move(cutlineCopy));
   }

   sampleCount s0 = TimeToSequenceSamples(t0);

   // Assume Strong-guarantee from Sequence::Paste
   mSequence->Paste(s0, newClip->mSequence.get());

   // Assume No-fail-guarantee in the remaining
   MarkChanged();
   auto sampleTime = 1.0 / GetRate();
   mEnvelope->PasteEnvelope
      (s0.as_double()/mRate + GetSequenceStartTime(), newClip->mEnvelope.get(), sampleTime);
   OffsetCutLines(t0, newClip->GetPlayEndTime() - newClip->GetPlayStartTime());

   for (auto &holder : newCutlines)
      mCutLines.push_back(std::move(holder));
}

/*! @excsafety{Strong} */
void WaveClip::InsertSilence( double t, double len, double *pEnvelopeValue )
{
   if (t == GetPlayStartTime() && t > GetSequenceStartTime())
      ClearSequence(GetSequenceStartTime(), t);
   else if (t == GetPlayEndTime() && t < GetSequenceEndTime()) {
      ClearSequence(t, GetSequenceEndTime());
      SetTrimRight(.0);
   }

   auto s0 = TimeToSequenceSamples(t);
   auto slen = (sampleCount)floor(len * mRate + 0.5);

   // use Strong-guarantee
   GetSequence()->InsertSilence(s0, slen);

   // use No-fail-guarantee
   OffsetCutLines(t, len);

   const auto sampleTime = 1.0 / GetRate();
   auto pEnvelope = GetEnvelope();
   if ( pEnvelopeValue ) {

      // Preserve limit value at the end
      auto oldLen = pEnvelope->GetTrackLen();
      auto newLen = oldLen + len;
      pEnvelope->Cap( sampleTime );

      // Ramp across the silence to the given value
      pEnvelope->SetTrackLen( newLen, sampleTime );
      pEnvelope->InsertOrReplace
         ( pEnvelope->GetOffset() + newLen, *pEnvelopeValue );
   }
   else
      pEnvelope->InsertSpace( t, len );

   MarkChanged();
}

/*! @excsafety{Strong} */
void WaveClip::AppendSilence( double len, double envelopeValue )
{
   auto t = GetPlayEndTime();
   InsertSilence( t, len, &envelopeValue );
}

/*! @excsafety{Strong} */
void WaveClip::Clear(double t0, double t1)
{
    auto st0 = t0;
    auto st1 = t1;
    auto offset = .0;
    if (st0 <= GetPlayStartTime())
    {
        offset = (t0 - GetPlayStartTime()) + GetTrimLeft();
        st0 = GetSequenceStartTime();

        SetTrimLeft(.0);
    }
    if (st1 >= GetPlayEndTime())
    {
        st1 = GetSequenceEndTime();
        SetTrimRight(.0);
    }
    ClearSequence(st0, st1);

    if (offset != .0)
        Offset(offset);        
}

void WaveClip::ClearLeft(double t)
{
   if (t > GetPlayStartTime() && t < GetPlayEndTime())
   {
      ClearSequence(GetSequenceStartTime(), t);
      SetTrimLeft(.0);
      SetSequenceStartTime(t);
   }
}

void WaveClip::ClearRight(double t)
{
   if (t > GetPlayStartTime() && t < GetPlayEndTime())
   {
      ClearSequence(t, GetSequenceEndTime());
      SetTrimRight(.0);
   }
}

void WaveClip::ClearSequence(double t0, double t1)
{
    auto clip_t0 = std::max(t0, GetSequenceStartTime());
    auto clip_t1 = std::min(t1, GetSequenceEndTime());

    auto s0 = TimeToSequenceSamples(clip_t0);
    auto s1 = TimeToSequenceSamples(clip_t1);

    if (s0 != s1)
    {
        // use Strong-guarantee
        GetSequence()->Delete(s0, s1 - s0);

        // use No-fail-guarantee in the remaining

        // msmeyer
        //
        // Delete all cutlines that are within the given area, if any.
        //
        // Note that when cutlines are active, two functions are used:
        // Clear() and ClearAndAddCutLine(). ClearAndAddCutLine() is called
        // whenever the user directly calls a command that removes some audio, e.g.
        // "Cut" or "Clear" from the menu. This command takes care about recursive
        // preserving of cutlines within clips. Clear() is called when internal
        // operations want to remove audio. In the latter case, it is the right
        // thing to just remove all cutlines within the area.
        //

        // May DELETE as we iterate, so don't use range-for
        for (auto it = mCutLines.begin(); it != mCutLines.end();)
        {
            WaveClip* clip = it->get();
            double cutlinePosition = GetSequenceStartTime() + clip->GetSequenceStartTime();
            if (cutlinePosition >= t0 && cutlinePosition <= t1)
            {
                // This cutline is within the area, DELETE it
                it = mCutLines.erase(it);
            }
            else
            {
                if (cutlinePosition >= t1)
                {
                    clip->Offset(clip_t0 - clip_t1);
                }
                ++it;
            }
        }

        // Collapse envelope
        auto sampleTime = 1.0 / GetRate();
        GetEnvelope()->CollapseRegion(t0, t1, sampleTime);
    }


    MarkChanged();
}

/*! @excsafety{Weak}
-- This WaveClip remains destructible in case of AudacityException.
But some cutlines may be deleted */
void WaveClip::ClearAndAddCutLine(double t0, double t1)
{
   if (t0 > GetPlayEndTime() || t1 < GetPlayStartTime())
      return; // time out of bounds

   const double clip_t0 = std::max( t0, GetPlayStartTime() );
   const double clip_t1 = std::min( t1, GetPlayEndTime() );

   auto newClip = std::make_unique< WaveClip >
      (*this, mSequence->GetFactory(), true, clip_t0, clip_t1);

   newClip->SetSequenceStartTime( clip_t0 - GetSequenceStartTime() );

   // Remove cutlines from this clip that were in the selection, shift
   // left those that were after the selection
   // May DELETE as we iterate, so don't use range-for
   for (auto it = mCutLines.begin(); it != mCutLines.end();)
   {
      WaveClip* clip = it->get();
      double cutlinePosition = GetSequenceStartTime() + clip->GetSequenceStartTime();
      if (cutlinePosition >= t0 && cutlinePosition <= t1)
         it = mCutLines.erase(it);
      else
      {
         if (cutlinePosition >= t1)
         {
            clip->Offset(clip_t0 - clip_t1);
         }
         ++it;
      }
   }

   // Clear actual audio data
   auto s0 = TimeToSequenceSamples(t0);
   auto s1 = TimeToSequenceSamples(t1);

   // use Weak-guarantee
   GetSequence()->Delete(s0, s1-s0);

   // Collapse envelope
   auto sampleTime = 1.0 / GetRate();
   GetEnvelope()->CollapseRegion( t0, t1, sampleTime );
   
   MarkChanged();

   mCutLines.push_back(std::move(newClip));
}

bool WaveClip::FindCutLine(double cutLinePosition,
                           double* cutlineStart /* = NULL */,
                           double* cutlineEnd /* = NULL */) const
{
   for (const auto &cutline: mCutLines)
   {
      if (fabs(GetSequenceStartTime() + cutline->GetSequenceStartTime() - cutLinePosition) < 0.0001)
      {
         if (cutlineStart)
            *cutlineStart = GetSequenceStartTime() + cutline->GetSequenceStartTime();
         if (cutlineEnd)
            *cutlineEnd = GetSequenceStartTime() + cutline->GetSequenceEndTime();
         return true;
      }
   }

   return false;
}

/*! @excsafety{Strong} */
void WaveClip::ExpandCutLine(double cutLinePosition)
{
   auto end = mCutLines.end();
   auto it = std::find_if( mCutLines.begin(), end,
      [&](const WaveClipHolder &cutline) {
         return fabs(GetSequenceStartTime() + cutline->GetSequenceStartTime() - cutLinePosition) < 0.0001;
      } );

   if ( it != end ) {
      auto cutline = it->get();
      // assume Strong-guarantee from Paste

      // Envelope::Paste takes offset into account, WaveClip::Paste doesn't!
      // Do this to get the right result:
      cutline->mEnvelope->SetOffset(0);

      Paste(GetSequenceStartTime()+cutline->GetSequenceStartTime(), cutline);
      // Now erase the cutline,
      // but be careful to find it again, because Paste above may
      // have modified the array of cutlines (if our cutline contained
      // another cutline!), invalidating the iterator we had.
      end = mCutLines.end();
      it = std::find_if(mCutLines.begin(), end,
         [=](const WaveClipHolder &p) { return p.get() == cutline; });
      if (it != end)
         mCutLines.erase(it); // deletes cutline!
      else {
         wxASSERT(false);
      }
   }
}

bool WaveClip::RemoveCutLine(double cutLinePosition)
{
   for (auto it = mCutLines.begin(); it != mCutLines.end(); ++it)
   {
      const auto &cutline = *it;
      //std::numeric_limits<double>::epsilon() or (1.0 / static_cast<double>(mRate))? 
      if (fabs(GetSequenceStartTime() + cutline->GetSequenceStartTime() - cutLinePosition) < 0.0001)
      {
         mCutLines.erase(it); // deletes cutline!
         return true;
      }
   }

   return false;
}

/*! @excsafety{No-fail} */
void WaveClip::OffsetCutLines(double t0, double len)
{
   for (const auto &cutLine : mCutLines)
   {
      if (GetSequenceStartTime() + cutLine->GetSequenceStartTime() >= t0)
         cutLine->Offset(len);
   }
}

void WaveClip::CloseLock()
{
   GetSequence()->CloseLock();
   for (const auto &cutline: mCutLines)
      cutline->CloseLock();
}

void WaveClip::SetRate(int rate)
{
   mRate = rate;
   auto newLength = mSequence->GetNumSamples().as_double() / mRate;
   mEnvelope->RescaleTimes( newLength );
   MarkChanged();
}

/*! @excsafety{Strong} */
void WaveClip::Resample(int rate, BasicUI::ProgressDialog *progress)
{
   // Note:  it is not necessary to do this recursively to cutlines.
   // They get resampled as needed when they are expanded.

   if (rate == mRate)
      return; // Nothing to do

   double factor = (double)rate / (double)mRate;
   ::Resample resample(true, factor, factor); // constant rate resampling

   const size_t bufsize = 65536;
   Floats inBuffer{ bufsize };
   Floats outBuffer{ bufsize };
   sampleCount pos = 0;
   bool error = false;
   int outGenerated = 0;
   auto numSamples = mSequence->GetNumSamples();

   auto newSequence =
      std::make_unique<Sequence>(mSequence->GetFactory(), mSequence->GetSampleFormat());

   /**
    * We want to keep going as long as we have something to feed the resampler
    * with OR as long as the resampler spews out samples (which could continue
    * for a few iterations after we stop feeding it)
    */
   while (pos < numSamples || outGenerated > 0)
   {
      const auto inLen = limitSampleBufferSize( bufsize, numSamples - pos );

      bool isLast = ((pos + inLen) == numSamples);

      if (!mSequence->Get((samplePtr)inBuffer.get(), floatSample, pos, inLen, true))
      {
         error = true;
         break;
      }

      const auto results = resample.Process(factor, inBuffer.get(), inLen, isLast,
                                            outBuffer.get(), bufsize);
      outGenerated = results.second;

      pos += results.first;

      if (outGenerated < 0)
      {
         error = true;
         break;
      }

      newSequence->Append((samplePtr)outBuffer.get(), floatSample,
                          outGenerated);

      if (progress)
      {
         auto updateResult = progress->Poll(
            pos.as_long_long(),
            numSamples.as_long_long()
         );
         error = (updateResult != BasicUI::ProgressResult::Success);
         if (error)
            throw UserException{};
      }
   }

   if (error)
      throw SimpleMessageBoxException{
         ExceptionType::Internal,
         XO("Resampling failed."),
         XO("Warning"),
         "Error:_Resampling"
      };
   else
   {
      // Use No-fail-guarantee in these steps
      mSequence = std::move(newSequence);
      mRate = rate;
      Caches::ForEach( std::mem_fn( &WaveClipListener::Invalidate ) );
   }
}

// Used by commands which interact with clips using the keyboard.
// When two clips are immediately next to each other, the GetPlayEndTime()
// of the first clip and the GetPlayStartTime() of the second clip may not
// be exactly equal due to rounding errors.
bool WaveClip::SharesBoundaryWithNextClip(const WaveClip* next) const
{
   double endThis = GetRate() * GetPlayStartTime() + GetPlaySamplesCount().as_double();
   double startNext = next->GetRate() * next->GetPlayStartTime();

   // given that a double has about 15 significant digits, using a criterion
   // of half a sample should be safe in all normal usage.
   return fabs(startNext - endThis) < 0.5;
}

void WaveClip::SetName(const wxString& name)
{
   mName = name;
}

const wxString& WaveClip::GetName() const
{
   return mName;
}

sampleCount WaveClip::TimeToSamples(double time) const noexcept
{
    return sampleCount(floor(time * mRate + 0.5));
}

double WaveClip::SamplesToTime(sampleCount s) const noexcept
{
    return s.as_double() / mRate;
}

void WaveClip::SetSilence(sampleCount offset, sampleCount length)
{
    GetSequence()->SetSilence(TimeToSamples(GetTrimLeft()) + offset, length);
    MarkChanged();
}

sampleCount WaveClip::GetSequenceSamplesCount() const
{
    return mSequence->GetNumSamples();
}

double WaveClip::GetPlayStartTime() const noexcept
{
    return mSequenceOffset + SamplesToTime(TimeToSamples(mTrimLeft));
}

void WaveClip::SetPlayStartTime(double time)
{
    SetSequenceStartTime(time - mTrimLeft);
}

double WaveClip::GetPlayEndTime() const
{
    auto numSamples = mSequence->GetNumSamples();

    double maxLen = GetSequenceStartTime() + ((numSamples + mAppendBufferLen).as_double()) / mRate 
       - SamplesToTime(TimeToSamples(mTrimRight));
    // JS: calculated value is not the length;
    // it is a maximum value and can be negative; no clipping to 0

    return maxLen;
}

sampleCount WaveClip::GetPlayStartSample() const
{
    return TimeToSamples(GetPlayStartTime());
}

sampleCount WaveClip::GetPlayEndSample() const
{
    return GetPlayStartSample() + GetPlaySamplesCount();
}

sampleCount WaveClip::GetPlaySamplesCount() const
{
    return mSequence->GetNumSamples()
       - TimeToSamples(mTrimRight) - TimeToSamples(mTrimLeft);
}

void WaveClip::SetTrimLeft(double trim)
{
    mTrimLeft = std::max(.0, trim);
}

double WaveClip::GetTrimLeft() const noexcept
{
    return mTrimLeft;
}

void WaveClip::SetTrimRight(double trim)
{
    mTrimRight = std::max(.0, trim);
}

double WaveClip::GetTrimRight() const noexcept
{
    return mTrimRight;
}

void WaveClip::TrimLeft(double deltaTime)
{
    mTrimLeft += deltaTime;
}

void WaveClip::TrimRight(double deltaTime)
{
    mTrimRight += deltaTime;
}

void WaveClip::TrimLeftTo(double to)
{
    mTrimLeft = std::clamp(to, GetSequenceStartTime(), GetPlayEndTime()) - GetSequenceStartTime();
}

void WaveClip::TrimRightTo(double to)
{
    mTrimRight = GetSequenceEndTime() - std::clamp(to, GetPlayStartTime(), GetSequenceEndTime());
}

double WaveClip::GetSequenceStartTime() const noexcept
{
    // JS: mSequenceOffset is the minimum value and it is returned; no clipping to 0
    return mSequenceOffset;
}

void WaveClip::SetSequenceStartTime(double startTime)
{
    mSequenceOffset = startTime;
    mEnvelope->SetOffset(startTime);
}

double WaveClip::GetSequenceEndTime() const
{
    auto numSamples = mSequence->GetNumSamples();

    double maxLen = GetSequenceStartTime() + (numSamples + mAppendBufferLen).as_double() / mRate;
    // JS: calculated value is not the length;
    // it is a maximum value and can be negative; no clipping to 0

    return maxLen;
}

sampleCount WaveClip::GetSequenceStartSample() const
{
    return TimeToSamples(mSequenceOffset);
}

sampleCount WaveClip::GetSequenceEndSample() const
{
    return GetSequenceStartSample() + mSequence->GetNumSamples();
}

void WaveClip::Offset(double delta) noexcept
{
    SetSequenceStartTime(GetSequenceStartTime() + delta);
}

// Bug 2288 allowed overlapping clips.
// This was a classic fencepost error.
// We are within the clip if start < t <= end.
// Note that BeforeClip and AfterClip must be consistent 
// with this definition.
bool WaveClip::WithinPlayRegion(double t) const
{
    auto ts = TimeToSamples(t);
    return ts > GetPlayStartSample() && ts < GetPlayEndSample() + mAppendBufferLen;
}

bool WaveClip::BeforePlayStartTime(double t) const
{
    auto ts = TimeToSamples(t);
    return ts <= GetPlayStartSample();
}

bool WaveClip::AfterPlayEndTime(double t) const
{
    auto ts = TimeToSamples(t);
    return ts >= GetPlayEndSample() + mAppendBufferLen;
}

sampleCount WaveClip::TimeToSequenceSamples(double t) const
{
    if (t < GetSequenceStartTime())
        return 0;
    else if (t > GetSequenceEndTime())
        return mSequence->GetNumSamples();
    return TimeToSamples(t - GetSequenceStartTime());
}

sampleCount WaveClip::ToSequenceSamples(sampleCount s) const
{
    return s - GetSequenceStartSample();
}
