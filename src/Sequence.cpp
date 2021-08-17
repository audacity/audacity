/**********************************************************************

  Audacity: A Digital Audio Editor

  Sequence.cpp

  Dominic Mazzoni

*******************************************************************//**
\file Sequence.cpp
\brief Implements classes Sequence and SeqBlock.

*//****************************************************************//**

\class Sequence
\brief A WaveTrack contains WaveClip(s).
   A WaveClip contains a Sequence. A Sequence is primarily an
   interface to an array of SeqBlock instances, corresponding to
   the audio sample blocks in the database.
   Contrast with RingBuffer.

*//****************************************************************//**

\class SeqBlock
\brief Data structure containing pointer to a sample block and
   a start time. Element of a BlockArray.

*//*******************************************************************/



#include "Sequence.h"

#include <algorithm>
#include <float.h>
#include <math.h>

#include <wx/intl.h>
#include <wx/filefn.h>
#include <wx/ffile.h>
#include <wx/log.h>

#include "SampleBlock.h"
#include "InconsistencyException.h"
#include "widgets/AudacityMessageBox.h"

size_t Sequence::sMaxDiskBlockSize = 1048576;

// Sequence methods
Sequence::Sequence(
   const SampleBlockFactoryPtr &pFactory, sampleFormat format)
:  mpFactory(pFactory),
   mSampleFormat(format),
   mMinSamples(sMaxDiskBlockSize / SAMPLE_SIZE(mSampleFormat) / 2),
   mMaxSamples(mMinSamples * 2)
{
}

// essentially a copy constructor - but you must pass in the
// current project, because we might be copying from one
// project to another
Sequence::Sequence(
   const Sequence &orig, const SampleBlockFactoryPtr &pFactory)
:  mpFactory(pFactory),
   mSampleFormat(orig.mSampleFormat),
   mMinSamples(orig.mMinSamples),
   mMaxSamples(orig.mMaxSamples)
{
   Paste(0, &orig);
}

Sequence::~Sequence()
{
}

size_t Sequence::GetMaxBlockSize() const
{
   return mMaxSamples;
}

size_t Sequence::GetIdealBlockSize() const
{
   return mMaxSamples;
}

bool Sequence::CloseLock()
{
   for (unsigned int i = 0; i < mBlock.size(); i++)
      mBlock[i].sb->CloseLock();

   return true;
}

sampleFormat Sequence::GetSampleFormat() const
{
   return mSampleFormat;
}

/*
bool Sequence::SetSampleFormat(sampleFormat format)
{
   if (mBlock.size() > 0 || mNumSamples > 0)
      return false;

   mSampleFormat = format;
   return true;
}
*/

namespace {
   void ensureSampleBufferSize(SampleBuffer &buffer, sampleFormat format,
                               size_t &size, size_t required,
                               SampleBuffer *pSecondBuffer = nullptr)
   {
      // This should normally do nothing, but it is a defense against corrupt
      // projects than might have inconsistent block files bigger than the
      // expected maximum size.
      if (size < required) {
         // reallocate
         buffer.Allocate(required, format);
         if (pSecondBuffer && pSecondBuffer->ptr())
            pSecondBuffer->Allocate(required, format);
         if (!buffer.ptr() || (pSecondBuffer && !pSecondBuffer->ptr())) {
            // malloc failed
            // Perhaps required is a really crazy value,
            // and perhaps we should throw an AudacityException, but that is
            // a second-order concern
            THROW_INCONSISTENCY_EXCEPTION;
         }
         size = required;
      }
   }
}

/*! @excsafety{Strong} */
bool Sequence::ConvertToSampleFormat(sampleFormat format,
   const std::function<void(size_t)> & progressReport)
{
   if (format == mSampleFormat)
      // no change
      return false;

   if (mBlock.size() == 0)
   {
      mSampleFormat = format;
      return true;
   }

   const sampleFormat oldFormat = mSampleFormat;
   mSampleFormat = format;

   const auto oldMinSamples = mMinSamples, oldMaxSamples = mMaxSamples;
   // These are the same calculations as in the constructor.
   mMinSamples = sMaxDiskBlockSize / SAMPLE_SIZE(mSampleFormat) / 2;
   mMaxSamples = mMinSamples * 2;

   bool bSuccess = false;
   auto cleanup = finally( [&] {
      if (!bSuccess) {
         // Conversion failed. Revert these member vars.
         mSampleFormat = oldFormat;
         mMaxSamples = oldMaxSamples;
         mMinSamples = oldMinSamples;
      }
   } );

   BlockArray newBlockArray;
   // Use the ratio of old to NEW mMaxSamples to make a reasonable guess
   // at allocation.
   newBlockArray.reserve
      (1 + mBlock.size() * ((float)oldMaxSamples / (float)mMaxSamples));

   {
      size_t oldSize = oldMaxSamples;
      SampleBuffer bufferOld(oldSize, oldFormat);
      size_t newSize = oldMaxSamples;
      SampleBuffer bufferNew(newSize, format);

      for (size_t i = 0, nn = mBlock.size(); i < nn; i++)
      {
         SeqBlock &oldSeqBlock = mBlock[i];
         const auto &oldBlockFile = oldSeqBlock.sb;
         const auto len = oldBlockFile->GetSampleCount();
         ensureSampleBufferSize(bufferOld, oldFormat, oldSize, len);
         Read(bufferOld.ptr(), oldFormat, oldSeqBlock, 0, len, true);

         ensureSampleBufferSize(bufferNew, format, newSize, len);
         CopySamples(bufferOld.ptr(), oldFormat, bufferNew.ptr(), format, len);

         // Note this fix for http://bugzilla.audacityteam.org/show_bug.cgi?id=451,
         // using Blockify, allows (len < mMinSamples).
         // This will happen consistently when going from more bytes per sample to fewer...
         // This will create a block that's smaller than mMinSamples, which
         // shouldn't be allowed, but we agreed it's okay for now.
         //vvv ANSWER-ME: Does this cause any bugs, or failures on write, elsewhere?
         //    If so, need to special-case (len < mMinSamples) and start combining data
         //    from the old blocks... Oh no!

         // Using Blockify will handle the cases where len > the NEW mMaxSamples. Previous code did not.
         const auto blockstart = oldSeqBlock.start;
         Blockify(*mpFactory, mMaxSamples, mSampleFormat,
                  newBlockArray, blockstart, bufferNew.ptr(), len);

         if (progressReport)
            progressReport(len);
      }
   }

   // Invalidate all the old, non-aliased block files.
   // Aliased files will be converted at save, per comment above.

   // Commit the changes to block file array
   CommitChangesIfConsistent
      (newBlockArray, mNumSamples, wxT("Sequence::ConvertToSampleFormat()"));

   // Commit the other changes
   bSuccess = true;

   return true;
}

std::pair<float, float> Sequence::GetMinMax(
   sampleCount start, sampleCount len, bool mayThrow) const
{
   if (len == 0 || mBlock.size() == 0) {
      return {
         0.f,
         // FLT_MAX?  So it doesn't look like a spurious '0' to a caller?

         0.f
         // -FLT_MAX?  So it doesn't look like a spurious '0' to a caller?
      };
   }

   float min = FLT_MAX;
   float max = -FLT_MAX;

   unsigned int block0 = FindBlock(start);
   unsigned int block1 = FindBlock(start + len - 1);

   // First calculate the min/max of the blocks in the middle of this region;
   // this is very fast because we have the min/max of every entire block
   // already in memory.

   for (unsigned b = block0 + 1; b < block1; ++b) {
      auto results = mBlock[b].sb->GetMinMaxRMS(mayThrow);

      if (results.min < min)
         min = results.min;
      if (results.max > max)
         max = results.max;
   }

   // Now we take the first and last blocks into account, noting that the
   // selection may only partly overlap these blocks.  If the overall min/max
   // of either of these blocks is within min...max, then we can ignore them.
   // If not, we need read some samples and summaries from disk.
   {
      const SeqBlock &theBlock = mBlock[block0];
      const auto &theFile = theBlock.sb;
      auto results = theFile->GetMinMaxRMS(mayThrow);

      if (results.min < min || results.max > max) {
         // start lies within theBlock:
         auto s0 = ( start - theBlock.start ).as_size_t();
         const auto maxl0 = (
            // start lies within theBlock:
            theBlock.start + theFile->GetSampleCount() - start
         ).as_size_t();
         wxASSERT(maxl0 <= mMaxSamples); // Vaughan, 2011-10-19
         const auto l0 = limitSampleBufferSize ( maxl0, len );

         results = theFile->GetMinMaxRMS(s0, l0, mayThrow);
         if (results.min < min)
            min = results.min;
         if (results.max > max)
            max = results.max;
      }
   }

   if (block1 > block0)
   {
      const SeqBlock &theBlock = mBlock[block1];
      const auto &theFile = theBlock.sb;
      auto results = theFile->GetMinMaxRMS(mayThrow);

      if (results.min < min || results.max > max) {

         // start + len - 1 lies in theBlock:
         const auto l0 = ( start + len - theBlock.start ).as_size_t();
         wxASSERT(l0 <= mMaxSamples); // Vaughan, 2011-10-19

         results = theFile->GetMinMaxRMS(0, l0, mayThrow);
         if (results.min < min)
            min = results.min;
         if (results.max > max)
            max = results.max;
      }
   }

   return { min, max };
}

float Sequence::GetRMS(sampleCount start, sampleCount len, bool mayThrow) const
{
   // len is the number of samples that we want the rms of.
   // it may be longer than a block, and the code is carefully set up to handle that.
   if (len == 0 || mBlock.size() == 0)
      return 0.f;

   double sumsq = 0.0;
   sampleCount length = 0; // this is the cumulative length of the bits we have the ms of so far, and should end up == len

   unsigned int block0 = FindBlock(start);
   unsigned int block1 = FindBlock(start + len - 1);

   // First calculate the rms of the blocks in the middle of this region;
   // this is very fast because we have the rms of every entire block
   // already in memory.
   for (unsigned b = block0 + 1; b < block1; b++) {
      const SeqBlock &theBlock = mBlock[b];
      const auto &sb = theBlock.sb;
      auto results = sb->GetMinMaxRMS(mayThrow);

      const auto fileLen = sb->GetSampleCount();
      const auto blockRMS = results.RMS;
      sumsq += blockRMS * blockRMS * fileLen;
      length += fileLen;
   }

   // Now we take the first and last blocks into account, noting that the
   // selection may only partly overlap these blocks.
   // If not, we need read some samples and summaries from disk.
   {
      const SeqBlock &theBlock = mBlock[block0];
      const auto &sb = theBlock.sb;
      // start lies within theBlock
      auto s0 = ( start - theBlock.start ).as_size_t();
      // start lies within theBlock
      const auto maxl0 =
         (theBlock.start + sb->GetSampleCount() - start).as_size_t();
      wxASSERT(maxl0 <= mMaxSamples); // Vaughan, 2011-10-19
      const auto l0 = limitSampleBufferSize( maxl0, len );

      auto results = sb->GetMinMaxRMS(s0, l0, mayThrow);
      const auto partialRMS = results.RMS;
      sumsq += partialRMS * partialRMS * l0;
      length += l0;
   }

   if (block1 > block0) {
      const SeqBlock &theBlock = mBlock[block1];
      const auto &sb = theBlock.sb;

      // start + len - 1 lies within theBlock
      const auto l0 = ( start + len - theBlock.start ).as_size_t();
      wxASSERT(l0 <= mMaxSamples); // PRL: I think Vaughan missed this

      auto results = sb->GetMinMaxRMS(0, l0, mayThrow);
      const auto partialRMS = results.RMS;
      sumsq += partialRMS * partialRMS * l0;
      length += l0;
   }

   // PRL: catch bugs like 1320:
   wxASSERT(length == len);

   return sqrt(sumsq / length.as_double() );
}

// Must pass in the correct factory for the result.  If it's not the same
// as in this, then block contents must be copied.
std::unique_ptr<Sequence> Sequence::Copy( const SampleBlockFactoryPtr &pFactory,
   sampleCount s0, sampleCount s1) const
{
   // Make a new Sequence object for the specified factory:
   auto dest = std::make_unique<Sequence>(pFactory, mSampleFormat);
   if (s0 >= s1 || s0 >= mNumSamples || s1 < 0) {
      return dest;
   }

   // Decide whether to share sample blocks or make new copies, when whole block
   // contents are used -- must copy if factories are different:
   auto pUseFactory = (pFactory == mpFactory) ? nullptr : pFactory.get();

   int numBlocks = mBlock.size();

   int b0 = FindBlock(s0);
   const int b1 = FindBlock(s1 - 1);
   wxASSERT(b0 >= 0);
   wxASSERT(b0 < numBlocks);
   wxASSERT(b1 < numBlocks);
   wxUnusedVar(numBlocks);
   wxASSERT(b0 <= b1);

   dest->mBlock.reserve(b1 - b0 + 1);

   auto bufferSize = mMaxSamples;
   SampleBuffer buffer(bufferSize, mSampleFormat);

   int blocklen;

   // Do any initial partial block

   const SeqBlock &block0 = mBlock[b0];
   if (s0 != block0.start) {
      const auto &sb = block0.sb;
      // Nonnegative result is length of block0 or less:
      blocklen =
         ( std::min(s1, block0.start + sb->GetSampleCount()) - s0 ).as_size_t();
      wxASSERT(blocklen <= (int)mMaxSamples); // Vaughan, 2012-02-29
      ensureSampleBufferSize(buffer, mSampleFormat, bufferSize, blocklen);
      Get(b0, buffer.ptr(), mSampleFormat, s0, blocklen, true);

      dest->Append(buffer.ptr(), mSampleFormat, blocklen);
   }
   else
      --b0;

   // If there are blocks in the middle, use the blocks whole
   for (int bb = b0 + 1; bb < b1; ++bb)
      AppendBlock(pUseFactory, mSampleFormat,
         dest->mBlock, dest->mNumSamples, mBlock[bb]);
      // Increase ref count or duplicate file

   // Do the last block
   if (b1 > b0) {
      // Probable case of a partial block
      const SeqBlock &block = mBlock[b1];
      const auto &sb = block.sb;
      // s1 is within block:
      blocklen = (s1 - block.start).as_size_t();
      wxASSERT(blocklen <= (int)mMaxSamples); // Vaughan, 2012-02-29
      if (blocklen < (int)sb->GetSampleCount()) {
         ensureSampleBufferSize(buffer, mSampleFormat, bufferSize, blocklen);
         Get(b1, buffer.ptr(), mSampleFormat, block.start, blocklen, true);
         dest->Append(buffer.ptr(), mSampleFormat, blocklen);
      }
      else
         // Special case of a whole block
         AppendBlock(pUseFactory, mSampleFormat,
            dest->mBlock, dest->mNumSamples, block);
         // Increase ref count or duplicate file
   }

   dest->ConsistencyCheck(wxT("Sequence::Copy()"));

   return dest;
}

namespace {
   inline bool Overflows(double numSamples)
   {
      return numSamples > wxLL(9223372036854775807);
   }

   SampleBlockPtr ShareOrCopySampleBlock(
      SampleBlockFactory *pFactory, sampleFormat format, SampleBlockPtr sb )
   {
      if ( pFactory ) {
         // must copy contents to a fresh SampleBlock object in another database
         auto sampleCount = sb->GetSampleCount();
         SampleBuffer buffer{ sampleCount, format };
         sb->GetSamples( buffer.ptr(), format, 0, sampleCount );
         sb = pFactory->Create( buffer.ptr(), sampleCount, format );
      }
      else
         // Can just share
         ;
      return sb;
   }
}

/*! @excsafety{Strong} */
void Sequence::Paste(sampleCount s, const Sequence *src)
{
   if ((s < 0) || (s > mNumSamples))
   {
      wxLogError(
         wxT("Sequence::Paste: sampleCount s %s is < 0 or > mNumSamples %s)."),
         // PRL:  Why bother with Internat when the above is just wxT?
         Internat::ToString(s.as_double(), 0),
         Internat::ToString(mNumSamples.as_double(), 0));
      THROW_INCONSISTENCY_EXCEPTION;
   }

   // Quick check to make sure that it doesn't overflow
   if (Overflows((mNumSamples.as_double()) + (src->mNumSamples.as_double())))
   {
      wxLogError(
         wxT("Sequence::Paste: mNumSamples %s + src->mNumSamples %s would overflow."),
         // PRL:  Why bother with Internat when the above is just wxT?
         Internat::ToString(mNumSamples.as_double(), 0),
         Internat::ToString(src->mNumSamples.as_double(), 0));
      THROW_INCONSISTENCY_EXCEPTION;
   }

   if (src->mSampleFormat != mSampleFormat)
   {
      wxLogError(
         wxT("Sequence::Paste: Sample format to be pasted, %s, does not match destination format, %s."),
         GetSampleFormatStr(src->mSampleFormat).Debug(),
         GetSampleFormatStr(mSampleFormat).Debug());
      THROW_INCONSISTENCY_EXCEPTION;
   }

   const BlockArray &srcBlock = src->mBlock;
   auto addedLen = src->mNumSamples;
   const unsigned int srcNumBlocks = srcBlock.size();
   auto sampleSize = SAMPLE_SIZE(mSampleFormat);

   if (addedLen == 0 || srcNumBlocks == 0)
      return;

   const size_t numBlocks = mBlock.size();

   // Decide whether to share sample blocks or make new copies, when whole block
   // contents are used -- must copy if factories are different:
   auto pUseFactory =
      (src->mpFactory == mpFactory) ? nullptr : mpFactory.get();

   if (numBlocks == 0 ||
       (s == mNumSamples && mBlock.back().sb->GetSampleCount() >= mMinSamples)) {
      // Special case: this track is currently empty, or it's safe to append
      // onto the end because the current last block is longer than the
      // minimum size

      // Build and swap a copy so there is a strong exception safety guarantee
      BlockArray newBlock{ mBlock };
      sampleCount samples = mNumSamples;
      for (unsigned int i = 0; i < srcNumBlocks; i++)
         // AppendBlock may throw for limited disk space, if pasting from
         // one project into another.
         AppendBlock(pUseFactory, mSampleFormat,
            newBlock, samples, srcBlock[i]);

      CommitChangesIfConsistent
         (newBlock, samples, wxT("Paste branch one"));
      return;
   }

   const int b = (s == mNumSamples) ? mBlock.size() - 1 : FindBlock(s);
   wxASSERT((b >= 0) && (b < (int)numBlocks));
   SeqBlock *const pBlock = &mBlock[b];
   const auto length = pBlock->sb->GetSampleCount();
   const auto largerBlockLen = addedLen + length;
   // PRL: when insertion point is the first sample of a block,
   // and the following test fails, perhaps we could test
   // whether coalescence with the previous block is possible.
   if (largerBlockLen <= mMaxSamples) {
      // Special case: we can fit all of the NEW samples inside of
      // one block!

      SeqBlock &block = *pBlock;
      // largerBlockLen is not more than mMaxSamples...
      SampleBuffer buffer(largerBlockLen.as_size_t(), mSampleFormat);

      // ...and addedLen is not more than largerBlockLen
      auto sAddedLen = addedLen.as_size_t();
      // s lies within block:
      auto splitPoint = ( s - block.start ).as_size_t();
      Read(buffer.ptr(), mSampleFormat, block, 0, splitPoint, true);
      src->Get(0, buffer.ptr() + splitPoint*sampleSize,
               mSampleFormat, 0, sAddedLen, true);
      Read(buffer.ptr() + (splitPoint + sAddedLen) * sampleSize,
           mSampleFormat, block,
           splitPoint, length - splitPoint, true);

      // largerBlockLen is not more than mMaxSamples...
      block.sb = mpFactory->Create(
         buffer.ptr(),
         largerBlockLen.as_size_t(),
         mSampleFormat);

      // Don't make a duplicate array.  We can still give Strong-guarantee
      // if we modify only one block in place.

      // use No-fail-guarantee in remaining steps
      for (unsigned int i = b + 1; i < numBlocks; i++)
         mBlock[i].start += addedLen;

      mNumSamples += addedLen;

      // This consistency check won't throw, it asserts.
      // Proof that we kept consistency is not hard.
      ConsistencyCheck(wxT("Paste branch two"), false);
      return;
   }

   // Case three: if we are inserting four or fewer blocks,
   // it's simplest to just lump all the data together
   // into one big block along with the split block,
   // then resplit it all
   BlockArray newBlock;
   newBlock.reserve(numBlocks + srcNumBlocks + 2);
   newBlock.insert(newBlock.end(), mBlock.begin(), mBlock.begin() + b);

   SeqBlock &splitBlock = mBlock[b];
   auto splitLen = splitBlock.sb->GetSampleCount();
   // s lies within splitBlock
   auto splitPoint = ( s - splitBlock.start ).as_size_t();

   unsigned int i;
   if (srcNumBlocks <= 4) {

      // addedLen is at most four times maximum block size
      auto sAddedLen = addedLen.as_size_t();
      const auto sum = splitLen + sAddedLen;

      SampleBuffer sumBuffer(sum, mSampleFormat);
      Read(sumBuffer.ptr(), mSampleFormat, splitBlock, 0, splitPoint, true);
      src->Get(0, sumBuffer.ptr() + splitPoint * sampleSize,
               mSampleFormat,
               0, sAddedLen, true);
      Read(sumBuffer.ptr() + (splitPoint + sAddedLen) * sampleSize, mSampleFormat,
           splitBlock, splitPoint,
           splitLen - splitPoint, true);

      Blockify(*mpFactory, mMaxSamples, mSampleFormat,
               newBlock, splitBlock.start, sumBuffer.ptr(), sum);
   } else {

      // The final case is that we're inserting at least five blocks.
      // We divide these into three groups: the first two get merged
      // with the first half of the split block, the middle ones get
      // used whole, and the last two get merged with the last
      // half of the split block.

      const auto srcFirstTwoLen =
          srcBlock[0].sb->GetSampleCount() + srcBlock[1].sb->GetSampleCount();
      const auto leftLen = splitPoint + srcFirstTwoLen;

      const SeqBlock &penultimate = srcBlock[srcNumBlocks - 2];
      const auto srcLastTwoLen =
         penultimate.sb->GetSampleCount() +
         srcBlock[srcNumBlocks - 1].sb->GetSampleCount();
      const auto rightSplit = splitBlock.sb->GetSampleCount() - splitPoint;
      const auto rightLen = rightSplit + srcLastTwoLen;

      SampleBuffer sampleBuffer(std::max(leftLen, rightLen), mSampleFormat);

      Read(sampleBuffer.ptr(), mSampleFormat, splitBlock, 0, splitPoint, true);
      src->Get(0, sampleBuffer.ptr() + splitPoint*sampleSize,
         mSampleFormat, 0, srcFirstTwoLen, true);

      Blockify(*mpFactory, mMaxSamples, mSampleFormat,
               newBlock, splitBlock.start, sampleBuffer.ptr(), leftLen);

      for (i = 2; i < srcNumBlocks - 2; i++) {
         const SeqBlock &block = srcBlock[i];
         auto sb = ShareOrCopySampleBlock(
            pUseFactory, mSampleFormat, block.sb );
         newBlock.push_back(SeqBlock(sb, block.start + s));
      }

      auto lastStart = penultimate.start;
      src->Get(srcNumBlocks - 2, sampleBuffer.ptr(), mSampleFormat,
               lastStart, srcLastTwoLen, true);
      Read(sampleBuffer.ptr() + srcLastTwoLen * sampleSize, mSampleFormat,
           splitBlock, splitPoint, rightSplit, true);

      Blockify(*mpFactory, mMaxSamples, mSampleFormat,
               newBlock, s + lastStart, sampleBuffer.ptr(), rightLen);
   }

   // Copy remaining blocks to NEW block array and
   // swap the NEW block array in for the old
   for (i = b + 1; i < numBlocks; i++)
      newBlock.push_back(mBlock[i].Plus(addedLen));

   CommitChangesIfConsistent
      (newBlock, mNumSamples + addedLen, wxT("Paste branch three"));
}

/*! @excsafety{Strong} */
void Sequence::SetSilence(sampleCount s0, sampleCount len)
{
   SetSamples(NULL, mSampleFormat, s0, len);
}

/*! @excsafety{Strong} */
void Sequence::InsertSilence(sampleCount s0, sampleCount len)
{
   auto &factory = *mpFactory;

   // Quick check to make sure that it doesn't overflow
   if (Overflows((mNumSamples.as_double()) + (len.as_double())))
      THROW_INCONSISTENCY_EXCEPTION;

   if (len <= 0)
      return;

   // Create a NEW track containing as much silence as we
   // need to insert, and then call Paste to do the insertion.

   Sequence sTrack(mpFactory, mSampleFormat);

   auto idealSamples = GetIdealBlockSize();

   sampleCount pos = 0;

   // Could nBlocks overflow a size_t?  Not very likely.  You need perhaps
   // 2 ^ 52 samples which is over 3000 years at 44.1 kHz.
   auto nBlocks = (len + idealSamples - 1) / idealSamples;
   sTrack.mBlock.reserve(nBlocks.as_size_t());

   if (len >= idealSamples) {
      auto silentFile = factory.CreateSilent(
         idealSamples,
         mSampleFormat);
      while (len >= idealSamples) {
         sTrack.mBlock.push_back(SeqBlock(silentFile, pos));

         pos += idealSamples;
         len -= idealSamples;
      }
   }
   if (len != 0) {
      // len is not more than idealSamples:
      sTrack.mBlock.push_back(SeqBlock(
         factory.CreateSilent(len.as_size_t(), mSampleFormat), pos));
      pos += len;
   }

   sTrack.mNumSamples = pos;

   // use Strong-guarantee
   Paste(s0, &sTrack);
}

void Sequence::AppendBlock( SampleBlockFactory *pFactory, sampleFormat format,
   BlockArray &mBlock, sampleCount &mNumSamples, const SeqBlock &b)
{
   // Quick check to make sure that it doesn't overflow
   if (Overflows((mNumSamples.as_double()) + ((double)b.sb->GetSampleCount())))
      THROW_INCONSISTENCY_EXCEPTION;

   auto sb = ShareOrCopySampleBlock( pFactory, format, b.sb );
   SeqBlock newBlock(sb, mNumSamples);

   // We can assume newBlock.sb is not null

   mBlock.push_back(newBlock);
   mNumSamples += newBlock.sb->GetSampleCount();

   // Don't do a consistency check here because this
   // function gets called in an inner loop.
}

sampleCount Sequence::GetBlockStart(sampleCount position) const
{
   int b = FindBlock(position);
   return mBlock[b].start;
}

size_t Sequence::GetBestBlockSize(sampleCount start) const
{
   // This method returns a nice number of samples you should try to grab in
   // one big chunk in order to land on a block boundary, based on the starting
   // sample.  The value returned will always be nonzero and will be no larger
   // than the value of GetMaxBlockSize()

   if (start < 0 || start >= mNumSamples)
      return mMaxSamples;

   int b = FindBlock(start);
   int numBlocks = mBlock.size();

   const SeqBlock &block = mBlock[b];
   // start is in block:
   auto result = (block.start + block.sb->GetSampleCount() - start).as_size_t();

   decltype(result) length;
   while(result < mMinSamples && b+1<numBlocks &&
         ((length = mBlock[b+1].sb->GetSampleCount()) + result) <= mMaxSamples) {
      b++;
      result += length;
   }

   wxASSERT(result > 0 && result <= mMaxSamples);

   return result;
}

bool Sequence::HandleXMLTag(const wxChar *tag, const wxChar **attrs)
{
   auto &factory = *mpFactory;

   /* handle waveblock tag and its attributes */
   if (!wxStrcmp(tag, wxT("waveblock")))
   {
      SeqBlock wb;

      // Give SampleBlock a go at the attributes first
      wb.sb = factory.CreateFromXML(mSampleFormat, attrs);
      if (wb.sb == nullptr)
      {
         mErrorOpening = true;
         return false;
      }

      // loop through attrs, which is a null-terminated list of attribute-value pairs
      while(*attrs)
      {
         const wxChar *attr = *attrs++;
         const wxChar *value = *attrs++;

         if (!value)
         {
            break;
         }

         long long nValue = 0;

         const wxString strValue = value;	// promote string, we need this for all

         if (wxStrcmp(attr, wxT("start")) == 0)
         {
            // This attribute is a sample offset, so can be 64bit
            if (!XMLValueChecker::IsGoodInt64(strValue) || !strValue.ToLongLong(&nValue) || (nValue < 0))
            {
               mErrorOpening = true;
               return false;
            }

            wb.start = nValue;
         }
      }

      mBlock.push_back(wb);

      return true;
   }

   /* handle sequence tag and its attributes */
   if (!wxStrcmp(tag, wxT("sequence")))
   {
      while(*attrs)
      {
         const wxChar *attr = *attrs++;
         const wxChar *value = *attrs++;

         if (!value)
         {
            break;
         }

         long long nValue = 0;

         const wxString strValue = value;	// promote string, we need this for all

         if (!wxStrcmp(attr, wxT("maxsamples")))
         {
            // This attribute is a sample count, so can be 64bit
            if (!XMLValueChecker::IsGoodInt64(strValue) || !strValue.ToLongLong(&nValue) || (nValue < 0))
            {
               mErrorOpening = true;
               return false;
            }

            // Dominic, 12/10/2006:
            //    Let's check that maxsamples is >= 1024 and <= 64 * 1024 * 1024
            //    - that's a pretty wide range of reasonable values.
            if ((nValue < 1024) || (nValue > 64 * 1024 * 1024))
            {
               mErrorOpening = true;
               return false;
            }

            // nValue is now safe for size_t
            mMaxSamples = nValue;
         }
         else if (!wxStrcmp(attr, wxT("sampleformat")))
         {
            // This attribute is a sample format, normal int
            long fValue;
            if (!XMLValueChecker::IsGoodInt(strValue) || !strValue.ToLong(&fValue) || (fValue < 0) || !IsValidSampleFormat(fValue))
            {
               mErrorOpening = true;
               return false;
            }
            mSampleFormat = (sampleFormat)fValue;
         }
         else if (!wxStrcmp(attr, wxT("numsamples")))
         {
            // This attribute is a sample count, so can be 64bit
            if (!XMLValueChecker::IsGoodInt64(strValue) || !strValue.ToLongLong(&nValue) || (nValue < 0))
            {
               mErrorOpening = true;
               return false;
            }
            mNumSamples = nValue;
         }
      } // while

      return true;
   }

   return false;
}

void Sequence::HandleXMLEndTag(const wxChar *tag)
{
   if (wxStrcmp(tag, wxT("sequence")) != 0)
   {
      return;
   }

   // Make sure that the sequence is valid.

   // Make sure that start times and lengths are consistent
   sampleCount numSamples = 0;
   for (unsigned b = 0, nn = mBlock.size(); b < nn;  b++)
   {
      SeqBlock &block = mBlock[b];
      if (block.start != numSamples)
      {
         wxLogWarning(
            wxT("Gap detected in project file.\n")
            wxT("   Start (%s) for block file %lld is not one sample past end of previous block (%s).\n")
            wxT("   Moving start so blocks are contiguous."),
            // PRL:  Why bother with Internat when the above is just wxT?
            Internat::ToString(block.start.as_double(), 0),
            block.sb->GetBlockID(),
            Internat::ToString(numSamples.as_double(), 0));
         block.start = numSamples;
         mErrorOpening = true;
      }
      numSamples += block.sb->GetSampleCount();
   }

   if (mNumSamples != numSamples)
   {
      wxLogWarning(
         wxT("Gap detected in project file. Correcting sequence sample count from %s to %s."),
         // PRL:  Why bother with Internat when the above is just wxT?
         Internat::ToString(mNumSamples.as_double(), 0),
         Internat::ToString(numSamples.as_double(), 0));
      mNumSamples = numSamples;
      mErrorOpening = true;
   }
}

XMLTagHandler *Sequence::HandleXMLChild(const wxChar *tag)
{
   if (!wxStrcmp(tag, wxT("waveblock")))
   {
      return this;
   }

   return nullptr;
}

// Throws exceptions rather than reporting errors.
void Sequence::WriteXML(XMLWriter &xmlFile) const
// may throw
{
   unsigned int b;

   xmlFile.StartTag(wxT("sequence"));

   xmlFile.WriteAttr(wxT("maxsamples"), mMaxSamples);
   xmlFile.WriteAttr(wxT("sampleformat"), (size_t)mSampleFormat);
   xmlFile.WriteAttr(wxT("numsamples"), mNumSamples.as_long_long() );

   for (b = 0; b < mBlock.size(); b++) {
      const SeqBlock &bb = mBlock[b];

      // See http://bugzilla.audacityteam.org/show_bug.cgi?id=451.
      if (bb.sb->GetSampleCount() > mMaxSamples)
      {
         // PRL:  Bill observed this error.  Not sure how it was caused.
         // I have added code in ConsistencyCheck that should abort the
         // editing operation that caused this, not fixing
         // the problem but moving the point of detection earlier if we
         // find a reproducible case.
         auto sMsg =
            XO("Sequence has block file exceeding maximum %s samples per block.\nTruncating to this maximum length.")
               .Format( Internat::ToString(((wxLongLong)mMaxSamples).ToDouble(), 0) );
         AudacityMessageBox(
            sMsg,
            XO("Warning - Truncating Overlong Block File"),
            wxICON_EXCLAMATION | wxOK);
         wxLogWarning(sMsg.Translation()); //Debug?
//         bb.sb->SetLength(mMaxSamples);
      }

      xmlFile.StartTag(wxT("waveblock"));
      xmlFile.WriteAttr(wxT("start"), bb.start.as_long_long() );

      bb.sb->SaveXML(xmlFile);

      xmlFile.EndTag(wxT("waveblock"));
   }

   xmlFile.EndTag(wxT("sequence"));
}

int Sequence::FindBlock(sampleCount pos) const
{
   wxASSERT(pos >= 0 && pos < mNumSamples);

   if (pos == 0)
      return 0;

   int numBlocks = mBlock.size();

   size_t lo = 0, hi = numBlocks, guess;
   sampleCount loSamples = 0, hiSamples = mNumSamples;

   while (true) {
      //this is not a binary search, but a
      //dictionary search where we guess something smarter than the binary division
      //of the unsearched area, since samples are usually proportional to block file number.
      const double frac = (pos - loSamples).as_double() /
         (hiSamples - loSamples).as_double();
      guess = std::min(hi - 1, lo + size_t(frac * (hi - lo)));
      const SeqBlock &block = mBlock[guess];

      wxASSERT(block.sb->GetSampleCount() > 0);
      wxASSERT(lo <= guess && guess < hi && lo < hi);

      if (pos < block.start) {
         wxASSERT(lo != guess);
         hi = guess;
         hiSamples = block.start;
      }
      else {
         const sampleCount nextStart = block.start + block.sb->GetSampleCount();
         if (pos < nextStart)
            break;
         else {
            wxASSERT(guess < hi - 1);
            lo = guess + 1;
            loSamples = nextStart;
         }
      }
   }

   const int rval = guess;
   wxASSERT(rval >= 0 && rval < numBlocks &&
            pos >= mBlock[rval].start &&
            pos < mBlock[rval].start + mBlock[rval].sb->GetSampleCount());

   return rval;
}

//static
bool Sequence::Read(samplePtr buffer, sampleFormat format,
                    const SeqBlock &b, size_t blockRelativeStart, size_t len,
                    bool mayThrow)
{
   const auto &sb = b.sb;

   wxASSERT(blockRelativeStart + len <= sb->GetSampleCount());

   // Either throws, or of !mayThrow, tells how many were really read
   auto result = sb->GetSamples(buffer, format, blockRelativeStart, len, mayThrow);

   if (result != len)
   {
      wxLogWarning(wxT("Expected to read %ld samples, got %ld samples."),
                   len, result);
      return false;
   }

   return true;
}

bool Sequence::Get(samplePtr buffer, sampleFormat format,
   sampleCount start, size_t len, bool mayThrow) const
{
   if (start == mNumSamples) {
      return len == 0;
   }

   if (start < 0 || start + len > mNumSamples) {
      if (mayThrow)
         THROW_INCONSISTENCY_EXCEPTION;
      ClearSamples( buffer, floatSample, 0, len );
      return false;
   }
   int b = FindBlock(start);

   return Get(b, buffer, format, start, len, mayThrow);
}

bool Sequence::Get(int b, samplePtr buffer, sampleFormat format,
   sampleCount start, size_t len, bool mayThrow) const
{
   bool result = true;
   while (len) {
      const SeqBlock &block = mBlock[b];
      // start is in block
      const auto bstart = (start - block.start).as_size_t();
      // bstart is not more than block length
      const auto blen = std::min(len, block.sb->GetSampleCount() - bstart);

      if (! Read(buffer, format, block, bstart, blen, mayThrow) )
         result = false;

      len -= blen;
      buffer += (blen * SAMPLE_SIZE(format));
      b++;
      start += blen;
   }
   return result;
}

// Pass NULL to set silence
/*! @excsafety{Strong} */
void Sequence::SetSamples(constSamplePtr buffer, sampleFormat format,
                   sampleCount start, sampleCount len)
{
   auto &factory = *mpFactory;

   const auto size = mBlock.size();

   if (start < 0 || start + len > mNumSamples)
      THROW_INCONSISTENCY_EXCEPTION;

   size_t tempSize = mMaxSamples;
   // to do:  allocate this only on demand
   SampleBuffer scratch(tempSize, mSampleFormat);

   SampleBuffer temp;
   if (buffer && format != mSampleFormat) {
      temp.Allocate(tempSize, mSampleFormat);
   }

   int b = FindBlock(start);
   BlockArray newBlock;
   std::copy( mBlock.begin(), mBlock.begin() + b, std::back_inserter(newBlock) );

   while (len > 0
      // Redundant termination condition,
      // but it guards against infinite loop in case of inconsistencies
      // (too-small files, not yet seen?)
      // that cause the loop to make no progress because blen == 0
      && b < (int)size
   ) {
      newBlock.push_back( mBlock[b] );
      SeqBlock &block = newBlock.back();
      // start is within block
      const auto bstart = ( start - block.start ).as_size_t();
      const auto fileLength = block.sb->GetSampleCount();

      // the std::min is a guard against inconsistent Sequence
      const auto blen =
         limitSampleBufferSize( fileLength - std::min( bstart, fileLength ),
                                len );
      wxASSERT(blen == 0 || bstart + blen <= fileLength);

#if 0
      // PRL:  This inconsistency (too-big file) has been seen in "the wild"
      // in 2.2.0.  It is the least problematic kind of inconsistency.
      // We will tolerate it for 2.2.1.
      // Not known whether it is only in projects saved in earlier versions.
      // After 2.2.1, we should detect and correct it at file loading time.
      if (fileLength > mMaxSamples) {
         THROW_INCONSISTENCY_EXCEPTION;
      }
#endif

      ensureSampleBufferSize(scratch, mSampleFormat, tempSize, fileLength,
                             &temp);

      auto useBuffer = buffer;
      if (buffer && format != mSampleFormat)
      {
         // To do: remove the extra movement.
         // Note: we ensured temp can hold fileLength.  blen is not more
         CopySamples(buffer, format, temp.ptr(), mSampleFormat, blen);
         useBuffer = temp.ptr();
      }

      // We don't ever write to an existing block; to support Undo,
      // we copy the old block entirely into memory, dereference it,
      // make the change, and then write the NEW block to disk.

      if ( bstart > 0 || blen < fileLength ) {
         // First or last block is only partially overwritten
         Read(scratch.ptr(), mSampleFormat, block, 0, fileLength, true);

         if (useBuffer) {
            auto sampleSize = SAMPLE_SIZE(mSampleFormat);
            memcpy(scratch.ptr() +
                   bstart * sampleSize, useBuffer, blen * sampleSize);
         }
         else
            ClearSamples(scratch.ptr(), mSampleFormat, bstart, blen);

         block.sb = factory.Create(
            scratch.ptr(),
            fileLength,
            mSampleFormat);
      }
      else {
         // Avoid reading the disk when the replacement is total
         if (useBuffer)
            block.sb = factory.Create(useBuffer, fileLength, mSampleFormat);
         else
            block.sb = factory.CreateSilent(fileLength, mSampleFormat);
      }

      // blen might be zero for inconsistent Sequence...
      if( buffer )
         buffer += (blen * SAMPLE_SIZE(format));

      len -= blen;
      start += blen;

      // ... but this, at least, always guarantees some loop progress:
      b++;
   }

   std::copy( mBlock.begin() + b, mBlock.end(), std::back_inserter(newBlock) );

   CommitChangesIfConsistent( newBlock, mNumSamples, wxT("SetSamples") );
}

namespace {

struct MinMaxSumsq
{
   MinMaxSumsq(const float *pv, int count, int divisor)
   {
      min = FLT_MAX, max = -FLT_MAX, sumsq = 0.0f;
      while (count--) {
         float v;
         switch (divisor) {
         default:
         case 1:
            // array holds samples
            v = *pv++;
            if (v < min)
               min = v;
            if (v > max)
               max = v;
            sumsq += v * v;
            break;
         case 256:
         case 65536:
            // array holds triples of min, max, and rms values
            v = *pv++;
            if (v < min)
               min = v;
            v = *pv++;
            if (v > max)
               max = v;
            v = *pv++;
            sumsq += v * v;
            break;
         }
      }
   }

   float min;
   float max;
   float sumsq;
};

}

bool Sequence::GetWaveDisplay(float *min, float *max, float *rms, int* bl,
                              size_t len, const sampleCount *where) const
{
   wxASSERT(len > 0);
   const auto s0 = std::max(sampleCount(0), where[0]);
   if (s0 >= mNumSamples)
      // None of the samples asked for are in range. Abandon.
      return false;

   // In case where[len - 1] == where[len], raise the limit by one,
   // so we load at least one pixel for column len - 1
   // ... unless the mNumSamples ceiling applies, and then there are other defenses
   const auto s1 =
      std::min(mNumSamples, std::max(1 + where[len - 1], where[len]));
   Floats temp{ mMaxSamples };

   decltype(len) pixel = 0;

   auto srcX = s0;
   decltype(srcX) nextSrcX = 0;
   int lastRmsDenom = 0;
   int lastDivisor = 0;
   auto whereNow = std::min(s1 - 1, where[0]);
   decltype(whereNow) whereNext = 0;
   // Loop over block files, opening and reading and closing each
   // not more than once
   unsigned nBlocks = mBlock.size();
   const unsigned int block0 = FindBlock(s0);
   for (unsigned int b = block0; b < nBlocks; ++b) {
      if (b > block0)
         srcX = nextSrcX;
      if (srcX >= s1)
         break;

      // Find the range of sample values for this block that
      // are in the display.
      const SeqBlock &seqBlock = mBlock[b];
      const auto start = seqBlock.start;
      nextSrcX = std::min(s1, start + seqBlock.sb->GetSampleCount());

      // The column for pixel p covers samples from
      // where[p] up to but excluding where[p + 1].

      // Find the range of pixels covered by the current block file
      // (Their starting samples covered by it, to be exact)
      decltype(len) nextPixel;
      if (nextSrcX >= s1)
         // last pass
         nextPixel = len;
      else {
         nextPixel = pixel;
         // Taking min with s1 - 1, here and elsewhere, is another defense
         // to be sure the last pixel column gets at least one sample
         while (nextPixel < len &&
                (whereNext = std::min(s1 - 1, where[nextPixel])) < nextSrcX)
            ++nextPixel;
      }
      if (nextPixel == pixel)
         // The entire block's samples fall within one pixel column.
         // Either it's a rare odd block at the end, or else,
         // we must be really zoomed out!
         // Omit the entire block's contents from min/max/rms
         // calculation, which is not correct, but correctness might not
         // be worth the compute time if this happens every pixel
         // column. -- PRL
         continue;
      if (nextPixel == len)
         whereNext = s1;

      // Decide the summary level
      const double samplesPerPixel =
         (whereNext - whereNow).as_double() / (nextPixel - pixel);
      const int divisor =
           (samplesPerPixel >= 65536) ? 65536
         : (samplesPerPixel >= 256) ? 256
         : 1;

      int blockStatus = b;

      // How many samples or triples are needed?

      const size_t startPosition =
         // srcX and start are in the same block
         std::max(sampleCount(0), (srcX - start) / divisor).as_size_t();
      const size_t inclusiveEndPosition =
         // nextSrcX - 1 and start are in the same block
         std::min((sampleCount(mMaxSamples) / divisor) - 1,
                  (nextSrcX - 1 - start) / divisor).as_size_t();
      const auto num = 1 + inclusiveEndPosition - startPosition;
      if (num <= 0) {
         // What?  There was a zero length block file?
         wxASSERT(false);
         // Do some defense against this case anyway
         while (pixel < nextPixel) {
            min[pixel] = max[pixel] = rms[pixel] = 0;
            bl[pixel] = blockStatus;//MC
            ++pixel;
         }
         continue;
      }

      // Read from the block file or its summary
      switch (divisor) {
      default:
      case 1:
         // Read samples
         // no-throw for display operations!
         Read((samplePtr)temp.get(), floatSample, seqBlock, startPosition, num, false);
         break;
      case 256:
         // Read triples
         // Ignore the return value.
         // This function fills with zeroes if read fails
         seqBlock.sb->GetSummary256(temp.get(), startPosition, num);
         break;
      case 65536:
         // Read triples
         // Ignore the return value.
         // This function fills with zeroes if read fails
         seqBlock.sb->GetSummary64k(temp.get(), startPosition, num);
         break;
      }
      
      auto filePosition = startPosition;

      // The previous pixel column might straddle blocks.
      // If so, impute some of the data to it.
      if (b > block0 && pixel > 0) {
         // whereNow and start are in the same block
         auto midPosition = ((whereNow - start) / divisor).as_size_t();
         int diff(midPosition - filePosition);
         if (diff > 0) {
            MinMaxSumsq values(temp.get(), diff, divisor);
            const int lastPixel = pixel - 1;
            float &lastMin = min[lastPixel];
            lastMin = std::min(lastMin, values.min);
            float &lastMax = max[lastPixel];
            lastMax = std::max(lastMax, values.max);
            float &lastRms = rms[lastPixel];
            int lastNumSamples = lastRmsDenom * lastDivisor;
            lastRms = sqrt(
               (lastRms * lastRms * lastNumSamples + values.sumsq * divisor) /
               (lastNumSamples + diff * divisor)
            );

            filePosition = midPosition;
         }
      }

      // Loop over file positions
      int rmsDenom = 0;
      for (; filePosition <= inclusiveEndPosition;) {
         // Find range of pixel columns for this file position
         // (normally just one, but maybe more when zoomed very close)
         // and the range of positions for those columns
         // (normally one or more, for that one column)
         auto pixelX = pixel + 1;
         decltype(filePosition) positionX = 0;
         while (pixelX < nextPixel &&
            filePosition ==
               (positionX = (
                  // s1 - 1 or where[pixelX] and start are in the same block
                  (std::min(s1 - 1, where[pixelX]) - start) / divisor).as_size_t() )
         )
            ++pixelX;
         if (pixelX >= nextPixel)
            positionX = 1 + inclusiveEndPosition;

         // Find results to assign
         rmsDenom = (positionX - filePosition);
         wxASSERT(rmsDenom > 0);
         const float *const pv =
            temp.get() + (filePosition - startPosition) * (divisor == 1 ? 1 : 3);
         MinMaxSumsq values(pv, std::max(0, rmsDenom), divisor);

         // Assign results
         std::fill(&min[pixel], &min[pixelX], values.min);
         std::fill(&max[pixel], &max[pixelX], values.max);
         std::fill(&bl[pixel], &bl[pixelX], blockStatus);
         std::fill(&rms[pixel], &rms[pixelX], (float)sqrt(values.sumsq / rmsDenom));

         pixel = pixelX;
         filePosition = positionX;
      }

      wxASSERT(pixel == nextPixel);
      whereNow = whereNext;
      pixel = nextPixel;
      lastDivisor = divisor;
      lastRmsDenom = rmsDenom;
   } // for each block file

   wxASSERT(pixel == len);

   return true;
}

size_t Sequence::GetIdealAppendLen() const
{
   int numBlocks = mBlock.size();
   const auto max = GetMaxBlockSize();

   if (numBlocks == 0)
      return max;

   const auto lastBlockLen = mBlock.back().sb->GetSampleCount();
   if (lastBlockLen >= max)
      return max;
   else
      return max - lastBlockLen;
}

/*! @excsafety{Strong} */
SeqBlock::SampleBlockPtr Sequence::AppendNewBlock(
   constSamplePtr buffer, sampleFormat format, size_t len)
{
   return DoAppend( buffer, format, len, false );
}

/*! @excsafety{Strong} */
void Sequence::AppendSharedBlock(const SeqBlock::SampleBlockPtr &pBlock)
{
   auto len = pBlock->GetSampleCount();

   // Quick check to make sure that it doesn't overflow
   if (Overflows(mNumSamples.as_double() + ((double)len)))
      THROW_INCONSISTENCY_EXCEPTION;

   BlockArray newBlock;
   newBlock.emplace_back( pBlock, mNumSamples );
   auto newNumSamples = mNumSamples + len;

   AppendBlocksIfConsistent(newBlock, false,
                            newNumSamples, wxT("Append"));

// JKC: During generate we use Append again and again.
// If generating a long sequence this test would give O(n^2)
// performance - not good!
#ifdef VERY_SLOW_CHECKING
   ConsistencyCheck(wxT("Append"));
#endif
}

/*! @excsafety{Strong} */
void Sequence::Append(constSamplePtr buffer, sampleFormat format, size_t len)
{
   DoAppend(buffer, format, len, true);
}

/*! @excsafety{Strong} */
SeqBlock::SampleBlockPtr Sequence::DoAppend(
   constSamplePtr buffer, sampleFormat format, size_t len, bool coalesce)
{
   SeqBlock::SampleBlockPtr result;

   if (len == 0)
      return result;

   auto &factory = *mpFactory;

   // Quick check to make sure that it doesn't overflow
   if (Overflows(mNumSamples.as_double() + ((double)len)))
      THROW_INCONSISTENCY_EXCEPTION;

   BlockArray newBlock;
   sampleCount newNumSamples = mNumSamples;

   // If the last block is not full, we need to add samples to it
   int numBlocks = mBlock.size();
   SeqBlock *pLastBlock;
   decltype(pLastBlock->sb->GetSampleCount()) length;
   size_t bufferSize = mMaxSamples;
   SampleBuffer buffer2(bufferSize, mSampleFormat);
   bool replaceLast = false;
   if (coalesce &&
       numBlocks > 0 &&
       (length =
        (pLastBlock = &mBlock.back())->sb->GetSampleCount()) < mMinSamples) {
      // Enlarge a sub-minimum block at the end
      const SeqBlock &lastBlock = *pLastBlock;
      const auto addLen = std::min(mMaxSamples - length, len);

      Read(buffer2.ptr(), mSampleFormat, lastBlock, 0, length, true);

      CopySamples(buffer,
                  format,
                  buffer2.ptr() + length * SAMPLE_SIZE(mSampleFormat),
                  mSampleFormat,
                  addLen);

      const auto newLastBlockLen = length + addLen;
      SampleBlockPtr pBlock = factory.Create(
         buffer2.ptr(),
         newLastBlockLen,
         mSampleFormat);
      SeqBlock newLastBlock(pBlock, lastBlock.start);

      newBlock.push_back( newLastBlock );

      len -= addLen;
      newNumSamples += addLen;
      buffer += addLen * SAMPLE_SIZE(format);

      replaceLast = true;
   }
   // Append the rest as NEW blocks
   while (len) {
      const auto idealSamples = GetIdealBlockSize();
      const auto addedLen = std::min(idealSamples, len);
      SampleBlockPtr pBlock;
      if (format == mSampleFormat) {
         pBlock = factory.Create(buffer, addedLen, mSampleFormat);
         // It's expected that when not requesting coalescence, the
         // data should fit in one block
         wxASSERT( coalesce || !result );
         result = pBlock;
      }
      else {
         CopySamples(buffer, format, buffer2.ptr(), mSampleFormat, addedLen);
         pBlock = factory.Create(buffer2.ptr(), addedLen, mSampleFormat);
      }

      newBlock.push_back(SeqBlock(pBlock, newNumSamples));

      buffer += addedLen * SAMPLE_SIZE(format);
      newNumSamples += addedLen;
      len -= addedLen;
   }

   AppendBlocksIfConsistent(newBlock, replaceLast,
                            newNumSamples, wxT("Append"));

// JKC: During generate we use Append again and again.
// If generating a long sequence this test would give O(n^2)
// performance - not good!
#ifdef VERY_SLOW_CHECKING
   ConsistencyCheck(wxT("Append"));
#endif

   return result;
}

void Sequence::Blockify(SampleBlockFactory &factory,
                        size_t mMaxSamples, sampleFormat mSampleFormat,
                        BlockArray &list, sampleCount start,
                        constSamplePtr buffer, size_t len)
{
   if (len <= 0)
      return;

   auto num = (len + (mMaxSamples - 1)) / mMaxSamples;
   list.reserve(list.size() + num);

   for (decltype(num) i = 0; i < num; i++) {
      SeqBlock b;

      const auto offset = i * len / num;
      b.start = start + offset;
      int newLen = ((i + 1) * len / num) - offset;
      auto bufStart = buffer + (offset * SAMPLE_SIZE(mSampleFormat));

      b.sb = factory.Create(bufStart, newLen, mSampleFormat);

      list.push_back(b);
   }
}

/*! @excsafety{Strong} */
void Sequence::Delete(sampleCount start, sampleCount len)
{
   if (len == 0)
      return;

   if (len < 0 || start < 0 || start + len > mNumSamples)
      THROW_INCONSISTENCY_EXCEPTION;

   auto &factory = *mpFactory;

   const unsigned int numBlocks = mBlock.size();

   const unsigned int b0 = FindBlock(start);
   unsigned int b1 = FindBlock(start + len - 1);

   auto sampleSize = SAMPLE_SIZE(mSampleFormat);

   SeqBlock *pBlock;
   decltype(pBlock->sb->GetSampleCount()) length;

   // One buffer for reuse in various branches here
   SampleBuffer scratch;
   // The maximum size that should ever be needed
   auto scratchSize = mMaxSamples + mMinSamples;

   // Special case: if the samples to DELETE are all within a single
   // block and the resulting length is not too small, perform the
   // deletion within this block:
   if (b0 == b1 &&
       (length = (pBlock = &mBlock[b0])->sb->GetSampleCount()) - len >= mMinSamples) {
      SeqBlock &b = *pBlock;
      // start is within block
      auto pos = ( start - b.start ).as_size_t();

      // Guard against failure of this anyway below with limitSampleBufferSize
      wxASSERT(len < length);

      // len must be less than length
      // because start + len - 1 is also in the block...
      auto newLen = ( length - limitSampleBufferSize( length, len ) );

      scratch.Allocate(scratchSize, mSampleFormat);
      ensureSampleBufferSize(scratch, mSampleFormat, scratchSize, newLen);

      Read(scratch.ptr(), mSampleFormat, b, 0, pos, true);
      Read(scratch.ptr() + (pos * sampleSize), mSampleFormat,
           b,
           // ... and therefore pos + len
           // is not more than the length of the block
           ( pos + len ).as_size_t(), newLen - pos, true);

      b.sb = factory.Create(scratch.ptr(), newLen, mSampleFormat);

      // Don't make a duplicate array.  We can still give Strong-guarantee
      // if we modify only one block in place.

      // use No-fail-guarantee in remaining steps

      for (unsigned int j = b0 + 1; j < numBlocks; j++)
         mBlock[j].start -= len;

      mNumSamples -= len;

      // This consistency check won't throw, it asserts.
      // Proof that we kept consistency is not hard.
      ConsistencyCheck(wxT("Delete - branch one"), false);
      return;
   }

   // Create a NEW array of blocks
   BlockArray newBlock;
   newBlock.reserve(numBlocks - (b1 - b0) + 2);

   // Copy the blocks before the deletion point over to
   // the NEW array
   newBlock.insert(newBlock.end(), mBlock.begin(), mBlock.begin() + b0);
   unsigned int i;

   // First grab the samples in block b0 before the deletion point
   // into preBuffer.  If this is enough samples for its own block,
   // or if this would be the first block in the array, write it out.
   // Otherwise combine it with the previous block (splitting them
   // 50/50 if necessary).
   const SeqBlock &preBlock = mBlock[b0];
   // start is within preBlock
   auto preBufferLen = ( start - preBlock.start ).as_size_t();
   if (preBufferLen) {
      if (preBufferLen >= mMinSamples || b0 == 0) {
         if (!scratch.ptr())
            scratch.Allocate(scratchSize, mSampleFormat);
         ensureSampleBufferSize(scratch, mSampleFormat, scratchSize, preBufferLen);
         Read(scratch.ptr(), mSampleFormat, preBlock, 0, preBufferLen, true);
         auto pFile =
            factory.Create(scratch.ptr(), preBufferLen, mSampleFormat);

         newBlock.push_back(SeqBlock(pFile, preBlock.start));
      } else {
         const SeqBlock &prepreBlock = mBlock[b0 - 1];
         const auto prepreLen = prepreBlock.sb->GetSampleCount();
         const auto sum = prepreLen + preBufferLen;

         if (!scratch.ptr())
            scratch.Allocate(scratchSize, mSampleFormat);
         ensureSampleBufferSize(scratch, mSampleFormat, scratchSize,
                                sum);

         Read(scratch.ptr(), mSampleFormat, prepreBlock, 0, prepreLen, true);
         Read(scratch.ptr() + prepreLen*sampleSize, mSampleFormat,
              preBlock, 0, preBufferLen, true);

         newBlock.pop_back();
         Blockify(*mpFactory, mMaxSamples, mSampleFormat,
                  newBlock, prepreBlock.start, scratch.ptr(), sum);
      }
   }
   else {
      // The sample where we begin deletion happens to fall
      // right on the beginning of a block.
   }

   // Now, symmetrically, grab the samples in block b1 after the
   // deletion point into postBuffer.  If this is enough samples
   // for its own block, or if this would be the last block in
   // the array, write it out.  Otherwise combine it with the
   // subsequent block (splitting them 50/50 if necessary).
   const SeqBlock &postBlock = mBlock[b1];
   // start + len - 1 lies within postBlock
   const auto postBufferLen = (
       (postBlock.start + postBlock.sb->GetSampleCount()) - (start + len)
   ).as_size_t();
   if (postBufferLen) {
      if (postBufferLen >= mMinSamples || b1 == numBlocks - 1) {
         if (!scratch.ptr())
            // Last use of scratch, can ask for smaller
            scratch.Allocate(postBufferLen, mSampleFormat);
         // start + len - 1 lies within postBlock
         auto pos = (start + len - postBlock.start).as_size_t();
         Read(scratch.ptr(), mSampleFormat, postBlock, pos, postBufferLen, true);
         auto file =
            factory.Create(scratch.ptr(), postBufferLen, mSampleFormat);

         newBlock.push_back(SeqBlock(file, start));
      } else {
         SeqBlock &postpostBlock = mBlock[b1 + 1];
         const auto postpostLen = postpostBlock.sb->GetSampleCount();
         const auto sum = postpostLen + postBufferLen;

         if (!scratch.ptr())
            // Last use of scratch, can ask for smaller
            scratch.Allocate(sum, mSampleFormat);
         // start + len - 1 lies within postBlock
         auto pos = (start + len - postBlock.start).as_size_t();
         Read(scratch.ptr(), mSampleFormat, postBlock, pos, postBufferLen, true);
         Read(scratch.ptr() + (postBufferLen * sampleSize), mSampleFormat,
              postpostBlock, 0, postpostLen, true);

         Blockify(*mpFactory, mMaxSamples, mSampleFormat,
                  newBlock, start, scratch.ptr(), sum);
         b1++;
      }
   }
   else {
      // The sample where we begin deletion happens to fall
      // right on the end of a block.
   }

   // Copy the remaining blocks over from the old array
   for (i = b1 + 1; i < numBlocks; i++)
      newBlock.push_back(mBlock[i].Plus(-len));

   CommitChangesIfConsistent
      (newBlock, mNumSamples - len, wxT("Delete - branch two"));
}

void Sequence::ConsistencyCheck(const wxChar *whereStr, bool mayThrow) const
{
   ConsistencyCheck(mBlock, mMaxSamples, 0, mNumSamples, whereStr, mayThrow);
}

void Sequence::ConsistencyCheck
   (const BlockArray &mBlock, size_t maxSamples, size_t from,
    sampleCount mNumSamples, const wxChar *whereStr,
    bool WXUNUSED(mayThrow))
{
   // Construction of the exception at the appropriate line of the function
   // gives a little more discrimination
   Optional<InconsistencyException> ex;

   unsigned int numBlocks = mBlock.size();

   unsigned int i;
   sampleCount pos = from < numBlocks ? mBlock[from].start : mNumSamples;
   if ( from == 0 && pos != 0 )
      ex.emplace( CONSTRUCT_INCONSISTENCY_EXCEPTION );

   for (i = from; !ex && i < numBlocks; i++) {
      const SeqBlock &seqBlock = mBlock[i];
      if (pos != seqBlock.start)
         ex.emplace( CONSTRUCT_INCONSISTENCY_EXCEPTION );

      if ( seqBlock.sb ) {
         const auto length = seqBlock.sb->GetSampleCount();
         if (length > maxSamples)
            ex.emplace( CONSTRUCT_INCONSISTENCY_EXCEPTION );
         pos += length;
      }
      else
         ex.emplace( CONSTRUCT_INCONSISTENCY_EXCEPTION );
   }
   if ( !ex && pos != mNumSamples )
      ex.emplace( CONSTRUCT_INCONSISTENCY_EXCEPTION );

   if ( ex )
   {
      wxLogError(wxT("*** Consistency check failed at %d after %s. ***"),
                 ex->GetLine(), whereStr);
      wxString str;
      DebugPrintf(mBlock, mNumSamples, &str);
      wxLogError(wxT("%s"), str);
      wxLogError(wxT("*** Please report this error to https://forum.audacityteam.org/. ***\n\n")
                 wxT("Recommended course of action:\n")
                 wxT("Undo the failed operation(s), then export or save your work and quit."));

      //if (mayThrow)
         //throw *ex;
      //else
         wxASSERT(false);
   }
}

void Sequence::CommitChangesIfConsistent
   (BlockArray &newBlock, sampleCount numSamples, const wxChar *whereStr)
{
   ConsistencyCheck( newBlock, mMaxSamples, 0, numSamples, whereStr ); // may throw

   // now commit
   // use No-fail-guarantee

   mBlock.swap(newBlock);
   mNumSamples = numSamples;
}

void Sequence::AppendBlocksIfConsistent
(BlockArray &additionalBlocks, bool replaceLast,
 sampleCount numSamples, const wxChar *whereStr)
{
   // Any additional blocks are meant to be appended,
   // replacing the final block if there was one.

   if (additionalBlocks.empty())
      return;

   bool tmpValid = false;
   SeqBlock tmp;

   if ( replaceLast && ! mBlock.empty() ) {
      tmp = mBlock.back(), tmpValid = true;
      mBlock.pop_back();
   }

   auto prevSize = mBlock.size();

   bool consistent = false;
   auto cleanup = finally( [&] {
      if ( !consistent ) {
         mBlock.resize( prevSize );
         if ( tmpValid )
            mBlock.push_back( tmp );
      }
   } );

   std::copy( additionalBlocks.begin(), additionalBlocks.end(),
              std::back_inserter( mBlock ) );

   // Check consistency only of the blocks that were added,
   // avoiding quadratic time for repeated checking of repeating appends
   ConsistencyCheck( mBlock, mMaxSamples, prevSize, numSamples, whereStr ); // may throw

   // now commit
   // use No-fail-guarantee

   mNumSamples = numSamples;
   consistent = true;
}

void Sequence::DebugPrintf
   (const BlockArray &mBlock, sampleCount mNumSamples, wxString *dest)
{
   unsigned int i;
   decltype(mNumSamples) pos = 0;

   for (i = 0; i < mBlock.size(); i++) {
      const SeqBlock &seqBlock = mBlock[i];
      *dest += wxString::Format
         (wxT("   Block %3u: start %8lld, len %8lld, refs %ld, id %lld"),
          i,
          seqBlock.start.as_long_long(),
          seqBlock.sb ? (long long) seqBlock.sb->GetSampleCount() : 0,
          seqBlock.sb ? seqBlock.sb.use_count() : 0,
          seqBlock.sb ? (long long) seqBlock.sb->GetBlockID() : 0);

      if ((pos != seqBlock.start) || !seqBlock.sb)
         *dest += wxT("      ERROR\n");
      else
         *dest += wxT("\n");

      if (seqBlock.sb)
         pos += seqBlock.sb->GetSampleCount();
   }
   if (pos != mNumSamples)
      *dest += wxString::Format
         (wxT("ERROR mNumSamples = %lld\n"), mNumSamples.as_long_long());
}

// static
void Sequence::SetMaxDiskBlockSize(size_t bytes)
{
   sMaxDiskBlockSize = bytes;
}

size_t Sequence::GetMaxDiskBlockSize()
{
   return sMaxDiskBlockSize;
}

bool Sequence::IsValidSampleFormat(const int nValue)
{
   return (nValue == int16Sample) || (nValue == int24Sample) || (nValue == floatSample);
}
