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
   the audio BlockFiles on disk.
   Contrast with RingBuffer.

*//****************************************************************//**

\class SeqBlock
\brief Data structure containing pointer to a BlockFile and
   a start time. Element of a BlockArray.

*//*******************************************************************/


#include "Audacity.h"

#include <algorithm>
#include <float.h>
#include <math.h>

#include <wx/dynarray.h>
#include <wx/intl.h>
#include <wx/filefn.h>
#include <wx/ffile.h>
#include <wx/log.h>

#include "Sequence.h"

#include "BlockFile.h"
#include "blockfile/ODDecodeBlockFile.h"
#include "DirManager.h"

#include "blockfile/SimpleBlockFile.h"
#include "blockfile/SilentBlockFile.h"

int Sequence::sMaxDiskBlockSize = 1048576;

// Sequence methods
Sequence::Sequence(DirManager * projDirManager, sampleFormat format)
   : mDirManager(projDirManager)
   , mSampleFormat(format)
   , mNumSamples(0)
   , mMinSamples(sMaxDiskBlockSize / SAMPLE_SIZE(mSampleFormat) / 2)
   , mMaxSamples(mMinSamples * 2)
   , mErrorOpening(false)
{
   mDirManager->Ref();
}

// essentially a copy constructor - but you must pass in the
// current project's DirManager, because we might be copying
// from one project to another
Sequence::Sequence(const Sequence &orig, DirManager *projDirManager)
   : mDirManager(projDirManager)
   , mNumSamples(0)
   , mSampleFormat(orig.mSampleFormat)
   , mMinSamples(orig.mMinSamples)
   , mMaxSamples(orig.mMaxSamples)
   , mErrorOpening(false)
{
   mDirManager->Ref();

   bool bResult = Paste(0, &orig);
   wxASSERT(bResult); // TO DO: Actually handle this.
   (void)bResult;
}

Sequence::~Sequence()
{
   DerefAllFiles();
   mDirManager->Deref();
}

void Sequence::DerefAllFiles()
{
   for (size_t i = 0, nn = mBlock.size(); i < nn; i++)
   {
      BlockFile *& pOldFile = mBlock[i].f;
      if (pOldFile) {
         mDirManager->Deref(pOldFile);
         pOldFile = NULL;
      }
   }
}

sampleCount Sequence::GetMaxBlockSize() const
{
   return mMaxSamples;
}

sampleCount Sequence::GetIdealBlockSize() const
{
   return mMaxSamples;
}

bool Sequence::Lock()
{
   for (unsigned int i = 0; i < mBlock.size(); i++)
      mBlock[i].f->Lock();

   return true;
}

bool Sequence::CloseLock()
{
   for (unsigned int i = 0; i < mBlock.size(); i++)
      mBlock[i].f->CloseLock();

   return true;
}

bool Sequence::Unlock()
{
   for (unsigned int i = 0; i < mBlock.size(); i++)
      mBlock[i].f->Unlock();

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

bool Sequence::ConvertToSampleFormat(sampleFormat format, bool* pbChanged)
{
   wxASSERT(pbChanged);
   *pbChanged = false;

   // Caller should check this no-change case before calling; we ignore it here.
   if (format == mSampleFormat)
      return true;

   if (mBlock.size() == 0)
   {
      mSampleFormat = format;
      *pbChanged = true;
      return true;
   }

   const sampleFormat oldFormat = mSampleFormat;
   mSampleFormat = format;

   const sampleCount oldMinSamples = mMinSamples, oldMaxSamples = mMaxSamples;
   // These are the same calculations as in the constructor.
   mMinSamples = sMaxDiskBlockSize / SAMPLE_SIZE(mSampleFormat) / 2;
   mMaxSamples = mMinSamples * 2;

   BlockArray newBlockArray;
   // Use the ratio of old to NEW mMaxSamples to make a reasonable guess at allocation.
   newBlockArray.reserve(1 + mBlock.size() * ((float)oldMaxSamples / (float)mMaxSamples));

   bool bSuccess = true;
   {
      SampleBuffer bufferOld(mMaxSamples, oldFormat);
      SampleBuffer bufferNew(mMaxSamples, format);

      for (size_t i = 0, nn = mBlock.size(); i < nn && bSuccess; i++)
      {
         SeqBlock &oldSeqBlock = mBlock[i];
         BlockFile* oldBlockFile = oldSeqBlock.f;

         sampleCount len = oldBlockFile->GetLength();

         bSuccess = (oldBlockFile->ReadData(bufferOld.ptr(), oldFormat, 0, len) > 0);
         if (!bSuccess)
            break;

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
         const sampleCount blockstart = oldSeqBlock.start;
         const unsigned prevSize = newBlockArray.size();
         Blockify(newBlockArray, blockstart, bufferNew.ptr(), len);
         bSuccess = (newBlockArray.size() > prevSize);
         if (bSuccess)
            *pbChanged = true;
      }
   }

   if (bSuccess)
   {
      // Invalidate all the old, non-aliased block files.
      // Aliased files will be converted at save, per comment above.

      DerefAllFiles();

      // Replace with NEW blocks.
      mBlock.swap(newBlockArray);
   }
   else
   {
      /* vvvvv We *should do the following, but TrackPanel::OnFormatChange() doesn't actually check the conversion results,
         it just assumes the conversion was successful.
         TODO: Uncomment this section when TrackPanel::OnFormatChange() is upgraded to check the results.

         PRL:  I don't understand why the comment above justifies leaving the sequence in an inconsistent state.
         If this function must fail, better to leave it as a no-op on this sequence.  I am uncommenting the
         lines below, and adding one to revert mMinSamples too.
         */

      // Conversion failed. Revert these member vars.
      mSampleFormat = oldFormat;
      mMaxSamples = oldMaxSamples;
      mMinSamples = oldMinSamples;

      *pbChanged = false;  // Revert overall change flag, in case we had some partial success in the loop.
   }

   bSuccess &= ConsistencyCheck(wxT("Sequence::ConvertToSampleFormat()"));

   return bSuccess;
}

bool Sequence::GetMinMax(sampleCount start, sampleCount len,
                         float * outMin, float * outMax) const
{
   if (len == 0 || mBlock.size() == 0) {
      *outMin = float(0.0);   // FLT_MAX?  So it doesn't look like a spurious '0' to a caller?
      *outMax = float(0.0);   // -FLT_MAX?  So it doesn't look like a spurious '0' to a caller?
      return true;
   }

   float min = FLT_MAX;
   float max = -FLT_MAX;

   unsigned int block0 = FindBlock(start);
   unsigned int block1 = FindBlock(start + len - 1);

   sampleCount s0, l0, maxl0;

   // First calculate the min/max of the blocks in the middle of this region;
   // this is very fast because we have the min/max of every entire block
   // already in memory.

   for (unsigned b = block0 + 1; b < block1; ++b) {
      float blockMin, blockMax, blockRMS;
      mBlock[b].f->GetMinMax(&blockMin, &blockMax, &blockRMS);

      if (blockMin < min)
         min = blockMin;
      if (blockMax > max)
         max = blockMax;
   }

   // Now we take the first and last blocks into account, noting that the
   // selection may only partly overlap these blocks.  If the overall min/max
   // of either of these blocks is within min...max, then we can ignore them.
   // If not, we need read some samples and summaries from disk.
   {
      float block0Min, block0Max, block0RMS;
      const SeqBlock &theBlock = mBlock[block0];
      BlockFile *const theFile = theBlock.f;
      theFile->GetMinMax(&block0Min, &block0Max, &block0RMS);

      if (block0Min < min || block0Max > max) {
         s0 = start - theBlock.start;
         l0 = len;
         maxl0 = theBlock.start + theFile->GetLength() - start;
         wxASSERT(maxl0 <= mMaxSamples); // Vaughan, 2011-10-19
         if (l0 > maxl0)
            l0 = maxl0;

         float partialMin, partialMax, partialRMS;
         theFile->GetMinMax(s0, l0,
            &partialMin, &partialMax, &partialRMS);
         if (partialMin < min)
            min = partialMin;
         if (partialMax > max)
            max = partialMax;
      }
   }

   if (block1 > block0)
   {
      float block1Min, block1Max, block1RMS;
      const SeqBlock &theBlock = mBlock[block1];
      BlockFile *const theFile = theBlock.f;
      theFile->GetMinMax(&block1Min, &block1Max, &block1RMS);

      if (block1Min < min || block1Max > max) {

         s0 = 0;
         l0 = (start + len) - theBlock.start;
         wxASSERT(l0 <= mMaxSamples); // Vaughan, 2011-10-19

         float partialMin, partialMax, partialRMS;
         theFile->GetMinMax(s0, l0,
            &partialMin, &partialMax, &partialRMS);
         if (partialMin < min)
            min = partialMin;
         if (partialMax > max)
            max = partialMax;
      }
   }

   *outMin = min;
   *outMax = max;

   return true;
}

bool Sequence::GetRMS(sampleCount start, sampleCount len,
                         float * outRMS) const
{
   // len is the number of samples that we want the rms of.
   // it may be longer than a block, and the code is carefully set up to handle that.
   if (len == 0 || mBlock.size() == 0) {
      *outRMS = float(0.0);
      return true;
   }

   double sumsq = 0.0;
   sampleCount length = 0; // this is the cumulative length of the bits we have the ms of so far, and should end up == len

   unsigned int block0 = FindBlock(start);
   unsigned int block1 = FindBlock(start + len - 1);

   sampleCount s0, l0, maxl0;

   // First calculate the rms of the blocks in the middle of this region;
   // this is very fast because we have the rms of every entire block
   // already in memory.
   for (unsigned b = block0 + 1; b < block1; b++) {
      float blockMin, blockMax, blockRMS;
      const SeqBlock &theBlock = mBlock[b];
      BlockFile *const theFile = theBlock.f;
      theFile->GetMinMax(&blockMin, &blockMax, &blockRMS);

      const sampleCount fileLen = theFile->GetLength();
      sumsq += blockRMS * blockRMS * fileLen;
      length += fileLen;
   }

   // Now we take the first and last blocks into account, noting that the
   // selection may only partly overlap these blocks.
   // If not, we need read some samples and summaries from disk.
   {
      const SeqBlock &theBlock = mBlock[block0];
      BlockFile *const theFile = theBlock.f;
      s0 = start - theBlock.start;
      l0 = len;
      maxl0 = theBlock.start + theFile->GetLength() - start;
      wxASSERT(maxl0 <= mMaxSamples); // Vaughan, 2011-10-19
      if (l0 > maxl0)
         l0 = maxl0;

      float partialMin, partialMax, partialRMS;
      theFile->GetMinMax(s0, l0, &partialMin, &partialMax, &partialRMS);

      sumsq += partialRMS * partialRMS * l0;
      length += l0;
   }

   if (block1 > block0) {
      const SeqBlock &theBlock = mBlock[block1];
      BlockFile *const theFile = theBlock.f;

      s0 = 0;
      l0 = (start + len) - theBlock.start;
      wxASSERT(l0 <= mMaxSamples); // PRL: I think Vaughan missed this

      float partialMin, partialMax, partialRMS;
      theFile->GetMinMax(s0, l0, &partialMin, &partialMax, &partialRMS);
      sumsq += partialRMS * partialRMS * l0;
      length += l0;
   }

   // PRL: catch bugs like 1320:
   wxASSERT(length == len);

   *outRMS = sqrt(sumsq/length);

   return true;
}

bool Sequence::Copy(sampleCount s0, sampleCount s1, Sequence **dest)
{
   *dest = 0;

   if (s0 >= s1 || s0 >= mNumSamples || s1 < 0)
      return false;

   int numBlocks = mBlock.size();
   int b0 = FindBlock(s0);
   const int b1 = FindBlock(s1 - 1);
   wxASSERT(b0 >= 0);
   wxASSERT(b0 < numBlocks);
   wxASSERT(b1 < numBlocks);
   wxASSERT(b0 <= b1);

   *dest = new Sequence(mDirManager, mSampleFormat);
   (*dest)->mBlock.reserve(b1 - b0 + 1);

   SampleBuffer buffer(mMaxSamples, mSampleFormat);

   int blocklen;

   // Do the first block

   const SeqBlock &block0 = mBlock[b0];
   if (s0 != block0.start) {
      BlockFile *const file = block0.f;
      blocklen = std::min(s1, block0.start + file->GetLength()) - s0;
      wxASSERT(file->IsAlias() || (blocklen <= mMaxSamples)); // Vaughan, 2012-02-29
      Get(b0, buffer.ptr(), mSampleFormat, s0, blocklen);

      (*dest)->Append(buffer.ptr(), mSampleFormat, blocklen);
   }
   else
      --b0;

   // If there are blocks in the middle, copy the blockfiles directly
   for (int bb = b0 + 1; bb < b1; ++bb)
      (*dest)->AppendBlock(mBlock[bb]); // Increase ref count or duplicate file

   // Do the last block
   if (b1 > b0) {
      const SeqBlock &block = mBlock[b1];
      BlockFile *const file = block.f;
      blocklen = (s1 - block.start);
      wxASSERT(file->IsAlias() || (blocklen <= mMaxSamples)); // Vaughan, 2012-02-29
      if (blocklen < file->GetLength()) {
         Get(b1, buffer.ptr(), mSampleFormat, block.start, blocklen);
         (*dest)->Append(buffer.ptr(), mSampleFormat, blocklen);
      }
      else
         // Special case, copy exactly
         (*dest)->AppendBlock(block); // Increase ref count or duplicate file
   }

   return ConsistencyCheck(wxT("Sequence::Copy()"));
}

namespace {
   inline bool Overflows(double numSamples)
   {
      return numSamples > wxLL(9223372036854775807);
   }
}

bool Sequence::Paste(sampleCount s, const Sequence *src)
{
   if ((s < 0) || (s > mNumSamples))
   {
      wxLogError(
         wxT("Sequence::Paste: sampleCount s %s is < 0 or > mNumSamples %s)."),
         // PRL:  Why bother with Internat when the above is just wxT?
         Internat::ToString(((wxLongLong)s).ToDouble(), 0).c_str(),
         Internat::ToString(((wxLongLong)mNumSamples).ToDouble(), 0).c_str());
      wxASSERT(false);
      return false;
   }

   // Quick check to make sure that it doesn't overflow
   if (Overflows(((double)mNumSamples) + ((double)src->mNumSamples)))
   {
      wxLogError(
         wxT("Sequence::Paste: mNumSamples %s + src->mNumSamples %s would overflow."),
         // PRL:  Why bother with Internat when the above is just wxT?
         Internat::ToString(((wxLongLong)mNumSamples).ToDouble(), 0).c_str(),
         Internat::ToString(((wxLongLong)src->mNumSamples).ToDouble(), 0).c_str());
      wxASSERT(false);
      return false;
   }

   if (src->mSampleFormat != mSampleFormat)
   {
      wxLogError(
         wxT("Sequence::Paste: Sample format to be pasted, %s, does not match destination format, %s."),
         GetSampleFormatStr(src->mSampleFormat), GetSampleFormatStr(src->mSampleFormat));
      wxASSERT(false);
      return false;
   }

   const BlockArray &srcBlock = src->mBlock;
   sampleCount addedLen = src->mNumSamples;
   const unsigned int srcNumBlocks = srcBlock.size();
   int sampleSize = SAMPLE_SIZE(mSampleFormat);

   if (addedLen == 0 || srcNumBlocks == 0)
      return true;

   const size_t numBlocks = mBlock.size();

   if (numBlocks == 0 ||
       (s == mNumSamples && mBlock.back().f->GetLength() >= mMinSamples)) {
      // Special case: this track is currently empty, or it's safe to append
      // onto the end because the current last block is longer than the
      // minimum size

      for (unsigned int i = 0; i < srcNumBlocks; i++)
         AppendBlock(srcBlock[i]); // Increase ref count or duplicate file

      return ConsistencyCheck(wxT("Paste branch one"));
   }

   const int b = (s == mNumSamples) ? mBlock.size() - 1 : FindBlock(s);
   wxASSERT((b >= 0) && (b < (int)numBlocks));
   SeqBlock *const pBlock = &mBlock[b];
   const sampleCount length = pBlock->f->GetLength();
   const sampleCount largerBlockLen = addedLen + length;
   // PRL: when insertion point is the first sample of a block,
   // and the following test fails, perhaps we could test
   // whether coalescence with the previous block is possible.
   if (largerBlockLen <= mMaxSamples) {
      // Special case: we can fit all of the NEW samples inside of
      // one block!

      SeqBlock &block = *pBlock;
      SampleBuffer buffer(largerBlockLen, mSampleFormat);

      int splitPoint = s - block.start;
      Read(buffer.ptr(), mSampleFormat, block, 0, splitPoint);
      src->Get(0, buffer.ptr() + splitPoint*sampleSize,
               mSampleFormat, 0, addedLen);
      Read(buffer.ptr() + (splitPoint + addedLen)*sampleSize,
           mSampleFormat, block,
           splitPoint, length - splitPoint);

      BlockFile *const file =
         mDirManager->NewSimpleBlockFile(buffer.ptr(), largerBlockLen, mSampleFormat);

      mDirManager->Deref(block.f);
      block.f = file;

      for (unsigned int i = b + 1; i < numBlocks; i++)
         mBlock[i].start += addedLen;

      mNumSamples += addedLen;

      return ConsistencyCheck(wxT("Paste branch two"));
   }

   // Case three: if we are inserting four or fewer blocks,
   // it's simplest to just lump all the data together
   // into one big block along with the split block,
   // then resplit it all
   BlockArray newBlock;
   newBlock.reserve(numBlocks + srcNumBlocks + 2);
   newBlock.insert(newBlock.end(), mBlock.begin(), mBlock.begin() + b);

   SeqBlock &splitBlock = mBlock[b];
   sampleCount splitLen = splitBlock.f->GetLength();
   int splitPoint = s - splitBlock.start;

   unsigned int i;
   if (srcNumBlocks <= 4) {

      sampleCount sum = splitLen + addedLen;

      SampleBuffer sumBuffer(sum, mSampleFormat);
      Read(sumBuffer.ptr(), mSampleFormat, splitBlock, 0, splitPoint);
      src->Get(0, sumBuffer.ptr() + splitPoint * sampleSize,
               mSampleFormat,
               0, addedLen);
      Read(sumBuffer.ptr() + (splitPoint + addedLen) * sampleSize, mSampleFormat,
           splitBlock, splitPoint,
           splitLen - splitPoint);

      Blockify(newBlock, splitBlock.start, sumBuffer.ptr(), sum);
   } else {

      // The final case is that we're inserting at least five blocks.
      // We divide these into three groups: the first two get merged
      // with the first half of the split block, the middle ones get
      // copied in as is, and the last two get merged with the last
      // half of the split block.

      const sampleCount srcFirstTwoLen =
          srcBlock[0].f->GetLength() + srcBlock[1].f->GetLength();
      const sampleCount leftLen = splitPoint + srcFirstTwoLen;

      const SeqBlock &penultimate = srcBlock[srcNumBlocks - 2];
      const sampleCount srcLastTwoLen =
         penultimate.f->GetLength() +
         srcBlock[srcNumBlocks - 1].f->GetLength();
      const sampleCount rightSplit = splitBlock.f->GetLength() - splitPoint;
      const sampleCount rightLen = rightSplit + srcLastTwoLen;

      SampleBuffer sampleBuffer(std::max(leftLen, rightLen), mSampleFormat);

      Read(sampleBuffer.ptr(), mSampleFormat, splitBlock, 0, splitPoint);
      src->Get(0, sampleBuffer.ptr() + splitPoint*sampleSize,
         mSampleFormat, 0, srcFirstTwoLen);

      Blockify(newBlock, splitBlock.start, sampleBuffer.ptr(), leftLen);

      for (i = 2; i < srcNumBlocks - 2; i++) {
         const SeqBlock &block = srcBlock[i];
         BlockFile *const file = mDirManager->CopyBlockFile(block.f);
         if (!file) {
            wxASSERT(false); // TODO: Handle this better, alert the user of failure.
            return false;
         }

         newBlock.push_back(SeqBlock(file, block.start + s));
      }

      sampleCount lastStart = penultimate.start;
      src->Get(srcNumBlocks - 2, sampleBuffer.ptr(), mSampleFormat,
               lastStart, srcLastTwoLen);
      Read(sampleBuffer.ptr() + srcLastTwoLen * sampleSize, mSampleFormat,
           splitBlock, splitPoint, rightSplit);

      Blockify(newBlock, s + lastStart, sampleBuffer.ptr(), rightLen);
   }

   mDirManager->Deref(splitBlock.f);

   // Copy remaining blocks to NEW block array and
   // swap the NEW block array in for the old
   for (i = b + 1; i < numBlocks; i++)
      newBlock.push_back(mBlock[i].Plus(addedLen));

   mBlock.swap(newBlock);

   mNumSamples += addedLen;

   return ConsistencyCheck(wxT("Paste branch three"));
}

bool Sequence::SetSilence(sampleCount s0, sampleCount len)
{
   return Set(NULL, mSampleFormat, s0, len);
}

bool Sequence::InsertSilence(sampleCount s0, sampleCount len)
{
   // Quick check to make sure that it doesn't overflow
   if (Overflows(((double)mNumSamples) + ((double)len)))
      return false;

   if (len <= 0)
      return true;

   // Create a NEW track containing as much silence as we
   // need to insert, and then call Paste to do the insertion.
   // We make use of a SilentBlockFile, which takes up no
   // space on disk.

   Sequence sTrack(mDirManager, mSampleFormat);

   sampleCount idealSamples = GetIdealBlockSize();

   sampleCount pos = 0;

   sTrack.mBlock.reserve((len + idealSamples - 1) / idealSamples);

   BlockFile *silentFile = 0;
   if (len >= idealSamples)
      silentFile = new SilentBlockFile(idealSamples);
   while (len >= idealSamples) {
      sTrack.mBlock.push_back(SeqBlock(silentFile, pos));
      mDirManager->Ref(silentFile);

      pos += idealSamples;
      len -= idealSamples;
   }
   if (silentFile)
      mDirManager->Deref(silentFile);
   if (len) {
      sTrack.mBlock.push_back(SeqBlock(new SilentBlockFile(len), pos));
      pos += len;
   }

   sTrack.mNumSamples = pos;

   bool bResult = Paste(s0, &sTrack);
   wxASSERT(bResult);

   return bResult && ConsistencyCheck(wxT("InsertSilence"));
}

bool Sequence::AppendAlias(const wxString &fullPath,
                           sampleCount start,
                           sampleCount len, int channel, bool useOD)
{
   // Quick check to make sure that it doesn't overflow
   if (Overflows(((double)mNumSamples) + ((double)len)))
      return false;

   SeqBlock newBlock(
      useOD?
         mDirManager->NewODAliasBlockFile(fullPath, start, len, channel):
         mDirManager->NewAliasBlockFile(fullPath, start, len, channel),
      mNumSamples
   );
   mBlock.push_back(newBlock);
   mNumSamples += len;

   return true;
}

bool Sequence::AppendCoded(const wxString &fName, sampleCount start,
                            sampleCount len, int channel, int decodeType)
{
   // Quick check to make sure that it doesn't overflow
   if (Overflows(((double)mNumSamples) + ((double)len)))
      return false;

   SeqBlock newBlock(
      mDirManager->NewODDecodeBlockFile(fName, start, len, channel, decodeType),
      mNumSamples
   );
   mBlock.push_back(newBlock);
   mNumSamples += len;

   return true;
}

bool Sequence::AppendBlock(const SeqBlock &b)
{
   // Quick check to make sure that it doesn't overflow
   if (Overflows(((double)mNumSamples) + ((double)b.f->GetLength())))
      return false;

   SeqBlock newBlock(
      mDirManager->CopyBlockFile(b.f), // Bump ref count if not locked, else copy
      mNumSamples
   );
   if (!newBlock.f) {
      /// \todo Error Could not paste!  (Out of disk space?)
      wxASSERT(false); // TODO: Handle this better, alert the user of failure.
      return false;
   }

   //Don't need to Ref because it was done by CopyBlockFile, above...
   //mDirManager->Ref(newBlock.f);

   mBlock.push_back(newBlock);
   mNumSamples += newBlock.f->GetLength();

   // Don't do a consistency check here because this
   // function gets called in an inner loop.

   return true;
}

///gets an int with OD flags so that we can determine which ODTasks should be run on this track after save/open, etc.
unsigned int Sequence::GetODFlags()
{
   unsigned int ret = 0;
   for (unsigned int i = 0; i < mBlock.size(); i++) {
      BlockFile *const file = mBlock[i].f;
      if(!file->IsDataAvailable())
         ret |= (static_cast<ODDecodeBlockFile*>(file))->GetDecodeType();
      else if(!file->IsSummaryAvailable())
         ret |= ODTask::eODPCMSummary;
   }
   return ret;
}

sampleCount Sequence::GetBlockStart(sampleCount position) const
{
   int b = FindBlock(position);
   return mBlock[b].start;
}

sampleCount Sequence::GetBestBlockSize(sampleCount start) const
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
   sampleCount result = (block.start + block.f->GetLength() - start);

   sampleCount length;
   while(result < mMinSamples && b+1<numBlocks &&
         ((length = mBlock[b+1].f->GetLength()) + result) <= mMaxSamples) {
      b++;
      result += length;
   }

   wxASSERT(result > 0 && result <= mMaxSamples);

   return result;
}

bool Sequence::HandleXMLTag(const wxChar *tag, const wxChar **attrs)
{
   sampleCount nValue;

   /* handle waveblock tag and its attributes */
   if (!wxStrcmp(tag, wxT("waveblock"))) {
      SeqBlock wb;

      // loop through attrs, which is a null-terminated list of
      // attribute-value pairs
      while(*attrs) {
         const wxChar *attr = *attrs++;
         const wxChar *value = *attrs++;

         if (!value)
            break;

         // Both these attributes have non-negative integer counts of samples, so
         // we can test & convert here, making sure that values > 2^31 are OK
         // because long clips will need them.
         const wxString strValue = value;
         if (!XMLValueChecker::IsGoodInt64(strValue) || !strValue.ToLongLong(&nValue) || (nValue < 0))
         {
            mErrorOpening = true;
            wxLogWarning(
               wxT("   Sequence has bad %s attribute value, %s, that should be a positive integer."),
               attr, strValue.c_str());
            return false;
         }

         if (!wxStrcmp(attr, wxT("start")))
            wb.start = nValue;

         // Vaughan, 2011-10-10: I don't think we ever write a "len" attribute for "waveblock" tag,
         // so I think this is actually legacy code, or something intended, but not completed.
         // Anyway, might as well leave this code in, especially now that it has the check
         // against mMaxSamples.
         if (!wxStrcmp(attr, wxT("len")))
         {
            // mMaxSamples should already have been set by calls to the "sequence" clause below.
            // The check intended here was already done in DirManager::HandleXMLTag(), where
            // it let the block be built, then checked against mMaxSamples, and deleted the block
            // if the size of the block is bigger than mMaxSamples.
            if (nValue > mMaxSamples)
            {
               mErrorOpening = true;
               return false;
            }
            mDirManager->SetLoadingBlockLength(nValue);
         }
      } // while

      mBlock.push_back(wb);
      mDirManager->SetLoadingTarget(&mBlock, mBlock.size() - 1);

      return true;
   }

   /* handle sequence tag and its attributes */
   if (!wxStrcmp(tag, wxT("sequence"))) {
      while(*attrs) {
         const wxChar *attr = *attrs++;
         const wxChar *value = *attrs++;

         if (!value)
            break;

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
            mMaxSamples = nValue;

            // PRL:  Is the following really okay?  DirManager might be shared across projects!
            // PRL:  Yes, because it only affects DirManager's behavior in opening the project.
            mDirManager->SetLoadingMaxSamples(mMaxSamples);
         }
         else if (!wxStrcmp(attr, wxT("sampleformat")))
         {
            // This attribute is a sample format, normal int
            long fValue;
            if (!XMLValueChecker::IsGoodInt(strValue) || !strValue.ToLong(&fValue) || (fValue < 0) || !XMLValueChecker::IsValidSampleFormat(fValue))
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

      //// Both mMaxSamples and mSampleFormat should have been set.
      //// Check that mMaxSamples is right for mSampleFormat, using the calculations from the constructor.
      //if ((mMinSamples != sMaxDiskBlockSize / SAMPLE_SIZE(mSampleFormat) / 2) ||
      //      (mMaxSamples != mMinSamples * 2))
      //{
      //   mErrorOpening = true;
      //   return false;
      //}

      return true;
   }

   return false;
}

void Sequence::HandleXMLEndTag(const wxChar *tag)
{
   if (wxStrcmp(tag, wxT("sequence")) != 0)
      return;

   // Make sure that the sequence is valid.
   // First, replace missing blockfiles with SilentBlockFiles
   for (unsigned b = 0, nn = mBlock.size(); b < nn; b++) {
      SeqBlock &block = mBlock[b];
      if (!block.f) {
         sampleCount len;

         if (b < nn - 1)
            len = mBlock[b+1].start - block.start;
         else
            len = mNumSamples - block.start;

         if (len > mMaxSamples)
         {
            // This could be why the blockfile failed, so limit
            // the silent replacement to mMaxSamples.
            wxLogWarning(
               wxT("   Sequence has missing block file with length %s > mMaxSamples %s.\n      Setting length to mMaxSamples. This will likely cause some block files to be considered orphans."),
               // PRL:  Why bother with Internat when the above is just wxT?
               Internat::ToString(((wxLongLong)len).ToDouble(), 0).c_str(),
               Internat::ToString(((wxLongLong)mMaxSamples).ToDouble(), 0).c_str());
            len = mMaxSamples;
         }
         block.f = new SilentBlockFile(len);
         wxLogWarning(
            wxT("Gap detected in project file. Replacing missing block file with silence."));
         mErrorOpening = true;
      }
   }

   // Next, make sure that start times and lengths are consistent
   sampleCount numSamples = 0;
   for (unsigned b = 0, nn = mBlock.size(); b < nn;  b++) {
      SeqBlock &block = mBlock[b];
      if (block.start != numSamples) {
         wxString sFileAndExtension = block.f->GetFileName().GetFullName();
         if (sFileAndExtension.IsEmpty())
            sFileAndExtension = wxT("(replaced with silence)");
         else
            sFileAndExtension = wxT("\"") + sFileAndExtension + wxT("\"");
         wxLogWarning(
            wxT("Gap detected in project file.\n")
            wxT("   Start (%s) for block file %s is not one sample past end of previous block (%s).\n")
            wxT("   Moving start so blocks are contiguous."),
            // PRL:  Why bother with Internat when the above is just wxT?
            Internat::ToString(((wxLongLong)(block.start)).ToDouble(), 0).c_str(),
            sFileAndExtension.c_str(),
            Internat::ToString(((wxLongLong)numSamples).ToDouble(), 0).c_str());
         block.start = numSamples;
         mErrorOpening = true;
      }
      numSamples += block.f->GetLength();
   }
   if (mNumSamples != numSamples) {
      wxLogWarning(
         wxT("Gap detected in project file. Correcting sequence sample count from %s to %s."),
         // PRL:  Why bother with Internat when the above is just wxT?
         Internat::ToString(((wxLongLong)mNumSamples).ToDouble(), 0).c_str(),
         Internat::ToString(((wxLongLong)numSamples).ToDouble(), 0).c_str());
      mNumSamples = numSamples;
      mErrorOpening = true;
   }
}

XMLTagHandler *Sequence::HandleXMLChild(const wxChar *tag)
{
   if (!wxStrcmp(tag, wxT("waveblock")))
      return this;
   else {
      mDirManager->SetLoadingFormat(mSampleFormat);
      return mDirManager;
   }
}

void Sequence::WriteXML(XMLWriter &xmlFile)
{
   unsigned int b;

   xmlFile.StartTag(wxT("sequence"));

   xmlFile.WriteAttr(wxT("maxsamples"), mMaxSamples);
   xmlFile.WriteAttr(wxT("sampleformat"), mSampleFormat);
   xmlFile.WriteAttr(wxT("numsamples"), mNumSamples);

   for (b = 0; b < mBlock.size(); b++) {
      SeqBlock &bb = mBlock[b];

      // See http://bugzilla.audacityteam.org/show_bug.cgi?id=451.
      // Also, don't check against mMaxSamples for AliasBlockFiles, because if you convert sample format,
      // mMaxSample gets changed to match the format, but the number of samples in the aliased file
      // has not changed (because sample format conversion was not actually done in the aliased file.
      if (!bb.f->IsAlias() && (bb.f->GetLength() > mMaxSamples))
      {
         wxString sMsg =
            wxString::Format(
               _("Sequence has block file with length %s > mMaxSamples %s.\nTruncating to mMaxSamples."),
               Internat::ToString(((wxLongLong)bb.f->GetLength()).ToDouble(), 0).c_str(),
               Internat::ToString(((wxLongLong)mMaxSamples).ToDouble(), 0).c_str());
         wxMessageBox(sMsg, _("Warning - Length in Writing Sequence"), wxICON_EXCLAMATION | wxOK);
         wxLogWarning(sMsg);
         bb.f->SetLength(mMaxSamples);
      }

      xmlFile.StartTag(wxT("waveblock"));
      xmlFile.WriteAttr(wxT("start"), bb.start);

      bb.f->SaveXML(xmlFile);

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

   sampleCount lo = 0, loSamples = 0;
   sampleCount hi = numBlocks, hiSamples = mNumSamples;
   sampleCount guess;

   while (true) {
      //this is not a binary search, but a
      //dictionary search where we guess something smarter than the binary division
      //of the unsearched area, since samples are usually proportional to block file number.
      const double frac = double(pos - loSamples) / (hiSamples - loSamples);
      guess = std::min(hi - 1, lo + sampleCount(frac * (hi - lo)));
      const SeqBlock &block = mBlock[guess];

      wxASSERT(block.f->GetLength() > 0);
      wxASSERT(lo <= guess && guess < hi && lo < hi);

      if (pos < block.start) {
         wxASSERT(lo != guess);
         hi = guess;
         hiSamples = block.start;
      }
      else {
         const sampleCount nextStart = block.start + block.f->GetLength();
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
            pos < mBlock[rval].start + mBlock[rval].f->GetLength());

   return rval;
}

bool Sequence::Read(samplePtr buffer, sampleFormat format,
                    const SeqBlock &b, sampleCount start, sampleCount len) const
{
   BlockFile *f = b.f;

   wxASSERT(start >= 0);
   wxASSERT(start + len <= f->GetLength());

   int result = f->ReadData(buffer, format, start, len);

   if (result != len)
   {
      wxLogWarning(wxT("Expected to read %ld samples, got %d samples."), len, result);
      if (result < 0)
         result = 0;
      ClearSamples(buffer, format, result, len-result);
   }

   return true;
}

bool Sequence::CopyWrite(SampleBuffer &scratch,
                         samplePtr buffer, SeqBlock &b,
                         sampleCount start, sampleCount len)
{
   // We don't ever write to an existing block; to support Undo,
   // we copy the old block entirely into memory, dereference it,
   // make the change, and then write the NEW block to disk.

   const sampleCount length = b.f->GetLength();
   wxASSERT(length <= mMaxSamples);
   wxASSERT(start + len <= length);
   wxASSERT(start >= 0);

   int sampleSize = SAMPLE_SIZE(mSampleFormat);

   Read(scratch.ptr(), mSampleFormat, b, 0, length);
   memcpy(scratch.ptr() + start*sampleSize, buffer, len*sampleSize);

   BlockFile *const oldBlockFile = b.f;
   b.f = mDirManager->NewSimpleBlockFile(scratch.ptr(), length, mSampleFormat);

   mDirManager->Deref(oldBlockFile);

   return true;
}

bool Sequence::Get(samplePtr buffer, sampleFormat format,
   sampleCount start, sampleCount len) const
{
   if (start == mNumSamples) {
      return len == 0;
   }

   if (start < 0 || start > mNumSamples ||
      start + len > mNumSamples)
      return false;
   int b = FindBlock(start);

   return Get(b, buffer, format, start, len);
}

bool Sequence::Get(int b, samplePtr buffer, sampleFormat format,
   sampleCount start, sampleCount len) const
{
   while (len) {
      const SeqBlock &block = mBlock[b];
      const sampleCount bstart = (start - (block.start));
      const sampleCount blen =
          std::min(len, block.f->GetLength() - bstart);

      Read(buffer, format, block, bstart, blen);

      len -= blen;
      buffer += (blen * SAMPLE_SIZE(format));
      b++;
      start += blen;
   }

   return true;
}

// Pass NULL to set silence
bool Sequence::Set(samplePtr buffer, sampleFormat format,
                   sampleCount start, sampleCount len)
{
   if (start < 0 || start >= mNumSamples ||
       start+len > mNumSamples)
      return false;

   SampleBuffer scratch(mMaxSamples, mSampleFormat);

   SampleBuffer temp;
   if (buffer && format != mSampleFormat) {
      temp.Allocate(std::min(len, mMaxSamples), mSampleFormat);
   }

   int b = FindBlock(start);

   while (len) {
      SeqBlock &block = mBlock[b];
      const sampleCount bstart = start - block.start;
      const sampleCount fileLength = block.f->GetLength();
      const int blen =
         std::min(len, fileLength - bstart);

      if (buffer) {
         if (format == mSampleFormat)
            CopyWrite(scratch, buffer, block, bstart, blen);
         else {
            // To do: remove the extra movement.  Can we copy-samples within CopyWrite?
            CopySamples(buffer, format, temp.ptr(), mSampleFormat, blen);
            CopyWrite(scratch, temp.ptr(), block, bstart, blen);
         }
         buffer += (blen * SAMPLE_SIZE(format));
      }
      else {
         // If it's a full block of silence
         if (start == block.start &&
             blen == fileLength) {

            mDirManager->Deref(block.f);
            block.f = new SilentBlockFile(blen);
         }
         else {
            // Odd partial blocks of silence at start or end.
            temp.Allocate(blen, format);
            ClearSamples(temp.ptr(), format, 0, blen);
            // Otherwise write silence just to the portion of the block
            CopyWrite(scratch, temp.ptr(), block, bstart, blen);
         }
      }

      len -= blen;
      start += blen;
      b++;
   }

   return ConsistencyCheck(wxT("Set"));
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
                              int len, const sampleCount *where)
{
   const sampleCount s0 = std::max(sampleCount(0), where[0]);
   if (s0 >= mNumSamples)
      // None of the samples asked for are in range. Abandon.
      return false;

   // In case where[len - 1] == where[len], raise the limit by one,
   // so we load at least one pixel for column len - 1
   // ... unless the mNumSamples ceiling applies, and then there are other defenses
   const sampleCount s1 =
      std::min(mNumSamples, std::max(1 + where[len - 1], where[len]));
   float *temp = new float[mMaxSamples];

   int pixel = 0;

   sampleCount srcX = s0;
   sampleCount nextSrcX = 0;
   int lastRmsDenom = 0;
   int lastDivisor = 0;
   sampleCount whereNow = std::min(s1 - 1, where[0]);
   sampleCount whereNext = 0;
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
      SeqBlock &seqBlock = mBlock[b];
      const sampleCount start = seqBlock.start;
      nextSrcX = std::min(s1, start + seqBlock.f->GetLength());

      // The column for pixel p covers samples from
      // where[p] up to but excluding where[p + 1].

      // Find the range of pixels covered by the current block file
      // (Their starting samples covered by it, to be exact)
      int nextPixel;
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
         double(whereNext - whereNow) / (nextPixel - pixel);
      const int divisor =
           (samplesPerPixel >= 65536) ? 65536
         : (samplesPerPixel >= 256) ? 256
         : 1;

      int blockStatus = b;

      // How many samples or triples are needed?
      const sampleCount startPosition =
         std::max(sampleCount(0), (srcX - start) / divisor);
      const sampleCount inclusiveEndPosition =
         std::min((mMaxSamples / divisor) - 1, (nextSrcX - 1 - start) / divisor);
      const sampleCount num = 1 + inclusiveEndPosition - startPosition;
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
         Read((samplePtr)temp, floatSample, seqBlock, startPosition, num);
         break;
      case 256:
         // Read triples
         //check to see if summary data has been computed
         if (seqBlock.f->IsSummaryAvailable())
            seqBlock.f->Read256(temp, startPosition, num);
         else
            //otherwise, mark the display as not yet computed
            blockStatus = -1 - b;
         break;
      case 65536:
         // Read triples
         //check to see if summary data has been computed
         if (seqBlock.f->IsSummaryAvailable())
            seqBlock.f->Read64K(temp, startPosition, num);
         else
            //otherwise, mark the display as not yet computed
            blockStatus = -1 - b;
         break;
      }
      
      sampleCount filePosition = startPosition;

      // The previous pixel column might straddle blocks.
      // If so, impute some of the data to it.
      if (b > block0 && pixel > 0) {
         sampleCount midPosition = (whereNow - start) / divisor;
         int diff(midPosition - filePosition);
         if (diff > 0) {
            MinMaxSumsq values(temp, diff, divisor);
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
         int pixelX = pixel + 1;
         sampleCount positionX = 0;
         while (pixelX < nextPixel &&
            filePosition ==
                (positionX = (std::min(s1 - 1, where[pixelX]) - start) / divisor)
         )
            ++pixelX;
         if (pixelX >= nextPixel)
            positionX = 1 + inclusiveEndPosition;

         // Find results to assign
         rmsDenom = (positionX - filePosition);
         wxASSERT(rmsDenom > 0);
         const float *const pv =
            temp + (filePosition - startPosition) * (divisor == 1 ? 1 : 3);
         MinMaxSumsq values(pv, rmsDenom, divisor);

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

   delete[] temp;

   return true;
}

sampleCount Sequence::GetIdealAppendLen()
{
   int numBlocks = mBlock.size();
   const sampleCount max = GetMaxBlockSize();

   if (numBlocks == 0)
      return max;

   const sampleCount lastBlockLen = mBlock.back().f->GetLength();
   if (lastBlockLen == max)
      return max;
   else
      return max - lastBlockLen;
}

bool Sequence::Append(samplePtr buffer, sampleFormat format,
                      sampleCount len, XMLWriter* blockFileLog /*=NULL*/)
{
   // Quick check to make sure that it doesn't overflow
   if (Overflows(((double)mNumSamples) + ((double)len)))
      return false;

   // If the last block is not full, we need to add samples to it
   int numBlocks = mBlock.size();
   sampleCount length;
   SeqBlock *pLastBlock;
   SampleBuffer buffer2(mMaxSamples, mSampleFormat);
   if (numBlocks > 0 &&
       (length =
        (pLastBlock = &mBlock.back())->f->GetLength()) < mMinSamples) {
      SeqBlock &lastBlock = *pLastBlock;
      const sampleCount addLen = std::min(mMaxSamples - length, len);

      Read(buffer2.ptr(), mSampleFormat, lastBlock, 0, length);

      CopySamples(buffer,
                  format,
                  buffer2.ptr() + length * SAMPLE_SIZE(mSampleFormat),
                  mSampleFormat,
                  addLen);

      const int newLastBlockLen = length + addLen;

      SeqBlock newLastBlock(
         mDirManager->NewSimpleBlockFile(buffer2.ptr(), newLastBlockLen, mSampleFormat,
            blockFileLog != NULL),
         lastBlock.start
      );
      if (blockFileLog)
         static_cast<SimpleBlockFile*>(newLastBlock.f)->SaveXML(*blockFileLog);

      mDirManager->Deref(lastBlock.f);
      lastBlock = newLastBlock;

      len -= addLen;
      mNumSamples += addLen;
      buffer += addLen * SAMPLE_SIZE(format);
   }
   // Append the rest as NEW blocks
   while (len) {
      const sampleCount idealSamples = GetIdealBlockSize();
      const sampleCount l = std::min(idealSamples, len);
      BlockFile *pFile;
      if (format == mSampleFormat) {
         pFile = mDirManager->NewSimpleBlockFile(buffer, l, mSampleFormat,
                                                blockFileLog != NULL);
      }
      else {
         CopySamples(buffer, format, buffer2.ptr(), mSampleFormat, l);
         pFile = mDirManager->NewSimpleBlockFile(buffer2.ptr(), l, mSampleFormat,
                                                blockFileLog != NULL);
      }

      if (blockFileLog)
         static_cast<SimpleBlockFile*>(pFile)->SaveXML(*blockFileLog);

      mBlock.push_back(SeqBlock(pFile, mNumSamples));

      buffer += l * SAMPLE_SIZE(format);
      mNumSamples += l;
      len -= l;
   }

// JKC: During generate we use Append again and again.
// If generating a long sequence this test would give O(n^2)
// performance - not good!
#ifdef VERY_SLOW_CHECKING
   ConsistencyCheck(wxT("Append"));
#endif

   return true;
}

void Sequence::Blockify(BlockArray &list, sampleCount start, samplePtr buffer, sampleCount len)
{
   if (len <= 0)
      return;

   const int num = (len + (mMaxSamples - 1)) / mMaxSamples;
   list.reserve(list.size() + num);

   for (int i = 0; i < num; i++) {
      SeqBlock b;

      const sampleCount offset = i * len / num;
      b.start = start + offset;
      int newLen = ((i + 1) * len / num) - offset;
      samplePtr bufStart = buffer + (offset * SAMPLE_SIZE(mSampleFormat));

      b.f = mDirManager->NewSimpleBlockFile(bufStart, newLen, mSampleFormat);

      list.push_back(b);
   }
}

bool Sequence::Delete(sampleCount start, sampleCount len)
{
   if (len == 0)
      return true;
   if (len < 0 || start < 0 || start >= mNumSamples)
      return false;

   //TODO: add a ref-deref mechanism to SeqBlock/BlockArray so we don't have to make this a critical section.
   //On-demand threads iterate over the mBlocks and the GUI thread deletes them, so for now put a mutex here over
   //both functions,
   DeleteUpdateMutexLocker locker(*this);

   const unsigned int numBlocks = mBlock.size();

   const unsigned int b0 = FindBlock(start);
   unsigned int b1 = FindBlock(start + len - 1);

   int sampleSize = SAMPLE_SIZE(mSampleFormat);

   // Special case: if the samples to DELETE are all within a single
   // block and the resulting length is not too small, perform the
   // deletion within this block:
   SeqBlock *pBlock;
   sampleCount length;

   // One buffer for reuse in various branches here
   SampleBuffer scratch;
   // The maximum size that will ever be needed
   const sampleCount scratchSize = mMaxSamples + mMinSamples;

   if (b0 == b1 && (length = (pBlock = &mBlock[b0])->f->GetLength()) - len >= mMinSamples) {
      SeqBlock &b = *pBlock;
      sampleCount pos = start - b.start;
      sampleCount newLen = length - len;

      scratch.Allocate(scratchSize, mSampleFormat);

      Read(scratch.ptr(), mSampleFormat, b, 0, pos);
      Read(scratch.ptr() + (pos * sampleSize), mSampleFormat,
           b, pos + len, newLen - pos);

      BlockFile *const oldFile = b.f;
      b = SeqBlock(
         mDirManager->NewSimpleBlockFile(scratch.ptr(), newLen, mSampleFormat),
         b.start
      );
      mDirManager->Deref(oldFile);

      for (unsigned int j = b0 + 1; j < numBlocks; j++)
         mBlock[j].start -= len;

      mNumSamples -= len;

      return ConsistencyCheck(wxT("Delete - branch one"));
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
   sampleCount preBufferLen = start - preBlock.start;
   if (preBufferLen) {
      if (preBufferLen >= mMinSamples || b0 == 0) {
         if (!scratch.ptr())
            scratch.Allocate(scratchSize, mSampleFormat);
         Read(scratch.ptr(), mSampleFormat, preBlock, 0, preBufferLen);
         BlockFile *const pFile =
            mDirManager->NewSimpleBlockFile(scratch.ptr(), preBufferLen, mSampleFormat);

         newBlock.push_back(SeqBlock(pFile, preBlock.start));
      } else {
         const SeqBlock &prepreBlock = mBlock[b0 - 1];
         const sampleCount prepreLen = prepreBlock.f->GetLength();
         const sampleCount sum = prepreLen + preBufferLen;

         if (!scratch.ptr())
            scratch.Allocate(scratchSize, mSampleFormat);

         Read(scratch.ptr(), mSampleFormat, prepreBlock, 0, prepreLen);
         Read(scratch.ptr() + prepreLen*sampleSize, mSampleFormat,
              preBlock, 0, preBufferLen);

         newBlock.erase(newBlock.end() - 1);
         Blockify(newBlock, prepreBlock.start, scratch.ptr(), sum);

         mDirManager->Deref(prepreBlock.f);
      }
   }
   else {
      // The sample where we begin deletion happens to fall
      // right on the beginning of a block.
   }

   if (b0 != b1) {
      mDirManager->Deref(preBlock.f);
   }

   // Next, DELETE blocks strictly between b0 and b1
   for (i = b0 + 1; i < b1; i++) {
      mDirManager->Deref(mBlock[i].f);
   }

   // Now, symmetrically, grab the samples in block b1 after the
   // deletion point into postBuffer.  If this is enough samples
   // for its own block, or if this would be the last block in
   // the array, write it out.  Otherwise combine it with the
   // subsequent block (splitting them 50/50 if necessary).
   const SeqBlock &postBlock = mBlock[b1];
   sampleCount postBufferLen =
       (postBlock.start + postBlock.f->GetLength()) - (start + len);
   if (postBufferLen) {
      if (postBufferLen >= mMinSamples || b1 == numBlocks - 1) {
         if (!scratch.ptr())
            // Last use of scratch, can ask for smaller
            scratch.Allocate(postBufferLen, mSampleFormat);
         sampleCount pos = (start + len) - postBlock.start;
         Read(scratch.ptr(), mSampleFormat, postBlock, pos, postBufferLen);
         BlockFile *const file =
            mDirManager->NewSimpleBlockFile(scratch.ptr(), postBufferLen, mSampleFormat);

         newBlock.push_back(SeqBlock(file, start));
      } else {
         SeqBlock &postpostBlock = mBlock[b1 + 1];
         sampleCount postpostLen = postpostBlock.f->GetLength();
         sampleCount sum = postpostLen + postBufferLen;

         if (!scratch.ptr())
            // Last use of scratch, can ask for smaller
            scratch.Allocate(sum, mSampleFormat);
         sampleCount pos = (start + len) - postBlock.start;
         Read(scratch.ptr(), mSampleFormat, postBlock, pos, postBufferLen);
         Read(scratch.ptr() + (postBufferLen * sampleSize), mSampleFormat,
              postpostBlock, 0, postpostLen);

         Blockify(newBlock, start, scratch.ptr(), sum);
         b1++;

         mDirManager->Deref(postpostBlock.f);
      }
   }
   else {
      // The sample where we begin deletion happens to fall
      // right on the end of a block.
   }
   mDirManager->Deref(postBlock.f);

   // Copy the remaining blocks over from the old array
   for (i = b1 + 1; i < numBlocks; i++)
      newBlock.push_back(mBlock[i].Plus(-len));

   // Substitute our NEW array for the old one
   mBlock.swap(newBlock);

   // Update total number of samples and do a consistency check.
   mNumSamples -= len;

   return ConsistencyCheck(wxT("Delete - branch two"));
}

bool Sequence::ConsistencyCheck(const wxChar *whereStr) const
{
   unsigned int i;
   sampleCount pos = 0;
   unsigned int numBlocks = mBlock.size();
   bool bError = false;

   for (i = 0; !bError && i < numBlocks; i++) {
      const SeqBlock &seqBlock = mBlock[i];
      if (pos != seqBlock.start)
         bError = true;

      if (seqBlock.f)
         pos += seqBlock.f->GetLength();
      else
         bError = true;
   }
   if (pos != mNumSamples)
      bError = true;

   if (bError)
   {
      wxLogError(wxT("*** Consistency check failed after %s. ***"), whereStr);
      wxString str;
      DebugPrintf(&str);
      wxLogError(wxT("%s"), str.c_str());
      wxLogError(wxT("*** Please report this error to feedback@audacityteam.org. ***\n\n")
                 wxT("Recommended course of action:\n")
                 wxT("Undo the failed operation(s), then export or save your work and quit."));
   }

   return !bError;
}

void Sequence::DebugPrintf(wxString *dest) const
{
   unsigned int i;
   int pos = 0;

   for (i = 0; i < mBlock.size(); i++) {
      const SeqBlock &seqBlock = mBlock[i];
      *dest += wxString::Format
         (wxT("   Block %3u: start %8lld, len %8lld, refs %d, "),
          i,
          (long long) seqBlock.start,
          seqBlock.f ? (long long) seqBlock.f->GetLength() : 0,
          seqBlock.f ? mDirManager->GetRefCount(seqBlock.f) : 0);

      if (seqBlock.f)
         *dest += seqBlock.f->GetFileName().GetFullName();
      else
         *dest += wxT("<missing block file>");

      if ((pos != seqBlock.start) || !seqBlock.f)
         *dest += wxT("      ERROR\n");
      else
         *dest += wxT("\n");

      if (seqBlock.f)
         pos += seqBlock.f->GetLength();
   }
   if (pos != mNumSamples)
      *dest += wxString::Format
         (wxT("ERROR mNumSamples = %lld\n"), (long long) mNumSamples);
}

// static
void Sequence::SetMaxDiskBlockSize(int bytes)
{
   sMaxDiskBlockSize = bytes;
}

int Sequence::GetMaxDiskBlockSize()
{
   return sMaxDiskBlockSize;
}

void Sequence::AppendBlockFile(BlockFile* blockFile)
{
   // We assume blockFile has the correct ref count already

   mBlock.push_back(SeqBlock(blockFile, mNumSamples));
   mNumSamples += blockFile->GetLength();

   // PRL:  I hoisted the intended consistency check out of the inner loop
   // See RecordingRecoveryHandler::HandleXMLEndTag

#ifdef VERY_SLOW_CHECKING
   ConsistencyCheck(wxT("AppendBlockFile"));
#endif
}
