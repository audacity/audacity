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
{
   mDirManager = projDirManager;
   mDirManager->Ref();
   mNumSamples = 0;
   mSampleFormat = format;
   mBlock = new BlockArray();

   mMinSamples = sMaxDiskBlockSize / SAMPLE_SIZE(mSampleFormat) / 2;
   mMaxSamples = mMinSamples * 2;
   mErrorOpening = false;
}

Sequence::Sequence(const Sequence &orig, DirManager *projDirManager)
{
   // essentially a copy constructor - but you must pass in the
   // current project's DirManager, because we might be copying
   // from one project to another

   mDirManager = projDirManager;
   mDirManager->Ref();
   mNumSamples = 0;
   mSampleFormat = orig.mSampleFormat;
   mMaxSamples = orig.mMaxSamples;
   mMinSamples = orig.mMinSamples;
   mErrorOpening = false;

   mBlock = new BlockArray();

   Paste(0, &orig);
}

Sequence::~Sequence()
{
   for (unsigned int i = 0; i < mBlock->Count(); i++) {
      if (mBlock->Item(i)->f)
         mDirManager->Deref(mBlock->Item(i)->f);
      delete mBlock->Item(i);
   }

   delete mBlock;
   mDirManager->Deref();
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
   for (unsigned int i = 0; i < mBlock->Count(); i++)
      mBlock->Item(i)->f->Lock();

   return true;
}

bool Sequence::CloseLock()
{
   for (unsigned int i = 0; i < mBlock->Count(); i++)
      mBlock->Item(i)->f->CloseLock();

   return true;
}

bool Sequence::Unlock()
{
   for (unsigned int i = 0; i < mBlock->Count(); i++)
      mBlock->Item(i)->f->Unlock();

   return true;
}

sampleFormat Sequence::GetSampleFormat() const
{
   return mSampleFormat;
}

bool Sequence::SetSampleFormat(sampleFormat format)
{
   if (mBlock->Count() > 0 || mNumSamples > 0)
      return false;

   mSampleFormat = format;
   return true;
}

bool Sequence::ConvertToSampleFormat(sampleFormat format)
{
   if (format == mSampleFormat)
      return true;

   if (mBlock->Count() == 0) {
      mSampleFormat = format;
      return true;
   }

   sampleFormat oldFormat = mSampleFormat;
   mSampleFormat = format;

   for (unsigned int i = 0; i < mBlock->Count(); i++) {
      SeqBlock *b = mBlock->Item(i);
      BlockFile *oldBlock = b->f;
      sampleCount len = b->f->GetLength();

      if (!oldBlock->IsAlias()) {
         BlockFile *newBlock;

         samplePtr buffer1 = NewSamples(len, oldFormat);
         samplePtr buffer2 = NewSamples(len, mSampleFormat);

         oldBlock->ReadData(buffer1, oldFormat, 0, len);
         CopySamples(buffer1, oldFormat,
                     buffer2, mSampleFormat, len);
         newBlock = mDirManager->NewSimpleBlockFile(buffer2, len, mSampleFormat);

         mBlock->Item(i)->f = newBlock;
         mDirManager->Deref(oldBlock);

         DeleteSamples(buffer2);
         DeleteSamples(buffer1);
      }
   }

   return true;
}

bool Sequence::GetMinMax(sampleCount start, sampleCount len,
                         float * outMin, float * outMax) const
{
   if (len == 0 || mBlock->Count() == 0) {
      *outMin = float(0.0);
      *outMax = float(0.0);
      return true;
   }

   float min = FLT_MAX;
   float max = -FLT_MAX;

   unsigned int block0 = FindBlock(start);
   unsigned int block1 = FindBlock(start + len);

   sampleCount s0, l0, maxl0;

   // First calculate the min/max of the blocks in the middle of this region;
   // this is very fast because we have the min/max of every entire block
   // already in memory.
   unsigned int b;

   for (b = block0 + 1; b < block1; b++) {
      float blockMin, blockMax, blockRMS;
      mBlock->Item(b)->f->GetMinMax(&blockMin, &blockMax, &blockRMS);

      if (blockMin < min)
         min = blockMin;
      if (blockMax > max)
         max = blockMax;
   }

   // Now we take the first and last blocks into account, noting that the
   // selection may only partly overlap these blocks.  If the overall min/max
   // of either of these blocks is within min...max, then we can ignore them.
   // If not, we need read some samples and summaries from disk.
   float block0Min, block0Max, block0RMS;
   mBlock->Item(block0)->f->GetMinMax(&block0Min, &block0Max, &block0RMS);

   if (block0Min < min || block0Max > max) {
      s0 = start - mBlock->Item(block0)->start;
      l0 = len;
      maxl0 = mBlock->Item(block0)->start + mBlock->Item(block0)->f->GetLength() - start;
      if (l0 > maxl0)
         l0 = maxl0;

      float partialMin, partialMax, partialRMS;
      mBlock->Item(block0)->f->GetMinMax(s0, l0,
                                         &partialMin, &partialMax, &partialRMS);
      if (partialMin < min)
         min = partialMin;
      if (partialMax > max)
         max = partialMax;
   }

   float block1Min, block1Max, block1RMS;
   mBlock->Item(block1)->f->GetMinMax(&block1Min, &block1Max, &block1RMS);

   if (block1 > block0 &&
       (block1Min < min || block1Max > max)) {

      s0 = 0;
      l0 = (start + len) - mBlock->Item(block1)->start;

      float partialMin, partialMax, partialRMS;
      mBlock->Item(block1)->f->GetMinMax(s0, l0,
                                         &partialMin, &partialMax, &partialRMS);
      if (partialMin < min)
         min = partialMin;
      if (partialMax > max)
         max = partialMax;
   }

   *outMin = min;
   *outMax = max;

   return true;
}

bool Sequence::GetRMS(sampleCount start, sampleCount len,
                         float * outRMS) const
{
   if (len == 0 || mBlock->Count() == 0) {
      *outRMS = float(0.0);
      return true;
   }

   double sumsq = 0.0;
   sampleCount length = 0;

   unsigned int block0 = FindBlock(start);
   unsigned int block1 = FindBlock(start + len);

   sampleCount s0, l0, maxl0;

   // First calculate the rms of the blocks in the middle of this region;
   // this is very fast because we have the rms of every entire block
   // already in memory.
   unsigned int b;

   for (b = block0 + 1; b < block1; b++) {
      float blockMin, blockMax, blockRMS;
      mBlock->Item(b)->f->GetMinMax(&blockMin, &blockMax, &blockRMS);

      sumsq += blockRMS * blockRMS * mBlock->Item(block0)->f->GetLength();
      length += mBlock->Item(block0)->f->GetLength();
   }

   // Now we take the first and last blocks into account, noting that the
   // selection may only partly overlap these blocks.
   // If not, we need read some samples and summaries from disk.
   s0 = start - mBlock->Item(block0)->start;
   l0 = len;
   maxl0 = mBlock->Item(block0)->start + mBlock->Item(block0)->f->GetLength() - start;
   if (l0 > maxl0)
      l0 = maxl0;

   float partialMin, partialMax, partialRMS;
   mBlock->Item(block0)->f->GetMinMax(s0, l0, &partialMin, &partialMax, &partialRMS);

   sumsq += partialRMS * partialRMS * l0;
   length += l0;

   if (block1 > block0) {
      s0 = 0;
      l0 = (start + len) - mBlock->Item(block1)->start;

      mBlock->Item(block1)->f->GetMinMax(s0, l0,
                                         &partialMin, &partialMax, &partialRMS);
      sumsq += partialRMS * partialRMS * l0;
      length += l0;
   }

   *outRMS = sqrt(sumsq/length);

   return true;
}

bool Sequence::Copy(sampleCount s0, sampleCount s1, Sequence **dest)
{
   *dest = 0;

   if (s0 >= s1 || s0 >= mNumSamples || s1 < 0)
      return false;

   int numBlocks = mBlock->Count();
   int b0 = FindBlock(s0);
   int b1 = FindBlock(s1);

   if (s1 == mNumSamples)
      b1 = numBlocks;

   *dest = new Sequence(mDirManager, mSampleFormat);

   samplePtr buffer = NewSamples(mMaxSamples, mSampleFormat);

   int blocklen;

   // Do the first block

   if (b0 >= 0 && b0 < numBlocks && s0 != mBlock->Item(b0)->start) {

      blocklen = (mBlock->Item(b0)->start + mBlock->Item(b0)->f->GetLength() - s0);
      if (blocklen > (s1 - s0))
         blocklen = s1 - s0;
      Get(buffer, mSampleFormat, s0, blocklen);

      (*dest)->Append(buffer, mSampleFormat, blocklen);
   }

   if (b0 >= 0 && b0 < numBlocks && s0 == mBlock->Item(b0)->start) {
      b0--;
   }
   // If there are blocks in the middle, copy the blockfiles directly
   for (int b = b0 + 1; b < b1; b++)
      ((Sequence *)*dest)->AppendBlock(mBlock->Item(b));

   // Do the last block
   if (b1 > b0 && b1 < numBlocks) {
      blocklen = (s1 - mBlock->Item(b1)->start);
      Get(buffer, mSampleFormat, mBlock->Item(b1)->start, blocklen);
      (*dest)->Append(buffer, mSampleFormat, blocklen);
   }
   
   DeleteSamples(buffer);
   
   return true;
}

bool Sequence::Paste(sampleCount s, const Sequence *src)
{
   if (s < 0)
      s = 0;
   if (s >= mNumSamples)
      s = mNumSamples;

   // Quick check to make sure that it doesn't overflow
   if (((double)mNumSamples) + ((double)src->mNumSamples) > wxLL(9223372036854775807))
      return false;

   BlockArray *srcBlock = src->mBlock;
   sampleCount addedLen = src->mNumSamples;
   unsigned int srcNumBlocks = srcBlock->Count();
   int sampleSize = SAMPLE_SIZE(mSampleFormat);

   if (addedLen == 0 || srcNumBlocks == 0)
      return true;

   unsigned int b = FindBlock(s);
   unsigned int numBlocks = mBlock->Count();

   if (numBlocks == 0 ||
       (s == mNumSamples && mBlock->Item(numBlocks-1)->f->GetLength() >= mMinSamples)) {
      // Special case: this track is currently empty, or it's safe to append
      // onto the end because the current last block is longer than the
      // minimum size

      for (unsigned int i = 0; i < srcNumBlocks; i++)
         AppendBlock(srcBlock->Item(i));

      return ConsistencyCheck(wxT("Paste branch one"));
   }

   if (b >= 0 && b < numBlocks
       && mBlock->Item(b)->f->GetLength() + addedLen < mMaxSamples) {
      // Special case: we can fit all of the new samples inside of
      // one block!

      samplePtr buffer = NewSamples(mMaxSamples, mSampleFormat);

      int splitPoint = s - mBlock->Item(b)->start;
      Read(buffer, mSampleFormat, mBlock->Item(b), 0, splitPoint);
      src->Get(buffer + splitPoint*sampleSize,
               mSampleFormat, 0, addedLen);
      Read(buffer + (splitPoint + addedLen)*sampleSize,
           mSampleFormat, mBlock->Item(b),
           splitPoint, mBlock->Item(b)->f->GetLength() - splitPoint);

      SeqBlock *largerBlock = new SeqBlock();
      largerBlock->start = mBlock->Item(b)->start;
      int largerBlockLen = mBlock->Item(b)->f->GetLength() + addedLen;
      if (largerBlockLen > mMaxSamples) 
         largerBlockLen = mMaxSamples; // Prevent overruns, per NGS report for UmixIt.
      largerBlock->f =
         mDirManager->NewSimpleBlockFile(buffer, largerBlockLen, mSampleFormat);

      mDirManager->Deref(mBlock->Item(b)->f);
      delete mBlock->Item(b);
      mBlock->Item(b) = largerBlock;

      for (unsigned int i = b + 1; i < numBlocks; i++)
         mBlock->Item(i)->start += addedLen;

      mNumSamples += addedLen;

      DeleteSamples(buffer);

      return ConsistencyCheck(wxT("Paste branch two"));
   }

   // Case two: if we are inserting four or fewer blocks,
   // it's simplest to just lump all the data together
   // into one big block along with the split block,
   // then resplit it all
   unsigned int i;

   BlockArray *newBlock = new BlockArray();
   newBlock->Alloc(numBlocks + srcNumBlocks + 2);
   int newNumBlocks = 0;

   for (i = 0; i < b; i++) {
      newBlock->Add(mBlock->Item(i));
      newNumBlocks++;
   }

   SeqBlock *splitBlock = mBlock->Item(b);
   sampleCount splitLen = mBlock->Item(b)->f->GetLength();
   int splitPoint = s - splitBlock->start;

   if (srcNumBlocks <= 4) {

      sampleCount sum = splitLen + addedLen;

      samplePtr sumBuffer = NewSamples(sum, mSampleFormat);
      Read(sumBuffer, mSampleFormat, splitBlock, 0, splitPoint);
      src->Get(sumBuffer + splitPoint * sampleSize,
               mSampleFormat,
               0, addedLen);
      Read(sumBuffer + (splitPoint + addedLen) * sampleSize, mSampleFormat,
           splitBlock, splitPoint,
           splitBlock->f->GetLength() - splitPoint);

      BlockArray *split = Blockify(sumBuffer, sum);
      for (i = 0; i < split->Count(); i++) {
         split->Item(i)->start += splitBlock->start;
         newBlock->Add(split->Item(i));
         newNumBlocks++;
      }
      delete split;
      DeleteSamples(sumBuffer);
   } else {

      // The final case is that we're inserting at least five blocks.
      // We divide these into three groups: the first two get merged
      // with the first half of the split block, the middle ones get
      // copied in as is, and the last two get merged with the last
      // half of the split block.

      sampleCount srcFirstTwoLen =
          srcBlock->Item(0)->f->GetLength() + srcBlock->Item(1)->f->GetLength();
      sampleCount leftLen = splitPoint + srcFirstTwoLen;

      samplePtr leftBuffer = NewSamples(leftLen, mSampleFormat);
      Read(leftBuffer, mSampleFormat, splitBlock, 0, splitPoint);
      src->Get(leftBuffer + splitPoint*sampleSize,
               mSampleFormat, 0, srcFirstTwoLen);

      BlockArray *split = Blockify(leftBuffer, leftLen);
      for (i = 0; i < split->Count(); i++) {
         split->Item(i)->start += splitBlock->start;
         newBlock->Add(split->Item(i));
         newNumBlocks++;
      }
      delete split;
      DeleteSamples(leftBuffer);

      for (i = 2; i < srcNumBlocks - 2; i++) {
         SeqBlock *insertBlock = new SeqBlock();
         insertBlock->start = srcBlock->Item(i)->start + s;

         insertBlock->f = mDirManager->CopyBlockFile(srcBlock->Item(i)->f);
         if (!insertBlock->f) {
            // TODO error: Could not paste!  (Out of disk space?)
            return false;
         }

         newBlock->Add(insertBlock);
         newNumBlocks++;
      }

      sampleCount srcLastTwoLen =
         srcBlock->Item(srcNumBlocks - 2)->f->GetLength() +
         srcBlock->Item(srcNumBlocks - 1)->f->GetLength();
      sampleCount rightSplit = splitBlock->f->GetLength() - splitPoint;
      sampleCount rightLen = rightSplit + srcLastTwoLen;

      samplePtr rightBuffer = NewSamples(rightLen, mSampleFormat);
      sampleCount lastStart = srcBlock->Item(srcNumBlocks - 2)->start;
      src->Get(rightBuffer, mSampleFormat,
               lastStart, srcLastTwoLen);
      Read(rightBuffer + srcLastTwoLen * sampleSize, mSampleFormat,
           splitBlock, splitPoint, rightSplit);

      sampleCount pos = s + lastStart;

      split = Blockify(rightBuffer, rightLen);
      for (i = 0; i < split->Count(); i++) {
         split->Item(i)->start += pos;
         newBlock->Add(split->Item(i));
         newNumBlocks++;
      }
      delete split;
      DeleteSamples(rightBuffer);
   }

   mDirManager->Deref(splitBlock->f);
   delete splitBlock;

   // Copy remaining blocks to new block array and
   // swap the new block array in for the old
   for (i = b + 1; i < numBlocks; i++) {
      mBlock->Item(i)->start += addedLen;
      newBlock->Add(mBlock->Item(i));
      newNumBlocks++;
   }

   delete mBlock;
   mBlock = newBlock;

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
   if (((double)mNumSamples) + ((double)len) > wxLL(9223372036854775807))
      return false;

   // Create a new track containing as much silence as we
   // need to insert, and then call Paste to do the insertion.
   // We make use of a SilentBlockFile, which takes up no
   // space on disk.

   Sequence *sTrack = new Sequence(mDirManager, mSampleFormat);

   sampleCount idealSamples = GetIdealBlockSize();

   sampleCount pos = 0;

   while (len) {
      sampleCount l = (len > idealSamples ? idealSamples : len);

      SeqBlock *w = new SeqBlock();
      w->start = pos;
      w->f = new SilentBlockFile(l);

      sTrack->mBlock->Add(w);

      pos += l;
      len -= l;
   }

   sTrack->mNumSamples = pos;

   bool bResult = Paste(s0, sTrack);

   delete sTrack;

   return bResult && ConsistencyCheck(wxT("InsertSilence"));
}

bool Sequence::AppendAlias(wxString fullPath,
                           sampleCount start,
                           sampleCount len, int channel,bool useOD)
{
   // Quick check to make sure that it doesn't overflow
   if (((double)mNumSamples) + ((double)len) > wxLL(9223372036854775807))
      return false;

   SeqBlock *newBlock = new SeqBlock();

   newBlock->start = mNumSamples;
   newBlock->f = useOD?
      mDirManager->NewODAliasBlockFile(fullPath, start, len, channel):
      mDirManager->NewAliasBlockFile(fullPath, start, len, channel);
   mBlock->Add(newBlock);
   mNumSamples += newBlock->f->GetLength();

   return true;
}

bool Sequence::AppendCoded(wxString fName, sampleCount start,
                            sampleCount len, int channel, int decodeType)
{
   // Quick check to make sure that it doesn't overflow
   if (((double)mNumSamples) + ((double)len) > wxLL(9223372036854775807))
      return false;

   SeqBlock *newBlock = new SeqBlock();

   newBlock->start = mNumSamples;
   newBlock->f = mDirManager->NewODDecodeBlockFile(fName, start, len, channel, decodeType);
   mBlock->Add(newBlock);
   mNumSamples += newBlock->f->GetLength();

   return true;
}

bool Sequence::AppendBlock(SeqBlock * b)
{
   // Quick check to make sure that it doesn't overflow
   if (((double)mNumSamples) + ((double)b->f->GetLength()) > wxLL(9223372036854775807))
      return false;

   SeqBlock *newBlock = new SeqBlock();
   newBlock->start = mNumSamples;
   newBlock->f = mDirManager->CopyBlockFile(b->f);
   if (!newBlock->f) {
      /// \todo Error Could not paste!  (Out of disk space?)
      return false;
   }

   //Don't need to Ref because it was done by CopyBlockFile, above...
   //mDirManager->Ref(newBlock->f);

   mBlock->Add(newBlock);
   mNumSamples += newBlock->f->GetLength();

   // Don't do a consistency check here because this
   // function gets called in an inner loop

   return true;
}

///gets an int with OD flags so that we can determine which ODTasks should be run on this track after save/open, etc.
unsigned int Sequence::GetODFlags()
{
   unsigned int ret = 0;
   for (unsigned int i = 0; i < mBlock->Count(); i++){
      if(!mBlock->Item(i)->f->IsDataAvailable())
         ret = ret|((ODDecodeBlockFile*)mBlock->Item(i)->f)->GetDecodeType();
      else if(!mBlock->Item(i)->f->IsSummaryAvailable())
         ret = ret|ODTask::eODPCMSummary;
   }
   return ret;
}

sampleCount Sequence::GetBestBlockSize(sampleCount start) const
{
   // This method returns a nice number of samples you should try to grab in
   // one big chunk in order to land on a block boundary, based on the starting
   // sample.  The value returned will always be nonzero and will be no larger
   // than the value of GetMaxBlockSize();
   int b = FindBlock(start);
   int numBlocks = mBlock->Count();
   
   sampleCount result = (mBlock->Item(b)->start + mBlock->Item(b)->f->GetLength() - start);
   
   while(result < mMinSamples && b+1<numBlocks &&
         (mBlock->Item(b+1)->f->GetLength()+result) <= mMaxSamples) {
      b++;
      result += mBlock->Item(b)->f->GetLength();
   }
   
   wxASSERT(result > 0 && result <= mMaxSamples);
   
   return result;
}

bool Sequence::HandleXMLTag(const wxChar *tag, const wxChar **attrs)
{
   long nValue;

   if (!wxStrcmp(tag, wxT("waveblock"))) {
      SeqBlock *wb = new SeqBlock();
      wb->f = 0;
      wb->start = 0;

      // loop through attrs, which is a null-terminated list of
      // attribute-value pairs
      while(*attrs) {
         const wxChar *attr = *attrs++;
         const wxChar *value = *attrs++;
         
         if (!value)
            break;
         
         // All these attributes have non-negative integer values, so just test & convert here.
         const wxString strValue = value;
         if (!XMLValueChecker::IsGoodInt(strValue) || !strValue.ToLong(&nValue) || (nValue < 0))
         {
            mErrorOpening = true;
            return false;
         }
         
         if (!wxStrcmp(attr, wxT("start")))
            wb->start = nValue;

         if (!wxStrcmp(attr, wxT("len")))
         {
            if (nValue > mMaxSamples) // mMaxSamples should already have been set by calls to the "sequence" clause below. 
            {
               mErrorOpening = true;
               return false;
            }
            mDirManager->SetLoadingBlockLength(nValue);
         }
      } // while

      mBlock->Add(wb);
      mDirManager->SetLoadingTarget(&wb->f);

      return true;
   }
   
   if (!wxStrcmp(tag, wxT("sequence"))) {
      while(*attrs) {
         const wxChar *attr = *attrs++;
         const wxChar *value = *attrs++;
         
         if (!value)
            break;
         
         // All these attributes have non-negative integer values, so just test & convert here.
         const wxString strValue = value;
         if (!XMLValueChecker::IsGoodInt(strValue) || !strValue.ToLong(&nValue) || (nValue < 0))
         {
            mErrorOpening = true;
            return false;
         }

         if (!wxStrcmp(attr, wxT("maxsamples")))
         {
            // Dominic, 12/10/2006:
			   //    Let's check that maxsamples is >= 1024 and <= 64 * 1024 * 1024 
			   //    - that's a pretty wide range of reasonable values.
            if ((nValue < 1024) || (nValue > 64 * 1024 * 1024))
            {
               mErrorOpening = true;
               return false;
            }
            mMaxSamples = nValue;
            mDirManager->SetMaxSamples(mMaxSamples);
         }
         else if (!wxStrcmp(attr, wxT("sampleformat")))
         {
            if (!XMLValueChecker::IsValidSampleFormat(nValue))
            {
               mErrorOpening = true;
               return false;
            }
            mSampleFormat = (sampleFormat)nValue;
         }
         else if (!wxStrcmp(attr, wxT("numsamples")))
            mNumSamples = nValue;
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

   // Make sure that the sequence is valid
   // First, replace missing blockfiles with SilentBlockFiles
   unsigned int b;
   for (b = 0; b < mBlock->Count(); b++) {
      if (!mBlock->Item(b)->f) {
         sampleCount len;

         if (b < mBlock->Count()-1)
            len = mBlock->Item(b+1)->start - mBlock->Item(b)->start;
         else
            len = mNumSamples - mBlock->Item(b)->start;

         if (len > mMaxSamples) 
         	// This could be why the blockfile failed, so limit 
         	// the silent replacement to mMaxSamples.
            len = mMaxSamples;
         mBlock->Item(b)->f = new SilentBlockFile(len);
         wxLogWarning(_("Gap detected in project file\n"));
         mErrorOpening = true;
      }
   }

   // Next, make sure that start times and lengths are consistent
   sampleCount numSamples = 0;
   for (b = 0; b < mBlock->Count(); b++) {
      if (mBlock->Item(b)->start != numSamples) {
         mBlock->Item(b)->start = numSamples;
         wxLogWarning(_("Gap detected in project file\n"));
         mErrorOpening = true;         
      }
      numSamples += mBlock->Item(b)->f->GetLength();
   }
   if (mNumSamples != numSamples) {
      mNumSamples = numSamples;
      wxLogWarning(_("Gap detected in project file\n"));
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

   for (b = 0; b < mBlock->Count(); b++) {
      SeqBlock *bb = mBlock->Item(b);

      wxASSERT(bb->f->GetLength() <= mMaxSamples);  // it has been reported that this has been seen in aup files, see bug 451

      xmlFile.StartTag(wxT("waveblock"));
      xmlFile.WriteAttr(wxT("start"), bb->start);

      bb->f->SaveXML(xmlFile);

      xmlFile.EndTag(wxT("waveblock"));
   }

   xmlFile.EndTag(wxT("sequence"));
}

int Sequence::FindBlock(sampleCount pos, sampleCount lo,
                        sampleCount guess, sampleCount hi) const
{
   wxASSERT(mBlock->Item(guess)->f->GetLength() > 0);
   wxASSERT(lo <= guess && guess <= hi && lo <= hi);

   if (pos >= mBlock->Item(guess)->start &&
       pos < mBlock->Item(guess)->start + mBlock->Item(guess)->f->GetLength())
      return guess;

   //this is a binary search, but we probably could benefit by something more like
   //dictionary search where we guess something smarter than the binary division
   //of the unsearched area, since samples are usually proportional to block file number.
   if (pos < mBlock->Item(guess)->start)
      return FindBlock(pos, lo, (lo + guess) / 2, guess);
   else
      return FindBlock(pos, guess + 1, (guess + 1 + hi) / 2, hi);
}

int Sequence::FindBlock(sampleCount pos) const
{
   wxASSERT(pos >= 0 && pos <= mNumSamples);

   int numBlocks = mBlock->Count();

   if (pos == 0)
      return 0;

   if (pos == mNumSamples)
      return (numBlocks - 1);

   int rval = FindBlock(pos, 0, numBlocks / 2, numBlocks);

   wxASSERT(rval >= 0 && rval < numBlocks &&
            pos >= mBlock->Item(rval)->start &&
            pos < mBlock->Item(rval)->start + mBlock->Item(rval)->f->GetLength());

   return rval;
}

bool Sequence::Read(samplePtr buffer, sampleFormat format,
                    SeqBlock * b, sampleCount start, sampleCount len) const
{
   wxASSERT(b);
   wxASSERT(start >= 0);
   wxASSERT(start + len <= b->f->GetLength());

   BlockFile *f = b->f;

   int result = f->ReadData(buffer, format, start, len);

   if (result != len) 
   {
      wxLogError(wxT("Expected to read %d samples, got %d samples.\n"), len, result);
      if (result < 0)
         result = 0;
      ClearSamples(buffer, format, result, len-result);
   }

   return true;
}

bool Sequence::CopyWrite(samplePtr buffer, SeqBlock *b,
                         sampleCount start, sampleCount len)
{
   // We don't ever write to an existing block; to support Undo,
   // we copy the old block entirely into memory, dereference it,
   // make the change, and then write the new block to disk.

   wxASSERT(b);
   wxASSERT(b->f->GetLength() <= mMaxSamples);
   wxASSERT(start + len <= b->f->GetLength());

   int sampleSize = SAMPLE_SIZE(mSampleFormat);
   samplePtr newBuffer = NewSamples(mMaxSamples, mSampleFormat);
   wxASSERT(newBuffer);

   Read(newBuffer, mSampleFormat, b, 0, b->f->GetLength());
   memcpy(newBuffer + start*sampleSize, buffer, len*sampleSize);

   BlockFile *oldBlockFile = b->f;
   b->f = mDirManager->NewSimpleBlockFile(newBuffer, b->f->GetLength(), mSampleFormat);

   mDirManager->Deref(oldBlockFile);

   DeleteSamples(newBuffer);

   return true;
}

bool Sequence::Get(samplePtr buffer, sampleFormat format,
                   sampleCount start, sampleCount len) const
{
   if (start < 0 || start > mNumSamples ||
       start+len > mNumSamples)
      return false;
   int b = FindBlock(start);

   while (len) {
      sampleCount blen =
          mBlock->Item(b)->start + mBlock->Item(b)->f->GetLength() - start;
      if (blen > len)
         blen = len;
      sampleCount bstart = (start - (mBlock->Item(b)->start));

      Read(buffer, format, mBlock->Item(b), bstart, blen);

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
   if (start < 0 || start > mNumSamples ||
       start+len > mNumSamples)
      return false;

   samplePtr temp = NULL;
   if (format != mSampleFormat) {
      temp = NewSamples(mMaxSamples, mSampleFormat);
      wxASSERT(temp);
   }

   samplePtr silence = NULL;
   if (!buffer) {
      silence = NewSamples(mMaxSamples, format);
      wxASSERT(silence);
      ClearSamples(silence, format, 0, mMaxSamples);
   }

   int b = FindBlock(start);

   while (len) {
      int blen = mBlock->Item(b)->start + mBlock->Item(b)->f->GetLength() - start;
      if (blen > len)
         blen = len;

      if (buffer) {
         if (format == mSampleFormat)
            CopyWrite(buffer, mBlock->Item(b), start - mBlock->Item(b)->start,
                      blen);
         else {
            CopySamples(buffer, format, temp, mSampleFormat, blen);
            CopyWrite(temp, mBlock->Item(b), start - mBlock->Item(b)->start,
                      blen);
         }
         buffer += (blen * SAMPLE_SIZE(format));
      }
      else {
         // If it's a full block of silence
         if (start == mBlock->Item(b)->start &&
             blen == mBlock->Item(b)->f->GetLength()) {

            mDirManager->Deref(mBlock->Item(b)->f);
            mBlock->Item(b)->f = new SilentBlockFile(blen);
         }
         else {
            // Otherwise write silence just to the portion of the block
            CopyWrite(silence, mBlock->Item(b),
                      start - mBlock->Item(b)->start, blen);
         }
      }

      len -= blen;
      start += blen;
      b++;
   }

   if (!buffer)
      DeleteSamples(silence);

   if (format != mSampleFormat)
      DeleteSamples(temp);

   return ConsistencyCheck(wxT("Set"));
}

bool Sequence::GetWaveDisplay(float *min, float *max, float *rms,int* bl,
                              int len, sampleCount *where,
                              double samplesPerPixel)
{
   sampleCount s0 = where[0];
   sampleCount s1 = where[len];

   // None of the samples asked for are in range. Abandon.
   if (s0 >= mNumSamples)
      return false;

   int divisor;
   if (samplesPerPixel >= 65536)
      divisor = 65536;
   else if (samplesPerPixel >= 256)
      divisor = 256;
   else
      divisor = 1;

   if (s1 > mNumSamples)
      s1 = mNumSamples;

   sampleCount srcX = s0;
   
   unsigned int block0 = FindBlock(s0);

   float *temp = new float[mMaxSamples];

   int pixel = 0;
   float theMin = 0.0;
   float theMax = 0.0;
   float sumsq = float(0.0);
   unsigned int b = block0;
   int jcount = 0;
   int blockStatus = 1;

   while (srcX < s1) {
      // Get more samples
      sampleCount num;

      num = ((mBlock->Item(b)->f->GetLength() -
              (srcX - mBlock->Item(b)->start)) + divisor - 1)
         / divisor;

      if (num > (s1 - srcX + divisor - 1) / divisor)
         num = (s1 - srcX + divisor - 1) / divisor;
      
      switch (divisor) {
      default:
      case 1:
         Read((samplePtr)temp, floatSample, mBlock->Item(b),
              srcX - mBlock->Item(b)->start, num);
              
         blockStatus=b;
         break;
      case 256:
         //check to see if summary data has been computed
         if(mBlock->Item(b)->f->IsSummaryAvailable())
         {
            mBlock->Item(b)->f->Read256(temp,
                 (srcX - mBlock->Item(b)->start) / divisor, num);
            blockStatus=b;
         }
         else
         {
            //otherwise, mark the display as not yet computed
            blockStatus=-1-b;
         }
         break;
      case 65536:
         //check to see if summary data has been computed
         if(mBlock->Item(b)->f->IsSummaryAvailable())
         {
            mBlock->Item(b)->f->Read64K(temp,
                 (srcX - mBlock->Item(b)->start) / divisor, num);
            blockStatus=b;
         }
         else
         {
            blockStatus=-1-b;
         }
         break;
      }
      
      // Get min/max/rms of samples for each pixel we can
      int x = 0;

      if (b==block0) {
         if (divisor > 1) {
            theMin = temp[0];
            theMax = temp[1];
         }
         else {
            theMin = temp[0];
            theMax = temp[0];
         }
         sumsq = float(0.0);
         jcount = 0;
      }
      
      while (x < num) {
         
         while (pixel < len &&
                where[pixel] / divisor == srcX / divisor + x) {
            if (pixel > 0) {
               min[pixel - 1] = theMin;
               max[pixel - 1] = theMax;
               bl[pixel - 1] = blockStatus;//MC
               if (jcount > 0)
                  rms[pixel - 1] = (float)sqrt(sumsq / jcount);
               else
                  rms[pixel - 1] = 0.0f;
            }
            pixel++;
            if (where[pixel] != where[pixel - 1]) {
               theMin = FLT_MAX;
               theMax = -FLT_MAX;
               sumsq = float(0.0);
               jcount = 0;
            }
         }
         
         sampleCount stop = (where[pixel] - srcX) / divisor;
         if (stop == x)
            stop++;
         if (stop > num)
            stop = num;
         
         switch (divisor) {
         default:
         case 1:
            while (x < stop) {
               if (temp[x] < theMin)
                  theMin = temp[x];
               if (temp[x] > theMax)
                  theMax = temp[x];
               sumsq += ((float)temp[x]) * ((float)temp[x]);
               x++;
               jcount++;
            }
            break;
         case 256:
         case 65536:
            while (x < stop) {
               if (temp[3 * x] < theMin)
                  theMin = temp[3 * x];
               if (temp[3 * x + 1] > theMax)
                  theMax = temp[3 * x + 1];
               sumsq += ((float)temp[3*x+2]) * ((float)temp[3*x+2]);
               x++;
               jcount++;
            }
            
            break;
         }
      }

      b++;

      srcX += num * divisor;

      if (b >= mBlock->Count())
         break;

      srcX = mBlock->Item(b)->start;

   }
   
   // Make sure that min[pixel - 1] doesn't segfault
   if (pixel <= 0)
      pixel = 1;

   if (pixel == 0)
      pixel++;

   if (pixel == 0)
      pixel++;

   while (pixel <= len) {
      min[pixel - 1] = theMin;
      max[pixel - 1] = theMax;
      bl[pixel - 1] = blockStatus;//mchinen
      if (jcount > 0)
         rms[pixel - 1] = (float)sqrt(sumsq / jcount);
      else
         rms[pixel - 1] = 0.0f;
      pixel++;
   }

   delete[] temp;

   return true;
}

sampleCount Sequence::GetIdealAppendLen()
{
   int numBlocks = mBlock->Count();
   sampleCount max = GetMaxBlockSize();
   sampleCount lastBlockLen;

   if (numBlocks == 0)
      return max;

   lastBlockLen = mBlock->Item(numBlocks-1)->f->GetLength();
   if (lastBlockLen == max)
      return max;
   else
      return max - lastBlockLen;
}

bool Sequence::Append(samplePtr buffer, sampleFormat format,
                      sampleCount len, XMLWriter* blockFileLog /*=NULL*/)
{
   // Quick check to make sure that it doesn't overflow
   if (((double)mNumSamples) + ((double)len) > wxLL(9223372036854775807))
      return false;

   // If the last block is not full, we need to add samples to it
   int numBlocks = mBlock->Count();
   if (numBlocks > 0 && mBlock->Item(numBlocks - 1)->f->GetLength() < mMinSamples) {
      SeqBlock *lastBlock = mBlock->Item(numBlocks - 1);
      sampleCount addLen;
      if (lastBlock->f->GetLength() + len < mMaxSamples)
         addLen = len;
      else
         addLen = GetIdealBlockSize() - lastBlock->f->GetLength();

      SeqBlock *newLastBlock = new SeqBlock();

      samplePtr buffer2 = NewSamples((lastBlock->f->GetLength() + addLen), mSampleFormat);
      Read(buffer2, mSampleFormat, lastBlock, 0, lastBlock->f->GetLength());

      CopySamples(buffer,
                  format,
                  buffer2 + lastBlock->f->GetLength() * SAMPLE_SIZE(mSampleFormat),
                  mSampleFormat,
                  addLen);

      newLastBlock->start = lastBlock->start;
      int newLastBlockLen = lastBlock->f->GetLength() + addLen;

      newLastBlock->f =
         mDirManager->NewSimpleBlockFile(buffer2, newLastBlockLen, mSampleFormat,
                                         blockFileLog != NULL);
      if (blockFileLog)
         ((SimpleBlockFile*)newLastBlock->f)->SaveXML(*blockFileLog);
         
      DeleteSamples(buffer2);

      mDirManager->Deref(lastBlock->f);
      delete lastBlock;
      mBlock->Item(numBlocks - 1) = newLastBlock;

      len -= addLen;
      mNumSamples += addLen;
      buffer += addLen * SAMPLE_SIZE(format);
   }
   // Append the rest as new blocks
   samplePtr temp = NULL;
   if (format != mSampleFormat) {
      temp = NewSamples(mMaxSamples, mSampleFormat);
      wxASSERT(temp);
      if (!temp) {
         wxMessageBox(_("Memory allocation failed -- NewSamples"));
         return false;
      }
   }
   while (len) {
      sampleCount idealSamples = GetIdealBlockSize();
      sampleCount l = (len > idealSamples ? idealSamples : len);
      SeqBlock *w = new SeqBlock();
      w->start = mNumSamples;

      if (format == mSampleFormat) {
         w->f = mDirManager->NewSimpleBlockFile(buffer, l, mSampleFormat,
                                                blockFileLog != NULL);
      }
      else {
         CopySamples(buffer, format, temp, mSampleFormat, l);
         w->f = mDirManager->NewSimpleBlockFile(temp, l, mSampleFormat,
                                                blockFileLog != NULL);
      }

      if (blockFileLog)
         ((SimpleBlockFile*)w->f)->SaveXML(*blockFileLog);

      mBlock->Add(w);

      buffer += l * SAMPLE_SIZE(format);
      mNumSamples += l;
      len -= l;
   }
   if (temp)
      DeleteSamples(temp);

// JKC: During generate we use Append again and again.
// If generating a long sequence this test would give O(n^2) 
// performance - not good!
#ifdef VERY_SLOW_CHECKING
   ConsistencyCheck(wxT("Append"));
#endif

   return true;
}

BlockArray *Sequence::Blockify(samplePtr buffer, sampleCount len)
{
   BlockArray *list = new BlockArray();
   list->Alloc(10);

   if (len == 0)
      return list;

   int num = (len + (mMaxSamples - 1)) / mMaxSamples;

   for (int i = 0; i < num; i++) {
      SeqBlock *b = new SeqBlock();

      b->start = i * len / num;
      int newLen = ((i + 1) * len / num) - b->start;
      samplePtr bufStart = buffer + (b->start * SAMPLE_SIZE(mSampleFormat));

      b->f = mDirManager->NewSimpleBlockFile(bufStart, newLen, mSampleFormat);

      list->Add(b);
   }

   return list;
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
   LockDeleteUpdateMutex();

   unsigned int numBlocks = mBlock->Count();
   unsigned int newNumBlocks = 0;

   unsigned int b0 = FindBlock(start);
   unsigned int b1 = FindBlock(start + len - 1);

   int sampleSize = SAMPLE_SIZE(mSampleFormat);

   // Special case: if the samples to delete are all within a single
   // block and the resulting length is not too small, perform the
   // deletion within this block:
   if (b0 == b1 && mBlock->Item(b0)->f->GetLength() - len >= mMinSamples) {
      SeqBlock *b = mBlock->Item(b0);
      sampleCount pos = start - b->start;
      sampleCount newLen = b->f->GetLength() - len;

      samplePtr buffer = NewSamples(newLen, mSampleFormat);

      Read(buffer, mSampleFormat, b, 0, pos);
      Read(buffer + (pos * sampleSize), mSampleFormat,
           b, pos + len, newLen - pos);

      SeqBlock *newBlock = new SeqBlock();
      newBlock->start = b->start;
      newBlock->f =
         mDirManager->NewSimpleBlockFile(buffer, newLen, mSampleFormat);

      mBlock->Item(b0) = newBlock;

      for (unsigned int j = b0 + 1; j < numBlocks; j++)
         mBlock->Item(j)->start -= len;

      DeleteSamples(buffer);

      mDirManager->Deref(b->f);
      delete b;

      mNumSamples -= len;
      UnlockDeleteUpdateMutex();
      
      return ConsistencyCheck(wxT("Delete - branch one"));
   }

   // Create a new array of blocks
   BlockArray *newBlock = new BlockArray();
   newBlock->Alloc(numBlocks - (b1 - b0) + 2);

   // Copy the blocks before the deletion point over to
   // the new array
   unsigned int i;
   for (i = 0; i < b0; i++) {
      newBlock->Add(mBlock->Item(i));
      newNumBlocks++;
   }

   // First grab the samples in block b0 before the deletion point
   // into preBuffer.  If this is enough samples for its own block,
   // or if this would be the first block in the array, write it out.
   // Otherwise combine it with the previous block (splitting them
   // 50/50 if necessary).
   SeqBlock *preBlock = mBlock->Item(b0);
   sampleCount preBufferLen = start - preBlock->start;
   if (preBufferLen) {
      if (preBufferLen >= mMinSamples || b0 == 0) {
         SeqBlock *insBlock = new SeqBlock();

         insBlock->start = preBlock->start;

         samplePtr preBuffer = NewSamples(preBufferLen, mSampleFormat);
         Read(preBuffer, mSampleFormat, preBlock, 0, preBufferLen);
         insBlock->f =
            mDirManager->NewSimpleBlockFile(preBuffer, preBufferLen, mSampleFormat);
         DeleteSamples(preBuffer);

         newBlock->Add(insBlock);
         newNumBlocks++;

         if (b0 != b1) {
            mDirManager->Deref(preBlock->f);
            delete preBlock;
         }
      } else {
         SeqBlock *prepreBlock = mBlock->Item(b0 - 1);
         sampleCount prepreLen = prepreBlock->f->GetLength();
         sampleCount sum = prepreLen + preBufferLen;

         samplePtr sumBuffer = NewSamples(sum, mSampleFormat);

         Read(sumBuffer, mSampleFormat, prepreBlock, 0, prepreLen);
         Read(sumBuffer + prepreLen*sampleSize, mSampleFormat,
              preBlock, 0, preBufferLen);

         BlockArray *split = Blockify(sumBuffer, sum);
         split->Item(0)->start += prepreBlock->start;
         newBlock->Item(b0 - 1) = split->Item(0);
         for (i = 1; i < split->Count(); i++) {
            split->Item(i)->start += prepreBlock->start;
            newBlock->Add(split->Item(i));
            newNumBlocks++;
         }
         delete split;

         DeleteSamples(sumBuffer);

         mDirManager->Deref(prepreBlock->f);
         delete prepreBlock;

         if (b0 != b1) {
            mDirManager->Deref(preBlock->f);
            delete preBlock;
         }
      }
   } else {
      // The sample where we begin deletion happens to fall
      // right on the beginning of a block.
      if (b0 != b1) {
         mDirManager->Deref(mBlock->Item(b0)->f);
         delete mBlock->Item(b0);
      }
   }

   // Next, delete blocks strictly between b0 and b1
   for (i = b0 + 1; i < b1; i++) {
      mDirManager->Deref(mBlock->Item(i)->f);
      delete mBlock->Item(i);
   }

   // Now, symmetrically, grab the samples in block b1 after the
   // deletion point into postBuffer.  If this is enough samples
   // for its own block, or if this would be the last block in
   // the array, write it out.  Otherwise combine it with the
   // subsequent block (splitting them 50/50 if necessary).
   SeqBlock *postBlock = mBlock->Item(b1);
   sampleCount postBufferLen =
       (postBlock->start + postBlock->f->GetLength()) - (start + len);
   if (postBufferLen) {
      if (postBufferLen >= mMinSamples || b1 == numBlocks - 1) {
         SeqBlock *insBlock = new SeqBlock();

         insBlock->start = start;

         samplePtr postBuffer = NewSamples(postBufferLen, mSampleFormat);
         sampleCount pos = (start + len) - postBlock->start;
         Read(postBuffer, mSampleFormat, postBlock, pos, postBufferLen);
         insBlock->f =
            mDirManager->NewSimpleBlockFile(postBuffer, postBufferLen, mSampleFormat);

         DeleteSamples(postBuffer);

         newBlock->Add(insBlock);
         newNumBlocks++;

         mDirManager->Deref(postBlock->f);
         delete postBlock;
      } else {
         SeqBlock *postpostBlock = mBlock->Item(b1 + 1);
         sampleCount postpostLen = postpostBlock->f->GetLength();
         sampleCount sum = postpostLen + postBufferLen;

         samplePtr sumBuffer = NewSamples(sum, mSampleFormat);
         sampleCount pos = (start + len) - postBlock->start;
         Read(sumBuffer, mSampleFormat, postBlock, pos, postBufferLen);
         Read(sumBuffer + (postBufferLen * sampleSize), mSampleFormat,
              postpostBlock, 0, postpostLen);

         BlockArray *split = Blockify(sumBuffer, sum);
         for (i = 0; i < split->Count(); i++) {
            split->Item(i)->start += start;
            newBlock->Add(split->Item(i));
            newNumBlocks++;
         }
         delete split;
         b1++;

         DeleteSamples(sumBuffer);

         mDirManager->Deref(postpostBlock->f);
         delete postpostBlock;
         mDirManager->Deref(postBlock->f);
         delete postBlock;
      }
   } else {
      // The sample where we begin deletion happens to fall
      // right on the end of a block.
      mDirManager->Deref(mBlock->Item(b1)->f);
      delete mBlock->Item(b1);
   }

   // Copy the remaining blocks over from the old array
   for (i = b1 + 1; i < numBlocks; i++) {
      mBlock->Item(i)->start -= len;
      newBlock->Add(mBlock->Item(i));
      newNumBlocks++;
   }

   // Substitute our new array for the old one
   delete mBlock;
   mBlock = newBlock;

   // Update total number of samples and do a consistency check.
   mNumSamples -= len;
   
   UnlockDeleteUpdateMutex();
   return ConsistencyCheck(wxT("Delete - branch two"));
}

bool Sequence::ConsistencyCheck(const wxChar *whereStr)
{
   unsigned int i;
   sampleCount pos = 0;
   unsigned int numBlocks = mBlock->Count();
   bool bError = false;

   for (i = 0; i < numBlocks; i++) {
      if (pos != mBlock->Item(i)->start)
         bError = true;
      pos += mBlock->Item(i)->f->GetLength();
   }
   if (pos != mNumSamples)
      bError = true;

   if (bError) 
   {
      wxLogError(wxT("*** Consistency check failed after %s ***\n"), whereStr);
      wxString str;
      DebugPrintf(&str);
      wxLogError(wxT("%s"), str.c_str());
      wxLogError(wxT("*** Please report this error to feedback@audacityteam.org ***\n\nRecommended course of action:\nUndo the failed operation(s), then export or save your work and quit.\n"));   
   }

   return !bError;
}

void Sequence::DebugPrintf(wxString *dest)
{
   unsigned int i;
   int pos = 0;

   for (i = 0; i < mBlock->Count(); i++) {
      *dest += wxString::Format
         (wxT("   Block %3d: start %8d len %8d refs %d %s"),
          i,
          mBlock->Item(i)->start,
          mBlock->Item(i)->f->GetLength(),
          mDirManager->GetRefCount(mBlock->Item(i)->f),
          mBlock->Item(i)->f->GetFileName().GetFullName().c_str());
      if (pos != mBlock->Item(i)->start)
         *dest += wxT("      ERROR\n");
      else
         *dest += wxT("\n");
      pos += mBlock->Item(i)->f->GetLength();
   }
   if (pos != mNumSamples)
      *dest += wxString::Format
         (wxT("ERROR mNumSamples = %d\n"), mNumSamples);
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
   SeqBlock *w = new SeqBlock();
   w->start = mNumSamples;
   w->f = blockFile;
   mBlock->Add(w);
   mNumSamples += blockFile->GetLength();

#ifdef VERY_SLOW_CHECKING
   ConsistencyCheck(wxT("AppendBlockFile"));
#endif
}
