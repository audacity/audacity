/**********************************************************************

  Audacity: A Digital Audio Editor

  ODComputeSummaryTask.cpp

  Created by Michael Chinen (mchinen) on 6/8/08.
  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

******************************************************************//**

\class ODComputeSummaryTask
\brief Computes the summary data for a PCM (WAV) file and writes it to disk,
updating the ODPCMAliasBlockFile and the GUI of the newly available data.

*//*******************************************************************/



#include "ODComputeSummaryTask.h"
#include "../blockfile/ODPCMAliasBlockFile.h"
#include "../Sequence.h"
#include "../WaveTrack.h"
#include <wx/wx.h>

//36 blockfiles > 3 minutes stereo 44.1kHz per ODTask::DoSome
#define kNumBlockFilesPerDoSome 36

///Creates a NEW task that computes summaries for a wavetrack that needs to be specified through SetWaveTrack()
ODComputeSummaryTask::ODComputeSummaryTask()
{
   mMaxBlockFiles = 0;
   mComputedBlockFiles = 0;
   mHasUpdateRan=false;
}

movable_ptr<ODTask> ODComputeSummaryTask::Clone() const
{
   auto clone = make_movable<ODComputeSummaryTask>();
   clone->mDemandSample = GetDemandSample();
   // This std::move is needed to "upcast" the pointer type
   return std::move(clone);
}


///releases memory that the ODTask owns.  Subclasses should override.
void ODComputeSummaryTask::Terminate()
{
   //The terminate block won't allow DoSomeInternal and this method to be run async, so this is thread-safe.
   //Deref the block files since they are ref'ed when put into the array.
   mBlockFilesMutex.Lock();
   mBlockFiles.clear();
   mBlockFilesMutex.Unlock();
}

///Computes and writes the data for one BlockFile if it still has a refcount.
void ODComputeSummaryTask::DoSomeInternal()
{
   if(mBlockFiles.size()<=0)
   {
      mPercentCompleteMutex.Lock();
      mPercentComplete = 1.0;
      mPercentCompleteMutex.Unlock();
      return;
   }

   sampleCount blockStartSample = 0;
   sampleCount blockEndSample = 0;
   bool success =false;

   mBlockFilesMutex.Lock();
   for(size_t i=0; i < mWaveTracks.size() && mBlockFiles.size();i++)
   {
      const auto &bf = mBlockFiles[0];

      //first check to see if the ref count is at least 2.  It should have one
      //from when we added it to this instance's mBlockFiles array, and one from
      //the Wavetrack/sequence.  If it doesn't it has been deleted and we should forget it.
      if(bf.use_count() >= 2)
      {
         bf->DoWriteSummary();
         success = true;
         blockStartSample = bf->GetStart();
         blockEndSample = blockStartSample + bf->GetLength();
         mComputedBlockFiles++;
      }
      else
      {
         //the waveform in the wavetrack now is shorter, so we need to update mMaxBlockFiles
         //because now there is less work to do.
         mMaxBlockFiles--;
      }

      //take it out of the array - we are done with it.
      mBlockFiles.erase(mBlockFiles.begin());

      //This is a bit of a convenience in case someone tries to terminate the task by closing the trackpanel or window.
      //ODComputeSummaryTask::Terminate() uses this lock to remove everything, and we don't want it to wait since the UI is being blocked.
      mBlockFilesMutex.Unlock();
      wxThread::This()->Yield();
      mBlockFilesMutex.Lock();

      //upddate the gui for all associated blocks.  It doesn't matter that we're hitting more wavetracks then we should
      //because this loop runs a number of times equal to the number of tracks, they probably are getting processed in
      //the next iteration at the same sample window.
      mWaveTrackMutex.Lock();
      for(size_t i=0;i<mWaveTracks.size();i++)
      {
         if(success && mWaveTracks[i])
            mWaveTracks[i]->AddInvalidRegion(blockStartSample,blockEndSample);
      }
      mWaveTrackMutex.Unlock();
   }

   mBlockFilesMutex.Unlock();

   //update percentage complete.
   CalculatePercentComplete();
}

///compute the next time we should take a break in terms of overall percentage.
///We want to do a constant number of blockfiles.
float ODComputeSummaryTask::ComputeNextWorkUntilPercentageComplete()
{
   if(mMaxBlockFiles==0)
     return 1.0;

   float nextPercent;
   mPercentCompleteMutex.Lock();
   nextPercent=mPercentComplete + ((float)kNumBlockFilesPerDoSome/(mMaxBlockFiles+1));
   mPercentCompleteMutex.Unlock();

   return nextPercent;
}

void ODComputeSummaryTask::MarkUpdateRan()
{
   mHasUpdateRanMutex.Lock();
   mHasUpdateRan=true;
   mHasUpdateRanMutex.Unlock();
}

bool ODComputeSummaryTask::HasUpdateRan()
{
   bool ret;
   mHasUpdateRanMutex.Lock();
   ret = mHasUpdateRan;
   mHasUpdateRanMutex.Unlock();
   return ret;
}

void ODComputeSummaryTask::CalculatePercentComplete()
{
   bool hasUpdateRan;
   hasUpdateRan = HasUpdateRan();
   mPercentCompleteMutex.Lock();
   if(hasUpdateRan)
      mPercentComplete = (float) 1.0 - ((float)mBlockFiles.size() / (mMaxBlockFiles+1));
   else
      mPercentComplete =0.0;
   mPercentCompleteMutex.Unlock();
}

///creates the order of the wavetrack to load.
///by default left to right, or frome the point the user has clicked.
void ODComputeSummaryTask::Update()
{
   std::vector< std::shared_ptr< ODPCMAliasBlockFile > > tempBlocks;

   mWaveTrackMutex.Lock();

   for(size_t j=0;j<mWaveTracks.size();j++)
   {
      if(mWaveTracks[j])
      {
         BlockArray *blocks;
         Sequence *seq;

         //gather all the blockfiles that we should process in the wavetrack.
         for (const auto &clip : mWaveTracks[j]->GetAllClips()) {
            seq = clip->GetSequence();
            //This lock may be way too big since the whole file is one sequence.
            //TODO: test for large files and find a way to break it down.
            Sequence::DeleteUpdateMutexLocker locker(*seq);

            //See Sequence::Delete() for why need this for now..
            //We don't need the mBlockFilesMutex here because it is only for the vector list.
            //These are existing blocks, and its wavetrack or blockfiles won't be deleted because
            //of the respective mWaveTrackMutex lock and LockDeleteUpdateMutex() call.
            blocks = clip->GetSequenceBlockArray();
            int i;
            int insertCursor;

            insertCursor =0;//OD TODO:see if this works, removed from inner loop (bfore was n*n)

            for(i=0; i<(int)blocks->size(); i++)
            {
               //if there is data but no summary, this blockfile needs summarizing.
               SeqBlock &block = (*blocks)[i];
               const auto &file = block.f;
               if(file->IsDataAvailable() && !file->IsSummaryAvailable())
               {
                  const auto odpcmaFile =
                     std::static_pointer_cast<ODPCMAliasBlockFile>(file);
                  odpcmaFile->SetStart(block.start);
                  odpcmaFile->SetClipOffset(sampleCount(
                     clip->GetStartTime()*clip->GetRate()
                  ));

                  //these will always be linear within a sequence-lets take advantage of this by keeping a cursor.
                  while(insertCursor<(int)tempBlocks.size()&&
                     tempBlocks[insertCursor]->GetStart() + tempBlocks[insertCursor]->GetClipOffset() <
                        odpcmaFile->GetStart() + odpcmaFile->GetClipOffset())
                     insertCursor++;

                  tempBlocks.insert(tempBlocks.begin() + insertCursor++, odpcmaFile);
               }
            }
         }
      }
   }
   mWaveTrackMutex.Unlock();

   //get the NEW order.
   mBlockFilesMutex.Lock();
   OrderBlockFiles(tempBlocks);
   mBlockFilesMutex.Unlock();

   MarkUpdateRan();
}



///Computes the summary calculation queue order of the blockfiles
void ODComputeSummaryTask::OrderBlockFiles
   (std::vector< std::shared_ptr< ODPCMAliasBlockFile > > &unorderedBlocks)
{
   mBlockFiles.clear();
   //Order the blockfiles into our queue in a fancy convenient way.  (this could be user-prefs)
   //for now just put them in linear.  We start the order from the first block that includes the ondemand sample
   //(which the user sets by clicking.)
   //Note that this code assumes that the array is sorted in time.

   //find the startpoint
   auto processStartSample = GetDemandSample();
   for(int i= ((int)unorderedBlocks.size())-1;i>= 0;i--)
   {
      //check to see if the refcount is at least two before we add it to the list.
      //There should be one Ref() from the one added by this ODTask, and one from the track.
      //If there isn't, then the block was deleted for some reason and we should ignore it.
      if(unorderedBlocks[i].use_count() >= 2)
      {
         //test if the blockfiles are near the task cursor.  we use the last mBlockFiles[0] as our point of reference
         //and add ones that are closer.
         if(mBlockFiles.size() &&
            unorderedBlocks[i]->GetGlobalEnd() >= processStartSample &&
                ( mBlockFiles[0]->GetGlobalEnd() < processStartSample ||
                  unorderedBlocks[i]->GetGlobalStart() <= mBlockFiles[0]->GetGlobalStart())
            )
         {
            //insert at the front of the list if we get blockfiles that are after the demand sample
            mBlockFiles.insert(mBlockFiles.begin()+0,unorderedBlocks[i]);
         }
         else
         {
            //otherwise no priority
            mBlockFiles.push_back(unorderedBlocks[i]);
         }
         if(mMaxBlockFiles< (int) mBlockFiles.size())
            mMaxBlockFiles = mBlockFiles.size();
      }
      else
      {
         //Otherwise, let it be deleted and forget about it.
      }
   }
}
