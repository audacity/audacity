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
#include "../WaveClip.h"
#include "../WaveTrack.h"
#include <wx/wx.h>

//36 blockfiles > 3 minutes stereo 44.1kHz per ODTask::DoSome
#define nBlockFilesPerDoSome 36

///Creates a NEW task that computes summaries for a wavetrack that needs to be specified through SetWaveTrack()
ODComputeSummaryTask::ODComputeSummaryTask()
{
   mMaxBlockFiles = 0;
   mHasUpdateRan=false;
}

std::unique_ptr<ODTask> ODComputeSummaryTask::Clone() const
{
   auto clone = std::make_unique<ODComputeSummaryTask>();
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

   mBlockFilesMutex.Lock();
   for(size_t j=0; j < mWaveTracks.size() && mBlockFiles.size();j++)
   {
      bool success = false;
      const auto bf = mBlockFiles[0].lock();

      sampleCount blockStartSample = 0;
      sampleCount blockEndSample = 0;

      if(bf)
      {
         // WriteSummary might throw, but this is a worker thread, so stop
         // the exceptions here!
         success = GuardedCall<bool>( [&] {
            bf->DoWriteSummary();
            return true;
         } );
         blockStartSample = bf->GetStart();
         blockEndSample = blockStartSample + bf->GetLength();
      }
      else
      {
         success = true;
         // The block file disappeared.
         //the waveform in the wavetrack now is shorter, so we need to update mMaxBlockFiles
         //because now there is less work to do.
         mMaxBlockFiles--;
      }

      if (success)
      {
         //take it out of the array - we are done with it.
         mBlockFiles.erase(mBlockFiles.begin());
      }
      else
         // The task does not make progress
         ;

      //This is a bit of a convenience in case someone tries to terminate the task by closing the trackpanel or window.
      //ODComputeSummaryTask::Terminate() uses this lock to remove everything, and we don't want it to wait since the UI is being blocked.
      mBlockFilesMutex.Unlock();
      wxThread::This()->Yield();
      mBlockFilesMutex.Lock();

      //update the gui for all associated blocks.  It doesn't matter that we're hitting more wavetracks then we should
      //because this loop runs a number of times equal to the number of tracks, they probably are getting processed in
      //the next iteration at the same sample window.
      if (success && bf) {
         mWaveTrackMutex.Lock();
         for(size_t i=0;i<mWaveTracks.size();i++)
         {
            auto waveTrack = mWaveTracks[i].lock();
            if(success && waveTrack)
            {
#if 0 // LLL: Commented out while removing OD file handling              
               waveTrack->AddInvalidRegion(blockStartSample,blockEndSample);
#endif
            }
         }
         mWaveTrackMutex.Unlock();
      }
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
   nextPercent=mPercentComplete + ((float)nBlockFilesPerDoSome/(mMaxBlockFiles+1));
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
   std::vector< std::weak_ptr< ODPCMAliasBlockFile > > tempBlocks;

   mWaveTrackMutex.Lock();

   for(size_t j=0;j<mWaveTracks.size();j++)
   {
      auto waveTrack = mWaveTracks[j].lock();
      if(waveTrack)
      {
         BlockArray *blocks;
         Sequence *seq;

         //gather all the blockfiles that we should process in the wavetrack.
         for (const auto &clip : waveTrack->GetAllClips()) {
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
                  {
                     std::shared_ptr< ODPCMAliasBlockFile > ptr;
                     while(insertCursor < (int)tempBlocks.size() &&
                           (!(ptr = tempBlocks[insertCursor].lock()) ||
                            ptr->GetStart() + ptr->GetClipOffset() <
                            odpcmaFile->GetStart() + odpcmaFile->GetClipOffset()))
                        insertCursor++;
                  }

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
   (std::vector< std::weak_ptr< ODPCMAliasBlockFile > > &unorderedBlocks)
{
   mBlockFiles.clear();
   //Order the blockfiles into our queue in a fancy convenient way.  (this could be user-prefs)
   //for now just put them in linear.  We start the order from the first block that includes the ondemand sample
   //(which the user sets by clicking.)
   //Note that this code assumes that the array is sorted in time.

   //find the startpoint
   auto processStartSample = GetDemandSample();
   std::shared_ptr< ODPCMAliasBlockFile > firstBlock;
   for(auto i = unorderedBlocks.size(); i--;)
   {
      auto ptr = unorderedBlocks[i].lock();
      if(ptr)
      {
         //test if the blockfiles are near the task cursor.  we use the last mBlockFiles[0] as our point of reference
         //and add ones that are closer.
         if(firstBlock &&
            ptr->GetGlobalEnd() >= processStartSample &&
                ( firstBlock->GetGlobalEnd() < processStartSample ||
                  ptr->GetGlobalStart() <= firstBlock->GetGlobalStart())
            )
         {
            //insert at the front of the list if we get blockfiles that are after the demand sample
            firstBlock = ptr;
            mBlockFiles.insert(mBlockFiles.begin(), unorderedBlocks[i]);
         }
         else
         {
            //otherwise no priority
            if ( !firstBlock )
               firstBlock = ptr;
            mBlockFiles.push_back(unorderedBlocks[i]);
         }
         if(mMaxBlockFiles< (int) mBlockFiles.size())
            mMaxBlockFiles = mBlockFiles.size();
      }
      else
      {
         // The block file disappeared.
         // Let it be deleted and forget about it.
      }
   }
}
