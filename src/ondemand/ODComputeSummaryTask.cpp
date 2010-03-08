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
#include <wx/wx.h>

//36 blockfiles > 3 minutes stereo 44.1kHz per ODTask::DoSome
#define kNumBlockFilesPerDoSome 36

///Creates a new task that computes summaries for a wavetrack that needs to be specified through SetWaveTrack()
ODComputeSummaryTask::ODComputeSummaryTask()
{
   mMaxBlockFiles = 0;
   mComputedBlockFiles = 0;
   mHasUpdateRan=false;

}
 
ODTask* ODComputeSummaryTask::Clone()
{
   ODComputeSummaryTask* clone = new ODComputeSummaryTask;
   clone->mDemandSample=GetDemandSample();
   return clone;
   
}


///releases memory that the ODTask owns.  Subclasses should override.
void ODComputeSummaryTask::Terminate()
{
   //The terminate cblock won't allow DoSomeInternal and this method to be run async, so this is thread-safe
   
   //we are going to take things out of the array.  But first deref them since we ref them when we put them in.
   
   mBlockFilesMutex.Lock();
   for(unsigned int i=0;i<mBlockFiles.size();i++)
      mBlockFiles[i]->Deref();
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
   
   ODPCMAliasBlockFile* bf;
   sampleCount blockStartSample;
   sampleCount blockEndSample;
   bool success =false;
   
   
   mBlockFilesMutex.Lock();
   for(size_t i=0; i < mWaveTracks.size() && mBlockFiles.size();i++)
   {
      bf = mBlockFiles[0];
      
      //first check to see if the ref count is at least 2.  It should have one 
      //from when we added it to this instance's mBlockFiles array, and one from
      //the Wavetrack/sequence.  If it doesn't it has been deleted and we should forget it.
      if(bf->RefCount()>=2)
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
      
      //Release the refcount we placed on it.
      bf->Deref();
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

///by default creates the order of the wavetrack to load.
void ODComputeSummaryTask::Update()
{

   std::vector<ODPCMAliasBlockFile*> tempBlocks;
   
   mWaveTrackMutex.Lock();
   
   for(size_t j=0;j<mWaveTracks.size();j++)
   {
      if(mWaveTracks[j])
      {
         WaveClip *clip;
         BlockArray *blocks;
         Sequence *seq;
         
         //gather all the blockfiles that we should process in the wavetrack.
         WaveClipList::compatibility_iterator node = mWaveTracks[j]->GetClipIterator();
                  
         while(node) {
            clip = node->GetData();
            seq = clip->GetSequence();
            //This lock may be way too big since the whole file is one sequence.  
            //TODO: test for large files and find a way to break it down.
            seq->LockDeleteUpdateMutex();
            
            //See Sequence::Delete() for why need this for now..
            //We don't need the mBlockFilesMutex here because it is only for the vector list.  
            //These are existing blocks, and its wavetrack or blockfiles won't be deleted because 
            //of the respective mWaveTrackMutex lock and LockDeleteUpdateMutex() call. 
            blocks = clip->GetSequenceBlockArray();
            int i;
            int insertCursor;
            
            insertCursor =0;//OD TODO:see if this works, removed from inner loop (bfore was n*n)
            
            for(i=0; i<(int)blocks->GetCount(); i++)
            {
               //if there is data but no summary, this blockfile needs summarizing.
               if(blocks->Item(i)->f->IsDataAvailable() && !blocks->Item(i)->f->IsSummaryAvailable())
               {
                  blocks->Item(i)->f->Ref();
                  ((ODPCMAliasBlockFile*)blocks->Item(i)->f)->SetStart(blocks->Item(i)->start);
                  ((ODPCMAliasBlockFile*)blocks->Item(i)->f)->SetClipOffset((sampleCount)(clip->GetStartTime()*clip->GetRate()));
                  
                  //these will always be linear within a sequence-lets take advantage of this by keeping a cursor.
                  while(insertCursor<(int)tempBlocks.size()&& 
                     (sampleCount)(tempBlocks[insertCursor]->GetStart()+tempBlocks[insertCursor]->GetClipOffset()) < 
                        (sampleCount)(((ODPCMAliasBlockFile*)blocks->Item(i)->f)->GetStart()+((ODPCMAliasBlockFile*)blocks->Item(i)->f)->GetClipOffset()))
                     insertCursor++;
                  
                  tempBlocks.insert(tempBlocks.begin()+insertCursor++,(ODPCMAliasBlockFile*)blocks->Item(i)->f);
               }
            }   
            seq->UnlockDeleteUpdateMutex();
            node = node->GetNext();
         }
      }
   }
   mWaveTrackMutex.Unlock();
   
   //get the new order.
   mBlockFilesMutex.Lock();
   OrderBlockFiles(tempBlocks);
   mBlockFilesMutex.Unlock();
   
   MarkUpdateRan();
}



///Orders the input as either On-Demand or default layered order.
void ODComputeSummaryTask::OrderBlockFiles(std::vector<ODPCMAliasBlockFile*> &unorderedBlocks)
{
   //we are going to take things out of the array.  But first deref them since we ref them when we put them in.
   for(unsigned int i=0;i<mBlockFiles.size();i++)
      mBlockFiles[i]->Deref();
   mBlockFiles.clear();
   //TODO:order the blockfiles into our queue in a fancy convenient way.  (this could be user-prefs)
   //for now just put them in linear.  We start the order from the first block that includes the ondemand sample
   //(which the user sets by clicking.)   
   //Note that this code assumes that the array is sorted in time.
   
   //find the startpoint
   sampleCount processStartSample = GetDemandSample(); 
   for(int i= ((int)unorderedBlocks.size())-1;i>= 0;i--)
   {
      //check to see if the refcount is at least two before we add it to the list.
      //There should be one Ref() from the one added by this ODTask, and one from the track.  
      //If there isn't, then the block was deleted for some reason and we should ignore it.
      if(unorderedBlocks[i]->RefCount()>=2)
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
         unorderedBlocks[i]->Deref();
      }
   }
   
}  