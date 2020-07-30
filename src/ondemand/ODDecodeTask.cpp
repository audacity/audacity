/**********************************************************************

  Audacity: A Digital Audio Editor

  ODDecodeTask

  Created by Michael Chinen (mchinen) on 8/10/08.
  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

******************************************************************//**

\class ODDecodeTask
\brief Decodes files and writes block files in .au format,
updating the ODDecodeBlockFile and the GUI of the newly available data.

*//*******************************************************************/

#include "../Audacity.h"
#include "ODDecodeTask.h"

#include "../blockfile/ODDecodeBlockFile.h"
#include "../Sequence.h"
#include "../WaveClip.h"
#include "../WaveTrack.h"
#include <wx/wx.h>

///Creates a NEW task that decodes files
ODDecodeTask::ODDecodeTask()
{
   mMaxBlockFiles = 0;
}


///Computes and writes the data for one BlockFile if it still has a refcount.
void ODDecodeTask::DoSomeInternal()
{
   if(mBlockFiles.size()<=0)
   {
      mPercentCompleteMutex.Lock();
      mPercentComplete = 1.0;
      mPercentCompleteMutex.Unlock();
      return;
   }

   ODFileDecoder* decoder;

   for(size_t j=0; j < mWaveTracks.size() && mBlockFiles.size();j++)
   {
      const auto bf = mBlockFiles[0].lock();
      sampleCount blockStartSample = 0;
      sampleCount blockEndSample = 0;
      bool success =false;

      int ret = 1;

      if(bf)
      {
         //OD TODO: somehow pass the bf a reference to the decoder that manages its file.
         //we need to ensure that the filename won't change or be moved.  We do this by calling LockRead(),
         //which the dirmanager::EnsureSafeFilename also does.
         {
            auto locker = bf->LockForRead();
            //Get the decoder.  If the file was moved, we need to create another one and init it.
            decoder = GetOrCreateMatchingFileDecoder( &*bf );
            if(!decoder->IsInitialized())
               decoder->Init();
            bf->SetODFileDecoder(decoder);
            // Does not throw:
            ret = bf->DoWriteBlockFile();
         }

         if(ret >= 0) {
            success = true;
            blockStartSample = bf->GetStart();
            blockEndSample = blockStartSample + bf->GetLength();
         }
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

      //Release the refcount we placed on it if we are successful
      if( bf && success ) {
         //upddate the gui for all associated blocks.  It doesn't matter that we're hitting more wavetracks then we should
         //because this loop runs a number of times equal to the number of tracks, they probably are getting processed in
         //the next iteration at the same sample window.
         mWaveTrackMutex.Lock();
         for(size_t i=0;i<mWaveTracks.size();i++)
         {
            auto waveTrack = mWaveTracks[i].lock();
            if(waveTrack)
            {
#if 0 // LLL: Commented out while removing OD files handling               
               waveTrack->AddInvalidRegion(blockStartSample,blockEndSample);
#endif
            }
         }
         mWaveTrackMutex.Unlock();
      }
   }

   //update percentage complete.
   CalculatePercentComplete();
}

void ODDecodeTask::CalculatePercentComplete()
{
   mPercentCompleteMutex.Lock();
   mPercentComplete = (float) 1.0 - ((float)mBlockFiles.size() / (mMaxBlockFiles+1));
   mPercentCompleteMutex.Unlock();
}

bool ODDecodeTask::SeekingAllowed()
{
   for (unsigned int i = 0; i < mDecoders.size(); i++) {
      if(!mDecoders[i]->SeekingAllowed())
         return false;
   }
   return true;
}

///by default creates the order of the wavetrack to load.
void ODDecodeTask::Update()
{
   std::vector< std::weak_ptr< ODDecodeBlockFile > > tempBlocks;

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
            //TODO:this lock is way to big since the whole file is one sequence.  find a way to break it down.
            seq->LockDeleteUpdateMutex();

            //See Sequence::Delete() for why need this for now..
            blocks = clip->GetSequenceBlockArray();
            int i;
            int insertCursor;

            insertCursor =0;//OD TODO:see if this works, removed from inner loop (bfore was n*n)
            for (i = 0; i<(int)blocks->size(); i++)
            {
               //since we have more than one ODDecodeBlockFile, we will need type flags to cast.
               SeqBlock &block = (*blocks)[i];
               const auto &file = block.f;
               std::shared_ptr<ODDecodeBlockFile> oddbFile;
               if (!file->IsDataAvailable() &&
                   (oddbFile =
                       std::static_pointer_cast<ODDecodeBlockFile>(file))->GetDecodeType() == this->GetODType())
               {
                  oddbFile->SetStart(block.start);
                  oddbFile->SetClipOffset(sampleCount(
                     clip->GetStartTime()*clip->GetRate()
                  ));

                  //these will always be linear within a sequence-lets take advantage of this by keeping a cursor.
                  {
                     std::shared_ptr< ODDecodeBlockFile > ptr;
                     while(insertCursor < (int)tempBlocks.size() &&
                           (!(ptr = tempBlocks[insertCursor].lock()) ||
                            ptr->GetStart() + ptr->GetClipOffset() <
                            oddbFile->GetStart() + oddbFile->GetClipOffset()))
                        insertCursor++;
                  }

                  tempBlocks.insert(tempBlocks.begin() + insertCursor++, oddbFile);
               }
            }

            seq->UnlockDeleteUpdateMutex();
         }
      }
   }
   mWaveTrackMutex.Unlock();

   //get the NEW order.
   OrderBlockFiles(tempBlocks);
}



///Orders the input as either On-Demand or default layered order.
void ODDecodeTask::OrderBlockFiles
   (std::vector< std::weak_ptr< ODDecodeBlockFile > > &unorderedBlocks)
{
   mBlockFiles.clear();
   //TODO:order the blockfiles into our queue in a fancy convenient way.  (this could be user-prefs)
   //for now just put them in linear.  We start the order from the first block that includes the ondemand sample
   //(which the user sets by clicking.)   note that this code is pretty hacky - it assumes that the array is sorted in time.

   //find the startpoint
   auto processStartSample = GetDemandSample();
   std::shared_ptr< ODDecodeBlockFile > firstBlock;
   for(auto i = unorderedBlocks.size(); i--; )
   {
      auto ptr = unorderedBlocks[i].lock();
      if(ptr)
      {
         //test if the blockfiles are near the task cursor.  we use the last mBlockFiles[0] as our point of reference
         //and add ones that are closer.
         //since the order is linear right to left, this will add blocks so that the ones on the right side of the target
         //are processed first, with the ones closer being processed earlier.  Then the ones on the left side get processed.
         if(mBlockFiles.size() &&
            ptr->GetGlobalEnd() >= processStartSample &&
                ( firstBlock->GetGlobalEnd() < processStartSample ||
                  ptr->GetGlobalStart() <= firstBlock->GetGlobalStart()) )
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
      }
   }

}



///changes the tasks associated with this Waveform to process the task from a different point in the track
///this is overridden from ODTask because certain classes don't allow users to seek sometimes, or not at all.
void ODDecodeTask::DemandTrackUpdate(WaveTrack* track, double seconds)
{
   //only update if the subclass says we can seek.
   if(SeekingAllowed())
      ODTask::DemandTrackUpdate(track,seconds);
}


///there could be the ODDecodeBlockFiles of several FLACs in one track (after copy and pasting)
///so we keep a list of decoders that keep track of the file names, etc, and check the blocks against them.
///Blocks that have IsDataAvailable()==false are blockfiles to be decoded.  if BlockFile::GetDecodeType()==ODDecodeTask::GetODType() then
///this decoder should handle it.  Decoders are accessible with the methods below.  These aren't thread-safe and should only
///be called from the decoding thread.
ODFileDecoder* ODDecodeTask::GetOrCreateMatchingFileDecoder(ODDecodeBlockFile* blockFile)
{
   ODFileDecoder* ret=NULL;
   //see if the filename matches any of our decoders, if so, return it.
   for(int i=0;i<(int)mDecoders.size();i++)
   {
      //we check filename and decode type, since two types of ODDecoders might work with the same filetype
      //e.g., FFmpeg and LibMad import both do MP3s.  TODO: is this necessary? in theory we filter this when
      //updating our list of blockfiles.
      if(mDecoders[i]->GetFileName()==blockFile->GetAudioFileName().GetFullPath() &&
         GetODType() == blockFile->GetDecodeType() )
      {
         ret = mDecoders[i].get();
         break;
      }
   }

   //otherwise, create and add one, and return it.
   if(!ret)
   {
      ret=CreateFileDecoder(blockFile->GetAudioFileName().GetFullPath());
   }
   return ret;
}
int ODDecodeTask::GetNumFileDecoders()
{
   return mDecoders.size();
}
