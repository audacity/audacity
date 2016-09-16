/**********************************************************************

  Audacity: A Digital Audio Editor

  ODComputeSummaryTask.cpp

  Created by Michael Chinen (mchinen) on 8/10/08.
  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

******************************************************************//**

\class ODComputeSummaryTask
\brief Computes the summary data for a PCM (WAV) file and writes it to disk,
updating the ODPCMAliasBlockFile and the GUI of the newly available data.

*//*******************************************************************/

#include "../Audacity.h"
#include "ODDecodeTask.h"
#include "../blockfile/ODDecodeBlockFile.h"
#include "../Sequence.h"
#include "../WaveTrack.h"
#include <wx/wx.h>

///Creates a NEW task that computes summaries for a wavetrack that needs to be specified through SetWaveTrack()
ODDecodeTask::ODDecodeTask()
{
   mMaxBlockFiles = 0;
   mComputedBlockFiles = 0;
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
   sampleCount blockStartSample = 0;
   sampleCount blockEndSample = 0;
   bool success =false;

   for(size_t i=0; i < mWaveTracks.size() && mBlockFiles.size();i++)
   {
      const auto &bf = mBlockFiles[0];

      int ret = 1;

      //first check to see if the ref count is at least 2.  It should have one
      //from when we added it to this instance's mBlockFiles array, and one from
      //the Wavetrack/sequence.  If it doesn't it has been deleted and we should forget it.
      if(bf.use_count()>=2)
      {
         //OD TODO: somehow pass the bf a reference to the decoder that manages it's file.
         //we need to ensure that the filename won't change or be moved.  We do this by calling LockRead(),
         //which the dirmanager::EnsureSafeFilename also does.
         bf->LockRead();
         //Get the decoder.  If the file was moved, we need to create another one and init it.
         decoder = GetOrCreateMatchingFileDecoder( &*bf );
         if(!decoder->IsInitialized())
            decoder->Init();
         bf->SetODFileDecoder(decoder);
         ret = bf->DoWriteBlockFile();
         bf->UnlockRead();

         if(ret >= 0) {
            success = true;
            blockStartSample = bf->GetStart();
            blockEndSample = blockStartSample + bf->GetLength();
            mComputedBlockFiles++;
         }
      }
      else
      {
         //the waveform in the wavetrack now is shorter, so we need to update mMaxBlockFiles
         //because now there is less work to do.
         mMaxBlockFiles--;
      }

      //Release the refcount we placed on it if we are successful
      if(ret >= 0 ) {
         //take it out of the array - we are done with it.
         mBlockFiles.erase(mBlockFiles.begin());

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
   std::vector< std::shared_ptr< ODDecodeBlockFile > > tempBlocks;

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
            //TODO:this lock is way to big since the whole file is one sequence.  find a way to break it down.
            seq->LockDeleteUpdateMutex();

            //See Sequence::Delete() for why need this for now..
            blocks = clip->GetSequenceBlockArray();
            int i;
            int insertCursor;

            insertCursor =0;//OD TODO:see if this works, removed from inner loop (bfore was n*n)
            for (i = 0; i<(int)blocks->size(); i++)
            {
               //since we have more than one ODBlockFile, we will need type flags to cast.
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
                  while(insertCursor<(int)tempBlocks.size()&&
                     tempBlocks[insertCursor]->GetStart() + tempBlocks[insertCursor]->GetClipOffset() <
                        oddbFile->GetStart() + oddbFile->GetClipOffset())
                     insertCursor++;

                  tempBlocks.insert(tempBlocks.begin()+insertCursor++, oddbFile);
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
   (std::vector< std::shared_ptr< ODDecodeBlockFile > > &unorderedBlocks)
{
   mBlockFiles.clear();
   //TODO:order the blockfiles into our queue in a fancy convenient way.  (this could be user-prefs)
   //for now just put them in linear.  We start the order from the first block that includes the ondemand sample
   //(which the user sets by clicking.)   note that this code is pretty hacky - it assumes that the array is sorted in time.

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
         //since the order is linear right to left, this will add blocks so that the ones on the right side of the target
         //are processed first, with the ones closer being processed earlier.  Then the ones on the left side get processed.
         if(mBlockFiles.size() &&
            unorderedBlocks[i]->GetGlobalEnd() >= processStartSample &&
                ( mBlockFiles[0]->GetGlobalEnd() < processStartSample ||
                  unorderedBlocks[i]->GetGlobalStart() <= mBlockFiles[0]->GetGlobalStart()) )
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


///there could be the ODBlockFiles of several FLACs in one track (after copy and pasting)
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



///This should handle unicode converted to UTF-8 on mac/linux, but OD TODO:check on windows
ODFileDecoder::ODFileDecoder(const wxString & fName)
   : mFName{ fName }
{
   mInited = false;
}

ODFileDecoder::~ODFileDecoder()
{
}

bool ODFileDecoder::IsInitialized()
{
   bool ret;
   mInitedLock.Lock();
   ret = mInited;
   mInitedLock.Unlock();
   return ret;
}

///Derived classes should call this after they have parsed the header.
void ODFileDecoder::MarkInitialized()
{
   mInitedLock.Lock();
   mInited=true;
   mInitedLock.Unlock();
}

