/**********************************************************************

  Audacity: A Digital Audio Editor

  ODDecodeTask.h

  Created by Michael Chinen (mchinen) on 8/10/08.
  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

******************************************************************//**

\class ODDecodeTask
\brief Decodes a file into a simpleBlockFile, but not immediately.

This is an abstract class that subclasses will have to derive the types
from.  For any type there should only be one ODDecodeTask associated with
a given track.
There could be the ODBlockFiles of several FLACs in one track (after copy and pasting),
so things aren't as simple as they seem - the implementation needs to be
robust enough to allow all the user changes such as copy/paste, DELETE, and so on.

*//*******************************************************************/




#ifndef __AUDACITY_ODDecodeTask__
#define __AUDACITY_ODDecodeTask__

#include "../MemoryX.h"
#include <vector>
#include "ODTask.h"
#include "ODTaskThread.h"
class ODDecodeBlockFile;
class WaveTrack;
class ODFileDecoder;


/// A class representing a modular task to be used with the On-Demand structures.
class ODDecodeTask /* not final */ : public ODTask
{
 public:
   ODDecodeTask();
   virtual ~ODDecodeTask(){};

   // NEW virtual:
   virtual bool SeekingAllowed();

   ///changes the tasks associated with this Waveform to process the task from a different point in the track
   ///this is overridden from ODTask because certain classes don't allow users to seek sometimes, or not at all.
   void DemandTrackUpdate(WaveTrack* track, double seconds) override;

   ///Return the task name
   const char* GetTaskName() override { return "ODDecodeTask"; }

   const wxChar* GetTip() override { return _("Decoding Waveform"); }

   ///Subclasses should override to return respective type.
   unsigned int GetODType() override { return eODNone; }

   ///Creates an ODFileDecoder that decodes a file of filetype the subclass handles.
   virtual ODFileDecoder* CreateFileDecoder(const wxString & fileName)=0;

   ///there could be the ODBlockFiles of several FLACs in one track (after copy and pasting)
   ///so we keep a list of decoders that keep track of the file names, etc, and check the blocks against them.
   ///Blocks that have IsDataAvailable()==false are blockfiles to be decoded.  if BlockFile::GetDecodeType()==ODDecodeTask::GetODType() then
   ///this decoder should handle it.  Decoders are accessible with the methods below.  These aren't thread-safe and should only
   ///be called from the decoding thread.
   // NEW virtuals:
   virtual ODFileDecoder* GetOrCreateMatchingFileDecoder(ODDecodeBlockFile* blockFile);
   virtual int GetNumFileDecoders();


protected:

   ///recalculates the percentage complete.
   void CalculatePercentComplete() override;

   ///Computes and writes the data for one BlockFile if it still has a refcount.
   void DoSomeInternal() override;

   ///Readjusts the blockfile order in the default manner.  If we have had an ODRequest
   ///Then it updates in the OD manner.
   void Update() override;

   ///Orders the input as either On-Demand or default layered order.
   void OrderBlockFiles
      (std::vector< std::shared_ptr< ODDecodeBlockFile > > &unorderedBlocks);


   std::vector<std::shared_ptr<ODDecodeBlockFile>> mBlockFiles;
   std::vector<movable_ptr<ODFileDecoder>> mDecoders;

   int mMaxBlockFiles;
   int mComputedBlockFiles;

};

///class to decode a particular file (one per file).  Saves info such as filename and length (after the header is read.)
class ODFileDecoder /* not final */
{
public:
   ///This should handle unicode converted to UTF-8 on mac/linux, but OD TODO:check on windows
   ODFileDecoder(const wxString& fName);
   virtual ~ODFileDecoder();

   ///Read header.  Subclasses must override.  Probably should save the info somewhere.
   ///Ideally called once per decoding of a file.  This complicates the task because
   virtual bool ReadHeader()=0;
   virtual bool Init(){return ReadHeader();}

   virtual bool SeekingAllowed(){return true;}

   ///Decodes the samples for this blockfile from the real file into a float buffer.
   ///This is file specific, so subclasses must implement this only.
   ///the buffer should be created by the ODFileDecoder implementing this method.
   ///It should set the format parameter so that the client code can deal with it.
   ///This class should call ReadHeader() first, so it knows the length, and can prepare
   ///the file object if it needs to.
   ///returns negative value for failure, 0 or positive value for success.
   virtual int Decode(SampleBuffer & data, sampleFormat & format, sampleCount start, size_t len, unsigned int channel) = 0;

   const wxString &GetFileName(){return mFName;}

   bool IsInitialized();

protected:
   ///Derived classes should call this after they have parsed the header.
   void MarkInitialized();

   bool     mInited;
   ODLock   mInitedLock;

   const wxString  mFName;

   unsigned int mSampleRate;
   unsigned int mNumSamples;//this may depend on the channel - so TODO: we should probably let the decoder create/modify the track info directly.
   unsigned int mNumChannels;
};

#endif



