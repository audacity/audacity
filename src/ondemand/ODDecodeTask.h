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
There could be the ODDecodeBlockFiles of several FLACs in one track (after copy and pasting),
so things aren't as simple as they seem - the implementation needs to be
robust enough to allow all the user changes such as copy/paste, DELETE, and so on.

*//*******************************************************************/




#ifndef __AUDACITY_ODDecodeTask__
#define __AUDACITY_ODDecodeTask__

#include <vector>
#include "ODTask.h"
#include "../Internat.h"
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

   TranslatableString GetTip() override { return XO("Decoding Waveform"); }

   ///Subclasses should override to return respective type.
   unsigned int GetODType() override { return eODNone; }

   ///Creates an ODFileDecoder that decodes a file of filetype the subclass handles.
   virtual ODFileDecoder* CreateFileDecoder(const wxString & fileName)=0;

   ///there could be the ODDecodeBlockFiles of several FLACs in one track (after copy and pasting)
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
      (std::vector< std::weak_ptr< ODDecodeBlockFile > > &unorderedBlocks);


   std::vector<std::weak_ptr<ODDecodeBlockFile>> mBlockFiles;
   std::vector<std::unique_ptr<ODFileDecoder>> mDecoders;

   int mMaxBlockFiles;

};

#endif



