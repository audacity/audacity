/**********************************************************************

  Audacity: A Digital Audio Editor

  ODPCMAliasBlockFile.cpp

  Created by Michael Chinen (mchinen)
  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

******************************************************************//**

\class ODPCMAliasBlockFile
\brief ODPCMAliasBlockFile is a special type of PCMAliasBlockFile that does not necessarily have summary data available
The summary is eventually computed and written to a file in a background thread.

Load On-Demand implementation of the AliasBlockFile for PCM files.

to load large files more quickly, we take skip computing the summary data and put
ODPCMAliasBlockFiles in the sequence as place holders.  A background thread loads and
computes the summary data into these classes.
ODPCMAliasBlockFiles are unlike all other BlockFiles are not immutable (for the most part,) because when new
summary data is computed for an existing ODPCMAliasBlockFile we save the buffer then and write the Summary File.

All BlockFile methods that treat the summary data as a buffer that exists in its BlockFile
are implemented here to behave when the data is not available yet.

Some of these methods have been overridden only because they used the unsafe wxLog calls in the base class.
*//*******************************************************************/






#ifndef __AUDACITY_ODPCMALIASBLOCKFILE__
#define __AUDACITY_ODPCMALIASBLOCKFILE__

#include "PCMAliasBlockFile.h"
#include "../BlockFile.h"
#include "../ondemand/ODTaskThread.h"
#include "../DirManager.h"
#include <wx/thread.h>

/// An AliasBlockFile that references uncompressed data in an existing file
class ODPCMAliasBlockFile : public PCMAliasBlockFile
{
 public:
   /// Constructs a PCMAliasBlockFile, writing the summary to disk
   ODPCMAliasBlockFile(wxFileName baseFileName,
                        wxFileName aliasedFileName, sampleCount aliasStart,
                        sampleCount aliasLen, int aliasChannel);
   ODPCMAliasBlockFile(wxFileName existingSummaryFileName,
                        wxFileName aliasedFileName, sampleCount aliasStart,
                        sampleCount aliasLen, int aliasChannel,
                        float min, float max, float rms, bool summaryAvailable);
   virtual ~ODPCMAliasBlockFile();

   //checks to see if summary data has been computed and written to disk yet.  Thread safe.  Blocks if we are writing summary data.
   virtual bool IsSummaryAvailable();

   /// Returns TRUE if the summary has not yet been written, but is actively being computed and written to disk
   virtual bool IsSummaryBeingComputed(){return mSummaryBeingComputed;}

   //Calls that rely on summary files need to be overidden
   virtual wxLongLong GetSpaceUsage();
   /// Gets extreme values for the specified region
   virtual void GetMinMax(sampleCount start, sampleCount len,
                          float *outMin, float *outMax, float *outRMS);
   /// Gets extreme values for the entire block
   virtual void GetMinMax(float *outMin, float *outMax, float *outRMS);
   /// Returns the 256 byte summary data block
   virtual bool Read256(float *buffer, sampleCount start, sampleCount len);
   /// Returns the 64K summary data block
   virtual bool Read64K(float *buffer, sampleCount start, sampleCount len);

   ///Makes new ODPCMAliasBlockFile or PCMAliasBlockFile depending on summary availability
   virtual BlockFile *Copy(wxFileName fileName);

   ///Saves as xml ODPCMAliasBlockFile or PCMAliasBlockFile depending on summary availability
   virtual void SaveXML(XMLWriter &xmlFile);

   ///Reconstructs from XML a ODPCMAliasBlockFile and reschedules it for OD loading
   static BlockFile *BuildFromXML(DirManager &dm, const wxChar **attrs);

   ///Writes the summary file if summary data is available
   virtual void Recover(void);

   ///A public interface to WriteSummary
   void DoWriteSummary();

   ///Sets the value that indicates where the first sample in this block corresponds to the global sequence/clip.  Only for display use.
   void SetStart(sampleCount startSample){mStart = startSample;}

   ///Gets the value that indicates where the first sample in this block corresponds to the global sequence/clip.  Only for display use.
   sampleCount GetStart(){return mStart;}

   /// Locks the blockfile only if it has a file that exists.
   void Lock();

   /// Unlocks the blockfile only if it has a file that exists.
   void Unlock();

   ///sets the amount of samples the clip associated with this blockfile is offset in the wavetrack (non effecting)
   void SetClipOffset(sampleCount numSamples){mClipOffset= numSamples;}

   ///Gets the number of samples the clip associated with this blockfile is offset by.
   sampleCount GetClipOffset(){return mClipOffset;}

   //returns the number of samples from the beginning of the track that this blockfile starts at
   sampleCount GetGlobalStart(){return mClipOffset+mStart;}

   //returns the number of samples from the beginning of the track that this blockfile ends at
   sampleCount GetGlobalEnd(){return mClipOffset+mStart+GetLength();}


   //Below calls are overrided just so we can take wxlog calls out, which are not threadsafe.

   /// Reads the specified data from the aliased file using libsndfile
   virtual int ReadData(samplePtr data, sampleFormat format,
                        sampleCount start, sampleCount len);

   /// Read the summary into a buffer
   virtual bool ReadSummary(void *data);

   ///sets the file name the summary info will be saved in.  threadsafe.
   virtual void SetFileName(wxFileName &name);
   virtual wxFileName GetFileName();

   //when the file closes, it locks the blockfiles, but it calls this so we can check if it has been saved before.
   virtual void CloseLock();

   /// Prevents a read on other threads.
   virtual void LockRead();
   /// Allows reading on other threads.
   virtual void UnlockRead();

  protected:
   virtual void WriteSummary();
   virtual void *CalcSummary(samplePtr buffer, sampleCount len,
                             sampleFormat format);

  private:
   //Thread-safe versions
   virtual void Ref();
   virtual bool Deref();
   //needed for Ref/Deref access.
   friend class DirManager;
   friend class ODComputeSummaryTask;
   friend class ODDecodeTask;

   ODLock mWriteSummaryMutex;

   //need to protect this since it is changed from the main thread upon save.
   ODLock mFileNameMutex;

   ///Also need to protect the aliased file name.
   ODLock mAliasedFileNameMutex;

   //lock the read data - libsndfile can't handle two reads at once?
   ODLock mReadDataMutex;


   //lock the Ref counting
   ODLock mDerefMutex;
   ODLock mRefMutex;

   ODLock    mSummaryAvailableMutex;
   bool mSummaryAvailable;
   bool mSummaryBeingComputed;
   bool mHasBeenSaved;

   ///for reporting after task is complete.  Only for display use.
   sampleCount mStart;

   ///the ODTask needs to know where this blockfile lies in the track, so for convenience, we have this here.
   sampleCount mClipOffset;
};

#endif

