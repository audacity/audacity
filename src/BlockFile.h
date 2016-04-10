/**********************************************************************

  Audacity: A Digital Audio Editor

  BlockFile.h

  Joshua Haberman
  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_BLOCKFILE__
#define __AUDACITY_BLOCKFILE__

#include "MemoryX.h"
#include <wx/string.h>
#include <wx/ffile.h>
#include <wx/filename.h>

#include "xml/XMLTagHandler.h"
#include "xml/XMLWriter.h"

#include "SampleFormat.h"


class SummaryInfo {
 public:
   SummaryInfo(sampleCount samples);

   int            fields; /* Usually 3 for Min, Max, RMS */
   sampleFormat   format;
   int            bytesPerFrame;
   sampleCount    frames64K;
   int            offset64K;
   sampleCount    frames256;
   int            offset256;
   int            totalSummaryBytes;
};



class PROFILE_DLL_API BlockFile /* not final, abstract */ {
 public:

   // Constructor / Destructor

   /// Construct a BlockFile.
   BlockFile(wxFileName fileName, sampleCount samples);
   virtual ~BlockFile();

   // Reading

   /// Retrieves audio data from this BlockFile
   virtual int ReadData(samplePtr data, sampleFormat format,
                        sampleCount start, sampleCount len) const = 0;

   // Other Properties

   // Write cache to disk, if it has any
   virtual bool GetNeedWriteCacheToDisk() { return false; }
   virtual void WriteCacheToDisk() { /* no cache by default */ }

   // Fill read cache of block file, if it has any
   virtual bool GetNeedFillCache() { return false; }
   virtual void FillCache() { /* no cache by default */ }

   /// Stores a representation of this file in XML
   virtual void SaveXML(XMLWriter &xmlFile) = 0;

   /// Gets the filename of the disk file associated with this BlockFile
   /// (can be empty -- some BlockFiles, like SilentBlockFile, correspond to
   ///  no file on disk)
   virtual wxFileName GetFileName() const;
   virtual void SetFileName(wxFileName &name);

   virtual sampleCount GetLength() const { return mLen; }
   virtual void SetLength(const sampleCount newLen) { mLen = newLen; }

   /// Locks this BlockFile, to prevent it from being moved
   virtual void Lock();
   /// Unlock this BlockFile, allowing it to be moved
   virtual void Unlock();
   /// Returns TRUE if this BlockFile is locked
   virtual bool IsLocked();

   /// Gets extreme values for the specified region
   virtual void GetMinMax(sampleCount start, sampleCount len,
                          float *outMin, float *outMax, float *outRMS) const;
   /// Gets extreme values for the entire block
   virtual void GetMinMax(float *outMin, float *outMax, float *outRMS) const;
   /// Returns the 256 byte summary data block
   virtual bool Read256(float *buffer, sampleCount start, sampleCount len);
   /// Returns the 64K summary data block
   virtual bool Read64K(float *buffer, sampleCount start, sampleCount len);

   /// Returns TRUE if this block references another disk file
   virtual bool IsAlias() const { return false; }

   /// Returns TRUE if this block's complete summary has been computed and is ready (for OD)
   virtual bool IsSummaryAvailable() const {return true;}

   /// Returns TRUE if this block's complete data is ready to be accessed by Read()
   virtual bool IsDataAvailable() const {return true;}

   /// Returns TRUE if the summary has not yet been written, but is actively being computed and written to disk
   virtual bool IsSummaryBeingComputed(){return false;}

   /// Create a NEW BlockFile identical to this, using the given filename
   virtual BlockFile *Copy(wxFileName newFileName) = 0;

   virtual wxLongLong GetSpaceUsage() const = 0;

   /// if the on-disk state disappeared, either recover it (if it was
   //summary only), write out a placeholder of silence data (missing
   //.au) or mark the blockfile to deal some other way without spewing
   //errors.
   virtual void Recover() = 0;
   /// if we've detected an on-disk problem, the user opted to
   //continue and the error persists, don't keep reporting it.  The
   //Object implements this functionality internally, but we want to
   //be able to tell the logging to shut up from outside too.
   void SilenceLog() const { mSilentLog = TRUE; }

   ///when the project closes, it locks the blockfiles.
   ///Override this in case it needs special treatment
   virtual void CloseLock(){Lock();}

   /// Prevents a read on other threads.  The basic blockfile runs on only one thread, so does nothing.
   virtual void LockRead() const {}
   /// Allows reading on other threads.
   virtual void UnlockRead() const {}

 private:

   friend class DirManager;
   friend class AudacityApp;
   //needed for Ref/Deref access.
   friend class ODComputeSummaryTask;
   friend class ODDecodeTask;
   friend class ODPCMAliasBlockFile;

   virtual void Ref() const;
   virtual bool Deref() const;
   virtual int RefCount(){return mRefCount;}

 protected:
   /// Calculate summary data for the given sample data
   virtual void *CalcSummary(samplePtr buffer, sampleCount len,
                             sampleFormat format,
                             // This gets filled, if the caller needs to deallocate.  Else it is null.
                             ArrayOf<char> &cleanup);
   /// Read the summary section of the file.  Derived classes implement.
   virtual bool ReadSummary(void *data) = 0;

   /// Byte-swap the summary data, in case it was saved by a system
   /// on a different platform
   virtual void FixSummary(void *data);

 private:
   int mLockCount;
   mutable int mRefCount;

   static ArrayOf<char> fullSummary;

 protected:
   wxFileName mFileName;
   sampleCount mLen;
   SummaryInfo mSummaryInfo;
   float mMin, mMax, mRMS;
   mutable bool mSilentLog;
};

/// A BlockFile that refers to data in an existing file

/// An AliasBlockFile references an existing disk file for its storage
/// instead of copying the data.  It still writes a file to disk, but
/// only stores summary data in it.
///
/// This is a common base class for all alias block files.  It handles
/// reading and writing summary data, leaving very little for derived
/// classes to need to implement.
class AliasBlockFile /* not final */ : public BlockFile
{
 public:

   // Constructor / Destructor

   /// Constructs an AliasBlockFile
   AliasBlockFile(wxFileName baseFileName,
                  wxFileName aliasedFileName, sampleCount aliasStart,
                  sampleCount aliasLen, int aliasChannel);
   AliasBlockFile(wxFileName existingSummaryFileName,
                  wxFileName aliasedFileName, sampleCount aliasStart,
                  sampleCount aliasLen, int aliasChannel,
                  float min, float max, float RMS);
   virtual ~AliasBlockFile();

   // Reading

   wxLongLong GetSpaceUsage() const override;

   /// as SilentLog (which would affect Summary data access), but
   // applying to Alias file access
   void SilenceAliasLog() const { mSilentAliasLog = TRUE; }

   //
   // These methods are for advanced use only!
   //
   wxFileName GetAliasedFileName() { return mAliasedFileName; }
   void ChangeAliasedFileName(wxFileName newAliasedFile);
   bool IsAlias() const override { return true; }

 protected:
   // Introduce a NEW virtual.
   /// Write the summary to disk, using the derived ReadData() to get the data
   virtual void WriteSummary();
   /// Read the summary into a buffer
   bool ReadSummary(void *data) override;

   wxFileName  mAliasedFileName;
   sampleCount mAliasStart;
   int         mAliasChannel;
   mutable bool        mSilentAliasLog;
};

#endif

