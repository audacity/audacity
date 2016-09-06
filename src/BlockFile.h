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

#include "wxFileNameWrapper.h"

#include "ondemand/ODTaskThread.h"


class SummaryInfo {
 public:
   SummaryInfo(size_t samples);

   int            fields; /* Usually 3 for Min, Max, RMS */
   sampleFormat   format;
   int            bytesPerFrame;
   size_t         frames64K;
   int            offset64K;
   size_t         frames256;
   int            offset256;
   int            totalSummaryBytes;
};



class BlockFile;
using BlockFilePtr = std::shared_ptr<BlockFile>;

template< typename Result, typename... Args >
inline std::shared_ptr< Result > make_blockfile (Args && ... args)
{
   return std::make_shared< Result > ( std::forward< Args > ( args )... );
}

class PROFILE_DLL_API BlockFile /* not final, abstract */ {
 public:

   // Constructor / Destructor

   /// Construct a BlockFile.
   BlockFile(wxFileNameWrapper &&fileName, size_t samples);
   virtual ~BlockFile();

   static unsigned long gBlockFileDestructionCount;

   // Reading

   /// Retrieves audio data from this BlockFile
   virtual size_t ReadData(samplePtr data, sampleFormat format,
                        size_t start, size_t len) const = 0;

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
   /// Avoids copying wxFileName by returning a reference, but for some subclasses
   /// of BlockFile, you must exclude other threads from changing the name so long
   /// as you have only a reference.  Thus, this wrapper object that guarantees release
   /// of any lock when it goes out of scope.  Call mLocker.reset() to unlock it sooner.
   struct GetFileNameResult {
      const wxFileName &name;
      ODLocker mLocker;

      GetFileNameResult(const wxFileName &name_, ODLocker &&locker = ODLocker{})
      : name{ name_ }, mLocker{ std::move(locker) } {}

      GetFileNameResult(const GetFileNameResult&) PROHIBITED;
      GetFileNameResult &operator= (const GetFileNameResult&) PROHIBITED;

      GetFileNameResult(GetFileNameResult &&that)
      : name{ that.name }, mLocker{ std::move(that.mLocker) } {}
   };
   virtual GetFileNameResult GetFileName() const;
   virtual void SetFileName(wxFileNameWrapper &&name);

   size_t GetLength() const { return mLen; }
   void SetLength(size_t newLen) { mLen = newLen; }

   /// Locks this BlockFile, to prevent it from being moved
   virtual void Lock();
   /// Unlock this BlockFile, allowing it to be moved
   virtual void Unlock();
   /// Returns TRUE if this BlockFile is locked
   virtual bool IsLocked();

   /// Gets extreme values for the specified region
   virtual void GetMinMax(size_t start, size_t len,
                          float *outMin, float *outMax, float *outRMS) const;
   /// Gets extreme values for the entire block
   virtual void GetMinMax(float *outMin, float *outMax, float *outRMS) const;
   /// Returns the 256 byte summary data block
   virtual bool Read256(float *buffer, size_t start, size_t len);
   /// Returns the 64K summary data block
   virtual bool Read64K(float *buffer, size_t start, size_t len);

   /// Returns TRUE if this block references another disk file
   virtual bool IsAlias() const { return false; }

   /// Returns TRUE if this block's complete summary has been computed and is ready (for OD)
   virtual bool IsSummaryAvailable() const {return true;}

   /// Returns TRUE if this block's complete data is ready to be accessed by Read()
   virtual bool IsDataAvailable() const {return true;}

   /// Returns TRUE if the summary has not yet been written, but is actively being computed and written to disk
   virtual bool IsSummaryBeingComputed(){return false;}

   /// Create a NEW BlockFile identical to this, using the given filename
   virtual BlockFilePtr Copy(wxFileNameWrapper &&newFileName) = 0;

   // Report disk space usage.
   using DiskByteCount = unsigned long long;
   virtual DiskByteCount GetSpaceUsage() const = 0;

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
   ///Override this in case it needs special treatment.
   // not balanced by unlocking calls.
   virtual void CloseLock(){Lock();}

   /// Prevents a read on other threads.  The basic blockfile runs on only one thread, so does nothing.
   virtual void LockRead() const {}
   /// Allows reading on other threads.
   virtual void UnlockRead() const {}

 private:

 protected:
   /// Calculate summary data for the given sample data
   /// Overrides have differing details of memory management
   virtual void *CalcSummary(samplePtr buffer, size_t len,
                             sampleFormat format,
                             // This gets filled, if the caller needs to deallocate.  Else it is null.
                             ArrayOf<char> &cleanup);
   // Common, nonvirtual calculation routine for the use of the above
   void CalcSummaryFromBuffer(const float *fbuffer, size_t len,
                              float *summary256, float *summary64K);

   /// Read the summary section of the file.  Derived classes implement.
   virtual bool ReadSummary(void *data) = 0;

   /// Byte-swap the summary data, in case it was saved by a system
   /// on a different platform
   virtual void FixSummary(void *data);

 private:
   int mLockCount;

   static ArrayOf<char> fullSummary;

 protected:
   wxFileNameWrapper mFileName;
   size_t mLen;
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
   AliasBlockFile(wxFileNameWrapper &&baseFileName,
                  wxFileNameWrapper &&aliasedFileName, sampleCount aliasStart,
                  size_t aliasLen, int aliasChannel);
   AliasBlockFile(wxFileNameWrapper &&existingSummaryFileName,
                  wxFileNameWrapper &&aliasedFileName, sampleCount aliasStart,
                  size_t aliasLen, int aliasChannel,
                  float min, float max, float RMS);
   virtual ~AliasBlockFile();

   // Reading

   DiskByteCount GetSpaceUsage() const override;

   /// as SilentLog (which would affect Summary data access), but
   // applying to Alias file access
   void SilenceAliasLog() const { mSilentAliasLog = TRUE; }

   //
   // These methods are for advanced use only!
   //
   const wxFileName &GetAliasedFileName() const { return mAliasedFileName; }
   void ChangeAliasedFileName(wxFileNameWrapper &&newAliasedFile);
   bool IsAlias() const override { return true; }

 protected:
   // Introduce a NEW virtual.
   /// Write the summary to disk, using the derived ReadData() to get the data
   virtual void WriteSummary();
   /// Read the summary into a buffer
   bool ReadSummary(void *data) override;

   wxFileNameWrapper mAliasedFileName;
   sampleCount mAliasStart;
   int         mAliasChannel;
   mutable bool        mSilentAliasLog;
};

#endif

