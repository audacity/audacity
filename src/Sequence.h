/**********************************************************************

  Audacity: A Digital Audio Editor

  Sequence.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_SEQUENCE__
#define __AUDACITY_SEQUENCE__

#include "MemoryX.h"
#include <vector>

#include "SampleFormat.h"
#include "xml/XMLTagHandler.h"
#include "xml/XMLWriter.h"
#include "ondemand/ODTaskThread.h"

#include "audacity/Types.h"

class BlockFile;
using BlockFilePtr = std::shared_ptr<BlockFile>;

class DirManager;

// This is an internal data structure!  For advanced use only.
class SeqBlock {
 public:
   BlockFilePtr f;
   ///the sample in the global wavetrack that this block starts at.
   sampleCount start;

   SeqBlock()
      : f{}, start(0)
   {}

   SeqBlock(const BlockFilePtr &f_, sampleCount start_)
      : f(f_), start(start_)
   {}

   // Construct a SeqBlock with changed start, same file
   SeqBlock Plus(sampleCount delta) const
   {
      return SeqBlock(f, start + delta);
   }
};
class BlockArray : public std::vector<SeqBlock> {};
using BlockPtrArray = std::vector<SeqBlock*>; // non-owning pointers

class PROFILE_DLL_API Sequence final : public XMLTagHandler{
 public:

   //
   // Static methods
   //

   static void SetMaxDiskBlockSize(size_t bytes);
   static size_t GetMaxDiskBlockSize();

   //
   // Constructor / Destructor / Duplicator
   //

   Sequence(const std::shared_ptr<DirManager> &projDirManager, sampleFormat format);

   // The copy constructor and duplicate operators take a
   // DirManager as a parameter, because you might be copying
   // from one project to another...
   Sequence(const Sequence &orig, const std::shared_ptr<DirManager> &projDirManager);

   // Sequence cannot be copied without specifying a DirManager
   Sequence(const Sequence&) PROHIBITED;
   Sequence& operator= (const Sequence&) PROHIBITED;

   ~Sequence();

   //
   // Editing
   //

   sampleCount GetNumSamples() const { return mNumSamples; }

   bool Get(samplePtr buffer, sampleFormat format,
            sampleCount start, size_t len, bool mayThrow) const;

   // Note that len is not size_t, because nullptr may be passed for buffer, in
   // which case, silence is inserted, possibly a large amount.
   void SetSamples(samplePtr buffer, sampleFormat format,
            sampleCount start, sampleCount len);

   // where is input, assumed to be nondecreasing, and its size is len + 1.
   // min, max, rms, bl are outputs, and their lengths are len.
   // Each position in the output arrays corresponds to one column of pixels.
   // The column for pixel p covers samples from
   // where[p] up to (but excluding) where[p + 1].
   // bl is negative wherever data are not yet available.
   // Return true if successful.
   bool GetWaveDisplay(float *min, float *max, float *rms, int* bl,
                       size_t len, const sampleCount *where) const;

   // Return non-null, or else throw!
   std::unique_ptr<Sequence> Copy(sampleCount s0, sampleCount s1) const;
   void Paste(sampleCount s0, const Sequence *src);

   size_t GetIdealAppendLen() const;
   void Append(samplePtr buffer, sampleFormat format, size_t len,
               XMLWriter* blockFileLog=NULL);
   void Delete(sampleCount start, sampleCount len);
   void AppendAlias(const FilePath &fullPath,
                    sampleCount start,
                    size_t len, int channel, bool useOD);

   void AppendCoded(const FilePath &fName, sampleCount start,
                            size_t len, int channel, int decodeType);

   ///gets an int with OD flags so that we can determine which ODTasks should be run on this track after save/open, etc.
   unsigned int GetODFlags();

   // Append a blockfile. The blockfile pointer is then "owned" by the
   // sequence. This function is used by the recording log crash recovery
   // code, but may be useful for other purposes. The blockfile must already
   // be registered within the dir manager hash. This is the case
   // when the blockfile is created using DirManager::NewSimpleBlockFile or
   // loaded from an XML file via DirManager::HandleXMLTag
   void AppendBlockFile(const BlockFilePtr &blockFile);

   void SetSilence(sampleCount s0, sampleCount len);
   void InsertSilence(sampleCount s0, sampleCount len);

   const std::shared_ptr<DirManager> &GetDirManager() { return mDirManager; }

   //
   // XMLTagHandler callback methods for loading and saving
   //

   bool HandleXMLTag(const wxChar *tag, const wxChar **attrs) override;
   void HandleXMLEndTag(const wxChar *tag) override;
   XMLTagHandler *HandleXMLChild(const wxChar *tag) override;
   void WriteXML(XMLWriter &xmlFile) const /* not override */;

   bool GetErrorOpening() { return mErrorOpening; }

   //
   // Lock/Unlock all of this sequence's BlockFiles, keeping them
   // from being moved.  Call this if you want to copy a
   // track to a different DirManager.  See BlockFile.h
   // for details.
   //

   bool Lock();
   bool Unlock();

   bool CloseLock();//similar to Lock but should be called upon project close.
   // not balanced by unlocking calls.

   //
   // Manipulating Sample Format
   //

   sampleFormat GetSampleFormat() const;

   // Return true iff there is a change
   bool ConvertToSampleFormat(sampleFormat format);

   //
   // Retrieving summary info
   //

   std::pair<float, float> GetMinMax(
      sampleCount start, sampleCount len, bool mayThrow) const;
   float GetRMS(sampleCount start, sampleCount len, bool mayThrow) const;

   //
   // Getting block size and alignment information
   //

   // This returns a possibly large or negative value
   sampleCount GetBlockStart(sampleCount position) const;

   // These return a nonnegative number of samples meant to size a memory buffer
   size_t GetBestBlockSize(sampleCount start) const;
   size_t GetMaxBlockSize() const;
   size_t GetIdealBlockSize() const;

   //
   // This should only be used if you really, really know what
   // you're doing!
   //

   BlockArray &GetBlockArray() {return mBlock;}

   ///
   void LockDeleteUpdateMutex(){mDeleteUpdateMutex.Lock();}
   void UnlockDeleteUpdateMutex(){mDeleteUpdateMutex.Unlock();}

   // RAII idiom wrapping the functions above
   struct DeleteUpdateMutexLocker {
      DeleteUpdateMutexLocker(Sequence &sequence)
         : mSequence(sequence)
      {
         mSequence.LockDeleteUpdateMutex();
      }
      ~DeleteUpdateMutexLocker()
      {
         mSequence.UnlockDeleteUpdateMutex();
      }
   private:
      Sequence &mSequence;
   };

 private:

   //
   // Private static variables
   //

   static size_t    sMaxDiskBlockSize;

   //
   // Private variables
   //

   std::shared_ptr<DirManager> mDirManager;

   BlockArray    mBlock;
   sampleFormat  mSampleFormat;

   // Not size_t!  May need to be large:
   sampleCount   mNumSamples{ 0 };

   size_t   mMinSamples; // min samples per block
   size_t   mMaxSamples; // max samples per block

   bool          mErrorOpening{ false };

   ///To block the Delete() method against the ODCalcSummaryTask::Update() method
   ODLock   mDeleteUpdateMutex;

   //
   // Private methods
   //

   int FindBlock(sampleCount pos) const;

   static void AppendBlock
      (DirManager &dirManager,
       BlockArray &blocks, sampleCount &numSamples, const SeqBlock &b);

   static bool Read(samplePtr buffer, sampleFormat format,
             const SeqBlock &b,
             size_t blockRelativeStart, size_t len, bool mayThrow);

   // Accumulate NEW block files onto the end of a block array.
   // Does not change this sequence.  The intent is to use
   // CommitChangesIfConsistent later.
   static void Blockify
      (DirManager &dirManager, size_t maxSamples, sampleFormat format,
       BlockArray &list, sampleCount start, samplePtr buffer, size_t len);

   bool Get(int b, samplePtr buffer, sampleFormat format,
      sampleCount start, size_t len, bool mayThrow) const;

public:

   //
   // Public methods
   //

   // This function throws if the track is messed up
   // because of inconsistent block starts & lengths
   void ConsistencyCheck (const wxChar *whereStr, bool mayThrow = true) const;

   // This function prints information to stdout about the blocks in the
   // tracks and indicates if there are inconsistencies.
   static void DebugPrintf
      (const BlockArray &block, sampleCount numSamples, wxString *dest);

private:
   static void ConsistencyCheck
      (const BlockArray &block, size_t maxSamples, size_t from,
       sampleCount numSamples, const wxChar *whereStr,
       bool mayThrow = true);

   // The next two are used in methods that give a strong guarantee.
   // They either throw because final consistency check fails, or swap the
   // changed contents into place.

   void CommitChangesIfConsistent
      (BlockArray &newBlock, sampleCount numSamples, const wxChar *whereStr);

   void AppendBlocksIfConsistent
      (BlockArray &additionalBlocks, bool replaceLast,
       sampleCount numSamples, const wxChar *whereStr);

};

#endif // __AUDACITY_SEQUENCE__

