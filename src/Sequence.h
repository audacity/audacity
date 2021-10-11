/**********************************************************************

  Audacity: A Digital Audio Editor

  Sequence.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_SEQUENCE__
#define __AUDACITY_SEQUENCE__


#include <vector>
#include <functional>

#include "SampleFormat.h"
#include "XMLTagHandler.h"

#include "SampleCount.h"

class SampleBlock;
class SampleBlockFactory;
using SampleBlockFactoryPtr = std::shared_ptr<SampleBlockFactory>;

// This is an internal data structure!  For advanced use only.
class SeqBlock {
 public:
   using SampleBlockPtr = std::shared_ptr<SampleBlock>;
   SampleBlockPtr sb;
   ///the sample in the global wavetrack that this block starts at.
   sampleCount start;

   SeqBlock()
      : sb{}, start(0)
   {}

   SeqBlock(const SampleBlockPtr &sb_, sampleCount start_)
      : sb(sb_), start(start_)
   {}

   // Construct a SeqBlock with changed start, same file
   SeqBlock Plus(sampleCount delta) const
   {
      return SeqBlock(sb, start + delta);
   }
};
class BlockArray : public std::vector<SeqBlock> {};
using BlockPtrArray = std::vector<SeqBlock*>; // non-owning pointers

// Put extra symbol information in the release build, for the purpose of gathering
// profiling information (as from Windows Process Monitor), when there otherwise
// isn't a need for AUDACITY_DLL_API.
#ifdef IS_ALPHA
   #define PROFILE_DLL_API AUDACITY_DLL_API
#else
   #define PROFILE_DLL_API
#endif

class PROFILE_DLL_API Sequence final : public XMLTagHandler{
 public:

   //
   // Static methods
   //

   static void SetMaxDiskBlockSize(size_t bytes);
   static size_t GetMaxDiskBlockSize();

   //! true if nValue is one of the sampleFormat enum values
   static bool IsValidSampleFormat(const int nValue);

   //
   // Constructor / Destructor / Duplicator
   //

   Sequence(const SampleBlockFactoryPtr &pFactory, sampleFormat format);

   Sequence(const Sequence &orig, const SampleBlockFactoryPtr &pFactory);

   Sequence( const Sequence& ) = delete;
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
   void SetSamples(constSamplePtr buffer, sampleFormat format,
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
   // Must pass in the correct factory for the result.  If it's not the same
   // as in this, then block contents must be copied.
   std::unique_ptr<Sequence> Copy( const SampleBlockFactoryPtr &pFactory,
      sampleCount s0, sampleCount s1) const;
   void Paste(sampleCount s0, const Sequence *src);

   size_t GetIdealAppendLen() const;
   void Append(constSamplePtr buffer, sampleFormat format, size_t len);

   //! Append data, not coalescing blocks, returning a pointer to the new block.
   SeqBlock::SampleBlockPtr AppendNewBlock(
      constSamplePtr buffer, sampleFormat format, size_t len);
   //! Append a complete block, not coalescing
   void AppendSharedBlock(const SeqBlock::SampleBlockPtr &pBlock);
   void Delete(sampleCount start, sampleCount len);

   void SetSilence(sampleCount s0, sampleCount len);
   void InsertSilence(sampleCount s0, sampleCount len);

   const SampleBlockFactoryPtr &GetFactory() { return mpFactory; }

   //
   // XMLTagHandler callback methods for loading and saving
   //

   bool HandleXMLTag(const wxChar *tag, const wxChar **attrs) override;
   void HandleXMLEndTag(const wxChar *tag) override;
   XMLTagHandler *HandleXMLChild(const wxChar *tag) override;
   void WriteXML(XMLWriter &xmlFile) const /* not override */;

   bool GetErrorOpening() { return mErrorOpening; }

   //
   // Lock all of this sequence's sample blocks, keeping them
   // from being destroyed when closing.

   bool CloseLock();//should be called upon project close.
   // not balanced by unlocking calls.

   //
   // Manipulating Sample Format
   //

   sampleFormat GetSampleFormat() const;

   // Return true iff there is a change
   bool ConvertToSampleFormat(sampleFormat format, 
      const std::function<void(size_t)> & progressReport = {});

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

   BlockArray &GetBlockArray() { return mBlock; }
   const BlockArray &GetBlockArray() const { return mBlock; }

 private:

   //
   // Private static variables
   //

   static size_t    sMaxDiskBlockSize;

   //
   // Private variables
   //

   SampleBlockFactoryPtr mpFactory;

   BlockArray    mBlock;
   sampleFormat  mSampleFormat;

   // Not size_t!  May need to be large:
   sampleCount   mNumSamples{ 0 };

   size_t   mMinSamples; // min samples per block
   size_t   mMaxSamples; // max samples per block

   bool          mErrorOpening{ false };

   //
   // Private methods
   //

   int FindBlock(sampleCount pos) const;

   SeqBlock::SampleBlockPtr DoAppend(
      constSamplePtr buffer, sampleFormat format, size_t len, bool coalesce);

   static void AppendBlock(SampleBlockFactory *pFactory, sampleFormat format,
                           BlockArray &blocks,
                           sampleCount &numSamples,
                           const SeqBlock &b);

   static bool Read(samplePtr buffer,
                    sampleFormat format,
                    const SeqBlock &b,
                    size_t blockRelativeStart,
                    size_t len,
                    bool mayThrow);

   // Accumulate NEW block files onto the end of a block array.
   // Does not change this sequence.  The intent is to use
   // CommitChangesIfConsistent later.
   static void Blockify(SampleBlockFactory &factory,
                        size_t maxSamples,
                        sampleFormat format,
                        BlockArray &list,
                        sampleCount start,
                        constSamplePtr buffer,
                        size_t len);

   bool Get(int b,
            samplePtr buffer,
            sampleFormat format,
            sampleCount start,
            size_t len,
            bool mayThrow) const;

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

