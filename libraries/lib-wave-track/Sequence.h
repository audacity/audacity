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
#include "SequenceInterface.h"

#include "SampleCount.h"

class SampleBlock;
class SampleBlockFactory;
using SampleBlockFactoryPtr = std::shared_ptr<SampleBlockFactory>;

class WAVE_TRACK_API Sequence final : public SequenceInterface {
 public:

   //
   // Static methods
   //

   static void SetMaxDiskBlockSize(size_t bytes);
   static size_t GetMaxDiskBlockSize();

   //! true if nValue is one of the sampleFormat enum values
   static bool IsValidSampleFormat(const int nValue);

private:
   friend class CachingSequence;
   //
   // Constructor / Destructor / Duplicator
   //

   Sequence(const SampleBlockFactoryPtr &pFactory, SampleFormats formats);

   Sequence(const Sequence &orig, const SampleBlockFactoryPtr &pFactory);

   Sequence( const Sequence& ) = delete;
   Sequence& operator= (const Sequence&) PROHIBITED;

public:
   ~Sequence();

   //
   // Editing
   //

   sampleCount GetNumSamples() const override { return mNumSamples; }

   bool Get(samplePtr buffer, sampleFormat format,
            sampleCount start, size_t len, bool mayThrow) const override;

   //! Pass nullptr to set silence
   /*! Note that len is not size_t, because nullptr may be passed for buffer, in
      which case, silence is inserted, possibly a large amount. */
   /*! @excsafety{Strong} */
   void SetSamples(constSamplePtr buffer, sampleFormat format,
      sampleCount start, sampleCount len,
      sampleFormat effectiveFormat /*!<
         Make the effective format of the data at least the minumum of this
         value and `format`.  (Maybe wider, if merging with preexistent data.)
         If the data are later narrowed from stored format, but not narrower
         than the effective, then no dithering will occur.
      */
   ) override;

   // Return non-null, or else throw!
   // Must pass in the correct factory for the result.  If it's not the same
   // as in this, then block contents must be copied.
   std::unique_ptr<SequenceInterface> Copy(
      const SampleBlockFactoryPtr& pFactory, sampleCount s0,
      sampleCount s1) const override;
   /*! @excsafety{Strong} */
   void Paste(sampleCount s0, const SequenceInterface *src) override;

   size_t GetIdealAppendLen() const override;

   /*!
       Samples may be retained in a memory buffer, pending Flush()
       If there are exceptions, an unspecified prefix of buffer may be
       appended
   
       @return true if at least one sample block was added
       @excsafety{Weak}
    */
   bool Append(
      constSamplePtr buffer, sampleFormat format, size_t len, size_t stride,
      sampleFormat effectiveFormat /*!<
         Make the effective format of the data at least the minumum of this
         value and `format`.  (Maybe wider, if merging with preexistent data.)
         If the data are later narrowed from stored format, but not narrower
         than the effective, then no dithering will occur.
      */
   ) override;

   /*! @excsafety{Mixed} */
   /*! @excsafety{No-fail} -- The Sequence will be in a flushed state. */
   /*! @excsafety{Partial}
   -- Some initial portion (maybe none) of the append buffer of the
    Sequence gets appended; no previously flushed contents are lost. */
   void Flush() override;

   //! Append data, not coalescing blocks, returning a pointer to the new block.
   //! No dithering applied.
   /*! @excsafety{Strong} */
   SeqBlock::SampleBlockPtr AppendNewBlock(
      constSamplePtr buffer, sampleFormat format, size_t len) override;
   //! Append a complete block, not coalescing
   /*! @excsafety{Strong} */
   void AppendSharedBlock(const SeqBlock::SampleBlockPtr &pBlock) override;
   /*! @excsafety{Strong} */
   void Delete(sampleCount start, sampleCount len) override;

   /*! @excsafety{Strong} */
   void SetSilence(sampleCount s0, sampleCount len) override;
   /*! @excsafety{Strong} */
   void InsertSilence(sampleCount s0, sampleCount len) override;

   const SampleBlockFactoryPtr &GetFactory() const override { return mpFactory; }

   //
   // XMLTagHandler callback methods for loading and saving
   //

   bool HandleXMLTag(const std::string_view& tag, const AttributesList& attrs) override;
   void HandleXMLEndTag(const std::string_view& tag) override;
   XMLTagHandler *HandleXMLChild(const std::string_view& tag) override;
   void WriteXML(XMLWriter &xmlFile) const override;

   bool GetErrorOpening() const { return mErrorOpening; }

   //
   // Lock all of this sequence's sample blocks, keeping them
   // from being destroyed when closing.

   bool CloseLock() override;//should be called upon project close.
   // not balanced by unlocking calls.

   //
   // Manipulating Sample Format
   //

   SampleFormats GetSampleFormats() const override;

   //! @return whether there was a change
   /*! @excsafety{Strong} */
   bool ConvertToSampleFormat(sampleFormat format,
      const std::function<void(size_t)> & progressReport = {}) override;

   //
   // Retrieving summary info
   //

   std::pair<float, float> GetMinMax(
      sampleCount start, sampleCount len, bool mayThrow) const override;
   float GetRMS(sampleCount start, sampleCount len, bool mayThrow) const override;

   //
   // Getting block size and alignment information
   //

   size_t GetMaxBlockSize() const override;

   //
   // This should only be used if you really, really know what
   // you're doing!
   //

   const BlockArray &GetBlockArray() const override { return mBlock; }

   size_t GetAppendBufferLen() const override { return mAppendBufferLen; }
   constSamplePtr GetAppendBuffer() const override { return mAppendBuffer.ptr(); }

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
   SampleFormats  mSampleFormats;

   // Not size_t!  May need to be large:
   sampleCount   mNumSamples{ 0 };

   size_t   mMinSamples; // min samples per block
   size_t   mMaxSamples; // max samples per block

   SampleBuffer  mAppendBuffer {};
   size_t        mAppendBufferLen { 0 };
   sampleFormat  mAppendEffectiveFormat{ narrowestSampleFormat };

   bool          mErrorOpening{ false };

   //
   // Private methods
   //

   void Paste(sampleCount s0, const Sequence *src);

   //! Does not do any dithering
   /*! @excsafety{Strong} */
   SeqBlock::SampleBlockPtr DoAppend(
      constSamplePtr buffer, sampleFormat format, size_t len, bool coalesce);

   static void AppendBlock(SampleBlockFactory *pFactory, sampleFormat format,
                           BlockArray &blocks,
                           sampleCount &numSamples,
                           const SeqBlock &b);

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

   int FindBlock(sampleCount pos) const override;

   static bool Read(samplePtr buffer, sampleFormat format,
             const SeqBlock &b,
             size_t blockRelativeStart, size_t len, bool mayThrow);

   // This function throws if the track is messed up
   // because of inconsistent block starts & lengths
   void ConsistencyCheck (const wxChar *whereStr, bool mayThrow = true) const override;

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

