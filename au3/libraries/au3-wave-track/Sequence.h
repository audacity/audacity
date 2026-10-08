/**********************************************************************

  Audacity: A Digital Audio Editor

  Sequence.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_SEQUENCE__
#define __AUDACITY_SEQUENCE__

#include <atomic>
#include <deque>
#include <functional>

#include "au3-math/SampleFormat.h"
#include "au3-math/SampleCount.h"
#include "au3-xml/XMLTagHandler.h"
#include "au3-stretching-sequence/AudioSegmentSampleView.h"

class SampleBlock;
class SampleBlockFactory;
using SampleBlockFactoryPtr = std::shared_ptr<SampleBlockFactory>;

// This is an internal data structure!  For advanced use only.
class SeqBlock
{
public:
    using SampleBlockPtr = std::shared_ptr<SampleBlock>;
    SampleBlockPtr sb;
    ///the sample in the global wavetrack that this block starts at.
    sampleCount start;

    SeqBlock()
        : sb{}, start(0)
    {}

    SeqBlock(const SampleBlockPtr& sb_, sampleCount start_)
        : sb(sb_), start(start_)
    {}

    // Construct a SeqBlock with changed start, same file
    SeqBlock Plus(sampleCount delta) const
    {
        return SeqBlock(sb, start + delta);
    }
};
class BlockArray : public std::deque<SeqBlock>
{
};
using BlockPtrArray = std::deque<SeqBlock*>; // non-owning pointers

class WAVE_TRACK_API Sequence final : public XMLTagHandler
{
public:

    static const char* Sequence_tag;
    static const char* WaveBlock_tag;

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

    Sequence(const SampleBlockFactoryPtr& pFactory, SampleFormats formats);

    //! Does not copy un-flushed append buffer data
    Sequence(const Sequence& orig, const SampleBlockFactoryPtr& pFactory);

    Sequence(const Sequence&) = delete;
    Sequence& operator=(const Sequence&) = delete;

    ~Sequence();

    //
    // Editing
    //

    sampleCount GetNumSamples() const { return mNumSamples; }

    //! Get a range of samples from the sequence
    /*
     If the requested range is not all defined, the buffer may still contain
     valid results, with zero filling

     @param start offset into sequence
     @param len number of samples to put in buffer
     @param mayThrow if true, propagate read error exceptions
     @return whether [start, start + len) was within the defined range of the
     sequence and there were no read errors
     */
    bool Get(samplePtr buffer, sampleFormat format, sampleCount start, size_t len, bool mayThrow) const;

    /*!
     Get a view of the lesser of `len` samples or what remains after `start`
     @pre `start < GetNumSamples()`
     */
    AudioSegmentSampleView
    GetFloatSampleView(sampleCount start, size_t len, bool mayThrow) const;

    //! Pass nullptr to set silence
    /*! Note that len is not size_t, because nullptr may be passed for buffer, in
       which case, silence is inserted, possibly a large amount. */
    /*! @excsafety{Strong} */
    void SetSamples(constSamplePtr buffer, sampleFormat format, sampleCount start, sampleCount len, sampleFormat effectiveFormat /*!<
         Make the effective format of the data at least the minumum of this
         value and `format`.  (Maybe wider, if merging with preexistent data.)
         If the data are later narrowed from stored format, but not narrower
         than the effective, then no dithering will occur.
      */
                    );

    // Return non-null, or else throw!
    // Must pass in the correct factory for the result.  If it's not the same
    // as in this, then block contents must be copied.
    std::unique_ptr<Sequence> Copy(const SampleBlockFactoryPtr& pFactory, sampleCount s0, sampleCount s1) const;
    /*! @excsafety{Strong} */
    void Paste(sampleCount s0, const Sequence* src);

    size_t GetIdealAppendLen() const;

    /*!
        Samples may be retained in a memory buffer, pending Flush()
        If there are exceptions, an unspecified prefix of buffer may be
        appended

        @return true if at least one sample block was added
        @excsafety{Weak}
     */
    bool Append(
        constSamplePtr buffer, sampleFormat format, size_t len, size_t stride, sampleFormat effectiveFormat /*!<
         Make the effective format of the data at least the minumum of this
         value and `format`.  (Maybe wider, if merging with preexistent data.)
         If the data are later narrowed from stored format, but not narrower
         than the effective, then no dithering will occur.
      */
        );

    /*! @excsafety{Mixed} */
    /*! @excsafety{No-fail} -- The Sequence will be in a flushed state. */
    /*! @excsafety{Partial}
    -- Some initial portion (maybe none) of the append buffer of the
     Sequence gets appended; no previously flushed contents are lost. */
    void Flush();

    //! Append data, not coalescing blocks, returning a pointer to the new block.
    //! No dithering applied.
    /*! @excsafety{Strong} */
    SeqBlock::SampleBlockPtr AppendNewBlock(
        constSamplePtr buffer, sampleFormat format, size_t len);
    //! Append a complete block, not coalescing
    /*! @excsafety{Strong} */
    void AppendSharedBlock(const SeqBlock::SampleBlockPtr& pBlock);
    /*! @excsafety{Strong} */
    void Delete(sampleCount start, sampleCount len);

    /*! @excsafety{Strong} */
    void SetSilence(sampleCount s0, sampleCount len);

    //! If `s` falls strictly inside a block, replaces that block with two new
    //! blocks that meet at `s`. The samples are unchanged.
    //! @return whether a block was split
    bool SplitBlockAt(sampleCount s);

    //! Replaces the `count` blocks starting at index `first` by `blocks`; the
    //! starts of the following blocks are shifted if the length changes
    void ReplaceBlocks(size_t first, size_t count, const std::vector<SeqBlock::SampleBlockPtr>& blocks);

    //! Edit-locks the blocks that start in [s0, s1). Silent blocks are first
    //! replaced by ordinary blocks of zeros, because silent blocks are shared
    //! by all silences of the same length and have no id of their own.
    /*!
     @pre no block straddles `s0` or `s1` (see SplitBlockAt)
     @return whether any block was newly locked
     */
    bool LockBlocks(sampleCount s0, sampleCount s1);

    //! Replaces each edit-locked block by an unlocked copy of its samples, so
    //! that a user-level copy doesn't share (and inherit the lock of) the
    //! original. Not for undo backups, which must share the same blocks.
    //! @return whether any block was replaced
    bool DeepCopyEditLockedBlocks();
    /*! @excsafety{Strong} */
    void InsertSilence(sampleCount s0, sampleCount len);

    const SampleBlockFactoryPtr& GetFactory() const
    {
        return mpFactory;
    }

    //
    // XMLTagHandler callback methods for loading and saving
    //

    bool HandleXMLTag(const std::string_view& tag, const AttributesList& attrs) override;
    void HandleXMLEndTag(const std::string_view& tag) override;
    XMLTagHandler* HandleXMLChild(const std::string_view& tag) override;
    void WriteXML(XMLWriter& xmlFile) const /* not override */;

    bool GetErrorOpening() const { return mErrorOpening; }

    //
    // Lock all of this sequence's sample blocks, keeping them
    // from being destroyed when closing.

    //! Should be called upon project close.  Not balanced by unlocking calls.
    /*! @excsafety{No-fail} */
    bool CloseLock() noexcept;

    //
    // Manipulating Sample Format
    //

    SampleFormats GetSampleFormats() const;

    //! @return whether there was a change of format
    /*! @excsafety{Strong} */
    bool ConvertToSampleFormat(sampleFormat format, const std::function<void(size_t)>& progressReport = {});

    //
    // Retrieving summary info
    //

    std::pair<float, float> GetMinMax(
        sampleCount start, sampleCount len, bool mayThrow) const;
    float GetRMS(sampleCount start, sampleCount len, bool mayThrow) const;

    //
    // Getting block size and alignment information
    //

    // These return a nonnegative number of samples meant to size a memory buffer
    size_t GetBestBlockSize(sampleCount start) const;
    size_t GetMaxBlockSize() const;
    size_t GetIdealBlockSize() const;

    //
    // This should only be used if you really, really know what
    // you're doing!
    //
    const BlockArray& GetBlockArray() const { return mBlock; }

    size_t GetAppendBufferLen() const { return mAppendBufferLen; }
    constSamplePtr GetAppendBuffer() const { return mAppendBuffer.ptr(); }

private:

    //
    // Private static variables
    //

    static size_t sMaxDiskBlockSize;

    //
    // Private variables
    //

    SampleBlockFactoryPtr mpFactory;
    std::atomic<size_t> mBlockCount{ 0 };
    BlockArray mBlock;
    SampleFormats mSampleFormats;

    // Not size_t!  May need to be large:
    sampleCount mNumSamples{ 0 };

    size_t mMinSamples;  // min samples per block
    size_t mMaxSamples;  // max samples per block

    SampleBuffer mAppendBuffer {};
    size_t mAppendBufferLen { 0 };
    sampleFormat mAppendEffectiveFormat{ narrowestSampleFormat };

    bool mErrorOpening{ false };

    //
    // Private methods
    //

    //! @return possibly a large or negative value
    sampleCount GetBlockStart(sampleCount position) const;

    //! Does not do any dithering
    /*! @excsafety{Strong} */
    SeqBlock::SampleBlockPtr DoAppend(
        constSamplePtr buffer, sampleFormat format, size_t len, bool coalesce);

    static void AppendBlock(SampleBlockFactory* pFactory, sampleFormat format, BlockArray& blocks, sampleCount& numSamples,
                            const SeqBlock& b);

    // Accumulate NEW block files onto the end of a block array.
    // Does not change this sequence.  The intent is to use
    // CommitChangesIfConsistent later.
    static void Blockify(SampleBlockFactory& factory, size_t maxSamples, sampleFormat format, BlockArray& list, sampleCount start,
                         constSamplePtr buffer, size_t len);

    bool Get(int b, samplePtr buffer, sampleFormat format, sampleCount start, size_t len, bool mayThrow) const;

public:

    //
    // Public methods
    //

    int FindBlock(sampleCount pos) const;

    static bool Read(samplePtr buffer, sampleFormat format, const SeqBlock& b, size_t blockRelativeStart, size_t len, bool mayThrow);

    // This function throws if the track is messed up
    // because of inconsistent block starts & lengths
    void ConsistencyCheck(const wxChar* whereStr, bool mayThrow = true) const;

    // This function prints information to stdout about the blocks in the
    // tracks and indicates if there are inconsistencies.
    static void DebugPrintf(const BlockArray& block, sampleCount numSamples, wxString* dest);

private:
    static void ConsistencyCheck(const BlockArray& block, size_t maxSamples, size_t from, sampleCount numSamples, const wxChar* whereStr,
                                 bool mayThrow = true);

    // The next two are used in methods that give a strong guarantee.
    // They either throw because final consistency check fails, or swap the
    // changed contents into place.

    void CommitChangesIfConsistent(BlockArray& newBlock, sampleCount numSamples, const wxChar* whereStr);

    void AppendBlocksIfConsistent(BlockArray& additionalBlocks, bool replaceLast, sampleCount numSamples, const wxChar* whereStr);
};

#endif // __AUDACITY_SEQUENCE__
