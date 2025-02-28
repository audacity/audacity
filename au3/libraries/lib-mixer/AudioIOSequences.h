/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioIOSequences.h

  Paul Licameli

  @brief interfaces for playable and recordable sample channel groups

**********************************************************************/
#ifndef __AUDACITY_AUDIO_IO_SEQUENCES__
#define __AUDACITY_AUDIO_IO_SEQUENCES__

#include "WideSampleSequence.h"
class ChannelGroup;

/*!
 Extends the interface for random access into a sample stream with tests for
 muting and solo
 */
struct MIXER_API PlayableSequence : WideSampleSequence {
    ~PlayableSequence() override;

    //! Find associated ChannelGroup if any
    virtual const ChannelGroup* FindChannelGroup() const = 0;

    //! May vary asynchronously
    virtual bool GetSolo() const = 0;

    //! May vary asynchronously
    virtual bool GetMute() const = 0;
};

using ConstPlayableSequences
    =std::vector<std::shared_ptr<const PlayableSequence> >;

/*!
 An interface for recording, by appending sequentially
 (but it also requires random access for insertion of silence)
 */
struct MIXER_API RecordableSequence {
    virtual ~RecordableSequence();
    virtual sampleFormat GetSampleFormat() const = 0;
    virtual double GetRate() const = 0;

    //! A constant property
    /*!
     @post result: `result > 0`
     */
    virtual size_t NChannels() const = 0;

    /** @brief Append the sample data to the track. You must call Flush()
     after the last Append.
     @pre `iChannel < NChannels()`
     @return true in case a block was flushed from memory to underlying DB
     */
    virtual bool Append(size_t iChannel, constSamplePtr buffer, sampleFormat format, size_t len, unsigned int stride, sampleFormat effectiveFormat /*!<
         Make the effective format of the data at least the minumum of this
         value and `format`.  (Maybe wider, if merging with preexistent data.)
         If the data are later narrowed from stored format, but not narrower
         than the effective, then no dithering will occur.
      */
                        ) = 0;

    //! Flush must be called after last Append
    virtual void Flush() = 0;

    //! Called in exception handling after possibly unbalanced calls to Append
    //! in different channels. Allows the sequence to repair consistency.
    virtual void RepairChannels() = 0;

    virtual void InsertSilence(double t, double len) = 0;
};

using RecordableSequences = std::vector<std::shared_ptr<RecordableSequence> >;

//! This is defined just to enable `dynamic_cast` on it
class MIXER_API OtherPlayableSequence
{
public:
    virtual ~OtherPlayableSequence();
};

#endif
