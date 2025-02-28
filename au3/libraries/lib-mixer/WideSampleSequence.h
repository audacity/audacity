/**********************************************************************

Audacity: A Digital Audio Editor

WideSampleSequence.h

Paul Licameli split from SampleFrame.h

**********************************************************************/
#ifndef __AUDACITY_WIDE_SAMPLE_SEQUENCE_
#define __AUDACITY_WIDE_SAMPLE_SEQUENCE_

#include "AudioGraphChannel.h"
#include "SampleCount.h"
#include "SampleFormat.h"

class WideSampleSequence;

//! An interface for random-access fetches from a collection of streams of
//! samples, associated with the same time; also defines an envelope that
//! applies to all the streams.
class MIXER_API WideSampleSequence : public AudioGraph::Channel
{
public:
    virtual ~WideSampleSequence();

    //! A constant property
    /*!
     @post result: `result > 0`
     */
    virtual size_t NChannels() const = 0;

    //! Extra gain factor to apply to a channel when mixing,
    //! may change asynchronously
    virtual float GetChannelVolume(int channel) const = 0;

    //! Retrieve samples from a sequence in floating-point format, regardless of
    //! the storage format
    /*!
     @param iChannel index of first channel to fetch
     @param nBuffers counts buffers
     @param buffers receive the samples
     @param start starting sample, relative to absolute time zero
     @param len how many samples to get.  buffers are assumed sufficiently large
     @param fill how to assign values for sample positions between clips
     @param mayThrow if false, fill buffer with zeros when there is failure to
        retrieve samples; else throw
     @param[out] pNumWithinClips Report how many samples were copied from within
        clips, rather than filled according to fillFormat; but these were not
        necessarily one contiguous range.

     @pre `iChannel + nBuffers <= NChannels()`
     @return false when `mayThrow` is false and not all samples could be
        retrieved
     @post if return value is false, buffers are zero-filled
     */
    bool GetFloats(size_t iChannel, size_t nBuffers, float* const buffers[], sampleCount start, size_t len, bool backwards = false,
                   fillFormat fill = FillFormat::fillZero, bool mayThrow = true, sampleCount* pNumWithinClips = nullptr) const;

    //! Retrieve samples of one of the channels from a sequence in a specified
    //! format
    /*!
     @param format sample format of the destination buffer
     @param backward retrieves samples from `start` (inclusive) to `start + len`
     if false, else from `start` (exclusive) to `start - len` in reverse order.
     @return whether successful; if not, assume nothing about buffer contents
     */
    virtual bool DoGet(
        size_t iChannel, size_t nBuffers, const samplePtr buffers[], sampleFormat format, sampleCount start, size_t len, bool backward,
        fillFormat fill = FillFormat::fillZero, bool mayThrow = true,
        // Report how many samples were copied from within clips, rather than
        // filled according to fillFormat; but these were not necessarily one
        // contiguous range.
        sampleCount* pNumWithinClips = nullptr) const = 0;

    virtual double GetStartTime() const = 0;
    virtual double GetEndTime() const = 0;
    virtual double GetRate() const = 0;

    //! Convert correctly between an (absolute) time in seconds and a number of
    //! samples.
    /*!
     This method will not give the correct results if used on a relative time
     (difference of two times). Each absolute time must be converted and the
     numbers of samples differenced:
         sampleCount start = sequence->TimeToLongSamples(t0);
         sampleCount end = sequence->TimeToLongSamples(t1);
         sampleCount len = (sampleCount)(end - start);
     NOT the likes of:
         sampleCount len = sequence->TimeToLongSamples(t1 - t0);
     See also WideSampleSequence::TimeToLongSamples().
     @param t0 The time (floating point seconds) to convert
     @return The number of samples from the start of the sequence which lie
     before the given time
     */
    sampleCount TimeToLongSamples(double t0) const;
    //! Convert correctly between a number of samples and an (absolute) time in
    //! seconds.
    /*!
     @param pos The time number of samples from the start of the sequence to
     convert
     @return The time in seconds
     */
    double LongSamplesToTime(sampleCount pos) const;

    /*!
     @return `LongSamplesToTime(TimeToLongSamples(t))`
     */
    double SnapToSample(double t) const;

    //! @return widest effective SampleFormat in any part of the track
    virtual sampleFormat WidestEffectiveFormat() const = 0;

    //! @return whether envelope values are all unit
    virtual bool HasTrivialEnvelope() const = 0;

    //! Fetch envelope values corresponding to uniformly separated sample times
    //! starting at the given time
    /*!
     @param backwards if true, fetch values in reverse order, from `t0` to
        `t0 - bufferLen / rate`
     */
    virtual void GetEnvelopeValues(
        double* buffer, size_t bufferLen, double t0, bool backwards) const = 0;
};

#endif
