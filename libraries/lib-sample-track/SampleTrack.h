/**********************************************************************

Audacity: A Digital Audio Editor

SampleTrack.h
@brief abstract Track sub-type that maps times to sample values

Paul Licameli split from WaveTrack.h

**********************************************************************/

#ifndef __AUDACITY_SAMPLE_TRACK__
#define __AUDACITY_SAMPLE_TRACK__

#include "SampleCount.h"
#include "SampleFormat.h"
#include "Track.h"

enum class sampleFormat : unsigned;

class SampleTrack;

using SampleTrackAttachments = ClientData::Site<
   SampleTrack,
   ClientData::Cloneable<>,
   ClientData::DeepCopying
>;

class SAMPLE_TRACK_API SampleTrack /* not final */
   : public PlayableTrack
   , public SampleTrackAttachments
{
public:
   using Attachments = SampleTrackAttachments;

   SampleTrack();
   SampleTrack(const SampleTrack &other, ProtectedCreationArg&&);
   ~SampleTrack() override;

   const TypeInfo &GetTypeInfo() const override;
   static const TypeInfo &ClassTypeInfo();

   virtual sampleFormat GetSampleFormat() const = 0;

   /*! May be called from a worker thread */
   virtual ChannelType GetChannelIgnoringPan() const = 0;

   virtual double GetRate() const = 0;

   //! @return widest effective SampleFormat in any part of the track
   virtual sampleFormat WidestEffectiveFormat() const = 0;

   //! @return whether envelope values are all unit
   virtual bool HasTrivialEnvelope() const = 0;

   //! Fetch envelope values corresponding to uniformly separated sample times starting at the given time
   virtual void GetEnvelopeValues(double *buffer, size_t bufferLen,
                         double t0) const = 0;

   //! Takes gain and pan into account
   virtual float GetChannelGain(int channel) const = 0;

   //! This returns a nonnegative number of samples meant to size a memory buffer
   virtual size_t GetBestBlockSize(sampleCount t) const = 0;

   //! This returns a nonnegative number of samples meant to size a memory buffer
   virtual size_t GetMaxBlockSize() const = 0;

   //! This returns a possibly large or negative value
   virtual sampleCount GetBlockStart(sampleCount t) const = 0;

   //! Retrieve samples from a track in floating-point format, regardless of the storage format
   /*!
    @param buffer receives the samples
    @param start starting sample, relative to absolute time zero (not to the track's offset value)
    @param len how many samples to get.  buffer is assumed sufficiently large
    @param fill how to assign values for sample positions between clips
    @param mayThrow if false, fill buffer with zeros when there is failure to retrieve samples; else throw
    @param[out] pNumWithinClips Report how many samples were copied from within clips, rather
       than filled according to fillFormat; but these were not necessarily one contiguous range.
    */
   bool GetFloats(float *buffer, sampleCount start, size_t len,
      fillFormat fill = fillZero, bool mayThrow = true,
      sampleCount * pNumWithinClips = nullptr) const
   {
      //! Cast the pointer to pass it to Get() which handles multiple destination formats
      return Get(reinterpret_cast<samplePtr>(buffer),
         floatSample, start, len, fill, mayThrow, pNumWithinClips);
   }

   //! Retrieve samples from a track in a specified format
   /*!
    @copydetails SampleTrack::GetFloats()
    @param format sample format of the destination buffer
    */
   virtual bool Get(samplePtr buffer, sampleFormat format,
      sampleCount start, size_t len,
      fillFormat fill = fillZero,
      bool mayThrow = true,
      // Report how many samples were copied from within clips, rather than
      // filled according to fillFormat; but these were not necessarily one
      // contiguous range.
      sampleCount * pNumWithinClips = nullptr) const = 0;

   /** @brief Convert correctly between an (absolute) time in seconds and a number of samples.
    *
    * This method will not give the correct results if used on a relative time (difference of two
    * times). Each absolute time must be converted and the numbers of samples differenced:
    *    sampleCount start = track->TimeToLongSamples(t0);
    *    sampleCount end = track->TimeToLongSamples(t1);
    *    sampleCount len = (sampleCount)(end - start);
    * NOT the likes of:
    *    sampleCount len = track->TimeToLongSamples(t1 - t0);
    * See also SampleTrack::TimeToLongSamples().
    * @param t0 The time (floating point seconds) to convert
    * @return The number of samples from the start of the track which lie before the given time.
    */
   sampleCount TimeToLongSamples(double t0) const;
   /** @brief Convert correctly between a number of samples and an (absolute) time in seconds.
    *
    * @param pos The time number of samples from the start of the track to convert.
    * @return The time in seconds.
    */
   double LongSamplesToTime(sampleCount pos) const;
};

ENUMERATE_TRACK_TYPE(SampleTrack)

class SAMPLE_TRACK_API WritableSampleTrack /* not final */
   : public SampleTrack
{
public:
   WritableSampleTrack();
   WritableSampleTrack(
      const WritableSampleTrack &other, ProtectedCreationArg&&);
   ~WritableSampleTrack() override;

   const TypeInfo &GetTypeInfo() const override;
   static const TypeInfo &ClassTypeInfo();

   /** @brief Append the sample data to the track. You must call Flush()
    * after the last Append.
    *
    * @return true in case a block was flushed from memory to underlying DB
    */
   virtual bool Append(constSamplePtr buffer, sampleFormat format,
      size_t len, unsigned int stride=1,
      sampleFormat effectiveFormat = widestSampleFormat /*!<
         Make the effective format of the data at least the minumum of this
         value and `format`.  (Maybe wider, if merging with preexistent data.)
         If the data are later narrowed from stored format, but not narrower
         than the effective, then no dithering will occur.
      */
   ) = 0;

   //! Flush must be called after last Append
   virtual void Flush() = 0;
};

ENUMERATE_TRACK_TYPE(WritableSampleTrack)

#endif
