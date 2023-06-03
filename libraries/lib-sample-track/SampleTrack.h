/**********************************************************************

Audacity: A Digital Audio Editor

SampleTrack.h
@brief abstract Track sub-type that maps times to sample values

Paul Licameli split from WaveTrack.h

**********************************************************************/

#ifndef __AUDACITY_SAMPLE_TRACK__
#define __AUDACITY_SAMPLE_TRACK__

#include "PlayableTrack.h"
#include "SampleCount.h"
#include "SampleFormat.h"
#include "WideSampleSequence.h"

enum class sampleFormat : unsigned;

class SampleTrack;

using SampleTrackAttachments = ClientData::Site<
   SampleTrack,
   ClientData::Cloneable< ClientData::UniquePtr >,
   ClientData::DeepCopying
>;

class SAMPLE_TRACK_API SampleTrack /* not final */
   : public PlayableTrack
   , public SampleTrackAttachments
   , public WideSampleSequence
{
public:
   using Attachments = SampleTrackAttachments;

   SampleTrack();
   SampleTrack(const SampleTrack &other, ProtectedCreationArg&&);
   ~SampleTrack() override;

   // Fix the otherwise ambiguous lookup of these virtual function names
   using Track::GetStartTime;
   using Track::GetEndTime;

   const TypeInfo &GetTypeInfo() const override;
   static const TypeInfo &ClassTypeInfo();

   virtual sampleFormat GetSampleFormat() const = 0;
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
