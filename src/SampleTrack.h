/**********************************************************************

Audacity: A Digital Audio Editor

SampleTrack.h
@brief abstract Track sub-type that maps times to sample values

Paul Licameli split from WaveTrack.h

**********************************************************************/

#ifndef __AUDACITY_SAMPLE_TRACK__
#define __AUDACITY_SAMPLE_TRACK__

#include "Track.h"

class AUDACITY_DLL_API SampleTrack /* not final */
   : public PlayableTrack {
public:
   ~SampleTrack() override;

   const TypeInfo &GetTypeInfo() const override;
   static const TypeInfo &ClassTypeInfo();
};

ENUMERATE_TRACK_TYPE(SampleTrack)

#endif

