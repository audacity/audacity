/**********************************************************************

  Audacity: A Digital Audio Editor

  Generator.h

  Effects that generate audio can derive from Generator.

  Dominic Mazzoni
  Vaughan Johnson

**********************************************************************/


#ifndef __AUDACITY_GENERATOR__
#define __AUDACITY_GENERATOR__

#include "StatefulEffect.h"
#include "SampleCount.h"

class TrackList;

// Base class for Generators (effects which fill a given duration)
class Generator /* not final */ : public StatefulEffect
{
public:
   Generator() { }

protected:
   //! GenerateTrack() must be overridden by the actual generator class
   /*!
    @pre `mDuration > 0.0`
    @pre `tmp` contains exactly one channel group
    @post `tmp` is filled with data
    */
   virtual bool GenerateTrack(const EffectSettings &settings, TrackList &tmp)
      = 0;

   // Precondition:
   // mDuration is set to the amount of time to generate in seconds
   // Postcondition:
   // If mDuration was valid (>= 0), then the tracks are replaced by the
   // generated results and true is returned. Otherwise, return false.
   AUDACITY_DLL_API bool Process(EffectInstance &instance, EffectSettings &settings) override;
};

#endif
