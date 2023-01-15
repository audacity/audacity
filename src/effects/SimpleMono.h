/**********************************************************************

  Audacity: A Digital Audio Editor

  SimpleMono.h

  Dominic Mazzoni

  This abstract class simplifies the implementation of a basic
  monaural effect.  Inherit from it if your effect doesn't just
  modifies a track in place and doesn't care how many samples
  it gets at a time.  Your derived class only needs to implement
  GetSymbol, GetEffectAction, and ProcessSimpleMono.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_SIMPLE_MONO__
#define __AUDACITY_EFFECT_SIMPLE_MONO__

#include "StatefulEffect.h"

class WaveTrack;

class EffectSimpleMono /* not final */ : public StatefulEffect
{
public:
   bool Process(EffectContext &context,
      EffectInstance &instance, EffectSettings &settings) override;

protected:

   // Override this method if you need to do things
   // before every track (including the first one)
   // NEW override
   virtual bool NewTrackSimpleMono() = 0;

   // Override this method to actually process audio
   virtual bool ProcessSimpleMono(float *buffer, size_t len) = 0;

protected:
   // Other useful information
   int    mCurTrackNum;
   double mCurRate;
   double mCurT0;
   double mCurT1;
   int    mCurChannel;

private:
   bool ProcessOne(EffectContext &context,
      WaveTrack * t, sampleCount start, sampleCount end);
};

#endif
