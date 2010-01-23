/**********************************************************************

  Audacity: A Digital Audio Editor

  SimpleMono.h

  Dominic Mazzoni

  This abstract class simplifies the implementation of a basic
  monaural effect.  Inherit from it if your effect doesn't just
  modifies a track in place and doesn't care how many samples
  it gets at a time.  Your derived class only needs to implement
  GetEffectName, GetEffectAction, and ProcessSimpleMono.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_SIMPLE_MONO__
#define __AUDACITY_EFFECT_SIMPLE_MONO__

#include "Effect.h"

class WaveTrack;

class EffectSimpleMono:public Effect {

 public:
   virtual bool Process();

 private:
   bool ProcessOne(WaveTrack * t, sampleCount start, sampleCount end);

 protected:  

   // Override this method if you need to do things
   // before every track (including the first one)
   virtual bool NewTrackSimpleMono();

   // Override this method to actually process audio
   virtual bool ProcessSimpleMono(float *buffer, sampleCount len) = 0;

   // Other useful information
   int    mCurTrackNum;
   double mCurRate;
   double mCurT0;
   double mCurT1;
   int    mCurChannel;

};

#endif

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 2c7cdbc8-b34e-46c2-8b9b-5669bb1af61b

