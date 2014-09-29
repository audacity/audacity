/**********************************************************************

  Audacity: A Digital Audio Editor

  Generator.h

  Two Abstract classes, Generator, and BlockGenerator, that effects which
  generate audio should derive from.

  Block Generator breaks the synthesis task up into smaller parts.

  Dominic Mazzoni
  Vaughan Johnson

**********************************************************************/


#ifndef __AUDACITY_GENERATOR__
#define __AUDACITY_GENERATOR__

#include "Effect.h"

// Base class for Generators (effects which fill a given duration)
class Generator : public Effect
{
public:
   Generator() : mDuration(sDefaultGenerateLen) { }

protected:
   // Amount of time to generate, in seconds
   double mDuration;

   // [ GenerateTrack() must be overridden by the actual generator class ]
   // Precondition:  mDuration > 0.0
   // Postcondition: <tmp> is filled with the data intended for <track>
   virtual bool GenerateTrack(WaveTrack *tmp,
                              const WaveTrack &track,
                              int ntrack) = 0;

   virtual bool Init() { return true; }

   // Actions to perform at the respective points in the generation process
   virtual void BeforeGenerate() { };
   virtual void BeforeTrack(const WaveTrack & WXUNUSED(track)) { };

   // Actions to perform upon respective outcomes
   // (For example, Success might save the parameters that were used.)
   virtual void Success() { };
   virtual void Failure() { };

   // Precondition:
   // mDuration is set to the amount of time to generate in seconds
   // Postcondition:
   // If mDuration was valid (>= 0), then the tracks are replaced by the
   // generated results and true is returned. Otherwise, return false.
   bool Process();
};

// Abstract generator which creates the sound in discrete blocks, whilst
// showing a progress bar
class BlockGenerator : public Generator {
public:
   BlockGenerator() { }
protected:
   // Number of samples to generate
   sampleCount numSamples;

   // Postcondition: data[0..block) filled with generated samples
   virtual void GenerateBlock(float *data,
                              const WaveTrack &track,
                              sampleCount block) = 0;

   // Generate the track, one block at a time, & adding the results to tmp
   bool GenerateTrack(WaveTrack *tmp, const WaveTrack &track, int ntrack);
};

#endif
