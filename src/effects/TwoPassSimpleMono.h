/**********************************************************************

  Audacity: A Digital Audio Editor

  TwoPassSimpleMono.h

  Dominic Mazzoni

  This bit by Martyn Shaw.

**********************************************************************/
#ifndef __AUDACITY_EFFECT_TWOPASSSIMPLEMONO__
#define __AUDACITY_EFFECT_TWOPASSSIMPLEMONO__

#include "SimpleMono.h"


class WaveTrack;

class AUDACITY_DLL_API EffectTwoPassSimpleMono /* not final */
   : public StatefulEffect
{
public:
   // Effect implementation

   bool Process(EffectContext &context,
      EffectInstance &instance, EffectSettings &settings) override;

protected:
   // EffectTwoPassSimpleMono implementation

   // Override these methods if you need to initialize something
   // before each pass. Return None if processing should stop.
   // These should not depend on mOutputTracks having been set up via CopyInputTracks().
   virtual bool InitPass1();
   virtual bool InitPass2();

   // NEW virtuals

   // Override these methods if you need to do things
   // before every track (including the first one)
   virtual bool NewTrackPass1();
   virtual bool NewTrackPass2();

   // Override this method to actually process audio
   virtual bool ProcessPass1
      (float * WXUNUSED(buffer), size_t WXUNUSED(len))
   { return false; }

   virtual bool ProcessPass2
      (float * WXUNUSED(buffer), size_t WXUNUSED(len))
   { return false; }

   // Override this method to actually process audio with access to 2 sequential buffers at a time
   // Either buffer1 or buffer2 may be modified as needed
   // This allows implementation of processing with delays
   // The default just calls the one-buffer-at-a-time method
   virtual bool TwoBufferProcessPass1
      (float *buffer1, size_t len1, float * WXUNUSED(buffer2), size_t WXUNUSED(len2))
   { if(buffer1 != NULL) return ProcessPass1(buffer1, len1); else return true; }
   virtual bool TwoBufferProcessPass2
      (float *buffer1, size_t len1, float * WXUNUSED(buffer2), size_t WXUNUSED(len2))
   { if(buffer1 != NULL) return ProcessPass2(buffer1, len1); else return true; }

   // End of NEW virtuals

   // Call this if you know in advance that no second pass will be needed.
   // This is used as a hint for the progress bar
   void DisableSecondPass() { mSecondPassDisabled = true; }

   // Other useful information
   int    mCurTrackNum;
   double mCurRate;
   double mCurT0;
   double mCurT1;
   int    mCurChannel;
   int    mPass;
   bool   mSecondPassDisabled;

   std::shared_ptr<TrackList> mWorkTracks;
   std::shared_ptr<TrackList> *mTrackLists[2];

private:
   bool ProcessOne(EffectContext &context,
      WaveTrack * t, WaveTrack * outTrack,
      sampleCount start, sampleCount end);
   bool ProcessPass(EffectContext &context, EffectSettings &settings);
};

#endif
