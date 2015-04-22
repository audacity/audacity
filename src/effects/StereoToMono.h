/**********************************************************************

  Audacity: A Digital Audio Editor

  StereoToMono.h

  Lynn Allan

**********************************************************************/

#ifndef __AUDACITY_EFFECT_STEREO_TO_MONO__
#define __AUDACITY_EFFECT_STEREO_TO_MONO__

#include <wx/string.h>

#include "Effect.h"

#define STEREOTOMONO_PLUGIN_SYMBOL XO("Stereo To Mono")

class EffectStereoToMono : public Effect
{
public:
   EffectStereoToMono();
   virtual ~EffectStereoToMono();

   // IdentInterface implementation

   virtual wxString GetSymbol();
   virtual wxString GetDescription();

   // EffectIdentInterface implementation

   virtual EffectType GetType();
   virtual bool IsInteractive();

   // EffectClientInterface implementation

   virtual int GetAudioInCount();
   virtual int GetAudioOutCount();

   // Effect implementation

   virtual bool Process();
   virtual bool IsHidden();

private:
   // EffectStereoToMono implementation

   bool ProcessOne(int count);

private:
   sampleCount mStart;
   sampleCount mEnd;
   WaveTrack *mLeftTrack;
   WaveTrack *mRightTrack;
   WaveTrack *mOutTrack;
};

#endif

