/**********************************************************************

  Audacity: A Digital Audio Editor

  Reverse.h

  Mark Phillips

  This class reverses the selected audio.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_REVERSE__
#define __AUDACITY_EFFECT_REVERSE__

#include <wx/string.h>

#include "../WaveTrack.h"

#include "Effect.h"

#define REVERSE_PLUGIN_SYMBOL XO("Reverse")

class EffectReverse : public Effect
{
public:
   EffectReverse();
   virtual ~EffectReverse();

   // IdentInterface implementation

   virtual wxString GetSymbol();
   virtual wxString GetDescription();

   // EffectIdentInterface implementation

   virtual EffectType GetType();
   virtual bool IsInteractive();

   // Effect implementation

   virtual bool Process();

private:
   // EffectReverse implementation

   bool ProcessOneClip(int count, WaveTrack* track,
                   sampleCount start, sampleCount len, sampleCount originalStart, sampleCount originalEnd);
   bool ProcessOneWave(int count, WaveTrack* track, sampleCount start, sampleCount len);
 };

#endif

