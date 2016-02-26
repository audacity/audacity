/**********************************************************************

  Audacity: A Digital Audio Editor

  Reverse.h

  Mark Phillips

  This class reverses the selected audio.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_REVERSE__
#define __AUDACITY_EFFECT_REVERSE__

#include <wx/string.h>

#include "Effect.h"

#define REVERSE_PLUGIN_SYMBOL XO("Reverse")

class EffectReverse final : public Effect
{
public:
   EffectReverse();
   virtual ~EffectReverse();

   // IdentInterface implementation

   wxString GetSymbol() override;
   wxString GetDescription() override;

   // EffectIdentInterface implementation

   EffectType GetType() override;
   bool IsInteractive() override;

   // Effect implementation

   bool Process() override;

private:
   // EffectReverse implementation

   bool ProcessOneClip(int count, WaveTrack* track,
                   sampleCount start, sampleCount len, sampleCount originalStart, sampleCount originalEnd);
   bool ProcessOneWave(int count, WaveTrack* track, sampleCount start, sampleCount len);
 };

#endif

