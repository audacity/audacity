/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2021 Audacity Team.

   DeepLearningEffect.h
   Hugo Flores Garcia

******************************************************************/
/**

\file DeepLearningEffect.h
\brief a generic, waveform-to-waveform, deep learning effect. 

*/
/*******************************************************************/

#pragma once

#include "DeepModel.h"
#include "DeepLearningEffectBase.h"
#include "effects/Effect.h"

class wxStaticText;
class ShuttleGui;
class wxButton;
class WaveTrack;

class DeepLearningEffect final: public DeepLearningEffectBase
{
public:
   static const ComponentInterfaceSymbol Symbol;

   DeepLearningEffect();
   virtual ~DeepLearningEffect();

   // DeepLearningEffectBase implementation

   std::string GetDeepEffectID() override;

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() override;
   TranslatableString GetDescription() override;
   ManualPageID ManualPage() override;

   // EffectDefinitionInterface implementation

   EffectType GetType() override;

   // Effect implementation
   bool ProcessOne(WaveTrack * track, double tStart, double tEnd) override;

private:

   std::vector<WaveTrack::Holder> CreateSourceTracks(WaveTrack *track, 
                                             std::vector<std::string> &labels);
   void PostProcessSources(WaveTrack *leader, std::vector<WaveTrack::Holder> &sourceTracks, 
                           sampleFormat fmt, int sampleRate);

};
