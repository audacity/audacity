/**********************************************************************
   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2021 Audacity Team.
   DeepLearningAnalyzer.h
   Hugo Flores Garcia
   Aldo Aguilar
******************************************************************/
/**
\file DeepLearningAnalyzer.h
\brief A labeler which uses a deep learning model process an input audio track, and annotate it with labels. 
To do this, labeler models should output class indexes and timestamp data to write labels to a Label track.
*******************************************************************/

#pragma once

#include "DeepModel.h"
#include "DeepLearningEffectBase.h"
#include "effects/Effect.h"

class wxStaticText;
class ShuttleGui;
class wxButton;
class WaveTrack;

using Stamp = std::pair<double, double>;

class DeepLearningAnalyzer final: public DeepLearningEffectBase
{
public:
   static const ComponentInterfaceSymbol Symbol;

   DeepLearningAnalyzer();
   virtual ~DeepLearningAnalyzer();

   std::string GetDeepEffectID() override;

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;
   ManualPageID ManualPage() const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;

   // Effect implementation
   bool ProcessOne(WaveTrack *track, double tStart, double tEnd) override;

private:
   void TensorToLabelTrack(torch::Tensor output, AddedAnalysisTrack &labelTrack, 
                           double tStart, double tEnd, torch::Tensor timestamps, 
                           const std::vector<std::string> &classList);
};
