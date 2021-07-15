/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2021 Audacity Team.
   License: GPL v2.  See License.txt.

   Labeler.h
   Hugo Flores Garcia
   Aldo Aguilar

******************************************************************/
/**

\file Labeler
\brief Labeler is an effect for labeling audio components in a track. 

******************************************************************/
/**


\class EffectLabeler
\breif A labeler which uses a deep learning model to output probits and
time sereies data to add labels to an audio track.


//*******************************************************************/

#ifndef __AUDACITY_EFFECT_DEEPLEARNING__
#define __AUDACITY_EFFECT_DEEPLEARNING__

#include "deeplearning/DeepModel.h"
#include "deeplearning/EffectDeepLearning.h"
#include "Effect.h"

class wxStaticText;
class ShuttleGui;
class wxButton;

class EffectLabeler final: public EffectDeepLearning
{
public:
   static const ComponentInterfaceSymbol Symbol;

   EffectLabeler();
   virtual ~EffectLabeler();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() override;
   TranslatableString GetDescription() override;
   wxString ManualPage() override;

   // EffectDefinitionInterface implementation

   EffectType GetType() override;

   // Effect implementation
   bool ProcessOne(WaveTrack * track, double tStart, double tEnd) override;

   void PopulateOrExchange(ShuttleGui &S) override;
   void PopulateMetadata(ShuttleGui &S);
   void AddMetadataEntry(ShuttleGui &S, std::string desc, std::string key);

private:
   // handlers
   void OnLoadButton(wxCommandEvent &event);

   std::vector<WaveTrack::Holder> CreateSourceTracks(WaveTrack *track, 
                                             std::vector<std::string> &labels);
   void PostProcessSources(std::vector<WaveTrack::Holder> &sourceTracks, 
                           sampleFormat fmt, int sampleRate);

private:

   wxButton *mLoadModelBtn;
   wxStaticText *mDescription;
   std::vector<wxString> mClasses;

   std::map<std::string, wxStaticText*> mMetadataFields;
   void UpdateMetadataFields();
   void TensorToLabelTrack(torch::Tensor output, std::shared_ptr<AddedAnalysisTrack> labelTrack, 
                                   double tStart, double tEnd, torch::Tensor timestamps);
   DECLARE_EVENT_TABLE()
};

#endif