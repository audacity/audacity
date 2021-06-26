/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2021 Audacity Team.
   License: GPL v2.  See License.txt.

   SourceSep.h
   Hugo Flores Garcia

******************************************************************/
/**

\class SourceSep
\brief SourceSep is an effect for source separation using DeepLearning 

TODO: add more desc

*/
/*******************************************************************/

#ifndef __AUDACITY_EFFECT_DEEPLEARNING__
#define __AUDACITY_EFFECT_DEEPLEARNING__

#include "deeplearning/DeepModel.h"
#include "deeplearning/EffectDeepLearning.h"
#include "Effect.h"

class wxStaticText;
class ShuttleGui;
class wxButton;

class EffectSourceSep final: public EffectDeepLearning
{
public:
   static const ComponentInterfaceSymbol Symbol;

   EffectSourceSep();
   virtual ~EffectSourceSep();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() override;
   TranslatableString GetDescription() override;
   wxString ManualPage() override;

   // EffectDefinitionInterface implementation

   EffectType GetType() override;

   // EffectClientInterface implementation

   // TODO: may not need these
   // bool DefineParams(ShuttleParams &S) override;
   // bool GetAutomationParameters(CommandParameters &parms) override;
   // bool SetAutomationParameters(CommandParameters &parms) override;

   // Effect implementation
   bool ProcessOne(WaveTrack * track, double tStart, double tEnd) override;

   void PopulateOrExchange(ShuttleGui &S) override;
   void PopulateMetadata(ShuttleGui &S);
   void AddMetadataEntry(ShuttleGui &S, std::string desc, std::string key);
   // bool TransferDataToWindow() override;
   // bool TransferDataFromWindow() override;

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

   std::map<std::string, wxStaticText*> mMetadataFields;
   void UpdateMetadataFields();

   DECLARE_EVENT_TABLE()
};

#endif