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

#ifndef __AUDACITY_EFFECT_SOURCESEP__
#define __AUDACITY_EFFECT_SOURCESEP__

#include "deeplearning/DeepModel.h"
#include "Effect.h"

class wxStaticText;
class ShuttleGui;
class wxButton;

// struct BlockIndex {
//    sampleCount start;
//    size_t len;
// };

// BlockIndex.first corresponds to the starting sample of a block
// BlockIndex.second corresponds to the length of the block
using BlockIndex = std::pair<sampleCount, size_t>;

class EffectSourceSep final: public Effect
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
   bool Process() override;
   bool ProcessOne(WaveTrack * track, double tStart, double tEnd);

   void PopulateOrExchange(ShuttleGui &S) override;
   void PopulateMetadata(ShuttleGui &S);
   // bool TransferDataToWindow() override;
   // bool TransferDataFromWindow() override;

protected:
   // track --> tensor conversions
   void Preprocess(WaveTrack *leader);
   size_t GetNumChannels(WaveTrack *leader){return TrackList::Channels(leader).size();}

   // builds a mono tensor with shape (1, samples)
   torch::Tensor BuildMonoTensor(WaveTrack *track, float *buffer, 
                                 sampleCount start, size_t len);
   // wraps the forward pass in an effect exception
   torch::Tensor ForwardPass(torch::Tensor input); 

   std::vector<BlockIndex> GetBlockIndices(WaveTrack *track, 
                                           double tStart, double tEnd);

private:
   // handlers
   void OnLoadButton(wxCommandEvent &event);

   std::vector<WaveTrack::Holder> BuildOutputSourceTracks(WaveTrack *track, 
                                             std::vector<std::string> &labels);

   // given output tensor with shape (channels, samples), will 
   void WriteOutputToSourceTracks(torch::Tensor output, 
                                  std::vector<WaveTrack::Holder> &sourceTracks, 
                                  double tStart, double tEnd);
   void PostProcessSources(std::vector<WaveTrack::Holder> &sourceTracks, 
                           sampleFormat fmt, int sampleRate);

private:
   std::unique_ptr<DeepModel> mModel;

   int mCurrentTrackNum;

   wxButton *mLoadModelBtn;
   wxStaticText *mDescription;

   std::map<std::string, wxStaticText*> mMetadataFields;
   void UpdateMetadataFields();

   DECLARE_EVENT_TABLE()
};

#endif