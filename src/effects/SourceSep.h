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

   // gets the number of channels in a (possibly multichannel) track
   size_t GetNumChannels(WaveTrack *leader){return TrackList::Channels(leader).size();}

   // builds a mono tensor with shape (1, samples)
   // from a track
   torch::Tensor BuildMonoTensor(WaveTrack *track, float *buffer, 
                                 sampleCount start, size_t len);

   // wraps the forward pass in an exception
   torch::Tensor ForwardPass(torch::Tensor input); 

   // writes an output tensor to a track
   // tensor should be shape (1, samples)
   void TensorToTrack(torch::Tensor output, WaveTrack::Holder track, 
                      double tStart, double tEnd);

   std::vector<BlockIndex> GetBlockIndices(WaveTrack *track, 
                                           double tStart, double tEnd);

private:
   // handlers
   void OnLoadButton(wxCommandEvent &event);

   std::vector<WaveTrack::Holder> CreateSourceTracks(WaveTrack *track, 
                                             std::vector<std::string> &labels);
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