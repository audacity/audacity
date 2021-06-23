/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2021 Audacity Team.
   License: GPL v2.  See License.txt.

   SourceSep.cpp
   Hugo Flores Garcia

******************************************************************/
/**

\class SourceSep
\brief SourceSep is an effect for source separation using deep learning 

TODO: add a more thorough description

*/
/*******************************************************************/

#include "SourceSep.h"

#include <wx/stattext.h>

#include "../FileNames.h"
#include "../Shuttle.h"
#include "../ShuttleGui.h"
#include "../WaveTrack.h"

#include "LoadEffects.h"

#include <torch/script.h>

const ComponentInterfaceSymbol EffectSourceSep::Symbol
{ XO("Source Separation") };

// register source separation
namespace{ BuiltinEffectsModule::Registration< EffectSourceSep > reg; }

// register event handlers
BEGIN_EVENT_TABLE(EffectSourceSep, wxEvtHandler)
   EVT_BUTTON(wxID_ANY, EffectSourceSep::OnLoadButton)
END_EVENT_TABLE()

EffectSourceSep::EffectSourceSep()
{
   SetLinearEffectFlag(false);
}

EffectSourceSep::~EffectSourceSep()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol EffectSourceSep::GetSymbol()
{
   return Symbol;
}

TranslatableString EffectSourceSep::GetDescription()
{
   return XO("Source Separation!"); // TODO
}

wxString EffectSourceSep::ManualPage()
{
   return wxT("Source Separation"); // TODO
}

// EffectDefinitionInterface implementation

EffectType EffectSourceSep::GetType()
{
   return EffectTypeProcess;
}

// EffectClientInterface implementation

// Effect implementation

// ProcessOne() takes a track, transforms it to bunch of buffer-blocks,
// performs a forward pass through the deep model, and writes 
// the output to new tracks. 
bool EffectSourceSep::ProcessOne(WaveTrack *leader,
                           double tStart, double tEnd)
{
   if (leader == NULL)
      return false;

   // keep track of the sample format and rate,
   // we want to convert all output tracks to this
   sampleFormat origFmt = leader->GetSampleFormat();
   int origRate = leader->GetRate();

   // resample the track, and make sure that we update the start and end 
   // sampleCounts to reflect the new sample rate
   // TODO: we should prevent from  having to resample the whole track
   mProgress->SetMessage(XO("Resampling Track"));
   leader->Resample(mModel->GetSampleRate(), mProgress);

   // initialize source tracks, one for each source that we will separate
   std::vector<WaveTrack::Holder> sourceTracks;
   std::vector<std::string>sourceLabels = mModel->GetLabels();
   sourceTracks = CreateSourceTracks(leader, sourceLabels);
   
   // Initiate processing buffer, most likely shorter than
   // the length of the selection being processed.
   Floats buffer{ leader->GetMaxBlockSize() };

   // get each of the blocks we will process
   for (BlockIndex block : GetBlockIndices(leader, tStart, tEnd))
   {
      //Get a blockSize of samples (smaller than the size of the buffer)
      sampleCount samplePos = block.first;
      size_t blockSize = block.second;
   
      // get a torch tensor from the leader track
      torch::Tensor input = BuildMonoTensor(leader, buffer.get(), 
                                            samplePos, blockSize); 

      // forward pass!
      torch::Tensor output = ForwardPass(input);

      // write each source output to the source tracks
      for (size_t idx = 0; idx < output.size(0) ; idx++)
         TensorToTrack(output[idx].unsqueeze(0), sourceTracks[idx], 
                       tStart, tEnd); 

      // Update the Progress meter
      double tPos = leader->LongSamplesToTime(samplePos); 
      if (TrackProgress(mCurrentTrackNum, (tPos - tStart) / (tEnd - tStart))) 
         return false;
   }

   // postprocess the source tracks to the user's sample rate and format
   PostProcessSources(sourceTracks, origFmt, origRate);

   // resample the leader back
   leader->Resample(origRate);

   return true;
}

std::vector<WaveTrack::Holder> EffectSourceSep::CreateSourceTracks
(WaveTrack *leader, std::vector<std::string> &labels)
{
   std::vector<WaveTrack::Holder> sources;
   for (auto &label : labels)
   {
      WaveTrack::Holder srcTrack = leader->EmptyCopy();

      // append the source name to the track's name
      srcTrack->SetName(srcTrack->GetName() + wxString("-" + label));
      sources.emplace_back(srcTrack);
   }
   return sources;
}

void EffectSourceSep::PostProcessSources
(std::vector<WaveTrack::Holder> &sourceTracks, sampleFormat fmt, int sampleRate)
{
   // flush all output track buffers
   // convert to the original rate and format
   for (std::shared_ptr<WaveTrack> track : sourceTracks)
   {
      track->Flush();
      track->ConvertToSampleFormat(fmt);
      track->Resample(sampleRate);
      AddToOutputTracks(track);
   }
}

// UI stuff
void EffectSourceSep::PopulateOrExchange(ShuttleGui &S)
{
   S.StartVerticalLay(wxCENTER, true);
   {

      PopulateMetadata(S);

      S.StartHorizontalLay(wxCENTER, false);
      {
         mLoadModelBtn = S.AddButton(XXO("&Load Source Separation Model"));

         std::string modelDesc;
         if (mModel->IsLoaded()) 
            modelDesc = "loaded model successfully";
         else 
            modelDesc = "pls load a model!";

         mDescription = S.AddVariableText(
            TranslatableString(wxString(modelDesc).c_str(), {}));
      }
      S.EndHorizontalLay();
   }
   S.EndVerticalLay();

}

void EffectSourceSep::PopulateMetadata(ShuttleGui &S)
{
   // TODO: this does not take into account the possible
   // depth of the metadata.
   rapidjson::Document document = mModel->GetMetadata();
   S.StartVerticalLay(wxCENTER, false);
   {
      //TODO: bold not working, nicer table style
#define ADD_METADATA_ENTRY(desc, key) \
         S.StartHorizontalLay(wxLEFT, false); \
         { \
            S.AddVariableText(XXO(desc)); \
            std::string value = mModel->QueryMetadata(key); \
            wxStaticText *text =  S.AddVariableText( \
               TranslatableString(wxString(value.c_str()), {}) \
               ); \
            wxFont font = text->GetFont(); \
            font.SetWeight(wxFONTWEIGHT_BOLD); \
            font.MakeBold(); \
            text->SetFont(font); \
            mMetadataFields[key] = text; \
         } \
         S.EndHorizontalLay(); \

      ADD_METADATA_ENTRY("&Model Name:", "name")
      ADD_METADATA_ENTRY("&Separation Sample Rate:", "sample_rate")
      ADD_METADATA_ENTRY("&Domain:", "domain")
      ADD_METADATA_ENTRY("&Number of Sources:", "n_src")
      ADD_METADATA_ENTRY("&Output Sources:", "labels")
   }
   S.EndVerticalLay();
}

void EffectSourceSep::UpdateMetadataFields(){
   for (auto pair : mMetadataFields){
      std::string key = pair.first;
      wxStaticText *field = pair.second;

      std::string value = mModel->QueryMetadata(key.c_str());
      field->SetLabel(wxString(value));
      field->SetName(wxString(value));
   }
}

void EffectSourceSep::OnLoadButton(wxCommandEvent &WXUNUSED(event))
{
   auto path = FileNames::SelectFile(FileNames::Operation::Import,
                                     XO("Load Source Separation Model"),
                                     wxEmptyString,
                                     wxEmptyString, 
                                     wxT("ts"),
                                    { FileNames::FileType(
                                       XO("TorchScript Files"), 
                                       {wxT("ts")}, true
                                    )},
                                     wxFD_OPEN | wxRESIZE_BORDER,
                                     nullptr);

   if (path.empty()) return;

   // attempt load deep learning model
   // TODO: what's the fallback when the model doesn't load? 
   wxString descStr;
   if (!mModel->Load(path.ToStdString()))
      descStr = wxString("error loading model :(");
   else
      descStr = wxString("loaded model succesfully!");

   mDescription->SetLabel(descStr);
   mDescription->SetName(descStr);

   UpdateMetadataFields();
}