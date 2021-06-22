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
   // create a deep model
   mModel = std::make_unique<DeepModel>();
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

bool EffectSourceSep::Process()
{
   // Similar to EffectSoundTouch::Process() and EffectChangeSpeed::Process()

   // Iterate over each track.
   // All needed because this effect needs to introduce
   // silence in the sync-lock group tracks to keep sync
   CopyInputTracks(true); // Set up mOutputTracks.
   bool bGoodResult = true;

   mCurrentTrackNum = 0;

   // NOTE: because we will append the separated tracks to mOutputTracks in ProcessOne(), 
   // we need to collect the track pointers before calling ProcessOne()
   std::vector< WaveTrack* > pOutWaveTracks;
   for ( WaveTrack *track : mOutputTracks->SelectedLeaders< WaveTrack >())
      pOutWaveTracks.emplace_back(track);

   // now that we have all the tracks we want to process, 
   // go ahead and separate!
   for ( WaveTrack* pOutWaveTrack : pOutWaveTracks) {

      // if the track has bound channels, bail
      bool isMono = GetNumChannels(pOutWaveTrack) == 1;
      if (!isMono)
      {
         Effect::MessageBox(XO("The Source Separation effect can only process mono tracks. Please select a mono track and try again "));
         bGoodResult = false;
         return bGoodResult;
      }

      //Get start and end times from track
      double tStart = pOutWaveTrack->GetStartTime();
      double tEnd = pOutWaveTrack->GetEndTime();

      //Set the current bounds to whichever left marker is
      //greater and whichever right marker is less:
      tStart = wxMax(mT0, tStart);
      tEnd = wxMin(mT1, tEnd);

      // Process only if the right marker is to the right of the left marker
      if (tEnd > tStart) {
         //ProcessOne() (implemented below) processes a single track
         if (!ProcessOne(pOutWaveTrack, tStart, tEnd))
            bGoodResult = false;
      }
      // increment current track
      mCurrentTrackNum++;
   }

   ReplaceProcessedTracks(bGoodResult);

   return bGoodResult;
}

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
   leader->Resample(mModel->GetSampleRate(), mProgress);

   // initialize source tracks, one for each source that we will separate
   std::vector<WaveTrack::Holder> sourceTracks;
   std::vector<std::string>sourceLabels = mModel->GetLabels();
   sourceTracks = CreateSourceTracks(leader, sourceLabels);
   
   // Initiate processing buffers, most likely shorter than
   // the length of the selection being processed.
   Floats buffer{ leader->GetMaxBlockSize() };

   // get each of the blocks we will process
   for (BlockIndex block : GetBlockIndices(leader, tStart, tEnd))
   {
      //Get a blockSize of samples (smaller than the size of the buffer)
      sampleCount samplePos = block.first;
      size_t blockSize = block.second;
   
      // get a torch tensor from the leader track
      // TODO: get rid of the Floats entirely and simply pass the data_ptr
      // to empty torch contiguous zeros
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

   return true;
}

// gets a list of starting samples and block lengths 
// dictated by the track, so we can process the audio
// audio in blocks
std::vector<BlockIndex> EffectSourceSep::GetBlockIndices
(WaveTrack *track, double tStart, double tEnd)
{
   std::vector<BlockIndex> blockIndices;

   sampleCount start = track->TimeToLongSamples(tStart);
   sampleCount end = track->TimeToLongSamples(tEnd);

   //Get the length of the selection (as double). len is
   //used simple to calculate a progress meter, so it is easier
   //to make it a double now than it is to do it later
   double len = (end - start).as_double();

   //Go through the track one buffer at a time. samplePos counts which
   //sample the current buffer starts at.
   bool bGoodResult = true;
   sampleCount samplePos = start;
   while (samplePos < end) 
   {
      //Get a blockSize of samples (smaller than the size of the buffer)
      size_t blockSize = limitSampleBufferSize(
         /*bufferSize*/ track->GetBestBlockSize(samplePos),
         /*limit*/ end - samplePos
      );

      blockIndices.emplace_back(BlockIndex(samplePos, blockSize));

      // Increment the sample pointer
      samplePos += blockSize;
   }

   return blockIndices;
}

torch::Tensor EffectSourceSep::BuildMonoTensor(WaveTrack *track, float *buffer, 
                              sampleCount start, size_t len)
{
   //Get the samples from the track and put them in the buffer
   if (!track->GetFloats(buffer, start, len))
      throw std::exception();

   // get tensor input from buffer
   torch::Tensor audio = torch::from_blob(buffer, len, 
                                          torch::TensorOptions().dtype(torch::kFloat));
   audio = audio.unsqueeze(0); // add channel dimension

   return audio;
}

torch::Tensor EffectSourceSep::ForwardPass(torch::Tensor input)
{
   torch::Tensor output;
   try
   {
      output = mModel->Forward(input);
   }
   catch (const std::exception &e)
   {
      std::cerr<<e.what();
      Effect::MessageBox(XO("An error occurred during the forward pass"),
         wxOK | wxICON_ERROR
      );

      output = torch::zeros_like(input);
   }
   return output;
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

void EffectSourceSep::TensorToTrack(torch::Tensor output, WaveTrack::Holder track, 
                                   double tStart, double tEnd)
{
   // TODO: exception: input audio should be shape (1, samples)
   if (!(output.size(0) == 1))
      throw std::exception(); 

   // get the data pointer
   float *data = output.contiguous().data_ptr<float>();
   size_t outputLen = output.size(-1);

   // add the data to a temporary track, then 
   // paste on our output track
   WaveTrack::Holder tmp = track->EmptyCopy();
   tmp->Append((samplePtr)data, floatSample, outputLen);
   tmp->Flush();

   try {
      track->ClearAndPaste(tStart, tEnd, tmp.get());
   }
   catch (const std::exception &e)
   { 
      Effect::MessageBox(XO("Error copying tensor data to output track"),
      wxOK | wxICON_ERROR 
      ); 
   }
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
                                     wxEmptyString, //TODO: add default extenstion
                                    { FileNames::AllFiles }, //TODO: change this to our deepmodel type
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