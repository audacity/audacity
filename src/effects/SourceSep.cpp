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
   // Similar to EffectSoundTouch::Process()
   // Similar to EffectChangeSpeed::Process()

   // Iterate over each track.
   // All needed because this effect needs to introduce
   // silence in the sync-lock group tracks to keep sync
   CopyInputTracks(true); // Set up mOutputTracks.
   bool bGoodResult = true;

   mCurrentTrackNum = 0;

   for ( auto pOutWaveTrack : mOutputTracks->Selected< WaveTrack >() ) {
      //Get start and end times from track
      double tStart = pOutWaveTrack->GetStartTime();
      double tEnd = pOutWaveTrack->GetEndTime();

      //Set the current bounds to whichever left marker is
      //greater and whichever right marker is less:
      tStart = wxMax(mT0, tStart);
      tEnd = wxMin(mT1, tEnd);

      // Process only if the right marker is to the right of the left marker
      if (tEnd > tStart) {
         //Transform the marker timepoints to samples
         sampleCount start = pOutWaveTrack->TimeToLongSamples(tStart);
         sampleCount end = pOutWaveTrack->TimeToLongSamples(tEnd);

         //ProcessOne() (implemented below) processes a single track
         if (!ProcessOne(pOutWaveTrack, start, end))
            bGoodResult = false;
      }
      // increment current track
      mCurrentTrackNum++;
   }

   ReplaceProcessedTracks(bGoodResult);

   return bGoodResult;
}

// ProcessOne() takes a track, transforms it to bunch of buffer-blocks,
// and calls libsamplerate code on these blocks.
bool EffectSourceSep::ProcessOne(WaveTrack *track,
                           sampleCount start, sampleCount end)
{
   if (track == NULL)
      return false;

   // preprocess the track per the deep model
   // TODO: need to do this to copy of the track, and make sure
   // all the output audio is converted to match the output track 
   mModel->PreprocessTrack(track);

   // initialization, per examples of Mixer::Mixer and
   // EffectSoundTouch::ProcessOne
   // create a new track for each source that we will separate
   std::vector<WaveTrack::Holder> outputSourceTracks;
   for (auto &label : mModel->GetLabels())
      outputSourceTracks.emplace_back(track->EmptyCopy());

   //Get the length of the selection (as double). len is
   //used simple to calculate a progress meter, so it is easier
   //to make it a double now than it is to do it later
   double len = (end - start).as_double();

   // Initiate processing buffers, most likely shorter than
   // the length of the selection being processed.
   size_t inBufferSize = track->GetMaxBlockSize();
   Floats inBuffer{ inBufferSize };

   //Go through the track one buffer at a time. samplePos counts which
   //sample the current buffer starts at.
   bool bGoodResult = true;
   sampleCount samplePos = start;
   while (samplePos < end) {
      //Get a blockSize of samples (smaller than the size of the buffer)
      size_t blockSize = limitSampleBufferSize(
         /*bufferSize*/ track->GetBestBlockSize(samplePos),
         /*limit*/ end - samplePos
      );

      //Get the samples from the track and put them in the buffer
      track->GetFloats(inBuffer.get(), samplePos, blockSize);

      // get tensor input from buffer
      torch::Tensor input = torch::from_blob(inBuffer.get(), 
                                             blockSize, 
                                             torch::TensorOptions().dtype(torch::kFloat32));
      input = input.unsqueeze(0); // add channel dimension

      // forward pass!
      torch::Tensor output;
      try
      {
         output = mModel->Forward(input);
      }
      catch (const std::exception &e)
      {
         Effect::MessageBox(XO("An error occurred during the forward pass"),
            wxOK | wxICON_ERROR
         );
         bGoodResult = false;
         output = input;
         return bGoodResult;
      }

      // determine size of output buffers and create a  new
      // buffer for each output sources
      size_t outputLen = output.size(-1);
      auto dbg = output.sizes();
      // std::vector<Floats> outputBuffers;

      // copy data from output tensor to the output tracks
      for (size_t idx = 0 ; idx < output.size(0) ; idx++)
      {
         // create a new buffer and fill it with the output
         // NOTE: may need to copy samples or transfer ownership
         // index into the first channel (n_src, channels, samples)
         float *data = output[idx][0].contiguous().data_ptr<float>();
         try 
         { 
            outputSourceTracks.at(idx)->Set((samplePtr)inBuffer.get(), floatSample, samplePos, blockSize); 
            // outputSourceTracks.at(idx)->Set((samplePtr)data, floatSample, samplePos, outputLen); 
            outputSourceTracks.at(idx)->Flush(); 
         }
         catch (const std::exception &e)
         { 
            Effect::MessageBox(XO("Error copying tensor data to output track"),
            wxOK | wxICON_ERROR 
            );
            return false; 
         }
      }

      // Increment the sample pointer
      samplePos += outputLen;

      // Update the Progress meter
      if (TrackProgress(mCurrentTrackNum, (samplePos - start).as_double() / len)) {
         bGoodResult = false;
         break;
      }
   }

   // flush all output track buffers
   if (bGoodResult)
   {
      for (std::shared_ptr<WaveTrack> track : outputSourceTracks)
      {
         track->Flush();
         AddToOutputTracks(track);
      }
   }

   return bGoodResult;
}

bool EffectSourceSep::Separate()
{
   return true;
}

void EffectSourceSep::PopulateOrExchange(ShuttleGui &S)
{
   S.StartVerticalLay(wxCENTER, true);
   {

      // if (mModel->IsLoaded())
      PopulateMetadata(S);

      S.StartHorizontalLay(wxCENTER, false);
      {
         mLoadModelBtn = S.AddButton(XXO("&Load Source Separation Model"));
         mDescription = S.AddVariableText(XO("pls load a model!"));
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
            mMetadataFields.insert(std::make_pair(key, text)); \
         } \
         S.EndHorizontalLay(); \

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
   mModel->Load(path.ToStdString());

   wxString descStr("loaded model succesfully!");
   mDescription->SetLabel(descStr);
   mDescription->SetName(descStr);

   UpdateMetadataFields();
}