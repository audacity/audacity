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
   // Set up mOutputTracks.
   CopyInputTracks(false);

   int nTrack = 0;
   bool bGoodResult = true;
   double maxDestLen = 0.0; // used to change selection to generated bit

   // TODO: copy pasted this from repeat, fill out with source sep. 
   for (auto track : mOutputTracks->Selected<WaveTrack>())
   {
      double trackStart = track->GetStartTime();
      double trackEnd = track->GetEndTime();
      double t0 = mT0 < trackStart ? trackStart : mT0;
      double t1 = mT1 > trackEnd ? trackEnd : mT1;

      auto start = track->TimeToLongSamples(t0);
      auto end = track->TimeToLongSamples(t1);
      auto len = end - start;
      double tLen = track->LongSamplesToTime(len);
      double tc = mT0 + tLen;

      int repeatCount = 2;
      auto dest = track->Copy(mT0, mT1);
      for (int j = 0; j < repeatCount; j++)
      {
         if (TrackProgress(nTrack, j / repeatCount)) // TrackProgress returns true on Cancel.
         {
            bGoodResult = false;
            return bGoodResult;
         }
         track->Paste(tc, dest.get());
         tc += tLen;
      }
      if (tc > maxDestLen)
         maxDestLen = tc;
      nTrack++;
   }

   if (bGoodResult)
   {
      // Select the NEW bits + original bit
      mT1 = maxDestLen;
   }

   ReplaceProcessedTracks(bGoodResult);
   return bGoodResult;
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