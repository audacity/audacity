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
namespace{ BuiltinEffectsModule::Registration<EffectSourceSep> reg; }

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
   return XO("Source Separation"); // TODO
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
   S.StartHorizontalLay(wxCENTER, false);
   {
      mLoadModelBtn = S.AddButton(XXO("&Load Source Separation Model"));
      mDescription = S.AddVariableText(XO("pls load a model!"));
   }
   S.EndHorizontalLay();
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
   mModel->Load(path);

   wxString descStr("loaded a deep model succesfully!");
   mDescription->SetLabel(descStr);
   mDescription->SetName(descStr);
}