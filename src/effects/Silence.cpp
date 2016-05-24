/**********************************************************************

  Audacity: A Digital Audio Editor

  Silence.cpp

  Dominic Mazzoni

*******************************************************************//**

\class EffectSilence
\brief An effect to add silence.

*//*******************************************************************/

#include "../Audacity.h"
#include "Silence.h"

#include <wx/intl.h>

#include "../ShuttleGui.h"
#include "../WaveTrack.h"

EffectSilence::EffectSilence()
{
   SetLinearEffectFlag(true);
}

EffectSilence::~EffectSilence()
{
}

// IdentInterface implementation

wxString EffectSilence::GetSymbol()
{
   return SILENCE_PLUGIN_SYMBOL;
}

wxString EffectSilence::GetDescription()
{
   return XO("Creates audio of zero amplitude");
}

// EffectIdentInterface implementation

EffectType EffectSilence::GetType()
{
   return EffectTypeGenerate;
}

// Effect implementation

void EffectSilence::PopulateOrExchange(ShuttleGui & S)
{
   S.StartVerticalLay();
   {
      S.StartHorizontalLay();
      {
         S.AddPrompt(_("Duration:"));
         mDurationT = safenew
            NumericTextCtrl(NumericConverter::TIME,
                              S.GetParent(),
                              wxID_ANY,
                              GetDurationFormat(),
                              GetDuration(),
                              mProjectRate,
                              wxDefaultPosition,
                              wxDefaultSize,
                              true);
         mDurationT->SetName(_("Duration"));
         mDurationT->EnableMenu();
         S.AddWindow(mDurationT, wxALIGN_CENTER | wxALL);
      }
      S.EndHorizontalLay();
   }
   S.EndVerticalLay();

   return;
}

bool EffectSilence::TransferDataToWindow()
{
   mDurationT->SetValue(GetDuration());

   return true;
}

bool EffectSilence::TransferDataFromWindow()
{
   SetDuration(mDurationT->GetValue());

   return true;
}

bool EffectSilence::GenerateTrack(WaveTrack *tmp,
                                  const WaveTrack & WXUNUSED(track),
                                  int WXUNUSED(ntrack))
{
   bool bResult = tmp->InsertSilence(0.0, GetDuration());
   wxASSERT(bResult);
   return bResult;
}
