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

// ComponentInterface implementation

ComponentInterfaceSymbol EffectSilence::GetSymbol()
{
   return SILENCE_PLUGIN_SYMBOL;
}

wxString EffectSilence::GetDescription()
{
   return _("Creates audio of zero amplitude");
}

wxString EffectSilence::ManualPage()
{
   return wxT("Silence");
}


// EffectDefinitionInterface implementation

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
            NumericTextCtrl(S.GetParent(), wxID_ANY,
                              NumericConverter::TIME,
                              GetDurationFormat(),
                              GetDuration(),
                               mProjectRate,
                               NumericTextCtrl::Options{}
                                  .AutoPos(true));
         mDurationT->SetName(_("Duration"));
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
   tmp->InsertSilence(0.0, GetDuration());
   return true;
}
