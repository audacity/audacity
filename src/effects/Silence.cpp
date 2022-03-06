/**********************************************************************

  Audacity: A Digital Audio Editor

  Silence.cpp

  Dominic Mazzoni

*******************************************************************//**

\class EffectSilence
\brief An effect to add silence.

*//*******************************************************************/


#include "Silence.h"
#include "LoadEffects.h"

#include <wx/intl.h>

#include "../ShuttleGui.h"
#include "../WaveTrack.h"
#include "../widgets/NumericTextCtrl.h"

const ComponentInterfaceSymbol EffectSilence::Symbol
/* i18n-hint: noun */
{ XC("Silence", "generator") };

namespace{ BuiltinEffectsModule::Registration< EffectSilence > reg; }

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
   return Symbol;
}

TranslatableString EffectSilence::GetDescription()
{
   return XO("Creates audio of zero amplitude");
}

ManualPageID EffectSilence::ManualPage()
{
   return L"Silence";
}


// EffectDefinitionInterface implementation

EffectType EffectSilence::GetType()
{
   return EffectTypeGenerate;
}

// Effect implementation

std::unique_ptr<EffectUIValidator>
EffectSilence::PopulateOrExchange(ShuttleGui & S, EffectSettingsAccess &)
{
   S.StartVerticalLay();
   {
      S.StartHorizontalLay();
      {
         S.AddPrompt(XXO("&Duration:"));
         mDurationT = safenew
            NumericTextCtrl(S.GetParent(), wxID_ANY,
                              NumericConverter::TIME,
                              GetDurationFormat(),
                              GetDuration(),
                               mProjectRate,
                               NumericTextCtrl::Options{}
                                  .AutoPos(true));
         S.Name(XO("Duration"))
            .Position(wxALIGN_CENTER | wxALL)
            .AddWindow(mDurationT);
      }
      S.EndHorizontalLay();
   }
   S.EndVerticalLay();

   return nullptr;
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
