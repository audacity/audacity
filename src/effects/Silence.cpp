/**********************************************************************

  Audacity: A Digital Audio Editor

  Silence.cpp

  Dominic Mazzoni

*******************************************************************//**

\class EffectSilence
\brief An effect to add silence.

*//*******************************************************************/
#include "Silence.h"
#include "EffectEditor.h"
#include "LoadEffects.h"

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

ComponentInterfaceSymbol EffectSilence::GetSymbol() const
{
   return Symbol;
}

TranslatableString EffectSilence::GetDescription() const
{
   return XO("Creates audio of zero amplitude");
}

ManualPageID EffectSilence::ManualPage() const
{
   return L"Silence";
}


// EffectDefinitionInterface implementation

EffectType EffectSilence::GetType() const
{
   return EffectTypeGenerate;
}

// Effect implementation

std::unique_ptr<EffectEditor> EffectSilence::PopulateOrExchange(
   ShuttleGui & S, EffectInstance &, EffectSettingsAccess &access,
   const EffectOutputs *)
{
   S.StartVerticalLay();
   {
      S.StartHorizontalLay();
      {
         S.AddPrompt(XXO("&Duration:"));
         auto &extra = access.Get().extra;
         mDurationT = safenew
            NumericTextCtrl(S.GetParent(), wxID_ANY,
                              NumericConverter::TIME,
                              extra.GetDurationFormat(),
                              extra.GetDuration(),
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

bool EffectSilence::TransferDataToWindow(const EffectSettings &settings)
{
   mDurationT->SetValue(settings.extra.GetDuration());

   return true;
}

bool EffectSilence::TransferDataFromWindow(EffectSettings &settings)
{
   settings.extra.SetDuration(mDurationT->GetValue());

   return true;
}

bool EffectSilence::GenerateTrack(
   EffectContext &, EffectSettings &settings,
   WaveTrack *tmp, const WaveTrack &, int)
{
   tmp->InsertSilence(0.0, settings.extra.GetDuration());
   return true;
}
