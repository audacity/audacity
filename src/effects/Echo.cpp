/**********************************************************************

  Audacity: A Digital Audio Editor

  Echo.cpp

  Dominic Mazzoni
  Vaughan Johnson (dialog)

*******************************************************************//**

\class EffectEcho
\brief An Effect that causes an echo, variable delay and volume.

*//****************************************************************//**

\class EchoDialog
\brief EchoDialog used with EffectEcho

*//*******************************************************************/


#include "Echo.h"
#include "LoadEffects.h"

#include <wx/intl.h>

#include "../ShuttleGui.h"
#include "../widgets/AudacityMessageBox.h"
#include "../widgets/valnum.h"

const EffectParameterMethods& EffectEcho::Parameters() const
{
   static CapturedParameters<EffectEcho,
      Delay, Decay
   > parameters;
   return parameters;
}

const ComponentInterfaceSymbol EffectEcho::Symbol
{ XO("Echo") };

namespace{ BuiltinEffectsModule::Registration< EffectEcho > reg; }

EffectEcho::EffectEcho()
{
   SetLinearEffectFlag(true);
}

EffectEcho::~EffectEcho()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol EffectEcho::GetSymbol() const
{
   return Symbol;
}

TranslatableString EffectEcho::GetDescription() const
{
   return XO("Repeats the selected audio again and again");
}

ManualPageID EffectEcho::ManualPage() const
{
   return L"Echo";
}

// EffectDefinitionInterface implementation

EffectType EffectEcho::GetType() const
{
   return EffectTypeProcess;
}

unsigned EffectEcho::GetAudioInCount() const
{
   return 1;
}

unsigned EffectEcho::GetAudioOutCount() const
{
   return 1;
}

bool EffectEcho::ProcessInitialize(
   EffectSettings &, sampleCount, ChannelNames)
{
   auto& echoSettings = mSettings;

   if (echoSettings.delay == 0.0)
   {
      return false;
   }

   histPos = 0;
   auto requestedHistLen = (sampleCount) (mSampleRate * echoSettings.delay);

   // Guard against extreme delay values input by the user
   try {
      // Guard against huge delay values from the user.
      // Don't violate the assertion in as_size_t
      if (requestedHistLen !=
            (histLen = static_cast<size_t>(requestedHistLen.as_long_long())))
         throw std::bad_alloc{};
      history.reinit(histLen, true);
   }
   catch ( const std::bad_alloc& ) {
      Effect::MessageBox( XO("Requested value exceeds memory capacity.") );
      return false;
   }

   return history != NULL;
}

bool EffectEcho::ProcessFinalize()
{
   history.reset();
   return true;
}

size_t EffectEcho::ProcessBlock(EffectSettings &,
   const float *const *inBlock, float *const *outBlock, size_t blockLen)
{
   auto& echoSettings = mSettings;

   const float *ibuf = inBlock[0];
   float *obuf = outBlock[0];

   for (decltype(blockLen) i = 0; i < blockLen; i++, histPos++)
   {
      if (histPos == histLen)
      {
         histPos = 0;
      }
      history[histPos] = obuf[i] = ibuf[i] + history[histPos] * echoSettings.decay;
   }

   return blockLen;
}



struct EffectEcho::Validator
   : DefaultEffectUIValidator
{
   Validator(EffectUIClientInterface& effect,
      EffectSettingsAccess& access, EffectEchoSettings& settings)
      : DefaultEffectUIValidator{ effect, access }
      , mSettings{ settings }
   {}
   virtual ~Validator() = default;

   Effect& GetEffect() const { return static_cast<Effect&>(mEffect); }

   bool ValidateUI() override;
   bool UpdateUI() override;

   void PopulateOrExchange(ShuttleGui& S);

   EffectEchoSettings& mSettings;
};




std::unique_ptr<EffectUIValidator>
EffectEcho::PopulateOrExchange(ShuttleGui & S, EffectSettingsAccess& access)
{
   auto& echoSettings = mSettings;

   S.AddSpace(0, 5);

   S.StartMultiColumn(2, wxALIGN_CENTER);
   {
      S.Validator<FloatingPointValidator<double>>(
            3, &echoSettings.delay, NumValidatorStyle::NO_TRAILING_ZEROES,
            Delay.min, Delay.max )
         .AddTextBox(XXO("&Delay time (seconds):"), L"", 10);

      S.Validator<FloatingPointValidator<double>>(
            3, &echoSettings.decay, NumValidatorStyle::NO_TRAILING_ZEROES,
            Decay.min, Decay.max)
         .AddTextBox(XXO("D&ecay factor:"), L"", 10);
   }
   S.EndMultiColumn();
   return nullptr;
}


bool EffectEcho::Validator::ValidateUI()
{
   mAccess.ModifySettings
   (
      [this](EffectSettings& settings)
   {
      // pass back the modified settings to the MessageBuffer

      // TODO uncomment at last step
      //EffectEcho::GetSettings(settings) = mSettings;
   }
   );

   return true;
}


bool EffectEcho::Validator::UpdateUI()
{
   // get the settings from the MessageBuffer and write them to our local copy
   const auto& settings = mAccess.Get();

   // TODO uncomment at last step
   //mSettings = GetSettings(settings);

   return true;
}

