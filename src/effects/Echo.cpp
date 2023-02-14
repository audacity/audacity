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

#include "ShuttleGui.h"
#include "../widgets/AudacityMessageBox.h"
#include "valnum.h"

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


struct EffectEcho::Instance
   : public PerTrackEffect::Instance
   , public EffectInstanceWithBlockSize
{
   Instance(const PerTrackEffect& effect)
      : PerTrackEffect::Instance{ effect }
   {}

   bool ProcessInitialize(EffectSettings& settings, double sampleRate,
      ChannelNames chanMap) override;

   size_t ProcessBlock(EffectSettings& settings,
      const float* const* inBlock, float* const* outBlock, size_t blockLen)  override;

   bool ProcessFinalize() noexcept override;

   unsigned GetAudioOutCount() const override
   {
      return 1;
   }

   unsigned GetAudioInCount() const override
   {
      return 1;
   }

   Floats history;
   size_t histPos;
   size_t histLen;
};



std::shared_ptr<EffectInstance> EffectEcho::MakeInstance() const
{
   return std::make_shared<Instance>(*this);
}




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

bool EffectEcho::Instance::ProcessInitialize(
   EffectSettings& settings, double sampleRate, ChannelNames)
{
   auto& echoSettings = GetSettings(settings);  
   if (echoSettings.delay == 0.0)
      return false;

   histPos = 0;
   auto requestedHistLen = (sampleCount) (sampleRate * echoSettings.delay);

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
      mProcessor.MessageBox( XO("Requested value exceeds memory capacity.") );
      return false;
   }

   return history != NULL;
}

bool EffectEcho::Instance::ProcessFinalize() noexcept
{
   return true;
}

size_t EffectEcho::Instance::ProcessBlock(EffectSettings& settings,
   const float *const *inBlock, float *const *outBlock, size_t blockLen)
{
   auto& echoSettings = GetSettings(settings);
   
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
   : EffectUIValidator
{
   Validator(EffectUIClientInterface& effect,
      EffectSettingsAccess& access, const EffectEchoSettings& settings)
      : EffectUIValidator{ effect, access }
      , mSettings{ settings }
   {}
   virtual ~Validator() = default;

   Effect& GetEffect() const { return static_cast<Effect&>(mEffect); }

   bool ValidateUI() override;
   bool UpdateUI() override;

   void PopulateOrExchange(ShuttleGui& S);

   EffectEchoSettings mSettings;
};



std::unique_ptr<EffectUIValidator> EffectEcho::PopulateOrExchange(
   ShuttleGui & S, EffectInstance &, EffectSettingsAccess &access,
   const EffectOutputs *)
{
   auto& settings = access.Get();
   auto& myEffSettings = GetSettings(settings);
   auto result = std::make_unique<Validator>(*this, access, myEffSettings);
   result->PopulateOrExchange(S);
   return result;
}


void EffectEcho::Validator::PopulateOrExchange(ShuttleGui & S)
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
}


bool EffectEcho::Validator::ValidateUI()
{
   mAccess.ModifySettings
   (
      [this](EffectSettings& settings)
   {
      // pass back the modified settings to the MessageBuffer

      EffectEcho::GetSettings(settings) = mSettings;
      return nullptr;
   }
   );

   return true;
}


bool EffectEcho::Validator::UpdateUI()
{
   // get the settings from the MessageBuffer and write them to our local copy
   const auto& settings = mAccess.Get();

   mSettings = GetSettings(settings);

   return true;
}

