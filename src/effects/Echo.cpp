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


struct EffectEcho::Instance
   : public PerTrackEffect::Instance
   , public EffectInstanceWithBlockSize
{
   Instance(const PerTrackEffect& effect)
      : PerTrackEffect::Instance{ effect }
   {}

   void SetSampleRate(double rate) override { mSampleRate = rate; }

   bool ProcessInitialize(EffectSettings& settings,
      sampleCount totalLen, ChannelNames chanMap) override;

   bool ProcessFinalize() override;

   size_t ProcessBlock(EffectSettings& settings,
      const float* const* inBlock, float* const* outBlock, size_t blockLen)  override;

   double mSampleRate{};

   // state
   Floats history;
   size_t histPos;
   size_t histLen;
};


std::shared_ptr<EffectInstance>
EffectEcho::MakeInstance(EffectSettings& settings) const
{
   return std::make_shared<Instance>(*this);
}




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
   //Parameters().Reset(*this);
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


bool EffectEcho::Instance::ProcessInitialize(
   EffectSettings& settings, sampleCount, ChannelNames)
{

   auto& echoSettings = GetSettings(settings);

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

      mProcessor.MessageBox( XO("Requested value exceeds memory capacity.") );

      return false;
   }

   return history != NULL;
}


bool EffectEcho::Instance::ProcessFinalize()
{
   history.reset();
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
   : DefaultEffectUIValidator
{
   Validator(EffectUIClientInterface& effect,
      EffectSettingsAccess& access, const EffectEchoSettings& settings)
      : DefaultEffectUIValidator{ effect, access }
      // Copy settings
      , mSettings{ settings }
   {}
   virtual ~Validator() = default;

   Effect& GetEffect() const { return static_cast<Effect&>(mEffect); }

   bool ValidateUI() override;
   bool UpdateUI() override;
   
   void PopulateOrExchange(ShuttleGui& S,
      const EffectSettings& settings, double projectRate);

   EffectEchoSettings mSettings;
};


void EffectEcho::Validator::PopulateOrExchange(ShuttleGui& S, const EffectSettings& settings, double aRate)
{
   S.AddSpace(0, 5);

   S.StartMultiColumn(2, wxALIGN_CENTER);
   {
      S.Validator<FloatingPointValidator<double>>(
            3, &(mSettings.delay), NumValidatorStyle::NO_TRAILING_ZEROES,
            Delay.min, Delay.max )
         .AddTextBox(XXO("&Delay time (seconds):"), L"", 10);

      S.Validator<FloatingPointValidator<double>>(
            3, &(mSettings.decay), NumValidatorStyle::NO_TRAILING_ZEROES,
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


std::unique_ptr<EffectUIValidator>
EffectEcho::PopulateOrExchange(ShuttleGui& S, EffectSettingsAccess& access)
{
   auto& settings = access.Get();
   auto& myEffSettings = GetSettings(settings);
   auto result = std::make_unique<Validator>(*this, access, myEffSettings);
   result->PopulateOrExchange(S, settings, mProjectRate);
   return result;   
}

