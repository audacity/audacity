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

#include "../Audacity.h"
#include "Echo.h"

#include <float.h>

#include <wx/intl.h>

#include "../ShuttleGui.h"
#include "../Shuttle.h"
#include "../widgets/ErrorDialog.h"
#include "../widgets/valnum.h"
#include "../SampleFormat.h"

// Define keys, defaults, minimums, and maximums for the effect parameters
//
//     Name    Type     Key            Def   Min      Max      Scale
Param( Delay,  float,   wxT("Delay"),   1.0f, 0.001f,  FLT_MAX, 1.0f );
Param( Decay,  float,   wxT("Decay"),   0.5f, 0.0f,    FLT_MAX, 1.0f );

EffectEcho::EffectEcho()
{
   delay = DEF_Delay;
   decay = DEF_Decay;

   SetLinearEffectFlag(true);
}

EffectEcho::~EffectEcho()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol EffectEcho::GetSymbol()
{
   return ECHO_PLUGIN_SYMBOL;
}

wxString EffectEcho::GetDescription()
{
   return _("Repeats the selected audio again and again");
}

wxString EffectEcho::ManualPage()
{
   return wxT("Echo");
}

// EffectDefinitionInterface implementation

EffectType EffectEcho::GetType()
{
   return EffectTypeProcess;
}

// EffectClientInterface implementation

unsigned EffectEcho::GetAudioInCount()
{
   return 1;
}

unsigned EffectEcho::GetAudioOutCount()
{
   return 1;
}

bool EffectEcho::ProcessInitialize(sampleCount WXUNUSED(totalLen), ChannelNames WXUNUSED(chanMap))
{
   if (delay == 0.0)
   {
      return false;
   }

   histPos = 0;
   auto requestedHistLen = (sampleCount) (mSampleRate * delay);

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
      Effect::MessageBox(_("Requested value exceeds memory capacity."));
      return false;
   }

   return history != NULL;
}

bool EffectEcho::ProcessFinalize()
{
   history.reset();
   return true;
}

size_t EffectEcho::ProcessBlock(float **inBlock, float **outBlock, size_t blockLen)
{
   float *ibuf = inBlock[0];
   float *obuf = outBlock[0];

   for (decltype(blockLen) i = 0; i < blockLen; i++, histPos++)
   {
      if (histPos == histLen)
      {
         histPos = 0;
      }
      history[histPos] = obuf[i] = ibuf[i] + history[histPos] * decay;
   }

   return blockLen;
}

bool EffectEcho::DefineParams( ShuttleParams & S ){
   S.SHUTTLE_PARAM( delay, Delay );
   S.SHUTTLE_PARAM( decay, Decay );
   return true;
}


bool EffectEcho::GetAutomationParameters(CommandParameters & parms)
{
   parms.WriteFloat(KEY_Delay, delay);
   parms.WriteFloat(KEY_Decay, decay);

   return true;
}

bool EffectEcho::SetAutomationParameters(CommandParameters & parms)
{
   ReadAndVerifyFloat(Delay);
   ReadAndVerifyFloat(Decay);

   delay = Delay;
   decay = Decay;

   return true;
}

void EffectEcho::PopulateOrExchange(ShuttleGui & S)
{
   S.AddSpace(0, 5);

   S.StartMultiColumn(2, wxALIGN_CENTER);
   {
      FloatingPointValidator<double> vldDelay(3, &delay, NumValidatorStyle::NO_TRAILING_ZEROES);
      vldDelay.SetRange(MIN_Delay, MAX_Delay);
      S.AddTextBox(_("Delay time (seconds):"), wxT(""), 10)->SetValidator(vldDelay);

      FloatingPointValidator<double> vldDecay(3, &decay, NumValidatorStyle::NO_TRAILING_ZEROES);
      vldDecay.SetRange(MIN_Decay, MAX_Decay);
      S.AddTextBox(_("Decay factor:"), wxT(""), 10)->SetValidator(vldDecay);
   }
   S.EndMultiColumn();
}

bool EffectEcho::TransferDataToWindow()
{
   if (!mUIParent->TransferDataToWindow())
   {
      return false;
   }

   return true;
}

bool EffectEcho::TransferDataFromWindow()
{
   if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow())
   {
      return false;
   }

   return true;
}

