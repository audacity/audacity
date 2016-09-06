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
#include "../widgets/valnum.h"

// Define keys, defaults, minimums, and maximums for the effect parameters
//
//     Name    Type     Key            Def   Min      Max      Scale
Param( Delay,  float,   XO("Delay"),   1.0f, 0.001f,  FLT_MAX, 1.0f );
Param( Decay,  float,   XO("Decay"),   0.5f, 0.0f,    FLT_MAX, 1.0f );

EffectEcho::EffectEcho()
{
   delay = DEF_Delay;
   decay = DEF_Decay;

   SetLinearEffectFlag(true);
}

EffectEcho::~EffectEcho()
{
}

// IdentInterface implementation

wxString EffectEcho::GetSymbol()
{
   return ECHO_PLUGIN_SYMBOL;
}

wxString EffectEcho::GetDescription()
{
   return XO("Repeats the selected audio again and again");
}

// EffectIdentInterface implementation

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
      history = new float[histLen];
   }
   catch ( const std::bad_alloc& ) {
      wxMessageBox(_("Requested value exceeds memory capacity."));
      return false;
   }

   memset(history, 0, sizeof(float) * histLen);
   
   return history != NULL;
}

bool EffectEcho::ProcessFinalize()
{
   delete [] history;

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

bool EffectEcho::GetAutomationParameters(EffectAutomationParameters & parms)
{
   parms.WriteFloat(KEY_Delay, delay);
   parms.WriteFloat(KEY_Decay, decay);

   return true;
}

bool EffectEcho::SetAutomationParameters(EffectAutomationParameters & parms)
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
      FloatingPointValidator<double> vldDelay(3, &delay, NUM_VAL_NO_TRAILING_ZEROES);
      vldDelay.SetRange(MIN_Delay, MAX_Delay);
      S.AddTextBox(_("Delay time (seconds):"), wxT(""), 10)->SetValidator(vldDelay);

      FloatingPointValidator<double> vldDecay(3, &decay, NUM_VAL_NO_TRAILING_ZEROES);
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

