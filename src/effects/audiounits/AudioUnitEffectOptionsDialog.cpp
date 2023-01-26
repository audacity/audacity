/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file AudioUnitEffectOptionsDialog.h

  Dominic Mazzoni
  Leland Lucius

  Paul Licameli split from AudioUnitEffect.cpp

**********************************************************************/

#if USE_AUDIO_UNITS
#include "AudioUnitEffectOptionsDialog.h"
#include "ConfigInterface.h"
#include "../../ShuttleGui.h"

///////////////////////////////////////////////////////////////////////////////
//
// AudioUnitEffectOptionsDialog
//
///////////////////////////////////////////////////////////////////////////////

BEGIN_EVENT_TABLE(AudioUnitEffectOptionsDialog, wxDialogWrapper)
   EVT_BUTTON(wxID_OK, AudioUnitEffectOptionsDialog::OnOk)
END_EVENT_TABLE()

AudioUnitEffectOptionsDialog::AudioUnitEffectOptionsDialog(
   const EffectDefinitionInterface &effect
)  : wxDialogWrapper{ nullptr, wxID_ANY, XO("Audio Unit Effect Options") }
   , mEffect{ effect }
{
   // Consult preferences
   // Decide mUseLatency, which affects GetLatency(), which is actually used
   // so far only in destructive effect processing
   GetConfig(effect, PluginSettings::Shared, OptionsKey, UseLatencyKey,
      mUseLatency, true);

   wxString uiType;
   GetConfig(effect, PluginSettings::Shared, OptionsKey, UITypeKey,
      uiType, FullValue.MSGID().GET() /* Config stores un-localized string */);
   // Get the localization of the string for display to the user
   mUITypeString = TranslatableString{ uiType, {} };

   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);
}

AudioUnitEffectOptionsDialog::~AudioUnitEffectOptionsDialog()
{
}

void AudioUnitEffectOptionsDialog::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(5);
   S.StartHorizontalLay(wxEXPAND, 1);
   {
      S.StartVerticalLay(false);
      {
         S.StartStatic(XO("Latency Compensation"));
         {
            S.AddVariableText(XO(
"As part of their processing, some Audio Unit effects must delay returning "
"audio to Audacity. When not compensating for this delay, you will "
"notice that small silences have been inserted into the audio. "
"Enabling this option will provide that compensation, but it may "
"not work for all Audio Unit effects."),
               false, 0, 650);

            S.StartHorizontalLay(wxALIGN_LEFT);
            {
               S.TieCheckBox(XXO("Enable &compensation"),
                             mUseLatency);
            }
            S.EndHorizontalLay();
         }
         S.EndStatic();

         S.StartStatic(XO("User Interface"));
         {
            S.AddVariableText(XO(
"Select \"Full\" to use the graphical interface if supplied by the Audio Unit."
" Select \"Generic\" to use the system supplied generic interface."
#if defined(HAVE_AUDIOUNIT_BASIC_SUPPORT)
" Select \"Basic\" for a basic text-only interface."
#endif
" Reopen the effect for this to take effect."),
               false, 0, 650);

            S.StartHorizontalLay(wxALIGN_LEFT);
            {
               S.TieChoice(XXO("Select &interface"),
                  mUITypeString,
                  {
                     FullValue,
                     GenericValue,
#if defined(HAVE_AUDIOUNIT_BASIC_SUPPORT)
                     BasicValue,
#endif
                  });
            }
            S.EndHorizontalLay();
         }
         S.EndStatic();
      }
      S.EndVerticalLay();
   }
   S.EndHorizontalLay();
   S.AddStandardButtons();
   Layout();
   Fit();
   Center();
}

void AudioUnitEffectOptionsDialog::OnOk(wxCommandEvent & WXUNUSED(evt))
{
   if (!Validate())
      return;

   // This re-visits the controls, not to create them but to transfer values out
   ShuttleGui S(this, eIsGettingFromDialog);
   PopulateOrExchange(S);

   // un-translate the type
   auto uiType = mUITypeString.MSGID().GET();

   // Save changed values to the config file
   SetConfig(mEffect, PluginSettings::Shared, OptionsKey, UseLatencyKey,
      mUseLatency);
   SetConfig(mEffect, PluginSettings::Shared, OptionsKey, UITypeKey, uiType);
   
   EndModal(wxID_OK);
}
#endif
