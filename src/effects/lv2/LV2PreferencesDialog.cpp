/**********************************************************************

  Audacity: A Digital Audio Editor

  LV2PreferencesDialog.cpp

  Paul Licameli split from LV2Prefernces.cpp

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2 or later.  See License.txt.

**********************************************************************/

#if defined(USE_LV2)

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wparentheses"
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
#elif defined(__clang__)
#pragma clang diagnostic ignored "-Wparentheses"
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
#endif

#include "LV2PreferencesDialog.h"
#include "LV2Preferences.h"
#include "ConfigInterface.h"
#include "ShuttleGui.h"
#include "../../widgets/valnum.h"

BEGIN_EVENT_TABLE(LV2PreferencesDialog, wxDialogWrapper)
   EVT_BUTTON(wxID_OK, LV2PreferencesDialog::OnOk)
END_EVENT_TABLE()

LV2PreferencesDialog::LV2PreferencesDialog(
   const EffectDefinitionInterface &effect
)  : wxDialogWrapper{ nullptr, wxID_ANY, XO("LV2 Effect Settings") }
   , mEffect{ effect }
{
   using namespace LV2Preferences;
   GetBufferSize(mEffect, mBufferSize);
   GetUseLatency(mEffect, mUseLatency);
   GetUseGUI(mEffect, mUseGUI);
   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);
}

LV2PreferencesDialog::~LV2PreferencesDialog() = default;

void LV2PreferencesDialog::PopulateOrExchange(ShuttleGui &S)
{
   S.SetBorder(5);
   S.StartHorizontalLay(wxEXPAND, 1);
   {
      S.StartVerticalLay(false);
      {
         // This really shouldn't be required for LV2 plugins because they have the ability
         // to specify their exact requirements in the TTL file and/or to check the host
         // supplied min/max values.  However, I've run across one (Harrison Consoles XT-EQ)
         // that crashed on sizes greater than 8192.
         S.StartStatic(XO("Buffer Size"));
         {
            IntegerValidator<int> vld(&mBufferSize);
            vld.SetRange(8, LV2Preferences::DEFAULT_BLOCKSIZE);

            S.AddVariableText( XO(
"The buffer size controls the number of samples sent to the effect "
"on each iteration. Smaller values will cause slower processing and "
"some effects require 8192 samples or less to work properly. However "
"most effects can accept large buffers and using them will greatly "
"reduce processing time."),
               false, 0, 650);

            S.StartHorizontalLay(wxALIGN_LEFT);
            {
               wxTextCtrl *t;
               t = S.TieNumericTextBox(
                  XXO("&Buffer Size (8 to %d) samples:")
                     .Format( LV2Preferences::DEFAULT_BLOCKSIZE ),
                  mBufferSize,
                  12);
               t->SetMinSize(wxSize(100, -1));
               t->SetValidator(vld);
            }
            S.EndHorizontalLay();
         }
         S.EndStatic();

         S.StartStatic(XO("Latency Compensation"));
         {
            S.AddVariableText( XO(
"As part of their processing, some LV2 effects must delay returning "
"audio to Audacity. When not compensating for this delay, you will "
"notice that small silences have been inserted into the audio. "
"Enabling this setting will provide that compensation, but it may "
"not work for all LV2 effects."),
               false, 0, 650);

            S.StartHorizontalLay(wxALIGN_LEFT);
            {
               S.TieCheckBox(XXO("Enable &compensation"),
                             mUseLatency);
            }
            S.EndHorizontalLay();
         }
         S.EndStatic();

         S.StartStatic(XO("Graphical Mode"));
         {
            S.AddVariableText( XO(
"LV2 effects can have a graphical interface for setting parameter values."
" A basic text-only method is also available. "
" Reopen the effect for this to take effect."),
               false, 0, 650);
            S.TieCheckBox(XXO("Enable &graphical interface"),
                          mUseGUI);
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

void LV2PreferencesDialog::OnOk(wxCommandEvent &WXUNUSED(evt))
{
   if (!Validate())
   {
      return;
   }

   ShuttleGui S(this, eIsGettingFromDialog);
   PopulateOrExchange(S);

   using namespace LV2Preferences;
   SetBufferSize(mEffect, mBufferSize);
   SetUseLatency(mEffect, mUseLatency);
   SetUseGUI(mEffect, mUseGUI);

   EndModal(wxID_OK);
}

#endif
