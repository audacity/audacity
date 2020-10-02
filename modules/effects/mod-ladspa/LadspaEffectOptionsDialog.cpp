/**********************************************************************

  Audacity: A Digital Audio Editor

  LadspaEffectOptionsDialog.cpp

  Dominic Mazzoni

  Paul Licameli split from LadspaEffect.cpp

*//*******************************************************************/
#include "LadspaEffectOptionsDialog.h"
#include "LadspaInstance.h"
#include "ConfigInterface.h"
#include "ShuttleGui.h"

BEGIN_EVENT_TABLE(LadspaEffectOptionsDialog, wxDialogWrapper)
   EVT_BUTTON(wxID_OK, LadspaEffectOptionsDialog::OnOk)
END_EVENT_TABLE()

LadspaEffectOptionsDialog::LadspaEffectOptionsDialog(
   const EffectDefinitionInterface &effect
)  : wxDialogWrapper{ nullptr, wxID_ANY, XO("LADSPA Effect Options") }
   , mEffect{ effect }
   , mUseLatency{ LadspaInstance::LoadUseLatency(mEffect) }
{
   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);
}

LadspaEffectOptionsDialog::~LadspaEffectOptionsDialog()
{
}

void LadspaEffectOptionsDialog::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(5);
   S.StartHorizontalLay(wxEXPAND, 1);
   {
      S.StartVerticalLay(false);
      {
         S.StartStatic(XO("Latency Compensation"));
         {
            S.AddVariableText( XO(
"As part of their processing, some LADSPA effects must delay returning "
"audio to Audacity. When not compensating for this delay, you will "
"notice that small silences have been inserted into the audio. "
"Enabling this option will provide that compensation, but it may "
"not work for all LADSPA effects."),
               false, 0, 650);

            S.StartHorizontalLay(wxALIGN_LEFT);
            {
               S.TieCheckBox(XXO("Enable &compensation"),
                             mUseLatency);
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

void LadspaEffectOptionsDialog::OnOk(wxCommandEvent & WXUNUSED(evt))
{
   if (!Validate())
   {
      return;
   }

   ShuttleGui S(this, eIsGettingFromDialog);
   // Note this call re-visits the controls, not to create them but to fetch
   // the values, in this case mUseLatency
   PopulateOrExchange(S);

   LadspaInstance::SaveUseLatency(mEffect, mUseLatency);

   EndModal(wxID_OK);
}
