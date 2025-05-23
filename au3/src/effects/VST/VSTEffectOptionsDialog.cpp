/**********************************************************************

  Audacity: A Digital Audio Editor

  VSTEffectOptionsDialog.cpp

  Dominic Mazzoni

  Paul Licameli split from VSTEffect.cpp


*//********************************************************************/

#include "VSTEffectOptionsDialog.h"

#include "ShuttleGui.h"
#include "../../widgets/valnum.h"

#include "ConfigInterface.h"

BEGIN_EVENT_TABLE(VSTEffectOptionsDialog, wxDialogWrapper)
EVT_BUTTON(wxID_OK, VSTEffectOptionsDialog::OnOk)
END_EVENT_TABLE()

VSTEffectOptionsDialog::VSTEffectOptionsDialog(
    const EffectDefinitionInterface& effect)
    : wxDialogWrapper{nullptr, wxID_ANY, XO("VST Effect Options")}
    , mEffect{effect}
{
    GetConfig(mEffect, PluginSettings::Shared, wxT("Options"),
              wxT("BufferSize"), mBufferSize, 8192);
    GetConfig(mEffect, PluginSettings::Shared, wxT("Options"),
              wxT("UseLatency"), mUseLatency, true);
    GetConfig(mEffect, PluginSettings::Shared, wxT("Options"),
              wxT("UseGUI"), mUseGUI, true);

    ShuttleGui S(this, eIsCreating);
    PopulateOrExchange(S);
}

VSTEffectOptionsDialog::~VSTEffectOptionsDialog()
{
}

void VSTEffectOptionsDialog::PopulateOrExchange(ShuttleGui& S)
{
    S.SetBorder(5);
    S.StartHorizontalLay(wxEXPAND, 1);
    {
        S.StartVerticalLay(false);
        {
            S.StartStatic(XO("Buffer Size"));
            {
                S.AddVariableText(XO(
                                      "The buffer size controls the number of samples sent to the effect "
                                      "on each iteration. Smaller values will cause slower processing and "
                                      "some effects require 8192 samples or less to work properly. However "
                                      "most effects can accept large buffers and using them will greatly "
                                      "reduce processing time."),
                                  false, 0, 650);

                S.StartHorizontalLay(wxALIGN_LEFT);
                {
                    wxTextCtrl* t;
                    t = S.Validator<IntegerValidator<int> >(
                        &mBufferSize, NumValidatorStyle::DEFAULT, 8, 1048576 * 1)
                        .MinSize({ 100, -1 })
                        .TieNumericTextBox(XXO("&Buffer Size (8 to 1048576 samples):"),
                                           mBufferSize,
                                           12);
                }
                S.EndHorizontalLay();
            }
            S.EndStatic();

            S.StartStatic(XO("Latency Compensation"));
            {
                S.AddVariableText(XO(
                                      "As part of their processing, some VST effects must delay returning "
                                      "audio to Audacity. When not compensating for this delay, you will "
                                      "notice that small silences have been inserted into the audio. "
                                      "Enabling this option will provide that compensation, but it may "
                                      "not work for all VST effects."),
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
                S.AddVariableText(XO(
                                      "Most VST effects have a graphical interface for setting parameter values."
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

void VSTEffectOptionsDialog::OnOk(wxCommandEvent& WXUNUSED(evt))
{
    if (!Validate()) {
        return;
    }

    ShuttleGui S(this, eIsGettingFromDialog);
    PopulateOrExchange(S);

    SetConfig(mEffect, PluginSettings::Shared, wxT("Options"),
              wxT("BufferSize"), mBufferSize);
    SetConfig(mEffect, PluginSettings::Shared, wxT("Options"),
              wxT("UseLatency"), mUseLatency);
    SetConfig(mEffect, PluginSettings::Shared, wxT("Options"),
              wxT("UseGUI"), mUseGUI);

    EndModal(wxID_OK);
}
