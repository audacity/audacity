/**********************************************************************

  Audacity: A Digital Audio Editor

  VSTEffectOptionsDialog.h

  Dominic Mazzoni

  Paul Licameli split from VSTEffect.cpp

**********************************************************************/
#ifndef __AUDACITY_VST_EFFECT_OPTIONS_DIALOG__
#define __AUDACITY_VST_EFFECT_OPTIONS_DIALOG__

#include "wxPanelWrapper.h"

class EffectDefinitionInterface;
class ShuttleGui;

//! Dialog for configuring latency, buffer size and graphics mode for a
//! VST effect.
class VSTEffectOptionsDialog final : public wxDialogWrapper
{
public:
    explicit VSTEffectOptionsDialog(const EffectDefinitionInterface& effect);
    virtual ~VSTEffectOptionsDialog();

    void PopulateOrExchange(ShuttleGui& S);

    void OnOk(wxCommandEvent& evt);

private:
    const EffectDefinitionInterface& mEffect;
    int mBufferSize;
    bool mUseLatency;
    bool mUseGUI;

    DECLARE_EVENT_TABLE()
};

#endif
