/**********************************************************************

  Audacity: A Digital Audio Editor

  Nyquist.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EFFECT_NYQUIST__
#define __AUDACITY_EFFECT_NYQUIST__

#include "../StatefulEffectUIServices.h"
#include "NyquistBase.h"
#include "wxPanelWrapper.h"

class wxArrayString;
class wxFileName;
class wxCheckBox;
class wxTextCtrl;

class EffectOutputTracks;

class NyquistEffect final : public NyquistBase, public StatefulEffectUIServices
{
public:
    using NyquistBase::NyquistBase;

private:
    void BuildPromptWindow(ShuttleGui& S);
    void BuildEffectWindow(ShuttleGui& S);

    bool TransferDataToPromptWindow();
    bool TransferDataToEffectWindow();

    bool TransferDataFromPromptWindow();
    bool TransferDataFromEffectWindow();

    int ShowHostInterface(
        EffectBase& plugin, wxWindow& parent, const EffectDialogFactory& factory, std::shared_ptr<EffectInstance>& pInstance,
        EffectSettingsAccess& access, bool forceModal = false) override;
    std::unique_ptr<EffectEditor> PopulateOrExchange(
        ShuttleGui& S, EffectInstance& instance, EffectSettingsAccess& access, const EffectOutputs* pOutputs) override;
    bool TransferDataToWindow(const EffectSettings& settings) override;
    bool TransferDataFromWindow(EffectSettings& settings) override;

protected:
    wxTextCtrl* mCommandText;

    DECLARE_EVENT_TABLE()
    void OnLoad(wxCommandEvent& evt);
    void OnSave(wxCommandEvent& evt);
    void OnDebug(wxCommandEvent& evt);

    void OnText(wxCommandEvent& evt);
    void OnSlider(wxCommandEvent& evt);
    void OnChoice(wxCommandEvent& evt);
    void OnTime(wxCommandEvent& evt);
    void OnFileButton(wxCommandEvent& evt);

private:
    wxWeakRef<wxWindow> mUIParent {};
};

class NyquistOutputDialog final : public wxDialogWrapper
{
public:
    NyquistOutputDialog(
        const TranslatableString& title, const TranslatableString& message);

private:
    void OnOk(wxCommandEvent& event);

private:
    DECLARE_EVENT_TABLE()
};

#endif
