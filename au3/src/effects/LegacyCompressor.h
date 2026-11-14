/**********************************************************************

  Audacity: A Digital Audio Editor

  LegacyCompressor.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EFFECT_COMPRESSOR__
#define __AUDACITY_EFFECT_COMPRESSOR__

#include "LegacyCompressorBase.h"
#include "StatefulEffectUIServices.h"
#include "wxPanelWrapper.h"

class wxCheckBox;
class wxSlider;
class wxStaticText;
class EffectLegacyCompressorPanel;
class ShuttleGui;

class EffectLegacyCompressor final : public LegacyCompressorBase, public StatefulEffectUIServices
{
public:
    std::unique_ptr<EffectEditor> PopulateOrExchange(
        ShuttleGui& S, EffectInstance& instance, EffectSettingsAccess& access, const EffectOutputs* pOutputs) override;
    bool DoTransferDataFromWindow();
    bool TransferDataToWindow(const EffectSettings& settings) override;
    bool TransferDataFromWindow(EffectSettings& settings) override;

    DECLARE_EVENT_TABLE()
private:
    void OnSlider(wxCommandEvent& evt);
    void UpdateUI();

    wxWeakRef<wxWindow> mUIParent {};

    EffectLegacyCompressorPanel* mPanel;

    wxStaticText* mThresholdLabel;
    wxSlider* mThresholdSlider;
    wxStaticText* mThresholdText;

    wxStaticText* mNoiseFloorLabel;
    wxSlider* mNoiseFloorSlider;
    wxStaticText* mNoiseFloorText;

    wxStaticText* mRatioLabel;
    wxSlider* mRatioSlider;
    wxStaticText* mRatioText;

    wxStaticText* mAttackLabel;
    wxSlider* mAttackSlider;
    wxStaticText* mAttackText;

    wxStaticText* mDecayLabel;
    wxSlider* mDecaySlider;
    wxStaticText* mDecayText;

    wxCheckBox* mGainCheckBox;
    wxCheckBox* mPeakCheckBox;
};

class EffectLegacyCompressorPanel final : public wxPanelWrapper
{
public:
    EffectLegacyCompressorPanel(
        wxWindow* parent, wxWindowID winid, double& threshold, double& noiseFloor, double& ratio);

private:
    void OnPaint(wxPaintEvent& evt);
    void OnSize(wxSizeEvent& evt);

private:
    double& threshold;
    double& noiseFloor;
    double& ratio;

    DECLARE_EVENT_TABLE()
};

#endif
