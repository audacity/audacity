#pragma once

#include "DynamicRangeProcessorHistory.h"
#include "EffectEditor.h"
#include "Internat.h" // TranslatableString
#include <optional>
#include <wx/weakref.h>

class DynamicRangeProcessorHistoryPanel;
class RulerPanel;
class wxDialog;
class wxSlider;
class wxTextCtrl;
class CompressorInstance;
class ShuttleGui;
struct CompressorSettings;
struct LimiterSettings;

struct DynamicRangeProcessorParameterAttributes
{
    const TranslatableString caption;
    const bool exponentialSlider = false;
};

static const DynamicRangeProcessorParameterAttributes
    compressorThresholdDbAttributes { XXO("&Threshold (dB)") };

static const DynamicRangeProcessorParameterAttributes
    compressorMakupGainDbAttributes { XXO("&Make-up gain (dB)") };

static const DynamicRangeProcessorParameterAttributes
    limiterThresholdDbAttributes { XXO("&Threshold (dB)") };

static const DynamicRangeProcessorParameterAttributes
    limiterMakeupTargetDbAttributes { XXO("&Make-up target (dB)") };

static const DynamicRangeProcessorParameterAttributes kneeWidthDbAttributes {
    XXO("Knee &width (dB)")
};

static const DynamicRangeProcessorParameterAttributes
    compressionRatioAttributes { XXO("Rati&o:"), true };

static const DynamicRangeProcessorParameterAttributes lookaheadMsAttributes {
    XXO("&Lookahead (ms)"), true
};

static const DynamicRangeProcessorParameterAttributes attackMsAttributes {
    XXO("Attac&k (ms)"), true
};

static const DynamicRangeProcessorParameterAttributes releaseMsAttributes {
    XXO("&Release (ms)"), true
};

//! Abstracts different parameter types (CompressorParameter and
//! LimiterParameter) of the derived dynamic-range processor classes, giving
//! access to those members needed by the UI.
class DynamicRangeProcessorParameter
{
public:
    virtual ~DynamicRangeProcessorParameter() = default;
    virtual double Min() const = 0;
    virtual double Max() const = 0;
    virtual double SliderMin() const = 0;
    virtual double SliderMax() const = 0;
    virtual double TextToSlider() const = 0;
};

enum class ControllerCategory
{
    CompressionCurve,
    TimeSmoothing,
};

struct ExtendedCompressorParameter
{
    double& value;
    const DynamicRangeProcessorParameterAttributes& attributes;
    const std::shared_ptr<DynamicRangeProcessorParameter> param;
    const ControllerCategory category;
    wxTextCtrl* text = nullptr;
    wxSlider* slider = nullptr;
};

class DynamicRangeProcessorEditor : public EffectEditor
{
public:
    DynamicRangeProcessorEditor(
        wxWindow* parent, CompressorInstance& instance, bool isRealtime, const EffectUIServices& services, EffectSettingsAccess& access);

protected:
    /*
     * \pre `parameters` are sorted by categoy, `CompressionCurve` first and then
     * `TimeSmoothing`.
     */
    void Initialize(std::vector<ExtendedCompressorParameter> parameters);
    static constexpr auto dbStep = 0.1;

public:
    void PopulateOrExchange(ShuttleGui& S);

private:
    void AddCompressionCurvePanel(ShuttleGui& S, const CompressorSettings&);
    void AddSliderPanel(ShuttleGui& S);
    void AddCheckboxPanel(
        ShuttleGui& S, const DynamicRangeProcessorSettings& settings);
    void AddClipIndicator(ShuttleGui& S);

    virtual const CompressorSettings* GetCompressorSettings() const
    {
        return nullptr;
    }

    virtual const LimiterSettings* GetLimiterSettings() const
    {
        return nullptr;
    }

    CompressorSettings* GetCompressorSettings()
    {
        return const_cast<CompressorSettings*>(
            const_cast<const DynamicRangeProcessorEditor&>(*this)
            .GetCompressorSettings());
    }

    LimiterSettings* GetLimiterSettings()
    {
        return const_cast<LimiterSettings*>(
            const_cast<const DynamicRangeProcessorEditor&>(*this)
            .GetLimiterSettings());
    }

    void AddTextboxAndSlider(ShuttleGui& S, ExtendedCompressorParameter& param);

    bool ValidateUI() final override;
    bool UpdateUI() final override;
    void OnCheckbox(
        bool newVal, double& setting,
        void (DynamicRangeProcessorHistoryPanel::*)(bool));

    struct HistoryPanels
    {
        DynamicRangeProcessorHistoryPanel* historyPanel;
        RulerPanel* rulerPanel;
    };

    wxWeakRef<wxWindow> mUIParent;
    std::vector<ExtendedCompressorParameter> mParameters;
    wxDialog& mTopLevelParent;
    CompressorInstance& mCompressorInstance;
    const bool mIsRealtime;
    int mFullHeight { 0 };
};
