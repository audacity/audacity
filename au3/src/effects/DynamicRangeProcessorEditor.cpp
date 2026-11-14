#include "DynamicRangeProcessorEditor.h"
#include "AllThemeResources.h"
#include "BasicUI.h"
#include "ClipIndicatorPanel.h"
#include "CompressionMeterPanel.h"
#include "CompressorProcessor.h"
#include "DynamicRangeProcessorHistoryPanel.h"
#include "DynamicRangeProcessorPanelCommon.h"
#include "DynamicRangeProcessorTransferFunctionPanel.h"
#include "EffectInterface.h"
#include "ShuttleGui.h"
#include "Theme.h"
#include "widgets/LinearDBFormat.h"
#include "widgets/RulerPanel.h"
#include "widgets/valnum.h"
#include <wx/checkbox.h>
#include <wx/dialog.h>
#include <wx/sizer.h>
#include <wx/slider.h>
#include <wx/textctrl.h>

#if wxUSE_ACCESSIBILITY
#   include "WindowAccessible.h"
#endif

namespace {
using TFPanel = DynamicRangeProcessorTransferFunctionPanel;
using HistPanel = DynamicRangeProcessorHistoryPanel;

constexpr auto historyPanelId = wxID_HIGHEST + 1;
constexpr auto historyRulerPanelId = wxID_HIGHEST + 2;
constexpr auto transferFunctionPanelId = wxID_HIGHEST + 3;
constexpr auto checkboxId = wxID_HIGHEST + 4;
constexpr auto compressionMeterRulerPanelId = wxID_HIGHEST + 5;
constexpr auto compressionMeterPanelId = wxID_HIGHEST + 6;
constexpr auto clipIndicatorId = wxID_HIGHEST + 7;
constexpr auto rulerWidth = 30;
constexpr auto borderSize = 5;
constexpr auto compressionMeterPanelWidth = 30;

float GetDbRange(float maxCompressionDb)
{
    constexpr auto minRange = 3.f;
    return std::max(minRange, maxCompressionDb / 2);
}

DynamicRangeProcessorSettings GetSettings(EffectSettingsAccess& access)
{
    const CompressorSettings* compressorSettings
        =access.Get().cast<CompressorSettings>();
    const LimiterSettings* limiterSettings
        =access.Get().cast<LimiterSettings>();
    return compressorSettings
           ? DynamicRangeProcessorSettings { *compressorSettings }
           : DynamicRangeProcessorSettings { *limiterSettings };
}

auto MakeRulerPanel(
    wxWindow* parent, wxOrientation orientation, float dbRange,
    int id = wxID_ANY)
{
    // Overridden by `.MinSize()` calls
    const wxSize minSize { -1, -1 };

    return safenew RulerPanel(
        parent, id, orientation, minSize,
        orientation == wxVERTICAL ? RulerPanel::Range { 0.0, -dbRange }
        : RulerPanel::Range { -dbRange, 0.0 },
        LinearDBFormat::Instance(), XO(""),
        RulerPanel::Options {}
        .LabelEdges(false)
        .TicksAtExtremes(true)
        .Flip(orientation == wxVERTICAL)
        .TickColour(theTheme.Colour(clrGraphLabels)));
}

constexpr auto C = 5.;

// Assumes x to be in [0, 1]
auto MapExponentially(double x)
{
    return (std::exp(C * x) - 1) / (std::exp(C) - 1.);
}

auto MapExponentially(double min, double max, double x)
{
    // Map min and max to 0 and 1, and x accordingly, before applying
    // exponential, or we may run into NaNs.
    const auto a = 1.0 / (max - min);
    const auto b = -min / (max - min);
    const auto result = (MapExponentially(a * x + b) - b) / a;
    // MH: Because we're using NumValidatorStyle::ONE_TRAILING_ZERO, we also
    // quantize here to 0.1. (I think that's why.)
    return std::round(result * 10) / 10;
}

auto MapLogarithmically(double x)
{
    return (std::log(x * (std::exp(C) - 1) + 1)) / C;
}

auto MapLogarithmically(double min, double max, double x)
{
    const auto a = 1.0 / (max - min);
    const auto b = -min / (max - min);
    return (MapLogarithmically(a * x + b) - b) / a;
}

auto SliderToTextValue(double value, const ExtendedCompressorParameter& setting)
{
    const auto scaled = value / setting.param->TextToSlider();
    return setting.attributes.exponentialSlider
           ? MapExponentially(
        setting.param->SliderMin(), setting.param->SliderMax(),
        scaled)
           : scaled;
}

auto TextToSliderValue(ExtendedCompressorParameter& setting)
{
    const auto unscaled = setting.attributes.exponentialSlider
                          ? MapLogarithmically(
        setting.param->SliderMin(),
        setting.param->SliderMax(), setting.value)
                          : setting.value;
    return unscaled * setting.param->TextToSlider();
}
} // namespace

DynamicRangeProcessorEditor::DynamicRangeProcessorEditor(
    wxWindow* parent, CompressorInstance& instance, bool isRealtime,
    const EffectUIServices& services, EffectSettingsAccess& access)
    : EffectEditor{services, access}
    , mUIParent{parent}
    , mTopLevelParent(static_cast<wxDialog&>(*wxGetTopLevelParent(parent)))
    , mCompressorInstance{instance}
    , mIsRealtime{isRealtime}
{
}

void DynamicRangeProcessorEditor::Initialize(
    std::vector<ExtendedCompressorParameter> parameters)
{
    assert(std::is_sorted(
               parameters.begin(), parameters.end(),
               [](const auto& a, const auto& b) { return a.category < b.category; }));
    mParameters = std::move(parameters);
}

void DynamicRangeProcessorEditor::PopulateOrExchange(ShuttleGui& S)
{
    S.SetBorder(borderSize);
    S.AddSpace(0, borderSize);

    const auto compressorSettings = GetCompressorSettings();
    if (compressorSettings) {
        S.StartMultiColumn(2, wxEXPAND);
        {
            S.SetStretchyCol(0);
            AddSliderPanel(S);
            AddCompressionCurvePanel(S, *compressorSettings);
        }
        S.EndMultiColumn();
    } else {
        AddSliderPanel(S);
    }

    if (!mIsRealtime) {
        // Not a real-time effect editor, no need for a graph
        return;
    }

    const auto settings = GetSettings(mAccess);

    const auto rulerPanel = MakeRulerPanel(
        mUIParent, wxVERTICAL,
        GetDbRange(CompressorProcessor::GetMaxCompressionDb(settings)),
        historyRulerPanelId);

    const auto onDbRangeChanged = [this](float newDbRange) {
        for (const auto id :
             { historyRulerPanelId, compressionMeterRulerPanelId }) {
            if (
                const auto panel = dynamic_cast<RulerPanel*>(
                    wxWindow::FindWindowById(id, mUIParent))) {
                panel->ruler.SetRange(0., -newDbRange);
                panel->Refresh();
            }
        }
        if (
            const auto compressionMeterPanel
                =dynamic_cast<CompressionMeterPanel*>(
                      wxWindow::FindWindowById(compressionMeterPanelId, mUIParent))) {
            compressionMeterPanel->SetDbRange(newDbRange);
        }
    };

    const auto histPanel = safenew HistPanel(
        mUIParent, historyPanelId, mCompressorInstance, onDbRangeChanged);
    histPanel->ShowInput(settings.showInput);
    histPanel->ShowOutput(settings.showOutput);
    histPanel->ShowActual(settings.showActual);
    histPanel->ShowTarget(settings.showTarget);

    S.StartMultiColumn(compressorSettings ? 3 : 1, wxEXPAND);
    {
        S.SetStretchyCol(0);
        AddCheckboxPanel(S, settings);
        if (compressorSettings) {
            AddClipIndicator(S);
            S.AddSpace(rulerWidth, 0);
        }
    }
    S.EndMultiColumn();

    S.SetSizerProportion(1);
    S.StartMultiColumn(5, wxEXPAND);
    {
        using namespace DynamicRangeProcessorPanel;

        S.SetStretchyCol(1);
        S.SetStretchyRow(0);
        S.AddSpace(borderSize, 0);
        S.Prop(1)
        .Position(wxALIGN_LEFT | wxALIGN_TOP | wxEXPAND)
        .MinSize({ HistPanel::minWidth, graphMinHeight })
        .AddWindow(histPanel);

        S.AddSpace(borderSize, 0);

        const auto onClipped = [this] {
            if (
                const auto panel = dynamic_cast<ClipIndicatorPanel*>(
                    wxWindow::FindWindowById(clipIndicatorId, mUIParent))) {
                panel->SetClipped();
            }
        };
        S.Prop(1)
        .Position(wxALIGN_LEFT | wxALIGN_TOP | wxEXPAND)
        .MinSize({ compressionMeterPanelWidth, graphMinHeight })
        .AddWindow(safenew CompressionMeterPanel(
                       mUIParent, compressionMeterPanelId, mCompressorInstance,
                       graphMinRangeDb, onClipped))
        ->Bind(wxEVT_LEFT_UP, [this](wxMouseEvent& evt) {
            if (
                const auto panel
                    =dynamic_cast<CompressionMeterPanel*>(evt.GetEventObject())) {
                panel->Reset();
            }
            if (
                const auto indicator = dynamic_cast<ClipIndicatorPanel*>(
                    wxWindow::FindWindowById(clipIndicatorId, mUIParent))) {
                indicator->Reset();
            }
        });

        S.Prop(1)
        .Position(wxEXPAND | wxALIGN_TOP)
        .MinSize({ rulerWidth, graphMinHeight })
        .AddWindow(rulerPanel);
    }
    S.EndMultiColumn();

    S.AddSpace(0, borderSize);
}

void DynamicRangeProcessorEditor::AddCheckboxPanel(
    ShuttleGui& S, const DynamicRangeProcessorSettings& settings)
{
#define GET_REF(settingName)                                        \
    GetCompressorSettings() ? GetCompressorSettings()->settingName : \
    GetLimiterSettings()->settingName

    S.StartVerticalLay(wxALIGN_BOTTOM, 0);
    {
        S.StartHorizontalLay(wxALIGN_LEFT, 0);
        {
            S.AddSpace(borderSize, 0);
            const auto input = S.AddCheckBox(XO("I&nput"), settings.showInput);
            input->Bind(wxEVT_CHECKBOX, [this](wxCommandEvent& evt) {
                OnCheckbox(
                    evt.IsChecked(), GET_REF(showInput), &HistPanel::ShowInput);
            });
            /* i18n-hint: show input on a graph */
            input->SetName(_("Show input"));

            const auto output = S.AddCheckBox(XO("O&utput"), settings.showOutput);
            output->Bind(wxEVT_CHECKBOX, [&](wxCommandEvent& evt) {
                OnCheckbox(
                    evt.IsChecked(), GET_REF(showOutput), &HistPanel::ShowOutput);
            });
            /* i18n-hint: show output on a graph */
            output->SetName(_("Show output"));

            /* i18n-hint: The effective compression, including smoothing. */
            const auto actual
                =S.AddCheckBox(XO("A&ctual compression"), settings.showActual);
            actual->Bind(wxEVT_CHECKBOX, [&](wxCommandEvent& evt) {
                OnCheckbox(
                    evt.IsChecked(), GET_REF(showActual), &HistPanel::ShowActual);
            });
            /* i18n-hint: show actual compression on a graph */
            actual->SetName(_("Show actual compression"));

            /* i18n-hint: The target compression, before smoothing. */
            const auto target
                =S.AddCheckBox(XO("Tar&get compression"), settings.showTarget);
            target->Bind(wxEVT_CHECKBOX, [&](wxCommandEvent& evt) {
                OnCheckbox(
                    evt.IsChecked(), GET_REF(showTarget), &HistPanel::ShowTarget);
            });
            /* i18n-hint: show target compression on a graph */
            target->SetName(_("Show target compression"));

#if wxUSE_ACCESSIBILITY
            // so that name can be set on a standard control
            safenew WindowAccessible(input);
            safenew WindowAccessible(output);
            safenew WindowAccessible(actual);
            safenew WindowAccessible(target);
#endif
        }
        S.EndHorizontalLay();
        S.AddSpace(0, borderSize);
    }
    S.EndVerticalLay();
}

void DynamicRangeProcessorEditor::AddClipIndicator(ShuttleGui& S)
{
    constexpr auto width = compressionMeterPanelWidth / 2 - 2;
    constexpr auto height = compressionMeterPanelWidth / 4;
    S.MinSize({ width, height })
    .AddWindow(
        safenew ClipIndicatorPanel(mUIParent, clipIndicatorId),
        wxALIGN_RIGHT | wxALIGN_BOTTOM)
    ->Bind(wxEVT_LEFT_UP, [this](wxMouseEvent& evt) {
        if (
            const auto panel = dynamic_cast<CompressionMeterPanel*>(
                wxWindow::FindWindowById(compressionMeterPanelId, mUIParent))) {
            panel->ResetClipped();
        }
        if (
            const auto indicator
                =dynamic_cast<ClipIndicatorPanel*>(evt.GetEventObject())) {
            indicator->Reset();
        }
    });
}

void DynamicRangeProcessorEditor::AddCompressionCurvePanel(
    ShuttleGui& S, const CompressorSettings& compressorSettings)
{
    S.StartMultiColumn(3);
    {
        S.SetStretchyRow(1);

        constexpr auto tfWidth = 200;

        // Horizontal ruler row
        S.AddSpace(borderSize, 0);
        S.Prop(1)
        .Position(wxALIGN_BOTTOM)
        .MinSize({ tfWidth, rulerWidth })
        .AddWindow(MakeRulerPanel(mUIParent, wxHORIZONTAL, TFPanel::rangeDb));
        S.AddSpace(rulerWidth, 0);

        // Transfer function row
        S.AddSpace(borderSize, 0);
        S.Prop(1).Position(wxEXPAND).AddWindow(safenew TFPanel(
                                                   mUIParent, transferFunctionPanelId, compressorSettings));
        S.Prop(1).Position(wxEXPAND).AddWindow(
            MakeRulerPanel(mUIParent, wxVERTICAL, TFPanel::rangeDb));
    }
    S.EndMultiColumn();
}

void DynamicRangeProcessorEditor::AddSliderPanel(ShuttleGui& S)
{
    using It = std::vector<ExtendedCompressorParameter>::iterator;
    const auto addTextboxAndSliders = [&](
        const TranslatableString& prompt,
        const It& begin, const It& end) {
        S.StartStatic(prompt);
        S.StartMultiColumn(3, wxEXPAND);
        {
            S.SetStretchyCol(2);
            std::for_each(begin, end, [&](ExtendedCompressorParameter& parameter) {
                AddTextboxAndSlider(S, parameter);
            });
        }
        S.EndMultiColumn();
        S.EndStatic();
    };

    const auto firstSmoothingParameterIt
        =std::find_if(mParameters.begin(), mParameters.end(), [](const auto& p) {
        return p.category == ControllerCategory::TimeSmoothing;
    });

    S.StartPanel();
    {
        addTextboxAndSliders(
            XO("Compression curve"), mParameters.begin(),
            firstSmoothingParameterIt);
        addTextboxAndSliders(
            XO("Smoothing"), firstSmoothingParameterIt, mParameters.end());
    }
    S.EndPanel();
}

void DynamicRangeProcessorEditor::AddTextboxAndSlider(
    ShuttleGui& S, ExtendedCompressorParameter& setting)
{
    setting.text = S.Validator<FloatingPointValidator<double> >(
        1, &setting.value, NumValidatorStyle::ONE_TRAILING_ZERO,
        setting.param->Min(), setting.param->Max())
                   .AddTextBox(setting.attributes.caption, L"", 12);

    setting.text->Bind(wxEVT_TEXT, [&](wxCommandEvent& evt) {
        if (!EnableApply(mUIParent, mUIParent->TransferDataFromWindow())) {
            return;
        }
        setting.slider->SetValue(TextToSliderValue(setting));
        ValidateUI();
        UpdateUI();
        Publish(EffectSettingChanged {});
    });

    setting.slider
        =S.Name(setting.attributes.caption)
          .Style(wxSL_HORIZONTAL)
          .MinSize({ 100, -1 })
          .AddSlider(
              {}, TextToSliderValue(setting), setting.param->SliderMax(),
              setting.param->SliderMin());

    setting.slider->Bind(wxEVT_SLIDER, [&](wxCommandEvent& evt) {
        setting.value = SliderToTextValue(setting.slider->GetValue(), setting);
        setting.text->GetValidator()->TransferToWindow();
        EnableApply(mUIParent, mUIParent->Validate());
        ValidateUI();
        UpdateUI();
        Publish(EffectSettingChanged {});
    });

    // For exponential slider, for right/down arrow keys, because
    // the setting value has precision of 1, ensure that
    // the change in slider position results a change in value of
    // at least 0.1, otherwise the slider position and setting value
    // may not change.
    setting.slider->Bind(wxEVT_SCROLL_LINEDOWN, [&](wxScrollEvent& evt) {
        if (setting.attributes.exponentialSlider
            && setting.value == SliderToTextValue(evt.GetInt(), setting)) {
            setting.value += 0.1;
            setting.slider->SetValue(std::round(TextToSliderValue(setting)));
        }
    });

    // And similarly for left/up arrow keys.
    setting.slider->Bind(wxEVT_SCROLL_LINEUP, [&](wxScrollEvent& evt) {
        if (setting.attributes.exponentialSlider
            && setting.value == SliderToTextValue(evt.GetInt(), setting)) {
            setting.value -= 0.1;
            setting.slider->SetValue(std::round(TextToSliderValue(setting)));
        }
    });
}

bool DynamicRangeProcessorEditor::ValidateUI()
{
    mAccess.ModifySettings([this](EffectSettings& settings) {
        // pass back the modified settings to the MessageBuffer
        if (auto compressorSettings = settings.cast<CompressorSettings>()) {
            *compressorSettings = *GetCompressorSettings();
        } else {
            *settings.cast<LimiterSettings>() = *GetLimiterSettings();
        }
        return nullptr;
    });
    return true;
}

bool DynamicRangeProcessorEditor::UpdateUI()
{
    if (CompressorSettings* compressorSettings = GetCompressorSettings()) {
        *compressorSettings = *mAccess.Get().cast<CompressorSettings>();
    } else {
        *GetLimiterSettings() = *mAccess.Get().cast<LimiterSettings>();
    }

    for (auto& setting : mParameters) {
        setting.slider->SetValue(std::round(TextToSliderValue(setting)));
    }

    if (
        auto tfPanel
            =wxWindow::FindWindowById(transferFunctionPanelId, mUIParent)) {
        tfPanel->Refresh();
    }

    return true;
}

void DynamicRangeProcessorEditor::OnCheckbox(
    bool newVal, double& setting,
    void (DynamicRangeProcessorHistoryPanel::*setter)(bool))
{
    setting = newVal;
    if (
        const auto panel = dynamic_cast<HistPanel*>(
            wxWindow::FindWindowById(historyPanelId, mUIParent))) {
        (panel->*setter)(newVal);
    }
    ValidateUI();
    Publish(EffectSettingChanged {});
}
