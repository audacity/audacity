#include "DynamicRangeProcessorEditor.h"
#include "AllThemeResources.h"
#include "BasicUI.h"
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

namespace
{
using TFPanel = DynamicRangeProcessorTransferFunctionPanel;
using HistPanel = DynamicRangeProcessorHistoryPanel;

constexpr auto historyPanelId = wxID_HIGHEST + 1;
constexpr auto historyRulerPanelId = wxID_HIGHEST + 2;
constexpr auto transferFunctionPanelId = wxID_HIGHEST + 3;
constexpr auto checkboxId = wxID_HIGHEST + 4;
constexpr auto rulerWidth = 30;
constexpr auto borderSize = 5;

float GetDbRange(float maxCompressionDb)
{
   constexpr auto minRange = 3.f;
   return std::max(minRange, maxCompressionDb / 2);
}

DynamicRangeProcessorSettings GetSettings(EffectSettingsAccess& access)
{
   const CompressorSettings* compressorSettings =
      access.Get().cast<CompressorSettings>();
   const LimiterSettings* limiterSettings =
      access.Get().cast<LimiterSettings>();
   return compressorSettings ?
             DynamicRangeProcessorSettings { *compressorSettings } :
             DynamicRangeProcessorSettings { *limiterSettings };
}

auto MakeRulerPanel(
   wxWindow* parent, wxOrientation orientation, float dbRange,
   int id = wxID_ANY)
{
   // Overridden by `.MinSize()` calls
   const wxSize minSize { -1, -1 };

   return safenew RulerPanel(
      parent, id, orientation, minSize,
      orientation == wxVERTICAL ? RulerPanel::Range { 0.0, -dbRange } :
                                  RulerPanel::Range { -dbRange, 0.0 },
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
   return setting.attributes.exponentialSlider ?
             MapExponentially(
                setting.param->Min(), setting.param->Max(), scaled) :
             scaled;
}

auto TextToSliderValue(ExtendedCompressorParameter& setting)
{
   const auto unscaled =
      setting.attributes.exponentialSlider ?
         MapLogarithmically(
            setting.param->Min(), setting.param->Max(), setting.value) :
         setting.value;
   return unscaled * setting.param->TextToSlider();
}
} // namespace

DynamicRangeProcessorEditor::DynamicRangeProcessorEditor(
   wxWindow* parent, CompressorInstance& instance, bool isRealtime,
   const EffectUIServices& services, EffectSettingsAccess& access)
    : EffectEditor { services, access }
    , mUIParent { parent }
    , mTopLevelParent(static_cast<wxDialog&>(*wxGetTopLevelParent(parent)))
    , mCompressorInstance { instance }
    , mIsRealtime { isRealtime }
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

   if (const auto compressorSettings = GetCompressorSettings())
      PopulateCompressorUpperHalf(S, *compressorSettings);
   else
      PopulateLimiterUpperHalf(S);

   if (!mIsRealtime)
      // Not a real-time effect editor, no need for a graph
      return;

   const auto settings = GetSettings(mAccess);

   const auto rulerPanel = MakeRulerPanel(
      mUIParent, wxVERTICAL,
      GetDbRange(CompressorProcessor::GetMaxCompressionDb(settings)),
      historyRulerPanelId);

   const auto histPanel = safenew HistPanel(
      mUIParent, historyPanelId, mCompressorInstance, [this](float newDbRange) {
         if (
            const auto panel = dynamic_cast<RulerPanel*>(
               wxWindow::FindWindowById(historyRulerPanelId, mUIParent)))
         {
            panel->ruler.SetRange(0., -newDbRange);
            panel->Refresh();
         }
      });
   histPanel->ShowInput(settings.showInput);
   histPanel->ShowOutput(settings.showOutput);
   histPanel->ShowOvershoot(settings.showOvershoot);
   histPanel->ShowUndershoot(settings.showUndershoot);

#define GET_REF(settingName)                                        \
   GetCompressorSettings() ? GetCompressorSettings()->settingName : \
                             GetLimiterSettings()->settingName

   S.StartHorizontalLay(wxALIGN_LEFT, 0);
   {
      S.AddSpace(borderSize, 0);
      S.AddCheckBox(XO("I&nput"), settings.showInput)
         ->Bind(wxEVT_CHECKBOX, [this](wxCommandEvent& evt) {
            OnCheckbox(
               evt.IsChecked(), GET_REF(showInput), &HistPanel::ShowInput);
         });
      S.AddCheckBox(XO("O&utput"), settings.showOutput)
         ->Bind(wxEVT_CHECKBOX, [&](wxCommandEvent& evt) {
            OnCheckbox(
               evt.IsChecked(), GET_REF(showOutput), &HistPanel::ShowOutput);
         });
      /* i18n-hint: when smoothing leads the output level to be momentarily
       * over the target */
      S.AddCheckBox(XO("O&vershoot"), settings.showOvershoot)
         ->Bind(wxEVT_CHECKBOX, [&](wxCommandEvent& evt) {
            OnCheckbox(
               evt.IsChecked(), GET_REF(showOvershoot),
               &HistPanel::ShowOvershoot);
         });
      /* i18n-hint: when smoothing leads the output level to be momentarily
       * under the target */
      S.AddCheckBox(XO("Under&shoot"), settings.showUndershoot)
         ->Bind(wxEVT_CHECKBOX, [&](wxCommandEvent& evt) {
            OnCheckbox(
               evt.IsChecked(), GET_REF(showUndershoot),
               &HistPanel::ShowUndershoot);
         });
   }
   S.EndHorizontalLay();

   S.AddSpace(0, borderSize);

   S.SetSizerProportion(1);
   S.StartMultiColumn(3, wxEXPAND);
   {
      S.SetStretchyCol(1);
      S.SetStretchyRow(0);
      S.AddSpace(borderSize, 0);
      S.Prop(1)
         .Position(wxALIGN_LEFT | wxALIGN_TOP | wxEXPAND)
         .MinSize({ HistPanel::minWidth, HistPanel::minHeight })
         .AddWindow(histPanel);

      S.Prop(1)
         .Position(wxEXPAND | wxALIGN_TOP)
         .MinSize({ rulerWidth, HistPanel::minHeight })
         .AddWindow(rulerPanel);
   }
   S.EndMultiColumn();

   S.AddSpace(0, borderSize);
}

void DynamicRangeProcessorEditor::PopulateLimiterUpperHalf(ShuttleGui& S)
{
   if (mIsRealtime)
   {
      S.StartMultiColumn(3, wxEXPAND);
      {
         S.SetStretchyCol(0);

         AddSliderPanel(S);
         S.AddSpace(borderSize, 0);
         AddCompressionMeterPanel(S);
      }
      S.EndMultiColumn();
   }
   else
      AddSliderPanel(S);
}

void DynamicRangeProcessorEditor::PopulateCompressorUpperHalf(
   ShuttleGui& S, const CompressorSettings& compressorSettings)
{
   if (mIsRealtime)
   {
      S.StartMultiColumn(4, wxEXPAND);
      {
         S.SetStretchyCol(0);

         AddSliderPanel(S);
         S.AddSpace(borderSize, 0);
         AddCompressionMeterPanel(S);
         AddCompressionCurvePanel(S, compressorSettings);
      }
      S.EndMultiColumn();
   }
   else
   {
      S.StartMultiColumn(2, wxEXPAND);
      {
         S.SetStretchyCol(0);
         AddSliderPanel(S);
         AddCompressionCurvePanel(S, compressorSettings);
      }
   }
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

   const auto firstSmoothingParameterIt =
      std::find_if(mParameters.begin(), mParameters.end(), [](const auto& p) {
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

void DynamicRangeProcessorEditor::AddCompressionMeterPanel(ShuttleGui& S)
{
   S.StartVerticalLay(0);
   {
      // Add vertical space above and below to align it with the slider
      // static boxes.
      S.AddSpace(0, 11);

      S.SetSizerProportion(1);
      S.StartMultiColumn(2, wxEXPAND);
      {
         S.SetStretchyCol(1);
         S.SetStretchyRow(0);

         constexpr auto height = 100;
         S.Prop(1)
            .Position(wxALIGN_LEFT | wxALIGN_TOP | wxEXPAND)
            .MinSize({ 30, height })
            .AddWindow(
               safenew CompressionMeterPanel(mUIParent, mCompressorInstance));

         S.Prop(1)
            .Position(wxEXPAND | wxALIGN_TOP)
            .MinSize({ 30, height })
            .AddWindow(MakeRulerPanel(
               mUIParent, wxVERTICAL,
               DynamicRangeProcessorPanel::compressorMeterRangeDb));
      }
      S.EndMultiColumn();

      S.AddSpace(0, 5);
   }
   S.EndVerticalLay();
}

void DynamicRangeProcessorEditor::AddTextboxAndSlider(
   ShuttleGui& S, ExtendedCompressorParameter& setting)
{
   setting.text = S.Validator<FloatingPointValidator<double>>(
                      1, &setting.value, NumValidatorStyle::ONE_TRAILING_ZERO,
                      setting.param->Min(), setting.param->Max())
                     .AddTextBox(setting.attributes.caption, L"", 12);

   setting.text->Bind(wxEVT_TEXT, [&](wxCommandEvent& evt) {
      if (!EnableApply(mUIParent, mUIParent->TransferDataFromWindow()))
         return;
      setting.slider->SetValue(TextToSliderValue(setting));
      ValidateUI();
      UpdateUI();
      Publish(EffectSettingChanged {});
   });

   setting.slider = S.Name(setting.attributes.caption)
                       .Style(wxSL_HORIZONTAL)
                       .MinSize({ 100, -1 })
                       .AddSlider(
                          {}, TextToSliderValue(setting), setting.param->Max(),
                          setting.param->Min());

   setting.slider->Bind(wxEVT_SLIDER, [&](wxCommandEvent& evt) {
      setting.value = SliderToTextValue(evt.GetInt(), setting);
      setting.text->GetValidator()->TransferToWindow();
      EnableApply(mUIParent, mUIParent->Validate());
      ValidateUI();
      UpdateUI();
      Publish(EffectSettingChanged {});
   });
}

bool DynamicRangeProcessorEditor::ValidateUI()
{
   mAccess.ModifySettings([this](EffectSettings& settings) {
      // pass back the modified settings to the MessageBuffer
      if (auto compressorSettings = settings.cast<CompressorSettings>())
         *compressorSettings = *GetCompressorSettings();
      else
         *settings.cast<LimiterSettings>() = *GetLimiterSettings();
      return nullptr;
   });
   return true;
}

bool DynamicRangeProcessorEditor::UpdateUI()
{
   if (CompressorSettings* compressorSettings = GetCompressorSettings())
      *compressorSettings = *mAccess.Get().cast<CompressorSettings>();
   else
      *GetLimiterSettings() = *mAccess.Get().cast<LimiterSettings>();

   for (auto& setting : mParameters)
      setting.slider->SetValue(TextToSliderValue(setting));

   if (
      auto tfPanel =
         wxWindow::FindWindowById(transferFunctionPanelId, mUIParent))
      tfPanel->Refresh();

   return true;
}

void DynamicRangeProcessorEditor::OnCheckbox(
   bool newVal, double& setting,
   void (DynamicRangeProcessorHistoryPanel::*setter)(bool))
{
   setting = newVal;
   if (
      const auto panel = dynamic_cast<HistPanel*>(
         wxWindow::FindWindowById(historyPanelId, mUIParent)))
      (panel->*setter)(newVal);
   ValidateUI();
   Publish(EffectSettingChanged {});
}
