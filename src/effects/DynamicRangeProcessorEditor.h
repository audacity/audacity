#pragma once

#include "../widgets/valnum.h"
#include "EffectEditor.h"
#include "EffectInterface.h"
#include "SettingsVisitor.h"
#include "ShuttleGui.h"
#include "processors/CompressorProcessor.h"
#include <optional>
#include <wx/slider.h>
#include <wx/textctrl.h>
#include <wx/weakref.h>

template <typename EditorType, typename SettingType>
class DynamicRangeProcessorEditor : public EffectEditor
{
public:
   DynamicRangeProcessorEditor(
      const EffectUIServices& services, EffectSettingsAccess& access,
      const SettingType& settings)
       : EffectEditor { services, access }
       , mSettings { settings }
   {
   }

   virtual ~DynamicRangeProcessorEditor() = default;

   using Parameter = EffectParameter<SettingType, double, double>;

protected:
   struct Control
   {
      const Parameter& param;
      double& value;
      wxTextCtrl* text = nullptr;
      wxSlider* slider = nullptr;
   };

   Control mThresholdDbCtrl { EditorType::thresholdDb, mSettings.thresholdDb };
   Control mKneeDbCtrl { EditorType::kneeDb, mSettings.kneeDb };
   Control mLookaheadMsCtrl { EditorType::lookaheadMs, mSettings.lookaheadMs };
   Control mReleaseMsCtrl { EditorType::releaseMs, mSettings.releaseMs };
   Control mMakeUpCtrl { EditorType::makeUpDb, mSettings.makeUpDb };

   bool ValidateUI() override;
   bool UpdateUI() override;

   void AddTextboxAndSlider(
      ShuttleGui& S, Control& ctrl, const TranslatableString& textCaption,
      const TranslatableString& sliderCaption,
      std::function<void(wxCommandEvent&)> onSlider = {},
      std::optional<double> alternativeMax = std::nullopt);

   void OnParameterSlider(
      wxCommandEvent& evt, wxTextCtrl& textCtrl, double& setting,
      const Parameter& param,
      const std::optional<double>& alternativeMax = std::nullopt);

   static SettingType& GetSettings(EffectSettings& settings)
   {
      auto pSettings = settings.cast<SettingType>();
      assert(pSettings);
      return *pSettings;
   }

   static const SettingType& GetSettings(const EffectSettings& settings)
   {
      auto pSettings = settings.cast<SettingType>();
      assert(pSettings);
      return *pSettings;
   }

   SettingType mSettings;
   wxWeakRef<wxWindow> mUIParent;
};

template <typename EditorType, typename SettingType>
void DynamicRangeProcessorEditor<EditorType, SettingType>::AddTextboxAndSlider(
   ShuttleGui& S, Control& ctrl, const TranslatableString& textCaption,
   const TranslatableString& sliderCaption,
   std::function<void(wxCommandEvent&)> onSlider,
   std::optional<double> alternativeMax)
{
   ctrl.text = S.Validator<FloatingPointValidator<double>>(
                   1, &ctrl.value, NumValidatorStyle::ONE_TRAILING_ZERO,
                   ctrl.param.min, alternativeMax.value_or(ctrl.param.max))
                  .AddTextBox(textCaption, L"", 12);

   ctrl.text->Bind(wxEVT_TEXT, [&](wxCommandEvent& evt) {
      if (!EnableApply(mUIParent, mUIParent->TransferDataFromWindow()))
         return;
      ctrl.slider->SetValue((int)(ctrl.value * ctrl.param.scale));
      ValidateUI();
      Publish(EffectSettingChanged {});
   });

   ctrl.slider = S.Name(sliderCaption)
                    .Style(wxSL_HORIZONTAL)
                    .MinSize({ 100, -1 })
                    .AddSlider(
                       {}, ctrl.param.def * ctrl.param.scale,
                       ctrl.param.max * ctrl.param.scale,
                       ctrl.param.min * ctrl.param.scale);

   ctrl.slider->Bind(wxEVT_SLIDER, [&, onSlider](wxCommandEvent& evt) {
      onSlider ? onSlider(evt) :
                 OnParameterSlider(evt, *ctrl.text, ctrl.value, ctrl.param);
      ctrl.text->GetValidator()->TransferToWindow();
      EnableApply(mUIParent, mUIParent->Validate());
      ValidateUI();
      Publish(EffectSettingChanged {});
   });
}

template <typename EditorType, typename SettingType>
void DynamicRangeProcessorEditor<EditorType, SettingType>::OnParameterSlider(
   wxCommandEvent& evt, wxTextCtrl& textCtrl, double& setting,
   const Parameter& param, const std::optional<double>& alternativeMax)
{
   auto& ms = mSettings;
   setting = evt.GetInt() / param.scale;
   if (alternativeMax.has_value() && setting == param.max)
      setting = *alternativeMax;
}

template <typename EditorType, typename SettingType>
bool DynamicRangeProcessorEditor<EditorType, SettingType>::ValidateUI()
{
   mAccess.ModifySettings([this](EffectSettings& settings) {
      // pass back the modified settings to the MessageBuffer
      GetSettings(settings) = mSettings;
      return nullptr;
   });
   return true;
}

template <typename EditorType, typename SettingType>
bool DynamicRangeProcessorEditor<EditorType, SettingType>::UpdateUI()
{
   // get the settings from the MessageBuffer and write them to our local
   // copy
   const auto& settings = mAccess.Get();
   mSettings = GetSettings(settings);
   auto& ms = mSettings;
   mThresholdDbCtrl.slider->SetValue(
      (int)(ms.thresholdDb * EditorType::thresholdDb.scale));
   mKneeDbCtrl.slider->SetValue((int)(ms.kneeDb * EditorType::kneeDb.scale));
   mLookaheadMsCtrl.slider->SetValue(
      (int)(ms.lookaheadMs * EditorType::lookaheadMs.scale));
   mReleaseMsCtrl.slider->SetValue(
      (int)(ms.releaseMs * EditorType::releaseMs.scale));
   mMakeUpCtrl.slider->SetValue(
      (int)(ms.makeUpDb * EditorType::makeUpDb.scale));
   return true;
}
