// clang-format off
/**********************************************************************

   Audacity: A Digital Audio Editor

   EffectWidget.h

   Matthieu Hodgkinson

   The `EffectWidget` is a variant that aims at covering the most common widgets
   found in effects UIs, together with other attributes such as name and
   enablement state.

   Resources that have motivated some of these widgets and their specifics (e.g. whether there is a min/max or not) include:
   * Audacity's Nyquist documentation for developers
     https://plugins.audacityteam.org/contributing/developing-your-own-plugins-and-scripts/creating-your-own-nyquist-plugins
   * The `VampPluginDescriptor` struct of vamp.h
   * The various Audacity built-in effects.

**********************************************************************/
// clang-format on

#pragma once

#include <optional>
#include <string>
#include <variant>
#include <vector>

enum class WidgetType
{
   Toggle,
   Choice,
   ChooseFile,
   DblText,
   DblSlider,
   IntText,
   IntSlider,
   NumericText, // As seen in e.g. Audacity's selection toolbar
   EditableText,
   InfoText,
   Unknown
};

template <enum WidgetType> struct WidgetValue;

template <> struct WidgetValue<WidgetType::Toggle>
{
   const bool value;
};

template <> struct WidgetValue<WidgetType::Choice>
{
   const std::vector<std::string> choices;
   const int index;
};

template <> struct WidgetValue<WidgetType::ChooseFile>
{
   const std::string path;
};

template <> struct WidgetValue<WidgetType::DblText>
{
   const std::optional<double> min;
   const std::optional<double> max;
   const double value;
};

template <> struct WidgetValue<WidgetType::DblSlider>
{
   const double min;
   const double max;
   const double value;
   const std::optional<int> numSteps;
};

template <> struct WidgetValue<WidgetType::IntText>
{
   const std::optional<int> min;
   const std::optional<int> max;
   const int value;
};

template <> struct WidgetValue<WidgetType::IntSlider>
{
   const int min;
   const int max;
   const int value;
};

template <> struct WidgetValue<WidgetType::NumericText>
{
   const double value;
};

template <> struct WidgetValue<WidgetType::EditableText>
{
   const std::string value;
};

template <> struct WidgetValue<WidgetType::InfoText>
{
   const std::string value;
};

using ToggleVal = WidgetValue<WidgetType::Toggle>;
using ChoiceVal = WidgetValue<WidgetType::Choice>;
using ChooseFileVal = WidgetValue<WidgetType::ChooseFile>;
using DblTextVal = WidgetValue<WidgetType::DblText>;
using DblSliderVal = WidgetValue<WidgetType::DblSlider>;
using IntTextVal = WidgetValue<WidgetType::IntText>;
using IntSliderVal = WidgetValue<WidgetType::IntSlider>;
using NumericTextVal = WidgetValue<WidgetType::NumericText>;
using EditableTextVal = WidgetValue<WidgetType::EditableText>;
using InfoTextVal = WidgetValue<WidgetType::InfoText>;

class EFFECTS_API EffectWidget
{
public:
   using Value = std::variant<
      ToggleVal,       //
      ChoiceVal,       //
      ChooseFileVal,   //
      DblTextVal,      //
      DblSliderVal,    //
      IntTextVal,      //
      IntSliderVal,    //
      NumericTextVal,  //
      EditableTextVal, //
      InfoTextVal      //
      >;

   EffectWidget(std::string name, Value, bool enabled, std::string tip = "");

   // Invariants: not expected to change during application lifetime.
   const std::string name; // May contain mnemonic.
   const std::string tip;

   // State of the widget.
   const Value value;
   const bool enabled;

   WidgetType GetWidgetType() const;

   const ToggleVal* AsToggle() const;
   const ChoiceVal* AsChoice() const;
   const ChooseFileVal* AsChooseFile() const;
   const DblTextVal* AsDblText() const;
   const DblSliderVal* AsDblTextWithSlider() const;
   const IntTextVal* AsIntText() const;
   const IntSliderVal* AsIntTextWithSlider() const;
   const NumericTextVal* AsNumericText() const;
   const EditableTextVal* AsEditableText() const;
   const InfoTextVal* AsInfoText() const;
};
