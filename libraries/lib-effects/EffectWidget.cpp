/**********************************************************************

   Audacity: A Digital Audio Editor

   EffectWidget.cpp

   Matthieu Hodgkinson

**********************************************************************/
#include "EffectWidget.h"
#include <cassert>

EffectWidget::EffectWidget(
   std::string name, Value value, bool enabled, std::string tip)
    : name { std::move(name) }
    , tip { std::move(tip) }
    , value { std::move(value) }
    , enabled { std::move(enabled) }
{
}

WidgetType EffectWidget::GetWidgetType() const
{
   if (std::holds_alternative<ToggleVal>(value))
      return WidgetType::Toggle;
   else if (std::holds_alternative<ChoiceVal>(value))
      return WidgetType::Choice;
   else if (std::holds_alternative<ChooseFileVal>(value))
      return WidgetType::ChooseFile;
   else if (std::holds_alternative<DblTextVal>(value))
      return WidgetType::DblText;
   else if (std::holds_alternative<DblSliderVal>(value))
      return WidgetType::DblSlider;
   else if (std::holds_alternative<IntTextVal>(value))
      return WidgetType::IntText;
   else if (std::holds_alternative<IntSliderVal>(value))
      return WidgetType::IntSlider;
   else if (std::holds_alternative<NumericTextVal>(value))
      return WidgetType::NumericText;
   else if (std::holds_alternative<EditableTextVal>(value))
      return WidgetType::EditableText;
   else if (std::holds_alternative<InfoTextVal>(value))
      return WidgetType::InfoText;
   
   assert(false);
   return WidgetType::Unknown;
}

const ToggleVal* EffectWidget::AsToggle() const
{
   if (GetWidgetType() != WidgetType::Toggle)
      return nullptr;
   return &std::get<ToggleVal>(value);
}

const ChoiceVal* EffectWidget::AsChoice() const
{
   if (GetWidgetType() != WidgetType::Choice)
      return nullptr;
   return &std::get<ChoiceVal>(value);
}

const ChooseFileVal* EffectWidget::AsChooseFile() const
{
   if (GetWidgetType() != WidgetType::ChooseFile)
      return nullptr;
   return &std::get<ChooseFileVal>(value);
}

const DblTextVal* EffectWidget::AsDblText() const
{
   if (GetWidgetType() != WidgetType::DblText)
      return nullptr;
   return &std::get<DblTextVal>(value);
}

const DblSliderVal* EffectWidget::AsDblTextWithSlider() const
{
   if (GetWidgetType() != WidgetType::DblSlider)
      return nullptr;
   return &std::get<DblSliderVal>(value);
}

const IntTextVal* EffectWidget::AsIntText() const
{
   if (GetWidgetType() != WidgetType::IntText)
      return nullptr;
   return &std::get<IntTextVal>(value);
}

const IntSliderVal* EffectWidget::AsIntTextWithSlider() const
{
   if (GetWidgetType() != WidgetType::IntSlider)
      return nullptr;
   return &std::get<IntSliderVal>(value);
}

const NumericTextVal* EffectWidget::AsNumericText() const
{
   if (GetWidgetType() != WidgetType::NumericText)
      return nullptr;
   return &std::get<NumericTextVal>(value);
}

const EditableTextVal* EffectWidget::AsEditableText() const
{
   if (GetWidgetType() != WidgetType::EditableText)
      return nullptr;
   return &std::get<EditableTextVal>(value);
}

const InfoTextVal* EffectWidget::AsInfoText() const
{
   if (GetWidgetType() != WidgetType::InfoText)
      return nullptr;
   return &std::get<InfoTextVal>(value);
}
