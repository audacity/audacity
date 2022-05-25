/**********************************************************************

  Audacity: A Digital Audio Editor

  @file VST3ParametersWindow.cpp

  @author Vitaly Sverchinsky

  @brief Part of Audacity VST3 module

**********************************************************************/


#include "VST3ParametersWindow.h"

#include <pluginterfaces/vst/ivsteditcontroller.h>

#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/combobox.h>
#include <wx/slider.h>

#include "MemoryX.h"
#include "VST3Utils.h"

VST3ParametersWindow::VST3ParametersWindow(wxWindow *parent,
                                           Steinberg::Vst::IEditController& editController,
                                           Steinberg::Vst::IComponentHandler& handler,
                                           wxWindowID id,
                                           const wxPoint& pos,
                                           const wxSize& size,
                                           long style,
                                           const wxString& name)
   : wxScrolledWindow(parent, id, pos, size, style, name)
   , mEditController(&editController)
   , mComponentHandler(&handler)
{
   using namespace Steinberg;

   auto sizer = std::make_unique<wxGridSizer>(3, 5, 5);

   for(int i = 0, count = editController.getParameterCount(); i < count; ++i)
   {
      Vst::ParameterInfo parameterInfo { };
      if(editController.getParameterInfo(i, parameterInfo) != kResultOk)
         continue;

      if(parameterInfo.flags & (Vst::ParameterInfo::kIsHidden | Vst::ParameterInfo::kIsProgramChange))
         continue;
      
      if((parameterInfo.flags & (Vst::ParameterInfo::kCanAutomate | Vst::ParameterInfo::kIsBypass | Vst::ParameterInfo::kIsReadOnly)) == 0)
         continue;

      sizer->Add(safenew wxStaticText(
         this,
         wxID_ANY,
         VST3Utils::ToWxString(parameterInfo.title),
         wxDefaultPosition,
         wxDefaultSize,
         wxALIGN_RIGHT), 0, wxEXPAND
      );

      const auto currentNormalizedValue = editController.getParamNormalized(parameterInfo.id);

      if(parameterInfo.flags & Vst::ParameterInfo::kIsReadOnly)
      {
         Vst::String128 displayValue = { 0 };
         editController.getParamStringByValue(parameterInfo.id, parameterInfo.defaultNormalizedValue, displayValue);
         sizer->Add(safenew wxStaticText(this, wxID_ANY, VST3Utils::ToWxString(displayValue)));
         sizer->AddStretchSpacer();
      }
      //toggle
      else if(parameterInfo.stepCount == 1)
      {
         const auto checkbox = safenew wxCheckBox (this, wxID_ANY, wxEmptyString);
         checkbox->SetValue(currentNormalizedValue != .0);
         checkbox->Bind(wxEVT_CHECKBOX, [this, id = parameterInfo.id](const wxCommandEvent& event){
            const auto checkbox = static_cast<wxCheckBox*>(event.GetEventObject());
            UpdateParameter(id, checkbox->GetValue() ? 1. : .0);
         });
         sizer->Add(checkbox, 0, wxEXPAND);
         sizer->AddStretchSpacer();
      }
      //list
      else if(parameterInfo.stepCount != 0 && (parameterInfo.flags & Vst::ParameterInfo::kIsList))
      {
         const auto choice = safenew wxChoice(this, wxID_ANY);

         for(auto j = 0; j <= parameterInfo.stepCount; ++j)
         {
            Vst::String128 displayValue = { 0 };
            editController.getParamStringByValue(
               parameterInfo.id,
               editController.plainParamToNormalized(parameterInfo.id, j),
               displayValue
            );
            choice->AppendString(VST3Utils::ToWxString(displayValue));
         }
         
         choice->SetSelection(
            static_cast<int>(editController.normalizedParamToPlain(parameterInfo.id, currentNormalizedValue))
         );
         choice->Bind(wxEVT_CHOICE, [this, id = parameterInfo.id](const wxCommandEvent& event) {
            const auto comboBox = static_cast<wxComboBox*>(event.GetEventObject());
            UpdateParameter(id, mEditController->plainParamToNormalized(id, comboBox->GetSelection()));
         });
         sizer->Add(choice, 0, wxEXPAND);
         sizer->AddStretchSpacer();
      }
      //continuous
      else if(parameterInfo.stepCount == 0)
      {
         static constexpr auto MaxValue = 100;
         const auto slider = safenew wxSlider(
            this,
            wxID_ANY,
            static_cast<int>(currentNormalizedValue * MaxValue),
            0,
            MaxValue);
         const auto unitsStr = VST3Utils::ToWxString(parameterInfo.units);
         const auto valueText = safenew wxStaticText(this, wxID_ANY, wxEmptyString);
         UpdateParameterValueText(valueText, parameterInfo.id, currentNormalizedValue, unitsStr);

         slider->Bind(wxEVT_SLIDER, [this, id = parameterInfo.id, valueText, unitsStr](wxCommandEvent& event) {
            const auto slider = static_cast<wxSlider*>(event.GetEventObject());
            const auto value = static_cast<Vst::ParamValue>(slider->GetValue()) / static_cast<Vst::ParamValue>(MaxValue);
            UpdateParameter(id, value);
            UpdateParameterValueText(valueText, id, value, unitsStr);
         });
         sizer->Add(slider, 0, wxEXPAND);
         sizer->Add(valueText);
      }
      //discrete
      else
      {
         const auto slider = safenew wxSlider (
            this,
            wxID_ANY,
            static_cast<int>(editController.normalizedParamToPlain(parameterInfo.id, currentNormalizedValue)),
            0,
            parameterInfo.stepCount
         );
         const auto unitsStr = VST3Utils::ToWxString(parameterInfo.units);
         const auto valueText = safenew wxStaticText(this, wxID_ANY, wxEmptyString);
         UpdateParameterValueText(valueText, parameterInfo.id, currentNormalizedValue, unitsStr);

         slider->Bind(wxEVT_SLIDER, [this, id = parameterInfo.id, valueText, unitsStr](wxCommandEvent& event){
            const auto slider = static_cast<wxSlider*>(event.GetEventObject());
            UpdateParameter(id, mEditController->plainParamToNormalized(id, slider->GetValue()));
            UpdateParameterValueText(valueText, id, mEditController->getParamNormalized(id), unitsStr);
         });
         sizer->Add(slider, 0, wxEXPAND);
         sizer->Add(valueText);
      }
   }

   SetSizer(sizer.release());
}

void VST3ParametersWindow::UpdateParameterValueText(wxStaticText* text, Steinberg::Vst::ParamID id, Steinberg::Vst::ParamValue normalizedValue, const wxString& units)
{
   Steinberg::Vst::String128 str {};
   Steinberg::Vst::ParameterInfo parameterInfo { };

   mEditController->getParamStringByValue(id, normalizedValue, str);
   if(units.empty())
      text->SetLabel(VST3Utils::ToWxString(str));
   else
      text->SetLabel(wxString::Format("%s %s", VST3Utils::ToWxString(str), units));
}

void VST3ParametersWindow::UpdateParameter(Steinberg::Vst::ParamID id, Steinberg::Vst::ParamValue normalizedValue)
{
   mEditController->setParamNormalized(id, normalizedValue);
   if(mComponentHandler->beginEdit(id) == Steinberg::kResultOk)
   {
      auto cleanup = finally([this, id] { mComponentHandler->endEdit(id); });
      mComponentHandler->performEdit(id, normalizedValue);
   }
}

VST3ParametersWindow* VST3ParametersWindow::Setup(wxWindow& parent, Steinberg::Vst::IEditController& editController,
   Steinberg::Vst::IComponentHandler& componentHandler)
{
   constexpr int WindowBorder { 5 };
   constexpr int WindowMaxHeight { 450 };

   auto parametersWindow = safenew VST3ParametersWindow(&parent,
      editController,
      componentHandler,
      wxID_ANY,
      wxDefaultPosition,
      wxDefaultSize,
      wxVSCROLL | wxTAB_TRAVERSAL);
   // This fools NVDA into not saying "Panel" when the dialog gets focus
   parametersWindow->SetName(wxT("\a"));
   parametersWindow->SetLabel(wxT("\a"));

   auto mainSizer = std::make_unique<wxBoxSizer>(wxVERTICAL);
   
   mainSizer->Add(parametersWindow, 1, wxEXPAND | wxALL, WindowBorder);

   auto minSize = parametersWindow->GetSizer()->CalcMin();
   if(minSize.GetHeight() > (WindowMaxHeight - WindowBorder * 2))
   {
      minSize.SetHeight(WindowMaxHeight);
      parametersWindow->SetScrollRate(0, 20);
   }
   else
      minSize.y += WindowBorder * 2;
   
   parent.SetMinSize(minSize);
   parent.SetSizer(mainSizer.release());

   return parametersWindow;
}
