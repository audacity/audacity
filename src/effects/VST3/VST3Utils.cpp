/**********************************************************************

  Audacity: A Digital Audio Editor

  @file VST3Utils.cpp

  @author Vitaly Sverchinsky

  @brief Part of Audacity VST3 module

**********************************************************************/

#include "VST3Utils.h"
#include "MemoryX.h"

#include <wx/string.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/spinctrl.h>
#include <wx/checkbox.h>
#include <wx/combobox.h>
#include <wx/slider.h>
#include <wx/scrolwin.h>

#include <pluginterfaces/vst/ivsteditcontroller.h>
#include <pluginterfaces/vst/ivstparameterchanges.h>

wxString VST3Utils::MakePluginPathString(const wxString& modulePath, const std::string& effectUIDString)
{
   return wxString::Format("%s;%s", modulePath, effectUIDString); 
}

bool VST3Utils::ParsePluginPath(const wxString& pluginPath, wxString* modulePath,
                                std::string* effectUIDString)
{
   const auto sep = pluginPath.Find(';', true);
   if(sep != wxNOT_FOUND &&
      //modulePath not empty
      sep > 0 &&
      //effectUIDString not empty
      static_cast<size_t>(sep) < pluginPath.Length() - 1)
   {
      if(modulePath != nullptr)
         *modulePath = pluginPath.Left(sep);
      if(effectUIDString != nullptr)
         *effectUIDString = pluginPath.Mid(static_cast<size_t>(sep) + 1);
      return true;
   }
   return false;
}

namespace
{

void FillParameters(
   wxWindow* parent,
   Steinberg::Vst::IEditController* editController,
   Steinberg::Vst::IComponentHandler* handler,
   const VST3Wrapper& wrapper)
{
   using namespace Steinberg;

   auto gridSizer = std::make_unique<wxGridSizer>(3, 5, 5);

   auto updateParameter = [controller = IPtr { editController }, handler = IPtr { handler }](Vst::ParamID id, Vst::ParamValue plainValue){
      const auto normalizedValue = controller->plainParamToNormalized(id, plainValue);
      controller->setParamNormalized(id, normalizedValue);
      if(handler->beginEdit(id) == kResultOk)
      {
         auto cleanup = finally([&]{ handler->endEdit(id); });
         handler->performEdit(id, normalizedValue);
      }
   };

   wrapper.ForEachParameter(

   [&](const Vst::ParameterInfo& parameterInfo)
   {
      
      if(parameterInfo.flags & (Vst::ParameterInfo::kIsHidden | Vst::ParameterInfo::kIsProgramChange))
         return true;

      if ((parameterInfo.flags & (Vst::ParameterInfo::kCanAutomate | Vst::ParameterInfo::kIsBypass | Vst::ParameterInfo::kIsReadOnly)) == 0)
         return true;

      gridSizer->Add(safenew wxStaticText(parent, wxID_ANY, VST3Utils::ToWxString(parameterInfo.title)));

      const auto currentNormalizedValue = editController->getParamNormalized(parameterInfo.id);

      if(parameterInfo.flags & Vst::ParameterInfo::kIsReadOnly)
      {
         Vst::String128 displayValue = { 0 };
         editController->getParamStringByValue(parameterInfo.id, parameterInfo.defaultNormalizedValue, displayValue);
         gridSizer->Add(safenew wxStaticText(parent, wxID_ANY, VST3Utils::ToWxString(displayValue)));
      }
      else if(parameterInfo.stepCount == 0)//continuous
      {
         const auto ctrlDouble = safenew wxSpinCtrlDouble(parent);
         ctrlDouble->SetRange(
            editController->normalizedParamToPlain(parameterInfo.id, 0),
            editController->normalizedParamToPlain(parameterInfo.id, 1)
         );
         ctrlDouble->SetIncrement((ctrlDouble->GetMax() - ctrlDouble->GetMin()) * 0.01);
         ctrlDouble->SetValue(editController->normalizedParamToPlain(parameterInfo.id, currentNormalizedValue));
         
         ctrlDouble->Bind(wxEVT_SPINCTRLDOUBLE, [updateParameter, id = parameterInfo.id](const wxSpinDoubleEvent& event) {
            updateParameter(id, event.GetValue());
         });
         gridSizer->Add(ctrlDouble, 0, wxEXPAND);
      }
      else if(parameterInfo.stepCount == 1)//toggle
      {
         const auto checkbox = safenew wxCheckBox (parent, wxID_ANY, wxEmptyString);
         checkbox->SetValue(currentNormalizedValue != .0);
         checkbox->Bind(wxEVT_CHECKBOX, [updateParameter, id = parameterInfo.id](const wxCommandEvent& event){
            const auto checkbox = static_cast<wxCheckBox*>(event.GetEventObject());
            updateParameter(id, checkbox->GetValue() ? 1. : .0);
         });
         gridSizer->Add(checkbox, 0, wxEXPAND);
      }
      else if(parameterInfo.stepCount > 0)//discrete range
      {
         const auto maxValue = parameterInfo.stepCount;
         if(parameterInfo.flags & Vst::ParameterInfo::kIsList)
         {
            const auto combo = safenew wxComboBox(parent, wxID_ANY);
            combo->SetEditable(false);

            for(auto j = 0; j <= maxValue; ++j)
            {
               Vst::String128 displayValue = { 0 };
               editController->getParamStringByValue(
                  parameterInfo.id,
                  editController->plainParamToNormalized(parameterInfo.id, j),
                  displayValue
               );
               combo->AppendString(VST3Utils::ToWxString(displayValue));
            }
            
            combo->SetSelection(
               static_cast<int>(editController->normalizedParamToPlain(parameterInfo.id, currentNormalizedValue))
            );
            combo->Bind(wxEVT_COMBOBOX, [updateParameter, id = parameterInfo.id](const wxCommandEvent& event) {
               const auto comboBox = static_cast<wxComboBox*>(event.GetEventObject());
               updateParameter(id, comboBox->GetSelection());
            });
            gridSizer->Add(combo, 0, wxEXPAND);
         }
         else
         {
            const auto slider = safenew wxSlider (
               parent,
               wxID_ANY,
               static_cast<int>(currentNormalizedValue * maxValue),
               0,
               maxValue
            );
            slider->SetValue(
               static_cast<int>(editController->normalizedParamToPlain(parameterInfo.id, currentNormalizedValue))
            );
            slider->Bind(wxEVT_SLIDER, [updateParameter, id = parameterInfo.id](wxCommandEvent& event){
               const auto slider = static_cast<wxSlider*>(event.GetEventObject());
               updateParameter(id, slider->GetValue());
            });
            gridSizer->Add(slider, 0, wxEXPAND);
         }
      }
      gridSizer->Add(safenew wxStaticText(parent, wxID_ANY, VST3Utils::ToWxString(parameterInfo.units)));

      return true;
   }
   );

   parent->SetSizer(gridSizer.release());
}

}

void VST3Utils::BuildPlainUI(
   wxWindow* parent,
   Steinberg::Vst::IEditController* editController,
   Steinberg::Vst::IComponentHandler* handler,
   const VST3Wrapper& wrapper )
{
   constexpr int WindowBorder { 5 };
   constexpr int WindowMaxHeight { 450 };

   auto scroll = safenew wxScrolledWindow(parent,
      wxID_ANY,
      wxDefaultPosition,
      wxDefaultSize,
      wxVSCROLL | wxTAB_TRAVERSAL);
   // This fools NVDA into not saying "Panel" when the dialog gets focus
   scroll->SetName(wxT("\a"));
   scroll->SetLabel(wxT("\a"));

   auto mainSizer = std::make_unique<wxBoxSizer>(wxVERTICAL);
   
   mainSizer->Add(scroll, 1, wxEXPAND | wxALL, WindowBorder);
   FillParameters(scroll, editController, handler, wrapper);

   auto minSize = scroll->GetSizer()->CalcMin() ;
   if(minSize.GetHeight() > (WindowMaxHeight - WindowBorder * 2))
   {
      minSize.SetHeight(WindowMaxHeight);
      scroll->SetScrollRate(0, 20);
   }
   else
      minSize.y += WindowBorder * 2;
   
   parent->SetMinSize(minSize);
   parent->SetSizer(mainSizer.release());
}

wxString VST3Utils::ToWxString(const Steinberg::Vst::TChar* str)
{
   static const wxCSConv csConv { wxFONTENCODING_UTF16 };
   return { reinterpret_cast<const char*>(str), csConv };
}

wxString VST3Utils::MakeAutomationParameterKey(const Steinberg::Vst::ParameterInfo& parameterInfo)
{
   auto suffix = ToWxString(parameterInfo.shortTitle);
   if(suffix.empty())
      suffix = ToWxString(parameterInfo.title);

   if(!suffix.empty())
      return wxString::Format("%lu_", static_cast<unsigned long>(parameterInfo.id)) + suffix;

   return wxString::Format("%lu", static_cast<unsigned long>(parameterInfo.id));
}

bool VST3Utils::ParseAutomationParameterKey(const wxString& key, Steinberg::Vst::ParamID& paramId)
{
   const auto pos = key.Find('_');
   const auto idStr = pos == wxNOT_FOUND ? key : key.Left(pos);
   unsigned long value { };
   if(idStr.ToULong(&value))
   {
      paramId = static_cast<Steinberg::Vst::ParamID>(value);
      return true;
   }
   return false;

}


const std::vector<VST3Wrapper::ParameterInfo>* VST3Wrapper::getParameterInfos() const
{
   if (mParameterInfos.size() == 0)
   {
      if (mEditController == nullptr)
         return nullptr;

      for (int i = 0, count = mEditController->getParameterCount(); i < count; ++i)
      {
         Steinberg::Vst::ParameterInfo vstParameterInfo{ };
         if (mEditController->getParameterInfo(i, vstParameterInfo) == Steinberg::kResultOk)
         {
            // Add all parameterinfos, regardless of whether they can be automated or not,
            // because different flags of them will be checked by different methods.
            mParameterInfos.push_back(vstParameterInfo);
         }
      }
   }

   return &mParameterInfos;
}


bool VST3Wrapper::ForEachParameter(ParameterVisitor visitor) const
{
   const auto pParameterInfos = getParameterInfos();
   if (pParameterInfos == nullptr)
      return false;
 
   for (const auto& parameterInfo : *pParameterInfos)
   {
      if ( ! visitor(parameterInfo) )
         break;
   }

   return true;
}


bool VST3Wrapper::AtLeastOne(ParameterVisitor visitor) const
{
   const auto pParameterInfos = getParameterInfos();
   if (pParameterInfos == nullptr)
      return false;

   for (const auto& parameterInfo : *pParameterInfos)
   {
      if (visitor(parameterInfo))
      {
         return true;
      }
   }

   return false;
}


