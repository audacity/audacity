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
#include <wx/choice.h>
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

   class VST3ParametersWindow : public wxScrolledWindow
   {
      Steinberg::IPtr<Steinberg::Vst::IEditController> mEditController;
      Steinberg::IPtr<Steinberg::Vst::IComponentHandler> mComponentHandler;
   public:
      VST3ParametersWindow(wxWindow *parent,
                         wxWindowID id = wxID_ANY,
                         const wxPoint& pos = wxDefaultPosition,
                         const wxSize& size = wxDefaultSize,
                         long style = wxScrolledWindowStyle,
                         const wxString& name = wxPanelNameStr)
                            : wxScrolledWindow(parent, id, pos, size, style, name)
      {
      }

      void Fill(Steinberg::Vst::IEditController& editController, Steinberg::Vst::IComponentHandler& handler)
      {
         using namespace Steinberg;

         DestroyChildren();

         mEditController = &editController;
         mComponentHandler = &handler;

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

            sizer->Add(safenew wxStaticText(this, wxID_ANY, VST3Utils::ToWxString(parameterInfo.title)));

            const auto currentNormalizedValue = editController.getParamNormalized(parameterInfo.id);

            if(parameterInfo.flags & Vst::ParameterInfo::kIsReadOnly)
            {
               Vst::String128 displayValue = { 0 };
               editController.getParamStringByValue(parameterInfo.id, parameterInfo.defaultNormalizedValue, displayValue);
               sizer->Add(safenew wxStaticText(this, wxID_ANY, VST3Utils::ToWxString(displayValue)));
            }
            else if(parameterInfo.stepCount == 0)//continuous
            {
               const auto ctrlDouble = safenew wxSpinCtrlDouble(this);
               ctrlDouble->SetRange(
                  editController.normalizedParamToPlain(parameterInfo.id, 0),
                  editController.normalizedParamToPlain(parameterInfo.id, 1)
               );
               ctrlDouble->SetIncrement((ctrlDouble->GetMax() - ctrlDouble->GetMin()) * 0.01);
               ctrlDouble->SetValue(editController.normalizedParamToPlain(parameterInfo.id, currentNormalizedValue));
               
               ctrlDouble->Bind(wxEVT_SPINCTRLDOUBLE, [this, id = parameterInfo.id](const wxSpinDoubleEvent& event) {
                  UpdateParameter(id, event.GetValue());
               });
               sizer->Add(ctrlDouble, 0, wxEXPAND);
            }
            else if(parameterInfo.stepCount == 1)//toggle
            {
               const auto checkbox = safenew wxCheckBox (this, wxID_ANY, wxEmptyString);
               checkbox->SetValue(currentNormalizedValue != .0);
               checkbox->Bind(wxEVT_CHECKBOX, [this, id = parameterInfo.id](const wxCommandEvent& event){
                  const auto checkbox = static_cast<wxCheckBox*>(event.GetEventObject());
                  UpdateParameter(id, checkbox->GetValue() ? 1. : .0);
               });
               sizer->Add(checkbox, 0, wxEXPAND);
            }
            else if(parameterInfo.stepCount > 0)//discrete range
            {
               const auto maxValue = parameterInfo.stepCount;
               if(parameterInfo.flags & Vst::ParameterInfo::kIsList)
               {
                  const auto choice = safenew wxChoice(this, wxID_ANY);

                  for(auto j = 0; j <= maxValue; ++j)
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
                     const auto choice = static_cast<wxChoice*>(event.GetEventObject());
                     UpdateParameter(id, choice->GetSelection());
                  });
                  sizer->Add(choice, 0, wxEXPAND);
               }
               else
               {
                  const auto slider = safenew wxSlider (
                     this,
                     wxID_ANY,
                     static_cast<int>(currentNormalizedValue * maxValue),
                     0,
                     maxValue
                  );
                  slider->SetValue(
                     static_cast<int>(editController.normalizedParamToPlain(parameterInfo.id, currentNormalizedValue))
                  );
                  slider->Bind(wxEVT_SLIDER, [this, id = parameterInfo.id](wxCommandEvent& event){
                     const auto slider = static_cast<wxSlider*>(event.GetEventObject());
                     UpdateParameter(id, slider->GetValue());
                  });
                  sizer->Add(slider, 0, wxEXPAND);
               }
            }
            sizer->Add(safenew wxStaticText(this, wxID_ANY, VST3Utils::ToWxString(parameterInfo.units)));
         }

         SetSizer(sizer.release());
      }

   private:

      void UpdateParameter(Steinberg::Vst::ParamID id, Steinberg::Vst::ParamValue plainValue)
      {
         const auto normalizedValue = mEditController->plainParamToNormalized(id, plainValue);
         mEditController->setParamNormalized(id, normalizedValue);
         if(mComponentHandler->beginEdit(id) == Steinberg::kResultOk)
         {
            auto cleanup = finally([this, id] { mComponentHandler->endEdit(id); });
            mComponentHandler->performEdit(id, normalizedValue);
         }
      }

   };
}

void VST3Utils::BuildPlainUI(
   wxWindow* parent,
   Steinberg::Vst::IEditController* editController,
   Steinberg::Vst::IComponentHandler* handler)
{
   constexpr int WindowBorder { 5 };
   constexpr int WindowMaxHeight { 450 };

   auto parametersWindow = safenew VST3ParametersWindow(parent,
      wxID_ANY,
      wxDefaultPosition,
      wxDefaultSize,
      wxVSCROLL | wxTAB_TRAVERSAL);
   // This fools NVDA into not saying "Panel" when the dialog gets focus
   parametersWindow->SetName(wxT("\a"));
   parametersWindow->SetLabel(wxT("\a"));

   auto mainSizer = std::make_unique<wxBoxSizer>(wxVERTICAL);
   
   mainSizer->Add(parametersWindow, 1, wxEXPAND | wxALL, WindowBorder);
   parametersWindow->Fill(*editController, *handler);

   auto minSize = parametersWindow->GetSizer()->CalcMin();
   if(minSize.GetHeight() > (WindowMaxHeight - WindowBorder * 2))
   {
      minSize.SetHeight(WindowMaxHeight);
      parametersWindow->SetScrollRate(0, 20);
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
