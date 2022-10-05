#pragma once

#include <pluginterfaces/base/smartpointer.h>

#include "EffectInterface.h"

class EffectBase;
class VST3Wrapper;

namespace Steinberg
{
   class IPlugView;
   class IPlugFrame;
}

class NumericTextCtrl;
class VST3ParametersWindow;

class VST3UIValidator
   : public wxEvtHandler
   , public EffectUIValidator
{
   VST3Wrapper& mWrapper;
   //Used if provided by the plugin and enabled in the settings
   Steinberg::IPtr<Steinberg::IPlugView> mPlugView;
   Steinberg::IPtr<Steinberg::IPlugFrame> mPlugFrame;
   wxWindow* mParent { nullptr };
   NumericTextCtrl* mDuration { nullptr };
   //Used if graphical plugin interface is disabled in the settings, or not provided by the plugin
   VST3ParametersWindow* mPlainUI { nullptr };
   
public:
   VST3UIValidator(wxWindow* parent, VST3Wrapper& wrapper, EffectBase &effect, EffectSettingsAccess &access, bool useNativeUI);
   ~VST3UIValidator() override;

   bool IsGraphicalUI() override;
   bool UpdateUI() override;
   bool ValidateUI() override;
   void OnClose() override;

private:
   void OnIdle(wxIdleEvent&);

   bool TryLoadNativeUI(wxWindow* parent);
   void OnEffectWindowResize(wxSizeEvent & evt);
};
