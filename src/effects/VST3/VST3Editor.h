#pragma once

#include <pluginterfaces/base/smartpointer.h>

#include "../EffectEditor.h"

class StatelessPerTrackEffect;
class VST3Wrapper;

namespace Steinberg
{
   class IPlugView;
   class IPlugFrame;
}

class NumericTextCtrl;
class VST3ParametersWindow;

class VST3Editor
   : public wxEvtHandler
   , public EffectEditor
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
   VST3Editor(wxWindow* parent, VST3Wrapper& wrapper,
      const StatelessPerTrackEffect &effect, EffectSettingsAccess &access,
      bool useNativeUI);
   ~VST3Editor() override;

   bool IsGraphicalUI() override;
   bool UpdateUI() override;
   bool ValidateUI() override;
   void OnClose() override;

private:
   void OnIdle(wxIdleEvent&);

   bool TryLoadNativeUI(wxWindow* parent);
};
