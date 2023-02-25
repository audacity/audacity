/**********************************************************************

  Audacity: A Digital Audio Editor

  VSTEditor.h

  Dominic Mazzoni

  Paul Licameli split from VSTEffect.h

**********************************************************************/
#if USE_VST

#ifndef __AUDACITY_VST_EDITOR__
#define __AUDACITY_VST_EDITOR__

#include "VSTWrapper.h"
#include "../EffectEditor.h"

#include <wx/weakref.h>
#include <wx/event.h>

class VSTInstance;
class wxSlider;
class wxStaticText;
class NumericTextCtrl;
class VSTControl;

class VSTTimer;

class VSTEditor final
   : public wxEvtHandler
   , public EffectEditor
   , public VSTUIWrapper
{
public:

   VSTEditor(VSTInstance&       instance, EffectType type, bool gui,
      const EffectUIServices&  services,
      EffectSettingsAccess&    access,
      wxWindow*                pParent,
      int                      numParams
   );

   ~VSTEditor() override;

   VSTInstance& GetInstance() const;

   bool ValidateUI() override;
   bool UpdateUI() override;

   void OnClose() override;

   void BuildPlain(EffectSettingsAccess& access, EffectType effectType, double projectRate);
   void BuildFancy(EffectInstance& instance);

   void OnTimer();

   std::unique_ptr<VSTTimer> mTimer;

   void RefreshParameters(int skip = -1) const;

   void Automate(int index, float value) override;

   void OnSlider(wxCommandEvent& evt);    

   int ShowDialog(bool nonModal);

   bool IsGraphicalUI() override;

   void Flush() override;

protected:
   void SizeWindow(int w, int h) override;

private:
   void NotifyParameterChanged(int index, float value);
   void OnIdle(wxIdleEvent &evt);

   VSTInstance& mInstance;
   const EffectType mType;
   const bool mGui;

   bool FetchSettingsFromInstance(EffectSettings& settings);
   bool StoreSettingsToInstance(const EffectSettings& settings);
   void NeedEditIdle(bool state);
   void NeedIdle() override;
   void Idle() override;

   void OnSizeWindow(wxCommandEvent& evt);

   int  mTimerGuard{ 0 };

   bool mWantsEditIdle{ false };
   bool mWantsIdle{ false };

   // Remembers last slider movements until idle time
   std::vector<std::pair<int, double>> mLastMovements{};

   ArrayOf<wxStaticText*> mNames;
   ArrayOf<wxSlider*> mSliders;
   ArrayOf<wxStaticText*> mDisplays;
   ArrayOf<wxStaticText*> mLabels;
   NumericTextCtrl* mDuration;

   wxWindow* mParent;
   wxWeakRef<wxDialog> mDialog;
   
   VSTControl* mControl{};

   // Mapping from parameter ID to string
   std::vector<wxString> mParamNames;

   int mNumParams{ 0 };
};

#endif

#endif // USE_VST
