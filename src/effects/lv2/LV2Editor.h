/**********************************************************************

  Audacity: A Digital Audio Editor

  @file LV2Editor.h
  @brief Event handling object for user interfaces of LV2 effects

  Paul Licameli split from LV2Effect.h

  Audacity(R) is copyright (c) 1999-2013 Audacity Team.
  License: GPL v2 or later.  See License.txt.

*********************************************************************/

#ifndef __AUDACITY_LV2_EDITOR__
#define __AUDACITY_LV2_EDITOR__

#if USE_LV2

#include "../EffectEditor.h"

#include <optional>
#include <vector>

#include <wx/timer.h>
#include <wx/window.h>
#include <wx/windowptr.h>

#include "LV2UIFeaturesList.h"
#include "LV2Ports.h"
#include "NativeWindow.h"

using SuilInstancePtr = Lilv_ptr<SuilInstance, suil_instance_free>;

// We use deprecated LV2 interfaces to remain compatible with older
// plug-ins, so disable warnings
LV2_DISABLE_DEPRECATION_WARNINGS

class wxCheckBox;
class wxChoice;
class wxSlider;
class wxTextCtrl;
class NumericTextCtrl;

class LV2EffectMeter;
class LV2Instance;
class LV2Wrapper;
class StatelessPerTrackEffect;

class LV2Editor final : public EffectEditor
   , public wxEvtHandler
   , LV2UIFeaturesList::UIHandler
{
public:
   LV2Editor(const StatelessPerTrackEffect &effect,
      const LilvPlugin &plug, LV2Instance &instance,
      EffectSettingsAccess &access, const EffectOutputs *pOutputs,
      double sampleRate,
      const LV2FeaturesList &features,
      const LV2Ports &ports, wxWindow *parent, bool useGUI);
   ~LV2Editor() override;

   bool ValidateUI() override;
   bool UpdateUI() override;
   bool IsGraphicalUI() override;
   void Disconnect() override;

   int ui_resize(int width, int height) override;
   void ui_closed() override;

#if defined(__WXGTK__)
   static void size_request(GtkWidget *widget, GtkRequisition *requisition,
      LV2Editor *pEditor);
   void SizeRequest(GtkWidget *widget, GtkRequisition *requisition);
#endif

   /*!
    @pre `pWrapper != nullptr`
    */
   bool BuildFancy(std::unique_ptr<LV2Wrapper> pWrapper,
      const EffectSettings &settings);
   bool BuildPlain(EffectSettingsAccess &access);

   void suil_port_write(uint32_t port_index,
      uint32_t buffer_size, uint32_t protocol, const void *buffer) override;
   uint32_t suil_port_index(const char *port_symbol) override;

   void UpdateControlPortValue(LV2EffectSettings& settings, size_t controlPortIndex, float value);

   void OnTrigger(wxCommandEvent & evt);
   void OnToggle(wxCommandEvent & evt);
   void OnChoice(wxCommandEvent & evt);
   void OnText(wxCommandEvent & evt);
   void OnSlider(wxCommandEvent & evt);

   void OnIdle(wxIdleEvent & evt);
   void OnSize(wxSizeEvent & evt);

   static std::shared_ptr<SuilHost> GetSuilHost();

   const LilvPlugin &mPlug;
   const EffectType mType;
   LV2Instance &mInstance;
   const EffectOutputs *mpOutputs{};
   const double mSampleRate;
   const LV2Ports &mPorts;
   std::unique_ptr<LV2Wrapper> mpWrapper;
   std::optional<const LV2UIFeaturesList> mUIFeatures;
   LV2PortUIStates mPortUIStates;

   std::shared_ptr<SuilHost> mSuilHost;
   wxWindow *mParent;
   bool mUseGUI{};

   // UI
   struct PlainUIControl {
      wxTextCtrl *mText{};
      //! Discriminate this union according to corresponding port's properties
      union {
         wxButton *button;
         wxCheckBox *checkbox;
         wxChoice *choice;
         LV2EffectMeter *meter;
         wxSlider *slider;
      };
   };
   //! Array in correspondence with the control ports
   std::vector<PlainUIControl> mPlainUIControls;
   void SetSlider(const LV2ControlPortState &state, const PlainUIControl &ctrl);

   // Two smart pointers are grouped because their destruction needs caution
   struct UI {
      void Destroy();
      ~UI() { Destroy(); }
      SuilInstancePtr mSuilInstance;
      wxWindowPtr<NativeWindow> mNativeWin{};
#ifdef __WXMAC__
      bool mJustLeakMemory{ false };
#endif
   } mUI;

   wxSize mNativeWinInitialSize{ wxDefaultSize };
   wxSize mNativeWinLastSize{ wxDefaultSize };
   bool mResizing{ false };
#if defined(__WXGTK__)
   bool mResized{ false };
#endif

   wxWeakRef<wxDialog> mDialog;
   bool mExternalUIClosed{ false };

   //! This must be destroyed before mSuilInstance
   struct Timer : wxTimer {
      LV2_External_UI_Widget* mExternalWidget{};
      void Notify() override;
   } mTimer;

   const LV2UI_Idle_Interface *mUIIdleInterface{};
   const LV2UI_Show_Interface *mUIShowInterface{};
   NumericTextCtrl *mDuration{};

   DECLARE_EVENT_TABLE()
};

#endif
#endif
