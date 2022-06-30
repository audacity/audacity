/**********************************************************************

  Audacity: A Digital Audio Editor

  LV2Effect.h

  Audacity(R) is copyright (c) 1999-2013 Audacity Team.
  License: GPL v2 or later.  See License.txt.

*********************************************************************/

#ifndef __AUDACITY_LV2_EFFECT__
#define __AUDACITY_LV2_EFFECT__


#if USE_LV2

class wxArrayString;

#include <vector>

#include <wx/event.h> // to inherit
#include <wx/timer.h>
#include <wx/weakref.h>

#include "lv2/data-access/data-access.h"
#include <suil/suil.h>
#include "lv2_external_ui.h"

#include "LV2FeaturesList.h"
#include "LV2Ports.h"
#include "../../ShuttleGui.h"
#include "SampleFormat.h"

#include "NativeWindow.h"

using LilvInstancePtr = Lilv_ptr<LilvInstance, lilv_instance_free>;
using LilvUIsPtr = Lilv_ptr<LilvUIs, lilv_uis_free>;
using SuilHostPtr = Lilv_ptr<SuilHost, suil_host_free>;
using SuilInstancePtr = Lilv_ptr<SuilInstance, suil_instance_free>;

// We use deprecated LV2 interfaces to remain compatible with older
// plug-ins, so disable warnings
LV2_DISABLE_DEPRECATION_WARNINGS

class wxSlider;
class wxTextCtrl;
class NumericTextCtrl;

#define LV2EFFECTS_VERSION wxT("1.0.0.0")
/* i18n-hint: abbreviates
   "Linux Audio Developer's Simple Plugin API (LADSPA) version 2" */
#define LV2EFFECTS_FAMILY XO("LV2")

// DECLARE_LOCAL_EVENT_TYPE(EVT_SIZEWINDOW, -1);

class LV2EffectMeter;
class LV2Wrapper;

class LV2Effect final : public LV2FeaturesList
{
public:
   LV2Effect(const LilvPlugin &plug);
   virtual ~LV2Effect();

   // ComponentInterface implementation

   PluginPath GetPath() const override;
   ComponentInterfaceSymbol GetSymbol() const override;
   VendorSymbol GetVendor() const override;
   wxString GetVersion() const override;
   TranslatableString GetDescription() const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;
   EffectFamilySymbol GetFamily() const override;
   bool IsInteractive() const override;
   bool IsDefault() const override;
   RealtimeSince RealtimeSupport() const override;
   bool SupportsAutomation() const override;

   bool SaveSettings(
      const EffectSettings &settings, CommandParameters & parms) const override;
   bool LoadSettings(
      const CommandParameters & parms, EffectSettings &settings) const override;

   bool LoadUserPreset(
      const RegistryPath & name, EffectSettings &settings) const override;
   bool SaveUserPreset(
      const RegistryPath & name, const EffectSettings &settings) const override;

   RegistryPaths GetFactoryPresets() const override;
   bool LoadFactoryPreset(int id, EffectSettings &settings) const override;

   unsigned GetAudioInCount() const override;
   unsigned GetAudioOutCount() const override;

   int GetMidiInCount() const override;
   int GetMidiOutCount() const override;

   int ShowClientInterface(
      wxWindow &parent, wxDialog &dialog, bool forceModal) override;

   bool InitializePlugin();

   // EffectUIClientInterface implementation

   std::shared_ptr<EffectInstance> MakeInstance() const override;
   std::shared_ptr<EffectInstance> DoMakeInstance();
   std::unique_ptr<EffectUIValidator> PopulateUI(
      ShuttleGui &S, EffectInstance &instance, EffectSettingsAccess &access)
   override;
   bool IsGraphicalUI() override;
   bool ValidateUI(EffectSettings &) override;
   bool CloseUI() override;

   bool CanExportPresets() override;
   void ExportPresets(const EffectSettings &settings) const override;
   void ImportPresets(EffectSettings &settings) override;

   bool HasOptions() override;
   void ShowOptions() override;

   // LV2Effect implementation

private:
   void InitializeSettings(const LV2Ports &ports, LV2EffectSettings &settings);

   struct PlainUIControl;

   bool LoadParameters(
      const RegistryPath & group, EffectSettings &settings) const;
   bool SaveParameters(
      const RegistryPath & group, const EffectSettings &settings) const;

   static int ui_resize(LV2UI_Feature_Handle handle, int width, int height);
   int UIResize(int width, int height);

   static void ui_closed(LV2UI_Controller controller);
   void UIClosed();

#if defined(__WXGTK__)
   static void size_request(GtkWidget *widget, GtkRequisition *requisition, LV2Effect *win);
   void SizeRequest(GtkWidget *widget, GtkRequisition *requisition);
#endif

   bool BuildFancy(LilvInstance &instance, const EffectSettings &settings);
   bool BuildPlain(EffectSettingsAccess &access);

   bool TransferDataToWindow(const EffectSettings &settings) override;
   void SetSlider(const LV2ControlPortState &state, const PlainUIControl &ctrl);

   void OnTrigger(wxCommandEvent & evt);
   void OnToggle(wxCommandEvent & evt);
   void OnChoice(wxCommandEvent & evt);
   void OnText(wxCommandEvent & evt);
   void OnSlider(wxCommandEvent & evt);

   void OnTimer(wxTimerEvent & evt);
   void OnIdle(wxIdleEvent & evt);
   void OnSize(wxSizeEvent & evt);
   void OnSizeWindow(wxCommandEvent & evt);

   static void suil_port_write_func(SuilController controller,
                                    uint32_t port_index,
                                    uint32_t buffer_size,
                                     uint32_t protocol,
                                     const void *buffer);
   void SuilPortWrite(uint32_t port_index,
                      uint32_t buffer_size,
                      uint32_t protocol,
                      const void *buffer);

   static uint32_t suil_port_index_func(SuilController controller,
                                    const char *port_symbol);
   uint32_t SuilPortIndex(const char *port_symbol);

private:
   const LV2Ports mPorts{ mPlug };
   LV2PortStates mPortStates{ mPorts };
   LV2PortUIStates mPortUIStates{ mPortStates, mPorts };

   LV2EffectSettings mSettings;

   //! This ignores its argument while we transition to statelessness
   //! but will later be rewritten as a static member function
   LV2EffectSettings &GetSettings(EffectSettings &) const {
      return const_cast<LV2EffectSettings &>(mSettings);
   }
   //! This ignores its argument while we transition to statelessness
   //! but will later be rewritten as a static member function
   const LV2EffectSettings &GetSettings(const EffectSettings &) const {
      return mSettings;
   }

   bool mWantsOptionsInterface{ false };
   bool mWantsStateInterface{ false };

   //! Holds lv2 library state for UI or for destructive processing
   std::unique_ptr<LV2Wrapper> mMaster;

   size_t mFramePos{};

   FloatBuffers mCVInBuffers;
   FloatBuffers mCVOutBuffers;

   double mLength{};

   wxTimer mTimer;

   wxWeakRef<wxDialog> mDialog;
   wxWindow *mParent{};

   bool mUseGUI{};

   // These objects contain C-style virtual function tables that we fill in
   const LV2UI_Resize mUIResizeFeature{ this, LV2Effect::ui_resize };
   // Not const, filled in when making a dialog
   LV2_Extension_Data_Feature mExtensionDataFeature{};

   const LilvNodePtr mHumanId{ lilv_plugin_get_name(&mPlug) };
   const LV2_External_UI_Host mExternalUIHost{
      LV2Effect::ui_closed, lilv_node_as_string(mHumanId.get()) };

   LV2_External_UI_Widget* mExternalWidget{};
   bool mExternalUIClosed{ false };

   //! Index into m_features
   size_t mInstanceAccessFeature{};
   //! Index into m_features
   size_t mParentFeature{};

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

   SuilHostPtr mSuilHost;
   SuilInstancePtr mSuilInstance;

   NativeWindow *mNativeWin{};
   wxSize mNativeWinInitialSize{ wxDefaultSize };
   wxSize mNativeWinLastSize{ wxDefaultSize };
   bool mResizing{ false };
#if defined(__WXGTK__)
   bool mResized{ false };
#endif

   const LV2UI_Idle_Interface *mUIIdleInterface{};
   const LV2UI_Show_Interface *mUIShowInterface{};

   NumericTextCtrl *mDuration{};

   // Mutable cache fields computed once on demand
   mutable bool mFactoryPresetsLoaded{ false };
   mutable RegistryPaths mFactoryPresetNames;
   mutable wxArrayString mFactoryPresetUris;

   DECLARE_EVENT_TABLE()

   friend class LV2Instance; // Remove this later
};

class LV2Instance final : public PerTrackEffect::Instance
{
public:
   LV2Instance(StatefulPerTrackEffect &effect, const LV2Ports &ports);
   ~LV2Instance() override;
   bool ProcessInitialize(EffectSettings &settings, double sampleRate,
      sampleCount totalLen, ChannelNames chanMap) override;
   size_t ProcessBlock(EffectSettings &settings,
      const float *const *inBlock, float *const *outBlock, size_t blockLen)
   override;
   sampleCount GetLatency(
      const EffectSettings &settings, double sampleRate) override;

   LV2Wrapper *GetWrapper() { return GetEffect().mMaster.get(); }

   //! Do nothing if there is already an LV2Wrapper.  Else try to make one
   //! but this may fail.  The wrapper object remains until this is destroyed.
   void MakeWrapper(const EffectSettings &settings,
      double projectRate, bool useOutput);

   size_t GetBlockSize() const override;
   size_t SetBlockSize(size_t maxBlockSize) override;

   bool RealtimeInitialize(EffectSettings &settings, double sampleRate)
      override;
   bool RealtimeAddProcessor(EffectSettings &settings,
      unsigned numChannels, float sampleRate) override;
   bool RealtimeFinalize(EffectSettings &settings) noexcept override;
   bool RealtimeSuspend() override;
   bool RealtimeResume() override;
   bool RealtimeProcessStart(EffectSettings &settings) override;
   size_t RealtimeProcess(size_t group,  EffectSettings &settings,
      const float *const *inbuf, float *const *outbuf, size_t numSamples)
      override;
   bool RealtimeProcessEnd(EffectSettings &settings) noexcept override;

private:
   LV2Effect &GetEffect() const {
      // Tolerate const_cast in this class while it sun-sets
      return static_cast<LV2Effect &>(
         const_cast<PerTrackEffect &>(mProcessor));
   }
   LV2EffectSettings &GetSettings(EffectSettings &settings) const {
      return GetEffect().GetSettings(settings);
   }
   const LV2EffectSettings &GetSettings(const EffectSettings &settings) const {
      return GetEffect().GetSettings(settings);
   }

   const LV2Ports &mPorts;

   //! Each holds lv2 library state for realtime processing of one track
   std::vector<std::unique_ptr<LV2Wrapper>> mSlaves;

   LV2_Atom_Forge mForge{};

   // Position info
   float mPositionSpeed{ 1.0f };
   int64_t mPositionFrame{ 0 };

   size_t mBlockSize{};
   size_t mUserBlockSize{};

   size_t mNumSamples{};
   bool mRolling{ false };
   bool mUseLatency{ false };
   bool mLatencyDone{ false };
};

#endif
#endif
