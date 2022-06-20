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

#include <thread>
#include <vector>

#include <wx/event.h> // to inherit
#include <wx/msgqueue.h>
#include <wx/timer.h>
#include <wx/weakref.h>

#include "lv2/atom/forge.h"
#include "lv2/data-access/data-access.h"
#include "lv2/state/state.h"
#include "lv2/worker/worker.h"
#include <suil/suil.h>
#include "lv2_external_ui.h"

#include "LV2FeaturesList.h"
#include "LV2Ports.h"
#include "../../ShuttleGui.h"
#include "SampleFormat.h"

#include "NativeWindow.h"

#include <unordered_map>

using LilvInstancePtr = Lilv_ptr<LilvInstance, lilv_instance_free>;
using LilvScalePointsPtr = Lilv_ptr<LilvScalePoints, lilv_scale_points_free>;
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

struct LV2EffectSettings {
   //! vector of values in correspondence with the control ports
   std::vector<float> values;
};

class LV2Effect final : public LV2FeaturesList
{
public:
   LV2Effect(const LilvPlugin *plug);
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
   bool SupportsRealtime() const override;
   bool SupportsAutomation() const override;

   bool SaveSettings(
      const EffectSettings &settings, CommandParameters & parms) const override;
   bool LoadSettings(
      const CommandParameters & parms, EffectSettings &settings) const override;

   bool LoadUserPreset(
      const RegistryPath & name, EffectSettings &settings) const override;
   bool DoLoadUserPreset(const RegistryPath & name, EffectSettings &settings);
   bool SaveUserPreset(
      const RegistryPath & name, const EffectSettings &settings) const override;

   RegistryPaths GetFactoryPresets() const override;
   bool LoadFactoryPreset(int id, EffectSettings &settings) const override;
   bool DoLoadFactoryPreset(int id);

   unsigned GetAudioInCount() const override;
   unsigned GetAudioOutCount() const override;

   int GetMidiInCount() const override;
   int GetMidiOutCount() const override;

   void SetSampleRate(double rate) override;
   size_t SetBlockSize(size_t maxBlockSize) override;
   size_t GetBlockSize() const override;

   sampleCount GetLatency() override;

   bool ProcessInitialize(EffectSettings &settings,
      sampleCount totalLen, ChannelNames chanMap) override;
   bool ProcessFinalize() override;
   size_t ProcessBlock(EffectSettings &settings,
      const float *const *inBlock, float *const *outBlock, size_t blockLen)
      override;

   bool RealtimeInitialize(EffectSettings &settings) override;
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
   struct PlainUIControl;

   bool LoadParameters(const RegistryPath & group, EffectSettings &settings);
   bool SaveParameters(
      const RegistryPath & group, const EffectSettings &settings) const;

   std::unique_ptr<LV2Wrapper>
   InitInstance(const EffectSettings &settings, float sampleRate);

   static int ui_resize(LV2UI_Feature_Handle handle, int width, int height);
   int UIResize(int width, int height);

   static void ui_closed(LV2UI_Controller controller);
   void UIClosed();

#if defined(__WXGTK__)
   static void size_request(GtkWidget *widget, GtkRequisition *requisition, LV2Effect *win);
   void SizeRequest(GtkWidget *widget, GtkRequisition *requisition);
#endif

   bool BuildFancy(const EffectSettings &settings);
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

   static const void *get_value_func(const char *port_symbol,
                                     void *user_data,
                                     uint32_t *size,
                                     uint32_t *type);
   const void *GetPortValue(const char *port_symbol,
                            uint32_t *size,
                            uint32_t *type);

   static void set_value_func(const char *port_symbol,
                              void *user_data,
                              const void *value,
                              uint32_t size,
                              uint32_t type);
   void SetPortValue(const char *port_symbol,
                     const void *value,
                     uint32_t size,
                     uint32_t type);

private:
   size_t mUserBlockSize{ mBlockSize };

   //! Mapping from index number among all ports, to position
   //! among the control ports only
   std::unordered_map<uint32_t, size_t> mControlPortMap;
   LV2ControlPortArray mControlPorts;
   LV2ControlPortStateArray mControlPortStates;
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

   LV2AudioPortArray mAudioPorts;
   unsigned mAudioIn{ 0 };
   unsigned mAudioOut{ 0 };

   LV2AtomPortArray mAtomPorts;
   LV2AtomPortStateArray mAtomPortStates;

   LV2AtomPortStatePtr mControlIn;
   LV2AtomPortStatePtr mControlOut;
   unsigned mMidiIn{ 0 };
   unsigned mMidiOut{ 0 };

   LV2CVPortArray mCVPorts;
   LV2CVPortStateArray mCVPortStates;
   unsigned mCVIn;
   unsigned mCVOut;

   std::unordered_map<TranslatableString, std::vector<int>> mGroupMap;
   TranslatableStrings mGroups;

   bool mWantsOptionsInterface{ false };
   bool mWantsStateInterface{ false };

   bool mUseLatency{ false };
   bool mLatencyDone{ false };
   bool mRolling{ false };

   //! Holds lv2 library state needed for the user interface
   std::unique_ptr<LV2Wrapper> mMaster;
   //! Holds lv2 library state for destructive processing
   std::unique_ptr<LV2Wrapper> mProcess;
   //! Each holds lv2 library state for realtime processing of one track
   std::vector<std::unique_ptr<LV2Wrapper>> mSlaves;

   size_t mNumSamples{};
   size_t mFramePos{};

   FloatBuffers mCVInBuffers;
   FloatBuffers mCVOutBuffers;

   // Position info
   float mPositionSpeed{ 1.0f };
   float mPositionFrame{ 0.0f };

   double mLength{};

   wxTimer mTimer;

   wxWeakRef<wxDialog> mDialog;
   wxWindow *mParent{};

   bool mUseGUI{};

   // These objects contain C-style virtual function tables that we fill in
   const LV2UI_Resize mUIResizeFeature{ this, LV2Effect::ui_resize };
   // Not const, filled in when making a dialog
   LV2_Extension_Data_Feature mExtensionDataFeature{};

   const LilvNodePtr mHumanId{ lilv_plugin_get_name(mPlug) };
   const LV2_External_UI_Host mExternalUIHost{
      LV2Effect::ui_closed, lilv_node_as_string(mHumanId.get()) };

   LV2_External_UI_Widget* mExternalWidget{};
   bool mExternalUIClosed{ false };

   LV2_Atom_Forge mForge{};

   //! Index into m_features
   size_t mInstanceAccessFeature{};
   //! Index into m_features
   size_t mParentFeature{};
   LV2_Feature *mWorkerScheduleFeature{};

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

   friend class LV2Wrapper;
};

class LV2Wrapper final
{
public:
   struct LV2Work {
      uint32_t size{};
      const void *data{};
   };

public:
   //! May spawn a thread
   LV2Wrapper(const LV2FeaturesList &featuresList,
      const LilvPlugin *plugin, double sampleRate);
   //! If a thread was started, joins it
   ~LV2Wrapper();
   void Activate();
   void Deactivate();
   LilvInstance *GetInstance() const;
   LV2_Handle GetHandle() const;
   float GetLatency() const;
   void SetFreeWheeling(bool enable);
   void SetSampleRate();
   void SetBlockSize();
   void ConsumeResponses();
   static LV2_Worker_Status schedule_work(LV2_Worker_Schedule_Handle handle,
                                          uint32_t size,
                                          const void *data);
   LV2_Worker_Status ScheduleWork(uint32_t size, const void *data);
   static LV2_Worker_Status respond(LV2_Worker_Respond_Handle handle,
                                    uint32_t size,
                                    const void *data);
   LV2_Worker_Status Respond(uint32_t size, const void *data);

private:
   void ThreadFunction();

   std::thread mThread;

   const LV2FeaturesList &mFeaturesList;
   LilvInstancePtr mInstance;
   LV2_Handle mHandle{};

   wxMessageQueue<LV2Work> mRequests;
   wxMessageQueue<LV2Work> mResponses;

   // Options extension
   const LV2_Options_Interface *mOptionsInterface{};

   // State extension
   const LV2_State_Interface *mStateInterface{};

   // Worker extension
   const LV2_Worker_Interface *mWorkerInterface{};
   // Another object with an explicit virtual function table
   LV2_Worker_Schedule mWorkerSchedule{ this, LV2Wrapper::schedule_work };

   float mLatency{ 0.0 };

   //! If true, do not spawn extra worker threads
   bool mFreeWheeling{ false };

   //! Written by main thread, read by worker, but atomic isn't needed because
   //! mRequests provides synchronization
   bool mStopWorker{ false };

   bool mActivated{ false };
};

#endif
#endif
