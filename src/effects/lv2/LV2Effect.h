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

#include <memory>
#include <thread>
#include <vector>

#include <wx/event.h> // to inherit
#include <wx/msgqueue.h>
#include <wx/timer.h>
#include <wx/weakref.h>

#include "lv2/atom/forge.h"
#include "lv2/data-access/data-access.h"
#include "lv2/log/log.h"
#include "lv2/options/options.h"
#include "lv2/state/state.h"
#include "lv2/uri-map/uri-map.h"
#include "lv2/worker/worker.h"
#include <suil/suil.h>
#include "lv2_external_ui.h"

#include "../StatefulPerTrackEffect.h"
#include "../../ShuttleGui.h"
#include "SampleFormat.h"

#include "LV2Symbols.h"
#include "NativeWindow.h"

#include "zix/ring.h"

#include <unordered_map>

using LilvNodesPtr = Lilv_ptr<LilvNodes, lilv_nodes_free>;
using LilvScalePointsPtr = Lilv_ptr<LilvScalePoints, lilv_scale_points_free>;
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

// Define a maximum block size in number of samples (not bytes)
#define DEFAULT_BLOCKSIZE 1048576

class LV2Port
{
public:
   LV2Port(const LilvPort *port,
           int index,
           bool isInput,
           const wxString & symbol,
           const wxString & name,
           TranslatableString group)
   :  mPort(port),
      mIndex(index),
      mIsInput(isInput),
      mSymbol(symbol),
      mName(name),
      mGroup(std::move(group))
   {
   };
   LV2Port(const LV2Port &) = default;
   LV2Port &operator = (const LV2Port &) = default;

   const LilvPort *mPort;

   uint32_t mIndex;
   bool mIsInput;

   wxString mSymbol;
   wxString mName;
   TranslatableString mGroup;
};

class LV2AudioPort : public LV2Port
{
public:
   LV2AudioPort(const LilvPort *port,
               int index,
               bool isInput,
               const wxString & symbol,
               const wxString & name,
               const TranslatableString & group)
   :  LV2Port(port, index, isInput, symbol, name, group)
   {
   }
};
using LV2AudioPortPtr = std::shared_ptr<LV2AudioPort>;
using LV2AudioPortArray = std::vector<LV2AudioPortPtr>;

class LV2AtomPort : public LV2Port
{
public:
   LV2AtomPort(const LilvPort *port,
               int index,
               bool isInput,
               const wxString & symbol,
               const wxString & name,
               const TranslatableString & group)
   :  LV2Port(port, index, isInput, symbol, name, group)
   {
  
      mIsMidi = false;
      mWantsPosition = false;
      mMinimumSize = 1024;
   }
   virtual ~LV2AtomPort() = default;

   uint32_t mMinimumSize;
   bool mIsMidi;
   bool mWantsPosition;

   std::vector<uint8_t> mBuffer;
   Lilv_ptr<ZixRing, zix_ring_free> mRing;
};
using LV2AtomPortPtr = std::shared_ptr<LV2AtomPort>;
using LV2AtomPortArray = std::vector<LV2AtomPortPtr>;

class LV2CVPort : public LV2Port
{
public:
   LV2CVPort(const LilvPort *port,
             int index,
             bool isInput,
             const wxString & symbol,
             const wxString & name,
             const TranslatableString & group)
   :  LV2Port(port, index, isInput, symbol, name, group)
   {
      mMin = 0.0;
      mMax = 1.0;
      mDef = 0.0;
      mHasLo = false;
      mHasHi = false;
   };

   float mMin;
   float mMax;
   float mDef;
   bool mHasLo;
   bool mHasHi;

   Floats mBuffer;
};
using LV2CVPortPtr = std::shared_ptr<LV2CVPort>;
using LV2CVPortArray = std::vector<LV2CVPortPtr>;

class LV2EffectMeter;

/** A structure that contains information about a single LV2 plugin port. */
class LV2ControlPort : public LV2Port
{
public:
   LV2ControlPort(const LilvPort *port,
                  int index,
                  bool isInput,
                  const wxString & symbol,
                  const wxString & name,
                  const TranslatableString & group)
   :  LV2Port(port, index, isInput, symbol, name, group)
   {
      mMin = 0.0;
      mMax = 0.0;
      mDef = 0.0;
      mVal = 0.0;
      mLst = 0.0;
      mTmp = 0.0;
      mDmy = 0.0;
      mLo = 0.0;
      mHi = 0.0;
      mHasLo = false;
      mHasHi = false;
      mToggle = false;
      mTrigger = false;
      mInteger = false;
      mSampleRate = false;
      mEnumeration = false;
      mLogarithmic = false;
      mNotOnGui = false;
      mCtrl.button = NULL;
      mText = NULL;
   };
 
   wxString mUnits;
   float mMin;
   float mMax;
   float mDef;
   float mVal;
   float mLst;
   float mTmp;
   float mDmy;
   float mLo;
   float mHi;
   bool mHasLo;
   bool mHasHi;
   bool mToggle;
   bool mTrigger;
   bool mInteger;
   bool mSampleRate;
   bool mEnumeration;
   bool mLogarithmic;
   bool mNotOnGui;

   // ScalePoints
   std::vector<double> mScaleValues;
   wxArrayString mScaleLabels;

   // UI
   wxTextCtrl *mText;
   union
   {
      wxButton *button;
      wxCheckBox *checkbox;
      wxChoice *choice;
      LV2EffectMeter *meter;
      wxSlider *slider;
   } mCtrl;
};
using LV2ControlPortPtr = std::shared_ptr<LV2ControlPort>;
using LV2ControlPortArray = std::vector<LV2ControlPortPtr>;

class LV2Wrapper;

// Define a reasonable default sequence size in bytes
#define DEFAULT_SEQSIZE 8192

class LV2Effect final : public StatefulPerTrackEffect
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
   bool LoadParameters(const RegistryPath & group, EffectSettings &settings);
   bool SaveParameters(
      const RegistryPath & group, const EffectSettings &settings) const;

   std::unique_ptr<LV2Wrapper> InitInstance(float sampleRate);

   static uint32_t uri_to_id(LV2_URI_Map_Callback_Data callback_data,
                             const char *map,
                             const char *uri);
   static LV2_URID urid_map(LV2_URID_Map_Handle handle, const char *uri);
   LV2_URID URID_Map(const char *uri);

   static const char *urid_unmap(LV2_URID_Unmap_Handle handle, LV2_URID urid);
   const char *URID_Unmap(LV2_URID urid);

   static int ui_resize(LV2UI_Feature_Handle handle, int width, int height);
   int UIResize(int width, int height);

   static void ui_closed(LV2UI_Controller controller);
   void UIClosed();

   static int log_printf(LV2_Log_Handle handle, LV2_URID type, const char *fmt, ...);
   static int log_vprintf(LV2_Log_Handle handle, LV2_URID type, const char *fmt, va_list ap);
   int LogVPrintf(LV2_URID type, const char *fmt, va_list ap);

#if defined(__WXGTK__)
   static void size_request(GtkWidget *widget, GtkRequisition *requisition, LV2Effect *win);
   void SizeRequest(GtkWidget *widget, GtkRequisition *requisition);
#endif

   size_t AddOption(LV2_URID, uint32_t size, LV2_URID, const void *value);
   /*!
    @param subject URI of a plugin
    @return whether all required features of subject are supported
    */
   bool ValidateOptions(const LilvNode *subject);
   /*!
    @param subject URI of a plugin
    @param required whether to check required or optional features of subject
    @return true only if `!required` or else all checked features are supported
    */
   bool CheckOptions(const LilvNode *subject, bool required);

   //! Get vector of pointers to features, whose `.data()` can be passed to lv2
   using FeaturePointers = std::vector<const LV2_Feature *>;
   FeaturePointers GetFeaturePointers() const;

   void AddFeature(const char *uri, const void *data);
   /*!
    @param subject URI of the host or of the UI identifies a resource in lv2
    @return whether all required features of subject are supported
    */
   bool ValidateFeatures(const LilvNode *subject);
   /*!
    @param subject URI of the host or of the UI identifies a resource in lv2
    @param required whether to check required or optional features of subject
    @return true only if `!required` or else all checked features are supported
    */
   bool CheckFeatures(const LilvNode *subject, bool required);

   bool BuildFancy();
   bool BuildPlain(EffectSettingsAccess &access);

   bool TransferDataToWindow() /* not override */;
   void SetSlider(const LV2ControlPortPtr & port);

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

   // lv2 functions require a pointer to non-const in places, but presumably
   // have no need to mutate the members of this structure
   LV2_URID_Map *URIDMapFeature() const
   { return const_cast<LV2_URID_Map*>(&mURIDMapFeature); }

private:
 
   // Declare local URI map
   LV2Symbols::URIDMap mURIDMap;

   const LilvPlugin *const mPlug;

   float mSampleRate{ 44100 };
   size_t mBlockSize{ DEFAULT_BLOCKSIZE };
   int mSeqSize{ DEFAULT_SEQSIZE };

   size_t mMinBlockSize{ 1 };
   size_t mMaxBlockSize{ mBlockSize };
   size_t mUserBlockSize{ mBlockSize };

   std::unordered_map<uint32_t, LV2ControlPortPtr> mControlPortMap;
   LV2ControlPortArray mControlPorts;

   LV2AudioPortArray mAudioPorts;
   unsigned mAudioIn{ 0 };
   unsigned mAudioOut{ 0 };

   LV2AtomPortArray mAtomPorts;
   LV2AtomPortPtr mControlIn;
   LV2AtomPortPtr mControlOut;
   unsigned mMidiIn{ 0 };
   unsigned mMidiOut{ 0 };

   LV2CVPortArray mCVPorts;
   unsigned mCVIn;
   unsigned mCVOut;

   std::unordered_map<TranslatableString, std::vector<int>> mGroupMap;
   TranslatableStrings mGroups;

   bool mWantsOptionsInterface{ false };
   bool mWantsStateInterface{ false };
   bool mWantsWorkerInterface{ false };
   bool mNoResize{ false };

   bool mUseLatency{ false };
   int mLatencyPort{ -1 };
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
   const LV2_URI_Map_Feature mUriMapFeature{
      this, LV2Effect::uri_to_id }; // Features we support
   const LV2_URID_Map mURIDMapFeature{ this, LV2Effect::urid_map };
   const LV2_URID_Unmap mURIDUnmapFeature{ this, LV2Effect::urid_unmap };
   const LV2UI_Resize mUIResizeFeature{ this, LV2Effect::ui_resize };
   const LV2_Log_Log mLogFeature{
      this, LV2Effect::log_printf, LV2Effect::log_vprintf };

   // Not const, filled in when making a dialog
   LV2_Extension_Data_Feature mExtensionDataFeature{};

   const LilvNodePtr mHumanId{ lilv_plugin_get_name(mPlug) };
   const LV2_External_UI_Host mExternalUIHost{
      LV2Effect::ui_closed, lilv_node_as_string(mHumanId.get()) };

   LV2_External_UI_Widget* mExternalWidget{};
   bool mExternalUIClosed{ false };

   LV2_Atom_Forge mForge{};
   
   std::vector<LV2_Options_Option> mOptions;
   size_t mBlockSizeOption{};
   size_t mSampleRateOption{};
   bool mSupportsNominalBlockLength{ false };
   bool mSupportsSampleRate{ false };

   std::vector<LV2_Feature> mFeatures;

   //! Index into m_features
   size_t mInstanceAccessFeature{};
   //! Index into m_features
   size_t mParentFeature{};
   LV2_Feature *mWorkerScheduleFeature{};

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

//! Use when lilv.h comments "must not be freed" or we use the node elsewhere,
//! or the node pointer is from iterating a LilvNodes collection
inline wxString LilvString(const LilvNode *node)
{
   return wxString::FromUTF8(lilv_node_as_string(node));
};

//! Use when lilv.h comments "Returned value must be freed by the caller."
//! We free it in this function.
//! Name suggests C++ move semantics applied to `node`, but only C types used
inline wxString LilvStringMove(LilvNode *node)
{
   LilvNodePtr temp{ node };
   wxString str = LilvString(node);
   return str;
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
   LV2Wrapper(const LV2Effect &effect,
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

   const LV2Effect &mEffect;
   LilvInstance *mInstance{};
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
