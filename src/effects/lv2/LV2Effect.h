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
#include "lv2/state/state.h"
#include "lv2/worker/worker.h"
#include <suil/suil.h>
#include "lv2_external_ui.h"

#include "LV2FeaturesList.h"
#include "../../ShuttleGui.h"
#include "SampleFormat.h"

#include "NativeWindow.h"

#include "zix/ring.h"

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

//! Immutable description of an LV2 port
class LV2Port {
public:
   LV2Port(const LilvPort *port, int index, bool isInput,
      const wxString &symbol, const wxString &name,
      const TranslatableString &group
   )  : mPort(port), mIndex(index), mIsInput(isInput)
      , mSymbol(symbol), mName(name), mGroup(group)
   {}
   const LilvPort *const mPort;
   const uint32_t mIndex;
   const bool mIsInput;
   const wxString mSymbol;
   const wxString mName;
   const TranslatableString mGroup;
};

//! Immutable description of an LV2 Audio port
class LV2AudioPort final : public LV2Port {
public:
   using LV2Port::LV2Port;
};
using LV2AudioPortPtr = std::shared_ptr<LV2AudioPort>;
using LV2AudioPortArray = std::vector<LV2AudioPortPtr>;

//! Immutable description of an LV2 Atom port
class LV2AtomPort final : public LV2Port {
public:
   LV2AtomPort(const LilvPort *port, int index, bool isInput,
      const wxString & symbol, const wxString & name,
      const TranslatableString & group,
      uint32_t minimumSize, bool isMidi, bool wantsPosition
   )  : LV2Port{ port, index, isInput, symbol, name, group }
      , mMinimumSize{ minimumSize }
      , mIsMidi{ isMidi }
      , mWantsPosition{ wantsPosition }
   {}
   const uint32_t mMinimumSize;
   const bool mIsMidi;
   const bool mWantsPosition;
};
using LV2AtomPortPtr = std::shared_ptr<LV2AtomPort>;
using LV2AtomPortArray = std::vector<LV2AtomPortPtr>;

//! State of an instance of an LV2 Atom port
struct LV2AtomPortState final {
   //! @pre `pPort != nullptr`
   explicit LV2AtomPortState(LV2AtomPortPtr pPort)
      : mpPort{ move(pPort) }
      , mRing{ zix_ring_new(mpPort->mMinimumSize) }
      , mBuffer( mpPort->mMinimumSize )
   {
      assert(mpPort);
      zix_ring_mlock(mRing.get());
   }
   const LV2AtomPortPtr mpPort;
   const Lilv_ptr<ZixRing, zix_ring_free> mRing;
   std::vector<uint8_t> mBuffer;
};
using LV2AtomPortStatePtr = std::shared_ptr<LV2AtomPortState>;
using LV2AtomPortStateArray = std::vector<LV2AtomPortStatePtr>;

//! Immutable description of an LV2 CV port (control data signal at sample rate)
class LV2CVPort final : public LV2Port {
public:
   LV2CVPort(const LilvPort *port, int index, bool isInput,
      const wxString & symbol, const wxString & name,
      const TranslatableString & group,
      float min, float max, float def, bool hasLo, bool hasHi
   )  : LV2Port(port, index, isInput, symbol, name, group)
      , mMin{ min }, mMax{ max }, mDef{ def }, mHasLo{ hasLo }, mHasHi{ hasHi }
   {}
   const float mMin;
   const float mMax;
   const float mDef;
   const bool mHasLo;
   const bool mHasHi;
};
using LV2CVPortPtr = std::shared_ptr<LV2CVPort>;
using LV2CVPortArray = std::vector<LV2CVPortPtr>;

//! State of an instance of an LV2 CV port
struct LV2CVPortState final {
   //! @pre `pPort != nullptr`
   explicit LV2CVPortState(LV2CVPortPtr pPort) : mpPort{ move(pPort) } {
      assert(mpPort);
   }
   const LV2CVPortPtr mpPort;
   Floats mBuffer;
};
//! No need yet for extra indirection
using LV2CVPortStateArray = std::vector<LV2CVPortState>;

class LV2EffectMeter;

/** A structure that contains information about a single LV2 plugin port. */
class LV2ControlPort final : public LV2Port
{
public:
   LV2ControlPort(const LilvPort *port, int index, bool isInput,
      const wxString & symbol, const wxString & name,
      const TranslatableString & group,
      std::vector<double> scaleValues, wxArrayString scaleLabels,
      const wxString &units,
      float min, float max, float def, bool hasLo, bool hasHi,
      bool toggle, bool enumeration, bool integer, bool sampleRate,
      bool trigger, bool logarithmic
   )  : LV2Port{ port, index, isInput, symbol, name, group }
      , mScaleValues{ move(scaleValues) }
      , mScaleLabels{ std::move(scaleLabels) }
      , mUnits{ units }
      , mMin{ min }, mMax{ max }, mDef{ def }
      , mHasLo{ hasLo }, mHasHi{ hasHi }
      , mToggle{ toggle }, mEnumeration{ enumeration }, mInteger{ integer }
      , mSampleRate{ sampleRate }
      , mTrigger{ trigger }, mLogarithmic{ logarithmic }
   {}
 
   // ScalePoints
   const std::vector<double> mScaleValues;
   const wxArrayString mScaleLabels;

   const wxString mUnits;
   const float mMin;
   const float mMax;
   const float mDef;
   const bool mHasLo;
   const bool mHasHi;
   const bool mToggle;
   const bool mEnumeration;
   const bool mInteger;
   const bool mSampleRate;
   const bool mTrigger;
   const bool mLogarithmic;

   float mVal{ 0.0 };

   //! Map a real number to one of the scale points
   size_t Discretize(float value) const {
      auto s = mScaleValues.size();
      for (; s > 0 && --s;)
         if (value >= mScaleValues[s])
            break;
      return s;
   }
};
using LV2ControlPortPtr = std::shared_ptr<LV2ControlPort>;
using LV2ControlPortArray = std::vector<LV2ControlPortPtr>;

//! State of an instance of an LV2 Control port
struct LV2ControlPortState final {
   //! @pre `pPort != nullptr`
   explicit LV2ControlPortState(LV2ControlPortPtr pPort)
      : mpPort{ move(pPort) }
   {
      assert(mpPort);
   }
   const LV2ControlPortPtr mpPort;
   //! Value of mTmp last seen by idle-time updater
   float mLst{ 0.0 };
   //! Value of UI control, as scaled by sample rate if that is required
   float mTmp{ 0.0 };
   //! Lower bound, as scaled by sample rate if that is required
   float mLo{ 0.0 };
   //! Upper bound, as scaled by sample rate if that is required
   float mHi{ 0.0 };
};
//! No need yet for extra indirection
using LV2ControlPortStateArray = std::vector<LV2ControlPortState>;

class LV2Wrapper;

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
