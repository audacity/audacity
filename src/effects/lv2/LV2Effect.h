/**********************************************************************

  Audacity: A Digital Audio Editor

  LV2Effect.h

  Audacity(R) is copyright (c) 1999-2013 Audacity Team.
  License: GPL v2.  See License.txt.

*********************************************************************/



#if USE_LV2

class wxArrayString;

#include <vector>

#include <wx/event.h> // to inherit
#include <wx/msgqueue.h>
#include <wx/thread.h>
#include <wx/timer.h>

#include "lv2/atom/forge.h"
#include "lv2/data-access/data-access.h"
#include "lv2/log/log.h"
#include "lv2/midi/midi.h"
#include "lv2/options/options.h"
#include "lv2/state/state.h"
#include "lv2/time/time.h"
#include "lv2/uri-map/uri-map.h"
#include "lv2/urid/urid.h"
#include "lv2/worker/worker.h"
#include "lv2/ui/ui.h"

#include <lilv/lilv.h>
#include <suil/suil.h>

#include "../../ShuttleGui.h"
#include "SampleFormat.h"

#include "LoadLV2.h"
#include "NativeWindow.h"

#include "lv2_external_ui.h"
#include "zix/ring.h"

#include <unordered_map>

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

class LV2Port
{
public:
   LV2Port(const LilvPort *port,
           int index,
           bool isInput,
           const wxString & symbol,
           const wxString & name,
           const wxString & group)
   :  mPort(port),
      mIndex(index),
      mIsInput(isInput),
      mSymbol(symbol),
      mName(name),
      mGroup(group)
   {
   };
   LV2Port(const LV2Port &) = default;
   LV2Port &operator = (const LV2Port &) = default;

   const LilvPort *mPort;

   uint32_t mIndex;
   bool mIsInput;

   wxString mSymbol;
   wxString mName;
   wxString mGroup;
};

class LV2AudioPort : public LV2Port
{
public:
   LV2AudioPort(const LilvPort *port,
               int index,
               bool isInput,
               const wxString & symbol,
               const wxString & name,
               const wxString & group)
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
               const wxString & group)
   :  LV2Port(port, index, isInput, symbol, name, group)
   {
  
      mIsMidi = false;
      mWantsPosition = false;
      mMinimumSize = 1024;
      mRing = NULL;
   }
   virtual ~LV2AtomPort()
   {
      if (mRing)
      {
         zix_ring_free(mRing);
      }
   }

   uint32_t mMinimumSize;
   bool mIsMidi;
   bool mWantsPosition;

   std::vector<uint8_t> mBuffer;
   ZixRing *mRing;
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
             const wxString & group)
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
                  const wxString & group)
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

class LV2EffectSettingsDialog;
class LV2Wrapper;

using URIDMap = std::vector<MallocString<>>;

class LV2Effect final : public wxEvtHandler,
                        public EffectClientInterface,
                        public EffectUIClientInterface
{
public:
   LV2Effect(const LilvPlugin *plug);
   virtual ~LV2Effect();

   // ComponentInterface implementation

   PluginPath GetPath() override;
   ComponentInterfaceSymbol GetSymbol() override;
   VendorSymbol GetVendor() override;
   wxString GetVersion() override;
   TranslatableString GetDescription() override;

   // EffectDefinitionInterface implementation

   EffectType GetType() override;
   EffectFamilySymbol GetFamily() override;
   bool IsInteractive() override;
   bool IsDefault() override;
   bool IsLegacy() override;
   bool SupportsRealtime() override;
   bool SupportsAutomation() override;

   // EffectClientInterface implementation

   bool SetHost(EffectHostInterface *host) override;

   unsigned GetAudioInCount() override;
   unsigned GetAudioOutCount() override;

   int GetMidiInCount() override;
   int GetMidiOutCount() override;

   void SetSampleRate(double rate) override;
   size_t SetBlockSize(size_t maxBlockSize) override;
   size_t GetBlockSize() const override;

   sampleCount GetLatency() override;
   size_t GetTailSize() override;

   bool IsReady() override;
   bool ProcessInitialize(sampleCount totalLen, ChannelNames chanMap = NULL) override;
   bool ProcessFinalize() override;
   size_t ProcessBlock(float **inbuf, float **outbuf, size_t size) override;

   bool RealtimeInitialize() override;
   bool RealtimeAddProcessor(unsigned numChannels, float sampleRate) override;
   bool RealtimeFinalize() override;
   bool RealtimeSuspend() override;
   bool RealtimeResume() override;
   bool RealtimeProcessStart() override;
   size_t RealtimeProcess(int group, float **inbuf, float **outbuf, size_t numSamples) override;
   bool RealtimeProcessEnd() override;

   bool ShowInterface( wxWindow &parent,
      const EffectDialogFactory &factory, bool forceModal = false) override;

   bool GetAutomationParameters(CommandParameters & parms) override;
   bool SetAutomationParameters(CommandParameters & parms) override;

   // EffectUIClientInterface implementation

   void SetHostUI(EffectUIHostInterface *host) override;
   bool PopulateUI(ShuttleGui &S) override;
   bool IsGraphicalUI() override;
   bool ValidateUI() override;
   bool HideUI() override;
   bool CloseUI() override;

   bool LoadUserPreset(const RegistryPath & name) override;
   bool SaveUserPreset(const RegistryPath & name) override;

   RegistryPaths GetFactoryPresets() override;
   bool LoadFactoryPreset(int id) override;
   bool LoadFactoryDefaults() override;

   bool CanExportPresets() override;
   void ExportPresets() override;
   void ImportPresets() override;

   bool HasOptions() override;
   void ShowOptions() override;

   // LV2Effect implementation

private:
   bool LoadParameters(const RegistryPath & group);
   bool SaveParameters(const RegistryPath & group);

   LV2Wrapper *InitInstance(float sampleRate);
   void FreeInstance(LV2Wrapper *wrapper);

   static uint32_t uri_to_id(LV2_URI_Map_Callback_Data callback_data,
                             const char *map,
                             const char *uri);
   static LV2_URID urid_map(LV2_URID_Map_Handle handle, const char *uri);
   LV2_URID URID_Map(const char *uri);
   static LV2_URID Lookup_URI(URIDMap & map, const char *uri, bool add = true);

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
   bool ValidateOptions(const LilvNode *subject);
   bool CheckOptions(const LilvNode *subject, const LilvNode *predicate, bool required);

   LV2_Feature *AddFeature(const char *uri, void *data);
   bool ValidateFeatures(const LilvNode *subject);
   bool CheckFeatures(const LilvNode *subject, const LilvNode *predicate, bool required);

   bool BuildFancy();
   bool BuildPlain();

   bool TransferDataToWindow() /* not override */;
   bool TransferDataFromWindow() /* not override */;
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

private:
 
   // Declare the global and local URI maps
   static URIDMap gURIDMap;
   URIDMap mURIDMap;

   // Declare the static LILV URI nodes
#undef NODE
#define NODE(n, u) static LilvNode *node_##n;
   NODELIST

   // Declare the static URIDs
#undef URID
#define URID(n, u) static LV2_URID urid_##n;
   URIDLIST

   const LilvPlugin *mPlug;

   EffectHostInterface *mHost;

   float mSampleRate;
   int mBlockSize;
   int mSeqSize;

   int mMinBlockSize;
   int mMaxBlockSize;
   int mUserBlockSize;

   std::unordered_map<uint32_t, LV2ControlPortPtr> mControlPortMap;
   LV2ControlPortArray mControlPorts;

   LV2AudioPortArray mAudioPorts;
   unsigned mAudioIn;
   unsigned mAudioOut;

   LV2AtomPortArray mAtomPorts;
   LV2AtomPortPtr mControlIn;
   LV2AtomPortPtr mControlOut;
   unsigned mMidiIn;
   unsigned mMidiOut;

   LV2CVPortArray mCVPorts;
   unsigned mCVIn;
   unsigned mCVOut;

   std::unordered_map<wxString, std::vector<int>> mGroupMap;
   wxArrayString mGroups;

   bool mWantsOptionsInterface;
   bool mWantsStateInterface;
   bool mWantsWorkerInterface;
   bool mNoResize;

   bool mUseLatency;
   int mLatencyPort;
   bool mLatencyDone;
   bool mRolling;
   bool mActivated;

   LV2Wrapper *mMaster;
   LV2Wrapper *mProcess;
   std::vector<LV2Wrapper *> mSlaves;

   FloatBuffers mMasterIn, mMasterOut;
   size_t mNumSamples;
   size_t mFramePos;

   FloatBuffers mCVInBuffers;
   FloatBuffers mCVOutBuffers;

   // Position info
   float mPositionSpeed;
   float mPositionFrame;

   double mLength;

   wxTimer mTimer;

   wxDialog *mDialog;
   wxWindow *mParent;
   EffectUIHostInterface *mUIHost;

   bool mUseGUI;

   // Features we support
   LV2_URI_Map_Feature mUriMapFeature;
   LV2_URID_Map mURIDMapFeature;
   LV2_URID_Unmap mURIDUnmapFeature;
   LV2UI_Resize mUIResizeFeature;
   LV2_Log_Log mLogFeature;
   LV2_Extension_Data_Feature mExtensionDataFeature;

   LV2_External_UI_Host mExternalUIHost;
   LV2_External_UI_Widget* mExternalWidget;
   bool mExternalUIClosed;

   LV2_Atom_Forge mForge;
   
   std::vector<LV2_Options_Option> mOptions;
   size_t mBlockSizeOption;
   size_t mSampleRateOption;
   bool mSupportsNominalBlockLength;
   bool mSupportsSampleRate;

   std::vector<std::unique_ptr<LV2_Feature>> mFeatures;

   LV2_Feature *mInstanceAccessFeature;
   LV2_Feature *mParentFeature;
   LV2_Feature *mWorkerScheduleFeature;

   bool mFreewheeling;

   SuilHost *mSuilHost;
   SuilInstance *mSuilInstance;

   NativeWindow *mNativeWin;
   wxSize mNativeWinInitialSize;
   wxSize mNativeWinLastSize;
   bool mResizing;
#if defined(__WXGTK__)
   bool mResized;
#endif

   LV2UI_Idle_Interface *mUIIdleInterface;
   LV2UI_Show_Interface *mUIShowInterface;

   NumericTextCtrl *mDuration;

   bool mFactoryPresetsLoaded;
   RegistryPaths mFactoryPresetNames;
   wxArrayString mFactoryPresetUris;

   DECLARE_EVENT_TABLE()

   friend class LV2Wrapper;
   friend class LV2EffectSettingsDialog;
   friend class LV2EffectsModule;
};

inline wxString LilvString(const LilvNode *node)
{
   return wxString::FromUTF8(lilv_node_as_string(node));
};

inline wxString LilvString(LilvNode *node, bool free)
{
   wxString str = LilvString(node);
   if (free)
   {
      lilv_node_free(node);
   }

   return str;
};

class LV2Wrapper : public wxThreadHelper
{
public:
   typedef struct LV2Work
   {
      uint32_t size;
      const void *data;
   } LV2Work;

public:
   LV2Wrapper(LV2Effect *effect);
   virtual ~LV2Wrapper();

   LilvInstance *Instantiate(const LilvPlugin *plugin,
                             double sampleRrate,
                             std::vector<std::unique_ptr<LV2_Feature>> & features);

   void *Entry();

   LilvInstance *GetInstance();
   
   LV2_Handle GetHandle();

   float GetLatency();

   void SetFreeWheeling(bool enable);

   void SetSampleRate();

   void SetBlockSize();

   void ConnectPorts(float **inbuf, float **outbuf);

   void SendResponses();

   static LV2_Worker_Status schedule_work(LV2_Worker_Schedule_Handle handle,
                                          uint32_t size,
                                          const void *data);
   LV2_Worker_Status ScheduleWork(uint32_t size, const void *data);

   static LV2_Worker_Status respond(LV2_Worker_Respond_Handle handle,
                                    uint32_t size,
                                    const void *data);

   LV2_Worker_Status Respond(uint32_t size, const void *data);

private:
   LV2Effect *mEffect;
   LilvInstance *mInstance;
   LV2_Handle mHandle;

   wxMessageQueue<LV2Work> mRequests;
   wxMessageQueue<LV2Work> mResponses;

   // Options extension
   LV2_Options_Interface *mOptionsInterface;

   // State extension
   LV2_State_Interface *mStateInterface;

   // Worker extension
   LV2_Worker_Interface *mWorkerInterface;
   LV2_Worker_Schedule mWorkerSchedule;

   float mLatency;
   bool mFreeWheeling;
   bool mStopWorker;
};

#endif
