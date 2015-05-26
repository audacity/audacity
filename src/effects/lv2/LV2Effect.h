/**********************************************************************

  Audacity: A Digital Audio Editor

  LV2Effect.h

  Audacity(R) is copyright (c) 1999-2013 Audacity Team.
  License: GPL v2.  See License.txt.

*********************************************************************/

#include "../../Audacity.h"

#if USE_LV2

#include <wx/checkbox.h>
#include <wx/dialog.h>
#include <wx/dynarray.h>
#include <wx/event.h>
#include <wx/slider.h>
#include <wx/stattext.h>
#include <wx/string.h>
#include <wx/textctrl.h>

#include "lv2/lv2plug.in/ns/ext/atom/forge.h"
#include "lv2/lv2plug.in/ns/ext/data-access/data-access.h"
#include "lv2/lv2plug.in/ns/ext/options/options.h"
#include "lv2/lv2plug.in/ns/ext/uri-map/uri-map.h"
#include "lv2/lv2plug.in/ns/ext/urid/urid.h"
#include "lv2/lv2plug.in/ns/extensions/ui/ui.h"

#include <lilv/lilv.h>
#include <suil/suil.h>

#include "../../widgets/NumericTextCtrl.h"

#include "LoadLV2.h"

#define LV2EFFECTS_VERSION wxT("1.0.0.0")
#define LV2EFFECTS_FAMILY wxT("LV2")

/** A structure that contains information about a single LV2 plugin port. */
class LV2Port
{
public:
   LV2Port()
   {
      mInput = false;
      mToggle = false;
      mTrigger = false;
      mInteger = false;
      mSampleRate = false;
      mEnumeration = false;
      mLogarithmic = false;
      mHasLo = false;
      mHasHi = false;
   }

   uint32_t mIndex;
   wxString mSymbol;
   wxString mName;
   wxString mGroup;
   wxString mUnits;
   float mMin;
   float mMax;
   float mDef;
   float mVal;
   float mTmp;
   float mLo;
   float mHi;
   bool mHasLo;
   bool mHasHi;
   bool mInput;
   bool mToggle;
   bool mTrigger;
   bool mInteger;
   bool mSampleRate;
   bool mEnumeration;
   bool mLogarithmic;

   LilvPort *mPort;

   // ScalePoints
   wxArrayDouble mScaleValues;
   wxArrayString mScaleLabels;
};

WX_DECLARE_OBJARRAY(LV2Port, LV2PortArray);
WX_DECLARE_STRING_HASH_MAP(wxArrayInt, LV2GroupMap);
WX_DEFINE_ARRAY_PTR(LilvInstance *, LV2SlaveArray);

class LV2EffectSettingsDialog;

class LV2Effect : public wxEvtHandler,
                  public EffectClientInterface,
                  public EffectUIClientInterface
{
public:
   LV2Effect(const LilvPlugin *plug);
   virtual ~LV2Effect();

   // IdentInterface implementation

   virtual wxString GetPath();
   virtual wxString GetSymbol();
   virtual wxString GetName();
   virtual wxString GetVendor();
   virtual wxString GetVersion();
   virtual wxString GetDescription();

   // EffectIdentInterface implementation

   virtual EffectType GetType();
   virtual wxString GetFamily();
   virtual bool IsInteractive();
   virtual bool IsDefault();
   virtual bool IsLegacy();
   virtual bool SupportsRealtime();
   virtual bool SupportsAutomation();

   // EffectClientInterface implementation

   virtual bool SetHost(EffectHostInterface *host);

   virtual int GetAudioInCount();
   virtual int GetAudioOutCount();

   virtual int GetMidiInCount();
   virtual int GetMidiOutCount();

   virtual void SetSampleRate(sampleCount rate);
   virtual sampleCount SetBlockSize(sampleCount maxBlockSize);

   virtual sampleCount GetLatency();
   virtual sampleCount GetTailSize();

   virtual bool IsReady();
   virtual bool ProcessInitialize(sampleCount totalLen, ChannelNames chanMap = NULL);
   virtual bool ProcessFinalize();
   virtual sampleCount ProcessBlock(float **inbuf, float **outbuf, sampleCount size);

   virtual bool RealtimeInitialize();
   virtual bool RealtimeAddProcessor(int numChannels, float sampleRate);
   virtual bool RealtimeFinalize();
   virtual bool RealtimeSuspend();
   virtual bool RealtimeResume();
   virtual bool RealtimeProcessStart();
   virtual sampleCount RealtimeProcess(int group,
                                       float **inbuf,
                                       float **outbuf,
                                       sampleCount numSamples);
   virtual bool RealtimeProcessEnd();

   virtual bool ShowInterface(wxWindow *parent, bool forceModal = false);

   virtual bool GetAutomationParameters(EffectAutomationParameters & parms);
   virtual bool SetAutomationParameters(EffectAutomationParameters & parms);

   // EffectUIClientInterface implementation

   virtual void SetHostUI(EffectUIHostInterface *host);
   virtual bool PopulateUI(wxWindow *parent);
   virtual bool IsGraphicalUI();
   virtual bool ValidateUI();
   virtual bool HideUI();
   virtual bool CloseUI();

   virtual bool LoadUserPreset(const wxString & name);
   virtual bool SaveUserPreset(const wxString & name);

   virtual wxArrayString GetFactoryPresets();
   virtual bool LoadFactoryPreset(int id);
   virtual bool LoadFactoryDefaults();

   virtual bool CanExportPresets();
   virtual void ExportPresets();
   virtual void ImportPresets();

   virtual bool HasOptions();
   virtual void ShowOptions();

   // LV2Effect implementation

private:
   bool Load();
   void Unload();

   bool LoadParameters(const wxString & group);
   bool SaveParameters(const wxString & group);

   LilvInstance *InitInstance(float sampleRate);
   void FreeInstance(LilvInstance *handle);

   static uint32_t uri_to_id(LV2_URI_Map_Callback_Data callback_data,
                             const char *map,
                             const char *uri);

   static LV2_URID urid_map(LV2_URID_Map_Handle handle, const char *uri);
   LV2_URID URID_Map(const char *uri);

   static const char *urid_unmap(LV2_URID_Unmap_Handle handle, LV2_URID urid);
   const char *URID_Unmap(LV2_URID urid);

   static int ui_resize(LV2UI_Feature_Handle handle, int width, int height);
   int UIResize(int width, int height);

   LV2_Options_Option *AddOption(const char *key, uint32_t size, const char *type, void *value);
   LV2_Feature *AddFeature(const char *uri, void *data);

   bool BuildFancy();
   bool BuildPlain();

   bool TransferDataToWindow();
   bool TransferDataFromWindow();
   void SetSlider(wxSlider *slider, const LV2Port & ctrl);

   void OnTrigger(wxCommandEvent & evt);
   void OnToggle(wxCommandEvent & evt);
   void OnChoice(wxCommandEvent & evt);
   void OnText(wxCommandEvent & evt);
   void OnSlider(wxCommandEvent & evt);

   void OnIdle(wxIdleEvent & evt);

   static void suil_write_func(SuilController controller,
                               uint32_t       port_index,
                               uint32_t       buffer_size,
                               uint32_t       protocol,
                               const void     *buffer);

   void UIWrite(uint32_t port_index,
                uint32_t buffer_size,
                uint32_t protocol,
                const void *buffer);

   static void set_value_func(const char *port_symbol,
                              void       *user_data,
                              const void *value,
                              uint32_t   size,
                              uint32_t   type);

   void SetPortValue(const char *port_symbol,
                     const void *value,
                     uint32_t   size,
                     uint32_t   type);

private:
   // Declare the static URI nodes
   #undef URI
   #define URI(n, u) static LilvNode *n;
   URILIST;

   const LilvPlugin *mPlug;

   EffectHostInterface *mHost;

   int mBlockSize;
   double mSampleRate;

   wxLongToLongHashMap mControlsMap;
   LV2PortArray mControls;
   wxArrayInt mAudioInputs;
   wxArrayInt mAudioOutputs;

   LV2GroupMap mGroupMap;
   wxArrayString mGroups;

   bool mUseLatency;
   int mLatencyPort;
   bool mLatencyDone;
   float mLatency;

   LilvInstance *mMaster;
   LilvInstance *mProcess;
   LV2SlaveArray mSlaves;

   float **mMasterIn;
   float **mMasterOut;
   sampleCount mNumSamples;

   double mLength;

   wxDialog *mDialog;
   wxWindow *mParent;
   EffectUIHostInterface *mUIHost;

   bool mUseGUI;
   wxWindow *mContainer;

   char **mURIMap;
   int mNumURIMap;

   LV2_URI_Map_Feature mUriMapFeature;
   LV2_URID_Map mURIDMapFeature;
   LV2_URID_Unmap mURIDUnmapFeature;
   LV2UI_Resize mUIResizeFeature;
   LV2_Extension_Data_Feature mExtDataFeature;
   
   LV2_Options_Option *mBlockSizeOption;
   LV2_Options_Option *mSampleRateOption;

   LV2_Options_Interface *mOptionsInterface;
   LV2_Options_Option *mOptions;
   int mNumOptions;

   LV2_Feature **mFeatures;
   int mNumFeatures;

   LV2_Feature *mInstanceAccessFeature;
   LV2_Feature *mParentFeature;

   const LV2UI_Idle_Interface *mIdleFeature;

   SuilHost *mSuilHost;
   SuilInstance *mSuilInstance;

   NumericTextCtrl *mDuration;
   wxSlider **mSliders;
   wxTextCtrl **mFields;

   bool mFactoryPresetsLoaded;
   wxArrayString mFactoryPresetNames;
   wxArrayString mFactoryPresetUris;

   DECLARE_EVENT_TABLE();

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


#endif
