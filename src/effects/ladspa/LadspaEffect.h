/**********************************************************************

  Audacity: A Digital Audio Editor

  LadspaEffect.h

  Dominic Mazzoni

**********************************************************************/

class wxSlider;
class wxStaticText;
class wxTextCtrl;
class wxCheckBox;

#include <wx/dialog.h>

#include "audacity/EffectInterface.h"
#include "audacity/ModuleInterface.h"
#include "audacity/PluginInterface.h"

#include "../../widgets/NumericTextCtrl.h"

#include "ladspa.h"

#define LADSPAEFFECTS_VERSION wxT("1.0.0.0")
#define LADSPAEFFECTS_FAMILY wxT("LADSPA")

///////////////////////////////////////////////////////////////////////////////
//
// LadspaEffect
//
///////////////////////////////////////////////////////////////////////////////

WX_DEFINE_ARRAY_PTR(LADSPA_Handle, LadspaSlaveArray);

class LadspaEffectMeter;

class LadspaEffect : public wxEvtHandler,
                     public EffectClientInterface,
                     public EffectUIClientInterface
{
public:
   LadspaEffect(const wxString & path, int index);
   virtual ~LadspaEffect();

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
   virtual sampleCount ProcessBlock(float **inBlock, float **outBlock, sampleCount blockLen);

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

   virtual bool LoadUserPreset(const wxString & name);
   virtual bool SaveUserPreset(const wxString & name);

   virtual wxArrayString GetFactoryPresets();
   virtual bool LoadFactoryPreset(int id);
   virtual bool LoadFactoryDefaults();

   // EffectUIClientInterface implementation

   virtual void SetHostUI(EffectUIHostInterface *host);
   virtual bool PopulateUI(wxWindow *parent);
   virtual bool IsGraphicalUI();
   virtual bool ValidateUI();
   virtual bool HideUI();
   virtual bool CloseUI();

   virtual bool CanExportPresets();
   virtual void ExportPresets();
   virtual void ImportPresets();

   virtual bool HasOptions();
   virtual void ShowOptions();

   // LadspaEffect implementation

private:
   bool Load();
   void Unload();

   bool LoadParameters(const wxString & group);
   bool SaveParameters(const wxString & group);

   LADSPA_Handle InitInstance(float sampleRate);
   void FreeInstance(LADSPA_Handle handle);

   void OnCheckBox(wxCommandEvent & evt);
   void OnSlider(wxCommandEvent & evt);
   void OnTextCtrl(wxCommandEvent & evt);
   void RefreshControls(bool outputOnly = false);

private:

   wxString mPath;
   int mIndex;
   EffectHostInterface *mHost;

   wxDynamicLibrary mLib;
   const LADSPA_Descriptor *mData;

   wxString pluginName;

   bool mReady;

   LADSPA_Handle mMaster;

   sampleCount mSampleRate;
   sampleCount mBlockSize;
   sampleCount mUserBlockSize;

   bool mInteractive;

   int mAudioIns;
   unsigned long *mInputPorts;

   int mAudioOuts;
   unsigned long *mOutputPorts;

   int mNumInputControls;
   float *mInputControls;
   int mNumOutputControls;
   float *mOutputControls;

   bool mUseLatency;
   int mLatencyPort;
   bool mLatencyDone;

   // Realtime processing
   LadspaSlaveArray mSlaves;

   EffectUIHostInterface *mUIHost;

   NumericTextCtrl *mDuration;
   wxDialog *mDialog;
   wxWindow *mParent;
   wxSlider **mSliders;
   wxTextCtrl **mFields;
   wxStaticText **mLabels;
   wxCheckBox **mToggles;
   LadspaEffectMeter **mMeters;

   DECLARE_EVENT_TABLE();

   friend class LadspaEffectsModule;
};

///////////////////////////////////////////////////////////////////////////////
//
// LadspaEffectsModule
//
///////////////////////////////////////////////////////////////////////////////

class LadspaEffectsModule : public ModuleInterface
{
public:
   LadspaEffectsModule(ModuleManagerInterface *moduleManager, const wxString *path);
   virtual ~LadspaEffectsModule();

   // IdentInterface implementatino

   virtual wxString GetPath();
   virtual wxString GetSymbol();
   virtual wxString GetName();
   virtual wxString GetVendor();
   virtual wxString GetVersion();
   virtual wxString GetDescription();

   // ModuleInterface implementation

   virtual bool Initialize();
   virtual void Terminate();

   virtual bool AutoRegisterPlugins(PluginManagerInterface & pm);
   virtual wxArrayString FindPlugins(PluginManagerInterface & pm);
   virtual bool RegisterPlugin(PluginManagerInterface & pm, const wxString & path);

   virtual bool IsPluginValid(const wxString & path);

   virtual IdentInterface *CreateInstance(const wxString & path);
   virtual void DeleteInstance(IdentInterface *instance);

   // LadspaEffectModule implementation

   wxArrayString GetSearchPaths();

private:
   ModuleManagerInterface *mModMan;
   wxString mPath;
};

