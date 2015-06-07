/**********************************************************************

  Audacity: A Digital Audio Editor

  Effect.cpp

  Dominic Mazzoni
  Vaughan Johnson
  Martyn Shaw

*******************************************************************//**

\class Effect
\brief Base class for many of the effects in Audacity.

*//****************************************************************//**

\class EffectDialog
\brief New (Jun-2006) base class for effects dialogs.  Likely to get
greater use in future.

*//*******************************************************************/

#include "../Audacity.h"

#include <wx/defs.h>
#include <wx/hashmap.h>
#include <wx/msgdlg.h>
#include <wx/sizer.h>
#include <wx/stockitem.h>
#include <wx/string.h>
#include <wx/tglbtn.h>
#include <wx/timer.h>
#include <wx/utils.h>
#include <wx/log.h>

#include "audacity/ConfigInterface.h"

#include "Effect.h"
#include "../AudioIO.h"
#include "../Mix.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../WaveTrack.h"
#include "../toolbars/ControlToolBar.h"
#include "../widgets/AButton.h"
#include "../widgets/ProgressDialog.h"
#include "../ondemand/ODManager.h"
#include "TimeWarper.h"
#include "nyquist/Nyquist.h"

#if defined(__WXMAC__)
#include <wx/mac/private.h>
#endif

static const int kDummyID = 30000;
static const int kSaveAsID = 30001;
static const int kImportID = 30002;
static const int kExportID = 30003;
static const int kDefaultsID = 30004;
static const int kOptionsID = 30005;
static const int kUserPresetsDummyID = 30006;
static const int kDeletePresetDummyID = 30007;
static const int kMenuID = 30100;
static const int kEnableID = 30101;
static const int kPlayID = 30102;
static const int kRewindID = 30103;
static const int kFFwdID = 30104;
static const int kPlaybackID = 30105;
static const int kCaptureID = 30106;
static const int kUserPresetsID = 31000;
static const int kDeletePresetID = 32000;
static const int kFactoryPresetsID = 33000;

const wxString Effect::kUserPresetIdent = wxT("User Preset:");
const wxString Effect::kFactoryPresetIdent = wxT("Factory Preset:");
const wxString Effect::kCurrentSettingsIdent = wxT("<Current Settings>");
const wxString Effect::kFactoryDefaultsIdent = wxT("<Factory Defaults>");

WX_DECLARE_VOIDPTR_HASH_MAP( bool, t2bHash );

Effect::Effect()
{
   mParent = NULL;

   mClient = NULL;

   mWarper = NULL;

   mTracks = NULL;
   mOutputTracks = NULL;
   mOutputTracksType = Track::None;
   mT0 = 0.0;
   mT1 = 0.0;
   mDuration = 0.0;
   mIsPreview = false;
   mIsLinearEffect = false;
   mPreviewWithNotSelected = false;
   mPreviewFullSelection = false;
   mNumTracks = 0;
   mNumGroups = 0;
   mProgress = NULL;

   mRealtimeSuspendLock.Enter();
   mRealtimeSuspendCount = 1;    // Effects are initially suspended
   mRealtimeSuspendLock.Leave();

   mUIParent = NULL;
   mUIDialog = NULL;

   mNumAudioIn = 0;
   mNumAudioOut = 0;

   mInBuffer = NULL;
   mOutBuffer = NULL;
   mInBufPos = NULL;
   mOutBufPos = NULL;

   mBufferSize = 0;
   mBlockSize = 0;
   mNumChannels = 0;

   mUIDebug = false;

   AudacityProject *p = GetActiveProject();
   mProjectRate = p ? p->GetRate() : 44100;

   mIsBatch = false;
}

Effect::~Effect()
{
   if (mOutputTracks)
   {
      delete mOutputTracks;
   }

   if (mWarper != NULL)
   {
      delete mWarper;
   }

   if (mUIDialog)
   {
      mUIDialog->Close();
   }
}

// EffectIdentInterface implementation

EffectType Effect::GetType()
{
   if (mClient)
   {
      return mClient->GetType();
   }

   return EffectTypeNone;
}

wxString Effect::GetPath()
{
   if (mClient)
   {
      return mClient->GetPath();
   }

   return BUILTIN_EFFECT_PREFIX + GetSymbol();
}

wxString Effect::GetSymbol()
{
   if (mClient)
   {
      return mClient->GetSymbol();
   }

   return wxEmptyString;
}

wxString Effect::GetName()
{
   if (mClient)
   {
      return mClient->GetName();
   }

   return GetSymbol();
}

wxString Effect::GetVendor()
{
   if (mClient)
   {
      return mClient->GetVendor();
   }

   return XO("Audacity");
}

wxString Effect::GetVersion()
{
   if (mClient)
   {
      return mClient->GetVersion();
   }

   return AUDACITY_VERSION_STRING;
}

wxString Effect::GetDescription()
{
   if (mClient)
   {
      return mClient->GetDescription();
   }

   return wxEmptyString;
}

wxString Effect::GetFamily()
{
   if (mClient)
   {
      return mClient->GetFamily();
   }

   return XO("Audacity");
}

bool Effect::IsInteractive()
{
   if (mClient)
   {
      return mClient->IsInteractive();
   }

   return true;
}

bool Effect::IsDefault()
{
   if (mClient)
   {
      return mClient->IsDefault();
   }

   return true;
}

bool Effect::IsLegacy() 
{
   if (mClient)
   {
      return false;
   }

   return true;
}

bool Effect::SupportsRealtime()
{
   if (mClient)
   {
      return mClient->SupportsRealtime();
   }

   return false;
}

bool Effect::SupportsAutomation()
{
   if (mClient)
   {
      return mClient->SupportsAutomation();
   }

   return true;
}

// EffectClientInterface implementation

bool Effect::SetHost(EffectHostInterface *host)
{
   if (mClient)
   {
      return mClient->SetHost(host);
   }

   return true;
}

int Effect::GetAudioInCount()
{
   if (mClient)
   {
      return mClient->GetAudioInCount();
   }

   return 0;
}

int Effect::GetAudioOutCount()
{
   if (mClient)
   {
      return mClient->GetAudioOutCount();
   }

   return 0;
}

int Effect::GetMidiInCount()
{
   if (mClient)
   {
      return mClient->GetMidiInCount();
   }

   return 0;
}

int Effect::GetMidiOutCount()
{
   if (mClient)
   {
      return mClient->GetMidiOutCount();
   }

   return 0;
}

void Effect::SetSampleRate(sampleCount rate)
{
   if (mClient)
   {
      mClient->SetSampleRate(rate);
   }

   mSampleRate = rate;
}

sampleCount Effect::SetBlockSize(sampleCount maxBlockSize)
{
   if (mClient)
   {
      return mClient->SetBlockSize(maxBlockSize);
   }

   mBlockSize = maxBlockSize;

   return mBlockSize;
}

sampleCount Effect::GetLatency()
{
   if (mClient)
   {
      return mClient->GetLatency();
   }

   return 0;
}

sampleCount Effect::GetTailSize()
{
   if (mClient)
   {
      return mClient->GetTailSize();
   }

   return 0;
}

bool Effect::IsReady()
{
   if (mClient)
   {
      return mClient->IsReady();
   }

   return true;
}

bool Effect::ProcessInitialize(sampleCount totalLen, ChannelNames chanMap)
{
   if (mClient)
   {
      return mClient->ProcessInitialize(totalLen, chanMap);
   }

   return true;
}

bool Effect::ProcessFinalize()
{
   if (mClient)
   {
      return mClient->ProcessFinalize();
   }

   return true;
}

sampleCount Effect::ProcessBlock(float **inBlock, float **outBlock, sampleCount blockLen)
{
   if (mClient)
   {
      return mClient->ProcessBlock(inBlock, outBlock, blockLen);
   }

   return 0;
}

bool Effect::RealtimeInitialize()
{
   if (mClient)
   {
      mBlockSize = mClient->SetBlockSize(512);
      return mClient->RealtimeInitialize();
   }

   return false;
}

bool Effect::RealtimeAddProcessor(int numChannels, float sampleRate)
{
   if (mClient)
   {
      return mClient->RealtimeAddProcessor(numChannels, sampleRate);
   }

   return true;
}

bool Effect::RealtimeFinalize()
{
   if (mClient)
   {
      return mClient->RealtimeFinalize();
   }

   return false;
}

bool Effect::RealtimeSuspend()
{
   if (mClient)
   {
      if (mClient->RealtimeSuspend())
      {
         mRealtimeSuspendLock.Enter();
         mRealtimeSuspendCount++;
         mRealtimeSuspendLock.Leave();
         return true;
      }
   }

   return false;
}

bool Effect::RealtimeResume()
{
   if (mClient)
   {
      if (mClient->RealtimeResume())
      {
         mRealtimeSuspendLock.Enter();
         mRealtimeSuspendCount--;
         mRealtimeSuspendLock.Leave();
         return true;
      }
   }

   return false;
}

bool Effect::RealtimeProcessStart()
{
   if (mClient)
   {
      return mClient->RealtimeProcessStart();
   }

   return true;
}

sampleCount Effect::RealtimeProcess(int group,
                                    float **inbuf,
                                    float **outbuf,
                                    sampleCount numSamples)
{
   if (mClient)
   {
      return mClient->RealtimeProcess(group, inbuf, outbuf, numSamples);
   }

   return 0;
}

bool Effect::RealtimeProcessEnd()
{
   if (mClient)
   {
      return mClient->RealtimeProcessEnd();
   }

   return true;
}

bool Effect::ShowInterface(wxWindow *parent, bool forceModal)
{
   if (!IsInteractive())
   {
      return true;
   }

   if (mUIDialog)
   {
      mUIDialog->Close(true);
      return false;
   }

   if (mClient)
   {
      return mClient->ShowInterface(parent, forceModal);
   }

   mParent = parent;

   mUIDialog = CreateUI(parent, this);
   if (!mUIDialog)
   {
      return false;
   }

   mUIDialog->Layout();
   mUIDialog->Fit();
   mUIDialog->SetMinSize(mUIDialog->GetSize());

   if (SupportsRealtime() && !forceModal)
   {
      mUIDialog->Show();

      // Return false to bypass effect processing
      return false;
   }

   bool res = mUIDialog->ShowModal() != 0;
   mUIDialog = NULL;
   mParent = NULL;

   return res;
}

bool Effect::GetAutomationParameters(EffectAutomationParameters & parms)
{
   if (mClient)
   {
      return mClient->GetAutomationParameters(parms);
   }

   return true;
}

bool Effect::SetAutomationParameters(EffectAutomationParameters & parms)
{
   if (mClient)
   {
      return mClient->SetAutomationParameters(parms);
   }

   return true;
}

bool Effect::LoadUserPreset(const wxString & name)
{
   if (mClient)
   {
      return mClient->LoadUserPreset(name);
   }

   wxString parms;
   if (!GetPrivateConfig(name, wxT("Parameters"), parms))
   {
      return false;
   }

   return SetAutomationParameters(parms);
}

bool Effect::SaveUserPreset(const wxString & name)
{
   if (mClient)
   {
      return mClient->SaveUserPreset(name);
   }

   wxString parms;
   if (!GetAutomationParameters(parms))
   {
      return false;
   }

   return SetPrivateConfig(name, wxT("Parameters"), parms);
}

wxArrayString Effect::GetFactoryPresets()
{
   if (mClient)
   {
      return mClient->GetFactoryPresets();
   }

   return wxArrayString();
}

bool Effect::LoadFactoryPreset(int id)
{
   if (mClient)
   {
      return mClient->LoadFactoryPreset(id);
   }

   return true;
}

bool Effect::LoadFactoryDefaults()
{
   if (mClient)
   {
      return mClient->LoadFactoryDefaults();
   }

   return LoadUserPreset(GetFactoryDefaultsGroup());
}

// EffectUIClientInterface implementation

void Effect::SetHostUI(EffectUIHostInterface *WXUNUSED(host))
{
}

bool Effect::PopulateUI(wxWindow *parent)
{
   mUIParent = parent;
   mUIParent->PushEventHandler(this);

//   LoadUserPreset(GetCurrentSettingsGroup());

   ShuttleGui S(mUIParent, eIsCreating);
   PopulateOrExchange(S);

   mUIParent->SetMinSize(mUIParent->GetSizer()->GetMinSize());

   return true;
}

bool Effect::IsGraphicalUI()
{
   return false;
}

bool Effect::ValidateUI()
{
   return mUIParent->Validate();
}

bool Effect::HideUI()
{
   return true;
}

bool Effect::CloseUI()
{
   mUIParent->RemoveEventHandler(this);

   mUIParent = NULL;

   return true;
}

bool Effect::CanExportPresets()
{
   return false;
}

void Effect::ExportPresets()
{
}

void Effect::ImportPresets()
{
}

bool Effect::HasOptions()
{
   return false;
}

void Effect::ShowOptions()
{
}

// EffectHostInterface implementation

double Effect::GetDefaultDuration()
{
   return 30.0;
}

double Effect::GetDuration()
{
   if (mDuration < 0.0)
   {
      mDuration = 0.0;
   }

   return mDuration;
}

wxString Effect::GetDurationFormat()
{
   return mDurationFormat;
}

void Effect::SetDuration(double seconds)
{
   if (seconds < 0.0)
   {
      seconds = 0.0;
   }

   if (GetType() == EffectTypeGenerate)
   {
      SetPrivateConfig(GetCurrentSettingsGroup(), wxT("LastUsedDuration"), seconds);
   }

   mDuration = seconds;
   mT1 = mT0 + mDuration;

   mIsSelection = false;

   return;
}

bool Effect::Apply()
{
   // This is absolute hackage...but easy and I can't think of another way just now.
   //
   // It should callback to the EffectManager to kick off the processing
   return GetActiveProject()->OnEffect(GetID(), AudacityProject::OnEffectFlags::kConfigured);
}

void Effect::Preview()
{
   Preview(false);
}

wxDialog *Effect::CreateUI(wxWindow *parent, EffectUIClientInterface *client)
{
   EffectUIHost *dlg = new EffectUIHost(parent, this, client);

   if (dlg->Initialize())
   {
      return dlg;
   }

   delete dlg;

   return NULL;
}

wxString Effect::GetUserPresetsGroup(const wxString & name)
{
   wxString group = wxT("UserPresets");
   if (!name.IsEmpty())
   {
      group += wxCONFIG_PATH_SEPARATOR + name;
   }

   return group;
}

wxString Effect::GetCurrentSettingsGroup()
{
   return wxT("CurrentSettings");
}

wxString Effect::GetFactoryDefaultsGroup()
{
   return wxT("FactoryDefaults");
}

wxString Effect::GetSavedStateGroup()
{
   return wxT("SavedState");
}

// ConfigClientInterface implementation
bool Effect::HasSharedConfigGroup(const wxString & group)
{
   return PluginManager::Get().HasSharedConfigGroup(GetID(), group);
}

bool Effect::GetSharedConfigSubgroups(const wxString & group, wxArrayString & subgroups)
{
   return PluginManager::Get().GetSharedConfigSubgroups(GetID(), group, subgroups);
}

bool Effect::GetSharedConfig(const wxString & group, const wxString & key, wxString & value, const wxString & defval)
{
   return PluginManager::Get().GetSharedConfig(GetID(), group, key, value, defval);
}

bool Effect::GetSharedConfig(const wxString & group, const wxString & key, int & value, int defval)
{
   return PluginManager::Get().GetSharedConfig(GetID(), group, key, value, defval);
}

bool Effect::GetSharedConfig(const wxString & group, const wxString & key, bool & value, bool defval)
{
   return PluginManager::Get().GetSharedConfig(GetID(), group, key, value, defval);
}

bool Effect::GetSharedConfig(const wxString & group, const wxString & key, float & value, float defval)
{
   return PluginManager::Get().GetSharedConfig(GetID(), group, key, value, defval);
}

bool Effect::GetSharedConfig(const wxString & group, const wxString & key, double & value, double defval)
{
   return PluginManager::Get().GetSharedConfig(GetID(), group, key, value, defval);
}

bool Effect::GetSharedConfig(const wxString & group, const wxString & key, sampleCount & value, sampleCount defval)
{
   return PluginManager::Get().GetSharedConfig(GetID(), group, key, value, defval);
}

bool Effect::SetSharedConfig(const wxString & group, const wxString & key, const wxString & value)
{
   return PluginManager::Get().SetSharedConfig(GetID(), group, key, value);
}

bool Effect::SetSharedConfig(const wxString & group, const wxString & key, const int & value)
{
   return PluginManager::Get().SetSharedConfig(GetID(), group, key, value);
}

bool Effect::SetSharedConfig(const wxString & group, const wxString & key, const bool & value)
{
   return PluginManager::Get().SetSharedConfig(GetID(), group, key, value);
}

bool Effect::SetSharedConfig(const wxString & group, const wxString & key, const float & value)
{
   return PluginManager::Get().SetSharedConfig(GetID(), group, key, value);
}

bool Effect::SetSharedConfig(const wxString & group, const wxString & key, const double & value)
{
   return PluginManager::Get().SetSharedConfig(GetID(), group, key, value);
}

bool Effect::SetSharedConfig(const wxString & group, const wxString & key, const sampleCount & value)
{
   return PluginManager::Get().SetSharedConfig(GetID(), group, key, value);
}

bool Effect::RemoveSharedConfigSubgroup(const wxString & group)
{
   return PluginManager::Get().RemoveSharedConfigSubgroup(GetID(), group);
}

bool Effect::RemoveSharedConfig(const wxString & group, const wxString & key)
{
   return PluginManager::Get().RemoveSharedConfig(GetID(), group, key);
}

bool Effect::HasPrivateConfigGroup(const wxString & group)
{
   return PluginManager::Get().HasPrivateConfigGroup(GetID(), group);
}

bool Effect::GetPrivateConfigSubgroups(const wxString & group, wxArrayString & subgroups)
{
   return PluginManager::Get().GetPrivateConfigSubgroups(GetID(), group, subgroups);
}

bool Effect::GetPrivateConfig(const wxString & group, const wxString & key, wxString & value, const wxString & defval)
{
   return PluginManager::Get().GetPrivateConfig(GetID(), group, key, value, defval);
}

bool Effect::GetPrivateConfig(const wxString & group, const wxString & key, int & value, int defval)
{
   return PluginManager::Get().GetPrivateConfig(GetID(), group, key, value, defval);
}

bool Effect::GetPrivateConfig(const wxString & group, const wxString & key, bool & value, bool defval)
{
   return PluginManager::Get().GetPrivateConfig(GetID(), group, key, value, defval);
}

bool Effect::GetPrivateConfig(const wxString & group, const wxString & key, float & value, float defval)
{
   return PluginManager::Get().GetPrivateConfig(GetID(), group, key, value, defval);
}

bool Effect::GetPrivateConfig(const wxString & group, const wxString & key, double & value, double defval)
{
   return PluginManager::Get().GetPrivateConfig(GetID(), group, key, value, defval);
}

bool Effect::GetPrivateConfig(const wxString & group, const wxString & key, sampleCount & value, sampleCount defval)
{
   return PluginManager::Get().GetPrivateConfig(GetID(), group, key, value, defval);
}

bool Effect::SetPrivateConfig(const wxString & group, const wxString & key, const wxString & value)
{
   return PluginManager::Get().SetPrivateConfig(GetID(), group, key, value);
}

bool Effect::SetPrivateConfig(const wxString & group, const wxString & key, const int & value)
{
   return PluginManager::Get().SetPrivateConfig(GetID(), group, key, value);
}

bool Effect::SetPrivateConfig(const wxString & group, const wxString & key, const bool & value)
{
   return PluginManager::Get().SetPrivateConfig(GetID(), group, key, value);
}

bool Effect::SetPrivateConfig(const wxString & group, const wxString & key, const float & value)
{
   return PluginManager::Get().SetPrivateConfig(GetID(), group, key, value);
}

bool Effect::SetPrivateConfig(const wxString & group, const wxString & key, const double & value)
{
   return PluginManager::Get().SetPrivateConfig(GetID(), group, key, value);
}

bool Effect::SetPrivateConfig(const wxString & group, const wxString & key, const sampleCount & value)
{
   return PluginManager::Get().SetPrivateConfig(GetID(), group, key, value);
}

bool Effect::RemovePrivateConfigSubgroup(const wxString & group)
{
   return PluginManager::Get().RemovePrivateConfigSubgroup(GetID(), group);
}

bool Effect::RemovePrivateConfig(const wxString & group, const wxString & key)
{
   return PluginManager::Get().RemovePrivateConfig(GetID(), group, key);
}

// Effect implementation

PluginID Effect::GetID()
{
   if (mClient)
   {
      return PluginManager::GetID(mClient);
   }

   return PluginManager::GetID(this);
}

bool Effect::Startup(EffectClientInterface *client)
{
   // Let destructor know we need to be shutdown
   mClient = client;

   // Set host so client startup can use our services
   if (!SetHost(this))
   {
      // Bail if the client startup fails
      mClient = NULL;
      return false;
   }

   mNumAudioIn = GetAudioInCount();
   mNumAudioOut = GetAudioOutCount();

   bool haveDefaults;
   GetPrivateConfig(GetFactoryDefaultsGroup(), wxT("Initialized"), haveDefaults, false);
   if (!haveDefaults)
   {
      SaveUserPreset(GetFactoryDefaultsGroup());
      SetPrivateConfig(GetFactoryDefaultsGroup(), wxT("Initialized"), true);
   }
   LoadUserPreset(GetCurrentSettingsGroup());

   return Startup();
}

bool Effect::Startup()
{
   return true;
}

bool Effect::GetAutomationParameters(wxString & parms)
{
   EffectAutomationParameters eap;

   if (mUIDialog && !TransferDataFromWindow())
   {
      return false;
   }

   if (!GetAutomationParameters(eap))
   {
      return false;
   }

   return eap.GetParameters(parms);
}

bool Effect::SetAutomationParameters(const wxString & parms)
{
   wxString preset = parms;
   bool success = false;
   if (preset.StartsWith(kUserPresetIdent))
   {
      preset.Replace(kUserPresetIdent, wxEmptyString, false);
      success = LoadUserPreset(GetUserPresetsGroup(preset));
   }
   else if (preset.StartsWith(kFactoryPresetIdent))
   {
      preset.Replace(kFactoryPresetIdent, wxEmptyString, false);
      wxArrayString presets = GetFactoryPresets();
      success = LoadFactoryPreset(presets.Index(preset));
   }
   else if (preset.StartsWith(kCurrentSettingsIdent))
   {
      preset.Replace(kCurrentSettingsIdent, wxEmptyString, false);
      success = LoadUserPreset(GetCurrentSettingsGroup());
   }
   else if (preset.StartsWith(kFactoryDefaultsIdent))
   {
      preset.Replace(kFactoryDefaultsIdent, wxEmptyString, false);
      success = LoadUserPreset(GetFactoryDefaultsGroup());
   }
   else
   {
      EffectAutomationParameters eap(parms);
      success = SetAutomationParameters(eap);
   }

   if (!success)
   {
      wxMessageBox(
         wxString::Format(
            _("Could not update effect \"%s\" with:\n%s"),
            GetName().c_str(),
            preset.c_str()
         )
      );

      return false;
   }

   if (!mUIDialog)
   {
      return true;
   }

   return TransferDataToWindow();
}

wxArrayString Effect::GetUserPresets()
{
   wxArrayString presets;

   GetPrivateConfigSubgroups(GetUserPresetsGroup(wxEmptyString), presets);

   presets.Sort();

   return presets;
}

bool Effect::HasCurrentSettings()
{
   return HasPrivateConfigGroup(GetCurrentSettingsGroup());
}

bool Effect::HasFactoryDefaults()
{
   return HasPrivateConfigGroup(GetFactoryDefaultsGroup());
}

wxString Effect::GetPreset(wxWindow * parent, const wxString & parms)
{
   EffectPresetsDialog dlg(parent, this);
   dlg.Layout();
   dlg.Fit();
   dlg.SetSize(dlg.GetMinSize());
   dlg.CenterOnParent();
   dlg.SetSelected(parms);

   if (dlg.ShowModal())
   {
      return dlg.GetSelected();
   }

   return wxEmptyString;
}

bool Effect::IsBatchProcessing()
{
   return mIsBatch;
}

void Effect::SetBatchProcessing(bool start)
{
   mIsBatch = start;

   if (start)
   {
      SaveUserPreset(GetSavedStateGroup());
   }
   else
   {
      LoadUserPreset(GetSavedStateGroup());
   }
}

bool Effect::DoEffect(wxWindow *parent,
                      double projectRate,
                      TrackList *list,
                      TrackFactory *factory,
                      SelectedRegion *selectedRegion,
                      bool shouldPrompt /* = true */)
{
   wxASSERT(selectedRegion->duration() >= 0.0);

   if (mOutputTracks)
   {
      delete mOutputTracks;
      mOutputTracks = NULL;
   }

   mFactory = factory;
   mProjectRate = projectRate;
   mParent = parent;
   mTracks = list;
   
   bool isSelection = false;

   mDuration = 0.0;

   if (GetType() == EffectTypeGenerate)
   {
      GetPrivateConfig(GetCurrentSettingsGroup(), wxT("LastUsedDuration"), mDuration, GetDefaultDuration());
   }

   mT0 = selectedRegion->t0();
   mT1 = selectedRegion->t1();
   if (mT1 > mT0)
   {
      // there is a selection: let's fit in there...
      // MJS: note that this is just for the TTC and is independent of the track rate
      // but we do need to make sure we have the right number of samples at the project rate
      double quantMT0 = QUANTIZED_TIME(mT0, mProjectRate);
      double quantMT1 = QUANTIZED_TIME(mT1, mProjectRate);
      mDuration = quantMT1 - quantMT0;
      mT1 = mT0 + mDuration;

      isSelection = true;
   }

   mDurationFormat = isSelection ? _("hh:mm:ss + samples") : _("hh:mm:ss + milliseconds");

#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   mF0 = selectedRegion->f0();
   mF1 = selectedRegion->f1();
   wxArrayString Names;
   if( mF0 != SelectedRegion::UndefinedFrequency )
      Names.Add(wxT("control-f0"));
   if( mF1 != SelectedRegion::UndefinedFrequency )
      Names.Add(wxT("control-f1"));
   SetPresetParameters( &Names, NULL );

#endif
   CountWaveTracks();

   // Note: Init may read parameters from preferences
   if (!Init())
   {
      return false;
   }

   // Prompting will be bypassed when applying an effect that has already 
   // been configured, e.g. repeating the last effect on a different selection.
   if (shouldPrompt && IsInteractive() && !PromptUser(parent))
   {
      return false;
   }

   bool returnVal = true;
   bool skipFlag = CheckWhetherSkipEffect();
   if (skipFlag == false)
   {
      mProgress = new ProgressDialog(GetName(),
                                     wxString::Format(_("Applying %s..."), GetName().c_str()),
                                     pdlgHideStopButton);
      returnVal = Process();
      delete mProgress;
      mProgress = NULL;
   }

   End();

   if (mOutputTracks)
   {
      delete mOutputTracks;
      mOutputTracks = NULL;
   }

   if (returnVal)
   {
      selectedRegion->setTimes(mT0, mT1);
   }

   return returnVal;
}

// All legacy effects should have this overridden
bool Effect::Init()
{
   return true;
}

// Remove this method once NoiseReduction gets migrated
bool Effect::PromptUser(wxWindow *parent)
{
   return ShowInterface(parent, IsBatchProcessing());
}

int Effect::GetPass()
{
   return mPass;
}

bool Effect::InitPass1()
{
   return true;
}

bool Effect::InitPass2()
{
   return false;
}

bool Effect::Process()
{
   CopyInputTracks(Track::All);
   bool bGoodResult = true;

   mPass = 1;
   if (InitPass1())
   {
      bGoodResult = ProcessPass();
      mPass = 2;
      if (bGoodResult && InitPass2())
      {
         bGoodResult = ProcessPass();
      }
   }

   ReplaceProcessedTracks(bGoodResult); 

   return bGoodResult;
}

bool Effect::ProcessPass()
{
   bool bGoodResult = true;
   bool isGenerator = GetType() == EffectTypeGenerate;
   bool editClipCanMove;
   gPrefs->Read(wxT("/GUI/EditClipCanMove"), &editClipCanMove, true);

   mInBuffer = NULL;
   mOutBuffer = NULL;

   ChannelName map[3];

   sampleCount prevBufferSize = 0;
   mBufferSize = 0;
   mBlockSize = 0;

   TrackListIterator iter(mOutputTracks);
   int count = 0;
   bool clear = false;
   Track* t = iter.First();

   for (t = iter.First(); t; t = iter.Next())
   {
      if (t->GetKind() != Track::Wave || !t->GetSelected())
      {
         if (t->IsSyncLockSelected())
         {
            t->SyncLockAdjust(mT1, mT0 + mDuration);
         }
         continue;
      }

      WaveTrack *left = (WaveTrack *)t;
      WaveTrack *right;
      sampleCount len;
      sampleCount leftStart;
      sampleCount rightStart;

      if (!isGenerator)
      {
         GetSamples(left, &leftStart, &len);
         mSampleCnt = len;
      }
      else
      {
         len = 0;
         leftStart = 0;
         mSampleCnt = left->TimeToLongSamples(mDuration);
      }

      mNumChannels = 1;

      if (left->GetChannel() == Track::LeftChannel)
      {
         map[0] = ChannelNameFrontLeft;
      }
      else if (left->GetChannel() == Track::RightChannel)
      {
         map[0] = ChannelNameFrontRight;
      }
      else
      {
         map[0] = ChannelNameMono;
      }
      map[1] = ChannelNameEOL;

      right = NULL;
      rightStart = 0;
      if (left->GetLinked() && mNumAudioIn > 1)
      {
         right = (WaveTrack *) iter.Next();
         if (!isGenerator)
         {
            GetSamples(right, &rightStart, &len);
         }
         clear = false;
         mNumChannels = 2;

         if (right->GetChannel() == Track::LeftChannel)
         {
            map[1] = ChannelNameFrontLeft;
         }
         else if (right->GetChannel() == Track::RightChannel)
         {
            map[1] = ChannelNameFrontRight;
         }
         else
         {
            map[1] = ChannelNameMono;
         }
         map[2] = ChannelNameEOL;
      }

      // Let the client know the sample rate
      SetSampleRate(left->GetRate());

      // Get the block size the client wants to use
      sampleCount max = left->GetMaxBlockSize() * 2;
      mBlockSize = SetBlockSize(max);

      // Calculate the buffer size to be at least the max rounded up to the clients
      // selected block size.
      prevBufferSize = mBufferSize;
      mBufferSize = ((max + (mBlockSize - 1)) / mBlockSize) * mBlockSize;

      // If the buffer size has changed, then (re)allocate the buffers
      if (prevBufferSize != mBufferSize)
      {
         // Get rid of any previous buffers
         if (mInBuffer)
         {
            for (int i = 0; i < mNumAudioIn; i++)
            {
               if (mInBuffer[i])
               {
                  delete [] mInBuffer[i];
               }
            }
            delete [] mInBuffer;
            delete [] mInBufPos;
         }

         // Always create the number of input buffers the client expects even if we don't have
         // the same number of channels.
         mInBufPos = new float *[mNumAudioIn];
         mInBuffer = new float *[mNumAudioIn];
         for (int i = 0; i < mNumAudioIn; i++)
         {
            mInBuffer[i] = new float[mBufferSize];
         }

         // We won't be using more than the first 2 buffers, so clear the rest (if any)
         for (int i = 2; i < mNumAudioIn; i++)
         {
            for (int j = 0; j < mBufferSize; j++)
            {
               mInBuffer[i][j] = 0.0;
            }
         }

         // Get rid of any previous buffers
         if (mOutBuffer)
         {
            for (int i = 0; i < mNumAudioOut; i++)
            {
               if (mOutBuffer[i])
               {
                  delete [] mOutBuffer[i];
               }
            }
            delete [] mOutBuffer;
            delete [] mOutBufPos;
         }

         // Always create the number of output buffers the client expects even if we don't have
         // the same number of channels.
         mOutBufPos = new float *[mNumAudioOut];
         mOutBuffer = new float *[mNumAudioOut];
         for (int i = 0; i < mNumAudioOut; i++)
         {
            // Output buffers get an extra mBlockSize worth to give extra room if
            // the plugin adds latency
            mOutBuffer[i] = new float[mBufferSize + mBlockSize];
         }
      }

      // (Re)Set the input buffer positions
      for (int i = 0; i < mNumAudioIn; i++)
      {
         mInBufPos[i] = mInBuffer[i];
      }

      // (Re)Set the output buffer positions
      for (int i = 0; i < mNumAudioOut; i++)
      {
         mOutBufPos[i] = mOutBuffer[i];
      }

      // Clear unused input buffers
      if (!right && !clear && mNumAudioIn > 1)
      {
         for (int j = 0; j < mBufferSize; j++)
         {
            mInBuffer[1][j] = 0.0;
         }
         clear = true;
      }

      // Go process the track(s)
      bGoodResult = ProcessTrack(count, map, left, right, leftStart, rightStart, len);
      if (!bGoodResult)
      {
         break;
      }

      count++;
   }

   if (mOutBuffer)
   {
      for (int i = 0; i < mNumAudioOut; i++)
      {
         delete [] mOutBuffer[i];
      }
      delete [] mOutBuffer;
      delete [] mOutBufPos;
      mOutBuffer = NULL;
      mOutBufPos = NULL;
   }

   if (mInBuffer)
   {
      for (int i = 0; i < mNumAudioIn; i++)
      {
         delete [] mInBuffer[i];
      }
      delete [] mInBuffer;
      delete [] mInBufPos;
      mInBuffer = NULL;
      mInBufPos = NULL;
   }

   if (bGoodResult && GetType() == EffectTypeGenerate)
   {
      mT1 = mT0 + mDuration;
   }

   return bGoodResult;
}

bool Effect::ProcessTrack(int count,
                          ChannelNames map,
                          WaveTrack *left,
                          WaveTrack *right,
                          sampleCount leftStart,
                          sampleCount rightStart,
                          sampleCount len)
{
   bool rc = true;

   // Give the plugin a chance to initialize
   if (!ProcessInitialize(len, map))
   {
      return false;
   }

   // For each input block of samples, we pass it to the effect along with a
   // variable output location.  This output location is simply a pointer into a
   // much larger buffer.  This reduces the number of calls required to add the
   // samples to the output track.
   //
   // Upon return from the effect, the output samples are "moved to the left" by
   // the number of samples in the current latency setting, effectively removing any
   // delay introduced by the effect.
   //
   // At the same time the total number of delayed samples are gathered and when
   // there is no further input data to process, the loop continues to call the
   // effect with an empty input buffer until the effect has had a chance to 
   // return all of the remaining delayed samples.
   sampleCount inLeftPos = leftStart;
   sampleCount inRightPos = rightStart;
   sampleCount outLeftPos = leftStart;
   sampleCount outRightPos = rightStart;

   sampleCount inputRemaining = len;
   sampleCount delayRemaining = 0;
   sampleCount curBlockSize = 0;
   sampleCount curDelay = 0;

   sampleCount inputBufferCnt = 0;
   sampleCount outputBufferCnt = 0;
   bool cleared = false;

   int chans = wxMin(mNumAudioOut, mNumChannels);

   WaveTrack *genLeft = NULL;
   WaveTrack *genRight = NULL;
   sampleCount genLength = 0;
   bool isGenerator = GetType() == EffectTypeGenerate;
   bool isProcessor = GetType() == EffectTypeProcess;
   if (isGenerator)
   {
      double genDur;
      if (mIsPreview) {
         gPrefs->Read(wxT("/AudioIO/EffectsPreviewLen"), &genDur, 6.0);
         genDur = wxMin(mDuration, CalcPreviewInputLength(genDur));
      }
      else {
         genDur = mDuration;
      }

      genLength = left->GetRate() * genDur;
      delayRemaining = genLength;
      cleared = true;

      // Create temporary tracks
      genLeft = mFactory->NewWaveTrack(left->GetSampleFormat(), left->GetRate());
      if (right)
      {
         genRight = mFactory->NewWaveTrack(right->GetSampleFormat(), right->GetRate());
      }
   }

   // Call the effect until we run out of input or delayed samples
   while (inputRemaining || delayRemaining)
   {
      // Still working on the input samples
      if (inputRemaining)
      {
         // Need to refill the input buffers
         if (inputBufferCnt == 0)
         {
            // Calculate the number of samples to get
            inputBufferCnt = mBufferSize;
            if (inputBufferCnt > inputRemaining)
            {
               inputBufferCnt = inputRemaining;
            }

            // Fill the input buffers
            left->Get((samplePtr) mInBuffer[0], floatSample, inLeftPos, inputBufferCnt);
            if (right)
            {
               right->Get((samplePtr) mInBuffer[1], floatSample, inRightPos, inputBufferCnt);
            }

            // Reset the input buffer positions
            for (int i = 0; i < mNumChannels; i++)
            {
               mInBufPos[i] = mInBuffer[i];
            }
         }

         // Calculate the number of samples to process
         curBlockSize = mBlockSize;
         if (curBlockSize > inputRemaining)
         {
            // We've reached the last block...set current block size to what's left 
            curBlockSize = inputRemaining;
            inputRemaining = 0;

            // Clear the remainder of the buffers so that a full block can be passed
            // to the effect
            sampleCount cnt = mBlockSize - curBlockSize;
            for (int i = 0; i < mNumChannels; i++)
            {
               for (int j = 0 ; j < cnt; j++)
               {
                  mInBufPos[i][j + curBlockSize] = 0.0;
               }
            }

            // Might be able to use up some of the delayed samples
            if (delayRemaining)
            {
               // Don't use more than needed
               if (delayRemaining < cnt)
               {
                  cnt = delayRemaining;
               }
               delayRemaining -= cnt;
               curBlockSize += cnt;
            }
         }
      }
      // We've exhausted the input samples and are now working on the delay
      else if (delayRemaining)
      {
         // Calculate the number of samples to process
         curBlockSize = mBlockSize;
         if (curBlockSize > delayRemaining)
         {
            curBlockSize = delayRemaining;
         }
         delayRemaining -= curBlockSize;

         // From this point on, we only want to feed zeros to the plugin
         if (!cleared)
         {
            // Reset the input buffer positions
            for (int i = 0; i < mNumChannels; i++)
            {
               mInBufPos[i] = mInBuffer[i];

               // And clear
               for (int j = 0; j < mBlockSize; j++)
               {
                  mInBuffer[i][j] = 0.0;
               }
            }
            cleared = true;
         }
      }

      // Finally call the plugin to process the block
      sampleCount processed;
      try
      {
         processed = ProcessBlock(mInBufPos, mOutBufPos, curBlockSize);
      }
      catch(...)
      {
         if (genLeft)
         {
            delete genLeft;
         }

         if (genRight)
         {
            delete genRight;
         }

         return false;
      }
      wxASSERT(processed == curBlockSize);

      // Bump to next input buffer position
      if (inputRemaining)
      {
         for (int i = 0; i < mNumChannels; i++)
         {
            mInBufPos[i] += curBlockSize;
         }
         inputRemaining -= curBlockSize;
         inputBufferCnt -= curBlockSize;
      }

      // "ls" and "rs" serve as the input sample index for the left and
      // right channels when processing the input samples.  If we flip
      // over to processing delayed samples, they simply become counters
      // for the progress display.
      inLeftPos += curBlockSize;
      inRightPos += curBlockSize;

      // Get the current number of delayed samples and accumulate
      if (isProcessor)
      {
         sampleCount delay = GetLatency();
         curDelay += delay;
         delayRemaining += delay;

         // If the plugin has delayed the output by more samples than our current
         // block size, then we leave the output pointers alone.  This effectively
         // removes those delayed samples from the output buffer.
         if (curDelay >= curBlockSize)
         {
            curDelay -= curBlockSize;
            curBlockSize = 0;
         }
         // We have some delayed samples, at the beginning of the output samples,
         // so overlay them by shifting the remaining output samples.
         else if (curDelay > 0)
         {
            curBlockSize -= curDelay;
            for (int i = 0; i < chans; i++)
            {
               memmove(mOutBufPos[i], mOutBufPos[i] + curDelay, sizeof(float) * curBlockSize);
            }
            curDelay = 0;
         }
      }

      // Adjust the number of samples in the output buffers
      outputBufferCnt += curBlockSize;

      // Still have room in the output buffers
      if (outputBufferCnt < mBufferSize)
      {
         // Bump to next output buffer position
         for (int i = 0; i < chans; i++)
         {
            mOutBufPos[i] += curBlockSize;
         }
      }
      // Output buffers have filled
      else
      {
         if (isProcessor)
         {
            // Write them out
            left->Set((samplePtr) mOutBuffer[0], floatSample, outLeftPos, outputBufferCnt);
            if (right)
            {
               if (chans >= 2)
               {
                  right->Set((samplePtr) mOutBuffer[1], floatSample, outRightPos, outputBufferCnt);
               }
               else
               {
                  right->Set((samplePtr) mOutBuffer[0], floatSample, outRightPos, outputBufferCnt);
               }
            }
         }
         else if (isGenerator)
         {
            genLeft->Append((samplePtr) mOutBuffer[0], floatSample, outputBufferCnt);
            if (genRight)
            {
               genRight->Append((samplePtr) mOutBuffer[1], floatSample, outputBufferCnt);
            }
         }

         // Reset the output buffer positions
         for (int i = 0; i < chans; i++)
         {
            mOutBufPos[i] = mOutBuffer[i];
         }

         // Bump to the next track position
         outLeftPos += outputBufferCnt;
         outRightPos += outputBufferCnt;
         outputBufferCnt = 0;
      }

      if (mNumChannels > 1)
      {
         if (TrackGroupProgress(count, (inLeftPos - leftStart) / (double) (isGenerator ? genLength : len)))
         {
            rc = false;
            break;
         }
      }
      else
      {
         if (TrackProgress(count, (inLeftPos - leftStart) / (double) (isGenerator ? genLength : len)))
         {
            rc = false;
            break;
         }
      }
   }

   // Put any remaining output
   if (outputBufferCnt)
   {
      if (isProcessor)
      {
         left->Set((samplePtr) mOutBuffer[0], floatSample, outLeftPos, outputBufferCnt);
         if (right)
         {
            if (chans >= 2)
            {
               right->Set((samplePtr) mOutBuffer[1], floatSample, outRightPos, outputBufferCnt);
            }
            else
            {
               right->Set((samplePtr) mOutBuffer[0], floatSample, outRightPos, outputBufferCnt);
            }
         }
      }
      else if (isGenerator)
      {
         genLeft->Append((samplePtr) mOutBuffer[0], floatSample, outputBufferCnt);
         if (genRight)
         {
            genRight->Append((samplePtr) mOutBuffer[1], floatSample, outputBufferCnt);
         }
      }
   }

   if (isGenerator)
   {
      AudacityProject *p = GetActiveProject();
      StepTimeWarper *warper = new StepTimeWarper(mT0 + genLength, genLength - (mT1 - mT0));

      // Transfer the data from the temporary tracks to the actual ones
      genLeft->Flush();
      // mT1 gives us the new selection. We want to replace up to GetSel1().
      left->ClearAndPaste(mT0, p->GetSel1(), genLeft, true, true, warper);
      delete genLeft;

      if (genRight)
      {
         genRight->Flush();
         right->ClearAndPaste(mT0, mT1, genRight, true, true, warper);
         delete genRight;
      }

      delete warper;
   }

   // Allow the plugin to cleanup
   if (!ProcessFinalize())
   {
      return false;
   }

   return rc;
}

void Effect::End()
{
}

void Effect::PopulateOrExchange(ShuttleGui & WXUNUSED(S))
{
   return;
}

bool Effect::TransferDataToWindow()
{
   return true;
}

bool Effect::TransferDataFromWindow()
{
   return true;
}

bool Effect::EnableApply(bool enable)
{
   // May be called during initialization, so try to find the dialog
   wxWindow *dlg = mUIDialog;
   if (!dlg && mUIParent)
   {
      dlg = wxGetTopLevelParent(mUIParent);
   }

   if (dlg)
   {
      wxWindow *apply = dlg->FindWindow(wxID_APPLY);

      // Don't allow focus to get trapped
      if (!enable)
      {
         wxWindow *focus = dlg->FindFocus();
         if (focus == apply)
         {
            dlg->FindWindow(wxID_CLOSE)->SetFocus();
         }
      }

      apply->Enable(enable);
   }

   EnablePreview(enable);

   return enable;
}

bool Effect::EnablePreview(bool enable)
{
   // May be called during initialization, so try to find the dialog
   wxWindow *dlg = mUIDialog;
   if (!dlg && mUIParent)
   {
      dlg = wxGetTopLevelParent(mUIParent);
   }

   if (dlg)
   {
      wxWindow *play = dlg->FindWindow(kPlayID);
      if (play)
      {
         wxWindow *rewind = dlg->FindWindow(kRewindID);
         wxWindow *ffwd = dlg->FindWindow(kFFwdID);

         // Don't allow focus to get trapped
         if (!enable)
         {
            wxWindow *focus = dlg->FindFocus();
            if (focus && (focus == play || focus == rewind || focus == ffwd))
            {
               dlg->FindWindow(wxID_CLOSE)->SetFocus();
            }
         }

         play->Enable(enable);
         if (SupportsRealtime())
         {
            rewind->Enable(enable);
            ffwd->Enable(enable);
         }
      }
   }

   return enable;
}

void Effect::EnableDebug(bool enable)
{
   mUIDebug = enable;
}

void Effect::SetLinearEffectFlag(bool linearEffectFlag)
{
   mIsLinearEffect = linearEffectFlag;
}

void Effect::SetPreviewFullSelectionFlag(bool previewDurationFlag)
{
   mPreviewFullSelection = previewDurationFlag;
}


void Effect::IncludeNotSelectedPreviewTracks(bool includeNotSelected)
{
   mPreviewWithNotSelected = includeNotSelected;
}

bool Effect::TotalProgress(double frac)
{
   int updateResult = (mProgress ?
      mProgress->Update(frac) :
      eProgressSuccess);
   return (updateResult != eProgressSuccess);
}

bool Effect::TrackProgress(int whichTrack, double frac, wxString msg)
{
   int updateResult = (mProgress ?
      mProgress->Update(whichTrack + frac, (double) mNumTracks, msg) :
      eProgressSuccess);
   return (updateResult != eProgressSuccess);
}

bool Effect::TrackGroupProgress(int whichGroup, double frac)
{
   int updateResult = (mProgress ?
      mProgress->Update(whichGroup + frac, (double) mNumGroups) :
      eProgressSuccess);
   return (updateResult != eProgressSuccess);
}

void Effect::GetSamples(WaveTrack *track, sampleCount *start, sampleCount *len)
{
   double trackStart = track->GetStartTime();
   double trackEnd = track->GetEndTime();
   double t0 = mT0 < trackStart ? trackStart : mT0;
   double t1 = mT1 > trackEnd ? trackEnd : mT1;

#if 0
   if (GetType() & INSERT_EFFECT) {
      t1 = t0 + mDuration;
      if (mT0 == mT1) {
         // Not really part of the calculation, but convenient to put here
         bool bResult = track->InsertSilence(t0, t1);
         wxASSERT(bResult); // TO DO: Actually handle this.
      }
   }
#endif

   if (t1 > t0) {
      *start = track->TimeToLongSamples(t0);
      sampleCount end = track->TimeToLongSamples(t1);
      *len = (sampleCount)(end - *start);
   }
   else {
      *start = 0;
      *len  = 0;
   }
}

void Effect::SetTimeWarper(TimeWarper *warper)
{
   if (mWarper != NULL)
   {
      delete mWarper;
      mWarper = NULL;
   }

   wxASSERT(warper != NULL);
   mWarper = warper;
}

TimeWarper *Effect::GetTimeWarper()
{
   wxASSERT(mWarper != NULL);
   return mWarper;
}

//
// private methods
//
// Use these two methods to copy the input tracks to mOutputTracks, if
// doing the processing on them, and replacing the originals only on success (and not cancel).
// Copy the group tracks that have tracks selected
void Effect::CopyInputTracks(int trackType)
{
   // Reset map
   mIMap.Clear();
   mOMap.Clear();

   mOutputTracks = new TrackList(true);
   mOutputTracksType = trackType;

   //iterate over tracks of type trackType (All types if Track::All)
   TrackListOfKindIterator aIt(trackType, mTracks);
   t2bHash added;

   for (Track *aTrack = aIt.First(); aTrack; aTrack = aIt.Next())
   {
      // Include selected tracks, plus sync-lock selected tracks for Track::All.
      if (aTrack->GetSelected() ||
            (trackType == Track::All && aTrack->IsSyncLockSelected()))
      {
         Track *o = aTrack->Duplicate();
         mOutputTracks->Add(o);
         mIMap.Add(aTrack);
         mOMap.Add(o);
      }
   }
}

void Effect::AddToOutputTracks(Track *t)
{
   mOutputTracks->Add(t);
   mIMap.Add(NULL);
   mOMap.Add(t);
}

// If bGoodResult, replace mTracks tracks with successfully processed mOutputTracks copies.
// Else clear and delete mOutputTracks copies.
void Effect::ReplaceProcessedTracks(const bool bGoodResult)
{
   wxASSERT(mOutputTracks != NULL); // Make sure we at least did the CopyInputTracks().

   if (!bGoodResult) {
      // Processing failed or was cancelled so throw away the processed tracks.
      mOutputTracks->Clear(true); // true => delete the tracks

      // Reset map
      mIMap.Clear();
      mOMap.Clear();

      //TODO:undo the non-gui ODTask transfer
      return;
   }

   TrackListIterator iterOut(mOutputTracks);

   Track *x;
   size_t cnt = mOMap.GetCount();
   size_t i = 0;

   for (Track *o = iterOut.First(); o; o = x, i++) {
      // If tracks were removed from mOutputTracks, then there will be
      // tracks in the map that must be removed from mTracks.
      while (i < cnt && mOMap[i] != o) {
         Track *t = (Track *) mIMap[i];
         if (t) {
            mTracks->Remove(t, true);
         }
         i++;
      }

      // This should never happen
      wxASSERT(i < cnt);

      // Remove the track from the output list...don't delete it
      x = iterOut.RemoveCurrent(false);

      Track *t = (Track *) mIMap[i];
      if (t == NULL)
      {
         // This track is a new addition to output tracks; add it to mTracks
         mTracks->Add(o);
      }
      else
      {
         // Replace mTracks entry with the new track
         mTracks->Replace(t, o, false);

         // Swap the wavecache track the ondemand task uses, since now the new
         // one will be kept in the project
         if (ODManager::IsInstanceCreated()) {
            ODManager::Instance()->ReplaceWaveTrack((WaveTrack *)t,
                                                    (WaveTrack *)o);
         }

         // No longer need the original track
         delete t;
      }
   }

   // If tracks were removed from mOutputTracks, then there may be tracks
   // left at the end of the map that must be removed from mTracks.
   while (i < cnt) {
      Track *t = (Track *) mIMap[i];
      if (t) {
         mTracks->Remove((Track *)mIMap[i], true);
      }
      i++;
   }

   // Reset map
   mIMap.Clear();
   mOMap.Clear();

   // Make sure we processed everything
   wxASSERT(iterOut.First() == NULL);

   // The output list is no longer needed
   delete mOutputTracks;
   mOutputTracks = NULL;
   mOutputTracksType = Track::None;
}

void Effect::CountWaveTracks()
{
   mNumTracks = 0;
   mNumGroups = 0;

   TrackListOfKindIterator iter(Track::Wave, mTracks);
   Track *t = iter.First();

   while(t) {
      if (!t->GetSelected()) {
         t = iter.Next();
         continue;
      }

      if (t->GetKind() == Track::Wave) {
         mNumTracks++;
         if (!t->GetLinked())
            mNumGroups++;
      }
      t = iter.Next();
   }
}

double Effect::CalcPreviewInputLength(double previewLength)
{
   return previewLength;
}

// RealtimeAddProcessor and RealtimeProcess use the same method of
// determining the current processor index, so updates to one should
// be reflected in the other.
bool Effect::RealtimeAddProcessor(int group, int chans, float rate)
{
   int ichans = chans;
   int ochans = chans;
   int gchans = chans;

   // Reset processor index
   if (group == 0)
   {
      mCurrentProcessor = 0;
      mGroupProcessor.Clear();
   }

   // Remember the processor starting index
   mGroupProcessor.Add(mCurrentProcessor);

   // Call the client until we run out of input or output channels
   while (ichans > 0 && ochans > 0)
   {
      // If we don't have enough input channels to accomodate the client's
      // requirements, then we replicate the input channels until the
      // client's needs are met.
      if (ichans < mNumAudioIn)
      {
         // All input channels have been consumed
         ichans = 0;
      }
      // Otherwise fullfil the client's needs with as many input channels as possible.
      // After calling the client with this set, we will loop back up to process more
      // of the input/output channels.
      else if (ichans >= mNumAudioIn)
      {
         gchans = mNumAudioIn;
         ichans -= gchans;
      }

      // If we don't have enough output channels to accomodate the client's
      // requirements, then we provide all of the output channels and fulfill
      // the client's needs with dummy buffers.  These will just get tossed.
      if (ochans < mNumAudioOut)
      {
         // All output channels have been consumed
         ochans = 0;
      }
      // Otherwise fullfil the client's needs with as many output channels as possible.
      // After calling the client with this set, we will loop back up to process more
      // of the input/output channels.
      else if (ochans >= mNumAudioOut)
      {
         ochans -= mNumAudioOut;
      }

      // Add a new processor
      mClient->RealtimeAddProcessor(gchans, rate);

      // Bump to next processor
      mCurrentProcessor++;
   }

   return true;
}

// RealtimeAddProcessor and RealtimeProcess use the same method of
// determining the current processor group, so updates to one should
// be reflected in the other.
sampleCount Effect::RealtimeProcess(int group,
                                    int chans,
                                    float **inbuf,
                                    float **outbuf,
                                    sampleCount numSamples)
{
   //
   // The caller passes the number of channels to process and specifies
   // the number of input and output buffers.  There will always be the
   // same number of output buffers as there are input buffers.
   //
   // Effects always require a certain number of input and output buffers,
   // so if the number of channels we're curently processing are different
   // than what the effect expects, then we use a few methods of satisfying
   // the effects requirements.
   float **clientIn = (float **) alloca(mNumAudioIn * sizeof(float *));
   float **clientOut = (float **) alloca(mNumAudioOut * sizeof(float *));
   float *dummybuf = (float *) alloca(numSamples * sizeof(float));
   sampleCount len = 0;
   int ichans = chans;
   int ochans = chans;
   int gchans = chans;
   int indx = 0;
   int ondx = 0;

   int processor = mGroupProcessor[group];

   // Call the client until we run out of input or output channels
   while (ichans > 0 && ochans > 0)
   {
      // If we don't have enough input channels to accomodate the client's
      // requirements, then we replicate the input channels until the
      // client's needs are met.
      if (ichans < mNumAudioIn)
      {
         for (int i = 0; i < mNumAudioIn; i++)
         {
            if (indx == ichans)
            {
               indx = 0;
            }
            clientIn[i] = inbuf[indx++];
         }

         // All input channels have been consumed
         ichans = 0;
      }
      // Otherwise fullfil the client's needs with as many input channels as possible.
      // After calling the client with this set, we will loop back up to process more
      // of the input/output channels.
      else if (ichans >= mNumAudioIn)
      {
         gchans = 0;
         for (int i = 0; i < mNumAudioIn; i++, ichans--, gchans++)
         {
            clientIn[i] = inbuf[indx++];
         }
      }

      // If we don't have enough output channels to accomodate the client's
      // requirements, then we provide all of the output channels and fulfill
      // the client's needs with dummy buffers.  These will just get tossed.
      if (ochans < mNumAudioOut)
      {
         for (int i = 0; i < mNumAudioOut; i++)
         {
            if (i < ochans)
            {
               clientOut[i] = outbuf[i];
            }
            else
            {
               clientOut[i] = dummybuf;
            }
         }

         // All output channels have been consumed
         ochans = 0;
      }
      // Otherwise fullfil the client's needs with as many output channels as possible.
      // After calling the client with this set, we will loop back up to process more
      // of the input/output channels.
      else if (ochans >= mNumAudioOut)
      {
         for (int i = 0; i < mNumAudioOut; i++, ochans--)
         {
            clientOut[i] = outbuf[ondx++];
         }
      }

      // Finally call the plugin to process the block
      len = 0;
      for (sampleCount block = 0; block < numSamples; block += mBlockSize)
      {
         sampleCount cnt = (block + mBlockSize > numSamples ? numSamples - block : mBlockSize);
         len += mClient->RealtimeProcess(processor, clientIn, clientOut, cnt);

         for (int i = 0 ; i < mNumAudioIn; i++)
         {
            clientIn[i] += cnt;
         }

         for (int i = 0 ; i < mNumAudioOut; i++)
         {
            clientOut[i] += cnt;
         }
      }

      // Bump to next processor
      processor++;
   }

   return len;
}

bool Effect::IsRealtimeActive()
{
   return mRealtimeSuspendCount == 0;
}

bool Effect::IsHidden()
{
   return false;
}

void Effect::Preview(bool dryOnly)
{
    if (mIsLinearEffect)
       wxLogDebug(wxT("Linear Effect"));
    else
       wxLogDebug(wxT("Non-linear Effect"));

   if (mNumTracks == 0) { // nothing to preview
      return;
   }

   if (gAudioIO->IsBusy()) {
      return;
   }

   wxWindow *FocusDialog = wxWindow::FindFocus();

   double previewDuration;
   bool isNyquist = (GetFamily().IsSameAs(NYQUISTEFFECTS_FAMILY))? true : false;
   bool isGenerator = GetType() == EffectTypeGenerate;

   // Mix a few seconds of audio from all of the tracks
   double previewLen;
   gPrefs->Read(wxT("/AudioIO/EffectsPreviewLen"), &previewLen, 6.0);

   double rate = mProjectRate;

   if (isNyquist && isGenerator) {
      previewDuration = CalcPreviewInputLength(previewLen);
   }
   else {
      previewDuration = wxMin(mDuration, CalcPreviewInputLength(previewLen));
   }

   double t1 = mT0 + previewDuration;

   if ((t1 > mT1) && !(isNyquist && isGenerator)) {
      t1 = mT1;
   }

   if (t1 <= mT0)
      return;

   bool success = true;
   WaveTrack *mixLeft = NULL;
   WaveTrack *mixRight = NULL;
   double oldT0 = mT0;
   double oldT1 = mT1;
   // Most effects should stop at t1.
   if (!mPreviewFullSelection)
      mT1 = t1;

   // Save the original track list
   TrackList *saveTracks = mTracks;

   // Build new tracklist from rendering tracks
   mTracks = new TrackList();

   // Linear Effect preview optimised by pre-mixing to one track.
   // Generators need to generate per track.
   if (mIsLinearEffect && !isGenerator) {
      success = ::MixAndRender(saveTracks, mFactory, rate, floatSample, mT0, t1,
                               &mixLeft, &mixRight);
      if (!success) {
         delete mTracks;
         mTracks = saveTracks;
         return;
      }

      mixLeft->Offset(-mixLeft->GetStartTime());
      mixLeft->InsertSilence(0.0, mT0);
      mixLeft->SetSelected(true);
      mixLeft->SetDisplay(WaveTrack::NoDisplay);
      mTracks->Add(mixLeft);
      if (mixRight) {
         mixRight->Offset(-mixRight->GetStartTime());
         mixRight->InsertSilence(0.0, mT0);
         mixRight->SetSelected(true);
         mTracks->Add(mixRight);
      }
   }
   else {
      TrackListOfKindIterator iter(Track::Wave, saveTracks);
      WaveTrack *src = (WaveTrack *) iter.First();
      while (src)
      {
         WaveTrack *dest;
         if (src->GetSelected() || mPreviewWithNotSelected) {
            src->Copy(mT0, t1, (Track **) &dest);
            dest->InsertSilence(0.0, mT0);
            dest->SetSelected(src->GetSelected());
            dest->SetDisplay(WaveTrack::NoDisplay);
            mTracks->Add(dest);
         }
         src = (WaveTrack *) iter.Next();
      }
   }

   // Update track/group counts
   CountWaveTracks();

   // Apply effect
   if (!dryOnly) {
      mProgress = new ProgressDialog(GetName(),
            _("Preparing preview"),
            pdlgHideCancelButton); // Have only "Stop" button.
      mIsPreview = true;
      success = Process();
      mIsPreview = false;
      delete mProgress;
      mProgress = NULL;
   }

   if (success)
   {
      WaveTrackArray playbackTracks;
      WaveTrackArray recordingTracks;

      SelectedTrackListOfKindIterator iter(Track::Wave, mTracks);
      WaveTrack *src = (WaveTrack *) iter.First();
      while (src) {
         playbackTracks.Add(src);
         src = (WaveTrack *) iter.Next();
      }
      if (isNyquist && isGenerator)
         t1 = mT1;

#ifdef EXPERIMENTAL_MIDI_OUT
      NoteTrackArray empty;
#endif
      // Start audio playing
      int token =
         gAudioIO->StartStream(playbackTracks, recordingTracks,
#ifdef EXPERIMENTAL_MIDI_OUT
                               empty,
#endif
                               rate, mT0, t1);

      if (token) {
         int previewing = eProgressSuccess;
wxLogDebug(wxT("mT0 %.3f   t1 %.3f"),mT0,t1);
         // The progress dialog must be deleted before stopping the stream
         // to allow events to flow to the app during StopStream processing.
         // The progress dialog blocks these events.
         ProgressDialog *progress =
            new ProgressDialog(GetName(), _("Previewing"), pdlgHideCancelButton);

         while (gAudioIO->IsStreamActive(token) && previewing == eProgressSuccess) {
            ::wxMilliSleep(100);
            previewing = progress->Update(gAudioIO->GetStreamTime() - mT0, t1 - mT0);
         }

         delete progress;

         gAudioIO->StopStream();

         while (gAudioIO->IsBusy()) {
            ::wxMilliSleep(100);
         }
      }
      else {
         wxMessageBox(_("Error while opening sound device. Please check the playback device settings and the project sample rate."),
                     _("Error"), wxOK | wxICON_EXCLAMATION, FocusDialog);
      }
   }

   if (FocusDialog) {
      FocusDialog->SetFocus();
   }

   delete mOutputTracks;
   mOutputTracks = NULL;

   mTracks->Clear(true); // true => delete the tracks
   delete mTracks;

   mTracks = saveTracks;
   mT0 = oldT0;
   mT1 = oldT1;

   // Effect is already inited; we call Process, End, and then Init
   // again, so the state is exactly the way it was before Preview
   // was called.
   if (!dryOnly) {
      End();
      Init();
   }
}

BEGIN_EVENT_TABLE(EffectDialog, wxDialog)
   EVT_BUTTON(wxID_OK, EffectDialog::OnOk)
END_EVENT_TABLE()

EffectDialog::EffectDialog(wxWindow * parent,
                           const wxString & title,
                           int type,
                           int flags,
                           int additionalButtons)
: wxDialog(parent, wxID_ANY, title, wxDefaultPosition, wxDefaultSize, flags)
{
   mType = type;
   mAdditionalButtons = additionalButtons;
}

void EffectDialog::Init()
{
   ShuttleGui S(this, eIsCreating);

   S.SetBorder(5);
   S.StartVerticalLay(true);
   {
      PopulateOrExchange(S);

      long buttons = eOkButton;
      if (mType != EffectTypeAnalyze)
      {
         buttons |= eCancelButton;
         if (mType == EffectTypeProcess)
         {
            buttons |= ePreviewButton;
         }
      }
      S.AddStandardButtons(buttons|mAdditionalButtons);
   }
   S.EndVerticalLay();

   Layout();
   Fit();
   SetMinSize(GetSize());
   Center();
}

/// This is a virtual function which will be overridden to
/// provide the actual parameters that we want for each
/// kind of dialog.
void EffectDialog::PopulateOrExchange(ShuttleGui & WXUNUSED(S))
{
   return;
}

bool EffectDialog::TransferDataToWindow()
{
   ShuttleGui S(this, eIsSettingToDialog);
   PopulateOrExchange(S);

   return true;
}

bool EffectDialog::TransferDataFromWindow()
{
   ShuttleGui S(this, eIsGettingFromDialog);
   PopulateOrExchange(S);

   return true;
}

bool EffectDialog::Validate()
{
   return true;
}

void EffectDialog::OnPreview(wxCommandEvent & WXUNUSED(evt))
{
   return;
}

void EffectDialog::OnOk(wxCommandEvent & WXUNUSED(evt))
{
   // On wxGTK (wx2.8.12), the default action is still executed even if
   // the button is disabled.  This appears to affect all wxDialogs, not
   // just our Effects dialogs.  So, this is a only temporary workaround
   // for legacy effects that disable the OK button.  Hopefully this has
   // been corrected in wx3.
   if (FindWindow(wxID_OK)->IsEnabled() && Validate() && TransferDataFromWindow())
   {
      EndModal(true);
   }

   return;
}

///////////////////////////////////////////////////////////////////////////////
//
// EffectPanel
//
///////////////////////////////////////////////////////////////////////////////

class EffectPanel : public wxPanel
{
public:
   EffectPanel(wxWindow *parent)
   :  wxPanel(parent)
   {
      // This fools NVDA into not saying "Panel" when the dialog gets focus
      SetName(wxT("\a"));
      SetLabel(wxT("\a"));

      mAcceptsFocus = true;
   }

   virtual ~EffectPanel()
   {
   }

   // ============================================================================
   // wxWindow implementation
   // ============================================================================

   virtual bool AcceptsFocus() const
   {
      return mAcceptsFocus;
   }

   // ============================================================================
   // EffectPanel implementation
   // ============================================================================
   void SetAccept(bool accept)
   {
      mAcceptsFocus = accept;
   }

private:
   bool mAcceptsFocus;
};

///////////////////////////////////////////////////////////////////////////////
//
// EffectUIHost
//
///////////////////////////////////////////////////////////////////////////////

#include "../../images/Effect.h"

BEGIN_EVENT_TABLE(EffectUIHost, wxDialog)
   EVT_INIT_DIALOG(EffectUIHost::OnInitDialog)
   EVT_ERASE_BACKGROUND(EffectUIHost::OnErase)
   EVT_PAINT(EffectUIHost::OnPaint)
   EVT_CLOSE(EffectUIHost::OnClose)
   EVT_BUTTON(wxID_APPLY, EffectUIHost::OnApply)
   EVT_BUTTON(wxID_CANCEL, EffectUIHost::OnCancel)
   EVT_BUTTON(eDebugID, EffectUIHost::OnDebug)
   EVT_BUTTON(kMenuID, EffectUIHost::OnMenu)
   EVT_CHECKBOX(kEnableID, EffectUIHost::OnEnable)
   EVT_BUTTON(kPlayID, EffectUIHost::OnPlay)
   EVT_BUTTON(kRewindID, EffectUIHost::OnRewind)
   EVT_BUTTON(kFFwdID, EffectUIHost::OnFFwd)
   EVT_MENU(kSaveAsID, EffectUIHost::OnSaveAs)
   EVT_MENU(kImportID, EffectUIHost::OnImport)
   EVT_MENU(kExportID, EffectUIHost::OnExport)
   EVT_MENU(kOptionsID, EffectUIHost::OnOptions)
   EVT_MENU(kDefaultsID, EffectUIHost::OnDefaults)
   EVT_MENU_RANGE(kUserPresetsID, kUserPresetsID + 999, EffectUIHost::OnUserPreset)
   EVT_MENU_RANGE(kDeletePresetID, kDeletePresetID + 999, EffectUIHost::OnDeletePreset)
   EVT_MENU_RANGE(kFactoryPresetsID, kFactoryPresetsID + 999, EffectUIHost::OnFactoryPreset)
END_EVENT_TABLE()

EffectUIHost::EffectUIHost(wxWindow *parent,
                           Effect *effect,
                           EffectUIClientInterface *client)
:  wxDialog(parent, wxID_ANY, effect->GetName(),
            wxDefaultPosition, wxDefaultSize,
            wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER | wxMINIMIZE_BOX | wxMAXIMIZE_BOX)
{
   SetName(effect->GetName());
   SetExtraStyle(wxWS_EX_VALIDATE_RECURSIVELY);

   mParent = parent;
   mEffect = effect;
   mClient = client;

   mProject = GetActiveProject();

   mInitialized = false;
   mSupportsRealtime = false;

   mDisableTransport = false;

   mEnabled = true;

   mPlayPos = 0.0;

   mClient->SetHostUI(this);
}

EffectUIHost::~EffectUIHost()
{
   CleanupRealtime();

   if (mClient)
   {
      mClient->CloseUI();
      mClient = NULL;
   }
}

// ============================================================================
// wxWindow implementation
// ============================================================================

#if defined(__WXMAC__)

// As mentioned below, we want to manipulate the window attributes, but doing
// so causes extra events to fire and those events lead to the rebuilding of
// the menus.  Unfortunately, if this happens when a modal dialog is displayed
// the menus become disabled until the menubar is completely rebuilt, like when
// leaving preferecnes.
//
// So, we only do this when NOT displaying a modal dialog since that's really
// only when it is needed.

bool EffectUIHost::Show(bool show)
{
   if (!mIsModal)
   {
      // We want the effects windows on the Mac to float above the project window
      // but still have normal modal dialogs appear above the effects windows and
      // not let the effect windows fall behind the project window.
      //
      // This seems to accomplish that, but time will be the real judge.
      WindowRef windowRef = (WindowRef) MacGetWindowRef();
      WindowGroupRef parentGroup = GetWindowGroup((WindowRef) ((wxFrame *)wxGetTopLevelParent(mParent))->MacGetWindowRef());
      ChangeWindowGroupAttributes(parentGroup, kWindowGroupAttrSharedActivation, kWindowGroupAttrMoveTogether);
      SetWindowGroup(windowRef, parentGroup);
   }
   mIsModal = false;

   return wxDialog::Show(show);
}
#endif

bool EffectUIHost::TransferDataToWindow()
{
   return mEffect->TransferDataToWindow();
}

bool EffectUIHost::TransferDataFromWindow()
{
   return mEffect->TransferDataFromWindow();
}

// ============================================================================
// wxDialog implementation
// ============================================================================

int EffectUIHost::ShowModal()
{
#if defined(__WXMAC__)
   // See explanation in EffectUIHost::Show()
   mIsModal = true;
#endif

#if defined(__WXMSW__)
   // Swap the Close and Apply buttons
   wxSizer *sz = mApplyBtn->GetContainingSizer();
   wxButton *apply = new wxButton(mApplyBtn->GetParent(), wxID_APPLY);
   sz->Replace(mCloseBtn, apply);
   sz->Replace(mApplyBtn, mCloseBtn);
   sz->Layout();
   delete mApplyBtn;
   mApplyBtn = apply;
   mApplyBtn->SetDefault();
   mApplyBtn->SetLabel(wxGetStockLabel(wxID_OK, 0));
   mCloseBtn->SetLabel(wxGetStockLabel(wxID_CANCEL, 0));
#else
   mApplyBtn->SetLabel(wxGetStockLabel(wxID_OK));
   mCloseBtn->SetLabel(wxGetStockLabel(wxID_CANCEL));
#endif

   Layout();

   return wxDialog::ShowModal();
}

// ============================================================================
// EffectUIHost implementation
// ============================================================================

bool EffectUIHost::Initialize()
{
   wxBoxSizer *vs = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer *hs = new wxBoxSizer(wxHORIZONTAL);

   EffectPanel *w = new EffectPanel(this);

   // Try to give the window a sensible default/minimum size
   w->SetMinSize(wxSize(wxMax(600, mParent->GetSize().GetWidth() * 2 / 3),
                        mParent->GetSize().GetHeight() / 2));

   mDisableTransport = !gAudioIO->IsAvailable(mProject);
   mPlaying = gAudioIO->IsStreamActive(); // not exactly right, but will suffice
   mCapturing = gAudioIO->IsStreamActive() && gAudioIO->GetNumCaptureChannels() > 0;

   if (!mClient->PopulateUI(w))
   {
      return false;
   }

   hs->Add(w, 1, wxEXPAND);
   vs->Add(hs, 1, wxEXPAND);

   wxPanel *buttonPanel = new wxPanel(this, wxID_ANY);
   wxPanel *bar = new wxPanel(buttonPanel, wxID_ANY);

   // This fools NVDA into not saying "Panel" when the dialog gets focus
   bar->SetName(wxT("\a"));
   bar->SetLabel(wxT("\a"));

   wxBoxSizer *bs = new wxBoxSizer(wxHORIZONTAL);

   mSupportsRealtime = mEffect->SupportsRealtime();
   mIsGUI = mClient->IsGraphicalUI();
   mIsBatch = mEffect->IsBatchProcessing();

   wxBitmapButton *bb;

   int margin = 0;

#if defined(__WXMAC__)
   margin = 3; // I'm sure it's needed because of the order things are created...
#endif   

   if (!mIsGUI)
   {
      mMenuBtn = new wxButton(bar, kMenuID, _("&Manage"));
      bs->Add(mMenuBtn, 0, wxALIGN_CENTER | wxTOP | wxBOTTOM, margin);
   }
   else
   {
      mMenuBtn = new wxBitmapButton(bar, kMenuID, CreateBitmap(effect_menu_xpm, true, false));
#if defined(__WXMAC__)
      mMenuBtn->SetName(_("&Manage"));
#else
      mMenuBtn->SetLabel(_("&Manage"));
#endif
      bs->Add(mMenuBtn);
   }
   mMenuBtn->SetToolTip(_("Manage presets and options"));

   bs->Add(5, 5);

   if (!mIsBatch)
   {
      if (!mIsGUI)
      {
         if (mSupportsRealtime)
         {
            mPlayToggleBtn = new wxButton(bar, kPlayID, _("Start &Playback"));
            mPlayToggleBtn->SetToolTip(_("Start and stop playback"));
            bs->Add(mPlayToggleBtn, 0, wxALIGN_CENTER | wxTOP | wxBOTTOM, margin);
         }
         else if (mEffect->GetType() != EffectTypeAnalyze)
         {
            mPlayToggleBtn = new wxButton(bar, kPlayID, _("&Preview"));
            mPlayToggleBtn->SetToolTip(_("Preview effect"));
            bs->Add(mPlayToggleBtn, 0, wxALIGN_CENTER | wxTOP | wxBOTTOM, margin);
         }
      }
      else
      {
         mPlayBM = CreateBitmap(effect_play_xpm, true, false);
         mPlayDisabledBM = CreateBitmap(effect_play_disabled_xpm, true, false);
         mStopBM = CreateBitmap(effect_stop_xpm, true, false);
         mStopDisabledBM = CreateBitmap(effect_stop_disabled_xpm, true, false);
         bb = new wxBitmapButton(bar, kPlayID, mPlayBM);
         bb->SetBitmapDisabled(mPlayDisabledBM);
         mPlayBtn = bb;
         bs->Add(mPlayBtn);
         if (!mSupportsRealtime)
         {
            mPlayBtn->SetToolTip(_("Preview effect"));
#if defined(__WXMAC__)
            mPlayBtn->SetName(_("Preview effect"));
#else
            mPlayBtn->SetLabel(_("&Preview effect"));
#endif
         }
      }

      if (mSupportsRealtime)
      {
         if (!mIsGUI)
         {
            mRewindBtn = new wxButton(bar, kRewindID, _("Skip &Backward"));
            bs->Add(mRewindBtn, 0, wxALIGN_CENTER | wxTOP | wxBOTTOM, margin);
         }
         else
         {
            bb = new wxBitmapButton(bar, kRewindID, CreateBitmap(effect_rewind_xpm, true, true));
            bb->SetBitmapDisabled(CreateBitmap(effect_rewind_disabled_xpm, true, true));
            mRewindBtn = bb;
#if defined(__WXMAC__)
            mRewindBtn->SetName(_("Skip &Backward"));
#else
            mRewindBtn->SetLabel(_("Skip &Backward"));
#endif
            bs->Add(mRewindBtn);
         }
         mRewindBtn->SetToolTip(_("Skip backward"));

         if (!mIsGUI)
         {
            mFFwdBtn = new wxButton(bar, kFFwdID, _("Skip &Forward"));
            bs->Add(mFFwdBtn, 0, wxALIGN_CENTER | wxTOP | wxBOTTOM, margin);
         }
         else
         {
            bb = new wxBitmapButton(bar, kFFwdID, CreateBitmap(effect_ffwd_xpm, true, true));
            bb->SetBitmapDisabled(CreateBitmap(effect_ffwd_disabled_xpm, true, true));
            mFFwdBtn = bb;
#if defined(__WXMAC__)
            mFFwdBtn->SetName(_("Skip &Foreward"));
#else
            mFFwdBtn->SetLabel(_("Skip &Foreward"));
#endif
            bs->Add(mFFwdBtn);
         }
         mFFwdBtn->SetToolTip(_("Skip forward"));

         bs->Add(5, 5);

         mEnableCb = new wxCheckBox(bar, kEnableID, _("&Enable"));
         mEnableCb->SetValue(mEnabled);
         mEnableCb->SetName(_("Enable"));
         bs->Add(mEnableCb, 0, wxALIGN_CENTER | wxTOP | wxBOTTOM, margin);
      }
   }

   bar->SetSizerAndFit(bs);

   long buttons = eApplyButton + eCloseButton;
   if (mEffect->mUIDebug)
   {
      buttons += eDebugButton;
   }

   buttonPanel->SetSizer(CreateStdButtonSizer(buttonPanel, buttons, bar));
   vs->Add(buttonPanel, 0, wxEXPAND | wxALIGN_CENTER_VERTICAL);

   SetSizer(vs);
   Layout();
   Fit();
   Center();

   mApplyBtn = (wxButton *) FindWindow(wxID_APPLY);
   mCloseBtn = (wxButton *) FindWindow(wxID_CANCEL);

   UpdateControls();

   w->SetAccept(!mIsGUI);
   (!mIsGUI ? w : FindWindow(wxID_APPLY))->SetFocus();
 
   LoadUserPresets();

   InitializeRealtime();

   return true;
}

void EffectUIHost::OnInitDialog(wxInitDialogEvent & evt)
{
   // Do default handling
   wxDialog::OnInitDialog(evt);

#if wxCHECK_VERSION(3, 0, 0)
//#warning "check to see if this still needed in wx3"
#endif

   // Pure hackage coming down the pike...
   //
   // I have no idea why, but if a wxTextCtrl is the first control in the
   // panel, then its contents will not be automatically selected when the
   // dialog is displayed.
   //
   // So, we do the selection manually.
   wxTextCtrl *focused = wxDynamicCast(FindFocus(), wxTextCtrl);
   if (focused)
   {
      focused->SelectAll();
   }
}

void EffectUIHost::OnErase(wxEraseEvent & WXUNUSED(evt))
{
   // Ignore it
}

void EffectUIHost::OnPaint(wxPaintEvent & WXUNUSED(evt))
{
   wxPaintDC dc(this);

   dc.Clear();
}

void EffectUIHost::OnClose(wxCloseEvent & WXUNUSED(evt))
{
   CleanupRealtime();

   Hide();

   mClient->CloseUI();
   mClient = NULL;

   Destroy();
}

void EffectUIHost::OnApply(wxCommandEvent & evt)
{
   // On wxGTK (wx2.8.12), the default action is still executed even if
   // the button is disabled.  This appears to affect all wxDialogs, not
   // just our Effects dialogs.  So, this is a only temporary workaround
   // for legacy effects that disable the OK button.  Hopefully this has
   // been corrected in wx3.
   if (!FindWindow(wxID_APPLY)->IsEnabled())
   {
      return;
   }

   // Honor the "select all if none" preference...a little hackish, but whatcha gonna do...
   if (!mIsBatch && mEffect->GetType() != EffectTypeGenerate && mProject->mViewInfo.selectedRegion.isPoint())
   {
      wxUint32 flags = 0;
      bool allowed = mProject->TryToMakeActionAllowed(flags,
                                                      WaveTracksSelectedFlag | TimeSelectedFlag,
                                                      WaveTracksSelectedFlag | TimeSelectedFlag);
      if (!allowed)
      {
         wxMessageBox(_("You must select audio in the project window."));
         return;
      }
   }

   if (!mClient->ValidateUI())
   {
      return;
   }

   // This will take care of calling TransferDataFromWindow()
   if (!mEffect->SaveUserPreset(mEffect->GetCurrentSettingsGroup()))
   {
      return;
   }

   mEffect->mUIResultID = evt.GetId();

   if (IsModal())
   {
      EndModal(true);

      Close();

      return;
   }

   // Progress dialog no longer yields, so this "shouldn't" be necessary (yet to be proven
   // for sure), but it is a nice visual cue that something is going on.
   mApplyBtn->Disable();

   mEffect->Apply();

   mApplyBtn->Enable();

   return;
}

void EffectUIHost::OnCancel(wxCommandEvent & evt)
{
   mEffect->mUIResultID = evt.GetId();

   if (IsModal())
   {
      EndModal(false);

      Close();

      return;
   }

   Hide();

   Close();

   return;
}

void EffectUIHost::OnDebug(wxCommandEvent & evt)
{
   OnApply(evt);

   mEffect->mUIResultID = evt.GetId();

   return;
}

void EffectUIHost::OnMenu(wxCommandEvent & WXUNUSED(evt))
{
   wxMenu *menu = new wxMenu();
   wxMenu *sub;

   LoadUserPresets();

   if (mUserPresets.GetCount() == 0)
   {
      menu->Append(kUserPresetsDummyID, _("User Presets"))->Enable(false);
   }
   else
   {
      sub = new wxMenu();
      for (size_t i = 0, cnt = mUserPresets.GetCount(); i < cnt; i++)
      {
         sub->Append(kUserPresetsID + i, mUserPresets[i]);
      }
      menu->Append(0, _("User Presets"), sub);
   }

   menu->Append(kSaveAsID, _("Save Preset..."));

   if (mUserPresets.GetCount() == 0)
   {
      menu->Append(kDeletePresetDummyID, _("Delete Preset"))->Enable(false);
   }
   else
   {
      sub = new wxMenu();
      for (size_t i = 0, cnt = mUserPresets.GetCount(); i < cnt; i++)
      {
         sub->Append(kDeletePresetID + i, mUserPresets[i]);
      }
      menu->Append(0, _("Delete Preset"), sub);
   }

   menu->AppendSeparator();

   wxArrayString factory = mEffect->GetFactoryPresets();

   sub = new wxMenu();
   sub->Append(kDefaultsID, _("Defaults"));
   if (factory.GetCount() > 0)
   {
      sub->AppendSeparator();
      for (size_t i = 0, cnt = factory.GetCount(); i < cnt; i++)
      {
         wxString label = factory[i];
         if (label.IsEmpty())
         {
            label = _("None");
         }

         sub->Append(kFactoryPresetsID + i, label);
      }
   }
   menu->Append(0, _("Factory Presets"), sub);

   menu->AppendSeparator();
   menu->Append(kImportID, _("Import..."))->Enable(mClient->CanExportPresets());
   menu->Append(kExportID, _("Export..."))->Enable(mClient->CanExportPresets());
   menu->AppendSeparator();
   menu->Append(kOptionsID, _("Options..."))->Enable(mClient->HasOptions());
   menu->AppendSeparator();

   sub = new wxMenu();

   sub->Append(kDummyID, wxString::Format(_("Type: %s"), mEffect->GetFamily().c_str()));
   sub->Append(kDummyID, wxString::Format(_("Name: %s"), mEffect->GetName().c_str()));
   sub->Append(kDummyID, wxString::Format(_("Version: %s"), mEffect->GetVersion().c_str()));
   sub->Append(kDummyID, wxString::Format(_("Vendor: %s"), mEffect->GetVendor().c_str()));
   sub->Append(kDummyID, wxString::Format(_("Description: %s"), mEffect->GetDescription().c_str()));

   menu->Append(0, _("About"), sub);

   wxWindow *btn = FindWindow(kMenuID);
   wxRect r = btn->GetRect();
   btn->PopupMenu(menu, r.GetLeft(), r.GetBottom());

   delete menu;
}

void EffectUIHost::OnEnable(wxCommandEvent & WXUNUSED(evt))
{
   mEnabled = mEnableCb->GetValue();

   if (mEnabled)
   {
      mEffect->RealtimeResume();
   }
   else
   {
      mEffect->RealtimeSuspend();
   }

   UpdateControls();
}

void EffectUIHost::OnPlay(wxCommandEvent & WXUNUSED(evt))
{
   if (!mSupportsRealtime)
   {
      if (!mClient->ValidateUI() || !mEffect->TransferDataFromWindow())
      {
         return;
      }

      mEffect->Preview(false);

      return;
   }

   if (mPlaying)
   {
      mPlayPos = gAudioIO->GetStreamTime();
      mProject->GetControlToolBar()->StopPlaying();
   }
   else
   {
      if (mProject->IsPlayRegionLocked())
      {
         double t0, t1;
         mProject->GetPlayRegion(&t0, &t1);
         mRegion.setTimes(t0, t1);
         mPlayPos = mRegion.t0();
      }
      else if (mProject->mViewInfo.selectedRegion.t0() != mRegion.t0() ||
               mProject->mViewInfo.selectedRegion.t1() != mRegion.t1())
      {
         mRegion = mProject->mViewInfo.selectedRegion;
         mPlayPos = mRegion.t0();
      }

      if (mPlayPos > mRegion.t1())
      {
         mPlayPos = mRegion.t1();
      }

      mProject->GetControlToolBar()->PlayPlayRegion
         (SelectedRegion(mPlayPos, mRegion.t1()),
          mProject->GetDefaultPlayOptions());
   }
}

void EffectUIHost::OnRewind(wxCommandEvent & WXUNUSED(evt))
{
   if (mPlaying)
   {
      double seek;
      gPrefs->Read(wxT("/AudioIO/SeekShortPeriod"), &seek, 1.0);

      double pos = gAudioIO->GetStreamTime();
      if (pos - seek < mRegion.t0())
      {
         seek = pos - mRegion.t0();
      }

      gAudioIO->SeekStream(-seek);
   }
   else
   {
      mPlayPos = mRegion.t0();
   }
}

void EffectUIHost::OnFFwd(wxCommandEvent & WXUNUSED(evt))
{
   if (mPlaying)
   {
      double seek;
      gPrefs->Read(wxT("/AudioIO/SeekShortPeriod"), &seek, 1.0);

      double pos = gAudioIO->GetStreamTime();
      if (mRegion.t0() < mRegion.t1() && pos + seek > mRegion.t1())
      {
         seek = mRegion.t1() - pos;
      }

      gAudioIO->SeekStream(seek);
   }
   else
   {
      // It allows to play past end of selection...probably useless
      mPlayPos = mRegion.t1();
   }
}

void EffectUIHost::OnPlayback(wxCommandEvent & evt)
{
   evt.Skip();

   if (evt.GetInt() != 0)
   {
      if (evt.GetEventObject() != mProject)
      {
         mDisableTransport = true;
      }
      else
      {
         mPlaying = true;
      }
   }
   else
   {
      mDisableTransport = false;
      mPlaying = false;
   }

   if (mPlaying)
   {
      mRegion = mProject->mViewInfo.selectedRegion;
      mPlayPos = mRegion.t0();
   }

   UpdateControls();
}

void EffectUIHost::OnCapture(wxCommandEvent & evt)
{
   evt.Skip();

   if (evt.GetInt() != 0)
   {
      if (evt.GetEventObject() != mProject)
      {
         mDisableTransport = true;
      }
      else
      {
         mCapturing = true;
      }
   }
   else
   {
      mDisableTransport = false;
      mCapturing = false;
   }

   UpdateControls();
}

void EffectUIHost::OnUserPreset(wxCommandEvent & evt)
{
   int preset = evt.GetId() - kUserPresetsID;

   mEffect->LoadUserPreset(mEffect->GetUserPresetsGroup(mUserPresets[preset]));

   return;
}

void EffectUIHost::OnFactoryPreset(wxCommandEvent & evt)
{
   mEffect->LoadFactoryPreset(evt.GetId() - kFactoryPresetsID);

   return;
}

void EffectUIHost::OnDeletePreset(wxCommandEvent & evt)
{
   wxString preset = mUserPresets[evt.GetId() - kDeletePresetID];

   int res = wxMessageBox(wxString::Format(_("Are you sure you want to delete \"%s\"?"), preset.c_str()),
                          _("Delete Preset"),
                          wxICON_QUESTION | wxYES_NO);
   if (res == wxYES)
   {
      mEffect->RemovePrivateConfigSubgroup(mEffect->GetUserPresetsGroup(preset));
   }

   LoadUserPresets();

   return;
}

void EffectUIHost::OnSaveAs(wxCommandEvent & WXUNUSED(evt))
{
   wxTextCtrl *text;
   wxString name;
   wxDialog dlg(this, wxID_ANY, wxString(_("Save Preset")));

   ShuttleGui S(&dlg, eIsCreating);

   S.StartPanel();
   {
      S.StartVerticalLay(1);
      {
         S.StartHorizontalLay(wxALIGN_LEFT, 0);
         {
            text = S.AddTextBox(_("Preset name:"), name, 30);
         }
         S.EndHorizontalLay();
         S.SetBorder(10);
         S.AddStandardButtons();
      }
      S.EndVerticalLay();
   }
   S.EndPanel();

   dlg.SetSize(dlg.GetSizer()->GetMinSize());
   dlg.Center();

   while (true)
   {
      int rc = dlg.ShowModal();

      if (rc != wxID_OK)
      {
         break;
      }

      name = text->GetValue();
      if (name.IsEmpty())
      {
         wxMessageDialog md(this,
                            _("You must specify a name"),
                            _("Save Preset"));
         md.Center();
         md.ShowModal();
         continue;
      }

      if (mUserPresets.Index(name) != wxNOT_FOUND)
      {
         wxMessageDialog md(this,
                            _("Preset already exists.\n\nReplace?"),
                            _("Save Preset"),
                            wxYES_NO | wxCANCEL | wxICON_EXCLAMATION);
         md.Center();
         int choice = md.ShowModal();
         if (choice == wxID_CANCEL)
         {
            break;
         }

         if (choice == wxID_NO)
         {
            continue;
         }
      }

      mEffect->SaveUserPreset(mEffect->GetUserPresetsGroup(name));
      LoadUserPresets();

      break;
   }

   return;
}

void EffectUIHost::OnImport(wxCommandEvent & WXUNUSED(evt))
{
   mClient->ImportPresets();

   LoadUserPresets();

   return;
}

void EffectUIHost::OnExport(wxCommandEvent & WXUNUSED(evt))
{
   mClient->ExportPresets();

   return;
}

void EffectUIHost::OnOptions(wxCommandEvent & WXUNUSED(evt))
{
   mClient->ShowOptions();

   return;
}

void EffectUIHost::OnDefaults(wxCommandEvent & WXUNUSED(evt))
{
   mEffect->LoadFactoryDefaults();

   return;
}

wxBitmap EffectUIHost::CreateBitmap(const char *xpm[], bool up, bool pusher)
{
   wxMemoryDC dc;
   wxBitmap pic(xpm);

   wxBitmap mod(pic.GetWidth() + 6, pic.GetHeight() + 6);
   dc.SelectObject(mod);

#if !defined(__WXMAC__)

#if defined(__WXGTK__)
   wxColour newColour = wxSystemSettings::GetColour(wxSYS_COLOUR_BACKGROUND);
#elif defined(__WXMSW__)
   wxColour newColour = wxSystemSettings::GetColour(wxSYS_COLOUR_BTNFACE);
#endif

   dc.SetBackground(wxBrush(newColour));
   dc.Clear();
#endif

   int offset = 3;
   if (pusher)
   {
      if (!up)
      {
         offset += 1;
      }
   }

   dc.DrawBitmap(pic, offset, offset, true);

   dc.SelectObject(wxNullBitmap);

   return mod;
}

void EffectUIHost::UpdateControls()
{
   if (mIsBatch)
   {
      return;
   }

   if (mCapturing || mDisableTransport)
   {
      // Don't allow focus to get trapped
      wxWindow *focus = FindFocus();
      if (focus == mRewindBtn || focus == mFFwdBtn || focus == mPlayBtn || focus == mEnableCb)
      {
         mCloseBtn->SetFocus();
      }
   }

   mApplyBtn->Enable(!mCapturing);
   if (mEffect->GetType() != EffectTypeAnalyze)
   {
      (!mIsGUI ? mPlayToggleBtn : mPlayBtn)->Enable(!(mCapturing || mDisableTransport));
   }

   if (mSupportsRealtime)
   {
      mRewindBtn->Enable(!(mCapturing || mDisableTransport));
      mFFwdBtn->Enable(!(mCapturing || mDisableTransport));
      mEnableCb->Enable(!(mCapturing || mDisableTransport));

      wxBitmapButton *bb;

      if (mPlaying)
      {
         if (!mIsGUI)
         {
            /* i18n-hint: The access key "&P" should be the same in
               "Stop &Playback" and "Start &Playback" */
            mPlayToggleBtn->SetLabel(_("Stop &Playback"));
            mPlayToggleBtn->Refresh();
         }
         else
         {
            bb = (wxBitmapButton *) mPlayBtn;
            bb->SetBitmapLabel(mStopBM);
            bb->SetBitmapDisabled(mStopDisabledBM);
            bb->SetToolTip(_("Stop"));
#if defined(__WXMAC__)
            bb->SetName(_("Stop &Playback"));
#else
            bb->SetLabel(_("Stop &Playback"));
#endif
         }
      }
      else
      {
         if (!mIsGUI)
         {
            /* i18n-hint: The access key "&P" should be the same in
               "Stop &Playback" and "Start &Playback" */
            mPlayToggleBtn->SetLabel(_("Start &Playback"));
            mPlayToggleBtn->Refresh();
         }
         else
         {
            bb = (wxBitmapButton *) mPlayBtn;
            bb->SetBitmapLabel(mPlayBM);
            bb->SetBitmapDisabled(mPlayDisabledBM);
            bb->SetToolTip(_("Play"));
#if defined(__WXMAC__)
            bb->SetName(_("Start &Playback"));
#else
            bb->SetLabel(_("Start &Playback"));
#endif
         }
      }
   }
}

void EffectUIHost::LoadUserPresets()
{
   mUserPresets.Clear();

   mEffect->GetPrivateConfigSubgroups(mEffect->GetUserPresetsGroup(wxEmptyString), mUserPresets);

   mUserPresets.Sort();

   return;
}

void EffectUIHost::InitializeRealtime()
{
   if (mSupportsRealtime && !mInitialized)
   {
      EffectManager::Get().RealtimeAddEffect(mEffect);

      wxTheApp->Connect(EVT_AUDIOIO_PLAYBACK,
                        wxCommandEventHandler(EffectUIHost::OnPlayback),
                        NULL,
                        this);

      wxTheApp->Connect(EVT_AUDIOIO_CAPTURE,
                        wxCommandEventHandler(EffectUIHost::OnCapture),
                        NULL,
                        this);

      mInitialized = true;
   }
}

void EffectUIHost::CleanupRealtime()
{
   if (mSupportsRealtime && mInitialized)
   {
      wxTheApp->Disconnect(EVT_AUDIOIO_PLAYBACK,
                           wxCommandEventHandler(EffectUIHost::OnPlayback),
                           NULL,
                           this);

      wxTheApp->Disconnect(EVT_AUDIOIO_CAPTURE,
                           wxCommandEventHandler(EffectUIHost::OnCapture),
                           NULL,
                           this);

      EffectManager::Get().RealtimeRemoveEffect(mEffect);

      mInitialized = false;
   }
}

///////////////////////////////////////////////////////////////////////////////
//
// EffectPresetsDialog
//
///////////////////////////////////////////////////////////////////////////////

enum
{
   ID_Type = 10000
};

BEGIN_EVENT_TABLE(EffectPresetsDialog, wxDialog)
   EVT_CHOICE(ID_Type, EffectPresetsDialog::OnType)
   EVT_LISTBOX_DCLICK(wxID_ANY, EffectPresetsDialog::OnOk)
   EVT_BUTTON(wxID_OK, EffectPresetsDialog::OnOk)
   EVT_BUTTON(wxID_CANCEL, EffectPresetsDialog::OnCancel)
END_EVENT_TABLE()

EffectPresetsDialog::EffectPresetsDialog(wxWindow *parent, Effect *effect)
:  wxDialog(parent, wxID_ANY, wxString(_("Select Preset")))
{
   ShuttleGui S(this, eIsCreating);
   S.StartVerticalLay();
   {
      S.StartTwoColumn();
      S.SetStretchyCol(1);
      {
         wxArrayString empty;

         S.AddPrompt(_("Type:"));
         mType = S.Id(ID_Type).AddChoice(wxT(""), wxT(""), &empty);
         mType->SetSelection(0);

         S.AddPrompt(_("&Preset:"));
         mPresets = S.AddListBox(&empty, wxLB_SINGLE | wxLB_NEEDED_SB );
      }
      S.EndTwoColumn();

      S.AddStandardButtons();
   }
   S.EndVerticalLay();

   mUserPresets = effect->GetUserPresets();
   mFactoryPresets = effect->GetFactoryPresets();

   if (mUserPresets.GetCount() > 0)
   {
      mType->Append(_("User Presets"));
   }

   if (mFactoryPresets.GetCount() > 0)
   {
      mType->Append(_("Factory Presets"));
   }

   if (effect->HasCurrentSettings())
   {
      mType->Append(_("Current Settings"));
   }

   if (effect->HasFactoryDefaults())
   {
      mType->Append(_("Factory Defaults"));
   }

   UpdateUI();
}

EffectPresetsDialog::~EffectPresetsDialog()
{
}

wxString EffectPresetsDialog::GetSelected() const
{
   return mSelection;
}

void EffectPresetsDialog::SetSelected(const wxString & parms)
{
   wxString preset = parms;
   if (preset.StartsWith(Effect::kUserPresetIdent))
   {
      preset.Replace(Effect::kUserPresetIdent, wxEmptyString, false);
      SetPrefix(_("User Presets"), preset);
   }
   else if (preset.StartsWith(Effect::kFactoryPresetIdent))
   {
      preset.Replace(Effect::kFactoryPresetIdent, wxEmptyString, false);
      SetPrefix(_("Factory Presets"), preset);
   }
   else if (preset.StartsWith(Effect::kCurrentSettingsIdent))
   {
      SetPrefix(_("Current Settings"), wxEmptyString);
   }
   else if (preset.StartsWith(Effect::kFactoryDefaultsIdent))
   {
      SetPrefix(_("Factory Defaults"), wxEmptyString);
   }
}

void EffectPresetsDialog::SetPrefix(const wxString & type, const wxString & prefix)
{
   mType->SetStringSelection(type);

   if (type.IsSameAs(_("User Presets")))
   {
      mPresets->Clear();
      mPresets->Append(mUserPresets);
      mPresets->Enable(true);
      mPresets->SetStringSelection(prefix);
      if (mPresets->GetSelection() == wxNOT_FOUND)
      {
         mPresets->SetSelection(0);
      }
      mSelection = Effect::kUserPresetIdent + mPresets->GetStringSelection();
   }
   else if (type.IsSameAs(_("Factory Presets")))
   {
      mPresets->Clear();
      for (size_t i = 0, cnt = mFactoryPresets.GetCount(); i < cnt; i++)
      {
         wxString label = mFactoryPresets[i];
         if (label.IsEmpty())
         {
            label = _("None");
         }
         mPresets->Append(label);
      }
      mPresets->Enable(true);
      mPresets->SetStringSelection(prefix);
      if (mPresets->GetSelection() == wxNOT_FOUND)
      {
         mPresets->SetSelection(0);
      }
      mSelection = Effect::kFactoryPresetIdent + mPresets->GetStringSelection();
   }
   else if (type.IsSameAs(_("Current Settings")))
   {
      mPresets->Clear();
      mPresets->Enable(false);
      mSelection = Effect::kCurrentSettingsIdent;
   }
   else if (type.IsSameAs(_("Factory Defaults")))
   {
      mPresets->Clear();
      mPresets->Enable(false);
      mSelection = Effect::kFactoryDefaultsIdent;
   }
}

void EffectPresetsDialog::UpdateUI()
{
   int selected = mType->GetSelection();
   if (selected == wxNOT_FOUND)
   {
      selected = 0;
      mType->SetSelection(selected);
   }
   wxString type = mType->GetString(selected);

   if (type.IsSameAs(_("User Presets")))
   {
      selected = mPresets->GetSelection();
      if (selected == wxNOT_FOUND)
      {
         selected = 0;
      }

      mPresets->Clear();
      mPresets->Append(mUserPresets);
      mPresets->Enable(true);
      mPresets->SetSelection(selected);
      mSelection = Effect::kUserPresetIdent + mPresets->GetString(selected);
   }
   else if (type.IsSameAs(_("Factory Presets")))
   {
      selected = mPresets->GetSelection();
      if (selected == wxNOT_FOUND)
      {
         selected = 0;
      }

      mPresets->Clear();
      for (size_t i = 0, cnt = mFactoryPresets.GetCount(); i < cnt; i++)
      {
         wxString label = mFactoryPresets[i];
         if (label.IsEmpty())
         {
            label = _("None");
         }
         mPresets->Append(label);
      }
      mPresets->Enable(true);
      mPresets->SetSelection(selected);
      mSelection = Effect::kFactoryPresetIdent + mPresets->GetString(selected);
   }
   else if (type.IsSameAs(_("Current Settings")))
   {
      mPresets->Clear();
      mPresets->Enable(false);
      mSelection = Effect::kCurrentSettingsIdent;
   }
   else if (type.IsSameAs(_("Factory Defaults")))
   {
      mPresets->Clear();
      mPresets->Enable(false);
      mSelection = Effect::kFactoryDefaultsIdent;
   }
}

void EffectPresetsDialog::OnType(wxCommandEvent & WXUNUSED(evt))
{
   UpdateUI();
}

void EffectPresetsDialog::OnOk(wxCommandEvent & WXUNUSED(evt))
{
   UpdateUI();

   EndModal(true);
}

void EffectPresetsDialog::OnCancel(wxCommandEvent & WXUNUSED(evt))
{
   mSelection = wxEmptyString;

   EndModal(false);
}
