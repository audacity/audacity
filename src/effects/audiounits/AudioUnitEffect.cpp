/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioUnitEffect.cpp

  Dominic Mazzoni
  Leland Lucius

*******************************************************************//**

\class AudioUnitEffect
\brief An Effect class that handles a wide range of effects.  ??Mac only??

*//*******************************************************************/

#include "../../Audacity.h"

#if USE_AUDIO_UNITS

#include <wx/defs.h>
#include <wx/button.h>
#include <wx/control.h>
#include <wx/dir.h>

#ifdef __WXMAC__
#include <wx/evtloop.h>
#endif

#include <wx/filename.h>
#include <wx/frame.h>
#include <wx/listctrl.h>
#include <wx/sizer.h>
#include <wx/settings.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/tokenzr.h>

#include "../../ShuttleGui.h"
#include "../../widgets/valnum.h"
#include "../../widgets/wxPanelWrapper.h"

#include "AudioUnitEffect.h"

struct CFReleaser
   { void operator () (const void *p) const { if (p) CFRelease(p); } };
template <typename T>
   using CFunique_ptr = std::unique_ptr<T, CFReleaser>;

struct AudioUnitParameterInfoEx : AudioUnitParameterInfo
{
   ~AudioUnitParameterInfoEx ()
   {
      if ((flags & kAudioUnitParameterFlag_HasCFNameString)
          &&
          (flags & kAudioUnitParameterFlag_CFNameRelease))
         CFRelease( cfNameString );
   }
};

// ============================================================================
// Module registration entry point
//
// This is the symbol that Audacity looks for when the module is built as a
// dynamic library.
//
// When the module is builtin to Audacity, we use the same function, but it is
// declared static so as not to clash with other builtin modules.
// ============================================================================
DECLARE_MODULE_ENTRY(AudacityModule)
{
   // Create and register the importer
   // Trust the module manager not to leak this
   return safenew AudioUnitEffectsModule(moduleManager, path);
}

// ============================================================================
// Register this as a builtin module
// ============================================================================
DECLARE_BUILTIN_MODULE(AudioUnitEffectsBuiltin);

///////////////////////////////////////////////////////////////////////////////
//
// AudioUnitEffectsModule
//
///////////////////////////////////////////////////////////////////////////////

AudioUnitEffectsModule::AudioUnitEffectsModule(ModuleManagerInterface *moduleManager,
                                               const wxString *path)
{
   mModMan = moduleManager;
   if (path)
   {
      mPath = *path;
   }
}

AudioUnitEffectsModule::~AudioUnitEffectsModule()
{
   mPath.Clear();
}

// ============================================================================
// IdentInterface implementation
// ============================================================================

wxString AudioUnitEffectsModule::GetPath()
{
   return mPath;
}

wxString AudioUnitEffectsModule::GetSymbol()
{
   return wxT("Audio Unit Effects");
}

wxString AudioUnitEffectsModule::GetName()
{
   return XO("Audio Unit Effects");
}

wxString AudioUnitEffectsModule::GetVendor()
{
   return XO("The Audacity Team");
}

wxString AudioUnitEffectsModule::GetVersion()
{
   // This "may" be different if this were to be maintained as a separate DLL
   return AUDIOUNITEFFECTS_VERSION;
}

wxString AudioUnitEffectsModule::GetDescription()
{
   return XO("Provides Audio Unit Effects support to Audacity");
}

// ============================================================================
// ModuleInterface implementation
// ============================================================================

bool AudioUnitEffectsModule::Initialize()
{
   // Nothing to do here
   return true;
}

void AudioUnitEffectsModule::Terminate()
{
   // Nothing to do here
   return;
}

bool AudioUnitEffectsModule::AutoRegisterPlugins(PluginManagerInterface & pm)
{
   // Nothing to be done here
   return true;
}

wxArrayString AudioUnitEffectsModule::FindPlugins(PluginManagerInterface & pm)
{
   wxArrayString effects;

   LoadAudioUnitsOfType(kAudioUnitType_Effect, effects);
   LoadAudioUnitsOfType(kAudioUnitType_Generator, effects);
   LoadAudioUnitsOfType(kAudioUnitType_MusicEffect, effects);
   LoadAudioUnitsOfType(kAudioUnitType_Mixer, effects);
   LoadAudioUnitsOfType(kAudioUnitType_Panner, effects);
   
   return effects;
}

bool AudioUnitEffectsModule::RegisterPlugin(PluginManagerInterface & pm, const wxString & path)
{
   wxString name;
   AudioComponent component = FindAudioUnit(path, name);
   if (component == NULL)
   {
      return false;
   }

   AudioUnitEffect effect(path, name, component);
   if (!effect.SetHost(NULL))
   {
      return false;
   }

   pm.RegisterPlugin(this, &effect);

   return true;
}

bool AudioUnitEffectsModule::IsPluginValid(const wxString & path)
{
   wxString name;
   return FindAudioUnit(path, name) != NULL;
}

IdentInterface *AudioUnitEffectsModule::CreateInstance(const wxString & path)
{
   // Acquires a resource for the application.
   wxString name;
   AudioComponent component = FindAudioUnit(path, name);
   if (component == NULL)
   {
      return NULL;
   }

   // Safety of this depends on complementary calls to DeleteInstance on the module manager side.
   return safenew AudioUnitEffect(path, name, component);
}

void AudioUnitEffectsModule::DeleteInstance(IdentInterface *instance)
{
   std::unique_ptr < AudioUnitEffect > {
      dynamic_cast<AudioUnitEffect *>(instance)
   };
}

// ============================================================================
// AudioUnitEffectsModule implementation
// ============================================================================

void AudioUnitEffectsModule::LoadAudioUnitsOfType(OSType inAUType,
                                                  wxArrayString & effects)
{
   AudioComponentDescription desc;
   AudioComponent component;

   desc.componentType = inAUType;
   desc.componentSubType = 0;
   desc.componentManufacturer = 0;
   desc.componentFlags = 0;
   desc.componentFlagsMask = 0;

   component = AudioComponentFindNext(NULL, &desc);
   while (component != NULL)
   {
      OSStatus result;
      AudioComponentDescription found;

      result = AudioComponentGetDescription(component, &found);
      if (result == noErr)
      {
         CFStringRef cfName;
         result = AudioComponentCopyName(component, &cfName);
         CFunique_ptr<const __CFString> uName{ cfName };

         if (result == noErr)
         {
            wxString name = wxCFStringRef::AsString(cfName);
      
            effects.Add(wxString::Format(wxT("%-4.4s/%-4.4s/%-4.4s/%s"),
                        FromOSType(found.componentManufacturer).c_str(),
                        FromOSType(found.componentType).c_str(),
                        FromOSType(found.componentSubType).c_str(),
                        name.c_str()));
         }
      }

      component = AudioComponentFindNext(component, &desc);
   }
}

AudioComponent AudioUnitEffectsModule::FindAudioUnit(const wxString & path,
                                                     wxString & name)
{
   wxStringTokenizer tokens(path, wxT("/"));

   AudioComponentDescription desc;

   desc.componentManufacturer = ToOSType(tokens.GetNextToken());
   desc.componentType = ToOSType(tokens.GetNextToken());
   desc.componentSubType = ToOSType(tokens.GetNextToken());
   desc.componentFlags = 0;
   desc.componentFlagsMask = 0;

   name = tokens.GetNextToken();

   return AudioComponentFindNext(NULL, &desc);
}

wxString AudioUnitEffectsModule::FromOSType(OSType type)
{
   OSType rev = (type & 0xff000000) >> 24 |
                (type & 0x00ff0000) >> 8  |
                (type & 0x0000ff00) << 8  |
                (type & 0x000000ff) << 24;
   
   return wxString::FromUTF8((char *)&rev, 4).c_str();
}

OSType AudioUnitEffectsModule::ToOSType(const wxString & type)
{
   wxCharBuffer buf = type.ToUTF8();

   OSType rev = ((unsigned char)buf.data()[0]) << 24 |
                ((unsigned char)buf.data()[1]) << 16 |
                ((unsigned char)buf.data()[2]) << 8 |
                ((unsigned char)buf.data()[3]);

   return rev;
}

///////////////////////////////////////////////////////////////////////////////
//
// AudioUnitEffectOptionsDialog
//
///////////////////////////////////////////////////////////////////////////////

class AudioUnitEffectOptionsDialog final : public wxDialogWrapper
{
public:
   AudioUnitEffectOptionsDialog(wxWindow * parent, EffectHostInterface *host);
   virtual ~AudioUnitEffectOptionsDialog();

   void PopulateOrExchange(ShuttleGui & S);

   void OnOk(wxCommandEvent & evt);

private:
   EffectHostInterface *mHost;

   bool mUseLatency;
   wxString mUIType;

   wxArrayString mUITypes;

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(AudioUnitEffectOptionsDialog, wxDialogWrapper)
   EVT_BUTTON(wxID_OK, AudioUnitEffectOptionsDialog::OnOk)
END_EVENT_TABLE()

AudioUnitEffectOptionsDialog::AudioUnitEffectOptionsDialog(wxWindow * parent, EffectHostInterface *host)
:  wxDialogWrapper(parent, wxID_ANY, wxString(_("Audio Unit Effect Options")))
{
   mHost = host;

   mUITypes.Add(_("Full"));
   mUITypes.Add(_("Generic"));
   mUITypes.Add(_("Basic"));

   mHost->GetSharedConfig(wxT("Options"), wxT("UseLatency"), mUseLatency, true);
   mHost->GetSharedConfig(wxT("Options"), wxT("UIType"), mUIType, wxT("Full"));

   mUIType = wxGetTranslation(mUIType);

   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);
}

AudioUnitEffectOptionsDialog::~AudioUnitEffectOptionsDialog()
{
}

void AudioUnitEffectOptionsDialog::PopulateOrExchange(ShuttleGui & S)
{
   
   S.SetBorder(5);
   S.StartHorizontalLay(wxEXPAND, 1);
   {
      S.StartVerticalLay(false);
      {
         S.StartStatic(_("Latency Compensation"));
         {
            S.AddVariableText(wxString() +
               _("As part of their processing, some Audio Unit effects must delay returning ") +
               _("audio to Audacity. When not compensating for this delay, you will ") +
               _("notice that small silences have been inserted into the audio. ") +
               _("Enabling this option will provide that compensation, but it may ") +
               _("not work for all Audio Unit effects."))->Wrap(650);

            S.StartHorizontalLay(wxALIGN_LEFT);
            {
               S.TieCheckBox(_("Enable &compensation"),
                             mUseLatency);
            }
            S.EndHorizontalLay();
         }
         S.EndStatic();

         S.StartStatic(_("User Interface"));
         {
            S.AddVariableText(wxString() +
               _("Select \"Full\" to use the graphical interface if supplied by the Audio Unit.") +
               _(" Select \"Generic\" to use the system supplied generic interface.") +
               _(" Select \"Basic\" for A basic text-only interface. ") +
               _(" Reopen the effect for this to take effect."))->Wrap(650);

            S.StartHorizontalLay(wxALIGN_LEFT);
            {
               S.TieChoice(_("Select &interface"),
                           mUIType,
                           &mUITypes);
            }
            S.EndHorizontalLay();
         }
         S.EndStatic();
      }
      S.EndVerticalLay();
   }
   S.EndHorizontalLay();

   S.AddStandardButtons();

   Layout();
   Fit();
   Center();
}

void AudioUnitEffectOptionsDialog::OnOk(wxCommandEvent & WXUNUSED(evt))
{
   if (!Validate())
   {
      return;
   }

   ShuttleGui S(this, eIsGettingFromDialog);
   PopulateOrExchange(S);

   if (mUIType == _("Full"))
   {
      mUIType = wxT("Full");
   }
   else if (mUIType == _("Generic"))
   {
      mUIType = wxT("Generic");
   }
   else if (mUIType == _("Basic"))
   {
      mUIType = wxT("Basic");
   }

   mHost->SetSharedConfig(wxT("Options"), wxT("UseLatency"), mUseLatency);
   mHost->SetSharedConfig(wxT("Options"), wxT("UIType"), mUIType);

   EndModal(wxID_OK);
}

///////////////////////////////////////////////////////////////////////////////
//
// AudioUnitEffectExportDialog
//
///////////////////////////////////////////////////////////////////////////////

#define PRESET_LOCAL_PATH wxT("/Library/Audio/Presets")
#define PRESET_USER_PATH wxT("~/Library/Audio/Presets")

class AudioUnitEffectExportDialog final : public wxDialogWrapper
{
public:
   AudioUnitEffectExportDialog(wxWindow * parent, AudioUnitEffect *effect);
   virtual ~AudioUnitEffectExportDialog();

   void PopulateOrExchange(ShuttleGui & S);

   void OnOk(wxCommandEvent & evt);

private:
   wxWindow *mParent;
   AudioUnitEffect *mEffect;

   wxListCtrl *mList;

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(AudioUnitEffectExportDialog, wxDialogWrapper)
   EVT_BUTTON(wxID_OK, AudioUnitEffectExportDialog::OnOk)
END_EVENT_TABLE()

AudioUnitEffectExportDialog::AudioUnitEffectExportDialog(wxWindow * parent, AudioUnitEffect *effect)
:  wxDialogWrapper(parent, wxID_ANY, wxString(_("Export Audio Unit Presets")))
{
   mEffect = effect;

   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);
}

AudioUnitEffectExportDialog::~AudioUnitEffectExportDialog()
{
}

void AudioUnitEffectExportDialog::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(5);
   S.StartHorizontalLay(wxEXPAND, 1);
   {
      S.StartVerticalLay(true);
      {
         S.StartStatic(_("Presets (may select multiple)"));
         {
            S.SetStyle(wxLC_REPORT | wxLC_HRULES | wxLC_VRULES |
                       wxLC_NO_SORT_HEADER);
            mList = S.AddListControlReportMode();
            mList->InsertColumn(0, _("Preset"), wxLIST_FORMAT_LEFT);
         }
         S.EndStatic();
      }
      S.EndVerticalLay();
   }
   S.EndHorizontalLay();

   S.AddStandardButtons();

   wxArrayString presets;

   mEffect->mHost->GetPrivateConfigSubgroups(mEffect->mHost->GetUserPresetsGroup(wxEmptyString), presets);

   presets.Sort();

   for (size_t i = 0, cnt = presets.GetCount(); i < cnt; i++)
   {
      mList->InsertItem(i, presets[i]);
   }

   mList->SetColumnWidth(0, wxLIST_AUTOSIZE);

   // Set the list size...with a little extra for good measure
   wxSize sz = mList->GetBestSize();
   sz.x += 5;
   sz.y += 5;
   mList->SetMinSize(sz);

   Layout();
   Fit();
   Center();
}

void AudioUnitEffectExportDialog::OnOk(wxCommandEvent & WXUNUSED(evt))
{
   // Save active settings
   wxString settingsName(wxT("Export Save"));
   mEffect->SaveParameters(settingsName);

   // Look for selected presets
   long sel = mList->GetNextItem(-1, wxLIST_NEXT_ALL, wxLIST_STATE_SELECTED);
   while (sel >= 0)
   {
      wxString name = mList->GetItemText(sel);

      // Make the preset current
      mEffect->LoadParameters(mEffect->mHost->GetUserPresetsGroup(name));

      // Make sure the user preset directory exists
      wxString path;
      path.Printf(wxT("%s/%s/%s/%s.aupreset"),
                  PRESET_USER_PATH,
                  mEffect->mVendor.c_str(),
                  mEffect->mName.c_str(),
                  name.c_str());
      wxFileName fn(path);
      fn.Normalize();
      fn.Mkdir(0755, wxPATH_MKDIR_FULL);
      path = fn.GetFullPath();

      // First set the name of the preset
      wxCFStringRef cfname(name);

      AUPreset preset;
      preset.presetNumber = -1; // indicates user preset
      preset.presetName = cfname;

      AudioUnitSetProperty(mEffect->mUnit,
                           kAudioUnitProperty_PresentPreset,
                           kAudioUnitScope_Global,
                           0,
                           &preset,
                           sizeof(preset));

      // Now retrieve the preset content
      CFPropertyListRef content;
      UInt32 size = sizeof(content);
      AudioUnitGetProperty(mEffect->mUnit,
                           kAudioUnitProperty_ClassInfo,
                           kAudioUnitScope_Global,
                           0,
                           &content,
                           &size);

      // And convert it to XML
      CFunique_ptr<const __CFData> xml {
         CFPropertyListCreateXMLData(kCFAllocatorDefault, content)
      };
      if (xml)
      {
         // Create the CFURL for the path
         CFunique_ptr<const __CFURL> url {
            CFURLCreateWithFileSystemPath(kCFAllocatorDefault,
                                          wxCFStringRef(path),
                                          kCFURLPOSIXPathStyle,
                                          false)
         };
         if (url)
         {
            SInt32 error;
            Boolean res = CFURLWriteDataAndPropertiesToResource(url.get(),
                                                                xml.get(),
                                                                NULL,
                                                                &error);
         }
      }

      // And continue to the next selected preset
      sel = mList->GetNextItem(sel, wxLIST_NEXT_ALL, wxLIST_STATE_SELECTED);
   }

   // Restore active settings
   mEffect->LoadParameters(settingsName);
   mEffect->mHost->RemovePrivateConfigSubgroup(settingsName);
   
   EndModal(wxID_OK);
}

///////////////////////////////////////////////////////////////////////////////
//
// AudioUnitEffectImportDialog
//
///////////////////////////////////////////////////////////////////////////////

class AudioUnitEffectImportDialog final : public wxDialogWrapper
{
public:
   AudioUnitEffectImportDialog(wxWindow * parent, AudioUnitEffect *effect);
   virtual ~AudioUnitEffectImportDialog();

   void PopulateOrExchange(ShuttleGui & S);

   void OnOk(wxCommandEvent & evt);

private:
   wxWindow *mParent;
   AudioUnitEffect *mEffect;

   wxListCtrl *mList;

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(AudioUnitEffectImportDialog, wxDialogWrapper)
   EVT_BUTTON(wxID_OK, AudioUnitEffectImportDialog::OnOk)
END_EVENT_TABLE()

AudioUnitEffectImportDialog::AudioUnitEffectImportDialog(wxWindow * parent, AudioUnitEffect *effect)
:  wxDialogWrapper(parent, wxID_ANY, wxString(_("Import Audio Unit Presets")))
{
   mEffect = effect;

   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);
}

AudioUnitEffectImportDialog::~AudioUnitEffectImportDialog()
{
}

void AudioUnitEffectImportDialog::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(5);
   S.StartHorizontalLay(wxEXPAND, 1);
   {
      S.StartVerticalLay(true);
      {
         S.StartStatic(_("Presets (may select multiple)"));
         {
            S.SetStyle(wxLC_REPORT | wxLC_HRULES | wxLC_VRULES |
                       wxLC_NO_SORT_HEADER);
            mList = S.AddListControlReportMode();
            mList->InsertColumn(0, _("Preset"), wxLIST_FORMAT_LEFT);
            mList->InsertColumn(1, _("Location"), wxLIST_FORMAT_LEFT);
         }
         S.EndStatic();
      }
      S.EndVerticalLay();
   }
   S.EndHorizontalLay();

   S.AddStandardButtons();

   wxArrayString presets;

   // Make sure the user preset directory exists
   wxString path;
   path.Printf(wxT("%s/%s/%s"),
               PRESET_LOCAL_PATH,
               mEffect->mVendor.c_str(),
               mEffect->mName.c_str());
   wxFileName fn(path);
   fn.Normalize();
   
   // Get all presets in the local domain for this effect
   wxDir::GetAllFiles(fn.GetFullPath(), &presets, wxT("*.aupreset"));

   fn.PrependDir(wxT("~"));
   fn.Normalize();

   // Get all presets in the user domain for this effect
   wxDir::GetAllFiles(fn.GetFullPath(), &presets, wxT("*.aupreset"));
   
   presets.Sort();

   for (size_t i = 0, cnt = presets.GetCount(); i < cnt; i++)
   {
      fn = presets[i];
      mList->InsertItem(i, fn.GetName());
      mList->SetItem(i, 1, fn.GetPath());
   }

   mList->SetColumnWidth(0, wxLIST_AUTOSIZE);
   mList->SetColumnWidth(1, wxLIST_AUTOSIZE);

   // Set the list size...with a little extra for good measure
   wxSize sz = mList->GetBestSize();
   sz.x += 5;
   sz.y += 5;
   mList->SetMinSize(sz);

   Layout();
   Fit();
   Center();
}

void AudioUnitEffectImportDialog::OnOk(wxCommandEvent & WXUNUSED(evt))
{
   // Save active settings
   wxString settingsName(wxT("Import Save"));
   mEffect->SaveParameters(settingsName);

   // Look for selected presets
   long sel = -1;
   while ((sel = mList->GetNextItem(sel, wxLIST_NEXT_ALL, wxLIST_STATE_SELECTED)) >= 0)
   {
      wxListItem item;
      item.SetId(sel);
      item.SetColumn(1);
      item.SetMask(wxLIST_MASK_TEXT);
      mList->GetItem(item);

      wxString path;
      path.Printf(wxT("%s/%s.aupreset"),
                  item.GetText().c_str(),
                  mList->GetItemText(sel).c_str());

      // Create the CFURL for the path
      CFunique_ptr<const __CFURL> url {
         CFURLCreateWithFileSystemPath(kCFAllocatorDefault,
                                       wxCFStringRef(path),
                                       kCFURLPOSIXPathStyle,
                                       false)
      };

      if (!url)
      {
         continue;
      }

      CFDataRef xml;
      SInt32 error;
      Boolean res = CFURLCreateDataAndPropertiesFromResource(kCFAllocatorDefault,
                                                             url.get(),
                                                             &xml,
                                                             NULL,
                                                             NULL,
                                                             &error);
      CFunique_ptr<const __CFData> uxml { xml };

      if (!res)
      {
         continue;
      }

      CFunique_ptr<char> content {
         (char*)CFPropertyListCreateFromXMLData(kCFAllocatorDefault,
                                         xml,
                                         kCFPropertyListImmutable,
                                         NULL)
      };

      if (!content)
      {
         continue;
      }

      OSStatus result = AudioUnitSetProperty(mEffect->mUnit,
                                             kAudioUnitProperty_ClassInfo,
                                             kAudioUnitScope_Global,
                                             0,
                                             &content,
                                             sizeof(content));

      mEffect->SaveUserPreset(mEffect->mHost->GetUserPresetsGroup(mList->GetItemText(sel)));
   }

   // Restore active settings
   mEffect->LoadParameters(settingsName);
   mEffect->mHost->RemovePrivateConfigSubgroup(settingsName);
   
   EndModal(wxID_OK);
}

///////////////////////////////////////////////////////////////////////////////
//
// AudioUnitEffect
//
///////////////////////////////////////////////////////////////////////////////

AudioUnitEffect::AudioUnitEffect(const wxString & path,
                                 const wxString & name,
                                 AudioComponent component,
                                 AudioUnitEffect *master)
{
   mPath = path;
   mName = name.AfterFirst(wxT(':')).Trim(true).Trim(false);
   mVendor = name.BeforeFirst(wxT(':')).Trim(true).Trim(false);
   mComponent = component;
   mMaster = master;

   mUnit = NULL;
   
   mBlockSize = 0.0;
   mInteractive = false;
   mIsGraphical = false;

   mUIHost = NULL;
   mDialog = NULL;
   mParent = NULL;

   mInputList = NULL;
   mOutputList = NULL;

   mUnitInitialized = false;

   mEventListenerRef = NULL;
}

AudioUnitEffect::~AudioUnitEffect()
{
   if (mUnitInitialized)
   {
      AudioUnitUninitialize(mUnit);
   }

   if (mEventListenerRef)
   {
      AUListenerDispose(mEventListenerRef);
   }

   if (mUnit)
   {
      AudioComponentInstanceDispose(mUnit);
   }
}

// ============================================================================
// IdentInterface implementation
// ============================================================================

wxString AudioUnitEffect::GetPath()
{
   return mPath;
}

wxString AudioUnitEffect::GetSymbol()
{
   return mName;
}

wxString AudioUnitEffect::GetName()
{
   return GetSymbol();
}

wxString AudioUnitEffect::GetVendor()
{
   return mVendor;
}

wxString AudioUnitEffect::GetVersion()
{
   UInt32 version;

   OSStatus result = AudioComponentGetVersion(mComponent, &version);

   return wxString::Format(wxT("%d.%d.%d"),
                           (version >> 16) & 0xffff,
                           (version >> 8) & 0xff,
                           version & 0xff);
}

wxString AudioUnitEffect::GetDescription()
{
   return wxT("N/A");
}

// ============================================================================
// EffectIdentInterface implementation
// ============================================================================

EffectType AudioUnitEffect::GetType()
{
   if (mAudioIns == 0 && mAudioOuts == 0)
   {
      return EffectTypeNone;
   }

   if (mAudioIns == 0)
   {
      return EffectTypeGenerate;
   }

   if (mAudioOuts == 0)
   {
      return EffectTypeAnalyze;
   }

   return EffectTypeProcess;
}

wxString AudioUnitEffect::GetFamily()
{
   return AUDIOUNITEFFECTS_FAMILY;
}

bool AudioUnitEffect::IsInteractive()
{
   return mInteractive;
}

bool AudioUnitEffect::IsDefault()
{
   return false;
}

bool AudioUnitEffect::IsLegacy()
{
   return false;
}

bool AudioUnitEffect::SupportsRealtime()
{
   return GetType() == EffectTypeProcess;
}

bool AudioUnitEffect::SupportsAutomation()
{
   OSStatus result;
   UInt32 dataSize;
   Boolean isWritable;

   result = AudioUnitGetPropertyInfo(mUnit,
                                     kAudioUnitProperty_ParameterList,
                                     kAudioUnitScope_Global,
                                     0,
                                     &dataSize,
                                     &isWritable);
   if (result != noErr)
   {
      return false;
   }

   UInt32 cnt = dataSize / sizeof(AudioUnitParameterID);
   AudioUnitParameterID *array = new AudioUnitParameterID[cnt];

   result = AudioUnitGetProperty(mUnit,
                                 kAudioUnitProperty_ParameterList,
                                 kAudioUnitScope_Global,
                                 0,
                                 array,
                                 &dataSize);  
   if (result != noErr)
   {
      delete [] array;
      return false;
   }

   for (int i = 0; i < cnt; i++)
   {
      AudioUnitParameterInfoEx info;
      dataSize = sizeof(info);
      result = AudioUnitGetProperty(mUnit,
                                    kAudioUnitProperty_ParameterInfo,
                                    kAudioUnitScope_Global,
                                    array[i],
                                    &info,
                                    &dataSize);  
      if (result != noErr)
      {
         delete [] array;
         return false;
      }

      if (info.flags & kAudioUnitParameterFlag_IsWritable)
      {
         // All we need is one
         delete [] array;
         return true;
      }
   }

   delete [] array;
   
   return false;
}

// ============================================================================
// EffectClientInterface Implementation
// ============================================================================

bool AudioUnitEffect::SetHost(EffectHostInterface *host)
{
   OSStatus result;
   
   mHost = host;
   
   mSampleRate = 44100;
   result = AudioComponentInstanceNew(mComponent, &mUnit);
   if (!mUnit)
   {
      return false;
   }

   GetChannelCounts();

   SetRateAndChannels();

   // Retrieve the desired number of frames per slice
   UInt32 dataSize = sizeof(mBlockSize);
   mBlockSize = 512;
   AudioUnitGetProperty(mUnit,
                        kAudioUnitProperty_MaximumFramesPerSlice,
                        kAudioUnitScope_Global,
                        0,
                        &mBlockSize,
                        &dataSize);

   // mHost will be null during registration
   if (mHost)
   {
      mHost->GetSharedConfig(wxT("Options"), wxT("UseLatency"), mUseLatency, true);
      mHost->GetSharedConfig(wxT("Options"), wxT("UIType"), mUIType, wxT("Full"));

      mUIType = wxGetTranslation(mUIType);

      bool haveDefaults;
      mHost->GetPrivateConfig(mHost->GetFactoryDefaultsGroup(), wxT("Initialized"), haveDefaults, false);
      if (!haveDefaults)
      {
         SaveParameters(mHost->GetFactoryDefaultsGroup());
         mHost->SetPrivateConfig(mHost->GetFactoryDefaultsGroup(), wxT("Initialized"), true);
      }

      LoadParameters(mHost->GetCurrentSettingsGroup());
   } 

   if (!mMaster)
   {
     result = AUEventListenerCreate(AudioUnitEffect::EventListenerCallback,
                                     this,
                                     (CFRunLoopRef)GetCFRunLoopFromEventLoop(GetCurrentEventLoop()),
                                     kCFRunLoopDefaultMode,
                                     0.0,
                                     0.0,
                                     &mEventListenerRef);
      if (result != noErr)
      {
         return false;
      }

      AudioUnitEvent event;
 
      event.mEventType = kAudioUnitEvent_ParameterValueChange;
      event.mArgument.mParameter.mAudioUnit = mUnit;
      event.mArgument.mParameter.mScope = kAudioUnitScope_Global;
      event.mArgument.mParameter.mElement = 0;

      UInt32 dataSize;
      Boolean isWritable;

      // Retrieve the list of properties
      result = AudioUnitGetPropertyInfo(mUnit,
                                        kAudioUnitProperty_ParameterList,
                                        kAudioUnitScope_Global,
                                        0,
                                        &dataSize,
                                        &isWritable);
      if (result != noErr)
      {
         return false;
      }

      // And get them
      UInt32 cnt = dataSize / sizeof(AudioUnitParameterID);
      AudioUnitParameterID *array = new AudioUnitParameterID[cnt];
   
      result = AudioUnitGetProperty(mUnit,
                                    kAudioUnitProperty_ParameterList,
                                    kAudioUnitScope_Global,
                                    0,
                                    array,
                                    &dataSize);  
      if (result != noErr)
      {
         delete [] array;
         return false;
      }

      // Register them as something we're interested in
      for (int i = 0; i < cnt; i++)
      {
         event.mArgument.mParameter.mParameterID = array[i];
         result = AUEventListenerAddEventType(mEventListenerRef,
                                              this,
                                              &event);
         if (result != noErr)
         {
            delete [] array;
            return false;
         }
      }

      delete [] array;

      event.mEventType = kAudioUnitEvent_PropertyChange;
      event.mArgument.mProperty.mAudioUnit = mUnit;
      event.mArgument.mProperty.mPropertyID = kAudioUnitProperty_Latency;
      event.mArgument.mProperty.mScope = kAudioUnitScope_Global;
      event.mArgument.mProperty.mElement = 0;

      result = AUEventListenerAddEventType(mEventListenerRef,
                                           this,
                                           &event);
      if (result != noErr)
      {
         return false;
      }

      AudioUnitCocoaViewInfo cocoaViewInfo;
      dataSize = sizeof(AudioUnitCocoaViewInfo);
   
      // Check for a Cocoa UI
      result = AudioUnitGetProperty(mUnit,
                                    kAudioUnitProperty_CocoaUI,
                                    kAudioUnitScope_Global,
                                    0,
                                    &cocoaViewInfo,
                                    &dataSize);

      bool hasCocoa = result == noErr;

      // Check for a Carbon UI
      AudioComponentDescription compDesc;
      dataSize = sizeof(compDesc);
      result = AudioUnitGetProperty(mUnit,
                                    kAudioUnitProperty_GetUIComponentList,
                                    kAudioUnitScope_Global,
                                    0,
                                    &compDesc,
                                    &dataSize);
      bool hasCarbon = result == noErr;

      mInteractive = (cnt > 0) || hasCocoa || hasCarbon;
   }

   return true;
}

unsigned AudioUnitEffect::GetAudioInCount()
{
   return mAudioIns;
}

unsigned AudioUnitEffect::GetAudioOutCount()
{
   return mAudioOuts;
}

int AudioUnitEffect::GetMidiInCount()
{
   return 0;
}

int AudioUnitEffect::GetMidiOutCount()
{
   return 0;
}

void AudioUnitEffect::SetSampleRate(double rate)
{
   mSampleRate = rate;
}

size_t AudioUnitEffect::SetBlockSize(size_t maxBlockSize)
{
   return mBlockSize;
}

sampleCount AudioUnitEffect::GetLatency()
{
   // Retrieve the latency (can be updated via an event)
   if (mUseLatency && !mLatencyDone)
   {
      mLatencyDone = true;

      Float64 latency = 0.0;
      UInt32 dataSize = sizeof(latency);
      AudioUnitGetProperty(mUnit,
                           kAudioUnitProperty_Latency,
                           kAudioUnitScope_Global,
                           0,
                           &latency,
                           &dataSize);  

      return sampleCount( latency * mSampleRate );
   }

   return 0;
}

size_t AudioUnitEffect::GetTailSize()
{
   // Retrieve the tail time
   Float64 tailTime = 0.0;
   UInt32 dataSize = sizeof(tailTime);
   AudioUnitGetProperty(mUnit,
                        kAudioUnitProperty_TailTime,
                        kAudioUnitScope_Global,
                        0,
                        &tailTime,
                        &dataSize);  

   return tailTime * mSampleRate;
}

bool AudioUnitEffect::IsReady()
{
   return mReady;
}

bool AudioUnitEffect::ProcessInitialize(sampleCount WXUNUSED(totalLen), ChannelNames WXUNUSED(chanMap))
{
   OSStatus result;

   mInputList = new AudioBufferList[mAudioIns];
   mInputList->mNumberBuffers = mAudioIns;
   
   mOutputList = new AudioBufferList[mAudioOuts];
   mOutputList->mNumberBuffers = mAudioOuts;

   memset(&mTimeStamp, 0, sizeof(AudioTimeStamp));
   mTimeStamp.mSampleTime = 0; // This is a double-precision number that should
                               // accumulate the number of frames processed so far
   mTimeStamp.mFlags = kAudioTimeStampSampleTimeValid;

   if (!SetRateAndChannels())
   {
      return false;
   }

   AURenderCallbackStruct callbackStruct;
   callbackStruct.inputProc = RenderCallback;
   callbackStruct.inputProcRefCon = this;
   result = AudioUnitSetProperty(mUnit,
                                 kAudioUnitProperty_SetRenderCallback,
                                 kAudioUnitScope_Input,
                                 0,
                                 &callbackStruct,
                                 sizeof(AURenderCallbackStruct));
   if (result != noErr)
   {
      printf("Setting input render callback failed.\n");
      return false;
   }

   result = AudioUnitReset(mUnit, kAudioUnitScope_Global, 0);
   if (result != noErr)
   {
      return false;
   }

   mLatencyDone = false;

   mReady = true;

   return true;
}

bool AudioUnitEffect::ProcessFinalize()
{
   mReady = false;

   if (mOutputList)
   {
      delete [] mOutputList;
      mOutputList = NULL;
   }

   if (mInputList)
   {
      delete [] mInputList;
      mInputList = NULL;
   }

   return true;
}

size_t AudioUnitEffect::ProcessBlock(float **inBlock, float **outBlock, size_t blockLen)
{
   for (int i = 0; i < mAudioIns; i++)
   {
      mInputList->mBuffers[i].mNumberChannels = 1;
      mInputList->mBuffers[i].mData = inBlock[i];
      mInputList->mBuffers[i].mDataByteSize = sizeof(float) * blockLen;
   }

   for (int i = 0; i < mAudioOuts; i++)
   {
      mOutputList->mBuffers[i].mNumberChannels = 1;
      mOutputList->mBuffers[i].mData = outBlock[i];
      mOutputList->mBuffers[i].mDataByteSize = sizeof(float) * blockLen;
   }

   AudioUnitRenderActionFlags flags = 0;
   OSStatus result;

   result = AudioUnitRender(mUnit,
                            &flags,
                            &mTimeStamp,
                            0,
                            blockLen,
                            mOutputList);
   if (result != noErr)
   {
      printf("Render failed: %d %4.4s\n", (int)result, (char *)&result);
      return 0;
   }

   mTimeStamp.mSampleTime += blockLen;

   return blockLen;
}

bool AudioUnitEffect::RealtimeInitialize()
{
   mMasterIn = new float *[mAudioIns];

   for (int i = 0; i < mAudioIns; i++)
   {
      mMasterIn[i] = new float[mBlockSize];
      memset(mMasterIn[i], 0, mBlockSize * sizeof(float));
   }

   mMasterOut = new float *[mAudioOuts];
   for (int i = 0; i < mAudioOuts; i++)
   {
      mMasterOut[i] = new float[mBlockSize];
   }

   return ProcessInitialize(0);
}

bool AudioUnitEffect::RealtimeAddProcessor(unsigned numChannels, float sampleRate)
{
   auto slave = make_movable<AudioUnitEffect>(mPath, mName, mComponent, this);
   if (!slave->SetHost(NULL))
      return false;

   slave->SetBlockSize(mBlockSize);
   slave->SetChannelCount(numChannels);
   slave->SetSampleRate(sampleRate);

   if (!CopyParameters(mUnit, slave->mUnit))
      return false;

   auto pSlave = slave.get();
   mSlaves.push_back(std::move(slave));

   return pSlave->ProcessInitialize(0);
}

bool AudioUnitEffect::RealtimeFinalize()
{
   for (size_t i = 0, cnt = mSlaves.size(); i < cnt; i++)
      mSlaves[i]->ProcessFinalize();
   mSlaves.clear();

   for (int i = 0; i < mAudioIns; i++)
   {
      delete [] mMasterIn[i];
   }
   delete [] mMasterIn;

   for (int i = 0; i < mAudioOuts; i++)
   {
      delete [] mMasterOut[i];
   }
   delete [] mMasterOut;

   return ProcessFinalize();
}

bool AudioUnitEffect::RealtimeSuspend()
{
   return true;
}

bool AudioUnitEffect::RealtimeResume()
{
   OSStatus result;

   result = AudioUnitReset(mUnit, kAudioUnitScope_Global, 0);
   if (result != noErr)
   {
      return false;
   }

   return true;
}

bool AudioUnitEffect::RealtimeProcessStart()
{
   for (int i = 0; i < mAudioIns; i++)
   {
      memset(mMasterIn[i], 0, mBlockSize * sizeof(float));
   }

   mNumSamples = 0;

   return true;
}

size_t AudioUnitEffect::RealtimeProcess(int group,
                                             float **inbuf,
                                             float **outbuf,
                                             size_t numSamples)
{
   wxASSERT(numSamples <= mBlockSize);

   for (int c = 0; c < mAudioIns; c++)
   {
      for (decltype(numSamples) s = 0; s < numSamples; s++)
      {
         mMasterIn[c][s] += inbuf[c][s];
      }
   }
   mNumSamples = wxMax(numSamples, mNumSamples);

   return mSlaves[group]->ProcessBlock(inbuf, outbuf, numSamples);
}

bool AudioUnitEffect::RealtimeProcessEnd()
{
   ProcessBlock(mMasterIn, mMasterOut, mNumSamples);

   return true;
}

bool AudioUnitEffect::ShowInterface(wxWindow *parent, bool forceModal)
{
   if (mDialog)
   {
      mDialog->Close(true);
      return false;
   }

   mDialog = mHost->CreateUI(parent, this);
   if (!mDialog)
   {
      return false;
   }

   if ((SupportsRealtime() || GetType() == EffectTypeAnalyze) && !forceModal)
   {
      mDialog->Show();

      return false;
   }

   bool res = mDialog->ShowModal() != 0;
   mDialog = NULL;

   return res;
}

bool AudioUnitEffect::GetAutomationParameters(EffectAutomationParameters & parms)
{
   OSStatus result;
   UInt32 dataSize;
   Boolean isWritable;

   result = AudioUnitGetPropertyInfo(mUnit,
                                     kAudioUnitProperty_ParameterList,
                                     kAudioUnitScope_Global,
                                     0,
                                     &dataSize,
                                     &isWritable);
   if (result != noErr)
   {
      return false;
   }

   UInt32 cnt = dataSize / sizeof(AudioUnitParameterID);
   AudioUnitParameterID *array = new AudioUnitParameterID[cnt];

   result = AudioUnitGetProperty(mUnit,
                                 kAudioUnitProperty_ParameterList,
                                 kAudioUnitScope_Global,
                                 0,
                                 array,
                                 &dataSize);  
   if (result != noErr)
   {
      delete [] array;
      return false;
   }

   for (int i = 0; i < cnt; i++)
   {
      AudioUnitParameterInfoEx info;
      dataSize = sizeof(info);
      result = AudioUnitGetProperty(mUnit,
                                    kAudioUnitProperty_ParameterInfo,
                                    kAudioUnitScope_Global,
                                    array[i],
                                    &info,
                                    &dataSize);  
      if (result != noErr)
      {
         delete [] array;
         return false;
      }

      wxString name;
      if (info.flags & kAudioUnitParameterFlag_HasCFNameString)
      {
         name = wxCFStringRef::AsString(info.cfNameString);
      }

      if (name.IsEmpty())
      {
         continue;
      }

      AudioUnitParameterValue value;
      result = AudioUnitGetParameter(mUnit,
                                     array[i],
                                     kAudioUnitScope_Global,
                                     0,
                                     &value);
      if (result != noErr)
      {
         delete [] array;
         return false;
      }
      parms.Write(name, value);
   }

   delete [] array;

   return true;
}

bool AudioUnitEffect::SetAutomationParameters(EffectAutomationParameters & parms)
{
   OSStatus result;
   UInt32 dataSize;
   Boolean isWritable;

   result = AudioUnitGetPropertyInfo(mUnit,
                                     kAudioUnitProperty_ParameterList,
                                     kAudioUnitScope_Global,
                                     0,
                                     &dataSize,
                                     &isWritable);
   if (result != noErr)
   {
      return false;
   }

   UInt32 cnt = dataSize / sizeof(AudioUnitParameterID);
   AudioUnitParameterID *array = new AudioUnitParameterID[cnt];

   result = AudioUnitGetProperty(mUnit,
                                 kAudioUnitProperty_ParameterList,
                                 kAudioUnitScope_Global,
                                 0,
                                 array,
                                 &dataSize);  
   if (result != noErr)
   {
      delete [] array;
      return false;
   }

   for (int i = 0; i < cnt; i++)
   {
      AudioUnitParameterInfoEx info;
      dataSize = sizeof(info);
      result = AudioUnitGetProperty(mUnit,
                                    kAudioUnitProperty_ParameterInfo,
                                    kAudioUnitScope_Global,
                                    array[i],
                                    &info,
                                    &dataSize);  
      if (result != noErr)
      {
         delete [] array;
         return false;
      }

      wxString name;
      if (info.flags & kAudioUnitParameterFlag_HasCFNameString)
      {
         name = wxCFStringRef::AsString(info.cfNameString);
      }

      if (name.IsEmpty())
      {
         continue;
      }


      double d = 0.0;
      if (!parms.Read(name, &d))
      {
         delete [] array;
         return false;
      }

      AudioUnitParameterValue value = d;
      result = AudioUnitSetParameter(mUnit,
                                     array[i],
                                     kAudioUnitScope_Global,
                                     0,
                                     value,
                                     0);
      if (result != noErr)
      {
         delete [] array;
         return false;
      }
   }

   delete [] array;

   AudioUnitParameter aup;
   aup.mAudioUnit = mUnit;
   aup.mParameterID = kAUParameterListener_AnyParameter;
   aup.mScope = kAudioUnitScope_Global;
   aup.mElement = 0;
   AUParameterListenerNotify(NULL, NULL, &aup);

   return true;
}

bool AudioUnitEffect::LoadUserPreset(const wxString & name)
{
   return LoadParameters(name);
}

bool AudioUnitEffect::SaveUserPreset(const wxString & name)
{
   return SaveParameters(name);
}

bool AudioUnitEffect::LoadFactoryPreset(int id)
{
   OSStatus result;

   // Retrieve the list of factory presets
   CFArrayRef array;
   UInt32 dataSize = sizeof(CFArrayRef);
   result = AudioUnitGetProperty(mUnit,
                                 kAudioUnitProperty_FactoryPresets,
                                 kAudioUnitScope_Global,
                                 0,
                                 &array,
                                 &dataSize);
   CFunique_ptr < const __CFArray > uarray { array };
   if (result != noErr)
   {
      return false;
   }

   if (id < 0 || id >= CFArrayGetCount(array))
   {
      return false;
   }

   AUPreset *preset = (AUPreset *) CFArrayGetValueAtIndex(array, id);

   result = AudioUnitSetProperty(mUnit,
                                 kAudioUnitProperty_PresentPreset,
                                 kAudioUnitScope_Global,
                                 0,
                                 preset,
                                 sizeof(AUPreset));
   if (result == noErr)
   {
      AudioUnitParameter aup;
      aup.mAudioUnit = mUnit;
      aup.mParameterID = kAUParameterListener_AnyParameter;
      aup.mScope = kAudioUnitScope_Global;
      aup.mElement = 0;
      AUParameterListenerNotify(NULL, NULL, &aup);
   }

   return result == noErr;
}

bool AudioUnitEffect::LoadFactoryDefaults()
{
   return LoadParameters(mHost->GetFactoryDefaultsGroup());
}

wxArrayString AudioUnitEffect::GetFactoryPresets()
{
   OSStatus result;
   wxArrayString presets;

   // Retrieve the list of factory presets
   CFArrayRef array;
   UInt32 dataSize = sizeof(CFArrayRef);
   result = AudioUnitGetProperty(mUnit,
                                 kAudioUnitProperty_FactoryPresets,
                                 kAudioUnitScope_Global,
                                 0,
                                 &array,
                                 &dataSize);
   CFunique_ptr< const __CFArray > uarray { array };
   if (result == noErr)
   {
      for (CFIndex i = 0, cnt = CFArrayGetCount(array); i < cnt; i++)
      {
         AUPreset *preset = (AUPreset *) CFArrayGetValueAtIndex(array, i);
         presets.Add(wxCFStringRef::AsString(preset->presetName));
      }
   }
                        
   return presets;
}

// ============================================================================
// EffectUIClientInterface Implementation
// ============================================================================

void AudioUnitEffect::SetHostUI(EffectUIHostInterface *host)
{
   mUIHost = host;
}

bool AudioUnitEffect::PopulateUI(wxWindow *parent)
{
   // OSStatus result;

   mDialog = static_cast<wxDialog *>(wxGetTopLevelParent(parent));
   mParent = parent;

   wxPanel *container;
   {
      auto mainSizer = std::make_unique<wxBoxSizer>(wxVERTICAL);

      wxASSERT(mParent); // To justify safenew
      container = safenew wxPanelWrapper(mParent, wxID_ANY);
      mainSizer->Add(container, 1, wxEXPAND);

      mParent->SetSizer(mainSizer.release());
   }

   if (mUIType == wxT("Plain"))
   {
      if (!CreatePlain(mParent))
      {
         return false;
      }
   }
   else
   {
      auto pControl = std::make_unique<AUControl>();
      if (!pControl)
      {
         return false;
      }

      if (!pControl->Create(container, mComponent, mUnit, mUIType == wxT("Full")))
      {
         return false;
      }

      {
         auto innerSizer = std::make_unique<wxBoxSizer>(wxVERTICAL);

         innerSizer->Add(pControl.release(), 1, wxEXPAND);
         container->SetSizer(innerSizer.release());
      }

      mParent->SetMinSize(wxDefaultSize);

#ifdef __WXMAC__
#ifdef __WX_EVTLOOP_BUSY_WAITING__
      wxEventLoop::SetBusyWaiting(true);
#endif
#endif
   }

   mParent->PushEventHandler(this);

   return true;
}

bool AudioUnitEffect::IsGraphicalUI()
{
   return mUIType != wxT("Plain");
}

bool AudioUnitEffect::ValidateUI()
{
#if 0
   if (!mParent->Validate())
   {
      return false;
   }

   if (GetType() == EffectTypeGenerate)
   {
      mHost->SetDuration(mDuration->GetValue());
   }
#endif
   return true;
}

bool AudioUnitEffect::CreatePlain(wxWindow *parent)
{
   return false;
}

bool AudioUnitEffect::HideUI()
{
#if 0
   if (GetType() == EffectTypeAnalyze || mNumOutputControls > 0)
   {
      return false;
   }
#endif
   return true;
}

bool AudioUnitEffect::CloseUI()
{
#ifdef __WXMAC__
#ifdef __WX_EVTLOOP_BUSY_WAITING__
   wxEventLoop::SetBusyWaiting(false);
#endif
#endif

   mParent->RemoveEventHandler(this);

   mUIHost = NULL;
   mParent = NULL;
   mDialog = NULL;

   return true;
}

bool AudioUnitEffect::CanExportPresets()
{
   return true;
}

void AudioUnitEffect::ExportPresets()
{
   AudioUnitEffectExportDialog dlg(mDialog, this);
   dlg.ShowModal();
}

void AudioUnitEffect::ImportPresets()
{
   AudioUnitEffectImportDialog dlg(mDialog, this);
   dlg.ShowModal();
}

bool AudioUnitEffect::HasOptions()
{
   return true;
}

void AudioUnitEffect::ShowOptions()
{
   AudioUnitEffectOptionsDialog dlg(mParent, mHost);
   if (dlg.ShowModal())
   {
      // Reinitialize configuration settings
      mHost->GetSharedConfig(wxT("Options"), wxT("UseLatency"), mUseLatency, true);
      mHost->GetSharedConfig(wxT("Options"), wxT("UIType"), mUIType, wxT("Full"));

      mUIType = wxGetTranslation(mUIType);
   }
}

// ============================================================================
// AudioUnitEffect Implementation
// ============================================================================

bool AudioUnitEffect::LoadParameters(const wxString & group)
{
   wxString parms;
   if (!mHost->GetPrivateConfig(group, wxT("Parameters"), parms, wxEmptyString))
   {
      return false;
   }

   EffectAutomationParameters eap;
   if (!eap.SetParameters(parms))
   {
      return false;
   }

   return SetAutomationParameters(eap);
}

bool AudioUnitEffect::SaveParameters(const wxString & group)
{
   EffectAutomationParameters eap;
   if (!GetAutomationParameters(eap))
   {
      return false;
   }

   wxString parms;
   if (!eap.GetParameters(parms))
   {
      return false;
   }

   return mHost->SetPrivateConfig(group, wxT("Parameters"), parms);
}

bool AudioUnitEffect::SetRateAndChannels()
{
   OSStatus result;

   if (mUnitInitialized)
   {
      AudioUnitUninitialize(mUnit);

      mUnitInitialized = false;
   }

   AudioStreamBasicDescription streamFormat {
      // Float64 mSampleRate;
      mSampleRate,

      // UInt32  mFormatID;
      kAudioFormatLinearPCM,

      // UInt32  mFormatFlags;
      (kAudioFormatFlagsNativeFloatPacked |
          kAudioFormatFlagIsNonInterleaved),

      // UInt32  mBytesPerPacket;
      sizeof(float),

      // UInt32  mFramesPerPacket;
      1,

      // UInt32  mBytesPerFrame;
      sizeof(float),

      // UInt32  mChannelsPerFrame;
      mAudioIns,

      // UInt32  mBitsPerChannel;
      sizeof(float) * 8,

      // UInt32  mReserved;
      0
   };

   result = AudioUnitSetProperty(mUnit,
                                 kAudioUnitProperty_SampleRate,
                                 kAudioUnitScope_Global,
                                 0,
                                 &mSampleRate,
                                 sizeof(Float64));
   if (result != noErr)
   {
      printf("%ls Didn't accept sample rate on global\n", GetName().wx_str());
      return false;
   }

   if (mAudioIns > 0)
   {
      result = AudioUnitSetProperty(mUnit,
                                    kAudioUnitProperty_SampleRate,
                                    kAudioUnitScope_Input,
                                    0,
                                    &mSampleRate,
                                    sizeof(Float64));
      if (result != noErr)
      {
         printf("%ls Didn't accept sample rate on input\n", GetName().wx_str());
         return false;
      }

      result = AudioUnitSetProperty(mUnit,
                                    kAudioUnitProperty_StreamFormat,
                                    kAudioUnitScope_Input,
                                    0,
                                    &streamFormat,
                                    sizeof(AudioStreamBasicDescription));
      if (result != noErr)
      {
         printf("%ls didn't accept stream format on input\n", GetName().wx_str());
         return false;
      }
   }

   if (mAudioOuts > 0)
   {
      result = AudioUnitSetProperty(mUnit,
                                    kAudioUnitProperty_SampleRate,
                                    kAudioUnitScope_Output,
                                    0,
                                    &mSampleRate,
                                    sizeof(Float64));
      if (result != noErr)
      {
         printf("%ls Didn't accept sample rate on output\n", GetName().wx_str());
         return false;
      }
   
      streamFormat.mChannelsPerFrame = mAudioOuts;
      result = AudioUnitSetProperty(mUnit,
                                    kAudioUnitProperty_StreamFormat,
                                    kAudioUnitScope_Output,
                                    0,
                                    &streamFormat,
                                    sizeof(AudioStreamBasicDescription));
   
      if (result != noErr)
      {
         printf("%ls didn't accept stream format on output\n", GetName().wx_str());
         return false;
      }
   }

   result = AudioUnitInitialize(mUnit);
   if (result != noErr)
   {
      printf("Couldn't initialize audio unit\n");
      return false;
   }

   mUnitInitialized = true;

   return true;
}

bool AudioUnitEffect::CopyParameters(AudioUnit srcUnit, AudioUnit dstUnit)
{
   OSStatus result;
   int numParameters, i;
   AudioUnitParameterID *parameters;
   Float32 parameterValue;
   UInt32 size;

   // Get number of parameters by passing NULL in the data field and
   // getting back the size of the parameter list

   size = 0;
   result = AudioUnitGetProperty(srcUnit,
                                   kAudioUnitProperty_ParameterList,
                                   kAudioUnitScope_Global,
                                   0,
                                   NULL,
                                   &size);
   if (result != 0)
   {
      printf("Couldn't get number of parameters\n");
      return false;
   }

   // Now get the list of all parameter IDs

   numParameters = size / sizeof(AudioUnitParameterID);
   parameters = new AudioUnitParameterID[numParameters];
   result = AudioUnitGetProperty(srcUnit,
                                   kAudioUnitProperty_ParameterList,
                                   kAudioUnitScope_Global,
                                   0,
                                   parameters,
                                   &size);
   if (result != 0)
   {
      printf("Couldn't get parameter list\n");
      delete[] parameters;
      return false;
   }

   // Copy the parameters from the main unit to the unit specific to
   // this track

   for (i = 0; i < numParameters; i++)
   {
      result = AudioUnitGetParameter(srcUnit,
                                       parameters[i],
                                       kAudioUnitScope_Global,
                                       0,
                                       &parameterValue);
      if (result != 0)
      {
         printf("Couldn't get parameter %d: ID=%d\n", i, (int)parameters[i]);
         continue;
      }

      result = AudioUnitSetParameter(dstUnit,
                                       parameters[i],
                                       kAudioUnitScope_Global,
                                       0,
                                       parameterValue,
                                       0);
      if (result != 0)
      {
         printf("Couldn't set parameter %d: ID=%d\n", i, (int)parameters[i]);
      }
   }

   delete[] parameters;

   return true;
}

unsigned AudioUnitEffect::GetChannelCount()
{
   return mNumChannels;
}

void AudioUnitEffect::SetChannelCount(unsigned numChannels)
{
   mNumChannels = numChannels;
}

OSStatus AudioUnitEffect::Render(AudioUnitRenderActionFlags *inActionFlags,
                                 const AudioTimeStamp *inTimeStamp,
                                 UInt32 inBusNumber,
                                 UInt32 inNumFrames,
                                 AudioBufferList *ioData)
{
   for (int i = 0; i < ioData->mNumberBuffers; i++)
   {
      ioData->mBuffers[i].mData = mInputList->mBuffers[i].mData;
   }

   return 0;
}

// static
OSStatus AudioUnitEffect::RenderCallback(void *inRefCon,
                                         AudioUnitRenderActionFlags *inActionFlags,
                                         const AudioTimeStamp *inTimeStamp,
                                         UInt32 inBusNumber,
                                         UInt32 inNumFrames,
                                         AudioBufferList *ioData)
{
   return ((AudioUnitEffect *) inRefCon)->Render(inActionFlags,
                                                 inTimeStamp,
                                                 inBusNumber,
                                                 inNumFrames,
                                                 ioData);
}

void AudioUnitEffect::EventListener(const AudioUnitEvent *inEvent,
                                    AudioUnitParameterValue inParameterValue)
{
   // Handle property changes
   if (inEvent->mEventType == kAudioUnitEvent_PropertyChange)
   {
      // We're only registered for Latency changes
      if (inEvent->mArgument.mProperty.mPropertyID == kAudioUnitProperty_Latency)
      {
         // Allow change to be used
         //mLatencyDone = false;
      }

      return;
   }

   // Only parameter changes at this point

   if (mMaster)
   {
      // We're a slave, so just set the parameter
      AudioUnitSetParameter(mUnit,
                            inEvent->mArgument.mParameter.mParameterID,
                            kAudioUnitScope_Global,
                            0,
                            inParameterValue,
                            0);
   }
   else
   {
      // We're the master, so propogate 
      for (size_t i = 0, cnt = mSlaves.size(); i < cnt; i++)
         mSlaves[i]->EventListener(inEvent, inParameterValue);
   }
}
                           
// static
void AudioUnitEffect::EventListenerCallback(void *inCallbackRefCon,
                                            void *inObject,
                                            const AudioUnitEvent *inEvent,
                                            UInt64 inEventHostTime,
                                            AudioUnitParameterValue inParameterValue)
{
   ((AudioUnitEffect *) inCallbackRefCon)->EventListener(inEvent,
                                                         inParameterValue);
}

void AudioUnitEffect::GetChannelCounts()
{
   Boolean isWritable = 0;
   UInt32  dataSize = 0;
   OSStatus result;

   // Does AU have channel info
   result = AudioUnitGetPropertyInfo(mUnit,
                                     kAudioUnitProperty_SupportedNumChannels,
                                     kAudioUnitScope_Global,
                                     0,
                                     &dataSize,
                                     &isWritable);
   if (result)
   {
      // None supplied.  Apparently all FX type units can do any number of INs
      // and OUTs as long as they are the same number.  In this case, we'll
      // just say stereo.
      //
      // We should probably check to make sure we're dealing with an FX type.
      mAudioIns = 2;
      mAudioOuts = 2;
      return;
   }

   AUChannelInfo *info = (AUChannelInfo *) malloc(dataSize);

   // Retrieve the channel info
   result = AudioUnitGetProperty(mUnit,
                                 kAudioUnitProperty_SupportedNumChannels,
                                 kAudioUnitScope_Global,
                                 0,
                                 info,
                                 &dataSize);
   if (result)
   {
      // Oh well, not much we can do out this case
      mAudioIns = 2;
      mAudioOuts = 2;

      free(info);
      return;
   }

   // This is where it gets weird...not sure what is the best
   // way to do this really.  If we knew how many ins/outs we
   // really needed, we could make a better choice.

   bool haven2m = false;   // nothing -> mono
   bool haven2s = false;   // nothing -> stereo
   bool havem2n = false;   // mono -> nothing
   bool haves2n = false;   // stereo -> nothing
   bool havem2m = false;   // mono -> mono
   bool haves2s = false;   // stereo -> stereo
   bool havem2s = false;   // mono -> stereo
   bool haves2m = false;   // stereo -> mono

   mAudioIns = 2;
   mAudioOuts = 2;

   // Look only for exact channel constraints
   for (int i = 0; i < dataSize / sizeof(AUChannelInfo); i++)
   {
      AUChannelInfo *ci = &info[i];

      int ic = ci->inChannels;
      int oc = ci->outChannels;

      if (ic < 0 && oc >= 0)
      {
         ic = 2;
      }
      else if (ic >= 0 && oc < 0)
      {
         oc = 2;
      }
      else if (ic < 0 && oc < 0)
      {
         ic = 2;
         oc = 2;
      }

      if (ic == 2 && oc == 2)
      {
         haves2s = true;
      }
      else if (ic == 1 && oc == 1)
      {
         havem2m = true;
      }   
      else if (ic == 1 && oc == 2)
      {
         havem2s = true;
      }
      else if (ic == 2 && oc == 1)
      {
         haves2m = true;
      }
      else if (ic == 0 && oc == 2)
      {
         haven2s = true;
      }
      else if (ic == 0 && oc == 1)
      {
         haven2m = true;
      }
      else if (ic == 1 && oc == 0)
      {
         havem2n = true;
      }
      else if (ic == 2 && oc == 0)
      {
         haves2n = true;
      }
   }

   if (haves2s)
   {
      mAudioIns = 2;
      mAudioOuts = 2;
   }
   else if (havem2m)
   {
      mAudioIns = 1;
      mAudioOuts = 1;
   }
   else if (havem2s)
   {
      mAudioIns = 1;
      mAudioOuts = 2;
   }
   else if (haves2m)
   {
      mAudioIns = 2;
      mAudioOuts = 1;
   }
   else if (haven2m)
   {
      mAudioIns = 0;
      mAudioOuts = 1;
   }
   else if (haven2s)
   {
      mAudioIns = 0;
      mAudioOuts = 2;
   }
   else if (haves2n)
   {
      mAudioIns = 2;
      mAudioOuts = 0;
   }
   else if (havem2n)
   {
      mAudioIns = 1;
      mAudioOuts = 0;
   }

   return;
}

#endif
