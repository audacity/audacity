/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file AudioUnitEffect.cpp

  Dominic Mazzoni
  Leland Lucius

*******************************************************************//**

\class AudioUnitEffect
\brief An Effect class that handles a wide range of effects.  ??Mac only??

*//*******************************************************************/



#if USE_AUDIO_UNITS
#include "AudioUnitEffect.h"
#include "AudacityException.h"
#include "AUControl.h"
#include "ModuleManager.h"
#include "SampleCount.h"
#include "ConfigInterface.h"
#include "CFResources.h"

#include <wx/defs.h>
#include <wx/base64.h>
#include <wx/button.h>
#include <wx/control.h>
#include <wx/crt.h>
#include <wx/dir.h>
#include <wx/ffile.h>

#ifdef __WXMAC__
#include <wx/evtloop.h>
#endif

#include <wx/filename.h>
#include <wx/frame.h>
#include <wx/listctrl.h>
#include <wx/log.h>
#include <wx/sizer.h>
#include <wx/settings.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/tokenzr.h>

#include "../../SelectFile.h"
#include "../../ShuttleGui.h"
#include "../../widgets/AudacityMessageBox.h"
#include "../../widgets/valnum.h"
#include "../../widgets/wxPanelWrapper.h"

//
// When a plug-ins state is saved to the settings file (as a preset),
// it can be one of two formats, binary or XML.  In either case, it
// gets base64 encoded before storing.
//
// The advantages of XML format is less chance of failures occurring
// when exporting.  But, it can take a bit more space per preset int
// the Audacity settings file.
//
// Using binary for now.  Use kCFPropertyListXMLFormat_v1_0 if XML
// format is desired.
//
#define PRESET_FORMAT kCFPropertyListBinaryFormat_v1_0

// Name of the settings key to use for the above value
#define PRESET_KEY wxT("Data")

// Where the presets are located
#define PRESET_LOCAL_PATH wxT("/Library/Audio/Presets")
#define PRESET_USER_PATH wxT("~/Library/Audio/Presets")

static const struct
{
   OSType componentManufacturer;
   OSType componentType;
   OSType componentSubType;
}
BlackList[] =
{
   { 'appl', 'augn', 'afpl' },   // Apple: AUAudioFilePlayer
   { 'appl', 'augn', 'sspl' },   // Apple: AUScheduledSoundPlayer
   { 'appl', 'augn', 'ttsp' },   // Apple: AUSpeechSynthesis
   { 'appl', 'augn', 'nrcv' },   // Apple: AUNetReceive
   { 'appl', 'aumx', '3dmx' },   // Apple: AUMixer3D
   { 'appl', 'aumx', 'mspl' },   // Apple: AUMultiSplitter
   { 'appl', 'aumx', 'mxcm' },   // Apple: AUMultiChannelMixer
   { 'appl', 'aumx', 'mxmx' },   // Apple: AUMatrixMixer
   { 'appl', 'aumx', 'smxr' },   // Apple: AUMixer
};

// Uncomment to include parameter IDs in the final name.  Only needed if it's
// discovered that many effects have duplicate names.  It could even be done
// at runtime by scanning an effects parameters to determine if dups are present
// and, if so, enable the clump and parameter IDs.
#define USE_EXTENDED_NAMES
class ParameterInfo final
{
public:
   ParameterInfo()
   {
      info = {};
   }

   bool Get(AudioUnit mUnit, AudioUnitParameterID parmID)
   {
      OSStatus result;
      UInt32 dataSize;

      info = {};
      dataSize = sizeof(info);
      result = AudioUnitGetProperty(mUnit,
                                    kAudioUnitProperty_ParameterInfo,
                                    kAudioUnitScope_Global,
                                    parmID,
                                    &info,
                                    &dataSize);  
      if (result != noErr)
      {
         return false;
      }

      if (info.flags & kAudioUnitParameterFlag_HasCFNameString)
      {
         name = wxCFStringRef::AsString(info.cfNameString);
      }
      else
      {
         name = wxString(info.name);
      }

#if defined(USE_EXTENDED_NAMES)
      // If the parameter has a non-empty name, then the final parameter name will
      // be either:
      //
      //    <parmID,ParameterName>
      //
      // or (if the name isn't available):
      //
      //    <parmID>
      if (!name.empty())
      {
         name.Replace(idBeg, wxT('_'));
         name.Replace(idSep, wxT('_'));
         name.Replace(idEnd, wxT('_'));
         name.Append(idSep);
      }
      name = wxString::Format(wxT("%c%s%x%c"),
                              idBeg,
                              name,
                              parmID,
                              idEnd);

      // If the parameter has a clumpID, then the final parameter name will be
      // either:
      //
      //    <clumpID,clumpName><parmID,ParameterName>
      //
      // or (if the clumpName isn't available):
      //
      //    <clumpID><parmID,ParameterName>
      if (info.flags & kAudioUnitParameterFlag_HasClump)
      {
         wxString clumpName;
         AudioUnitUtils::ParameterNameInfo clumpInfo{
            info.clumpID, kAudioUnitParameterName_Full
         };
         dataSize = sizeof(clumpInfo);

         result = AudioUnitGetProperty(mUnit,
                                       kAudioUnitProperty_ParameterClumpName,
                                       kAudioUnitScope_Global,
                                       0,
                                       &clumpInfo,
                                       &dataSize);  
         if (result == noErr)
         {
            clumpName =  wxCFStringRef::AsString(clumpInfo.outName);
            clumpName.Replace(idBeg, wxT('_'));
            clumpName.Replace(idSep, wxT('_'));
            clumpName.Replace(idEnd, wxT('_'));
            clumpName.Append(idSep);
         }
         name = wxString::Format(wxT("%c%s%x%c%s"),
                                 idBeg,
                                 clumpName,
                                 info.clumpID,
                                 idEnd,
                                 name);
      }
#endif

      return true;
   }

   static const char idBeg = wxT('<');
   static const char idSep = wxT(',');
   static const char idEnd = wxT('>');

   wxString name;
   AudioUnitParameterInfo info;
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
DECLARE_PROVIDER_ENTRY(AudacityModule)
{
   // Create and register the importer
   // Trust the module manager not to leak this
   return safenew AudioUnitEffectsModule();
}

// ============================================================================
// Register this as a builtin module
// ============================================================================
DECLARE_BUILTIN_PROVIDER(AudioUnitEffectsBuiltin);

///////////////////////////////////////////////////////////////////////////////
//
// AudioUnitEffectsModule
//
///////////////////////////////////////////////////////////////////////////////

AudioUnitEffectsModule::AudioUnitEffectsModule()
{
}

AudioUnitEffectsModule::~AudioUnitEffectsModule()
{
}

// ============================================================================
// ComponentInterface implementation
// ============================================================================

PluginPath AudioUnitEffectsModule::GetPath() const
{
   return {};
}

ComponentInterfaceSymbol AudioUnitEffectsModule::GetSymbol() const
{
   /* i18n-hint: Audio Unit is the name of an Apple audio software protocol */
   return XO("Audio Unit Effects");
}

VendorSymbol AudioUnitEffectsModule::GetVendor() const
{
   return XO("The Audacity Team");
}

wxString AudioUnitEffectsModule::GetVersion() const
{
   // This "may" be different if this were to be maintained as a separate DLL
   return AUDIOUNITEFFECTS_VERSION;
}

TranslatableString AudioUnitEffectsModule::GetDescription() const
{
   return XO("Provides Audio Unit Effects support to Audacity");
}

// ============================================================================
// PluginProvider implementation
// ============================================================================

const FileExtensions &AudioUnitEffectsModule::GetFileExtensions()
{
   static FileExtensions result{{ _T("au") }};
   return result;
}

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

EffectFamilySymbol AudioUnitEffectsModule::GetOptionalFamilySymbol()
{
#if USE_AUDIO_UNITS
   return AUDIOUNITEFFECTS_FAMILY;
#else
   return {};
#endif
}

void AudioUnitEffectsModule::AutoRegisterPlugins(PluginManagerInterface &)
{
}

PluginPaths AudioUnitEffectsModule::FindModulePaths(PluginManagerInterface &)
{
   PluginPaths effects;

   LoadAudioUnitsOfType(kAudioUnitType_Effect, effects);
   LoadAudioUnitsOfType(kAudioUnitType_Generator, effects);
   LoadAudioUnitsOfType(kAudioUnitType_Mixer, effects);
   LoadAudioUnitsOfType(kAudioUnitType_MusicEffect, effects);
   LoadAudioUnitsOfType(kAudioUnitType_Panner, effects);

   return effects;
}

unsigned AudioUnitEffectsModule::DiscoverPluginsAtPath(
   const PluginPath & path, TranslatableString &errMsg,
   const RegistrationCallback &callback)
{
   errMsg = {};
   wxString name;
   AudioComponent component = FindAudioUnit(path, name);
   if (component == NULL)
   {
      errMsg = XO("Could not find component");
      return 0;
   }

   AudioUnitEffect effect(path, name, component);
   if (!effect.InitializePlugin())
   {
      // TODO:  Is it worth it to discriminate all the ways SetHost might
      // return false?
      errMsg = XO("Could not initialize component");
      return 0;
   }

   if (callback)
   {
      callback(this, &effect);
   }

   return 1;
}

bool AudioUnitEffectsModule::IsPluginValid(const PluginPath & path, bool bFast)
{
   if (bFast)
   {
      return true;
   }

   wxString name;
   return FindAudioUnit(path, name) != NULL;
}

std::unique_ptr<ComponentInterface>
AudioUnitEffectsModule::LoadPlugin(const PluginPath & path)
{
   // Acquires a resource for the application.
   if (wxString name; auto component = FindAudioUnit(path, name)) {
      auto result = std::make_unique<AudioUnitEffect>(path, name, component);
      result->InitializePlugin();
      return result;
   }
   return nullptr;
}

// ============================================================================
// AudioUnitEffectsModule implementation
// ============================================================================

void AudioUnitEffectsModule::LoadAudioUnitsOfType(OSType inAUType,
                                                  PluginPaths & effects)
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
         CFStringRef cfName{};
         result = AudioComponentCopyName(component, &cfName);
         CF_ptr<CFStringRef> uName{ cfName };
         if (result == noErr) {
            wxString path;

            path.Printf(wxT("%-4.4s/%-4.4s/%-4.4s/%s"),
                        FromOSType(found.componentManufacturer),
                        FromOSType(found.componentType),
                        FromOSType(found.componentSubType),
                        wxCFStringRef::AsString(cfName));

            for (int i = 0; i < WXSIZEOF(BlackList); ++i) {
               if (BlackList[i].componentType == found.componentType &&
                   BlackList[i].componentSubType == found.componentSubType &&
                   BlackList[i].componentManufacturer ==
                      found.componentManufacturer) {
                  wxLogDebug(wxT("Blacklisted AU skipped: %s"), path);
                  result = !noErr;
                  break;
               }
            }
            if (result == noErr)
               effects.push_back(path);
         }
      }
      component = AudioComponentFindNext(component, &desc);
   }
}

AudioComponent AudioUnitEffectsModule::FindAudioUnit(const PluginPath & path,
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
   
   return wxString::FromUTF8(reinterpret_cast<char *>(&rev), 4);
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
   AudioUnitEffectOptionsDialog(
      wxWindow * parent, bool &useLatencey, wxString &uiType);
   virtual ~AudioUnitEffectOptionsDialog();
   void PopulateOrExchange(ShuttleGui & S);
   void OnOk(wxCommandEvent & evt);
private:
   bool &mUseLatency;
   wxString &mUIType;
   TranslatableString mUITypeString;
   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(AudioUnitEffectOptionsDialog, wxDialogWrapper)
   EVT_BUTTON(wxID_OK, AudioUnitEffectOptionsDialog::OnOk)
END_EVENT_TABLE()

AudioUnitEffectOptionsDialog::AudioUnitEffectOptionsDialog(
   wxWindow * parent, bool &useLatency, wxString &uiType)
: wxDialogWrapper(parent, wxID_ANY, XO("Audio Unit Effect Options"))
, mUseLatency{ useLatency }
, mUIType{ uiType }
// Get the localization of the string for display to the user
, mUITypeString{ mUIType, {} }
{
   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);
}

AudioUnitEffectOptionsDialog::~AudioUnitEffectOptionsDialog()
{
}

static const auto FullValue = XO("Full");
static const auto GenericValue = XO("Generic");
static const auto BasicValue = XO("Basic");

void AudioUnitEffectOptionsDialog::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(5);
   S.StartHorizontalLay(wxEXPAND, 1);
   {
      S.StartVerticalLay(false);
      {
         S.StartStatic(XO("Latency Compensation"));
         {
            S.AddVariableText(XO(
"As part of their processing, some Audio Unit effects must delay returning "
"audio to Audacity. When not compensating for this delay, you will "
"notice that small silences have been inserted into the audio. "
"Enabling this option will provide that compensation, but it may "
"not work for all Audio Unit effects."),
               false, 0, 650);

            S.StartHorizontalLay(wxALIGN_LEFT);
            {
               S.TieCheckBox(XXO("Enable &compensation"),
                             mUseLatency);
            }
            S.EndHorizontalLay();
         }
         S.EndStatic();

         S.StartStatic(XO("User Interface"));
         {
            S.AddVariableText(XO(
"Select \"Full\" to use the graphical interface if supplied by the Audio Unit."
" Select \"Generic\" to use the system supplied generic interface."
#if defined(HAVE_AUDIOUNIT_BASIC_SUPPORT)
" Select \"Basic\" for a basic text-only interface."
#endif
" Reopen the effect for this to take effect."),
               false, 0, 650);

            S.StartHorizontalLay(wxALIGN_LEFT);
            {
               S.TieChoice(XXO("Select &interface"),
                  mUITypeString,
                  {
                     FullValue,
                     GenericValue,
#if defined(HAVE_AUDIOUNIT_BASIC_SUPPORT)
                     BasicValue,
#endif
                  });
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
      return;

   // This re-visits the controls, not to create them but to transfer values out
   ShuttleGui S(this, eIsGettingFromDialog);
   PopulateOrExchange(S);

   // un-translate the type
   mUIType = mUITypeString.MSGID().GET();
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
   bool HasPresets();
   TranslatableString Import(const wxString & path, const wxString & name);

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
:  wxDialogWrapper(parent, wxID_ANY, XO("Import Audio Unit Presets"))
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
         S.StartStatic(XO("Presets (may select multiple)"));
         {
            mList = S.Style(wxLC_REPORT | wxLC_HRULES | wxLC_VRULES |
                       wxLC_NO_SORT_HEADER)
               .AddListControlReportMode({ XO("Preset"), XO("Location") });
         }
         S.EndStatic();
      }
      S.EndVerticalLay();
   }
   S.EndHorizontalLay();

   S.AddStandardButtons();

   FilePaths presets;
   wxFileName fn;

   // Generate the local domain path
   wxString path;
   path.Printf(wxT("%s/%s/%s"),
               PRESET_LOCAL_PATH,
               mEffect->mVendor,
               mEffect->mName);
   fn = path;
   fn.Normalize();
   
   // Get all presets in the local domain for this effect
   wxDir::GetAllFiles(fn.GetFullPath(), &presets, wxT("*.aupreset"));

   // Generate the user domain path
   path.Printf(wxT("%s/%s/%s"),
               PRESET_USER_PATH,
               mEffect->mVendor,
               mEffect->mName);
   fn = path;
   fn.Normalize();

   // Get all presets in the user domain for this effect
   wxDir::GetAllFiles(fn.GetFullPath(), &presets, wxT("*.aupreset"));
   
   presets.Sort();

   for (size_t i = 0, cnt = presets.size(); i < cnt; i++)
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

bool AudioUnitEffectImportDialog::HasPresets()
{
   return mList->GetItemCount() > 0;
}

TranslatableString AudioUnitEffectImportDialog::Import(
   const wxString & path, const wxString & name)
{
   // Generate the path
   wxString fullPath;
   fullPath.Printf(wxT("%s/%s.aupreset"),
                    path,
                    name);

   // Open the preset
   wxFFile f(fullPath, wxT("r"));
   if (!f.IsOpened())
   {
      return XO("Couldn't open \"%s\"").Format(fullPath);
   }

   // Load it into the buffer
   size_t len = f.Length();
   wxMemoryBuffer buf(len);
   if (f.Read(buf.GetData(), len) != len || f.Error())
   {
      return XO("Unable to read the preset from \"%s\"").Format(fullPath);
   }

   wxString parms = wxBase64Encode(buf.GetData(), len);
   if (parms.IsEmpty())
   {
      return XO("Failed to encode preset from \"%s\"").Format(fullPath);
   }

   // And write it to the config
   wxString group = UserPresetsGroup(name);
   if (!SetConfig(*mEffect,
      PluginSettings::Private, group, PRESET_KEY,
      parms))
   {
      return XO("Unable to store preset in config file");
   }

   return {};
}

void AudioUnitEffectImportDialog::OnOk(wxCommandEvent & evt)
{
   evt.Skip();
   
   // Import all selected presets
   long sel = -1;
   while ((sel = mList->GetNextItem(sel, wxLIST_NEXT_ALL, wxLIST_STATE_SELECTED)) >= 0)
   {
      wxListItem item;
      item.SetId(sel);
      item.SetColumn(1);
      item.SetMask(wxLIST_MASK_TEXT);
      mList->GetItem(item);

      wxString path = item.GetText();
      wxString name = mList->GetItemText(sel);
      auto msg = Import(path, name);

      if (!msg.empty())
      {
         AudacityMessageBox(
            XO("Could not import \"%s\" preset\n\n%s").Format(name, msg),
            XO("Import Audio Unit Presets"),
            wxOK | wxCENTRE,
            this);
         return;
      }
   }
  
   EndModal(wxID_OK);
}

///////////////////////////////////////////////////////////////////////////////
//
// AudioUnitEffect
//
///////////////////////////////////////////////////////////////////////////////

AudioUnitEffect::AudioUnitEffect(const PluginPath & path,
                                 const wxString & name,
                                 AudioComponent component,
                                 AudioUnitEffect *master)
   : AudioUnitWrapper{ component }
   , mPath{ path }
   , mName{ name.AfterFirst(wxT(':')).Trim(true).Trim(false) }
   , mVendor{ name.BeforeFirst(wxT(':')).Trim(true).Trim(false) }
   , mMaster{ master }
{
}

AudioUnitEffect::~AudioUnitEffect()
{
}

// ============================================================================
// ComponentInterface implementation
// ============================================================================

PluginPath AudioUnitEffect::GetPath() const
{
   return mPath;
}

ComponentInterfaceSymbol AudioUnitEffect::GetSymbol() const
{
   return mName;
}

VendorSymbol AudioUnitEffect::GetVendor() const
{
   return { mVendor };
}

wxString AudioUnitEffect::GetVersion() const
{
   UInt32 version;

   OSStatus result = AudioComponentGetVersion(mComponent, &version);

   return wxString::Format(wxT("%d.%d.%d"),
                           (version >> 16) & 0xffff,
                           (version >> 8) & 0xff,
                           version & 0xff);
}

TranslatableString AudioUnitEffect::GetDescription() const
{
   /* i18n-hint: Can mean "not available," "not applicable," "no answer" */
   return XO("n/a");
}

// ============================================================================
// EffectDefinitionInterface implementation
// ============================================================================

EffectType AudioUnitEffect::GetType() const
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

EffectFamilySymbol AudioUnitEffect::GetFamily() const
{
   return AUDIOUNITEFFECTS_FAMILY;
}

bool AudioUnitEffect::IsInteractive() const
{
   return mInteractive;
}

bool AudioUnitEffect::IsDefault() const
{
   return false;
}

bool AudioUnitEffect::SupportsRealtime() const
{
   return GetType() == EffectTypeProcess;
}

bool AudioUnitEffect::SupportsAutomation() const
{
   OSStatus result;
   UInt32 dataSize;
   Boolean isWritable;

   result = AudioUnitGetPropertyInfo(mUnit.get(),
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
   ArrayOf<AudioUnitParameterID> array{cnt};

   result = AudioUnitGetProperty(mUnit.get(),
                                 kAudioUnitProperty_ParameterList,
                                 kAudioUnitScope_Global,
                                 0,
                                 array.get(),
                                 &dataSize);  
   if (result != noErr)
   {
      return false;
   }

   for (int i = 0; i < cnt; i++)
   {
      ParameterInfo pi;

      if (pi.Get(mUnit.get(), array[i]))
      {
         if (pi.info.flags & kAudioUnitParameterFlag_IsWritable)
         {
            // All we need is one
            return true;
         }
      }
   }

   return false;
}

bool AudioUnitWrapper::CreateAudioUnit()
{
   AudioUnit unit{};
   auto result = AudioComponentInstanceNew(mComponent, &unit);
   if (!result)
      mUnit.reset(unit);
   return (!result && unit != nullptr);
}

bool AudioUnitEffect::InitializeInstance()
{
   if (!CreateAudioUnit())
      return false;

   mSampleRate = 44100;
   GetChannelCounts();
   SetRateAndChannels();

   // Retrieve the desired number of frames per slice
   UInt32 dataSize = sizeof(mBlockSize);
   mBlockSize = 512;
   AudioUnitGetProperty(mUnit.get(),
                        kAudioUnitProperty_MaximumFramesPerSlice,
                        kAudioUnitScope_Global,
                        0,
                        &mBlockSize,
                        &dataSize);

   // Is this really needed here or can it be done in MakeInstance()
   // only?  I think it can, but this is more a conservative change for now,
   // preserving what SetHost() did
   return MakeListener();
}

std::shared_ptr<EffectInstance>
AudioUnitEffect::MakeInstance(EffectSettings &settings) const
{
   return const_cast<AudioUnitEffect*>(this)->DoMakeInstance(settings);
}

std::shared_ptr<EffectInstance>
AudioUnitEffect::DoMakeInstance(EffectSettings &settings)
{
   if (mMaster)
      // This is a slave
      InitializeInstance();
   else
      // Don't HAVE a master -- this IS the master.
      LoadPreset(CurrentSettingsGroup(), settings);
   return std::make_shared<Instance>(*this);
}

constexpr auto OptionsKey = L"Options";
constexpr auto UseLatencyKey = L"UseLatency";
constexpr auto UITypeKey = L"UIType";

bool AudioUnitEffect::InitializePlugin()
{
   // To implement the services of EffectPlugin -- such as, a query of the
   // set of effect parameters, so that we can implement MakeSettings -- we
   // also need what is called an AudioComponentInstance, also called an
   // AudioUnit.
   // It's not just for implementing EffectInstance.  AudioUnits is unlike other
   // third party effect families that distinguish the notions of plug-in and
   // instance.

   // When AudioUnitEffect implements its own proper Instance class, this
   // should call CreateAudioUnit() directly and not do the rest of
   // InitializeInstance.
   if (!InitializeInstance())
      return false;


   // Consult preferences
   // Decide mUseLatency, which affects GetLatency(), which is actually used
   // so far only in destructive effect processing
   GetConfig(*this, PluginSettings::Shared, OptionsKey, UseLatencyKey,
      mUseLatency, true);
   // Decide whether to build plain or fancy user interfaces
   GetConfig(*this, PluginSettings::Shared, OptionsKey, UITypeKey,
      mUIType, FullValue.MSGID().GET() /* Config stores un-localized string */);

   // Once, persistently, for each AudioUnitEffect, the first time it is loaded:
   // Query the instance for parameters and their settings, and save that in
   // the configuration file as "factory default settings"
   bool haveDefaults;
   constexpr auto InitializedKey = L"Initialized";
   GetConfig(*this, PluginSettings::Private,
      FactoryDefaultsGroup(), InitializedKey, haveDefaults, false);
   if (!haveDefaults) {
      SavePreset(FactoryDefaultsGroup());
      SetConfig(*this, PluginSettings::Private,
         FactoryDefaultsGroup(), InitializedKey, true);
   }
   return true;
}

bool AudioUnitEffect::MakeListener()
{
   if (!mMaster)
   {
      // Don't have a master -- so this IS the master.
      OSStatus result;
      AUEventListenerRef eventListenerRef{};
      result = AUEventListenerCreate(AudioUnitEffect::EventListenerCallback,
                                    this,
                                    (CFRunLoopRef)GetCFRunLoopFromEventLoop(GetCurrentEventLoop()),
                                    kCFRunLoopDefaultMode,
                                    0.0,
                                    0.0,
                                    &eventListenerRef);
      if (result != noErr)
         return false;
      mEventListenerRef.reset(eventListenerRef);

      AudioUnitEvent event;
 
      event.mEventType = kAudioUnitEvent_ParameterValueChange;
      event.mArgument.mParameter.mAudioUnit = mUnit.get();
      event.mArgument.mParameter.mScope = kAudioUnitScope_Global;
      event.mArgument.mParameter.mElement = 0;

      UInt32 dataSize;
      Boolean isWritable;

      // Retrieve the list of parameters
      result = AudioUnitGetPropertyInfo(mUnit.get(),
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
      if (cnt != 0)
      {
         ArrayOf<AudioUnitParameterID> array {cnt};

         result = AudioUnitGetProperty(mUnit.get(),
                                       kAudioUnitProperty_ParameterList,
                                       kAudioUnitScope_Global,
                                       0,
                                       array.get(),
                                       &dataSize);  
         if (result != noErr)
         {
            return false;
         }

         // Register them as something we're interested in
         for (int i = 0; i < cnt; i++)
         {
            event.mArgument.mParameter.mParameterID = array[i];
            result = AUEventListenerAddEventType(mEventListenerRef.get(),
                                                 this,
                                                 &event);
            if (result != noErr)
            {
               return false;
            }
         }
      }

      event.mEventType = kAudioUnitEvent_PropertyChange;
      event.mArgument.mProperty.mAudioUnit = mUnit.get();
      event.mArgument.mProperty.mPropertyID = kAudioUnitProperty_Latency;
      event.mArgument.mProperty.mScope = kAudioUnitScope_Global;
      event.mArgument.mProperty.mElement = 0;

      result = AUEventListenerAddEventType(mEventListenerRef.get(),
                                           this,
                                           &event);
      if (result != noErr)
      {
         return false;
      }

      AudioUnitCocoaViewInfo cocoaViewInfo;
      dataSize = sizeof(AudioUnitCocoaViewInfo);
   
      // Check for a Cocoa UI
      result = AudioUnitGetProperty(mUnit.get(),
                                    kAudioUnitProperty_CocoaUI,
                                    kAudioUnitScope_Global,
                                    0,
                                    &cocoaViewInfo,
                                    &dataSize);

      bool hasCocoa = result == noErr;

      // Check for a Carbon UI
      AudioComponentDescription compDesc;
      dataSize = sizeof(compDesc);
      result = AudioUnitGetProperty(mUnit.get(),
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

unsigned AudioUnitEffect::GetAudioInCount() const
{
   return mAudioIns;
}

unsigned AudioUnitEffect::GetAudioOutCount() const
{
   return mAudioOuts;
}

int AudioUnitEffect::GetMidiInCount() const
{
   return 0;
}

int AudioUnitEffect::GetMidiOutCount() const
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

size_t AudioUnitEffect::GetBlockSize() const
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
      AudioUnitGetProperty(mUnit.get(),
                           kAudioUnitProperty_Latency,
                           kAudioUnitScope_Global,
                           0,
                           &latency,
                           &dataSize);  

      return sampleCount(latency * mSampleRate);
   }

   return 0;
}

#if 0
// TODO move to AudioUnitEffect::Instance when that class exists
size_t AudioUnitEffect::GetTailSize() const
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
#endif

bool AudioUnitEffect::ProcessInitialize(
   EffectSettings &, sampleCount, ChannelNames chanMap)
{
   mInputList.reinit(mAudioIns);
   mInputList[0].mNumberBuffers = mAudioIns;
   
   mOutputList.reinit(mAudioOuts);
   mOutputList[0].mNumberBuffers = mAudioOuts;

   memset(&mTimeStamp, 0, sizeof(AudioTimeStamp));
   mTimeStamp.mSampleTime = 0; // This is a double-precision number that should
                               // accumulate the number of frames processed so far
   mTimeStamp.mFlags = kAudioTimeStampSampleTimeValid;

   if (!SetRateAndChannels())
      return false;

   if (SetProperty(kAudioUnitProperty_SetRenderCallback,
      AudioUnitUtils::RenderCallback{ RenderCallback, this },
      kAudioUnitScope_Input)) {
      wxLogError("Setting input render callback failed.\n");
      return false;
   }

   if (AudioUnitReset(mUnit.get(), kAudioUnitScope_Global, 0))
      return false;

   if (!BypassEffect(false))
      return false;

   mLatencyDone = false;
   return true;
}

bool AudioUnitEffect::ProcessFinalize()
{
   mOutputList.reset();
   mInputList.reset();

   return true;
}

size_t AudioUnitEffect::ProcessBlock(EffectSettings &,
   const float *const *inBlock, float *const *outBlock, size_t blockLen)
{
   for (size_t i = 0; i < mAudioIns; i++)
   {
      mInputList[0].mBuffers[i].mNumberChannels = 1;
      mInputList[0].mBuffers[i].mData = const_cast<float*>(inBlock[i]);
      mInputList[0].mBuffers[i].mDataByteSize = sizeof(float) * blockLen;
   }

   for (size_t i = 0; i < mAudioOuts; i++)
   {
      mOutputList[0].mBuffers[i].mNumberChannels = 1;
      mOutputList[0].mBuffers[i].mData = outBlock[i];
      mOutputList[0].mBuffers[i].mDataByteSize = sizeof(float) * blockLen;
   }

   AudioUnitRenderActionFlags flags = 0;
   OSStatus result;

   result = AudioUnitRender(mUnit.get(),
                            &flags,
                            &mTimeStamp,
                            0,
                            blockLen,
                            mOutputList.get());
   if (result != noErr) {
      wxLogError("Render failed: %d %4.4s\n",
         static_cast<int>(result), reinterpret_cast<char *>(&result));
      return 0;
   }

   mTimeStamp.mSampleTime += blockLen;
   return blockLen;
}

bool AudioUnitEffect::RealtimeInitialize(EffectSettings &settings)
{
   return ProcessInitialize(settings, 0, nullptr);
}

bool AudioUnitEffect::RealtimeAddProcessor(
   EffectSettings &settings, unsigned, float sampleRate)
{
   auto slave = std::make_unique<AudioUnitEffect>(mPath, mName, mComponent, this);
   if (!slave->InitializeInstance())
      return false;

   slave->SetBlockSize(mBlockSize);
   slave->SetSampleRate(sampleRate);

   if (!CopyParameters(mUnit.get(), slave->mUnit.get()))
   {
      return false;
   }

   auto pSlave = slave.get();
   mSlaves.push_back(std::move(slave));

   return pSlave->ProcessInitialize(settings, 0, nullptr);
}

bool AudioUnitEffect::RealtimeFinalize(EffectSettings &) noexcept
{
return GuardedCall<bool>([&]{
   for (size_t i = 0, cnt = mSlaves.size(); i < cnt; i++)
   {
      mSlaves[i]->ProcessFinalize();
   }
   mSlaves.clear();
   return ProcessFinalize();
});
}

bool AudioUnitEffect::RealtimeSuspend()
{
   if (!BypassEffect(true))
   {
      return false;
   }

   for (size_t i = 0, cnt = mSlaves.size(); i < cnt; i++)
   {
      if (!mSlaves[i]->BypassEffect(true))
      {
         return false;
      }
   }

   return true;
}

bool AudioUnitEffect::RealtimeResume() noexcept
{
return GuardedCall<bool>([&]{
   if (!BypassEffect(false))
   {
      return false;
   }

   for (size_t i = 0, cnt = mSlaves.size(); i < cnt; i++)
   {
      if (!mSlaves[i]->BypassEffect(false))
      {
         return false;
      }
   }

   return true;
});
}

bool AudioUnitEffect::RealtimeProcessStart(EffectSettings &)
{
   return true;
}

size_t AudioUnitEffect::RealtimeProcess(int group, EffectSettings &settings,
   const float *const *inbuf, float *const *outbuf, size_t numSamples)
{
   wxASSERT(numSamples <= mBlockSize);
   return mSlaves[group]->ProcessBlock(settings, inbuf, outbuf, numSamples);
}

bool AudioUnitEffect::RealtimeProcessEnd(EffectSettings &) noexcept
{
   return true;
}

int AudioUnitEffect::ShowClientInterface(
   wxWindow &parent, wxDialog &dialog, bool forceModal)
{
   // Remember the dialog with a weak pointer, but don't control its lifetime
   mDialog = &dialog;
   if ((SupportsRealtime() || GetType() == EffectTypeAnalyze) && !forceModal)
   {
      mDialog->Show();
      return 0;
   }

   return mDialog->ShowModal();
}

bool AudioUnitEffect::SaveSettings(
   const EffectSettings &, CommandParameters & parms) const
{
   OSStatus result;
   UInt32 dataSize;
   Boolean isWritable;

   result = AudioUnitGetPropertyInfo(mUnit.get(),
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
   ArrayOf<AudioUnitParameterID> array {cnt};

   result = AudioUnitGetProperty(mUnit.get(),
                                 kAudioUnitProperty_ParameterList,
                                 kAudioUnitScope_Global,
                                 0,
                                 array.get(),
                                 &dataSize);  
   if (result != noErr)
   {
      return false;
   }

   for (int i = 0; i < cnt; i++)
   {
      ParameterInfo pi;

      if (!pi.Get(mUnit.get(), array[i]))
      {
         // Probably failed because of invalid parameter which can happen
         // if a plug-in is in a certain mode that doesn't contain the
         // parameter.  In any case, just ignore it.
         continue;
      }

      AudioUnitParameterValue value;
      result = AudioUnitGetParameter(mUnit.get(), array[i],
         kAudioUnitScope_Global, 0, &value);
      if (result != noErr)
      {
         // Probably failed because of invalid parameter which can happen
         // if a plug-in is in a certain mode that doesn't contain the
         // parameter.  In any case, just ignore it.
         continue;
      }

      parms.Write(pi.name, value);
   }

   return true;
}

bool AudioUnitEffect::LoadSettings(
   const CommandParameters & parms, EffectSettings &settings) const
{
   OSStatus result;
   UInt32 dataSize;
   Boolean isWritable;

   result = AudioUnitGetPropertyInfo(mUnit.get(),
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
   ArrayOf<AudioUnitParameterID> array {cnt};

   result = AudioUnitGetProperty(mUnit.get(),
                                 kAudioUnitProperty_ParameterList,
                                 kAudioUnitScope_Global,
                                 0,
                                 array.get(),
                                 &dataSize);  
   if (result != noErr)
   {
      return false;
   }

   for (int i = 0; i < cnt; i++)
   {
      ParameterInfo pi;

      if (!pi.Get(mUnit.get(), array[i]))
      {
         // Probably failed because of invalid parameter which can happen
         // if a plug-in is in a certain mode that doesn't contain the
         // parameter.  In any case, just ignore it.
         continue;
      }

      double d = 0.0;
      if (parms.Read(pi.name, &d))
      {
         AudioUnitParameterValue value = d;
         AudioUnitSetParameter(mUnit.get(), array[i], kAudioUnitScope_Global,
            0, value, 0);
         Notify(mUnit.get(), array[i]);
      }
   }

   return true;
}

bool AudioUnitEffect::LoadUserPreset(
   const RegistryPath & name, EffectSettings &settings) const
{
   // To do: externalize state so const_cast isn't needed
   return const_cast<AudioUnitEffect*>(this)->LoadPreset(name, settings);
}

bool AudioUnitEffect::SaveUserPreset(
   const RegistryPath & name, const EffectSettings &) const
{
   return SavePreset(name);
}

bool AudioUnitEffect::LoadFactoryPreset(int id, EffectSettings &) const
{
   OSStatus result;

   // Retrieve the list of factory presets
   CF_ptr<CFArrayRef> array;
   UInt32 dataSize = sizeof(CFArrayRef);
   result = AudioUnitGetProperty(mUnit.get(),
                                 kAudioUnitProperty_FactoryPresets,
                                 kAudioUnitScope_Global,
                                 0,
                                 &array,
                                 &dataSize);
   if (result != noErr || id < 0 || id >= CFArrayGetCount(array.get()))
      return false;

   if (!SetProperty(kAudioUnitProperty_PresentPreset,
      *static_cast<const AUPreset*>(CFArrayGetValueAtIndex(array.get(), id)))) {
      // Notify interested parties of change and propagate to slaves
      Notify(mUnit.get(), kAUParameterListener_AnyParameter);
      return true;
   }
   return false;
}

bool AudioUnitEffect::LoadFactoryDefaults(EffectSettings &settings) const
{
   // To do: externalize state so const_cast isn't needed
   return const_cast<AudioUnitEffect*>(this)
      ->LoadPreset(FactoryDefaultsGroup(), settings);
}

RegistryPaths AudioUnitEffect::GetFactoryPresets() const
{
   OSStatus result;
   RegistryPaths presets;

   // Retrieve the list of factory presets
   CF_ptr<CFArrayRef> array;
   UInt32 dataSize = sizeof(CFArrayRef);
   result = AudioUnitGetProperty(mUnit.get(),
                                 kAudioUnitProperty_FactoryPresets,
                                 kAudioUnitScope_Global,
                                 0,
                                 &array,
                                 &dataSize);
   if (result == noErr)
      for (CFIndex i = 0, cnt = CFArrayGetCount(array.get()); i < cnt; ++i)
         presets.push_back(wxCFStringRef::AsString(
            static_cast<const AUPreset*>(CFArrayGetValueAtIndex(array.get(), i))
               ->presetName));
   return presets;
}

// ============================================================================
// EffectUIClientInterface Implementation
// ============================================================================

std::unique_ptr<EffectUIValidator>
AudioUnitEffect::PopulateUI(ShuttleGui &S, EffectSettingsAccess &access)
{
   // OSStatus result;

   auto parent = S.GetParent();
   mDialog = static_cast<wxDialog *>(wxGetTopLevelParent(parent));
   mParent = parent;
   mpControl = NULL;

   wxPanel *container;
   {
      auto mainSizer = std::make_unique<wxBoxSizer>(wxVERTICAL);

      wxASSERT(mParent); // To justify safenew
      container = safenew wxPanelWrapper(mParent, wxID_ANY);
      mainSizer->Add(container, 1, wxEXPAND);

      mParent->SetSizer(mainSizer.release());
   }

#if defined(HAVE_AUDIOUNIT_BASIC_SUPPORT)
   if (mUIType == BasicValue.MSGID().GET()) {
      if (!CreatePlain(mParent))
         return nullptr;
   }
   else
#endif
   {
      auto pControl = Destroy_ptr<AUControl>(safenew AUControl);
      if (!pControl)
      {
         return nullptr;
      }

      if (!pControl->Create(container, mComponent, mUnit.get(),
         mUIType == FullValue.MSGID().GET()))
         return nullptr;

      {
         auto innerSizer = std::make_unique<wxBoxSizer>(wxVERTICAL);

         innerSizer->Add((mpControl = pControl.release()), 1, wxEXPAND);
         container->SetSizer(innerSizer.release());
      }

      mParent->SetMinSize(wxDefaultSize);

#ifdef __WXMAC__
#ifdef __WX_EVTLOOP_BUSY_WAITING__
      wxEventLoop::SetBusyWaiting(true);
#endif
#endif
   }

   if (mpControl)
   {
      mParent->PushEventHandler(this);
   }

   return std::make_unique<DefaultEffectUIValidator>(*this, access);
}

bool AudioUnitEffect::IsGraphicalUI()
{
   return mUIType != wxT("Plain");
}

bool AudioUnitEffect::ValidateUI([[maybe_unused]] EffectSettings &settings)
{
#if 0
   if (GetType() == EffectTypeGenerate)
      settings.extra.SetDuration(mDuration->GetValue());
#endif
   return true;
}

#if defined(HAVE_AUDIOUNIT_BASIC_SUPPORT)
bool AudioUnitEffect::CreatePlain(wxWindow *parent)
{
   // TODO???  Never implemented...
   return false;
}
#endif

bool AudioUnitEffect::CloseUI()
{
#ifdef __WXMAC__
#ifdef __WX_EVTLOOP_BUSY_WAITING__
   wxEventLoop::SetBusyWaiting(false);
#endif
   if (mpControl)
   {
      mParent->RemoveEventHandler(this);

      mpControl->Close();
      mpControl = nullptr;
   }
#endif

   mParent = NULL;
   mDialog = NULL;

   return true;
}

bool AudioUnitEffect::CanExportPresets()
{
   return true;
}

void AudioUnitEffect::ExportPresets(const EffectSettings &) const
{
   // Generate the user domain path
   wxFileName fn;
   fn.SetPath(PRESET_USER_PATH);
   fn.AppendDir(mVendor);
   fn.AppendDir(mName);
   fn.Normalize();
   FilePath path = fn.GetFullPath();

   if (!fn.Mkdir(fn.GetFullPath(), 0755, wxPATH_MKDIR_FULL))
   {
      wxLogError(wxT("Couldn't create the \"%s\" directory"), fn.GetPath());
      return;
   }

   // Ask the user for the name to use
   //
   // Passing a valid parent will cause some effects dialogs to malfunction
   // upon returning from the SelectFile().
   path = SelectFile(FileNames::Operation::_None,
      XO("Export Audio Unit Preset As %s:").Format(fn.GetFullPath()),
      fn.GetFullPath(),
      wxEmptyString,
      wxT("aupreset"),
      {
        { XO("Standard Audio Unit preset file"), { wxT("aupreset") }, true },
      },
      wxFD_SAVE | wxFD_OVERWRITE_PROMPT | wxRESIZE_BORDER,
      NULL);

   // User canceled...
   if (path.empty())
   {
      return;
   }

   auto msg = Export(path);
   if (!msg.empty())
   {
      AudacityMessageBox(
         XO("Could not export \"%s\" preset\n\n%s").Format(path, msg),
         XO("Export Audio Unit Presets"),
         wxOK | wxCENTRE,
         mParent);
   }
}

void AudioUnitEffect::ImportPresets(EffectSettings &)
{
   // Generate the user domain path
   wxFileName fn;
   fn.SetPath(PRESET_USER_PATH);
   fn.AppendDir(mVendor);
   fn.AppendDir(mName);
   fn.Normalize();
   FilePath path = fn.GetFullPath();

   // Ask the user for the name to use
   //
   // Passing a valid parent will cause some effects dialogs to malfunction
   // upon returning from the SelectFile().
   path = SelectFile(FileNames::Operation::_None,
      XO("Import Audio Unit Preset As %s:").Format(fn.GetFullPath()),
      fn.GetFullPath(),
      wxEmptyString,
      wxT("aupreset"),
      {
        { XO("Standard Audio Unit preset file"), { wxT("aupreset") }, true },
      },
      wxFD_OPEN | wxRESIZE_BORDER,
      NULL);

   // User canceled...
   if (path.empty())
   {
      return;
   }

   auto msg = Import(path);
   if (!msg.empty())
   {
      AudacityMessageBox(
         XO("Could not import \"%s\" preset\n\n%s").Format(path, msg),
         XO("Import Audio Unit Presets"),
         wxOK | wxCENTRE,
         mParent);
   }
}

bool AudioUnitEffect::HasOptions()
{
   return true;
}

void AudioUnitEffect::ShowOptions()
{
   AudioUnitEffectOptionsDialog dlg(mParent, mUseLatency, mUIType);
   if (dlg.ShowModal()) {
      // Save changed values to the config file
      SetConfig(*this, PluginSettings::Shared, OptionsKey, UseLatencyKey,
         mUseLatency);
      SetConfig(*this, PluginSettings::Shared, OptionsKey, UITypeKey, mUIType);
   }
}

// ============================================================================
// AudioUnitEffect Implementation
// ============================================================================

bool AudioUnitEffect::LoadPreset(
   const RegistryPath & group, EffectSettings &settings) const
{
   wxString parms;

   // Attempt to load old preset parameters and resave using new method
   if (GetConfig(*this, PluginSettings::Private, group, wxT("Parameters"),
      parms, wxEmptyString)) {
      CommandParameters eap;
      if (eap.SetParameters(parms))
         if (LoadSettings(eap, settings))
            if (SavePreset(group))
               RemoveConfig(*this, PluginSettings::Private,
                  group, wxT("Parameters"));
      return true;
   }

   // Retrieve the preset
   if (!GetConfig(*this, PluginSettings::Private, group, PRESET_KEY, parms,
      wxEmptyString)) {
      // Commented "CurrentSettings" gets tried a lot and useless messages appear
      // in the log
      //wxLogError(wxT("Preset key \"%s\" not found in group \"%s\""), PRESET_KEY, group);
      return false;
   }
   
   // Decode it
   wxMemoryBuffer buf = wxBase64Decode(parms);
   size_t bufLen = buf.GetDataLen();
   if (!bufLen) {
      wxLogError(wxT("Failed to decode \"%s\" preset"), group);
      return false;
   }
   const uint8_t *bufPtr = static_cast<uint8_t *>(buf.GetData());

   // Create a CFData object that references the decoded preset
   CF_ptr<CFDataRef> data{
      CFDataCreateWithBytesNoCopy(kCFAllocatorDefault,
         bufPtr, bufLen, kCFAllocatorNull)
   };
   if (!data) {
      wxLogError(wxT("Failed to convert \"%s\" preset to internal format"),
         group);
      return false;
   }

   // Convert it back to a property list.
   CF_ptr<CFPropertyListRef> content{
      CFPropertyListCreateWithData(kCFAllocatorDefault,
         data.get(), kCFPropertyListImmutable, nullptr,
         // TODO might retrieve more error information
         nullptr)
   };
   if (!content) {
      wxLogError(wxT("Failed to create property list for \"%s\" preset"), group);
      return false;
   }

   // See AUView::viewWillDraw
   if (mpControl)
      mpControl->ForceRedraw();

   // Finally, update the properties and parameters
   if (SetProperty(kAudioUnitProperty_ClassInfo, content)) {
      wxLogError(wxT("Failed to set class info for \"%s\" preset"), group);
      return false;
   }

   // Notify interested parties of change and propagate to slaves
   Notify(mUnit.get(), kAUParameterListener_AnyParameter);
   return true;
}

bool AudioUnitEffect::SavePreset(const RegistryPath & group) const
{
   // First set the name of the preset
   wxCFStringRef cfname(wxFileNameFromPath(group));

   // Define the preset property and set it in the audio unit
   if (SetProperty(
      kAudioUnitProperty_PresentPreset, AudioUnitUtils::UserPreset{ cfname }))
      return false;

   // Now retrieve the preset content
   CF_ptr<CFPropertyListRef> content;
   UInt32 size = sizeof(content);
   AudioUnitGetProperty(mUnit.get(),
                        kAudioUnitProperty_ClassInfo,
                        kAudioUnitScope_Global,
                        0,
                        &content,
                        &size);

   // And convert it to serialized binary data
   CF_ptr<CFDataRef> data{
      CFPropertyListCreateData(kCFAllocatorDefault,
         content.get(), PRESET_FORMAT, 0,
         // TODO might retrieve more error information
         nullptr)
   };
   if (!data)
      return false;

   // Nothing to do if we don't have any data
   if (const auto length = CFDataGetLength(data.get())) {
      // Base64 encode the returned binary property list
      auto parms = wxBase64Encode(CFDataGetBytePtr(data.get()), length);
      // And write it to the config
      if (!SetConfig(*this,
         PluginSettings::Private, group, PRESET_KEY, parms))
         return false;
   }
   return true;
}

bool AudioUnitEffect::SetRateAndChannels()
{
   mInitialization.reset();
   AudioUnitUtils::StreamBasicDescription streamFormat{
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
      0,

      // UInt32  mBitsPerChannel;
      sizeof(float) * 8,
   };

   const struct Info{
      unsigned nChannels;
      AudioUnitScope scope;
      const char *const msg; // used only in log messages
   } infos[]{
      { 1, kAudioUnitScope_Global, "global" },
      { mAudioIns, kAudioUnitScope_Input, "input" },
      { mAudioOuts, kAudioUnitScope_Output, "output" },
   };
   for (const auto &[nChannels, scope, msg] : infos) {
      if (nChannels) {
         if (SetProperty(kAudioUnitProperty_SampleRate, mSampleRate, scope)) {
            wxLogError("%ls Didn't accept sample rate on %s\n",
               // Exposing internal name only in logging
               GetSymbol().Internal().wx_str(), msg);
            return false;
         }
         if (scope != kAudioUnitScope_Global) {
            streamFormat.mChannelsPerFrame = nChannels;
            if (SetProperty(kAudioUnitProperty_StreamFormat,
               streamFormat, scope)) {
               wxLogError("%ls didn't accept stream format on %s\n",
                  // Exposing internal name only in logging
                  GetSymbol().Internal().wx_str(), msg);
               return false;
            }
         }
      }
   }

   if (AudioUnitInitialize(mUnit.get())) {
      wxLogError("Couldn't initialize audio unit\n");
      return false;
   }

   mInitialization.reset(mUnit.get());
   return true;
}

bool AudioUnitEffect::CopyParameters(AudioUnit srcUnit, AudioUnit dstUnit)
{
   OSStatus result;

   // Retrieve the class state from the source AU
   CF_ptr<CFPropertyListRef> content;
   UInt32 size = sizeof(content);
   result = AudioUnitGetProperty(srcUnit,
                                 kAudioUnitProperty_ClassInfo,
                                 kAudioUnitScope_Global,
                                 0,
                                 &content,
                                 &size);
   if (result != noErr)
      return false;

   // Set the destination AUs state from the source AU's content
   if (AudioUnitUtils::SetProperty(dstUnit,
      kAudioUnitProperty_ClassInfo, content))
      return false;

   // Notify interested parties
   Notify(dstUnit, kAUParameterListener_AnyParameter);

   return true;
}

TranslatableString AudioUnitEffect::Export(const wxString & path) const
{
   // Create the file
   wxFFile f(path, wxT("wb"));
   if (!f.IsOpened())
      return XO("Couldn't open \"%s\"").Format(path);

   // First set the name of the preset
   wxCFStringRef cfname(wxFileName(path).GetName());

   // Define the preset property and set it in the audio unit
   if (SetProperty(
      kAudioUnitProperty_PresentPreset, AudioUnitUtils::UserPreset{ cfname }))
      return XO("Failed to set preset name");

   // Now retrieve the preset content
   CF_ptr<CFPropertyListRef> content;
   UInt32 size = sizeof(content);
   auto result = AudioUnitGetProperty(mUnit.get(),
                                 kAudioUnitProperty_ClassInfo,
                                 kAudioUnitScope_Global,
                                 0,
                                 &content,
                                 &size);
   if (result)
      return XO("Failed to retrieve preset content");

   // And convert it to serialized XML data
   CF_ptr<CFDataRef> data{
      CFPropertyListCreateData(kCFAllocatorDefault,
         content.get(), kCFPropertyListXMLFormat_v1_0, 0,
         // TODO might retrieve more error information
         nullptr)
   };
   if (!data)
      return XO("Failed to convert property list to XML data");

   // Nothing to do if we don't have any data
   SInt32 length = CFDataGetLength(data.get());
   if (!length)
      return XO("XML data is empty after conversion");

   // Write XML data
   if (f.Write(CFDataGetBytePtr(data.get()), length) != length || f.Error())
      return XO("Failed to write XML preset to \"%s\"").Format(path);

   f.Close();
   return {};
}

TranslatableString AudioUnitEffect::Import(const wxString & path)
{
   // Open the preset
   wxFFile f(path, wxT("r"));
   if (!f.IsOpened())
      return XO("Couldn't open \"%s\"").Format(path);

   // Load it into the buffer
   size_t len = f.Length();
   wxMemoryBuffer buf(len);
   if (f.Read(buf.GetData(), len) != len || f.Error())
      return XO("Unable to read the preset from \"%s\"").Format(path);

   // Create a CFData object that references the decoded preset
   CF_ptr<CFDataRef> data{
      CFDataCreateWithBytesNoCopy(kCFAllocatorDefault,
         static_cast<const UInt8 *>(buf.GetData()), len, kCFAllocatorNull)
   };
   if (!data)
      return XO("Failed to convert preset to internal format");

   // Convert it back to a property list.
   CF_ptr<CFPropertyListRef> content{
      CFPropertyListCreateWithData(kCFAllocatorDefault,
         data.get(), kCFPropertyListImmutable, nullptr,
         // TODO might retrieve more error information
         nullptr)
   };
   if (!content)
      return XO("Failed to create property list for preset");

   // Finally, update the properties and parameters
   if (SetProperty(kAudioUnitProperty_ClassInfo, content))
      return XO("Failed to set class info for \"%s\" preset");

   // Notify interested parties of change and propagate to slaves
   Notify(mUnit.get(), kAUParameterListener_AnyParameter);

   return {};
}

void AudioUnitEffect::Notify(AudioUnit unit, AudioUnitParameterID parm) const
{
   // Notify any interested parties
   AudioUnitParameter aup = {};
   aup.mAudioUnit = unit;
   aup.mParameterID = parm;
   aup.mScope = kAudioUnitScope_Global;
   aup.mElement = 0;
   AUParameterListenerNotify(NULL, NULL, &aup);
}

OSStatus AudioUnitEffect::Render(AudioUnitRenderActionFlags *inActionFlags,
                                 const AudioTimeStamp *inTimeStamp,
                                 UInt32 inBusNumber,
                                 UInt32 inNumFrames,
                                 AudioBufferList *ioData)
{
   for (int i = 0; i < ioData->mNumberBuffers; i++)
      ioData->mBuffers[i].mData = mInputList[0].mBuffers[i].mData;

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
   return static_cast<AudioUnitEffect *>(inRefCon)->Render(inActionFlags,
      inTimeStamp, inBusNumber, inNumFrames, ioData);
}

void AudioUnitEffect::EventListener(const AudioUnitEvent *inEvent,
                                    AudioUnitParameterValue inParameterValue)
{
   // Handle property changes
   if (inEvent->mEventType == kAudioUnitEvent_PropertyChange)
   {
      // Handle latency changes
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
      AudioUnitSetParameter(mUnit.get(),
         inEvent->mArgument.mParameter.mParameterID,
         kAudioUnitScope_Global, 0, inParameterValue, 0);
   }
   else
   {
      // We're the master, so propagate 
      for (size_t i = 0, cnt = mSlaves.size(); i < cnt; i++)
      {
         mSlaves[i]->EventListener(inEvent, inParameterValue);
      }
   }
}

// static
void AudioUnitEffect::EventListenerCallback(void *inCallbackRefCon,
                                            void *inObject,
                                            const AudioUnitEvent *inEvent,
                                            UInt64 inEventHostTime,
                                            AudioUnitParameterValue inParameterValue)
{
   static_cast<AudioUnitEffect *>(inCallbackRefCon)
      ->EventListener(inEvent, inParameterValue);
}

void AudioUnitEffect::GetChannelCounts()
{
   Boolean isWritable = 0;
   UInt32  dataSize = 0;
   OSStatus result;

   // Does AU have channel info
   result = AudioUnitGetPropertyInfo(mUnit.get(),
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

   ArrayOf<char> buffer{ dataSize };
   auto info = (AUChannelInfo *) buffer.get();

   // Retrieve the channel info
   result = AudioUnitGetProperty(mUnit.get(),
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

bool AudioUnitEffect::BypassEffect(bool bypass)
{
   UInt32 value = (bypass ? 1 : 0);
   return !SetProperty(kAudioUnitProperty_BypassEffect, value);
}

#endif
