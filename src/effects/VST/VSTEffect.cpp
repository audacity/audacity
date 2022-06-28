/**********************************************************************

  Audacity: A Digital Audio Editor

  VSTEffect.cpp

  Dominic Mazzoni

  This class implements a VST Plug-in effect.  The plug-in must be
  loaded in a platform-specific way and passed into the constructor,
  but from here this class handles the interfacing.

********************************************************************//**

\class AEffect
\brief VST Effects class, conforming to VST layout.

*//********************************************************************/

//#define VST_DEBUG
//#define DEBUG_VST

// *******************************************************************
// WARNING:  This is NOT 64-bit safe
// *******************************************************************


#include "VSTEffect.h"
#include "ModuleManager.h"
#include "SampleCount.h"

#include "../../widgets/ProgressDialog.h"

#if 0
#if defined(BUILDING_AUDACITY)
#include "../../PlatformCompatibility.h"

// Make the main function private
#else
#define USE_VST 1
#endif
#endif

#if USE_VST

#include <limits.h>
#include <stdio.h>

#include <wx/setup.h> // for wxUSE_* macros
#include <wx/dynlib.h>
#include <wx/app.h>
#include <wx/defs.h>
#include <wx/buffer.h>
#include <wx/busyinfo.h>
#include <wx/button.h>
#include <wx/combobox.h>
#include <wx/file.h>
#include <wx/filename.h>
#include <wx/imaglist.h>
#include <wx/listctrl.h>
#include <wx/log.h>
#include <wx/module.h>
#include <wx/process.h>
#include <wx/recguard.h>
#include <wx/sizer.h>
#include <wx/slider.h>
#include <wx/scrolwin.h>
#include <wx/sstream.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/timer.h>
#include <wx/tokenzr.h>
#include <wx/utils.h>

#if defined(__WXMSW__)
#include <shlwapi.h>
#pragma comment(lib, "shlwapi")
#else
#include <dlfcn.h>
#endif

// TODO:  Unfortunately we have some dependencies on Audacity provided 
//        dialogs, widgets and other stuff.  This will need to be cleaned up.

#include "FileNames.h"
#include "PlatformCompatibility.h"
#include "../../SelectFile.h"
#include "../../ShuttleGui.h"
#include "../../widgets/valnum.h"
#include "../../widgets/AudacityMessageBox.h"
#include "../../widgets/NumericTextCtrl.h"
#include "XMLFileReader.h"
#include "Base64.h"

#if wxUSE_ACCESSIBILITY
#include "../../widgets/WindowAccessible.h"
#endif

#include "ConfigInterface.h"

#include <cstring>

// Put this inclusion last.  On Linux it makes some unfortunate pollution of
// preprocessor macro name space that interferes with other headers.
#if defined(__WXOSX__)
#include "VSTControlOSX.h"
#elif defined(__WXMSW__)
#include "VSTControlMSW.h"
#elif defined(__WXGTK__)
#include "VSTControlGTK.h"
#endif

static float reinterpretAsFloat(uint32_t x)
{
    static_assert(sizeof(float) == sizeof(uint32_t), "Cannot reinterpret uint32_t to float since sizes are different.");
    float f;
    std::memcpy(&f, &x, sizeof(float));
    return f;
}

static uint32_t reinterpretAsUint32(float f)
{
    static_assert(sizeof(float) == sizeof(uint32_t), "Cannot reinterpret float to uint32_t since sizes are different.");

    uint32_t x;
    std::memcpy(&x, &f, sizeof(uint32_t));
    return x;
}

// NOTE:  To debug the subprocess, use wxLogDebug and, on Windows, Debugview
//        from TechNet (Sysinternals).

// ============================================================================
//
// Module registration entry point
//
// This is the symbol that Audacity looks for when the module is built as a
// dynamic library.
//
// When the module is builtin to Audacity, we use the same function, but it is
// declared static so as not to clash with other builtin modules.
//
// ============================================================================
DECLARE_PROVIDER_ENTRY(AudacityModule)
{
   // Create our effects module and register
   // Trust the module manager not to leak this
   return safenew VSTEffectsModule();
}

// ============================================================================
//
// Register this as a builtin module
// 
// We also take advantage of the fact that wxModules are initialized before
// the wxApp::OnInit() method is called.  We check to see if Audacity was
// executed to scan a VST effect in a different process.
//
// ============================================================================
DECLARE_BUILTIN_PROVIDER(VSTBuiltin);


///////////////////////////////////////////////////////////////////////////////
///
/// Auto created at program start up, this initialises VST.
///
///////////////////////////////////////////////////////////////////////////////

VSTEffectsModule::VSTEffectsModule()
{
}

VSTEffectsModule::~VSTEffectsModule()
{
}

// ============================================================================
// ComponentInterface implementation
// ============================================================================

PluginPath VSTEffectsModule::GetPath() const
{
   return {};
}

ComponentInterfaceSymbol VSTEffectsModule::GetSymbol() const
{
   return XO("VST Effects");
}

VendorSymbol VSTEffectsModule::GetVendor() const
{
   return XO("The Audacity Team");
}

wxString VSTEffectsModule::GetVersion() const
{
   // This "may" be different if this were to be maintained as a separate DLL
   return AUDACITY_VERSION_STRING;
}

TranslatableString VSTEffectsModule::GetDescription() const
{
   return XO("Adds the ability to use VST effects in Audacity.");
}

// ============================================================================
// PluginProvider implementation
// ============================================================================

bool VSTEffectsModule::Initialize()
{
   // Nothing to do here
   return true;
}

void VSTEffectsModule::Terminate()
{
   // Nothing to do here
   return;
}

EffectFamilySymbol VSTEffectsModule::GetOptionalFamilySymbol()
{
#if USE_VST
   return VSTPLUGINTYPE;
#else
   return {};
#endif
}

const FileExtensions &VSTEffectsModule::GetFileExtensions()
{
   static FileExtensions result{{ _T("vst") }};
   return result;
}

FilePath VSTEffectsModule::InstallPath()
{
   // Not yet ready for VST drag-and-drop...
   // return FileNames::PlugInDir();

   return {};
}

void VSTEffectsModule::AutoRegisterPlugins(PluginManagerInterface &)
{
}

PluginPaths VSTEffectsModule::FindModulePaths(PluginManagerInterface & pm)
{
   FilePaths pathList;
   FilePaths files;

   // Check for the VST_PATH environment variable
   wxString vstpath = wxString::FromUTF8(getenv("VST_PATH"));
   if (!vstpath.empty())
   {
      wxStringTokenizer tok(vstpath, wxPATH_SEP);
      while (tok.HasMoreTokens())
      {
         pathList.push_back(tok.GetNextToken());
      }
   }

#if defined(__WXMAC__)  
#define VSTPATH wxT("/Library/Audio/Plug-Ins/VST")

   // Look in ~/Library/Audio/Plug-Ins/VST and /Library/Audio/Plug-Ins/VST
   pathList.push_back(wxGetHomeDir() + wxFILE_SEP_PATH + VSTPATH);
   pathList.push_back(VSTPATH);

   // Recursively search all paths for Info.plist files.  This will identify all
   // bundles.
   pm.FindFilesInPathList(wxT("Info.plist"), pathList, files, true);

   // Remove the 'Contents/Info.plist' portion of the names
   for (size_t i = 0; i < files.size(); i++)
   {
      files[i] = wxPathOnly(wxPathOnly(files[i]));
      if (!files[i].EndsWith(wxT(".vst")))
      {
         files.erase( files.begin() + i-- );
      }
   }

#elif defined(__WXMSW__)

   TCHAR dpath[MAX_PATH];
   TCHAR tpath[MAX_PATH];
   DWORD len;

   // Try HKEY_CURRENT_USER registry key first
   len = WXSIZEOF(tpath);
   if (SHRegGetUSValue(wxT("Software\\VST"),
                       wxT("VSTPluginsPath"),
                       NULL,
                       tpath,
                       &len,
                       FALSE,
                       NULL,
                       0) == ERROR_SUCCESS)
   {
      tpath[len] = 0;
      dpath[0] = 0;
      ExpandEnvironmentStrings(tpath, dpath, WXSIZEOF(dpath));
      pathList.push_back(dpath);
   }

   // Then try HKEY_LOCAL_MACHINE registry key
   len = WXSIZEOF(tpath);
   if (SHRegGetUSValue(wxT("Software\\VST"),
                       wxT("VSTPluginsPath"),
                       NULL,
                       tpath,
                       &len,
                       TRUE,
                       NULL,
                       0) == ERROR_SUCCESS)
   {
      tpath[len] = 0;
      dpath[0] = 0;
      ExpandEnvironmentStrings(tpath, dpath, WXSIZEOF(dpath));
      pathList.push_back(dpath);
   }

   // Add the default path last
   dpath[0] = 0;
   ExpandEnvironmentStrings(wxT("%ProgramFiles%\\Steinberg\\VSTPlugins"),
                            dpath,
                            WXSIZEOF(dpath));
   pathList.push_back(dpath);

   // Recursively scan for all DLLs
   pm.FindFilesInPathList(wxT("*.dll"), pathList, files, true);

#else

   // Nothing specified in the VST_PATH environment variable...provide defaults
   if (vstpath.empty())
   {
      // We add this "non-default" one
      pathList.push_back(wxT(LIBDIR) wxT("/vst"));

      // These are the defaults used by other hosts
      pathList.push_back(wxT("/usr/lib/vst"));
      pathList.push_back(wxT("/usr/local/lib/vst"));
      pathList.push_back(wxGetHomeDir() + wxFILE_SEP_PATH + wxT(".vst"));
   }

   // Recursively scan for all shared objects
   pm.FindFilesInPathList(wxT("*.so"), pathList, files, true);

#endif

   return { files.begin(), files.end() };
}

unsigned VSTEffectsModule::DiscoverPluginsAtPath(
   const PluginPath & path, TranslatableString &errMsg,
   const RegistrationCallback &callback)
{

   VSTEffect effect(path);
   if(effect.InitializePlugin())
   {
      auto effectIDs = effect.GetEffectIDs();
      if(effectIDs.empty())
         //Each VST plugin path in Audacity should have id(index) part in it
         effectIDs.push_back(0);

      for(auto id : effectIDs)
      {
         //Subsequent VSTEffect::Load may seem like overhead, but we need
         //to initialize EffectDefinitionInterface part, which includes
         //properly formatted plugin path
         VSTEffect subeffect(wxString::Format("%s;%d", path, id), &effect);
         if(callback)
            callback(this, &subeffect);
      }
      return effectIDs.size();
   }
   errMsg = XO("Could not load the library");
   return 0;
}

bool VSTEffectsModule::IsPluginValid(const PluginPath & path, bool bFast)
{
   if( bFast )
      return true;
   wxString realPath = path.BeforeFirst(wxT(';'));
   return wxFileName::FileExists(realPath) || wxFileName::DirExists(realPath);
}

std::unique_ptr<ComponentInterface>
VSTEffectsModule::LoadPlugin(const PluginPath & path)
{
   // Acquires a resource for the application.
   // For us, the ID is simply the path to the effect
   auto result = std::make_unique<VSTEffect>(path);
   result->InitializePlugin();
   return result;
}

// ============================================================================
// ModuleEffectInterface implementation
// ============================================================================

// ============================================================================
// VSTEffectsModule implementation
// ============================================================================

///////////////////////////////////////////////////////////////////////////////
//
// Dialog for configuring latency, buffer size and graphics mode for a
// VST effect.
//
///////////////////////////////////////////////////////////////////////////////
class VSTEffectOptionsDialog final : public wxDialogWrapper
{
public:
   VSTEffectOptionsDialog(wxWindow * parent, EffectDefinitionInterface &effect);
   virtual ~VSTEffectOptionsDialog();

   void PopulateOrExchange(ShuttleGui & S);

   void OnOk(wxCommandEvent & evt);

private:
   EffectDefinitionInterface &mEffect;
   int mBufferSize;
   bool mUseLatency;
   bool mUseGUI;

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(VSTEffectOptionsDialog, wxDialogWrapper)
   EVT_BUTTON(wxID_OK, VSTEffectOptionsDialog::OnOk)
END_EVENT_TABLE()

VSTEffectOptionsDialog::VSTEffectOptionsDialog(
   wxWindow * parent, EffectDefinitionInterface &effect)
:  wxDialogWrapper(parent, wxID_ANY, XO("VST Effect Options"))
, mEffect{ effect }
{
   GetConfig(mEffect, PluginSettings::Shared, wxT("Options"),
      wxT("BufferSize"), mBufferSize, 8192);
   GetConfig(mEffect, PluginSettings::Shared, wxT("Options"),
      wxT("UseLatency"), mUseLatency, true);
   GetConfig(mEffect, PluginSettings::Shared, wxT("Options"),
      wxT("UseGUI"), mUseGUI, true);

   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);
}

VSTEffectOptionsDialog::~VSTEffectOptionsDialog()
{
}

void VSTEffectOptionsDialog::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(5);
   S.StartHorizontalLay(wxEXPAND, 1);
   {
      S.StartVerticalLay(false);
      {
         S.StartStatic(XO("Buffer Size"));
         {
            S.AddVariableText( XO(
"The buffer size controls the number of samples sent to the effect "
"on each iteration. Smaller values will cause slower processing and "
"some effects require 8192 samples or less to work properly. However "
"most effects can accept large buffers and using them will greatly "
"reduce processing time."),
               false, 0, 650);

            S.StartHorizontalLay(wxALIGN_LEFT);
            {
               wxTextCtrl *t;
               t = S.Validator<IntegerValidator<int>>(
                     &mBufferSize, NumValidatorStyle::DEFAULT, 8, 1048576 * 1)
                  .MinSize( { 100, -1 } )
                  .TieNumericTextBox(XXO("&Buffer Size (8 to 1048576 samples):"),
                                       mBufferSize,
                                       12);
            }
            S.EndHorizontalLay();
         }
         S.EndStatic();

         S.StartStatic(XO("Latency Compensation"));
         {
            S.AddVariableText( XO(
"As part of their processing, some VST effects must delay returning "
"audio to Audacity. When not compensating for this delay, you will "
"notice that small silences have been inserted into the audio. "
"Enabling this option will provide that compensation, but it may "
"not work for all VST effects."),
               false, 0, 650);

            S.StartHorizontalLay(wxALIGN_LEFT);
            {
               S.TieCheckBox(XXO("Enable &compensation"),
                             mUseLatency);
            }
            S.EndHorizontalLay();
         }
         S.EndStatic();

         S.StartStatic(XO("Graphical Mode"));
         {
            S.AddVariableText( XO(
"Most VST effects have a graphical interface for setting parameter values."
" A basic text-only method is also available. "
" Reopen the effect for this to take effect."),
               false, 0, 650);
            S.TieCheckBox(XXO("Enable &graphical interface"),
                          mUseGUI);
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

void VSTEffectOptionsDialog::OnOk(wxCommandEvent & WXUNUSED(evt))
{
   if (!Validate())
   {
      return;
   }

   ShuttleGui S(this, eIsGettingFromDialog);
   PopulateOrExchange(S);

   SetConfig(mEffect, PluginSettings::Shared, wxT("Options"),
      wxT("BufferSize"), mBufferSize);
   SetConfig(mEffect, PluginSettings::Shared, wxT("Options"),
      wxT("UseLatency"), mUseLatency);
   SetConfig(mEffect, PluginSettings::Shared, wxT("Options"),
      wxT("UseGUI"), mUseGUI);

   EndModal(wxID_OK);
}

///////////////////////////////////////////////////////////////////////////////
///
/// Wrapper for wxTimer that calls a VST effect at regular intervals.
///
/// \todo should there be tests for no timer available?
///
///////////////////////////////////////////////////////////////////////////////
class VSTEffectTimer final : public wxTimer
{
public:
   VSTEffectTimer(VSTEffect *effect)
   :  wxTimer(),
      mEffect(effect)
   {
   }

   ~VSTEffectTimer()
   {
   }

   void Notify()
   {
      mEffect->OnTimer();
   }

private:
   VSTEffect *mEffect;
};

///////////////////////////////////////////////////////////////////////////////
//
// VSTEffect
//
///////////////////////////////////////////////////////////////////////////////
enum
{
   ID_Duration = 20000,
   ID_Sliders = 21000,
};

DEFINE_LOCAL_EVENT_TYPE(EVT_SIZEWINDOW);
DEFINE_LOCAL_EVENT_TYPE(EVT_UPDATEDISPLAY);

BEGIN_EVENT_TABLE(VSTEffect, wxEvtHandler)
   EVT_COMMAND_RANGE(ID_Sliders, ID_Sliders + 999, wxEVT_COMMAND_SLIDER_UPDATED, VSTEffect::OnSlider)

   // Events from the audioMaster callback
   EVT_COMMAND(wxID_ANY, EVT_SIZEWINDOW, VSTEffect::OnSizeWindow)
END_EVENT_TABLE()

// Needed to support shell plugins...sucks, but whatcha gonna do???
intptr_t VSTEffect::mCurrentEffectID;

typedef AEffect *(*vstPluginMain)(audioMasterCallback audioMaster);

intptr_t VSTEffect::AudioMaster(AEffect * effect,
                                int32_t opcode,
                                int32_t index,
                                intptr_t value,
                                void * ptr,
                                float opt)
{
   VSTEffect *vst = (effect ? (VSTEffect *) effect->ptr2 : NULL);

   // Handles operations during initialization...before VSTEffect has had a
   // chance to set its instance pointer.
   switch (opcode)
   {
      case audioMasterVersion:
         return (intptr_t) 2400;

      case audioMasterCurrentId:
         return mCurrentEffectID;

      case audioMasterGetVendorString:
         strcpy((char *) ptr, "Audacity Team");    // Do not translate, max 64 + 1 for null terminator
         return 1;

      case audioMasterGetProductString:
         strcpy((char *) ptr, "Audacity");         // Do not translate, max 64 + 1 for null terminator
         return 1;

      case audioMasterGetVendorVersion:
         return (intptr_t) (AUDACITY_VERSION << 24 |
                            AUDACITY_RELEASE << 16 |
                            AUDACITY_REVISION << 8 |
                            AUDACITY_MODLEVEL);

      // Some (older) effects depend on an effIdle call when requested.  An
      // example is the Antress Modern plugins which uses the call to update
      // the editors display when the program (preset) changes.
      case audioMasterNeedIdle:
         if (vst)
         {
            vst->NeedIdle();
            return 1;
         }
         return 0;

      // We would normally get this if the effect editor is dipslayed and something "major"
      // has changed (like a program change) instead of multiple automation calls.
      // Since we don't do anything with the parameters while the editor is displayed,
      // there's no need for us to do anything.
      case audioMasterUpdateDisplay:
         if (vst)
         {
            vst->UpdateDisplay();
            return 1;
         }
         return 0;

      // Return the current time info.
      case audioMasterGetTime:
         if (vst)
         {
            return (intptr_t) vst->GetTimeInfo();
         }
         return 0;

      // Inputs, outputs, or initial delay has changed...all we care about is initial delay.
      case audioMasterIOChanged:
         if (vst)
         {
            vst->SetBufferDelay(effect->initialDelay);
            return 1;
         }
         return 0;

      case audioMasterGetSampleRate:
         if (vst)
         {
            return (intptr_t) vst->GetSampleRate();
         }
         return 0;

      case audioMasterIdle:
         wxYieldIfNeeded();
         return 1;

      case audioMasterGetCurrentProcessLevel:
         if (vst)
         {
            return vst->GetProcessLevel();
         }
         return 0;

      case audioMasterGetLanguage:
         return kVstLangEnglish;

      // We always replace, never accumulate
      case audioMasterWillReplaceOrAccumulate:
         return 1;

      // Resize the window to accommodate the effect size
      case audioMasterSizeWindow:
         if (vst)
         {
            vst->SizeWindow(index, value);
         }
         return 1;

      case audioMasterCanDo:
      {
         char *s = (char *) ptr;
         if (strcmp(s, "acceptIOChanges") == 0 ||
            strcmp(s, "sendVstTimeInfo") == 0 ||
            strcmp(s, "startStopProcess") == 0 ||
            strcmp(s, "shellCategory") == 0 ||
            strcmp(s, "sizeWindow") == 0)
         {
            return 1;
         }

#if defined(VST_DEBUG)
#if defined(__WXMSW__)
         wxLogDebug(wxT("VST canDo: %s"), wxString::FromAscii((char *)ptr));
#else
         wxPrintf(wxT("VST canDo: %s\n"), wxString::FromAscii((char *)ptr));
#endif
#endif

         return 0;
      }

      case audioMasterBeginEdit:
      case audioMasterEndEdit:
         return 0;

      case audioMasterAutomate:
         if (vst)
         {
            vst->Automate(index, opt);
         }
         return 0;

      // We're always connected (sort of)
      case audioMasterPinConnected:

      // We don't do MIDI yet
      case audioMasterWantMidi:
      case audioMasterProcessEvents:

         // Don't need to see any messages about these
         return 0;
   }

#if defined(VST_DEBUG)
#if defined(__WXMSW__)
   wxLogDebug(wxT("vst: %p opcode: %d index: %d value: %p ptr: %p opt: %f user: %p"),
              effect, (int) opcode, (int) index, (void *) value, ptr, opt, vst);
#else
   wxPrintf(wxT("vst: %p opcode: %d index: %d value: %p ptr: %p opt: %f user: %p\n"),
            effect, (int) opcode, (int) index, (void *) value, ptr, opt, vst);
#endif
#endif

   return 0;
}

#if !defined(__WXMSW__)
void VSTEffect::ModuleDeleter::operator() (void* p) const
{
   if (p)
      dlclose(p);
}
#endif

#if defined(__WXMAC__)
void VSTEffect::ResourceHandle::reset()
{
   if (mpHandle)
      CFBundleCloseBundleResourceMap(mpHandle, mNum);
   mpHandle = nullptr;
   mNum = 0;
}
#endif

VSTEffect::VSTEffect(const PluginPath & path, VSTEffect *master)
:  mPath(path),
   mMaster(master)
{
   mModule = NULL;
   mAEffect = NULL;

   mTimer = std::make_unique<VSTEffectTimer>(this);
   mTimerGuard = 0;

   mInteractive = false;
   mAudioIns = 0;
   mAudioOuts = 0;
   mMidiIns = 0;
   mMidiOuts = 0;
   mBlockSize = mUserBlockSize = 8192;
   mBufferDelay = 0;
   mProcessLevel = 1;         // in GUI thread
   mHasPower = false;
   mWantsIdle = false;
   mWantsEditIdle = false;
   mUseLatency = true;
   mReady = false;

   memset(&mTimeInfo, 0, sizeof(mTimeInfo));
   mTimeInfo.samplePos = 0.0;
   mTimeInfo.sampleRate = 44100.0;  // this is a bogus value, but it's only for the display
   mTimeInfo.nanoSeconds = wxGetUTCTimeMillis().ToDouble();
   mTimeInfo.tempo = 120.0;
   mTimeInfo.timeSigNumerator = 4;
   mTimeInfo.timeSigDenominator = 4;
   mTimeInfo.flags = kVstTempoValid | kVstNanosValid;

   // UI
   
   mGui = false;
   mContainer = NULL;

   // If we're a slave then go ahead a load immediately
   if (mMaster)
   {
      Load();
   }
}

VSTEffect::~VSTEffect()
{
   Unload();
}

// ============================================================================
// ComponentInterface Implementation
// ============================================================================

PluginPath VSTEffect::GetPath() const
{
   return mPath;
}

ComponentInterfaceSymbol VSTEffect::GetSymbol() const
{
   return mName;
}

VendorSymbol VSTEffect::GetVendor() const
{
   return { mVendor };
}

wxString VSTEffect::GetVersion() const
{
   wxString version;

   bool skipping = true;
   for (int i = 0, s = 0; i < 4; i++, s += 8)
   {
      int dig = (mVersion >> s) & 0xff;
      if (dig != 0 || !skipping)
      {
         version += !skipping ? wxT(".") : wxT("");
         version += wxString::Format(wxT("%d"), dig);
         skipping = false;
      }
   }

   return version;
}

TranslatableString VSTEffect::GetDescription() const
{
   // VST does have a product string opcode and some effects return a short
   // description, but most do not or they just return the name again.  So,
   // try to provide some sort of useful information.
   return XO("Audio In: %d, Audio Out: %d").Format( mAudioIns, mAudioOuts );
}

// ============================================================================
// EffectDefinitionInterface Implementation
// ============================================================================

EffectType VSTEffect::GetType() const
{
   if (mAudioIns == 0 && mAudioOuts == 0 && mMidiIns == 0 && mMidiOuts == 0)
   {
      return EffectTypeTool;
   }

   if (mAudioIns == 0 && mMidiIns == 0)
   {
      return EffectTypeGenerate;
   }

   if (mAudioOuts == 0 && mMidiOuts == 0)
   {
      return EffectTypeAnalyze;
   }

   return EffectTypeProcess;
}


EffectFamilySymbol VSTEffect::GetFamily() const
{
   return VSTPLUGINTYPE;
}

bool VSTEffect::IsInteractive() const
{
   return mInteractive;
}

bool VSTEffect::IsDefault() const
{
   return false;
}

auto VSTEffect::RealtimeSupport() const -> RealtimeSince
{
   // TODO reenable after achieving statelessness
   return RealtimeSince::Never;
//   return GetType() == EffectTypeProcess
//      ? RealtimeSince::Always
//      : RealtimeSince::Never;
}

bool VSTEffect::SupportsAutomation() const
{
   return mAutomatable;
}

bool VSTEffect::InitializePlugin()
{
   if (!mAEffect)
   {
      Load();
   }

   if (!mAEffect)
   {
      return false;
   }

   // If we have a master then there's no need to load settings since the master will feed
   // us everything we need.
   if (mMaster)
   {
      return true;
   }

   return true;
}

std::shared_ptr<EffectInstance> VSTEffect::MakeInstance() const
{
   return const_cast<VSTEffect*>(this)->DoMakeInstance();
}

std::shared_ptr<EffectInstance> VSTEffect::DoMakeInstance()
{
   int userBlockSize;
   GetConfig(*this, PluginSettings::Shared, wxT("Options"),
      wxT("BufferSize"), userBlockSize, 8192);
   mUserBlockSize = std::max( 1, userBlockSize );
   GetConfig(*this, PluginSettings::Shared, wxT("Options"),
      wxT("UseLatency"), mUseLatency, true);
   mBlockSize = mUserBlockSize;
   return std::make_shared<Instance>(*this);
}

unsigned VSTEffect::GetAudioInCount() const
{
   return mAudioIns;
}

unsigned VSTEffect::GetAudioOutCount() const
{
   return mAudioOuts;
}

int VSTEffect::GetMidiInCount() const
{
   return mMidiIns;
}

int VSTEffect::GetMidiOutCount() const
{
   return mMidiOuts;
}

size_t VSTEffect::SetBlockSize(size_t maxBlockSize)
{
   mBlockSize = std::min( maxBlockSize, mUserBlockSize );
   return mBlockSize;
}

size_t VSTEffect::GetBlockSize() const
{
   return mBlockSize;
}

sampleCount VSTEffect::GetLatency()
{
   if (mUseLatency)
   {
      // ??? Threading issue ???
      auto delay = mBufferDelay;
      mBufferDelay = 0;
      return delay;
   }

   return 0;
}

bool VSTEffect::IsReady()
{
   return mReady;
}

bool VSTEffect::ProcessInitialize(
   EffectSettings &, double sampleRate, sampleCount, ChannelNames)
{
   return DoProcessInitialize(sampleRate);
}

bool VSTEffect::DoProcessInitialize(double sampleRate)
{
   // Initialize time info
   memset(&mTimeInfo, 0, sizeof(mTimeInfo));
   mTimeInfo.sampleRate = sampleRate;
   mTimeInfo.nanoSeconds = wxGetUTCTimeMillis().ToDouble();
   mTimeInfo.tempo = 120.0;
   mTimeInfo.timeSigNumerator = 4;
   mTimeInfo.timeSigDenominator = 4;
   mTimeInfo.flags = kVstTempoValid | kVstNanosValid | kVstTransportPlaying;

   // Set processing parameters...power must be off for this
   callDispatcher(effSetSampleRate, 0, 0, NULL, sampleRate);
   callDispatcher(effSetBlockSize, 0, mBlockSize, NULL, 0.0);

   // Turn on the power
   PowerOn();

   // Set the initial buffer delay
   SetBufferDelay(mAEffect->initialDelay);

   mReady = true;
   return true;
}

bool VSTEffect::ProcessFinalize()
{
   mReady = false;

   PowerOff();

   return true;
}

size_t VSTEffect::ProcessBlock(EffectSettings &,
   const float *const *inBlock, float *const *outBlock, size_t blockLen)
{
   // Only call the effect if there's something to do...some do not like zero-length block
   if (blockLen)
   {
      // Go let the plugin moleste the samples
      callProcessReplacing(inBlock, outBlock, blockLen);

      // And track the position
      mTimeInfo.samplePos += (double) blockLen;
   }

   return blockLen;
}

unsigned VSTEffect::GetChannelCount()
{
   return mNumChannels;
}

void VSTEffect::SetChannelCount(unsigned numChannels)
{
   mNumChannels = numChannels;
}

bool VSTEffect::RealtimeInitialize(EffectSettings &settings, double sampleRate)
{
   return ProcessInitialize(settings, sampleRate, 0, nullptr);
}

bool VSTEffect::RealtimeAddProcessor(
   EffectSettings &settings, unsigned numChannels, float sampleRate)
{
   auto slave = std::make_unique<VSTEffect>(mPath, this);

   slave->SetBlockSize(mBlockSize);
   slave->SetChannelCount(numChannels);

   int clen = 0;
   if (mAEffect->flags & effFlagsProgramChunks) {
      void *chunk = NULL;
      clen = (int) callDispatcher(effGetChunk, 1, 0, &chunk, 0.0); // get master's chunk, for the program only
      if (clen != 0)
         slave->callSetChunk(true, clen, chunk); // copy state to slave, for the program only
   }

   if (clen == 0) {
      callDispatcher(effBeginSetProgram, 0, 0, NULL, 0.0);
      for (int i = 0; i < mAEffect->numParams; i++)
         slave->callSetParameter(i, callGetParameter(i));
      callDispatcher(effEndSetProgram, 0, 0, NULL, 0.0);
   }

   if (!slave->ProcessInitialize(settings, sampleRate, 0, nullptr))
      return false;

   mSlaves.emplace_back(move(slave));
   return true;
}

bool VSTEffect::RealtimeFinalize(EffectSettings &) noexcept
{
return GuardedCall<bool>([&]{
   for (const auto &slave : mSlaves)
      slave->ProcessFinalize();
   mSlaves.clear();

   return ProcessFinalize();
});
}

bool VSTEffect::RealtimeSuspend()
{
   PowerOff();

   for (const auto &slave : mSlaves)
      slave->PowerOff();

   return true;
}

bool VSTEffect::RealtimeResume()
{
   PowerOn();

   for (const auto &slave : mSlaves)
      slave->PowerOn();

   return true;
}

bool VSTEffect::RealtimeProcessStart(EffectSettings &)
{
   return true;
}

size_t VSTEffect::RealtimeProcess(size_t group, EffectSettings &settings,
   const float *const *inbuf, float *const *outbuf, size_t numSamples)
{
   wxASSERT(numSamples <= mBlockSize);
   if (group >= mSlaves.size())
      return 0;
   return mSlaves[group]->ProcessBlock(settings, inbuf, outbuf, numSamples);
}

bool VSTEffect::RealtimeProcessEnd(EffectSettings &) noexcept
{
   return true;
}

///
/// Some history...
///
/// Before we ran into the Antress plugin problem with buffer size limitations,
/// (see below) we just had a plain old effect loop...get the input samples, pass
/// them to the effect, save the output samples.
///
/// But, the hack I put in to limit the buffer size to only 8k (normally 512k or so)
/// severely impacted performance.  So, Michael C. added some intermediate buffering
/// that sped things up quite a bit and this is how things have worked for quite a
/// while.  It still didn't get the performance back to the pre-hack stage, but it
/// was a definite benefit.
///
/// History over...
///
/// I've recently (May 2014) tried newer versions of the Antress effects and they
/// no longer seem to have a problem with buffer size.  So, I've made a bit of a
/// compromise...I've made the buffer size user configurable.  Should have done this
/// from the beginning.  I've left the default 8k, just in case, but now the user
/// can set the buffering based on their specific setup and needs.
///
/// And at the same time I added buffer delay compensation, which allows Audacity
/// to account for latency introduced by some effects.  This is based on information
/// provided by the effect, so it will not work with all effects since they don't
/// all provide the information (kn0ck0ut is one).
///
int VSTEffect::ShowClientInterface(
   wxWindow &parent, wxDialog &dialog, bool forceModal)
{
   //   mProcessLevel = 1;      // in GUI thread

   if (!IsReady())
   {
      // Set some defaults since some VSTs need them...these will be reset when
      // normal or realtime processing begins
      mBlockSize = 8192;
      DoProcessInitialize(mProjectRate);
   }

   // Remember the dialog with a weak pointer, but don't control its lifetime
   mDialog = &dialog;
   mDialog->CentreOnParent();

   if (SupportsRealtime() && !forceModal)
   {
      mDialog->Show();
      return 0;
   }

   return mDialog->ShowModal();
}

bool VSTEffect::SaveSettings(
   const EffectSettings &, CommandParameters & parms) const
{
   for (int i = 0; i < mAEffect->numParams; i++)
   {
      wxString name = GetString(effGetParamName, i);
      if (name.empty())
      {
         name.Printf(wxT("parm_%d"), i);
      }

      float value = callGetParameter(i);
      if (!parms.Write(name, value))
      {
         return false;
      }
   }

   return true;
}

bool VSTEffect::LoadSettings(
   const CommandParameters & parms, EffectSettings &settings) const
{
   constCallDispatcher(effBeginSetProgram, 0, 0, NULL, 0.0);
   for (int i = 0; i < mAEffect->numParams; i++)
   {
      wxString name = GetString(effGetParamName, i);
      if (name.empty())
      {
         name.Printf(wxT("parm_%d"), i);
      }

      double d = 0.0;
      if (!parms.Read(name, &d))
      {
         return false;
      }

      if (d >= -1.0 && d <= 1.0)
      {
         const_cast<VSTEffect*>(this)->callSetParameter(i, d);
         for (const auto &slave : mSlaves)
            slave->callSetParameter(i, d);
      }
   }
   constCallDispatcher(effEndSetProgram, 0, 0, NULL, 0.0);

   return true;
}

bool VSTEffect::LoadUserPreset(
   const RegistryPath & name, EffectSettings &settings) const
{
   // To do: externalize state so const_cast isn't needed
   return const_cast<VSTEffect*>(this)->DoLoadUserPreset(name, settings);
}

bool VSTEffect::DoLoadUserPreset(
   const RegistryPath & name, EffectSettings &settings)
{
   if (!LoadParameters(name, settings))
   {
      return false;
   }

   RefreshParameters();

   return true;
}

bool VSTEffect::SaveUserPreset(
   const RegistryPath & name, const EffectSettings &settings) const
{
   return SaveParameters(name, settings);
}

RegistryPaths VSTEffect::GetFactoryPresets() const
{
   RegistryPaths progs;

   // Some plugins, like Guitar Rig 5, only report 128 programs while they have hundreds.  While
   // I was able to come up with a hack in the Guitar Rig case to gather all of the program names
   // it would not let me set a program outside of the first 128.
   if (mVstVersion >= 2)
   {
      for (int i = 0; i < mAEffect->numPrograms; i++)
      {
         progs.push_back(GetString(effGetProgramNameIndexed, i));
      }
   }

   return progs;
}

bool VSTEffect::LoadFactoryPreset(int id, EffectSettings &) const
{
   // To do: externalize state so const_cast isn't needed
   return const_cast<VSTEffect*>(this)->DoLoadFactoryPreset(id);
}

bool VSTEffect::DoLoadFactoryPreset(int id)
{
   callSetProgram(id);

   RefreshParameters();

   return true;
}

// ============================================================================
// EffectUIClientInterface implementation
// ============================================================================

std::unique_ptr<EffectUIValidator> VSTEffect::PopulateUI(ShuttleGui &S,
   EffectInstance &, EffectSettingsAccess &access)
{
   auto parent = S.GetParent();
   mDialog = static_cast<wxDialog *>(wxGetTopLevelParent(parent));
   mParent = parent;

   mParent->PushEventHandler(this);

   // Determine if the VST editor is supposed to be used or not
   GetConfig(*this, PluginSettings::Shared, wxT("Options"),
                          wxT("UseGUI"),
                          mGui,
                          true);
   mGui = mAEffect->flags & effFlagsHasEditor ? mGui : false;

   // Must use the GUI editor if parameters aren't provided
   if (mAEffect->numParams == 0)
   {
      mGui = true;
   }

   // Build the appropriate dialog type
   if (mGui)
   {
      BuildFancy();
   }
   else
   {
      BuildPlain(access);
   }

   return std::make_unique<DefaultEffectUIValidator>(*this, access);
}

bool VSTEffect::IsGraphicalUI()
{
   return mGui;
}

bool VSTEffect::ValidateUI(EffectSettings &settings)
{
   if (GetType() == EffectTypeGenerate)
      settings.extra.SetDuration(mDuration->GetValue());

   return true;
}

bool VSTEffect::CloseUI()
{
#ifdef __WXMAC__
#ifdef __WX_EVTLOOP_BUSY_WAITING__
   wxEventLoop::SetBusyWaiting(false);
#endif
   mControl->Close();
#endif

   mParent->RemoveEventHandler(this);

   PowerOff();

   NeedEditIdle(false);

   RemoveHandler();

   mNames.reset();
   mSliders.reset();
   mDisplays.reset();
   mLabels.reset();

   mParent = NULL;
   mDialog = NULL;

   return true;
}

bool VSTEffect::CanExportPresets()
{
   return true;
}

// Throws exceptions rather than reporting errors.
void VSTEffect::ExportPresets(const EffectSettings &) const
{
   wxString path;

   // Ask the user for the real name
   //
   // Passing a valid parent will cause some effects dialogs to malfunction
   // upon returning from the SelectFile().
   path = SelectFile(FileNames::Operation::Presets,
      XO("Save VST Preset As:"),
      wxEmptyString,
      wxT("preset"),
      wxT("xml"),
      {
        { XO("Standard VST bank file"), { wxT("fxb") }, true },
        { XO("Standard VST program file"), { wxT("fxp") }, true },
        { XO("Audacity VST preset file"), { wxT("xml") }, true },
      },
      wxFD_SAVE | wxFD_OVERWRITE_PROMPT | wxRESIZE_BORDER,
      NULL);

   // User canceled...
   if (path.empty())
   {
      return;
   }

   wxFileName fn(path);
   wxString ext = fn.GetExt();
   if (ext.CmpNoCase(wxT("fxb")) == 0)
   {
      SaveFXB(fn);
   }
   else if (ext.CmpNoCase(wxT("fxp")) == 0)
   {
      SaveFXP(fn);
   }
   else if (ext.CmpNoCase(wxT("xml")) == 0)
   {
      // may throw
      SaveXML(fn);
   }
   else
   {
      // This shouldn't happen, but complain anyway
      AudacityMessageBox(
         XO("Unrecognized file extension."),
         XO("Error Saving VST Presets"),
         wxOK | wxCENTRE,
         mParent);

      return;
   }
}

//
// Load an "fxb", "fxp" or Audacuty "xml" file
//
// Based on work by Sven Giermann
//
void VSTEffect::ImportPresets(EffectSettings &)
{
   wxString path;

   // Ask the user for the real name
   path = SelectFile(FileNames::Operation::Presets,
      XO("Load VST Preset:"),
      wxEmptyString,
      wxT("preset"),
      wxT("xml"),
      { {
         XO("VST preset files"),
         { wxT("fxb"), wxT("fxp"), wxT("xml") },
         true
      } },
      wxFD_OPEN | wxRESIZE_BORDER,
      mParent);

   // User canceled...
   if (path.empty())
   {
      return;
   }

   wxFileName fn(path);
   wxString ext = fn.GetExt();
   bool success = false;
   if (ext.CmpNoCase(wxT("fxb")) == 0)
   {
      success = LoadFXB(fn);
   }
   else if (ext.CmpNoCase(wxT("fxp")) == 0)
   {
      success = LoadFXP(fn);
   }
   else if (ext.CmpNoCase(wxT("xml")) == 0)
   {
      success = LoadXML(fn);
   }
   else
   {
      // This shouldn't happen, but complain anyway
      AudacityMessageBox(
         XO("Unrecognized file extension."),
         XO("Error Loading VST Presets"),
         wxOK | wxCENTRE,
         mParent);

         return;
   }

   if (!success)
   {
      AudacityMessageBox(
         XO("Unable to load presets file."),
         XO("Error Loading VST Presets"),
         wxOK | wxCENTRE,
         mParent);

      return;
   }

   RefreshParameters();

   return;
}

bool VSTEffect::HasOptions()
{
   return true;
}

void VSTEffect::ShowOptions()
{
   VSTEffectOptionsDialog dlg(mParent, *this);
   if (dlg.ShowModal())
   {
      // Reinitialize configuration settings
      int userBlockSize;
      GetConfig(*this, PluginSettings::Shared, wxT("Options"),
         wxT("BufferSize"), userBlockSize, 8192);
      mUserBlockSize = std::max( 1, userBlockSize );
      GetConfig(*this, PluginSettings::Shared, wxT("Options"),
         wxT("UseLatency"), mUseLatency, true);
   }
}

// ============================================================================
// VSTEffect implementation
// ============================================================================

bool VSTEffect::Load()
{
   vstPluginMain pluginMain;
   bool success = false;

   long effectID = 0;
   wxString realPath = mPath.BeforeFirst(wxT(';'));
   mPath.AfterFirst(wxT(';')).ToLong(&effectID);
   mCurrentEffectID = (intptr_t) effectID;

   mModule = NULL;
   mAEffect = NULL;

#if defined(__WXMAC__)
   // Start clean
   mBundleRef.reset();

   mResource = ResourceHandle{};

   // Convert the path to a CFSTring
   wxCFStringRef path(realPath);

   // Create the bundle using the URL
   BundleHandle bundleRef{ CFBundleCreate(kCFAllocatorDefault,
      // Convert the path to a URL
      CF_ptr<CFURLRef>{
         CFURLCreateWithFileSystemPath(kCFAllocatorDefault,
            path, kCFURLPOSIXPathStyle, true)
      }.get()
   )};

   // Bail if the bundle wasn't created
   if (!bundleRef)
      return false;


   // Convert back to path
   UInt8 exePath[PLATFORM_MAX_PATH];
   Boolean good = CFURLGetFileSystemRepresentation(
      // Retrieve a reference to the executable
      CF_ptr<CFURLRef>{ CFBundleCopyExecutableURL(bundleRef.get()) }.get(),
      true, exePath, sizeof(exePath)
   );

   // Bail if we couldn't resolve the executable path
   if (good == FALSE)
      return false;

   // Attempt to open it
   mModule.reset((char*)dlopen((char *) exePath, RTLD_NOW | RTLD_LOCAL));
   if (!mModule)
      return false;

   // Try to locate the NEW plugin entry point
   pluginMain = (vstPluginMain) dlsym(mModule.get(), "VSTPluginMain");

   // If not found, try finding the old entry point
   if (pluginMain == NULL)
   {
      pluginMain = (vstPluginMain) dlsym(mModule.get(), "main_macho");
   }

   // Must not be a VST plugin
   if (pluginMain == NULL)
   {
      mModule.reset();
      return false;
   }

   // Need to keep the bundle reference around so we can map the
   // resources.
   mBundleRef = std::move(bundleRef);

   // Open the resource map ... some plugins (like GRM Tools) need this.
   mResource = ResourceHandle{
      mBundleRef.get(), CFBundleOpenBundleResourceMap(mBundleRef.get())
   };

#elif defined(__WXMSW__)

   {
      wxLogNull nolog;

      // Try to load the library
      auto lib = std::make_unique<wxDynamicLibrary>(realPath);
      if (!lib) 
         return false;

      // Bail if it wasn't successful
      if (!lib->IsLoaded())
         return false;

      // Try to find the entry point, while suppressing error messages
      pluginMain = (vstPluginMain) lib->GetSymbol(wxT("VSTPluginMain"));
      if (pluginMain == NULL)
      {
         pluginMain = (vstPluginMain) lib->GetSymbol(wxT("main"));
         if (pluginMain == NULL)
            return false;
      }

      // Save the library reference
      mModule = std::move(lib);
   }

#else

   // Attempt to load it
   //
   // Spent a few days trying to figure out why some VSTs where running okay and
   // others were hit or miss.  The cause was that we export all of Audacity's
   // symbols and some of the loaded libraries were picking up Audacity's and 
   // not their own.
   //
   // So far, I've only seen this issue on Linux, but we might just be getting
   // lucky on the Mac and Windows.  The sooner we stop exporting everything
   // the better.
   //
   // To get around the problem, I just added the RTLD_DEEPBIND flag to the load
   // and that "basically" puts Audacity last when the loader needs to resolve
   // symbols.
   //
   // Once we define a proper external API, the flags can be removed.
#ifndef RTLD_DEEPBIND
#define RTLD_DEEPBIND 0
#endif
   ModuleHandle lib {
      (char*) dlopen((const char *)wxString(realPath).ToUTF8(),
                     RTLD_NOW | RTLD_LOCAL | RTLD_DEEPBIND)
   };
   if (!lib) 
   {
      return false;
   }

   // Try to find the entry point, while suppressing error messages
   pluginMain = (vstPluginMain) dlsym(lib.get(), "VSTPluginMain");
   if (pluginMain == NULL)
   {
      pluginMain = (vstPluginMain) dlsym(lib.get(), "main");
      if (pluginMain == NULL)
         return false;
   }

   // Save the library reference
   mModule = std::move(lib);

#endif

   // Initialize the plugin
   try
   {
      mAEffect = pluginMain(VSTEffect::AudioMaster);
   }
   catch (...)
   {
      wxLogMessage(wxT("VST plugin initialization failed\n"));
      mAEffect = NULL;
   }

   // Was it successful?
   if (mAEffect)
   {
      // Save a reference to ourselves
      //
      // Note:  Some hosts use "user" and some use "ptr2/resvd2".  It might
      //        be worthwhile to check if user is NULL before using it and
      //        then falling back to "ptr2/resvd2".
      mAEffect->ptr2 = this;

      // Give the plugin an initial sample rate and blocksize
      callDispatcher(effSetSampleRate, 0, 0, NULL, 48000.0);
      callDispatcher(effSetBlockSize, 0, 512, NULL, 0);

      // Ask the plugin to identify itself...might be needed for older plugins
      callDispatcher(effIdentify, 0, 0, NULL, 0);

      // Open the plugin
      callDispatcher(effOpen, 0, 0, NULL, 0.0);

      // Get the VST version the plugin understands
      mVstVersion = callDispatcher(effGetVstVersion, 0, 0, NULL, 0);

      // Set it again in case plugin ignored it before the effOpen
      callDispatcher(effSetSampleRate, 0, 0, NULL, 48000.0);
      callDispatcher(effSetBlockSize, 0, 512, NULL, 0);

      // Ensure that it looks like a plugin and can deal with ProcessReplacing
      // calls.  Also exclude synths for now.
      if (mAEffect->magic == kEffectMagic &&
         !(mAEffect->flags & effFlagsIsSynth) &&
         mAEffect->flags & effFlagsCanReplacing)
      {
         if (mVstVersion >= 2)
         {
            mName = GetString(effGetEffectName);
            if (mName.length() == 0)
            {
               mName = GetString(effGetProductString);
            }
         }
         if (mName.length() == 0)
         {
            mName = wxFileName{realPath}.GetName();
         }

         if (mVstVersion >= 2)
         {
            mVendor = GetString(effGetVendorString);
            mVersion = wxINT32_SWAP_ON_LE(callDispatcher(effGetVendorVersion, 0, 0, NULL, 0));
         }
         if (mVersion == 0)
         {
            mVersion = wxINT32_SWAP_ON_LE(mAEffect->version);
         }

         if (mAEffect->flags & effFlagsHasEditor || mAEffect->numParams != 0)
         {
            mInteractive = true;
         }

         mAudioIns = mAEffect->numInputs;
         mAudioOuts = mAEffect->numOutputs;

         mMidiIns = 0;
         mMidiOuts = 0;

         // Check to see if parameters can be automated.  This isn't a guarantee
         // since it could be that the effect simply doesn't support the opcode.
         mAutomatable = false;
         for (int i = 0; i < mAEffect->numParams; i++)
         {
            if (callDispatcher(effCanBeAutomated, 0, i, NULL, 0.0))
            {
               mAutomatable = true;
               break;
            }
         }

         // Make sure we start out with a valid program selection
         // I've found one plugin (SoundHack +morphfilter) that will
         // crash Audacity when saving the initial default parameters
         // with this.
         callSetProgram(0);

         // Pretty confident that we're good to go
         success = true;
      }
   }

   if (!success)
   {
      Unload();
   }

   return success;
}

void VSTEffect::Unload()
{
   if (mDialog)
   {
      CloseUI();
   }

   if (mAEffect)
   {
      // Turn the power off
      PowerOff();

      // Finally, close the plugin
      callDispatcher(effClose, 0, 0, NULL, 0.0);
      mAEffect = NULL;
   }

   if (mModule)
   {
#if defined(__WXMAC__)
      mResource.reset();
      mBundleRef.reset();
#endif

      mModule.reset();
      mAEffect = NULL;
   }
}

std::vector<int> VSTEffect::GetEffectIDs()
{
   std::vector<int> effectIDs;

   // Are we a shell?
   if (mVstVersion >= 2 && (VstPlugCategory) callDispatcher(effGetPlugCategory, 0, 0, NULL, 0) == kPlugCategShell)
   {
      char name[64];
      int effectID;

      effectID = (int) callDispatcher(effShellGetNextPlugin, 0, 0, &name, 0);
      while (effectID)
      {
         effectIDs.push_back(effectID);
         effectID = (int) callDispatcher(effShellGetNextPlugin, 0, 0, &name, 0);
      }
   }

   return effectIDs;
}

bool VSTEffect::LoadParameters(
   const RegistryPath & group, EffectSettings &settings)
{
   wxString value;

   VstPatchChunkInfo info = {1, mAEffect->uniqueID, mAEffect->version, mAEffect->numParams, ""};
   GetConfig(*this, PluginSettings::Private, group, wxT("UniqueID"),
      info.pluginUniqueID, info.pluginUniqueID);
   GetConfig(*this, PluginSettings::Private, group, wxT("Version"),
      info.pluginVersion, info.pluginVersion);
   GetConfig(*this, PluginSettings::Private, group, wxT("Elements"),
      info.numElements, info.numElements);

   if ((info.pluginUniqueID != mAEffect->uniqueID) ||
       (info.pluginVersion != mAEffect->version) ||
       (info.numElements != mAEffect->numParams))
   {
      return false;
   }

   if (GetConfig(*this,
      PluginSettings::Private, group, wxT("Chunk"), value, wxEmptyString))
   {
      ArrayOf<char> buf{ value.length() / 4 * 3 };

      int len = Base64::Decode(value, buf.get());
      if (len)
      {
         callSetChunk(true, len, buf.get(), &info);
      }

      return true;
   }

   wxString parms;
   if (!GetConfig(*this,
      PluginSettings::Private, group, wxT("Parameters"), parms, wxEmptyString))
   {
      return false;
   }

   CommandParameters eap;
   if (!eap.SetParameters(parms))
   {
      return false;
   }

   return LoadSettings(eap, settings);
}

bool VSTEffect::SaveParameters(
   const RegistryPath & group, const EffectSettings &settings) const
{
   SetConfig(*this, PluginSettings::Private, group, wxT("UniqueID"),
      mAEffect->uniqueID);
   SetConfig(*this, PluginSettings::Private, group, wxT("Version"),
      mAEffect->version);
   SetConfig(*this, PluginSettings::Private, group, wxT("Elements"),
      mAEffect->numParams);

   if (mAEffect->flags & effFlagsProgramChunks)
   {
      void *chunk = NULL;
      int clen = (int) constCallDispatcher(effGetChunk, 1, 0, &chunk, 0.0);
      if (clen <= 0)
      {
         return false;
      }

      SetConfig(*this, PluginSettings::Private, group, wxT("Chunk"),
         Base64::Encode(chunk, clen));
      return true;
   }

   CommandParameters eap;
   if (!SaveSettings(settings, eap))
   {
      return false;
   }

   wxString parms;
   if (!eap.GetParameters(parms))
   {
      return false;
   }

   return SetConfig(*this, PluginSettings::Private,
      group, wxT("Parameters"), parms);
}

void VSTEffect::OnTimer()
{
   wxRecursionGuard guard(mTimerGuard);

   // Ignore it if we're recursing
   if (guard.IsInside())
   {
      return;
   }

   if (mVstVersion >= 2 && mWantsIdle)
   {
      int ret = callDispatcher(effIdle, 0, 0, NULL, 0.0);
      if (!ret)
      {
         mWantsIdle = false;
      }
   }

   if (mWantsEditIdle)
   {
      callDispatcher(effEditIdle, 0, 0, NULL, 0.0);
   }
}

void VSTEffect::NeedIdle()
{
   mWantsIdle = true;
   mTimer->Start(100);
}

void VSTEffect::NeedEditIdle(bool state)
{
   mWantsEditIdle = state;
   mTimer->Start(100);
}

VstTimeInfo *VSTEffect::GetTimeInfo()
{
   mTimeInfo.nanoSeconds = wxGetUTCTimeMillis().ToDouble();
   return &mTimeInfo;
}

float VSTEffect::GetSampleRate()
{
   return mTimeInfo.sampleRate;
}

int VSTEffect::GetProcessLevel()
{
   return mProcessLevel;
}

void VSTEffect::PowerOn()
{
   if (!mHasPower)
   {
      // Turn the power on
      callDispatcher(effMainsChanged, 0, 1, NULL, 0.0);

      // Tell the effect we're going to start processing
      if (mVstVersion >= 2)
      {
         callDispatcher(effStartProcess, 0, 0, NULL, 0.0);
      }

      // Set state
      mHasPower = true;
   }
}

void VSTEffect::PowerOff()
{
   if (mHasPower)
   {
      // Tell the effect we're going to stop processing
      if (mVstVersion >= 2)
      {
         callDispatcher(effStopProcess, 0, 0, NULL, 0.0);
      }

      // Turn the power off
      callDispatcher(effMainsChanged, 0, 0, NULL, 0.0);

      // Set state
      mHasPower = false;
   }
}

void VSTEffect::SizeWindow(int w, int h)
{
   // Queue the event to make the resizes smoother
   if (mParent)
   {
      wxCommandEvent sw(EVT_SIZEWINDOW);
      sw.SetInt(w);
      sw.SetExtraLong(h);
      mParent->GetEventHandler()->AddPendingEvent(sw);
   }

   return;
}

void VSTEffect::UpdateDisplay()
{
#if 0
   // Tell the dialog to refresh effect information
   if (mParent)
   {
      wxCommandEvent ud(EVT_UPDATEDISPLAY);
      mParent->GetEventHandler()->AddPendingEvent(ud);
   }
#endif
   return;
}

void VSTEffect::Automate(int index, float value)
{
   // Just ignore it if we're a slave
   if (mMaster)
   {
      return;
   }

   for (const auto &slave : mSlaves)
      slave->callSetParameter(index, value);

   return;
}

void VSTEffect::SetBufferDelay(int samples)
{
   // We do not support negative delay
   if (samples >= 0 && mUseLatency)
   {
      mBufferDelay = samples;
   }

   return;
}

int VSTEffect::GetString(wxString & outstr, int opcode, int index) const
{
   char buf[256];

   memset(buf, 0, sizeof(buf));

   // Assume we are passed a read-only dispatcher function code
   constCallDispatcher(opcode, index, 0, buf, 0.0);

   outstr = wxString::FromUTF8(buf);

   return 0;
}

wxString VSTEffect::GetString(int opcode, int index) const
{
   wxString str;

   GetString(str, opcode, index);

   return str;
}

void VSTEffect::SetString(int opcode, const wxString & str, int index)
{
   char buf[256];
   strcpy(buf, str.Left(255).ToUTF8());

   callDispatcher(opcode, index, 0, buf, 0.0);
}

intptr_t VSTEffect::callDispatcher(int opcode,
                                   int index, intptr_t value, void *ptr, float opt)
{
   // Needed since we might be in the dispatcher when the timer pops
   wxCRIT_SECT_LOCKER(locker, mDispatcherLock);
   return mAEffect->dispatcher(mAEffect, opcode, index, value, ptr, opt);
}

intptr_t VSTEffect::constCallDispatcher(int opcode,
   int index, intptr_t value, void *ptr, float opt) const
{
   // Assume we are passed a read-only dispatcher function code
   return const_cast<VSTEffect*>(this)
      ->callDispatcher(opcode, index, value, ptr, opt);
}

void VSTEffect::callProcessReplacing(const float *const *inputs,
   float *const *outputs, int sampleframes)
{
   mAEffect->processReplacing(mAEffect,
      const_cast<float**>(inputs),
      const_cast<float**>(outputs), sampleframes);
}

float VSTEffect::callGetParameter(int index) const
{
   return mAEffect->getParameter(mAEffect, index);
}

void VSTEffect::callSetParameter(int index, float value)
{
   if (mVstVersion == 0 || callDispatcher(effCanBeAutomated, 0, index, NULL, 0.0))
   {
      mAEffect->setParameter(mAEffect, index, value);

      for (const auto &slave : mSlaves)
         slave->callSetParameter(index, value);
   }
}

void VSTEffect::callSetProgram(int index)
{
   callDispatcher(effBeginSetProgram, 0, 0, NULL, 0.0);

   callDispatcher(effSetProgram, 0, index, NULL, 0.0);
   for (const auto &slave : mSlaves)
      slave->callSetProgram(index);

   callDispatcher(effEndSetProgram, 0, 0, NULL, 0.0);
}

void VSTEffect::callSetChunk(bool isPgm, int len, void *buf)
{
   VstPatchChunkInfo info;

   memset(&info, 0, sizeof(info));
   info.version = 1;
   info.pluginUniqueID = mAEffect->uniqueID;
   info.pluginVersion = mAEffect->version;
   info.numElements = isPgm ? mAEffect->numParams : mAEffect->numPrograms;

   callSetChunk(isPgm, len, buf, &info);
}

void VSTEffect::callSetChunk(bool isPgm, int len, void *buf, VstPatchChunkInfo *info)
{
   if (isPgm)
   {
      // Ask the effect if this is an acceptable program
      if (callDispatcher(effBeginLoadProgram, 0, 0, info, 0.0) == -1)
      {
         return;
      }
   }
   else
   {
      // Ask the effect if this is an acceptable bank
      if (callDispatcher(effBeginLoadBank, 0, 0, info, 0.0) == -1)
      {
         return;
      }
   }

   callDispatcher(effBeginSetProgram, 0, 0, NULL, 0.0);
   callDispatcher(effSetChunk, isPgm ? 1 : 0, len, buf, 0.0);
   callDispatcher(effEndSetProgram, 0, 0, NULL, 0.0);

   for (const auto &slave : mSlaves)
      slave->callSetChunk(isPgm, len, buf, info);
}

void VSTEffect::RemoveHandler()
{
}

static void OnSize(wxSizeEvent & evt)
{
   evt.Skip();

   // Once the parent dialog reaches its final size as indicated by
   // a non-default minimum size, we set the maximum size to match.
   // This is a bit of a hack to prevent VSTs GUI windows from resizing
   // there's no real reason to allow it.  But, there should be a better
   // way of handling it.
   wxWindow *w = (wxWindow *) evt.GetEventObject();
   wxSize sz = w->GetMinSize();

   if (sz != wxDefaultSize)
   {
      w->SetMaxSize(sz);
   }
}

void VSTEffect::BuildFancy()
{
   // Turn the power on...some effects need this when the editor is open
   PowerOn();

   auto control = Destroy_ptr<VSTControl>{ safenew VSTControl };
   if (!control)
   {
      return;
   }

   if (!control->Create(mParent, this))
   {
      return;
   }

   {
      auto mainSizer = std::make_unique<wxBoxSizer>(wxVERTICAL);

      mainSizer->Add((mControl = control.release()), 0, wxALIGN_CENTER);

      mParent->SetMinSize(wxDefaultSize);
      mParent->SetSizer(mainSizer.release());
   }

   NeedEditIdle(true);

   mDialog->Bind(wxEVT_SIZE, OnSize);

#ifdef __WXMAC__
#ifdef __WX_EVTLOOP_BUSY_WAITING__
   wxEventLoop::SetBusyWaiting(true);
#endif
#endif

   return;
}

void VSTEffect::BuildPlain(EffectSettingsAccess &access)
{
   wxASSERT(mParent); // To justify safenew
   wxScrolledWindow *const scroller = safenew wxScrolledWindow(mParent,
      wxID_ANY,
      wxDefaultPosition,
      wxDefaultSize,
      wxVSCROLL | wxTAB_TRAVERSAL);

   {
      auto mainSizer = std::make_unique<wxBoxSizer>(wxVERTICAL);

      // Try to give the window a sensible default/minimum size
      scroller->SetMinSize(wxSize(wxMax(600, mParent->GetSize().GetWidth() * 2 / 3),
         mParent->GetSize().GetHeight() / 2));
      scroller->SetScrollRate(0, 20);

      // This fools NVDA into not saying "Panel" when the dialog gets focus
      scroller->SetName(wxT("\a"));
      scroller->SetLabel(wxT("\a"));

      mainSizer->Add(scroller, 1, wxEXPAND | wxALL, 5);
      mParent->SetSizer(mainSizer.release());
   }

   mNames.reinit(static_cast<size_t>(mAEffect->numParams));
   mSliders.reinit(static_cast<size_t>(mAEffect->numParams));
   mDisplays.reinit(static_cast<size_t>(mAEffect->numParams));
   mLabels.reinit(static_cast<size_t>(mAEffect->numParams));

   {
      auto paramSizer = std::make_unique<wxStaticBoxSizer>(wxVERTICAL, scroller, _("Effect Settings"));

      {
         auto gridSizer = std::make_unique<wxFlexGridSizer>(4, 0, 0);
         gridSizer->AddGrowableCol(1);

         // Add the duration control for generators
         if (GetType() == EffectTypeGenerate)
         {
            wxControl *item = safenew wxStaticText(scroller, 0, _("Duration:"));
            gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 5);
            auto &extra = access.Get().extra;
            mDuration = safenew
               NumericTextCtrl(scroller, ID_Duration,
                  NumericConverter::TIME,
                  extra.GetDurationFormat(),
                  extra.GetDuration(),
                  mProjectRate,
                  NumericTextCtrl::Options{}
                     .AutoPos(true));
            mDuration->SetName( XO("Duration") );
            gridSizer->Add(mDuration, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);
            gridSizer->Add(1, 1, 0);
            gridSizer->Add(1, 1, 0);
         }

         // Find the longest parameter name.
         int namew = 0;
         int w;
         int h;
         for (int i = 0; i < mAEffect->numParams; i++)
         {
            wxString text = GetString(effGetParamName, i);

            if (text.Right(1) != wxT(':'))
            {
               text += wxT(':');
            }

            scroller->GetTextExtent(text, &w, &h);
            if (w > namew)
            {
               namew = w;
            }
         }

         scroller->GetTextExtent(wxT("HHHHHHHH"), &w, &h);

         for (int i = 0; i < mAEffect->numParams; i++)
         {
            mNames[i] = safenew wxStaticText(scroller,
               wxID_ANY,
               wxEmptyString,
               wxDefaultPosition,
               wxSize(namew, -1),
               wxALIGN_RIGHT | wxST_NO_AUTORESIZE);
            gridSizer->Add(mNames[i], 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 5);

            mSliders[i] = safenew wxSliderWrapper(scroller,
               ID_Sliders + i,
               0,
               0,
               1000,
               wxDefaultPosition,
               wxSize(200, -1));
            gridSizer->Add(mSliders[i], 0, wxALIGN_CENTER_VERTICAL | wxEXPAND | wxALL, 5);
#if wxUSE_ACCESSIBILITY
            // so that name can be set on a standard control
            mSliders[i]->SetAccessible(safenew WindowAccessible(mSliders[i]));
#endif

            mDisplays[i] = safenew wxStaticText(scroller,
               wxID_ANY,
               wxEmptyString,
               wxDefaultPosition,
               wxSize(w, -1),
               wxALIGN_RIGHT | wxST_NO_AUTORESIZE);
            gridSizer->Add(mDisplays[i], 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 5);

            mLabels[i] = safenew wxStaticText(scroller,
               wxID_ANY,
               wxEmptyString,
               wxDefaultPosition,
               wxSize(w, -1),
               wxALIGN_LEFT | wxST_NO_AUTORESIZE);
            gridSizer->Add(mLabels[i], 0, wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 5);
         }

         paramSizer->Add(gridSizer.release(), 1, wxEXPAND | wxALL, 5);
      }
      scroller->SetSizer(paramSizer.release());
   }

   RefreshParameters();

   mSliders[0]->SetFocus();
}

void VSTEffect::RefreshParameters(int skip)
{
   if (!mNames)
   {
      return;
   }

   for (int i = 0; i < mAEffect->numParams; i++)
   {
      wxString text = GetString(effGetParamName, i);

      text = text.Trim(true).Trim(false);

      wxString name = text;

      if (text.Right(1) != wxT(':'))
      {
         text += wxT(':');
      }
      mNames[i]->SetLabel(text);

      // For some parameters types like on/off, setting the slider value has
      // a side effect that causes it to only move when the parameter changes
      // from off to on.  However, this prevents changing the value using the
      // keyboard, so we skip the active slider if any.
      if (i != skip)
      {
         mSliders[i]->SetValue(callGetParameter(i) * 1000);
      }
      name = text;

      text = GetString(effGetParamDisplay, i);
      if (text.empty())
      {
         text.Printf(wxT("%.5g"),callGetParameter(i));
      }
      mDisplays[i]->SetLabel(wxString::Format(wxT("%8s"), text));
      name += wxT(' ') + text;

      text = GetString(effGetParamDisplay, i);
      if (!text.empty())
      {
         text.Printf(wxT("%-8s"), GetString(effGetParamLabel, i));
         mLabels[i]->SetLabel(wxString::Format(wxT("%8s"), text));
         name += wxT(' ') + text;
      }

      mSliders[i]->SetName(name);
   }
}

void VSTEffect::OnSizeWindow(wxCommandEvent & evt)
{
   if (!mControl)
   {
      return;
   }

   mControl->SetMinSize(wxSize(evt.GetInt(), (int) evt.GetExtraLong()));
   mControl->SetSize(wxSize(evt.GetInt(), (int) evt.GetExtraLong()));

   // DO NOT CHANGE THE ORDER OF THESE
   //
   // Guitar Rig (and possibly others) Cocoa VSTs can resize too large
   // if the bounds are unlimited.
   mDialog->SetMinSize(wxDefaultSize);
   mDialog->SetMaxSize(wxDefaultSize);
   mDialog->Layout();
   mDialog->SetMinSize(mDialog->GetBestSize());
   mDialog->SetMaxSize(mDialog->GetBestSize());
   mDialog->Fit();
}

void VSTEffect::OnSlider(wxCommandEvent & evt)
{
   wxSlider *s = (wxSlider *) evt.GetEventObject();
   int i = s->GetId() - ID_Sliders;

   callSetParameter(i, s->GetValue() / 1000.0);

   RefreshParameters(i);
}

bool VSTEffect::LoadFXB(const wxFileName & fn)
{
   bool ret = false;

   // Try to open the file...will be closed automatically when method returns
   wxFFile f(fn.GetFullPath(), wxT("rb"));
   if (!f.IsOpened())
   {
      return false;
   }

   // Allocate memory for the contents
   ArrayOf<unsigned char> data{ size_t(f.Length()) };
   if (!data)
   {
      AudacityMessageBox(
         XO("Unable to allocate memory when loading presets file."),
         XO("Error Loading VST Presets"),
         wxOK | wxCENTRE,
         mParent);
      return false;
   }
   unsigned char *bptr = data.get();

   do
   {
      // Read in the whole file
      ssize_t len = f.Read((void *) bptr, f.Length());
      if (f.Error())
      {
         AudacityMessageBox(
            XO("Unable to read presets file."),
            XO("Error Loading VST Presets"),
            wxOK | wxCENTRE,
            mParent);
         break;
      }

      // Most references to the data are via an "int" array
      int32_t *iptr = (int32_t *) bptr;

      // Verify that we have at least enough for the header
      if (len < 156)
      {
         break;
      }

      // Verify that we probably have an FX file
      if (wxINT32_SWAP_ON_LE(iptr[0]) != CCONST('C', 'c', 'n', 'K'))
      {
         break;
      }

      // Ignore the size...sometimes it's there, other times it's zero

      // Get the version and verify
      int version = wxINT32_SWAP_ON_LE(iptr[3]);
      if (version != 1 && version != 2)
      {
         break;
      }

      VstPatchChunkInfo info =
      {
         1,
         wxINT32_SWAP_ON_LE(iptr[4]),
         wxINT32_SWAP_ON_LE(iptr[5]),
         wxINT32_SWAP_ON_LE(iptr[6]),
         ""
      };

      // Ensure this program looks to belong to the current plugin
      if ((info.pluginUniqueID != mAEffect->uniqueID) &&
          (info.pluginVersion != mAEffect->version) &&
          (info.numElements != mAEffect->numPrograms))
      {
         break;
      }

      // Get the number of programs
      int numProgs = info.numElements;

      // Get the current program index
      int curProg = 0;
      if (version >= 2)
      {
         curProg = wxINT32_SWAP_ON_LE(iptr[7]);
         if (curProg < 0 || curProg >= numProgs)
         {
            break;
         }
      }

      // Is it a bank of programs?
      if (wxINT32_SWAP_ON_LE(iptr[2]) == CCONST('F', 'x', 'B', 'k'))
      {
         // Drop the header
         bptr += 156;
         len -= 156;

         unsigned char *tempPtr = bptr;
         ssize_t tempLen = len;

         // Validate all of the programs
         for (int i = 0; i < numProgs; i++)
         {
            if (!LoadFXProgram(&tempPtr, tempLen, i, true))
            {
               break;
            }
         }

         // Ask the effect if this is an acceptable bank
         if (callDispatcher(effBeginLoadBank, 0, 0, &info, 0.0) == -1)
         {
            return false;
         }

         // Start loading the individual programs
         for (int i = 0; i < numProgs; i++)
         {
            ret = LoadFXProgram(&bptr, len, i, false);
         }
      }
      // Or maybe a bank chunk?
      else if (wxINT32_SWAP_ON_LE(iptr[2]) == CCONST('F', 'B', 'C', 'h'))
      {
         // Can't load programs chunks if the plugin doesn't support it
         if (!(mAEffect->flags & effFlagsProgramChunks))
         {
            break;
         }

         // Verify that we have enough to grab the chunk size
         if (len < 160)
         {
            break;
         }

         // Get the chunk size
         int size = wxINT32_SWAP_ON_LE(iptr[39]);

         // We finally know the full length of the program
         int proglen = 160 + size;

         // Verify that we have enough for the entire program
         if (len < proglen)
         {
            break;
         }

         // Set the entire bank in one shot
         callSetChunk(false, size, &iptr[40], &info);

         // Success
         ret = true;
      }
      // Unrecognizable type
      else
      {
         break;
      }

      // Set the active program
      if (ret && version >= 2)
      {
         callSetProgram(curProg);
      }
   } while (false);

   return ret;
}

bool VSTEffect::LoadFXP(const wxFileName & fn)
{
   bool ret = false;

   // Try to open the file...will be closed automatically when method returns
   wxFFile f(fn.GetFullPath(), wxT("rb"));
   if (!f.IsOpened())
   {
      return false;
   }

   // Allocate memory for the contents
   ArrayOf<unsigned char> data{ size_t(f.Length()) };
   if (!data)
   {
      AudacityMessageBox(
         XO("Unable to allocate memory when loading presets file."),
         XO("Error Loading VST Presets"),
         wxOK | wxCENTRE,
         mParent);
      return false;
   }
   unsigned char *bptr = data.get();

   do
   {
      // Read in the whole file
      ssize_t len = f.Read((void *) bptr, f.Length());
      if (f.Error())
      {
         AudacityMessageBox(
            XO("Unable to read presets file."),
            XO("Error Loading VST Presets"),
            wxOK | wxCENTRE,
            mParent);
         break;
      }

      // Get (or default) currently selected program
      int i = 0; //mProgram->GetCurrentSelection();
      if (i < 0)
      {
         i = 0;   // default to first program
      }

      // Go verify and set the program
      ret = LoadFXProgram(&bptr, len, i, false);
   } while (false);

   return ret;
}

bool VSTEffect::LoadFXProgram(unsigned char **bptr, ssize_t & len, int index, bool dryrun)
{
   // Most references to the data are via an "int" array
   int32_t *iptr = (int32_t *) *bptr;

   // Verify that we have at least enough for a program without parameters
   if (len < 28)
   {
      return false;
   }

   // Verify that we probably have an FX file
   if (wxINT32_SWAP_ON_LE(iptr[0]) != CCONST('C', 'c', 'n', 'K'))
   {
      return false;
   }

   // Ignore the size...sometimes it's there, other times it's zero

   // Get the version and verify
#if defined(IS_THIS_AN_FXP_ARTIFICAL_LIMITATION)
   int version = wxINT32_SWAP_ON_LE(iptr[3]);
   if (version != 1)
   {
      return false;
   }
#endif

   VstPatchChunkInfo info =
   {
      1,
      wxINT32_SWAP_ON_LE(iptr[4]),
      wxINT32_SWAP_ON_LE(iptr[5]),
      wxINT32_SWAP_ON_LE(iptr[6]),
      ""
   };

   // Ensure this program looks to belong to the current plugin
   if ((info.pluginUniqueID != mAEffect->uniqueID) &&
         (info.pluginVersion != mAEffect->version) &&
         (info.numElements != mAEffect->numParams))
   {
      return false;
   }

   // Get the number of parameters
   int numParams = info.numElements;

   // At this point, we have to have enough to include the program name as well
   if (len < 56)
   {
      return false;
   }

   // Get the program name
   wxString progName(wxString::From8BitData((char *)&iptr[7]));

   // Might be a regular program
   if (wxINT32_SWAP_ON_LE(iptr[2]) == CCONST('F', 'x', 'C', 'k'))
   {
      // We finally know the full length of the program
      int proglen = 56 + (numParams * sizeof(float));

      // Verify that we have enough for all of the parameter values
      if (len < proglen)
      {
         return false;
      }

      // Validate all of the parameter values
      for (int i = 0; i < numParams; i++)
      {
         uint32_t ival = wxUINT32_SWAP_ON_LE(iptr[14 + i]);
         float val = reinterpretAsFloat(ival);
         if (val < 0.0 || val > 1.0)
         {
            return false;
         }
      }
         
      // They look okay...time to start changing things
      if (!dryrun)
      {
         // Ask the effect if this is an acceptable program
         if (callDispatcher(effBeginLoadProgram, 0, 0, &info, 0.0) == -1)
         {
            return false;
         }

         // Load all of the parameters
         callDispatcher(effBeginSetProgram, 0, 0, NULL, 0.0);
         for (int i = 0; i < numParams; i++)
         {
            wxUint32 val = wxUINT32_SWAP_ON_LE(iptr[14 + i]);
            callSetParameter(i, reinterpretAsFloat(val));
         }
         callDispatcher(effEndSetProgram, 0, 0, NULL, 0.0);
      }

      // Update in case we're loading an "FxBk" format bank file
      *bptr += proglen;
      len -= proglen;
   }
   // Maybe we have a program chunk
   else if (wxINT32_SWAP_ON_LE(iptr[2]) == CCONST('F', 'P', 'C', 'h'))
   {
      // Can't load programs chunks if the plugin doesn't support it
      if (!(mAEffect->flags & effFlagsProgramChunks))
      {
         return false;
      }

      // Verify that we have enough to grab the chunk size
      if (len < 60)
      {
         return false;
      }

      // Get the chunk size
      int size = wxINT32_SWAP_ON_LE(iptr[14]);

      // We finally know the full length of the program
      int proglen = 60 + size;

      // Verify that we have enough for the entire program
      if (len < proglen)
      {
         return false;
      }

      // Set the entire program in one shot
      if (!dryrun)
      {
         callSetChunk(true, size, &iptr[15], &info);
      }

      // Update in case we're loading an "FxBk" format bank file
      *bptr += proglen;
      len -= proglen;
   }
   else
   {
      // Unknown type
      return false;
   }
   
   if (!dryrun)
   {
      SetString(effSetProgramName, wxString(progName), index);
   }

   return true;
}

bool VSTEffect::LoadXML(const wxFileName & fn)
{
   mInChunk = false;
   mInSet = false;

   // default to read as XML file
   // Load the program
   XMLFileReader reader;
   bool ok = reader.Parse(this, fn.GetFullPath());

   // Something went wrong with the file, clean up
   if (mInSet)
   {
      callDispatcher(effEndSetProgram, 0, 0, NULL, 0.0);

      mInSet = false;
   }

   if (!ok)
   {
      // Inform user of load failure
      AudacityMessageBox(
         reader.GetErrorStr(),
         XO("Error Loading VST Presets"),
         wxOK | wxCENTRE,
         mParent);
      return false;
   }

   return true;
}

void VSTEffect::SaveFXB(const wxFileName & fn) const
{
   // Create/Open the file
   const wxString fullPath{fn.GetFullPath()};
   wxFFile f(fullPath, wxT("wb"));
   if (!f.IsOpened())
   {
      AudacityMessageBox(
         XO("Could not open file: \"%s\"").Format( fullPath ),
         XO("Error Saving VST Presets"),
         wxOK | wxCENTRE,
         mParent);
      return;
   }

   wxMemoryBuffer buf;
   wxInt32 subType;
   void *chunkPtr = nullptr;
   int chunkSize = 0;
   int dataSize = 148;
   wxInt32 tab[8];
   int curProg = 0 ; //mProgram->GetCurrentSelection();

   if (mAEffect->flags & effFlagsProgramChunks)
   {
      subType = CCONST('F', 'B', 'C', 'h');

      // read-only dispatcher function
      chunkSize = constCallDispatcher(effGetChunk, 0, 0, &chunkPtr, 0.0);
      dataSize += 4 + chunkSize;
   }
   else
   {
      subType = CCONST('F', 'x', 'B', 'k');

      for (int i = 0; i < mAEffect->numPrograms; i++)
      {
         SaveFXProgram(buf, i);
      }

      dataSize += buf.GetDataLen();
   }

   tab[0] = wxINT32_SWAP_ON_LE(CCONST('C', 'c', 'n', 'K'));
   tab[1] = wxINT32_SWAP_ON_LE(dataSize);
   tab[2] = wxINT32_SWAP_ON_LE(subType);
   tab[3] = wxINT32_SWAP_ON_LE(curProg >= 0 ? 2 : 1);
   tab[4] = wxINT32_SWAP_ON_LE(mAEffect->uniqueID);
   tab[5] = wxINT32_SWAP_ON_LE(mAEffect->version);
   tab[6] = wxINT32_SWAP_ON_LE(mAEffect->numPrograms);
   tab[7] = wxINT32_SWAP_ON_LE(curProg >= 0 ? curProg : 0);

   f.Write(tab, sizeof(tab));
   if (!f.Error())
   {
      char padding[124];
      memset(padding, 0, sizeof(padding));
      f.Write(padding, sizeof(padding));

      if (!f.Error())
      {
         if (mAEffect->flags & effFlagsProgramChunks)
         {
            wxInt32 size = wxINT32_SWAP_ON_LE(chunkSize);
            f.Write(&size, sizeof(size));
            f.Write(chunkPtr, chunkSize);
         }
         else
         {
            f.Write(buf.GetData(), buf.GetDataLen());
         }
      }
   }

   if (f.Error())
   {
      AudacityMessageBox(
         XO("Error writing to file: \"%s\"").Format( fullPath ),
         XO("Error Saving VST Presets"),
         wxOK | wxCENTRE,
         mParent);
   }

   f.Close();

   return;
}

void VSTEffect::SaveFXP(const wxFileName & fn) const
{
   // Create/Open the file
   const wxString fullPath{ fn.GetFullPath() };
   wxFFile f(fullPath, wxT("wb"));
   if (!f.IsOpened())
   {
      AudacityMessageBox(
         XO("Could not open file: \"%s\"").Format( fullPath ),
         XO("Error Saving VST Presets"),
         wxOK | wxCENTRE,
         mParent);
      return;
   }

   wxMemoryBuffer buf;

   // read-only dispatcher function
   int ndx = constCallDispatcher(effGetProgram, 0, 0, NULL, 0.0);
   SaveFXProgram(buf, ndx);

   f.Write(buf.GetData(), buf.GetDataLen());
   if (f.Error())
   {
      AudacityMessageBox(
         XO("Error writing to file: \"%s\"").Format( fullPath ),
         XO("Error Saving VST Presets"),
         wxOK | wxCENTRE,
         mParent);
   }

   f.Close();

   return;
}

void VSTEffect::SaveFXProgram(wxMemoryBuffer & buf, int index) const
{
   wxInt32 subType;
   void *chunkPtr;
   int chunkSize;
   int dataSize = 48;
   char progName[28];
   wxInt32 tab[7];

   // read-only dispatcher function
   constCallDispatcher(effGetProgramNameIndexed, index, 0, &progName, 0.0);
   progName[27] = '\0';
   chunkSize = strlen(progName);
   memset(&progName[chunkSize], 0, sizeof(progName) - chunkSize);

   if (mAEffect->flags & effFlagsProgramChunks)
   {
      subType = CCONST('F', 'P', 'C', 'h');

      // read-only dispatcher function
      chunkSize = constCallDispatcher(effGetChunk, 1, 0, &chunkPtr, 0.0);
      dataSize += 4 + chunkSize;
   }
   else
   {
      subType = CCONST('F', 'x', 'C', 'k');

      dataSize += (mAEffect->numParams << 2);
   }

   tab[0] = wxINT32_SWAP_ON_LE(CCONST('C', 'c', 'n', 'K'));
   tab[1] = wxINT32_SWAP_ON_LE(dataSize);
   tab[2] = wxINT32_SWAP_ON_LE(subType);
   tab[3] = wxINT32_SWAP_ON_LE(1);
   tab[4] = wxINT32_SWAP_ON_LE(mAEffect->uniqueID);
   tab[5] = wxINT32_SWAP_ON_LE(mAEffect->version);
   tab[6] = wxINT32_SWAP_ON_LE(mAEffect->numParams);

   buf.AppendData(tab, sizeof(tab));
   buf.AppendData(progName, sizeof(progName));

   if (mAEffect->flags & effFlagsProgramChunks)
   {
      wxInt32 size = wxINT32_SWAP_ON_LE(chunkSize);
      buf.AppendData(&size, sizeof(size));
      buf.AppendData(chunkPtr, chunkSize);
   }
   else
   {
      for (int i = 0; i < mAEffect->numParams; i++)
      {
         float val = callGetParameter(i);
         wxUint32 ival = wxUINT32_SWAP_ON_LE(reinterpretAsUint32(val));
         buf.AppendData(&ival, sizeof(ival));
      }
   }

   return;
}

// Throws exceptions rather than giving error return.
void VSTEffect::SaveXML(const wxFileName & fn) const
// may throw
{
   XMLFileWriter xmlFile{ fn.GetFullPath(), XO("Error Saving Effect Presets") };

   xmlFile.StartTag(wxT("vstprogrampersistence"));
   xmlFile.WriteAttr(wxT("version"), wxT("2"));

   xmlFile.StartTag(wxT("effect"));
   // Use internal name only in persistent information
   xmlFile.WriteAttr(wxT("name"), GetSymbol().Internal());
   xmlFile.WriteAttr(wxT("uniqueID"), mAEffect->uniqueID);
   xmlFile.WriteAttr(wxT("version"), mAEffect->version);
   xmlFile.WriteAttr(wxT("numParams"), mAEffect->numParams);

   xmlFile.StartTag(wxT("program"));
   xmlFile.WriteAttr(wxT("name"), wxEmptyString); //mProgram->GetValue());

   int clen = 0;
   if (mAEffect->flags & effFlagsProgramChunks)
   {
      void *chunk = NULL;

      // read-only dispatcher function
      clen = (int) constCallDispatcher(effGetChunk, 1, 0, &chunk, 0.0);
      if (clen != 0)
      {
         xmlFile.StartTag(wxT("chunk"));
         xmlFile.WriteSubTree(Base64::Encode(chunk, clen) + wxT('\n'));
         xmlFile.EndTag(wxT("chunk"));
      }
   }

   if (clen == 0)
   {
      for (int i = 0; i < mAEffect->numParams; i++)
      {
         xmlFile.StartTag(wxT("param"));

         xmlFile.WriteAttr(wxT("index"), i);
         xmlFile.WriteAttr(wxT("name"),
                           GetString(effGetParamName, i));
         xmlFile.WriteAttr(wxT("value"),
                           wxString::Format(wxT("%f"),
                           callGetParameter(i)));

         xmlFile.EndTag(wxT("param"));
      }
   }

   xmlFile.EndTag(wxT("program"));

   xmlFile.EndTag(wxT("effect"));

   xmlFile.EndTag(wxT("vstprogrampersistence"));

   xmlFile.Commit();
}

bool VSTEffect::HandleXMLTag(const std::string_view& tag, const AttributesList &attrs)
{
   if (tag == "vstprogrampersistence")
   {
      for (auto pair : attrs)
      {
         auto attr = pair.first;
         auto value = pair.second;

         if (attr == "version")
         {
            if (!value.TryGet(mXMLVersion))
            {
               return false;
            }

            if (mXMLVersion < 1 || mXMLVersion > 2)
            {
               return false;
            }
         }
         else
         {
            return false;
         }
      }

      return true;
   }

   if (tag == "effect")
   {
      memset(&mXMLInfo, 0, sizeof(mXMLInfo));
      mXMLInfo.version = 1;
      mXMLInfo.pluginUniqueID = mAEffect->uniqueID;
      mXMLInfo.pluginVersion = mAEffect->version;
      mXMLInfo.numElements = mAEffect->numParams;

      for (auto pair : attrs)
      {
         auto attr = pair.first;
         auto value = pair.second;

         if (attr == "name")
         {
            wxString strValue = value.ToWString();

            if (strValue != GetSymbol().Internal())
            {
               auto msg = XO("This parameter file was saved from %s. Continue?")
                  .Format( strValue );
               int result = AudacityMessageBox(
                  msg,
                  XO("Confirm"),
                  wxYES_NO,
                  mParent );
               if (result == wxNO)
               {
                  return false;
               }
            }
         }
         else if (attr == "version")
         {
            long version;
            if (!value.TryGet(version))
            {
               return false;
            }

            mXMLInfo.pluginVersion = (int) version;
         }
         else if (mXMLVersion > 1 && attr == "uniqueID")
         {
            long uniqueID;
            if (!value.TryGet(uniqueID))
            {
               return false;
            }

            mXMLInfo.pluginUniqueID = (int) uniqueID;
         }
         else if (mXMLVersion > 1 && attr == "numParams")
         {
            long numParams;
            if (!value.TryGet(numParams))
            {
               return false;
            }

            mXMLInfo.numElements = (int) numParams;
         }
         else
         {
            return false;
         }
      }

      return true;
   }

   if (tag == "program")
   {
      for (auto pair : attrs)
      {
         auto attr = pair.first;
         auto value = pair.second;

         if (attr == "name")
         {
            const wxString strValue = value.ToWString();

            if (strValue.length() > 24)
            {
               return false;
            }

            int ndx = 0; //mProgram->GetCurrentSelection();
            if (ndx == wxNOT_FOUND)
            {
               ndx = 0;
            }

            SetString(effSetProgramName, strValue, ndx);
         }
         else
         {
            return false;
         }
      }

      mInChunk = false;

      if (callDispatcher(effBeginLoadProgram, 0, 0, &mXMLInfo, 0.0) == -1)
      {
         return false;
      }

      callDispatcher(effBeginSetProgram, 0, 0, NULL, 0.0);

      mInSet = true;

      return true;
   }

   if (tag == "param")
   {
      long ndx = -1;
      double val = -1.0;

      for (auto pair : attrs)
      {
         auto attr = pair.first;
         auto value = pair.second;

         if (attr == "index")
         {
            if (!value.TryGet(ndx))
            {
               return false;
            }

            if (ndx < 0 || ndx >= mAEffect->numParams)
            {
               // Could be a different version of the effect...probably should
               // tell the user
               return false;
            }
         }
         // "name" attribute is ignored for params
         /* else if (attr == "name")
         {

            // Nothing to do with it for now
         }*/
         else if (attr == "value")
         {
            if (!value.TryGet(val))
            {
               return false;
            }

            if (val < 0.0 || val > 1.0)
            {
               return false;
            }
         }
      }

      if (ndx == -1 || val == -1.0)
      {
         return false;
      }

      callSetParameter(ndx, val);

      return true;
   }

   if (tag == "chunk")
   {
      mInChunk = true;
      return true;
   }

   return false;
}

void VSTEffect::HandleXMLEndTag(const std::string_view& tag)
{
   if (tag == "chunk")
   {
      if (mChunk.length())
      {
         ArrayOf<char> buf{ mChunk.length() / 4 * 3 };

         int len = Base64::Decode(mChunk, buf.get());
         if (len)
         {
            callSetChunk(true, len, buf.get(), &mXMLInfo);
         }

         mChunk.clear();
      }
      mInChunk = false;
   }

   if (tag == "program")
   {
      if (mInSet)
      {
         callDispatcher(effEndSetProgram, 0, 0, NULL, 0.0);

         mInSet = false;
      }
   }
}

void VSTEffect::HandleXMLContent(const std::string_view& content)
{
   if (mInChunk)
   {
      mChunk += wxString(std::string(content)).Trim(true).Trim(false);
   }
}

XMLTagHandler *VSTEffect::HandleXMLChild(const std::string_view& tag)
{
   if (tag == "vstprogrampersistence")
   {
      return this;
   }

   if (tag == "effect")
   {
      return this;
   }

   if (tag == "program")
   {
      return this;
   }

   if (tag == "param")
   {
      return this;
   }

   if (tag == "chunk")
   {
      return this;
   }

   return NULL;
}

#endif // USE_VST
