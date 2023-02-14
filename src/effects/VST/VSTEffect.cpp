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

#include <wx/setup.h> // for wxUSE_* macros
#include <wx/dynlib.h>
#include <wx/app.h>
#include <wx/defs.h>
#include <wx/buffer.h>
#include <wx/busyinfo.h>
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
#include "ShuttleGui.h"
#include "valnum.h"
#include "../../widgets/AudacityMessageBox.h"
#include "../../widgets/NumericTextCtrl.h"
#include "XMLFileReader.h"
#include "Base64.h"

#if wxUSE_ACCESSIBILITY
#include "WindowAccessible.h"
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
   return std::make_unique<VSTEffectsModule>();
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

   dpath[0] = 0;
   ExpandEnvironmentStrings(wxT("%COMMONPROGRAMFILES%\\VST2"),
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
         VSTEffect subeffect(wxString::Format("%s;%d", path, id));
         subeffect.Load();
         if(callback)
            callback(this, &subeffect);
      }
      return effectIDs.size();
   }
   errMsg = XO("Could not load the library");
   return 0;
}

std::unique_ptr<ComponentInterface>
VSTEffectsModule::LoadPlugin(const PluginPath & path)
{
   // Acquires a resource for the application.
   // For us, the ID is simply the path to the effect
   auto result = std::make_unique<VSTEffect>(path);
   if (!result->InitializePlugin())
      result.reset();
   return result;
}

bool VSTEffectsModule::CheckPluginExist(const PluginPath& path) const
{
   const auto modulePath = path.BeforeFirst(wxT(';'));
   return wxFileName::FileExists(modulePath) || wxFileName::DirExists(modulePath);
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
   VSTEffectTimer(VSTEffectValidator* pValidator)
   :  wxTimer(),
      mpValidator(pValidator)
   {
   }

   ~VSTEffectTimer()
   {
   }

   void Notify()
   {
      mpValidator->OnTimer();
   }

private:
   VSTEffectValidator* mpValidator;
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

wxDEFINE_EVENT(EVT_SIZEWINDOW, wxCommandEvent);
DEFINE_LOCAL_EVENT_TYPE(EVT_UPDATEDISPLAY);


typedef AEffect *(*vstPluginMain)(audioMasterCallback audioMaster);

intptr_t VSTEffectWrapper::AudioMaster(AEffect * effect,
                                       int32_t opcode,
                                       int32_t index,
                                       intptr_t value,
                                       void * ptr,
                                       float opt)
{
   VSTEffectWrapper* vst = (effect ? static_cast<VSTEffectWrapper*>(effect->ptr2) : nullptr);

   // Handles operations during initialization...before VSTEffect has had a
   // chance to set its instance pointer.
   switch (opcode)
   {
      case audioMasterVersion:
         return (intptr_t) 2400;

      case audioMasterCurrentId:
         return vst->mCurrentEffectID;

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

VSTEffect::VSTEffect(const PluginPath & path)
:  VSTEffectWrapper(path)
{
   memset(&mTimeInfo, 0, sizeof(mTimeInfo));
   mTimeInfo.samplePos = 0.0;
   mTimeInfo.sampleRate = 44100.0;  // this is a bogus value, but it's only for the display
   mTimeInfo.nanoSeconds = wxGetUTCTimeMillis().ToDouble();
   mTimeInfo.tempo = 120.0;
   mTimeInfo.timeSigNumerator = 4;
   mTimeInfo.timeSigDenominator = 4;
   mTimeInfo.flags = kVstTempoValid | kVstNanosValid;
}

VSTEffect::~VSTEffect()
{
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
   return VSTEffectWrapper::GetSymbol();
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
   if (mAudioIns == 0 && mAudioOuts == 0)
   {
      return EffectTypeTool;
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
   return RealtimeSince::Always;

   /* return GetType() == EffectTypeProcess
      ? RealtimeSince::Always
      : RealtimeSince::Never; */
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

   return true;
}


namespace
{
   struct VSTEffectMessage : EffectInstance::Message
   {
      using ParamVector = std::vector<std::optional<double> >;

      // Make a message from a chunk and ID-value pairs
      explicit VSTEffectMessage(std::vector<char> chunk, ParamVector params)
         : mChunk(std::move(chunk)),
           mParamsVec(std::move(params))
      {
      }

      // Make a message from a single parameter
      explicit VSTEffectMessage(int id, double value, size_t numParams)
      {
         mParamsVec.resize(numParams, std::nullopt);
         if (id < numParams)
            mParamsVec[id] = value;
      }

      ~VSTEffectMessage() override;

      std::unique_ptr<Message> Clone() const override;
      void Assign(Message&& src) override;
      void Merge(Message&& src) override;


      std::vector<char> mChunk;
      ParamVector       mParamsVec;

   };
}


VSTEffectMessage::~VSTEffectMessage() = default;

auto VSTEffectMessage::Clone() const -> std::unique_ptr<Message>
{
   auto result = std::make_unique<VSTEffectMessage>(*this);
   // Make sure of the chunk capacity
   result->mChunk.reserve(this->mChunk.capacity());

   return result;
}

void VSTEffectMessage::Assign(Message && src)
{
   VSTEffectMessage& vstSrc = static_cast<VSTEffectMessage&>(src);

   mChunk = vstSrc.mChunk;
   vstSrc.mChunk.resize(0);     // capacity will be preserved though

   assert(mParamsVec.size() == vstSrc.mParamsVec.size());

   for (size_t i = 0; i < mParamsVec.size(); i++)
   {
      mParamsVec[i] = vstSrc.mParamsVec[i];

      // consume the source value
      vstSrc.mParamsVec[i] = std::nullopt;
   }
}

void VSTEffectMessage::Merge(Message && src)
{
   VSTEffectMessage& vstSrc = static_cast<VSTEffectMessage&>(src);

   bool chunkWasAssigned = false;

   if ( ! vstSrc.mChunk.empty() )
   {
      mChunk = vstSrc.mChunk;
      chunkWasAssigned = true;
   }

   vstSrc.mChunk.resize(0);  // capacity will be preserved though

   assert(mParamsVec.size() == vstSrc.mParamsVec.size());

   for (size_t i = 0; i < mParamsVec.size(); i++)
   {
      if (chunkWasAssigned)
      {
         mParamsVec[i] = vstSrc.mParamsVec[i];
      }
      else
      {
         // if src val is nullopt, do not copy it to dest
         if (vstSrc.mParamsVec[i] != std::nullopt)
         {
            mParamsVec[i] = vstSrc.mParamsVec[i];
         }
      }

      // consume the source value
      vstSrc.mParamsVec[i] = std::nullopt;
   }

}


std::unique_ptr<EffectInstance::Message> VSTEffectInstance::MakeMessage() const
{
   // The purpose here is just to allocate vectors (chunk and paramVector)
   // with sufficient size, not to get the values too
   VSTEffectSettings settings;
   FetchSettings(settings, /* doFetch = */ false);

   VSTEffectMessage::ParamVector paramVector;
   paramVector.resize(mAEffect->numParams, std::nullopt);

   return std::make_unique<VSTEffectMessage>( std::move(settings.mChunk), std::move(paramVector) );
}


std::unique_ptr<EffectInstance::Message> VSTEffectInstance::MakeMessage(int id, double value) const
{
   return std::make_unique<VSTEffectMessage>(id, value, mAEffect->numParams);
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
   size_t userBlockSizeC = std::max( 1, userBlockSize );
   GetConfig(*this, PluginSettings::Shared, wxT("Options"),
      wxT("UseLatency"), mUseLatency, true);
  
 
   return std::make_shared<VSTEffectInstance>(*this, mPath, userBlockSizeC, userBlockSizeC, mUseLatency);
}

unsigned VSTEffectInstance::GetAudioInCount() const
{
   return mAudioIns;
}

unsigned VSTEffectInstance::GetAudioOutCount() const
{
   return mAudioOuts;
}

size_t VSTEffectInstance::SetBlockSize(size_t maxBlockSize)
{
   // Issue 3935 for IEM plug-ins, VST 2 versions:
   // It is mysterious why this further limitation of size works to
   // prevent the crashes in destructive processing, or why this is not
   // needed for non-destructive, but here it is
   // Those plugins report many channels (like 64) but most others will not
   // be affected by these lines with the default size of 8192
   // Note it may make the Block Size option of the settings dialog misleading
   auto numChannels = std::max({ 1u, GetAudioInCount(), GetAudioOutCount() });
   maxBlockSize = std::max(size_t(1),
      std::min(maxBlockSize, size_t(0x8000u / numChannels)));

   mBlockSize = std::min( maxBlockSize, mUserBlockSize );
   return mBlockSize;
}

size_t VSTEffectInstance::GetBlockSize() const
{
   return mBlockSize;
}

auto VSTEffectInstance::GetLatency(
   const EffectSettings& settings, double sampleRate) const -> SampleCount
{
   if (mUseLatency)
      return mBufferDelay;
   return 0;
}

bool VSTEffectInstance::IsReady()
{
   return mReady;
}

bool VSTEffectInstance::ProcessInitialize(
   EffectSettings& settings, double sampleRate, ChannelNames)
{
   // Issue 3942: Copy the contents of settings first.
   // settings may refer to what is in the RealtimeEffectState, but that might
   // get reassigned by EffectSettingsAccess::Set, when the validator's
   // Automate() is called-back by the plug-in during callSetParameter.
   // So this avoids a dangling reference.
   auto copiedSettings = GetSettings(settings);
   StoreSettings(copiedSettings);

   return DoProcessInitialize(sampleRate);
}

bool VSTEffectInstance::DoProcessInitialize(double sampleRate)
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


bool VSTEffectInstance::ProcessFinalize() noexcept
{
   return GuardedCall<bool>([&] {
      mReady = false;

      PowerOff();

      return true;
   });

}


size_t VSTEffectInstance::ProcessBlock(EffectSettings &,
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


bool VSTEffectInstance::RealtimeInitialize(EffectSettings &settings, double sampleRate)
{
   // Temporarily disconnect from any validator, so that setting the chunk
   // does not cause Automate() callbacks (as some effects will do) that then
   // would send slider movement messages that might destroy information in
   // the settings.
   auto vr = valueRestorer(mpOwningValidator, (VSTEffectUIWrapper*)nullptr);
   return ProcessInitialize(settings, sampleRate, {});
}

bool VSTEffectInstance::RealtimeAddProcessor(EffectSettings &settings,
   EffectOutputs *, unsigned numChannels, float sampleRate)
{
   if (!mRecruited)
   {
      // Assign self to the first processor
      mRecruited = true;
      return true;
   }

   auto &effect = static_cast<const PerTrackEffect &>(mProcessor);
   auto slave = std::make_unique<VSTEffectInstance>(
      const_cast<PerTrackEffect &>(effect),
      mPath, mBlockSize, mUserBlockSize, mUseLatency);

   slave->SetBlockSize(mBlockSize);

   if (!slave->ProcessInitialize(settings, sampleRate, ChannelNames()))
      return false;

   mSlaves.emplace_back(move(slave));
   return true;
}

bool VSTEffectInstance::RealtimeFinalize(EffectSettings&) noexcept
{
return GuardedCall<bool>([&]{

   if (mpOwningValidator)
      mpOwningValidator->Flush();

   mRecruited = false;

   for (const auto &slave : mSlaves)
      slave->ProcessFinalize();
   mSlaves.clear();

   return ProcessFinalize();
});
}

bool VSTEffectInstance::RealtimeSuspend()
{
   PowerOff();

   for (const auto &slave : mSlaves)
      slave->PowerOff();

   return true;
}

bool VSTEffectInstance::RealtimeResume()
{
   PowerOn();

   for (const auto &slave : mSlaves)
      slave->PowerOn();

   return true;
}


bool VSTEffectInstance::OnePresetWasLoadedWhilePlaying()
{
   return mPresetLoadedWhilePlaying.exchange(false);
}

void VSTEffectInstance::DeferChunkApplication()
{
   std::lock_guard<std::mutex> guard(mDeferredChunkMutex);

   if (! mChunkToSetAtIdleTime.empty() )
   {    
      ApplyChunk(mChunkToSetAtIdleTime);
      mChunkToSetAtIdleTime.resize(0);
   }
}


void VSTEffectInstance::ApplyChunk(std::vector<char>& chunk)
{
   VstPatchChunkInfo info = {
      1, mAEffect->uniqueID, mAEffect->version, mAEffect->numParams, "" };

   const auto len = chunk.size();
   const auto data = chunk.data();

   callSetChunk(true, len, data, &info);
   for (auto& slave : mSlaves)
      slave->callSetChunk(true, len, data, &info);
}


bool VSTEffectInstance::ChunkMustBeAppliedInMainThread() const
{
   // Some plugins (e.g. Melda) can not have their chunk set in the
   // audio thread, resulting in making the whole app hang.
   // This is why we defer the setting of the chunk in the main thread.

   const bool IsAudioThread = (mMainThreadId != std::this_thread::get_id());
   
   return IsAudioThread && mIsMeldaPlugin;
}


bool VSTEffectInstance::UsesMessages() const noexcept
{
   return true;
}

bool VSTEffectInstance::RealtimeProcessStart(MessagePackage& package)
{
   const bool applyChunkInMainThread = ChunkMustBeAppliedInMainThread();

   if (applyChunkInMainThread)
      mDeferredChunkMutex.lock();

   if (!package.pMessage)
      return true;

   auto& message = static_cast<VSTEffectMessage&>(*package.pMessage);

   auto &chunk = message.mChunk;

   if (!chunk.empty())
   {
      if (applyChunkInMainThread)
      {
         // Apply the chunk later
         //
         mChunkToSetAtIdleTime = chunk;
      }
      else
      {
         // Apply the chunk now
         ApplyChunk(chunk);
      }

      // Don't apply the chunk again until another message supplies a chunk
      chunk.resize(0);

      // Don't return yet.  Maybe some slider movements also accumulated after
      // the change of the chunk.

      const bool IsAudioThread = (mMainThreadId != std::this_thread::get_id());
      if (IsAudioThread)
      {
         // At the moment, the only reason why this method would be called in the audio thread,
         // is because a preset was loaded while playing

         mPresetLoadedWhilePlaying.store(true);
      }

   }


   assert(message.mParamsVec.size() == mAEffect->numParams);

   for (size_t paramID=0; paramID < mAEffect->numParams; paramID++)
   {
      if (message.mParamsVec[paramID])
      {
         float val = (float)(*message.mParamsVec[paramID]);

         // set the change on the recruited "this" instance
         callSetParameter(paramID, val);

         // set the change on any existing slaves
         for (auto& slave : mSlaves)
         {
            slave->callSetParameter(paramID, val);
         }

         // clear the used info
         message.mParamsVec[paramID] = std::nullopt;
      }
   }

   return true;
}

size_t VSTEffectInstance::RealtimeProcess(size_t group, EffectSettings &settings,
   const float *const *inbuf, float *const *outbuf, size_t numSamples)
{
   if (!mRecruited)
   {
      // unexpected!
      return 0;
   }

   wxASSERT(numSamples <= mBlockSize);

   if (group == 0)
   {
      // use the recruited "this" instance
      return ProcessBlock(settings, inbuf, outbuf, numSamples);
   }
   else if (group <= mSlaves.size())
   {
      // use the slave which maps to the group
      return mSlaves[group - 1]->ProcessBlock(settings, inbuf, outbuf, numSamples);
   }
   else
      return 0;
}

bool VSTEffectInstance::RealtimeProcessEnd(EffectSettings &) noexcept
{
   if ( ChunkMustBeAppliedInMainThread() )
      mDeferredChunkMutex.unlock();

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
   wxWindow &parent, wxDialog &dialog, EffectUIValidator* validator, bool forceModal)
{
   //   mProcessLevel = 1;      // in GUI thread

   VSTEffectValidator* vstValidator = static_cast<VSTEffectValidator*>(validator);

   return vstValidator->ShowDialog(/* nonModal = */ SupportsRealtime() && !forceModal);
}

int VSTEffectValidator::ShowDialog(bool nonModal)
{
   mDialog->CentreOnParent();

   if (nonModal)
   {
      mDialog->Show();
      return 0;
   }

   return mDialog->ShowModal();
}

bool VSTEffectValidator::IsGraphicalUI()
{
   return mEffect.IsGraphicalUI();
}

bool VSTEffect::SaveSettings(const EffectSettings& settings, CommandParameters& parms) const
{
   const VSTEffectSettings& vstSettings = GetSettings(settings);

   for (const auto& item : vstSettings.mParamsMap)
   {
      if (item.second)
      {
         const auto& name  =   item.first;
         const auto& value = *(item.second);

         if (!parms.Write(name, value))
         {
            return false;
         }
      }
   }

   return true;
}


bool VSTEffect::LoadSettings(const CommandParameters& parms, EffectSettings& settings) const
{
   VSTEffectSettings& vstSettings = GetSettings(settings);

   long index{};
   wxString key;
   double value = 0.0;
   if (parms.GetFirstEntry(key, index))
   {
      do
      {
         if (parms.Read(key, &value)) {
            auto &map = vstSettings.mParamsMap;
            auto iter = map.find(key);
            if (iter != map.end()) {
               if (iter->second)
                  // Should be guaranteed by MakeSettings
                  iter->second = value;
               else {
                  assert(false);
               }
            }
            else
               // Unknown parameter name in the file
               return false;
         }
      } while (parms.GetNextEntry(key, index));
   }

   // Loads key-value pairs only from a config file -- no chunk
   vstSettings.mChunk.resize(0);
   vstSettings.mVersion   = VSTEffectWrapper::mVersion;
   vstSettings.mUniqueID  = VSTEffectWrapper::mAEffect->uniqueID;
   vstSettings.mNumParams = VSTEffectWrapper::mAEffect->numParams;

   return true;
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

OptionalMessage
VSTEffect::LoadFactoryPreset(int id, EffectSettings& settings) const
{
   // To do: externalize state so const_cast isn't needed
   bool loadOK = const_cast<VSTEffect*>(this)->DoLoadFactoryPreset(id) &&
      FetchSettings(GetSettings(settings));
   if (!loadOK)
      return {};
   return MakeMessageFS(
      VSTEffectInstance::GetSettings(settings));
}

bool VSTEffect::DoLoadFactoryPreset(int id)
{
   callSetProgram(id);

   return true;
}

// ============================================================================
// EffectUIClientInterface implementation
// ============================================================================

std::unique_ptr<EffectUIValidator> VSTEffect::PopulateUI(ShuttleGui &S,
   EffectInstance& instance, EffectSettingsAccess &access,
   const EffectOutputs *)
{
   auto parent = S.GetParent();

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

   auto pParent = S.GetParent();

   auto& vst2Instance = dynamic_cast<VSTEffectInstance&>(instance);

   auto validator = std::make_unique<VSTEffectValidator>(
      vst2Instance, *this, access, pParent, mAEffect->numParams);

   // Also let the instance know about the validator, so it can forward
   // to it calls coming from the vst callback
   vst2Instance.SetOwningValidator(validator.get());


   // Build the appropriate dialog type
   if (mGui)
   {
      validator->BuildFancy(instance);
   }
   else
   {
      validator->BuildPlain(access, GetType(), mProjectRate);
   }


   return validator;
}

bool VSTEffect::IsGraphicalUI()
{
   return mGui;
}



bool VSTEffect::CloseUI()
{
   return true;
}

bool VSTEffect::CanExportPresets()
{
   return true;
}

// Throws exceptions rather than reporting errors.
void VSTEffect::ExportPresets(const EffectSettings& settings) const
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

   if ( ! StoreSettings(GetSettings(settings)) )
      return;

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
         nullptr);

      return;
   }
}

//
// Load an "fxb", "fxp" or Audacuty "xml" file
//
// Based on work by Sven Giermann
//
OptionalMessage VSTEffect::ImportPresets(EffectSettings& settings)
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
      nullptr);

   // User canceled...
   if (path.empty())
   {
      return {};
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
         nullptr);

      return {};
   }

   if (!success)
   {
      AudacityMessageBox(
         XO("Unable to load presets file."),
         XO("Error Loading VST Presets"),
         wxOK | wxCENTRE,
         nullptr);

      return {};
   }

   if (!FetchSettings(GetSettings(settings)))
      return {};

   return MakeMessageFS(
      VSTEffectInstance::GetSettings(settings));
}

bool VSTEffect::HasOptions()
{
   return true;
}

void VSTEffect::ShowOptions()
{
   VSTEffectOptionsDialog dlg(nullptr, *this);
   if (dlg.ShowModal())
   {
      // Reinitialize configuration settings
      
      GetConfig(*this, PluginSettings::Shared, wxT("Options"),
         wxT("UseLatency"), mUseLatency, true);
   }
}

// ============================================================================
// VSTEffect implementation
// ============================================================================

bool VSTEffectWrapper::Load()
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
      mAEffect = pluginMain(VSTEffectWrapper::AudioMaster);
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
      mAEffect->ptr2 = static_cast<VSTEffectWrapper*>(this);

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
      ResetModuleAndHandle();
   }

   return success;
}


void VSTEffectWrapper::Unload()
{
   if (mAEffect)
   {
      // Finally, close the plugin
      callDispatcher(effClose, 0, 0, NULL, 0.0);
      mAEffect = NULL;
   }

   //ResetModuleAndHandle();
}


void VSTEffectWrapper::ResetModuleAndHandle()
{
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


VSTEffectWrapper::~VSTEffectWrapper()
{
   Unload();
   ResetModuleAndHandle();
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


VstPatchChunkInfo VSTEffectWrapper::GetChunkInfo() const
{
   VstPatchChunkInfo info = { 1, mAEffect->uniqueID, mAEffect->version, mAEffect->numParams, "" };
   return info;
}

bool VSTEffectWrapper::IsCompatible(const VstPatchChunkInfo& info) const
{
   return  (info.pluginUniqueID == mAEffect->uniqueID) &&
           (info.pluginVersion  == mAEffect->version) &&
           (info.numElements    == mAEffect->numParams);
}

OptionalMessage VSTEffect::LoadUserPreset(
   const RegistryPath & group, EffectSettings &settings) const
{
   wxString value;

   auto info = GetChunkInfo();

   GetConfig(*this, PluginSettings::Private, group, wxT("UniqueID"),
      info.pluginUniqueID, info.pluginUniqueID);
   GetConfig(*this, PluginSettings::Private, group, wxT("Version"),
      info.pluginVersion, info.pluginVersion);
   GetConfig(*this, PluginSettings::Private, group, wxT("Elements"),
      info.numElements, info.numElements);

   if ( ! IsCompatible(info) )
   {
      return {};
   }

   if (GetConfig(*this,
      PluginSettings::Private, group, wxT("Chunk"), value, wxEmptyString))
   {
      ArrayOf<char> buf{ value.length() / 4 * 3 };

      int len = Base64::Decode(value, buf.get());
      if (len)
      {
         callSetChunk(true, len, buf.get(), &info);
         if (!FetchSettings(GetSettings(settings)))
            return {};
      }

      return MakeMessageFS(
         VSTEffectInstance::GetSettings(settings));
   }

   wxString parms;
   if (!GetConfig(*this,
      PluginSettings::Private, group, wxT("Parameters"), parms, wxEmptyString))
   {
      return {};
   }

   CommandParameters eap;
   if (!eap.SetParameters(parms))
   {
      return {};
   }

   const bool loadOK = LoadSettings(eap, settings) &&
      FetchSettings(GetSettings(settings));
   if (!loadOK)
      return {};

   return MakeMessageFS(
      VSTEffectInstance::GetSettings(settings));
}


bool VSTEffect::SaveUserPreset(
   const RegistryPath & group, const EffectSettings &settings) const
{
   const auto& vstSettings = GetSettings(settings);

   if ( ! StoreSettings(vstSettings) )
      return false;

   SetConfig(*this, PluginSettings::Private, group, wxT("UniqueID"), vstSettings.mUniqueID );
   SetConfig(*this, PluginSettings::Private, group, wxT("Version"),  vstSettings.mVersion  );
   SetConfig(*this, PluginSettings::Private, group, wxT("Elements"), vstSettings.mNumParams);

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

void VSTEffectUIWrapper::Flush()
{}

void VSTEffectValidator::Flush()
{
   mAccess.Flush();
}

void VSTEffectValidator::OnTimer()
{
   wxRecursionGuard guard(mTimerGuard);

   // Ignore it if we're recursing
   if (guard.IsInside())
   {
      return;
   }

   if (GetInstance().mVstVersion >= 2 && mWantsIdle)
   {
      int ret = GetInstance().callDispatcher(effIdle, 0, 0, NULL, 0.0);
      if (!ret)
      {
         mWantsIdle = false;
      }
   }

   if (mWantsEditIdle)
   {
      GetInstance().callDispatcher(effEditIdle, 0, 0, NULL, 0.0);
   }
}

void VSTEffectUIWrapper::NeedIdle()
{   
}

void VSTEffectInstance::NeedIdle()
{
   if (mpOwningValidator)
   {
      mpOwningValidator->NeedIdle();
   }
}

void VSTEffectValidator::NeedIdle()
{
   mWantsIdle = true;
   mTimer->Start(100);
}

void VSTEffectValidator::NeedEditIdle(bool state)
{
   mWantsEditIdle = state;
   mTimer->Start(100);
}



VstTimeInfo* VSTEffectWrapper::GetTimeInfo()
{
   mTimeInfo.nanoSeconds = wxGetUTCTimeMillis().ToDouble();
   return &mTimeInfo;
}

float VSTEffectWrapper::GetSampleRate()
{
   return mTimeInfo.sampleRate;
}

int VSTEffectWrapper::GetProcessLevel()
{
   return mProcessLevel;
}

void VSTEffectInstance::PowerOn()
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

void VSTEffectInstance::PowerOff()
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

void VSTEffectUIWrapper::SizeWindow(int w, int h)
{
}

void VSTEffectInstance::SizeWindow(int w, int h)
{
   if (mpOwningValidator)
   {
      mpOwningValidator->SizeWindow(w, h);
   }
}

void VSTEffectValidator::NotifyParameterChanged(int index, float value)
{
   const auto& settings = VSTEffectWrapper::GetSettings(mAccess.Get());

   GetInstance().ForEachParameter(
      [index, value, &settings, this](const auto& pi)
      {
         if (pi.mID != index)
            return true;

         auto it = settings.mParamsMap.find(pi.mName);

         // For consistency with other plugin families
         constexpr float epsilon = 1.0e-5f; 

         if (
            it == settings.mParamsMap.end() || !it->second.has_value() ||
            std::abs(*it->second - value) > epsilon)
            Publish(EffectSettingChanged { size_t(index), value });

         return false;
      });
}

void VSTEffectValidator::OnIdle(wxIdleEvent& evt)
{
   evt.Skip();
   if (!mLastMovements.empty()) {
      // Be sure the instance has got any messages
      mAccess.Flush();
      mAccess.ModifySettings([&](EffectSettings& settings) {
         // Update settings, for stickiness
         // But don't do a complete FetchSettingsFromInstance
         for (auto [index, value] : mLastMovements) {
            if (index >= 0 && index < mParamNames.size()) {
               const auto &string = mParamNames[index];
               auto &mySettings = VSTEffectWrapper::GetSettings(settings);
               mySettings.mParamsMap[string] = value;
            }
         }
         // Succeed but with a null message
         return nullptr;
      });
      for (auto [index, _] : mLastMovements)
         RefreshParameters(index);
      mLastMovements.clear();
   }

   GetInstance().DeferChunkApplication();

   if ( GetInstance().OnePresetWasLoadedWhilePlaying() )
   {
      RefreshParameters();
   }

}

void VSTEffectValidator::SizeWindow(int w, int h)
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


void VSTEffectWrapper::SetBufferDelay(int samples)
{
   // We do not support negative delay
   if (samples >= 0 && mUseLatency)
   {
      mBufferDelay = samples;
   }

   return;
}

int VSTEffectWrapper::GetString(wxString & outstr, int opcode, int index) const
{
   char buf[256];

   memset(buf, 0, sizeof(buf));

   // Assume we are passed a read-only dispatcher function code
   constCallDispatcher(opcode, index, 0, buf, 0.0);

   outstr = wxString::FromUTF8(buf);

   return 0;
}

wxString VSTEffectWrapper::GetString(int opcode, int index) const
{
   wxString str;

   GetString(str, opcode, index);

   return str;
}

void VSTEffectWrapper::SetString(int opcode, const wxString & str, int index)
{
   char buf[256];
   strcpy(buf, str.Left(255).ToUTF8());

   callDispatcher(opcode, index, 0, buf, 0.0);
}

intptr_t VSTEffectWrapper::callDispatcher(int opcode,
                                   int index, intptr_t value, void *ptr, float opt)
{
   // Needed since we might be in the dispatcher when the timer pops
   std::lock_guard guard(mDispatcherLock);

   return mAEffect->dispatcher(mAEffect, opcode, index, value, ptr, opt);
}

intptr_t VSTEffectWrapper::constCallDispatcher(int opcode,
   int index, intptr_t value, void *ptr, float opt) const
{
   // Assume we are passed a read-only dispatcher function code
   return const_cast<VSTEffectWrapper*>(this)
      ->callDispatcher(opcode, index, value, ptr, opt);
}

void VSTEffectInstance::callProcessReplacing(const float *const *inputs,
   float *const *outputs, int sampleframes)
{
   mAEffect->processReplacing(mAEffect,
      const_cast<float**>(inputs),
      const_cast<float**>(outputs), sampleframes);
}

float VSTEffectWrapper::callGetParameter(int index) const
{
   return mAEffect->getParameter(mAEffect, index);
}



void VSTEffectWrapper::callSetParameter(int index, float value) const
{
   if (mVstVersion == 0 || constCallDispatcher(effCanBeAutomated, 0, index, NULL, 0.0))
   {
      mAEffect->setParameter(mAEffect, index, value);
   }
}


void VSTEffectWrapper::callSetProgram(int index)
{
   callDispatcher(effBeginSetProgram, 0, 0, NULL, 0.0);

   callDispatcher(effSetProgram, 0, index, NULL, 0.0);

   callDispatcher(effEndSetProgram, 0, 0, NULL, 0.0);
}


void VSTEffectWrapper::callSetChunk(bool isPgm, int len, void *buf)
{
   VstPatchChunkInfo info;

   memset(&info, 0, sizeof(info));
   info.version = 1;
   info.pluginUniqueID = mAEffect->uniqueID;
   info.pluginVersion = mAEffect->version;
   info.numElements = isPgm ? mAEffect->numParams : mAEffect->numPrograms;

   callSetChunk(isPgm, len, buf, &info);
}

void VSTEffectWrapper::callSetChunk(bool isPgm, int len, void *buf, VstPatchChunkInfo *info) const
{
   if (isPgm)
   {
      // Ask the effect if this is an acceptable program
      if (constCallDispatcher(effBeginLoadProgram, 0, 0, info, 0.0) == -1)
      {
         return;
      }
   }
   else
   {
      // Ask the effect if this is an acceptable bank
      if (constCallDispatcher(effBeginLoadBank, 0, 0, info, 0.0) == -1)
      {
         return;
      }
   }

   constCallDispatcher(effBeginSetProgram, 0, 0, NULL, 0.0);
   constCallDispatcher(effSetChunk, isPgm ? 1 : 0, len, buf, 0.0);
   constCallDispatcher(effEndSetProgram, 0, 0, NULL, 0.0);
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

void VSTEffectValidator::BuildFancy(EffectInstance& instance)
{
   auto& vstEffInstance = dynamic_cast<VSTEffectInstance&>(instance);

   // Turn the power on...some effects need this when the editor is open
   vstEffInstance.PowerOn();

   auto control = Destroy_ptr<VSTControl>{ safenew VSTControl };
   if (!control)
   {
      return;
   }

   if (!control->Create(mParent, &vstEffInstance))
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
   
   
   BindTo(*mDialog, EVT_SIZEWINDOW, &VSTEffectValidator::OnSizeWindow);

#ifdef __WXMAC__
#ifdef __WX_EVTLOOP_BUSY_WAITING__
   wxEventLoop::SetBusyWaiting(true);
#endif
#endif

   return;
}

void VSTEffectValidator::BuildPlain(EffectSettingsAccess &access, EffectType effectType, double projectRate)
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

   mNames.reinit(static_cast<size_t>   (mNumParams));
   mSliders.reinit(static_cast<size_t> (mNumParams));
   mDisplays.reinit(static_cast<size_t>(mNumParams));
   mLabels.reinit(static_cast<size_t>  (mNumParams));

   {
      auto paramSizer = std::make_unique<wxStaticBoxSizer>(wxVERTICAL, scroller, _("Effect Settings"));

      {
         auto gridSizer = std::make_unique<wxFlexGridSizer>(4, 0, 0);
         gridSizer->AddGrowableCol(1);

         // Add the duration control for generators
         if (effectType == EffectTypeGenerate)
         {
            wxControl *item = safenew wxStaticText(scroller, 0, _("Duration:"));
            gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 5);
            auto &extra = access.Get().extra;
            mDuration = safenew
               NumericTextCtrl(scroller, ID_Duration,
                  NumericConverter::TIME,
                  extra.GetDurationFormat(),
                  extra.GetDuration(),
                  projectRate,
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
         for (int i = 0; i < mNumParams; i++)
         {
            wxString text = GetInstance().GetString(effGetParamName, i);

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

         for (int i = 0; i < mNumParams; i++)
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

            // Bind the slider to ::OnSlider
            BindTo(*mSliders[i], wxEVT_COMMAND_SLIDER_UPDATED, &VSTEffectValidator::OnSlider);

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

void VSTEffectValidator::RefreshParameters(int skip) const
{
   if (!mNames)
   {
      return;
   }

   for (int i = 0; i < mNumParams; i++)
   {
      wxString text = GetInstance().GetString(effGetParamName, i);

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
         mSliders[i]->SetValue(GetInstance().callGetParameter(i) * 1000);
      }
      name = text;

      text = GetInstance().GetString(effGetParamDisplay, i);
      if (text.empty())
      {
         text.Printf(wxT("%.5g"), GetInstance().callGetParameter(i));
      }
      mDisplays[i]->SetLabel(wxString::Format(wxT("%8s"), text));
      name += wxT(' ') + text;

      text = GetInstance().GetString(effGetParamDisplay, i);
      if (!text.empty())
      {
         text.Printf(wxT("%-8s"), GetInstance().GetString(effGetParamLabel, i));
         mLabels[i]->SetLabel(wxString::Format(wxT("%8s"), text));
         name += wxT(' ') + text;
      }

      mSliders[i]->SetName(name);
   }
}

void VSTEffectValidator::OnSizeWindow(wxCommandEvent & evt)
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

void VSTEffectValidator::OnSlider(wxCommandEvent & evt)
{
   wxSlider *s = (wxSlider *) evt.GetEventObject();
   int i = s->GetId() - ID_Sliders;
   float value = s->GetValue() / 1000.0;

   NotifyParameterChanged(i, value);
   // Send changed settings (only) to the worker thread
   mAccess.Set(GetInstance().MakeMessage(i, value));
   mLastMovements.emplace_back(i, value);
}

bool VSTEffectWrapper::LoadFXB(const wxFileName & fn)
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
         nullptr);
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
            nullptr);
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

bool VSTEffectWrapper::LoadFXP(const wxFileName & fn)
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
         nullptr);
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
            nullptr);
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

bool VSTEffectWrapper::LoadFXProgram(unsigned char **bptr, ssize_t & len, int index, bool dryrun)
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

bool VSTEffectWrapper::LoadXML(const wxFileName & fn)
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
         nullptr);
      return false;
   }

   return true;
}

void VSTEffectWrapper::SaveFXB(const wxFileName & fn) const
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
         nullptr);
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
         nullptr);
   }

   f.Close();

   return;
}

void VSTEffectWrapper::SaveFXP(const wxFileName & fn) const
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
         nullptr);
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
         nullptr);
   }

   f.Close();

   return;
}

void VSTEffectWrapper::SaveFXProgram(wxMemoryBuffer & buf, int index) const
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
void VSTEffectWrapper::SaveXML(const wxFileName & fn) const
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

bool VSTEffectWrapper::HandleXMLTag(const std::string_view& tag, const AttributesList &attrs)
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
                  nullptr );
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

void VSTEffectWrapper::HandleXMLEndTag(const std::string_view& tag)
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

void VSTEffectWrapper::HandleXMLContent(const std::string_view& content)
{
   if (mInChunk)
   {
      mChunk += wxString(std::string(content)).Trim(true).Trim(false);
   }
}

XMLTagHandler *VSTEffectWrapper::HandleXMLChild(const std::string_view& tag)
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


void VSTEffectWrapper::ForEachParameter(ParameterVisitor visitor) const
{
   for (int i = 0; i < mAEffect->numParams; i++)
   {
      wxString name = GetString(effGetParamName, i);
      if (name.empty())
      {
         name.Printf(wxT("parm_%d"), i);
      }
      else
         /* Easy fix for now for issue 3854, but this should be reconsidered
          There is the possibility that two parameter names might collide
          after normalizing.  A question is whether the normalizing was ever
          really needed for saving in a wxConfigFile.  Maybe not.  But then
          redefinition of the keys stored in the file may introduce versioning
          difficulties if there is an attempt to fix this in future Audacity.
          */
         name = CommandParameters::NormalizeName(name);

      ParameterInfo pi{ i, name };

      if (!visitor(pi))
         break;
   }
}


bool VSTEffectWrapper::FetchSettings(VSTEffectSettings& vstSettings, bool doFetch) const
{
   // Get the fallback ID-value parameters
   ForEachParameter
   (
      [&](const ParameterInfo& pi)
      {
         if (doFetch)
         {
            float val = callGetParameter(pi.mID);
            vstSettings.mParamsMap[pi.mName] = val;
         }
         else
         {
            vstSettings.mParamsMap[pi.mName] = std::nullopt;
         }
         return true;
      }
   );

   // These are here to be checked against for compatibility later
   vstSettings.mVersion   = mAEffect->version;
   vstSettings.mUniqueID  = mAEffect->uniqueID;
   vstSettings.mNumParams = mAEffect->numParams;

   // Get the chunk (if supported)
   vstSettings.mChunk.resize(0);

   if (mAEffect->flags & effFlagsProgramChunks)
   {
      void* chunk = nullptr;
      int clen = (int)constCallDispatcher(effGetChunk, 1, 0, &chunk, 0.0);
      if (clen > 0 && chunk) {
         vstSettings.mChunk.resize(clen);
         memcpy(vstSettings.mChunk.data(), chunk, clen);
      }

      if (!doFetch)
      {
         // Don't keep the contents, but keep a sufficiently allocated string,
         // with some extra space in case chunk length might vary
         auto size = vstSettings.mChunk.size();
         vstSettings.mChunk.resize(0);
         vstSettings.mChunk.reserve(2 * size);
      }
   }

   return true;
}

bool VSTEffectWrapper::StoreSettings(const VSTEffectSettings& vstSettings) const
{
   // First, make sure settings are compatibile with the plugin
   if ((vstSettings.mUniqueID  != mAEffect->uniqueID)   ||
//       (vstSettings.mVersion   != mAEffect->version)    ||
       (vstSettings.mNumParams != mAEffect->numParams)      )
   {
      return false;
   }


   // Try using the chunk first (if available)
   auto &chunk = vstSettings.mChunk;
   if (!chunk.empty())
   {
      VstPatchChunkInfo info = { 1, mAEffect->uniqueID, mAEffect->version, mAEffect->numParams, "" };
      callSetChunk(true, chunk.size(), const_cast<char *>(chunk.data()), &info);
   }


   // Settings (like the message) may store both a chunk, and also accumulated
   // slider movements to reapply after the chunk change.  Or it might be
   // no chunk and id-value pairs only

   constCallDispatcher(effBeginSetProgram, 0, 0, NULL, 0.0);

   ForEachParameter
   (
      [&](const ParameterInfo& pi)
      {
         const auto itr = vstSettings.mParamsMap.find(pi.mName);
         if (itr != vstSettings.mParamsMap.end())
         {
            const float& value = *(itr->second);

            if (value >= -1.0 && value <= 1.0)
            {
               callSetParameter(pi.mID, value);
            }
         }
         return true;
      }
   );
   
   constCallDispatcher(effEndSetProgram, 0, 0, NULL, 0.0);

   return true;
}

bool VSTEffectValidator::UpdateUI()
{
   // Update the controls on the plain UI
   RefreshParameters();

   return true;
}

ComponentInterfaceSymbol VSTEffectWrapper::GetSymbol() const
{
   return mName;
}

EffectSettings VSTEffect::MakeSettings() const
{
   VSTEffectSettings settings;
   FetchSettings(settings);
   return EffectSettings::Make<VSTEffectSettings>(std::move(settings));
}

VSTEffectValidator::~VSTEffectValidator()
{
   // Just for extra safety
   GetInstance().SetOwningValidator(nullptr);
}


std::unique_ptr<EffectInstance::Message>
VSTEffectWrapper::MakeMessageFS(const VSTEffectSettings &settings) const
{
   VSTEffectMessage::ParamVector paramVector;
   paramVector.resize(mAEffect->numParams, std::nullopt);

   ForEachParameter
   (
      [&](const VSTEffectWrapper::ParameterInfo& pi)
      {
         auto &slot = paramVector[pi.mID]; // operator [] may make a nullopt
         const auto iter = settings.mParamsMap.find(pi.mName),
            end = settings.mParamsMap.end();
         if (iter != end)
            slot = iter->second;
         return true;
      }
   );   

   return std::make_unique<VSTEffectMessage>(
      settings.mChunk /* vector copy */, std::move(paramVector));
}

VSTEffectValidator::VSTEffectValidator
(
   VSTEffectInstance&       instance,
   EffectUIClientInterface& effect,
   EffectSettingsAccess&    access,
   wxWindow*                pParent,
   int                      numParams
)
   : EffectUIValidator(effect, access),
     mInstance(instance),
     mParent(pParent),
     mDialog( static_cast<wxDialog*>(wxGetTopLevelParent(pParent)) ),
     mNumParams(numParams)
{
   // In case of nondestructive processing, put an initial message in the
   // queue for the instance
   mAccess.ModifySettings([&](EffectSettings &settings){
      return GetInstance().MakeMessageFS(VSTEffectInstance::GetSettings(settings));
   });

   auto settings = mAccess.Get();
   StoreSettingsToInstance(settings);

   //! Note the parameter names for later use
   mInstance.ForEachParameter([&](const VSTEffectWrapper::ParameterInfo &pi) {
      mParamNames.push_back(pi.mName);
      return true;
   } );

   mTimer = std::make_unique<VSTEffectTimer>(this);

   wxTheApp->Bind(wxEVT_IDLE, &VSTEffectValidator::OnIdle, this);
}


VSTEffectInstance& VSTEffectValidator::GetInstance() const
{
   return mInstance;
}



void VSTEffectWrapper::UpdateDisplay()
{
}


void VSTEffectUIWrapper::Automate(int index, float value)
{
}

void VSTEffectInstance::Automate(int index, float value)
{
   if (mMainThreadId != std::this_thread::get_id())
      return;

   if (mpOwningValidator)
   {
      mpOwningValidator->Automate(index, value);
   }
}


void VSTEffectValidator::Automate(int index, float value)
{
   NotifyParameterChanged(index, value);
   // Send changed settings (only) to the worker thread
   mAccess.Set(GetInstance().MakeMessage(index, value));
   mLastMovements.emplace_back(index, value);
}



VSTEffectInstance::VSTEffectInstance
(
   PerTrackEffect& effect,
   const PluginPath& path,
   size_t            blockSize,
   size_t            userBlockSize,
   bool              useLatency
)

   : PerTrackEffect::Instance(effect)
   , VSTEffectWrapper(path)
{
   // what also happens in the effect ctor
   //
   memset(&mTimeInfo, 0, sizeof(mTimeInfo));
   mTimeInfo.samplePos = 0.0;
   mTimeInfo.sampleRate = 44100.0;  // this is a bogus value, but it's only for the display
   mTimeInfo.nanoSeconds = wxGetUTCTimeMillis().ToDouble();
   mTimeInfo.tempo = 120.0;
   mTimeInfo.timeSigNumerator = 4;
   mTimeInfo.timeSigDenominator = 4;
   mTimeInfo.flags = kVstTempoValid | kVstNanosValid;

   mBlockSize = blockSize;
   mUserBlockSize = userBlockSize;
   mUseLatency = useLatency;

   Load();

   if (!IsReady() )
   {
      // Set some defaults since some VSTs need them...these will be reset when
      // normal or realtime processing begins
      mBlockSize = 8192;
      DoProcessInitialize(44100.0);
   }

   mIsMeldaPlugin = (mVendor == "MeldaProduction");
}


VSTEffectInstance::~VSTEffectInstance()
{
   PowerOff();
}


void VSTEffectInstance::SetOwningValidator(VSTEffectUIWrapper* vi)
{
   mpOwningValidator = vi;
}


bool VSTEffectValidator::FetchSettingsFromInstance(EffectSettings& settings)
{
   return mInstance.FetchSettings(
      // Change this when GetSettings becomes a static function
      static_cast<const VSTEffect&>(mEffect).GetSettings(settings));
}


bool VSTEffectValidator::StoreSettingsToInstance(const EffectSettings& settings)
{
   return mInstance.StoreSettings(
      // Change this when GetSettings becomes a static function
      static_cast<const VSTEffect&>(mEffect).GetSettings(settings));
}


bool VSTEffectValidator::ValidateUI()
{
   mAccess.ModifySettings([this](EffectSettings& settings)
   {
      const auto& eff = static_cast<VSTEffect&>(VSTEffectValidator::mEffect);
      if (eff.GetType() == EffectTypeGenerate)
         settings.extra.SetDuration(mDuration->GetValue());

      FetchSettingsFromInstance(settings);

      return GetInstance().MakeMessage();
   });

   return true;
}


void VSTEffectValidator::OnClose()
{

#ifdef __WXMAC__
#ifdef __WX_EVTLOOP_BUSY_WAITING__
   wxEventLoop::SetBusyWaiting(false);
#endif
   if (mControl)
      mControl->Close();
#endif

   // Tell the instance not to use me anymore - if we do not do this,
   // hiding the gui and then showing it again *while playing*, would leave
   // the instance with a dangling pointer to the old owning validator
   // for a fraction of time, thereby causing a crash.
   GetInstance().SetOwningValidator(nullptr);

   NeedEditIdle(false);

   mNames.reset();
   mSliders.reset();
   mDisplays.reset();
   mLabels.reset();

   mParent = NULL;
   mDialog = NULL;

   mAccess.Flush();

   ValidateUI();
}

#endif // USE_VST
