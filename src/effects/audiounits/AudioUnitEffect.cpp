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
#include "AudioUnitEffectOptionsDialog.h"
#include "AudioUnitInstance.h"
#include "AudioUnitEditor.h"
#include "SampleCount.h"
#include "ConfigInterface.h"

#include <optional>
#include <wx/defs.h>
#include <wx/base64.h>
#include <wx/control.h>
#include <wx/crt.h>
#include <wx/dir.h>

#ifdef __WXMAC__
#include <wx/evtloop.h>
#endif

#include <wx/filename.h>
#include <wx/frame.h>
#include <wx/listctrl.h>
#include <wx/log.h>
#include <wx/settings.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>

#include "SelectFile.h"
#include "ShuttleGui.h"
#include "AudacityMessageBox.h"
#include "../../widgets/valnum.h"

//
// When a plug-in's state is saved to the settings file (as a preset),
// it is in binary and gets base64 encoded before storing.
//
// When exporting, save as XML without base64 encoding.
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

AudioUnitEffect::~AudioUnitEffect()
{
}

#if 0
size_t AudioUnitInstance::GetTailSize() const
{
   // Retrieve the tail time
   Float64 tailTime = 0.0;
   if (!GetFixedSizeProperty(kAudioUnitProperty_TailTime, tailTime))
      return tailTime * mSampleRate;
   return 0;
}
#endif

int AudioUnitEffect::ShowClientInterface(const EffectPlugin &,
   wxWindow &parent, wxDialog &dialog,
   EffectEditor *, bool forceModal) const
{
   if ((SupportsRealtime() || GetType() == EffectTypeAnalyze) && !forceModal) {
      dialog.Show();
      return 0;
   }
   return dialog.ShowModal();
}

std::unique_ptr<EffectEditor> AudioUnitEffect::PopulateUI(
   const EffectPlugin &, ShuttleGui &S,
   EffectInstance &instance, EffectSettingsAccess &access,
   const EffectOutputs *) const
{
   wxString uiType;
   // Decide whether to build plain or fancy user interfaces
   GetConfig(*this, PluginSettings::Shared, OptionsKey, UITypeKey,
      uiType, FullValue.MSGID().GET() /* Config stores un-localized string */);
   return AudioUnitEditor::Create(*this, S, uiType, instance, access);
}

std::unique_ptr<EffectEditor> AudioUnitEffect::MakeEditor(
   ShuttleGui &, EffectInstance &, EffectSettingsAccess &,
   const EffectOutputs *) const
{
   //! Will not come here because Effect::PopulateUI is overridden
   assert(false);
   return nullptr;
}

#if defined(HAVE_AUDIOUNIT_BASIC_SUPPORT)
bool AudioUnitEffect::CreatePlain(wxWindow *parent)
{
   // TODO???  Never implemented...
   return false;
}
#endif

bool AudioUnitEffect::CloseUI() const
{
#ifdef __WXMAC__
#ifdef __WX_EVTLOOP_BUSY_WAITING__
   wxEventLoop::SetBusyWaiting(false);
#endif
#endif
   return true;
}

void AudioUnitEffect::ExportPresets(
   const EffectPlugin &, const EffectSettings &settings) const
{
   // Generate the user domain path
   wxFileName fn;
   fn.SetPath(PRESET_USER_PATH);
   fn.AppendDir(mVendor);
   fn.AppendDir(mName);
   fn.Normalize();
   FilePath path = fn.GetFullPath();

   if (!fn.Mkdir(fn.GetFullPath(), 0755, wxPATH_MKDIR_FULL)) {
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
      return;

   auto msg = Export(GetSettings(settings), path);
   if (!msg.empty())
      AudacityMessageBox(
         XO("Could not export \"%s\" preset\n\n%s").Format(path, msg),
         XO("Export Audio Unit Presets"),
         wxOK | wxCENTRE);
}

OptionalMessage AudioUnitEffect::ImportPresets(
   const EffectPlugin &, EffectSettings &settings) const
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
      fn.GetFullPath(), wxEmptyString, wxT("aupreset"),
      {
        { XO("Standard Audio Unit preset file"), { wxT("aupreset") }, true },
      },
      wxFD_OPEN | wxRESIZE_BORDER,
      nullptr);

   // User canceled...
   if (path.empty())
      return {};

   auto msg = Import(GetSettings(settings), path);
   if (!msg.empty()) {
      AudacityMessageBox(
         XO("Could not import \"%s\" preset\n\n%s").Format(path, msg),
         XO("Import Audio Unit Presets"),
         wxOK | wxCENTRE);
      return {};
   }

   return { nullptr };
}

void AudioUnitEffect::ShowOptions(const EffectPlugin &) const
{
   AudioUnitEffectOptionsDialog{ *this }.ShowModal();
}

// Inject factory hook to make AudioUnitEffect capable of UI
static AudioUnitEffectBase::Factory::SubstituteInUnique<AudioUnitEffect> scope;

#endif
