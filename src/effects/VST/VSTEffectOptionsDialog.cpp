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
#include "VSTEffectOptionsDialog.h"
#include "VSTEditor.h"
#include "VSTEffectsModule.h"
#include "VSTInstance.h"
#include "SampleCount.h"

#include "ProgressDialog.h"

#if 0
#if defined(BUILDING_AUDACITY)
#include "../../PlatformCompatibility.h"

// Make the main function private
#else
#define USE_VST 1
#endif
#endif

#if USE_VST

#include "SelectFile.h"
#include "ShuttleGui.h"
#include "../../widgets/valnum.h"
#include "AudacityMessageBox.h"
#include "ConfigInterface.h"

// NOTE:  To debug the subprocess, use wxLogDebug and, on Windows, Debugview
//        from TechNet (Sysinternals).

BEGIN_EVENT_TABLE(VSTEffectOptionsDialog, wxDialogWrapper)
   EVT_BUTTON(wxID_OK, VSTEffectOptionsDialog::OnOk)
END_EVENT_TABLE()

VSTEffectOptionsDialog::VSTEffectOptionsDialog(
   const EffectDefinitionInterface &effect
)  : wxDialogWrapper{ nullptr, wxID_ANY, XO("VST Effect Options") }
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

VSTEffect::~VSTEffect() = default;

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
int VSTEffect::ShowClientInterface(const EffectPlugin &,
   wxWindow &parent, wxDialog &dialog,
   EffectEditor* pEditor, bool forceModal) const
{
   //   mProcessLevel = 1;      // in GUI thread

   VSTEditor* vstEditor = static_cast<VSTEditor*>(pEditor);

   return vstEditor->ShowDialog(/* nonModal = */ SupportsRealtime() && !forceModal);
}

std::unique_ptr<EffectEditor> VSTEffect::PopulateUI(const EffectPlugin &,
   ShuttleGui &S, EffectInstance& instance, EffectSettingsAccess &access,
   const EffectOutputs *) const
{
   auto parent = S.GetParent();

   // Determine whether fancy UI is available
   bool gui = mGui;

   // Then use fancy UI only if preferences say so
   if (gui)
      GetConfig(*this, PluginSettings::Shared, wxT("Options"),
                             wxT("UseGUI"),
                             gui,
                          true);

   auto pParent = S.GetParent();

   auto& vst2Instance = dynamic_cast<VSTInstance&>(instance);

   auto editor = std::make_unique<VSTEditor>(
      vst2Instance, GetType(), gui, *this, access, pParent, mAEffect->numParams);

   // Also let the instance know about the validator, so it can forward
   // to it calls coming from the vst callback
   vst2Instance.SetOwningValidator(editor.get());


   // Build the appropriate dialog type
   if (mGui)
   {
      editor->BuildFancy(instance);
   }
   else
   {
      editor->BuildPlain(access, GetType(), mProjectRate);
   }


   return editor;
}

std::unique_ptr<EffectEditor> VSTEffect::MakeEditor(
   ShuttleGui &, EffectInstance &, EffectSettingsAccess &,
   const EffectOutputs *) const
{
   //! Will not come here because Effect::PopulateUI is overridden
   assert(false);
   return nullptr;
}

// Throws exceptions rather than reporting errors.
void VSTEffect::ExportPresets(
   const EffectPlugin &, const EffectSettings& settings) const
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
OptionalMessage VSTEffect::ImportPresets(const EffectPlugin&,
   EffectSettings& settings) const
{
   auto temp = std::make_unique<VSTEffect>(this->mPath);
   if (!temp->InitializePlugin())
      return {};
   return temp->ImportPresetsNC(settings);
}

OptionalMessage VSTEffect::ImportPresetsNC(EffectSettings& settings)
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
      VSTInstance::GetSettings(settings));
}

void VSTEffect::ShowOptions(const EffectPlugin &) const
{
   VSTEffectOptionsDialog{ *this }.ShowModal();
}

// Inject factory hook to make VSTEffect capable of UI
static VSTEffectsModule::Factory::SubstituteInUnique<VSTEffect> scope;

#endif // USE_VST
