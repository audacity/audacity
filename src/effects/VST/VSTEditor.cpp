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
#include <wx/module.h>
#include <wx/process.h>
#include <wx/recguard.h>
#include <wx/sizer.h>
#include <wx/scrolwin.h>
#include <wx/sstream.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/timer.h>
#include <wx/utils.h>

// TODO:  Unfortunately we have some dependencies on Audacity provided
//        dialogs, widgets and other stuff.  This will need to be cleaned up.

#include "PlatformCompatibility.h"
#include "SelectFile.h"
#include "ShuttleGui.h"
#include "../../widgets/valnum.h"
#include "AudacityMessageBox.h"
#include "../../widgets/NumericTextCtrl.h"

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

// NOTE:  To debug the subprocess, use wxLogDebug and, on Windows, Debugview
//        from TechNet (Sysinternals).

///////////////////////////////////////////////////////////////////////////////
//
// Dialog for configuring latency, buffer size and graphics mode for a
// VST effect.
//
///////////////////////////////////////////////////////////////////////////////
class VSTEffectOptionsDialog final : public wxDialogWrapper
{
public:
   explicit VSTEffectOptionsDialog(const EffectDefinitionInterface &effect);
   virtual ~VSTEffectOptionsDialog();

   void PopulateOrExchange(ShuttleGui & S);

   void OnOk(wxCommandEvent & evt);

private:
   const EffectDefinitionInterface &mEffect;
   int mBufferSize;
   bool mUseLatency;
   bool mUseGUI;

   DECLARE_EVENT_TABLE()
};

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
///
/// Wrapper for wxTimer that calls a VST effect at regular intervals.
///
/// \todo should there be tests for no timer available?
///
///////////////////////////////////////////////////////////////////////////////
class VSTTimer final : public wxTimer
{
public:
   VSTTimer(VSTEditor* pEditor)
   :  wxTimer(),
      mpEditor(pEditor)
   {
   }

   ~VSTTimer()
   {
   }

   void Notify()
   {
      mpEditor->OnTimer();
   }

private:
   VSTEditor* mpEditor;
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

int VSTEditor::ShowDialog(bool nonModal)
{
   mDialog->CentreOnParent();

   if (nonModal)
   {
      mDialog->Show();
      return 0;
   }

   return mDialog->ShowModal();
}

bool VSTEditor::IsGraphicalUI()
{
   return mGui;
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

void VSTEditor::Flush()
{
   mAccess.Flush();
}

void VSTEditor::OnTimer()
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

void VSTEditor::NeedIdle()
{
   mWantsIdle = true;
   mTimer->Start(100);
}

void VSTEditor::NeedEditIdle(bool state)
{
   mWantsEditIdle = state;
   mTimer->Start(100);
}

void VSTEditor::NotifyParameterChanged(int index, float value)
{
   const auto& settings = VSTWrapper::GetSettings(mAccess.Get());

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

void VSTEditor::OnIdle(wxIdleEvent& evt)
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
               auto &mySettings = VSTWrapper::GetSettings(settings);
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

void VSTEditor::SizeWindow(int w, int h)
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

void VSTEditor::BuildFancy(EffectInstance& instance)
{
   auto& vstEffInstance = dynamic_cast<VSTInstance&>(instance);

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


   BindTo(*mDialog, EVT_SIZEWINDOW, &VSTEditor::OnSizeWindow);

#ifdef __WXMAC__
#ifdef __WX_EVTLOOP_BUSY_WAITING__
   wxEventLoop::SetBusyWaiting(true);
#endif
#endif

   return;
}

void VSTEditor::BuildPlain(EffectSettingsAccess &access, EffectType effectType, double projectRate)
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
               NumericTextCtrl(FormatterContext::SampleRateContext(projectRate),
                  scroller, ID_Duration,
                  NumericConverterType_TIME,
                  extra.GetDurationFormat(),
                  extra.GetDuration(),
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
            BindTo(*mSliders[i], wxEVT_COMMAND_SLIDER_UPDATED, &VSTEditor::OnSlider);

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

void VSTEditor::RefreshParameters(int skip) const
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

void VSTEditor::OnSizeWindow(wxCommandEvent & evt)
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

void VSTEditor::OnSlider(wxCommandEvent & evt)
{
   wxSlider *s = (wxSlider *) evt.GetEventObject();
   int i = s->GetId() - ID_Sliders;
   float value = s->GetValue() / 1000.0;

   NotifyParameterChanged(i, value);
   // Send changed settings (only) to the worker thread
   mAccess.Set(GetInstance().MakeMessage(i, value));
   mLastMovements.emplace_back(i, value);
}

bool VSTEditor::UpdateUI()
{
   // Update the controls on the plain UI
   RefreshParameters();

   return true;
}

VSTEditor::~VSTEditor()
{
   // Just for extra safety
   GetInstance().SetOwningValidator(nullptr);
}

VSTEditor::VSTEditor(
   VSTInstance&             instance,
   EffectType               type,
   bool                     gui,
   const EffectUIServices&  services,
   EffectSettingsAccess&    access,
   wxWindow*                pParent,
   int                      numParams
)
   : EffectEditor(services, access),
     mType{ type },
     mInstance(instance),
     mGui{ gui },
     mParent(pParent),
     mDialog( static_cast<wxDialog*>(wxGetTopLevelParent(pParent)) ),
     mNumParams(numParams)
{
   // In case of nondestructive processing, put an initial message in the
   // queue for the instance
   mAccess.ModifySettings([&](EffectSettings &settings){
      return GetInstance().MakeMessageFS(VSTInstance::GetSettings(settings));
   });

   auto settings = mAccess.Get();
   StoreSettingsToInstance(settings);

   //! Note the parameter names for later use
   mInstance.ForEachParameter([&](const VSTWrapper::ParameterInfo &pi) {
      mParamNames.push_back(pi.mName);
      return true;
   } );

   mTimer = std::make_unique<VSTTimer>(this);

   wxTheApp->Bind(wxEVT_IDLE, &VSTEditor::OnIdle, this);
}


VSTInstance& VSTEditor::GetInstance() const
{
   return mInstance;
}

void VSTEditor::Automate(int index, float value)
{
   NotifyParameterChanged(index, value);
   // Send changed settings (only) to the worker thread
   mAccess.Set(GetInstance().MakeMessage(index, value));
   mLastMovements.emplace_back(index, value);
}

bool VSTEditor::FetchSettingsFromInstance(EffectSettings& settings)
{
   return mInstance.FetchSettings(VSTWrapper::GetSettings(settings));
}

bool VSTEditor::StoreSettingsToInstance(const EffectSettings& settings)
{
   return mInstance.StoreSettings(VSTWrapper::GetSettings(settings));
}

bool VSTEditor::ValidateUI()
{
   mAccess.ModifySettings([this](EffectSettings& settings)
   {
      if (mType == EffectTypeGenerate)
         settings.extra.SetDuration(mDuration->GetValue());

      FetchSettingsFromInstance(settings);

      return GetInstance().MakeMessage();
   });

   return true;
}

void VSTEditor::OnClose()
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
