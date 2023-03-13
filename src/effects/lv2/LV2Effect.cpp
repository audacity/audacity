/**********************************************************************

  Audacity: A Digital Audio Editor

  LV2Effect.cpp

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2 or later.  See License.txt.

**********************************************************************/

#if defined(USE_LV2)

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wparentheses"
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
#elif defined(__clang__)
#pragma clang diagnostic ignored "-Wparentheses"
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
#endif

#include "LV2Effect.h"
#include "LV2Instance.h"
#include "LV2Editor.h"
#include "LV2PreferencesDialog.h"
#include "LV2Wrapper.h"

#include <cmath>
#include <exception>
#include <functional>


#ifdef __WXMAC__
#include <wx/evtloop.h>
#endif

#include "AudacityMessageBox.h"

#if defined(__WXGTK__)
#include <gtk/gtk.h>
#endif

#if defined(__WXMSW__)
#include <wx/msw/wrapwin.h>
#endif

LV2Effect::~LV2Effect() = default;

int LV2Effect::ShowClientInterface(const EffectPlugin &, wxWindow &parent,
   wxDialog &dialog, EffectEditor *pEditor, bool forceModal) const
{
   if (pEditor)
      // Remember the dialog with a weak pointer, but don't control its lifetime
      static_cast<LV2Editor*>(pEditor)->mDialog = &dialog;
   // Try to give the window a sensible default/minimum size
   dialog.Layout();
   dialog.Fit();
   dialog.SetMinSize(dialog.GetSize());
   if (mFeatures.mNoResize)
      dialog.SetMaxSize(dialog.GetSize());
   if ((SupportsRealtime() || GetType() == EffectTypeAnalyze) && !forceModal) {
      dialog.Show();
      return 0;
   }
   return dialog.ShowModal();
}

// May come here before destructive processing
// Or maybe not (if you "Repeat Last Effect")
std::unique_ptr<EffectEditor> LV2Effect::PopulateUI(const EffectPlugin &,
   ShuttleGui &S, EffectInstance &instance, EffectSettingsAccess &access,
   const EffectOutputs *pOutputs) const
{
   auto &settings = access.Get();
   auto parent = S.GetParent();

   auto &myInstance = dynamic_cast<LV2Instance &>(instance);
   auto pWrapper =
      // Output port connection isn't needed for fancy UI wrapper.  Its
      // features are needed to make the suil_instance
      myInstance.MakeWrapper(settings, mProjectRate, nullptr);
   if (!pWrapper) {
      AudacityMessageBox( XO("Couldn't instantiate effect") );
      return nullptr;
   }

   // Determine if the GUI editor is supposed to be used or not
   bool useGUI = false;
   LV2Preferences::GetUseGUI(*this, useGUI);

   // Until I figure out where to put the "Duration" control in the
   // graphical editor, force usage of plain editor.
   if (GetType() == EffectTypeGenerate)
      useGUI = false;

   auto result = std::make_unique<LV2Editor>(*this, GetType(), mPlug,
      dynamic_cast<LV2Instance&>(instance),
      access, pOutputs, mProjectRate, mFeatures, mPorts, parent, useGUI);

#ifdef __WXMAC__
   const auto vendor = GetVendor().Msgid().Translation();
   const bool doX42Hack = vendor == "Robin Gareus";
   result->mUI.mJustLeakMemory = doX42Hack;
#endif

   if (result->mUseGUI)
      result->mUseGUI = result->BuildFancy(move(pWrapper), settings);
   if (!result->mUseGUI && !result->BuildPlain(access))
      return nullptr;
   result->UpdateUI();

   return result;
}

std::unique_ptr<EffectEditor> LV2Effect::MakeEditor(
   ShuttleGui &, EffectInstance &, EffectSettingsAccess &,
   const EffectOutputs *) const
{
   //! Will not come here because Effect::PopulateUI is overridden
   assert(false);
   return nullptr;
}

bool LV2Effect::CloseUI() const
{
#ifdef __WXMAC__
#ifdef __WX_EVTLOOP_BUSY_WAITING__
   wxEventLoop::SetBusyWaiting(false);
#endif
#endif

   return true;
}

void LV2Effect::ExportPresets(
   const EffectPlugin &, const EffectSettings &) const
{
}

OptionalMessage LV2Effect::ImportPresets(
   const EffectPlugin &, EffectSettings &) const
{
   return { nullptr };
}

void LV2Effect::ShowOptions(const EffectPlugin &) const
{
   LV2PreferencesDialog{ *this }.ShowModal();
}

// Inject factory hook to make LV2Effect capable of UI
static LV2EffectBase::Factory::SubstituteInUnique<LV2Effect> scope;

#endif
