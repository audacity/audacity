/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file EffectUIServices.h

  Paul Licameli split from EffectPlugin.h

**********************************************************************/
#ifndef __AUDACITY_EFFECT_UI_SERVICES__
#define __AUDACITY_EFFECT_UI_SERVICES__

#include "EffectInterface.h"
#include <memory>
#include <optional>

class EffectEditor;
class EffectInstance;
class EffectPlugin;
class EffectSettings;
class EffectOutputs;
class EffectUIServices;
class ShuttleGui;
class wxWindow;
class wxDialog;

using OptionalMessage =
   std::optional<std::unique_ptr<EffectSettingsAccess::Message>>;

struct DialogFactoryResults {
   wxDialog *pDialog{};
   //! constructed and successfully Init()-ed; or null for failure
   std::shared_ptr<EffectInstance> pInstance{};
   EffectEditor *pEditor{};
};

//! Type of function that creates a dialog for an effect
/*! The dialog may be modal or non-modal */
using EffectDialogFactory = std::function< DialogFactoryResults(
   wxWindow &parent, EffectPlugin &, EffectUIServices &,
   EffectSettingsAccess &) >;

//! Abstract base class to populate a UI and validate UI values.
//! It can import and export presets.
class WX_EFFECTS_API EffectUIServices /* not final */
{
public:
   // Display a message box, using effect's (translated) name as the prefix
   // for the title.
   enum : long { DefaultMessageBoxStyle = wxOK | wxCENTRE };
   static int DoMessageBox(const EffectPlugin &plugin,
      const TranslatableString& message,
      long style = DefaultMessageBoxStyle,
      const TranslatableString &titleStr = {});

   virtual ~EffectUIServices();

   //! The only non-const member function,
   //! it usually applies factory to plugin and self and given access
   /*!
    But there are a few unusual overrides for historical reasons that may ignore
    the factory.

    @param pInstance may be passed to factory, and is only guaranteed to have
    lifetime suitable for a modal dialog, unless the dialog stores a copy of
    pInstance

    @param access is only guaranteed to have lifetime suitable for a modal
    dialog, unless the dialog stores access.shared_from_this()

    @return 0 if destructive effect processing should not proceed (and there
    may be a non-modal dialog still opened); otherwise, modal dialog return code
    */
   virtual int ShowHostInterface(EffectPlugin &plugin,
      wxWindow &parent, const EffectDialogFactory &factory,
      std::shared_ptr<EffectInstance> &pInstance, EffectSettingsAccess &access,
      bool forceModal = false);

   /*!
    @return 0 if destructive effect processing should not proceed (and there
    may be a non-modal dialog still opened); otherwise, modal dialog return code
    */
   virtual int ShowClientInterface(const EffectPlugin &plugin,
      wxWindow &parent, wxDialog &dialog,
      EffectEditor *pEditor, bool forceModal = false) const = 0;

   //! Adds controls to a panel that is given as the parent window of `S`
   /*!
    @param S interface for adding controls to a panel in a dialog
    @param instance guaranteed to have a lifetime containing that of the returned
    object
    @param access guaranteed to have a lifetime containing that of the returned
    object
    @param pOutputs null, or else points to outputs with lifetime containing
    that of the returned object

    @return null for failure; else an object invoked to retrieve values of UI
    controls; it might also hold some state needed to implement event handlers
    of the controls; it will exist only while the dialog continues to exist
    */
   virtual std::unique_ptr<EffectEditor> PopulateUI(
      const EffectPlugin &plugin, ShuttleGui &S,
      EffectInstance &instance, EffectSettingsAccess &access,
      const EffectOutputs *pOutputs) const = 0;

   virtual void ExportPresets(
      const EffectPlugin &plugin, const EffectSettings &settings) const = 0;
   //! @return nullopt for failure
   [[nodiscard]] virtual OptionalMessage ImportPresets(
      const EffectPlugin &plugin, EffectSettings &settings) const = 0;

   virtual void ShowOptions(const EffectPlugin &plugin) const = 0;

   virtual bool ValidateUI(
      const EffectPlugin &context, EffectSettings &settings) const = 0;
   virtual bool CloseUI() const = 0;
};

#endif
