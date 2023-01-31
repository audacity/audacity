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
class ShuttleGui;
class wxWindow;
class wxDialog;

using OptionalMessage =
   std::optional<std::unique_ptr<EffectSettingsAccess::Message>>;

//! Abstract base class to populate a UI and validate UI values.
//! It can import and export presets.
class AUDACITY_DLL_API EffectUIServices /* not final */
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
