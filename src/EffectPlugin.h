/*!********************************************************************

   Audacity: A Digital Audio Editor

   @file EffectPlugin.h

   Paul Licameli
   split from EffectInterface.h
   
**********************************************************************/

#ifndef __AUDACITY_EFFECTPLUGIN_H__
#define __AUDACITY_EFFECTPLUGIN_H__

#include "ComponentInterfaceSymbol.h"

#include <functional>
#include <memory>

class EffectDefinitionInterface;

class wxDialog;
class wxWindow;
class EffectUIClientInterface;
class EffectSettings;
class EffectSettingsAccess;
class EffectPlugin;

//! Type of function that creates a dialog for an effect
/*! The dialog may be modal or non-modal */
using EffectDialogFactory = std::function< wxDialog* (
   wxWindow &parent, EffectPlugin &, EffectUIClientInterface &,
   EffectSettingsAccess & ) >;

class TrackList;
class WaveTrackFactory;
class NotifyingSelectedRegion;
class EffectInstance;

/***************************************************************************//**
\class EffectPlugin
@brief Factory of instances of an effect and of dialogs to control them
*******************************************************************************/
class AUDACITY_DLL_API EffectPlugin
{
public:
   using EffectSettingsAccessPtr = std::shared_ptr<EffectSettingsAccess>;

   const static wxString kUserPresetIdent;
   const static wxString kFactoryPresetIdent;
   const static wxString kCurrentSettingsIdent;
   const static wxString kFactoryDefaultsIdent;

   EffectPlugin &operator=(EffectPlugin&) = delete;
   virtual ~EffectPlugin();

   virtual const EffectDefinitionInterface& GetDefinition() const = 0;

   //! Usually applies factory to self and given access
   /*!
    But there are a few unusual overrides for historical reasons

    @param access is only guaranteed to have lifetime suitable for a modal
    dialog, unless the dialog stores access.shared_from_this()

    @return 0 if destructive effect processing should not proceed (and there
    may be a non-modal dialog still opened); otherwise, modal dialog return code
    */
   virtual int ShowHostInterface(
      wxWindow &parent, const EffectDialogFactory &factory,
      EffectSettingsAccess &access, bool forceModal = false
   ) = 0;

   virtual void Preview(EffectSettingsAccess &access, bool dryOnly) = 0;
   virtual bool SaveSettingsAsString(
      const EffectSettings &settings, wxString & parms) const = 0;
   virtual bool LoadSettingsFromString(
      const wxString & parms, EffectSettings &settings) const = 0;
   virtual bool IsBatchProcessing() const = 0;
   virtual void SetBatchProcessing() = 0;
   virtual void UnsetBatchProcessing() = 0;

   //! Unfortunately complicated dual-use function
   /*!
    Sometimes this is invoked only to do effect processing, as a delegate for
    another effect, but sometimes also to put up a dialog prompting the user for
    settings first.

    Create a user interface only if the supplied factory is not null.
    Factory may be null because we "Repeat last effect" or apply a macro

    Will only operate on tracks that have the "selected" flag set to true,
    which is consistent with Audacity's standard UI.

    @return true on success
    */
   virtual bool DoEffect(
      EffectSettings &settings, //!< Always given; only for processing
      double projectRate, TrackList *list,
      WaveTrackFactory *factory, NotifyingSelectedRegion &selectedRegion,
      unsigned flags,
      // Prompt the user for input only if the next arguments are not all null.
      wxWindow *pParent = nullptr,
      const EffectDialogFactory &dialogFactory = {},
      const EffectSettingsAccessPtr &pAccess = nullptr
         //!< Sometimes given; only for UI
   ) = 0;

   //! Update controls for the settings
   virtual bool TransferDataToWindow(const EffectSettings &settings) = 0;

   //! Update the given settings from controls
   virtual bool TransferDataFromWindow(EffectSettings &settings) = 0;

   //! Make an object maintaining short-term state of an Effect
   /*!
    One effect may have multiple instances extant simultaneously.
    Instances have state, may be implemented in foreign code, and are temporary,
    whereas EffectSettings represents persistent effect state that can be saved
    and reloaded from files.

    @param settings may be assumed to have a lifetime enclosing the instance's
    */
   virtual std::shared_ptr<EffectInstance>
   MakeInstance(EffectSettings &settings) = 0;
};

/***************************************************************************//**
\class EffectInstance
@brief Performs effect computation
*******************************************************************************/
class EffectInstance : public std::enable_shared_from_this<EffectInstance>
{
public:
   virtual ~EffectInstance();

   //! Call once to set up state for whole list of tracks to be processed
   /*!
    @return success
    */
   virtual bool Init() = 0;

   //! Actually do the effect here.
   /*!
    @return success
    */
   virtual bool Process(EffectSettings &settings) = 0;
};

#endif
