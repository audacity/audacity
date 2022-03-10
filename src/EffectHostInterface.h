/*!********************************************************************

   Audacity: A Digital Audio Editor

   @file EffectHostInterface.h

   Paul Licameli
   split from EffectInterface.h
   
**********************************************************************/

#ifndef __AUDACITY_EFFECTHOSTINTERFACE_H__
#define __AUDACITY_EFFECTHOSTINTERFACE_H__

#include "ComponentInterfaceSymbol.h"

#include <functional>
#include <memory>

class EffectDefinitionInterface;

/*************************************************************************************//**

\class EffectHostInterface 

\brief EffectHostInterface is a decorator of a EffectUIClientInterface.  It adds 
virtual (abstract) functions to get presets and actually apply the effect.  It uses
ConfigClientInterface to add Getters/setters for private and shared configs. 

*******************************************************************************************/
class AUDACITY_DLL_API EffectHostInterface
{
public:
   EffectHostInterface &operator=(EffectHostInterface&) = delete;

   virtual ~EffectHostInterface();

   virtual const EffectDefinitionInterface& GetDefinition() const = 0;

   virtual double GetDuration() = 0;
   virtual NumericFormatSymbol GetDurationFormat() = 0;
   virtual void SetDuration(double seconds) = 0;

   // Preset handling
   virtual RegistryPath GetUserPresetsGroup(const RegistryPath & name) = 0;
   virtual RegistryPath GetCurrentSettingsGroup() = 0;
   virtual RegistryPath GetFactoryDefaultsGroup() = 0;
};

class wxDialog;
class wxWindow;
class EffectUIClientInterface;
class EffectSettings;
class EffectSettingsAccess;
class EffectUIHostInterface;

//! Type of function that creates a dialog for an effect
/*! The dialog may be modal or non-modal */
using EffectDialogFactory = std::function< wxDialog* (
   wxWindow &parent, EffectUIHostInterface &, EffectUIClientInterface &,
   EffectSettingsAccess & ) >;

class TrackList;
class WaveTrackFactory;
class NotifyingSelectedRegion;
class EffectProcessor;

/*************************************************************************************//**

\class EffectUIHostInterface
@brief extends EffectHostInterface with UI-related services
*******************************************************************************************/
class AUDACITY_DLL_API EffectUIHostInterface : public EffectHostInterface
{
public:
   using EffectSettingsAccessPtr = std::shared_ptr<EffectSettingsAccess>;

   const static wxString kUserPresetIdent;
   const static wxString kFactoryPresetIdent;
   const static wxString kCurrentSettingsIdent;
   const static wxString kFactoryDefaultsIdent;

   EffectUIHostInterface &operator=(EffectUIHostInterface&) = delete;
   virtual ~EffectUIHostInterface();

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
   virtual bool GetAutomationParametersAsString(wxString & parms) = 0;
   virtual bool SetAutomationParametersFromString(const wxString & parms) = 0;
   virtual bool IsBatchProcessing() = 0;
   virtual void SetBatchProcessing(bool start) = 0;

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
   virtual bool Startup(EffectUIClientInterface *client) = 0;

   virtual bool TransferDataToWindow(const EffectSettings &settings) = 0;
   virtual bool TransferDataFromWindow(EffectSettings &settings) = 0;
};

#endif
