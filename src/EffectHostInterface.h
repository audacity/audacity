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

   virtual EffectDefinitionInterface &GetDefinition() = 0;

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

class EffectUIHostInterface;
using EffectDialogFactory = std::function<
   wxDialog* ( wxWindow &parent,
      EffectUIHostInterface&, EffectUIClientInterface& )
>;

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
   const static wxString kUserPresetIdent;
   const static wxString kFactoryPresetIdent;
   const static wxString kCurrentSettingsIdent;
   const static wxString kFactoryDefaultsIdent;

   EffectUIHostInterface &operator=(EffectUIHostInterface&) = delete;
   virtual ~EffectUIHostInterface();

   /*!
    @return 0 if destructive effect processing should not proceed (and there
    may be a non-modal dialog still opened); otherwise, modal dialog return code
    */
   virtual int ShowHostInterface(
      wxWindow &parent, const EffectDialogFactory &factory,
      bool forceModal = false
   ) = 0;

   virtual void Preview(bool dryOnly) = 0;
   virtual bool GetAutomationParametersAsString(wxString & parms) = 0;
   virtual bool SetAutomationParametersFromString(const wxString & parms) = 0;
   virtual bool IsBatchProcessing() = 0;
   virtual void SetBatchProcessing(bool start) = 0;
   // Returns true on success.  Will only operate on tracks that
   // have the "selected" flag set to true, which is consistent with
   // Audacity's standard UI.
   // Create a user interface only if the supplied function is not null.
   virtual bool DoEffect( double projectRate, TrackList *list,
      WaveTrackFactory *factory, NotifyingSelectedRegion &selectedRegion,
      unsigned flags,
      // Prompt the user for input only if these arguments are both not null.
      wxWindow *pParent = nullptr,
      const EffectDialogFactory &dialogFactory = {} ) = 0;
   virtual bool Startup(EffectUIClientInterface *client) = 0;

   virtual bool TransferDataToWindow() = 0;
   virtual bool TransferDataFromWindow() = 0;
};

#endif
