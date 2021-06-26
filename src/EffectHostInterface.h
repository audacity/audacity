/*!********************************************************************

   Audacity: A Digital Audio Editor

   @file EffectHostInterface.h

   Paul Licameli
   split from EffectInterface.h
   
**********************************************************************/

#ifndef __AUDACITY_EFFECTHOSTINTERFACE_H__
#define __AUDACITY_EFFECTHOSTINTERFACE_H__

#include "ConfigInterface.h"
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

using EffectDialogFactory = std::function<
   wxDialog* ( wxWindow &parent,
      EffectHostInterface&, EffectUIClientInterface& )
>;

/*************************************************************************************//**

\class EffectUIHostInterface
@brief extends EffectHostInterface with UI-related services
*******************************************************************************************/
class AUDACITY_DLL_API EffectUIHostInterface : public EffectHostInterface
{
public:
   virtual ~EffectUIHostInterface();

   /*!
    @return 0 if destructive effect processing should not proceed (and there
    may be a non-modal dialog still opened); otherwise, modal dialog return code
    */
   virtual int ShowHostInterface(
      wxWindow &parent, const EffectDialogFactory &factory,
      bool forceModal = false
   ) = 0;
};

#endif
