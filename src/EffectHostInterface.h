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

   virtual double GetDefaultDuration() = 0;
   virtual double GetDuration() = 0;
   virtual NumericFormatSymbol GetDurationFormat() = 0;
   virtual void SetDuration(double seconds) = 0;

   // Preset handling
   virtual RegistryPath GetUserPresetsGroup(const RegistryPath & name) = 0;
   virtual RegistryPath GetCurrentSettingsGroup() = 0;
   virtual RegistryPath GetFactoryDefaultsGroup() = 0;
};

/*************************************************************************************//**

\class EffectUIHostInterface

\brief EffectUIHostInterface has nothing in it.  It is provided so that an Effect
can call SetHostUI passing in a pointer to an EffectUIHostInterface.  It contains no
functionality and is provided, apparently, for type checking.  Since only EffectUIHost
uses it, EffectUIHost could be used instead.
*******************************************************************************************/
class AUDACITY_DLL_API EffectUIHostInterface
{
public:
   virtual ~EffectUIHostInterface();
};

#endif
