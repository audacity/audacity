/**********************************************************************

   Audacity: A Digital Audio Editor

   ConfigInterface.h

   Leland Lucius

   Copyright (c) 2014, Audacity Team 
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

   1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

   3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
   FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
   COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
   INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
   BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
   LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
   CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
   LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
   ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
   POSSIBILITY OF SUCH DAMAGE.

**********************************************************************/

#ifndef __AUDACITY_CONFIGINTERFACE_H__
#define __AUDACITY_CONFIGINTERFACE_H__

#include "Identifier.h"
#include <functional>
#include <tuple>
#include <type_traits>
#include <variant>
#include <vector>

namespace PluginSettings {

enum ConfigurationType : unsigned {
   Shared, Private
};

}

//! Supported types for settings
using ConfigValueTypes = std::tuple<
     wxString
   , int
   , bool
   , float
   , double
>;

//! Define a reference to a variable of one of the types in ConfigValueTypes
/*! Avoid repetition of the list of types */
template<bool is_const, typename> struct ConfigReferenceGenerator;
template<bool is_const, typename... Types>
struct ConfigReferenceGenerator<is_const, std::tuple<Types...>> {
   using type = std::variant< std::reference_wrapper<
      std::conditional_t<is_const, const Types, Types> >... >;
};
using ConfigReference =
   ConfigReferenceGenerator<false, ConfigValueTypes>::type;
using ConfigConstReference =
   ConfigReferenceGenerator<true, ConfigValueTypes>::type;

/*************************************************************************************//**

\class ConfigClientInterface

\brief ConfigClientInterface is an unholy get/set configuration class, which 
differentiates between private and shared config.  It should probably be replaced 
with a Shuttle.

*******************************************************************************************/
class COMPONENTS_API ConfigClientInterface /* not final */
{
public:
   using ConfigurationType = PluginSettings::ConfigurationType;

   virtual ~ConfigClientInterface();

   virtual bool HasConfigGroup(
      ConfigurationType type, const RegistryPath & group) = 0;
   virtual bool GetConfigSubgroups(
      ConfigurationType type, const RegistryPath & group,
      RegistryPaths & subgroups) = 0;

   // GetConfig with default value
   template<typename Value>
   bool GetConfig(ConfigurationType type, const RegistryPath & group,
      const RegistryPath & key, Value &var, Value defval)
   { return GetConfigValue(type, group, key,
      std::ref(var), std::cref(defval)); }

   // GetConfig with implicitly converted default value
   template<typename Value, typename ConvertibleToValue>
   bool GetConfig(ConfigurationType type, const RegistryPath & group,
      const RegistryPath & key, Value &var, ConvertibleToValue defval)
   { return GetConfig(type, group, key, var, static_cast<Value>(defval)); }

   // GetConfig with default value assumed to be Value{}
   template<typename Value>
   bool GetConfig(ConfigurationType type, const RegistryPath & group,
      const RegistryPath & key, Value &var)
   { return GetConfig(type, group, key, var, Value{}); }

   template<typename Value>
   bool SetConfig(ConfigurationType type, const RegistryPath & group,
      const RegistryPath & key, const Value &value)
   { return SetConfigValue(type, group, key, std::cref(value)); }

   virtual bool RemoveConfigSubgroup(
      ConfigurationType type, const RegistryPath & group) = 0;
   virtual bool RemoveConfig(
      ConfigurationType type, const RegistryPath & group,
      const RegistryPath & key) = 0;

protected:
   //! @pre var and defval wrap references to the same type (ignoring const)
   virtual bool GetConfigValue(ConfigurationType type, const RegistryPath & group,
      const RegistryPath & key,
      ConfigReference var, ConfigConstReference defval) = 0;
   virtual bool SetConfigValue(ConfigurationType type, const RegistryPath & group,
      const RegistryPath & key, ConfigConstReference value) = 0;
};

#endif // __AUDACITY_CONFIGINTERFACE_H__
