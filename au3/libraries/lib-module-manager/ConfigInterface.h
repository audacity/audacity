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
#include <vector>

class EffectDefinitionInterface;

#include "PluginInterface.h"

namespace PluginSettings {
MODULE_MANAGER_API bool HasConfigGroup(const EffectDefinitionInterface& ident, ConfigurationType type, const RegistryPath& group);
MODULE_MANAGER_API bool GetConfigSubgroups(const EffectDefinitionInterface& ident, ConfigurationType type, const RegistryPath& group,
                                           RegistryPaths& subgroups);

MODULE_MANAGER_API bool GetConfigValue(const EffectDefinitionInterface& ident, ConfigurationType type, const RegistryPath& group,
                                       const RegistryPath& key, ConfigReference var, ConfigConstReference value);

MODULE_MANAGER_API bool HasConfigValue(const EffectDefinitionInterface& ident, ConfigurationType type, const RegistryPath& group,
                                       const RegistryPath& key);

// GetConfig with default value
template<typename Value>
inline bool GetConfig(const EffectDefinitionInterface& ident,
                      ConfigurationType type, const RegistryPath& group,
                      const RegistryPath& key, Value& var, const Value& defval)
{
    return GetConfigValue(ident, type, group, key,
                          std::ref(var), std::cref(defval));
}

// GetConfig with implicitly converted default value
template<typename Value, typename ConvertibleToValue>
inline bool GetConfig(const EffectDefinitionInterface& ident,
                      ConfigurationType type, const RegistryPath& group,
                      const RegistryPath& key, Value& var, ConvertibleToValue defval)
{ return GetConfig(ident, type, group, key, var, static_cast<Value>(defval)); }

// Deleted overloads for const Value as destination
template<typename Value>
inline bool GetConfig(const EffectDefinitionInterface& ident, ConfigurationType type, const RegistryPath& group, const RegistryPath& key,
                      const Value& var, const Value& defval) = delete;
template<typename Value, typename ConvertibleToValue>
inline bool GetConfig(const EffectDefinitionInterface& ident, ConfigurationType type, const RegistryPath& group, const RegistryPath& key,
                      const Value& var, ConvertibleToValue defval)
= delete;

// GetConfig with default value assumed to be Value{}
template<typename Value>
inline bool GetConfig(const EffectDefinitionInterface& ident,
                      ConfigurationType type, const RegistryPath& group,
                      const RegistryPath& key, Value& var)
{
    return GetConfig(ident, type, group, key, var, Value {});
}

MODULE_MANAGER_API bool SetConfigValue(const EffectDefinitionInterface& ident, ConfigurationType type, const RegistryPath& group,
                                       const RegistryPath& key, ConfigConstReference value);

template<typename Value>
inline bool SetConfig(const EffectDefinitionInterface& ident,
                      ConfigurationType type, const RegistryPath& group,
                      const RegistryPath& key, const Value& value)
{
    return SetConfigValue(ident, type, group, key, std::cref(value));
}

MODULE_MANAGER_API bool RemoveConfigSubgroup(const EffectDefinitionInterface& ident, ConfigurationType type, const RegistryPath& group);
MODULE_MANAGER_API bool RemoveConfig(const EffectDefinitionInterface& ident, ConfigurationType type, const RegistryPath& group,
                                     const RegistryPath& key);
}

#endif // __AUDACITY_CONFIGINTERFACE_H__
