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
#include <vector>

namespace PluginSettings {

enum ConfigurationType : unsigned {
   Shared, Private
};

}

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

   virtual bool GetConfig(ConfigurationType type, const RegistryPath & group,
      const RegistryPath & key, wxString & value,
      const wxString & defval = {}) = 0;
   virtual bool GetConfig(ConfigurationType type, const RegistryPath & group,
      const RegistryPath & key, int & value, int defval = 0) = 0;
   virtual bool GetConfig(ConfigurationType type, const RegistryPath & group,
      const RegistryPath & key, bool & value, bool defval = false) = 0;
   virtual bool GetConfig(ConfigurationType type, const RegistryPath & group,
      const RegistryPath & key, float & value, float defval = 0.0f) = 0;
   virtual bool GetConfig(ConfigurationType type, const RegistryPath & group,
      const RegistryPath & key, double & value, double defval = 0.0) = 0;

   virtual bool SetConfig(ConfigurationType type, const RegistryPath & group,
      const RegistryPath & key, const wxString & value) = 0;
   virtual bool SetConfig(ConfigurationType type, const RegistryPath & group,
      const RegistryPath & key, const int & value) = 0;
   virtual bool SetConfig(ConfigurationType type, const RegistryPath & group,
      const RegistryPath & key, const bool & value) = 0;
   virtual bool SetConfig(ConfigurationType type, const RegistryPath & group,
      const RegistryPath & key, const float & value) = 0;
   virtual bool SetConfig(ConfigurationType type, const RegistryPath & group,
      const RegistryPath & key, const double & value) = 0;

   virtual bool RemoveConfigSubgroup(
      ConfigurationType type, const RegistryPath & group) = 0;
   virtual bool RemoveConfig(
      ConfigurationType type, const RegistryPath & group,
      const RegistryPath & key) = 0;
};

#endif // __AUDACITY_CONFIGINTERFACE_H__
