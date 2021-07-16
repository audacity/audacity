/**********************************************************************

   Audacity: A Digital Audio Editor

   PluginInterface.h

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

#ifndef __AUDACITY_PLUGININTERFACE_H__
#define __AUDACITY_PLUGININTERFACE_H__

#include "ConfigInterface.h"
#include "EffectInterface.h"
#include "ComponentInterface.h"
#include "Identifier.h"

class ModuleInterface;
using PluginID = wxString;
using PluginIDs = wxArrayString;

class COMPONENTS_API PluginManagerInterface /* not final */
{
public:

   virtual ~PluginManagerInterface();

   static const PluginID &DefaultRegistrationCallback(
      ModuleInterface *provider, ComponentInterface *ident );
   static const PluginID &AudacityCommandRegistrationCallback(
      ModuleInterface *provider, ComponentInterface *ident );

   //! Was the plugin registry already populated for a path (maybe from loading the config file)?
   /*!
   @param path an identifier for the plug-in with meaning defined by provider; not always a file path
   @param pName if supplied, a correction for the user visible name associated with the plug-in, if it is
    registered already.  (Needed because the configuration file only stores an internal name.)
    */
   virtual bool IsPluginRegistered(
      const PluginPath & path,
      const TranslatableString *pName = nullptr) = 0;

   virtual const PluginID & RegisterPlugin(ModuleInterface *module) = 0;
   virtual const PluginID & RegisterPlugin(ModuleInterface *provider, EffectDefinitionInterface *effect, int type) = 0;

   virtual void FindFilesInPathList(const wxString & pattern,
                                    const FilePaths & pathList,
                                    FilePaths & files,
                                    bool directories = false) = 0;

   virtual bool GetSharedConfigSubgroups(const PluginID & ID, const RegistryPath & group, RegistryPaths & subgroups) = 0;

   virtual bool GetSharedConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, wxString & value, const wxString & defval = wxString()) = 0;
   virtual bool GetSharedConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, int & value, int defval = 0) = 0;
   virtual bool GetSharedConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, bool & value, bool defval = false) = 0;
   virtual bool GetSharedConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, float & value, float defval = 0.0) = 0;
   virtual bool GetSharedConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, double & value, double defval = 0.0) = 0;

   virtual bool SetSharedConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, const wxString & value) = 0;
   virtual bool SetSharedConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, const int & value) = 0;
   virtual bool SetSharedConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, const bool & value) = 0;
   virtual bool SetSharedConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, const float & value) = 0;
   virtual bool SetSharedConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, const double & value) = 0;

   virtual bool RemoveSharedConfigSubgroup(const PluginID & ID, const RegistryPath & group) = 0;
   virtual bool RemoveSharedConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key) = 0;

   virtual bool GetPrivateConfigSubgroups(const PluginID & ID, const RegistryPath & group, RegistryPaths & subgroups) = 0;

   virtual bool GetPrivateConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, wxString & value, const wxString & defval = wxString()) = 0;
   virtual bool GetPrivateConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, int & value, int defval = 0) = 0;
   virtual bool GetPrivateConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, bool & value, bool defval = false) = 0;
   virtual bool GetPrivateConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, float & value, float defval = 0.0) = 0;
   virtual bool GetPrivateConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, double & value, double defval = 0.0) = 0;

   virtual bool SetPrivateConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, const wxString & value) = 0;
   virtual bool SetPrivateConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, const int & value) = 0;
   virtual bool SetPrivateConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, const bool & value) = 0;
   virtual bool SetPrivateConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, const float & value) = 0;
   virtual bool SetPrivateConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, const double & value) = 0;

   virtual bool RemovePrivateConfigSubgroup(const PluginID & ID, const RegistryPath & group) = 0;
   virtual bool RemovePrivateConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key) = 0;
};

#endif // __AUDACITY_PLUGININTERFACE_H__
