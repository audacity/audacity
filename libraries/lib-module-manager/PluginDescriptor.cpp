/**********************************************************************

  Audacity: A Digital Audio Editor

  PluginDescriptor.cpp

  Split from PluginManager.cpp

**********************************************************************/


///////////////////////////////////////////////////////////////////////////////
//
// Plugindescriptor
//
///////////////////////////////////////////////////////////////////////////////

#include "PluginDescriptor.h"

#include "EffectInterface.h"
#include "ModuleManager.h"


PluginType PluginDescriptor::GetPluginType() const
{
   return mPluginType;
}

const PluginID & PluginDescriptor::GetID() const
{
   return mID;
}

const PluginID & PluginDescriptor::GetProviderID() const
{
   return mProviderID;
}

const PluginPath & PluginDescriptor::GetPath() const
{
   return mPath;
}

const ComponentInterfaceSymbol & PluginDescriptor::GetSymbol() const
{
   return mSymbol;
}

const wxString& PluginDescriptor::GetUntranslatedVersion() const
{
   return mVersion;
}

const wxString& PluginDescriptor::GetVendor() const
{
   return mVendor;
}

bool PluginDescriptor::IsEnabled() const
{
   return mEnabled;
}

bool PluginDescriptor::IsValid() const
{
   return mValid;
}

void PluginDescriptor::SetPluginType(PluginType type)
{
   mPluginType = type;
}

void PluginDescriptor::SetID(const PluginID & ID)
{
   mID = ID;
}

void PluginDescriptor::SetProviderID(const PluginID & providerID)
{
   mProviderID = providerID;
}

void PluginDescriptor::SetPath(const PluginPath & path)
{
   mPath = path;
}

void PluginDescriptor::SetSymbol(const ComponentInterfaceSymbol & symbol)
{
   mSymbol = symbol;
}

void PluginDescriptor::SetVersion(const wxString & version)
{
   mVersion = version;
}

void PluginDescriptor::SetVendor(const wxString & vendor)
{
   mVendor = vendor;
}

void PluginDescriptor::SetEnabled(bool enable)
{
   mEnabled = enable;
}

void PluginDescriptor::SetValid(bool valid)
{
   mValid = valid;
}

// Effects

wxString PluginDescriptor::GetEffectFamily() const
{
   return mEffectFamily;
}

EffectType PluginDescriptor::GetEffectType() const
{
   return mEffectType;
}

bool PluginDescriptor::IsEffectInteractive() const
{
   return mEffectInteractive;
}

bool PluginDescriptor::IsEffectDefault() const
{
   return mEffectDefault;
}

bool PluginDescriptor::IsEffectLegacy() const
{
   return mEffectLegacy;
}

bool PluginDescriptor::IsEffectRealtime() const
{
   return mEffectRealtime;
}

bool PluginDescriptor::IsEffectAutomatable() const
{
   return mEffectAutomatable;
}

void PluginDescriptor::SetEffectFamily(const wxString & family)
{
   mEffectFamily = family;
}

void PluginDescriptor::SetEffectType(EffectType type)
{
   mEffectType = type;
}

void PluginDescriptor::SetEffectInteractive(bool interactive)
{
   mEffectInteractive = interactive;
}

void PluginDescriptor::SetEffectDefault(bool dflt)
{
   mEffectDefault = dflt;
}

void PluginDescriptor::SetEffectLegacy(bool legacy)
{
   mEffectLegacy = legacy;
}

void PluginDescriptor::SetEffectRealtime(bool realtime)
{
   mEffectRealtime = realtime;
}

void PluginDescriptor::SetEffectAutomatable(bool automatable)
{
   mEffectAutomatable = automatable;
}

// Importer

const wxString & PluginDescriptor::GetImporterIdentifier() const
{
   return mImporterIdentifier;
}

void PluginDescriptor::SetImporterIdentifier(const wxString & identifier)
{
   mImporterIdentifier = identifier;
}

const FileExtensions & PluginDescriptor::GetImporterExtensions()
   const
{
   return mImporterExtensions;
}

void PluginDescriptor::SetImporterExtensions( FileExtensions extensions )
{
   mImporterExtensions = std::move( extensions );
}
