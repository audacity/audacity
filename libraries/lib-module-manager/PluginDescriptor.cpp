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

PluginDescriptor::PluginDescriptor()
{
   mPluginType = PluginTypeNone;
   mEnabled = false;
   mValid = false;
   mInstance = nullptr;

   mEffectType = EffectTypeNone;
   mEffectInteractive = false;
   mEffectDefault = false;
   mEffectLegacy = false;
   mEffectRealtime = false;
   mEffectAutomatable = false;
}

PluginDescriptor::~PluginDescriptor()
{
}

PluginDescriptor &PluginDescriptor::operator =(PluginDescriptor &&) = default;

bool PluginDescriptor::IsLoaded() const
{
   return mInstance != nullptr;
}

ComponentInterface *PluginDescriptor::Load()
{
   if (!mInstance)
   {
      if (GetPluginType() == PluginTypeModule)
         mInstance = ModuleManager::Get().CreateProviderInstance(GetID(), GetPath());
      else
      {
         muInstance =
            ModuleManager::Get().LoadPlugin(GetProviderID(), GetPath());
         mInstance = muInstance.get();
      }
   }

   return mInstance;
}

void PluginDescriptor::Set(std::unique_ptr<ComponentInterface> instance)
{
   muInstance = std::move(instance);
   mInstance = muInstance.get();
}

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

wxString PluginDescriptor::GetUntranslatedVersion() const
{
   return mVersion;
}

wxString PluginDescriptor::GetVendor() const
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
