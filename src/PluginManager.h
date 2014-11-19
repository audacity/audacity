/**********************************************************************

  Audacity: A Digital Audio Editor

  PluginManager.h

  Leland Lucius

**********************************************************************/

#ifndef __AUDACITY_PLUGINMANAGER_H__
#define __AUDACITY_PLUGINMANAGER_H__

#include <wx/defs.h>
#include <wx/dynarray.h>
#include <wx/fileconf.h>
#include <wx/string.h>

#include <map>

#include "audacity/EffectInterface.h"
#include "audacity/ImporterInterface.h"
#include "audacity/ModuleInterface.h"
#include "audacity/PluginInterface.h"

///////////////////////////////////////////////////////////////////////////////
//
// PluginDescriptor
//
///////////////////////////////////////////////////////////////////////////////

typedef enum
{
   PluginTypeNone,
   PluginTypeEffect,
   PluginTypeExporter,
   PluginTypeImporter,
   PluginTypeModule,
} PluginType;

// TODO:  Convert this to multiple derived classes
class PluginDescriptor
{
public:
   PluginDescriptor();
   virtual ~PluginDescriptor();

   bool IsInstantiated();
   IdentInterface *GetInstance();
   void SetInstance(IdentInterface *instance);

   PluginType GetPluginType() const;
   void SetPluginType(PluginType type);

   // All plugins

   const wxString & GetID() const;
   const wxString & GetPath() const;
   const wxString & GetName() const;
   const wxString & GetVersion() const;
   const wxString & GetVendor() const;
   const wxString & GetDescription() const;
   const wxString & GetProviderID() const;
   bool IsEnabled() const;

   void SetID(const PluginID & ID);
   void SetPath(const wxString & path);
   void SetName(const wxString & name);
   void SetVersion(const wxString & version);
   void SetVendor(const wxString & vendor);
   void SetDescription(const wxString & description);
   void SetProviderID(const PluginID & providerID);
   void SetEnabled(bool enable);

   wxString GetMenuName() const;

   // Effect plugins only

   EffectType GetEffectType() const;
   const wxString & GetEffectFamily() const;
   bool IsEffectDefault() const;
   bool IsEffectInteractive() const;
   bool IsEffectLegacy() const;
   bool IsEffectRealtime() const;
   bool IsEffectAutomatable() const;

   void SetEffectType(EffectType type);
   void SetEffectFamily(const wxString & family);
   void SetEffectDefault(bool dflt);
   void SetEffectInteractive(bool interactive);
   void SetEffectLegacy(bool legacy);
   void SetEffectRealtime(bool realtime);
   void SetEffectAutomatable(bool automatable);

   // Importer plugins only

   const wxString & GetImporterIdentifier() const;
   const wxString & GetImporterFilterDescription() const;
   const wxArrayString & GetImporterExtensions() const;

   void SetImporterIdentifier(const wxString & identifier);
   void SetImporterFilterDescription(const wxString & filterDesc);
   void SetImporterExtensions(const wxArrayString & extensions);

private:

   // Common

   IdentInterface *mInstance;

   PluginType mPluginType;

   wxString mID;
   wxString mPath;
   wxString mName;
   wxString mVersion;
   wxString mVendor;
   wxString mDescription;
   wxString mProviderID;
   bool mEnabled;

   // Effects

   wxString mEffectFamily;
   EffectType mEffectType;
   bool mEffectInteractive;
   bool mEffectDefault;
   bool mEffectLegacy;
   bool mEffectRealtime;
   bool mEffectAutomatable;

   // Importers

   wxString mImporterIdentifier;
   wxString mImporterFilterDesc;
   wxArrayString mImporterExtensions;
};

///////////////////////////////////////////////////////////////////////////////
//
// PluginManager
//
///////////////////////////////////////////////////////////////////////////////

WX_DECLARE_STRING_HASH_MAP(wxArrayString, ArrayStringMap);

//WX_DECLARE_STRING_HASH_MAP(PluginDescriptor, PluginMap);
typedef std::map<PluginID, PluginDescriptor> PluginMap;

typedef wxArrayString PluginIDList;

class PluginRegistrationDialog;

class PluginManager : public PluginManagerInterface
{
public:
   PluginManager();
   virtual ~PluginManager();

   // PluginManagerInterface implementation

   void RegisterModulePlugin(IdentInterface *module);
   void RegisterEffectPlugin(IdentInterface *provider, EffectIdentInterface *effect);
   void RegisterImporterPlugin(IdentInterface *provider, ImporterInterface *importer);

   void FindFilesInPathList(const wxString & pattern,
                            const wxArrayString & pathList,
                            wxArrayString & files,
                            bool directories = false);

   virtual bool GetSharedConfigSubgroups(const PluginID & ID, const wxString & group, wxArrayString & subgroups);

   virtual bool GetSharedConfig(const PluginID & ID, const wxString & group, const wxString & key, wxString & value, const wxString & defval = _T(""));
   virtual bool GetSharedConfig(const PluginID & ID, const wxString & group, const wxString & key, int & value, int defval = 0);
   virtual bool GetSharedConfig(const PluginID & ID, const wxString & group, const wxString & key, bool & value, bool defval = false);
   virtual bool GetSharedConfig(const PluginID & ID, const wxString & group, const wxString & key, float & value, float defval = 0.0);
   virtual bool GetSharedConfig(const PluginID & ID, const wxString & group, const wxString & key, double & value, double defval = 0.0);
   virtual bool GetSharedConfig(const PluginID & ID, const wxString & group, const wxString & key, sampleCount & value, sampleCount defval = 0);

   virtual bool SetSharedConfig(const PluginID & ID, const wxString & group, const wxString & key, const wxString & value);
   virtual bool SetSharedConfig(const PluginID & ID, const wxString & group, const wxString & key, const int & value);
   virtual bool SetSharedConfig(const PluginID & ID, const wxString & group, const wxString & key, const bool & value);
   virtual bool SetSharedConfig(const PluginID & ID, const wxString & group, const wxString & key, const float & value);
   virtual bool SetSharedConfig(const PluginID & ID, const wxString & group, const wxString & key, const double & value);
   virtual bool SetSharedConfig(const PluginID & ID, const wxString & group, const wxString & key, const sampleCount & value);

   virtual bool RemoveSharedConfigSubgroup(const PluginID & ID, const wxString & group);
   virtual bool RemoveSharedConfig(const PluginID & ID, const wxString & group, const wxString & key);

   virtual bool GetPrivateConfigSubgroups(const PluginID & ID, const wxString & group, wxArrayString & subgroups);

   virtual bool GetPrivateConfig(const PluginID & ID, const wxString & group, const wxString & key, wxString & value, const wxString & defval = _T(""));
   virtual bool GetPrivateConfig(const PluginID & ID, const wxString & group, const wxString & key, int & value, int defval = 0);
   virtual bool GetPrivateConfig(const PluginID & ID, const wxString & group, const wxString & key, bool & value, bool defval = false);
   virtual bool GetPrivateConfig(const PluginID & ID, const wxString & group, const wxString & key, float & value, float defval = 0.0);
   virtual bool GetPrivateConfig(const PluginID & ID, const wxString & group, const wxString & key, double & value, double defval = 0.0);
   virtual bool GetPrivateConfig(const PluginID & ID, const wxString & group, const wxString & key, sampleCount & value, sampleCount defval = 0);

   virtual bool SetPrivateConfig(const PluginID & ID, const wxString & group, const wxString & key, const wxString & value);
   virtual bool SetPrivateConfig(const PluginID & ID, const wxString & group, const wxString & key, const int & value);
   virtual bool SetPrivateConfig(const PluginID & ID, const wxString & group, const wxString & key, const bool & value);
   virtual bool SetPrivateConfig(const PluginID & ID, const wxString & group, const wxString & key, const float & value);
   virtual bool SetPrivateConfig(const PluginID & ID, const wxString & group, const wxString & key, const double & value);
   virtual bool SetPrivateConfig(const PluginID & ID, const wxString & group, const wxString & key, const sampleCount & value);

   virtual bool RemovePrivateConfigSubgroup(const PluginID & ID, const wxString & group);
   virtual bool RemovePrivateConfig(const PluginID & ID, const wxString & group, const wxString & key);

   // PluginManager implementation

   void Initialize();
   void Terminate();

   static PluginManager & Get();

   bool HasType(PluginType type);
   void PurgeType(PluginType type);

   int GetPluginCount(PluginType type);
   const PluginDescriptor *GetPlugin(const PluginID & ID);

   const PluginDescriptor *GetFirstPlugin(PluginType type);
   const PluginDescriptor *GetNextPlugin(PluginType type);

   const PluginDescriptor *GetFirstPluginForEffectType(EffectType type);
   const PluginDescriptor *GetNextPluginForEffectType(EffectType type);

   bool IsRegistered(const PluginID & ID);
   void RegisterPlugin(const wxString & type, const wxString & path);

   bool IsPluginEnabled(const PluginID & ID);
   void EnablePlugin(const PluginID & ID, bool enable);

   const wxString & GetName(const PluginID & ID);
   IdentInterface *GetInstance(const PluginID & ID);
   void SetInstance(const PluginID & ID, IdentInterface *instance);  // TODO: Remove after conversion

   // 
   const PluginID & RegisterLegacyEffectPlugin(EffectIdentInterface *effect);

private:
   bool Load();
   void LoadGroup(const wxChar *group, PluginType type);
   void Save();
   void SaveGroup(const wxChar *group, PluginType type);

   void CheckForUpdates(bool forceRescan);
   void DisableMissing();
   wxArrayString IsNewOrUpdated(const wxArrayString & paths);

   PluginDescriptor & CreatePlugin(IdentInterface *ident, PluginType type);

   bool GetSubgroups(const wxString & group, wxArrayString & subgroups);

   bool GetConfig(const wxString & key, wxString & value, const wxString & defval = L"");
   bool GetConfig(const wxString & key, int & value, int defval = 0);
   bool GetConfig(const wxString & key, bool & value, bool defval = false);
   bool GetConfig(const wxString & key, float & value, float defval = 0.0);
   bool GetConfig(const wxString & key, double & value, double defval = 0.0);
   bool GetConfig(const wxString & key, sampleCount & value, sampleCount defval = 0);

   bool SetConfig(const wxString & key, const wxString & value);
   bool SetConfig(const wxString & key, const int & value);
   bool SetConfig(const wxString & key, const bool & value);
   bool SetConfig(const wxString & key, const float & value);
   bool SetConfig(const wxString & key, const double & value);
   bool SetConfig(const wxString & key, const sampleCount & value);

   wxString SharedGroup(const PluginID & ID, const wxString & group);
   wxString SharedKey(const PluginID & ID, const wxString & group, const wxString & key);
   wxString PrivateGroup(const PluginID & ID, const wxString & group);
   wxString PrivateKey(const PluginID & ID, const wxString & group, const wxString & key);

   // The PluginID must be kept unique.  Since the wxFileConfig class does not preserve
   // case, we use base64 encoding.
   wxString ConvertID(const PluginID & ID);
   wxString b64encode(const void *in, int len);
   int b64decode(wxString in, void *out);

private:
   static PluginManager mInstance;

   bool IsDirty();
   void SetDirty(bool dirty = true);
   wxFileConfig *mConfig;

   bool mDirty;
   int mCurrentIndex;

   PluginMap mPlugins;
   PluginMap::iterator mPluginsIter;

   friend class PluginRegistrationDialog;
};

#endif /* __AUDACITY_PLUGINMANAGER_H__ */
