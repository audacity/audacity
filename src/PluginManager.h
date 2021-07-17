/**********************************************************************

  Audacity: A Digital Audio Editor

  PluginManager.h

  Leland Lucius

**********************************************************************/

#ifndef __AUDACITY_PLUGINMANAGER_H__
#define __AUDACITY_PLUGINMANAGER_H__

#include <wx/defs.h>

#include "wxArrayStringEx.h"
#include <map>
#include <memory>

#include "EffectInterface.h"
#include "PluginInterface.h"

class wxArrayString;
class FileConfig;

///////////////////////////////////////////////////////////////////////////////
//
// PluginDescriptor
//
///////////////////////////////////////////////////////////////////////////////

typedef enum : unsigned {
   PluginTypeNone = 0,          // 2.1.0 placeholder entries...not used by 2.1.1 or greater
   PluginTypeStub =1,               // Used for plugins that have not yet been registered
   PluginTypeEffect =1<<1,
   PluginTypeAudacityCommand=1<<2,
   PluginTypeExporter=1<<3,
   PluginTypeImporter=1<<4,
   PluginTypeModule=1<<5,
} PluginType;

// TODO:  Convert this to multiple derived classes
class AUDACITY_DLL_API PluginDescriptor
{
public:
   PluginDescriptor();
   PluginDescriptor &operator =(PluginDescriptor &&);
   virtual ~PluginDescriptor();

   bool IsInstantiated() const;

   PluginType GetPluginType() const;

   // All plugins

   // These return untranslated strings
   const wxString & GetID() const;
   const wxString & GetProviderID() const;
   const PluginPath & GetPath() const;
   const ComponentInterfaceSymbol & GetSymbol() const;

   wxString GetUntranslatedVersion() const;
   // There is no translated version

   wxString GetVendor() const;

   bool IsEnabled() const;
   bool IsValid() const;

   void SetEnabled(bool enable);
   void SetValid(bool valid);

   // Effect plugins only

   // Internal string only, no translated counterpart!
   // (Use Effect::GetFamilyName instead)
   // This string persists in configuration files
   // So config compatibility will break if it is changed across Audacity versions
   wxString GetEffectFamily() const;

   EffectType GetEffectType() const;
   bool IsEffectDefault() const;
   bool IsEffectInteractive() const;
   bool IsEffectLegacy() const;
   bool IsEffectRealtime() const;
   bool IsEffectAutomatable() const;

   // Importer plugins only

   const wxString & GetImporterIdentifier() const;
   const TranslatableString & GetImporterFilterDescription() const;
   const FileExtensions & GetImporterExtensions() const;

private:
   friend class PluginManager;

   ComponentInterface *GetInstance();
   void SetInstance(std::unique_ptr<ComponentInterface> instance);

   void SetPluginType(PluginType type);

   // These should be passed an untranslated value
   void SetID(const PluginID & ID);
   void SetProviderID(const PluginID & providerID);
   void SetPath(const PluginPath & path);
   void SetSymbol(const ComponentInterfaceSymbol & symbol);

   // These should be passed an untranslated value wrapped in XO() so
   // the value will still be extracted for translation
   void SetVersion(const wxString & version);
   void SetVendor(const wxString & vendor);

   // "family" should be an untranslated string wrapped in wxT()
   void SetEffectFamily(const wxString & family);
   void SetEffectType(EffectType type);
   void SetEffectDefault(bool dflt);
   void SetEffectInteractive(bool interactive);
   void SetEffectLegacy(bool legacy);
   void SetEffectRealtime(bool realtime);
   void SetEffectAutomatable(bool automatable);

   void SetImporterIdentifier(const wxString & identifier);
   void SetImporterFilterDescription(const TranslatableString & filterDesc);
   void SetImporterExtensions(FileExtensions extensions);

   // Common

   // Among other purposes, PluginDescriptor acts as the resource handle,
   // or smart pointer, to a resource created in a plugin library, and is responsible
   // for a cleanup of this pointer.
   std::unique_ptr<ComponentInterface> muInstance; // may be null for a module
   ComponentInterface *mInstance;

   PluginType mPluginType;

   wxString mID;
   PluginPath mPath;
   ComponentInterfaceSymbol mSymbol;
   wxString mVersion;
   wxString mVendor;
   wxString mProviderID;
   bool mEnabled;
   bool mValid;

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
   FileExtensions mImporterExtensions;
};

///////////////////////////////////////////////////////////////////////////////
//
// PluginManager
//
///////////////////////////////////////////////////////////////////////////////

typedef std::map<PluginID, PluginDescriptor> PluginMap;

typedef wxArrayString PluginIDs;

class PluginRegistrationDialog;

class AUDACITY_DLL_API PluginManager final : public PluginManagerInterface
{
public:

   RegistryPath GetPluginEnabledSetting( const PluginID &ID ) const;
   RegistryPath GetPluginEnabledSetting( const PluginDescriptor &desc ) const;

   // PluginManagerInterface implementation

   bool IsPluginRegistered(
      const PluginPath &path, const TranslatableString *pSymbol) override;

   const PluginID & RegisterPlugin(ModuleInterface *module) override;
   const PluginID & RegisterPlugin(ModuleInterface *provider, ComponentInterface *command);
   const PluginID & RegisterPlugin(ModuleInterface *provider, EffectDefinitionInterface *effect, int type) override;

   void FindFilesInPathList(const wxString & pattern,
                                    const FilePaths & pathList,
                                    FilePaths & files,
                                    bool directories = false) override;

   bool HasSharedConfigGroup(const PluginID & ID, const RegistryPath & group) /* not override */;
   bool GetSharedConfigSubgroups(const PluginID & ID, const RegistryPath & group, RegistryPaths &subgroups) override;

   bool GetSharedConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, wxString & value, const wxString & defval = _T("")) override;
   bool GetSharedConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, int & value, int defval = 0) override;
   bool GetSharedConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, bool & value, bool defval = false) override;
   bool GetSharedConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, float & value, float defval = 0.0) override;
   bool GetSharedConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, double & value, double defval = 0.0) override;

   bool SetSharedConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, const wxString & value) override;
   bool SetSharedConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, const int & value) override;
   bool SetSharedConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, const bool & value) override;
   bool SetSharedConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, const float & value) override;
   bool SetSharedConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, const double & value) override;

   bool RemoveSharedConfigSubgroup(const PluginID & ID, const RegistryPath & group) override;
   bool RemoveSharedConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key) override;

   bool HasPrivateConfigGroup(const PluginID & ID, const RegistryPath & group) /* not override */;
   bool GetPrivateConfigSubgroups(const PluginID & ID, const RegistryPath & group, RegistryPaths &subgroups) override;

   bool GetPrivateConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, wxString & value, const wxString & defval = _T("")) override;
   bool GetPrivateConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, int & value, int defval = 0) override;
   bool GetPrivateConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, bool & value, bool defval = false) override;
   bool GetPrivateConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, float & value, float defval = 0.0) override;
   bool GetPrivateConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, double & value, double defval = 0.0) override;

   bool SetPrivateConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, const wxString & value) override;
   bool SetPrivateConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, const int & value) override;
   bool SetPrivateConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, const bool & value) override;
   bool SetPrivateConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, const float & value) override;
   bool SetPrivateConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, const double & value) override;

   bool RemovePrivateConfigSubgroup(const PluginID & ID, const RegistryPath & group) override;
   bool RemovePrivateConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key) override;

   // PluginManager implementation

   void Initialize();
   void Terminate();

   bool DropFile(const wxString &fileName);

   static PluginManager & Get();

   static PluginID GetID(ModuleInterface *module);
   static PluginID GetID(ComponentInterface *command);
   static PluginID GetID(EffectDefinitionInterface *effect);

   // This string persists in configuration files
   // So config compatibility will break if it is changed across Audacity versions
   static wxString GetPluginTypeString(PluginType type);

   int GetPluginCount(PluginType type);
   const PluginDescriptor *GetPlugin(const PluginID & ID) const;

   //! @name iteration over plugins of certain types, supporting range-for syntax
   //! @{
   class Iterator {
   public:
      //! Iterates all, even disabled
      explicit Iterator(PluginManager &manager);
      //! Iterates only enabled and matching plugins, with family enabled too if an effect
      Iterator(PluginManager &manager,
         int pluginType //!< bitwise or of values in PluginType
      );
      //! Iterates only enabled and matching effects, with family enabled too
      Iterator(PluginManager &manager, EffectType type);
      bool operator != (int) const {
         return mIterator != mPm.mPlugins.end();
      }
      Iterator &operator ++ ();
      auto &operator *() const { return mIterator->second; }
   private:
      void Advance(bool incrementing);
      const PluginManager &mPm;
      PluginMap::iterator mIterator;
      EffectType mEffectType{ EffectTypeNone };
      int mPluginType{ PluginTypeNone };
   };
   struct Range {
      Iterator first;
      Iterator begin() const { return first; }
      int end() const { return 0; }
   };

   Range AllPlugins() { return { Iterator{ *this } }; }
   Range PluginsOfType(int type) { return { Iterator{ *this, type } }; }
   Range EffectsOfType(EffectType type) { return { Iterator{ *this, type } }; }
   //! @}

   bool IsPluginEnabled(const PluginID & ID);
   void EnablePlugin(const PluginID & ID, bool enable);

   const ComponentInterfaceSymbol & GetSymbol(const PluginID & ID);
   ComponentInterface *GetInstance(const PluginID & ID);

   void CheckForUpdates(bool bFast = false);

   //! Used only by Nyquist Workbench module
   const PluginID & RegisterPlugin(
      std::unique_ptr<EffectDefinitionInterface> effect, PluginType type );
   void UnregisterPlugin(const PluginID & ID);

   //! Load from preferences
   void Load();
   //! Save to preferences
   void Save();

private:
   // private! Use Get()
   PluginManager();
   ~PluginManager();

   void LoadGroup(FileConfig *pRegistry, PluginType type);
   void SaveGroup(FileConfig *pRegistry, PluginType type);

   PluginDescriptor & CreatePlugin(const PluginID & id, ComponentInterface *ident, PluginType type);

   FileConfig *GetSettings();

   bool HasGroup(const RegistryPath & group);
   bool GetSubgroups(const RegistryPath & group, RegistryPaths & subgroups);

   bool GetConfig(const RegistryPath & key, wxString & value, const wxString & defval = L"");
   bool GetConfig(const RegistryPath & key, int & value, int defval = 0);
   bool GetConfig(const RegistryPath & key, bool & value, bool defval = false);
   bool GetConfig(const RegistryPath & key, float & value, float defval = 0.0);
   bool GetConfig(const RegistryPath & key, double & value, double defval = 0.0);

   bool SetConfig(const RegistryPath & key, const wxString & value);
   bool SetConfig(const RegistryPath & key, const int & value);
   bool SetConfig(const RegistryPath & key, const bool & value);
   bool SetConfig(const RegistryPath & key, const float & value);
   bool SetConfig(const RegistryPath & key, const double & value);

   /* Return values are keys for lookup in a config file */
   RegistryPath SettingsPath(const PluginID & ID, bool shared);
   RegistryPath SharedGroup(const PluginID & ID, const RegistryPath & group);
   RegistryPath SharedKey(const PluginID & ID, const RegistryPath & group, const RegistryPath & key);
   RegistryPath PrivateGroup(const PluginID & ID, const RegistryPath & group);
   RegistryPath PrivateKey(const PluginID & ID, const RegistryPath & group, const RegistryPath & key);

   // The PluginID must be kept unique.  Since the wxFileConfig class does not preserve
   // case, we use base64 encoding.
   wxString ConvertID(const PluginID & ID);
   wxString b64encode(const void *in, int len);
   int b64decode(const wxString &in, void *out);

private:
   friend std::default_delete<PluginManager>;
   static std::unique_ptr<PluginManager> mInstance;

   bool IsDirty();
   void SetDirty(bool dirty = true);
   std::unique_ptr<FileConfig> mSettings;

   bool mDirty;
   int mCurrentIndex;

   PluginMap mPlugins;
};

// Defining these special names in the low-level PluginManager.h
// is unfortunate
// Internal name should be stable across versions
#define NYQUIST_PROMPT_ID wxT("Nyquist Prompt")
// User-visible name might change in later versions
#define NYQUIST_PROMPT_NAME XO("Nyquist Prompt")

#endif /* __AUDACITY_PLUGINMANAGER_H__ */
