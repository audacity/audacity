/**********************************************************************

  Audacity: A Digital Audio Editor

  PluginManager.h

  Leland Lucius

**********************************************************************/

#ifndef __AUDACITY_PLUGINMANAGER_H__
#define __AUDACITY_PLUGINMANAGER_H__

#include <wx/defs.h>

#include "wxArrayStringEx.h"
#include <functional>
#include <map>
#include <memory>

#include "EffectInterface.h"
#include "PluginInterface.h"
#include "PluginDescriptor.h"

class wxArrayString;
class FileConfig;

///////////////////////////////////////////////////////////////////////////////
//
// PluginManager
//
///////////////////////////////////////////////////////////////////////////////

typedef std::map<PluginID, PluginDescriptor> PluginMap;

typedef wxArrayString PluginIDs;

class PluginRegistrationDialog;

class MODULE_MANAGER_API PluginManager final : public PluginManagerInterface
{
public:

   RegistryPath GetPluginEnabledSetting( const PluginID &ID ) const;
   RegistryPath GetPluginEnabledSetting( const PluginDescriptor &desc ) const;

   // PluginManagerInterface implementation

   bool IsPluginRegistered(
      const PluginPath &path, const TranslatableString *pSymbol) override;

   bool IsPluginLoaded(const wxString& ID) const;
   
   void RegisterPlugin(PluginDescriptor&& desc);
   const PluginID & RegisterPlugin(PluginProvider *provider) override;
   const PluginID & RegisterPlugin(PluginProvider *provider, ComponentInterface *command);
   const PluginID & RegisterPlugin(PluginProvider *provider, EffectDefinitionInterface *effect, int type) override;

   void FindFilesInPathList(const wxString & pattern,
                                    const FilePaths & pathList,
                                    FilePaths & files,
                                    bool directories = false) override;

   bool HasConfigGroup(ConfigurationType type, const PluginID & ID,
      const RegistryPath & group) /* not override */;
   bool GetConfigSubgroups(ConfigurationType type, const PluginID & ID,
      const RegistryPath & group,
      RegistryPaths & subgroups) override;

   bool HasConfigValue(ConfigurationType type, const PluginID & ID,
      const RegistryPath & group, const RegistryPath & key) override;

   bool GetConfigValue(ConfigurationType type, const PluginID & ID,
      const RegistryPath & group, const RegistryPath & key,
      ConfigReference var, ConfigConstReference defval) override;

   bool SetConfigValue(ConfigurationType type, const PluginID & ID,
      const RegistryPath & group, const RegistryPath & key,
      ConfigConstReference value) override;

   bool RemoveConfigSubgroup(ConfigurationType type,
      const PluginID & ID, const RegistryPath & group) override;
   bool RemoveConfig(ConfigurationType type, const PluginID & ID,
      const RegistryPath & group, const RegistryPath & key) override;

   // PluginManager implementation

   // Initialization must inject a factory to make a concrete subtype of
   // FileConfig
   using FileConfigFactory = std::function<
      std::unique_ptr<FileConfig>(const FilePath &localFilename ) >;
   /*! @pre `factory != nullptr` */
   void Initialize(FileConfigFactory factory);
   void Terminate();

   bool DropFile(const wxString &fileName);

   static PluginManager & Get();

   static PluginID GetID(PluginProvider *provider);
   static PluginID GetID(ComponentInterface *command);
   static PluginID OldGetID(const EffectDefinitionInterface* effect);
   static PluginID GetID(const EffectDefinitionInterface* effect);
   //! Parse English effect name from the result of
   //! GetID(const EffectDefinitionInterface*)
   static Identifier GetEffectNameFromID(const PluginID &ID);

   // This string persists in configuration files
   // So config compatibility will break if it is changed across Audacity versions
   static wxString GetPluginTypeString(PluginType type);

   int GetPluginCount(PluginType type);
   const PluginDescriptor *GetPlugin(const PluginID & ID) const;

   //! @name iteration over plugins of certain types, supporting range-for syntax
   //! @{
   class MODULE_MANAGER_API Iterator {
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
         return mIterator != mPm.mRegisteredPlugins.end();
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
   ComponentInterface *Load(const PluginID & ID);

   /**
    * \brief Ensures that all currently registered plugins still exist
    * and scans for new ones.
    * \return Array of pairs, first element of which is provider id and the second is module path
    */
   std::vector<std::pair<wxString, wxString>> CheckPluginUpdates();

   //! Used only by Nyquist Workbench module
   const PluginID & RegisterPlugin(
      std::unique_ptr<EffectDefinitionInterface> effect, PluginType type );
   void UnregisterPlugin(const PluginID & ID);

   //! Load from preferences
   void Load();
   //! Save to preferences
   void Save();

   //! What is the plugin registry version number now in the file?
   //! (Save() updates it)
   const PluginRegistryVersion &GetRegistryVersion() const override;

private:
   // private! Use Get()
   PluginManager();
   ~PluginManager();

   void InitializePlugins();

   void LoadGroup(FileConfig *pRegistry, PluginType type);
   void SaveGroup(FileConfig *pRegistry, PluginType type);

   PluginDescriptor & CreatePlugin(const PluginID & id, ComponentInterface *ident, PluginType type);

   FileConfig *GetSettings();

   bool HasGroup(const RegistryPath & group);
   bool GetSubgroups(const RegistryPath & group, RegistryPaths & subgroups);

   bool HasConfigValue(const RegistryPath & key);
   bool GetConfigValue(const RegistryPath & key, ConfigReference var,
         ConfigConstReference defval);
   bool SetConfigValue(const RegistryPath & key, ConfigConstReference value);

   /* Return values are keys for lookup in a config file */
   RegistryPath SettingsPath(ConfigurationType type, const PluginID & ID);
   RegistryPath Group(ConfigurationType type, const PluginID & ID,
      const RegistryPath & group);
   RegistryPath Key(ConfigurationType type, const PluginID & ID,
      const RegistryPath & group, const RegistryPath & key);

   // The PluginID must be kept unique.  Since the wxFileConfig class does not preserve
   // case, we use base64 encoding.
   wxString ConvertID(const PluginID & ID);

private:
   friend std::default_delete<PluginManager>;
   static std::unique_ptr<PluginManager> mInstance;

   bool IsDirty();
   void SetDirty(bool dirty = true);
   std::unique_ptr<FileConfig> mSettings;

   bool mDirty;
   int mCurrentIndex;

   PluginMap mRegisteredPlugins;
   std::map<PluginID, std::unique_ptr<ComponentInterface>> mLoadedInterfaces;

   PluginRegistryVersion mRegver;
};

// Defining these special names in the low-level PluginManager.h
// is unfortunate
// Internal name should be stable across versions
#define NYQUIST_PROMPT_ID wxT("Nyquist Prompt")
// User-visible name might change in later versions
#define NYQUIST_PROMPT_NAME XO("Nyquist Prompt")

// Latest version of the plugin registry config
constexpr auto REGVERCUR = "1.2";

#endif /* __AUDACITY_PLUGINMANAGER_H__ */
