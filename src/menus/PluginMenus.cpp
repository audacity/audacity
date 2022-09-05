

#include "AudioIO.h"
#include "../Benchmark.h"
#include "../commands/CommandDispatch.h"
#include "../CommonCommandFlags.h"
#include "../Journal.h"
#include "../Menus.h"
#include "PluginManager.h"
#include "../PluginRegistrationDialog.h"
#include "Prefs.h"
#include "Project.h"
#include "../ProjectSettings.h"
#include "../ProjectWindow.h"
#include "../ProjectWindows.h"
#include "../ProjectSelectionManager.h"
#include "RealtimeEffectPanel.h"
#include "../toolbars/ToolManager.h"
#include "../TrackPanelAx.h"
#include "TempDirectory.h"
#include "UndoManager.h"
#include "../commands/CommandContext.h"
#include "../commands/CommandManager.h"
#include "../effects/EffectManager.h"
#include "../effects/EffectUI.h"
#include "RealtimeEffectManager.h"
#include "../prefs/EffectsPrefs.h"
#include "../prefs/PrefsDialog.h"
#include "../widgets/AudacityMessageBox.h"

#include <wx/stdpaths.h>

#include "XMLFileReader.h"

// private helper classes and functions
namespace {

using EffectsMenuGroups = std::vector<std::pair<TranslatableString, std::vector<TranslatableString>>>;

struct EffectsMenuGroupsHandler : XMLTagHandler
{
   struct EffectsHandler : XMLTagHandler
   {
      std::optional<std::string> textContent;
      std::vector<TranslatableString>& effects;

      EffectsHandler(std::vector<TranslatableString>& effects) : effects(effects) { }

      bool HandleXMLTag(const std::string_view& tag, const AttributesList& attrs) override { return true; }
      void HandleXMLContent(const std::string_view& text) override { textContent = text; }
      void HandleXMLEndTag(const std::string_view& tag) override
      {
         if(textContent.has_value() && tag == "Effect")
            effects.emplace_back(TranslatableString { *textContent, {} });
         textContent.reset();
      }
      XMLTagHandler* HandleXMLChild(const std::string_view& tag) override
      {
         if(tag == "Effect")
            return this;
         return nullptr;
      }

   };

   struct GroupHandler : XMLTagHandler
   {
      std::optional<std::string> textContent;
      std::unique_ptr<EffectsHandler> effectsHandler;
      std::pair<TranslatableString, std::vector<TranslatableString>>& group;

      GroupHandler(std::pair<TranslatableString, std::vector<TranslatableString>>& group) : group(group) { }

      bool HandleXMLTag(const std::string_view& tag, const AttributesList& attrs) override { return true; }
      void HandleXMLContent(const std::string_view& text) override { textContent = text; }
      void HandleXMLEndTag(const std::string_view& tag) override
      {
         if(textContent.has_value() && tag == "Name")
            group.first = TranslatableString { *textContent, { } };
         textContent.reset();
      }
      XMLTagHandler* HandleXMLChild(const std::string_view& tag) override
      {
         if(tag == "Effects")
         {
            effectsHandler = std::make_unique<EffectsHandler>(group.second);
            return &*effectsHandler;
         }
         if(tag == "Name")
            return this;

         return nullptr;
      }
   };

   EffectsMenuGroups& groups;
   std::unique_ptr<GroupHandler> groupHandler;

   EffectsMenuGroupsHandler(EffectsMenuGroups& groups) : groups(groups) { }

   bool HandleXMLTag(const std::string_view& tag, const AttributesList& attrs) override { return true; }

   XMLTagHandler* HandleXMLChild(const std::string_view& tag) override
   {
      if(tag == "Group")
      {
         groups.resize(groups.size() + 1);
         groupHandler = std::make_unique<GroupHandler>(groups.back());
         return &*groupHandler;
      }
      return nullptr;
   }
};

EffectsMenuGroups LoadEffectsMenuGroups(const wxString& path)
{
   EffectsMenuGroups result;
   EffectsMenuGroupsHandler handler(result);

   XMLFileReader reader;
   reader.Parse(&handler, path);
   return result;
}

enum class GroupBy
{
   Publisher,
   Type
};

enum class SortBy
{
   Name,
   PublisherName,
   TypeName
};
   
// Some weird special case stuff just for Noise Reduction so that there is
// more informative help
CommandFlag FixBatchFlags(CommandFlag batchflags, const PluginDescriptor* plug)
{
   if ( plug->GetSymbol().Msgid() == XO( "Noise Reduction" ) )
      return ( batchflags | NoiseReductionTimeSelectedFlag() ) & ~TimeSelectedFlag();
   return batchflags;
}

   
bool IsEnabledPlugin(const PluginDescriptor* plug)
{
   if( PluginManager::Get().IsPluginLoaded(plug->GetID()) && EffectManager::Get().IsHidden(plug->GetID()) )
         return false;
   if ( !plug->IsEnabled() ){
      return false;// don't add to menus!
   }
   return true;
}

bool IsDefaultPlugin(const PluginDescriptor* plug)
{
   if (plug->IsEffectDefault()
#ifdef EXPERIMENTAL_DA
      // Move Nyquist prompt into nyquist group.
      && (plug->GetSymbol() !=
            ComponentInterfaceSymbol("Nyquist Effects Prompt"))
      && (plug->GetSymbol() != ComponentInterfaceSymbol("Nyquist Tools Prompt"))
      && (plug->GetSymbol() != ComponentInterfaceSymbol(NYQUIST_PROMPT_ID))
#endif
      )
      return true;
   return false;
}

bool IsBundledPlugin(const PluginDescriptor* plug)
{
   if(IsDefaultPlugin(plug))
      return true;
   auto applicationResourcePath = wxFileName(FileNames::ResourcesDir());
   auto pluginPath = wxFileName(plug->GetPath());
   pluginPath.MakeAbsolute();
   return pluginPath.GetPath().StartsWith(applicationResourcePath.GetPath());
}

auto MakeGroupsFilter(const EffectsMenuGroups& list) -> auto
{
   return [=](const PluginDescriptor* plug)
   {
      if(!IsEnabledPlugin(plug))
         return false;

      for(auto& p : list)
      {
         for(auto& name : p.second)
         {
            if(name == plug->GetSymbol().Msgid())
               return true;
         }
      }
      return false;
   };
}

// Forward-declared function has its definition below with OnEffect in view
void AddEffectMenuItemGroup(
   MenuTable::BaseItemPtrs &table,
   const TranslatableStrings & names,
   const PluginIDs & plugs,
   const std::vector<CommandFlag> & flags,
   bool useSubgroups);

   
void AddGroupedEffectMenuItems(
   MenuTable::BaseItemPtrs &table,
   std::vector<const PluginDescriptor*> & plugs,
   CommandFlag batchflags,
   CommandFlag realflags,
   GroupBy groupBy,
   bool useSubgroups)
{
   TranslatableString last;
   TranslatableString current;

   size_t pluginCnt = plugs.size();

   TranslatableStrings groupNames;
   PluginIDs groupPlugs;
   std::vector<CommandFlag> groupFlags;

   for (size_t i = 0; i < pluginCnt; i++)
   {
      const PluginDescriptor *plug = plugs[i];

      auto name = plug->GetSymbol().Msgid();

      if (plug->IsEffectInteractive())
         name += XO("...");
      
      if (groupBy == GroupBy::Publisher/*wxT("groupby:publisher")*/)
      {
         current = EffectManager::Get().GetVendorName(plug->GetID());
         if (current.empty())
         {
            current = XO("Unknown");
         }
      }
      else if (groupBy == GroupBy::Type /*wxT("groupby:type")*/)
      {
         current = EffectManager::Get().GetEffectFamilyName(plug->GetID());
         if (current.empty())
         {
            current = XO("Unknown");
         }
      }

      if (current != last)
      {
         using namespace MenuTable;
         BaseItemPtrs temp;
         bool bInSubmenu = !last.empty() && (groupNames.size() > 1);

         AddEffectMenuItemGroup(temp,
            groupNames,
            groupPlugs, groupFlags, useSubgroups);

         table.push_back( MenuOrItems( wxEmptyString,
            ( bInSubmenu ? last : TranslatableString{} ), std::move( temp )
         ) );

         groupNames.clear();
         groupPlugs.clear();
         groupFlags.clear();
         last = current;
      }

      groupNames.push_back( name );
      groupPlugs.push_back(plug->GetID());
      groupFlags.push_back(FixBatchFlags( batchflags, plug ) );
   }

   if (groupNames.size() > 0)
   {
      using namespace MenuTable;
      BaseItemPtrs temp;
      bool bInSubmenu = groupNames.size() > 1;

      AddEffectMenuItemGroup(temp,
         groupNames, groupPlugs, groupFlags, useSubgroups);

      table.push_back( MenuOrItems( wxEmptyString,
         ( bInSubmenu ? current : TranslatableString{} ), std::move( temp )
      ) );
   }
}

void AddSortedEffectMenuItems(
   MenuTable::BaseItemPtrs &table,
   std::vector<const PluginDescriptor*> & plugs,
   CommandFlag batchflags,
   CommandFlag realflags,
   SortBy sortBy,
   bool useSubgroups)
{
   size_t pluginCnt = plugs.size();

   TranslatableStrings groupNames;
   PluginIDs groupPlugs;
   std::vector<CommandFlag> groupFlags;

   for (size_t i = 0; i < pluginCnt; i++)
   {
      const PluginDescriptor *plug = plugs[i];

      auto name = plug->GetSymbol().Msgid();

      if (plug->IsEffectInteractive())
         name += XO("...");

      TranslatableString group;
      if (sortBy == SortBy::PublisherName/* wxT("sortby:publisher:name")*/)
      {
         group = EffectManager::Get().GetVendorName(plug->GetID());
      }
      else if (sortBy == SortBy::TypeName /*wxT("sortby:type:name")*/)
      {
         group = EffectManager::Get().GetEffectFamilyName(plug->GetID());
      }

      if (plug->IsEffectDefault())
      {
         group = {};
      }

      groupNames.push_back(
         group.empty()
            ? name
            : XO("%s: %s").Format( group, name )
      );

      groupPlugs.push_back(plug->GetID());
      groupFlags.push_back(FixBatchFlags( batchflags, plug ) );
   }

   if (groupNames.size() > 0)
   {
      AddEffectMenuItemGroup(
         table, groupNames, groupPlugs, groupFlags, useSubgroups);
   }
}

auto MakeAddGroupItems(const EffectsMenuGroups& list, CommandFlag batchflags, CommandFlag realflags) -> auto
{
   return [=](MenuTable::BaseItemPtrs& items, std::vector<const PluginDescriptor*>& plugs)
   {
      for(auto& p : list)
      {
         TranslatableStrings groupNames;
         PluginIDs groupPlugs;
         std::vector<CommandFlag> groupFlags;
         
         auto srcNames = p.second;
         std::sort(srcNames.begin(), srcNames.end(), TranslationLess);

         for(auto& name : srcNames)
         {
            auto it = std::find_if(plugs.begin(), plugs.end(), [&name](const PluginDescriptor* other)
            {
               return name == other->GetSymbol().Msgid();
            });
            if(it == plugs.end())
               continue;

            auto plug = *it;
            if(plug->IsEffectInteractive())
               groupNames.push_back(name + XO("..."));
            else
               groupNames.push_back( name );

            groupPlugs.push_back(plug->GetID());
            groupFlags.push_back(FixBatchFlags( batchflags, plug ) );
         }

         if (!groupNames.empty())
         {
            using namespace MenuTable;
            BaseItemPtrs temp;

            AddEffectMenuItemGroup(temp,
               groupNames, groupPlugs, groupFlags, false);

            items.push_back( MenuOrItems( wxEmptyString,
               p.first, std::move( temp )
            ) );
         }
      }
   };
}

struct MenuSectionBuilder
{
   std::vector<const PluginDescriptor*> plugins;

   std::function<bool(const PluginDescriptor*)> filter;
   std::function<bool(const PluginDescriptor*, const PluginDescriptor*)> compare;
   std::function<void(MenuTable::BaseItemPtrs&, std::vector<const PluginDescriptor*>&)> add;
};

bool ShowManager(
   PluginManager &pm, wxWindow *parent)
{
   PluginRegistrationDialog dlg(parent);
   return dlg.ShowModal() == wxID_OK;
}

void DoManagePluginsMenu(AudacityProject &project)
{
   auto &window = GetProjectFrame( project );
   auto &pm = PluginManager::Get();
   if (ShowManager(pm, &window))
      MenuCreator::RebuildAllMenuBars();
}

void DoManageRealtimeEffectsSidePanel(AudacityProject &project)
{
   auto &trackFocus = TrackFocus::Get(project);
   auto &panel = RealtimeEffectPanel::Get(project);
   if (panel.IsShown())
      panel.HidePanel();
   else
      panel.ShowPanel(trackFocus.Get(), true);
}

bool CompareEffectsByName(const PluginDescriptor *a, const PluginDescriptor *b)
{
   return
      std::make_pair( a->GetSymbol().Translation(), a->GetPath() ) <
      std::make_pair( b->GetSymbol().Translation(), b->GetPath() );
}

bool CompareEffectsByPublisher(
   const PluginDescriptor *a, const PluginDescriptor *b)
{
   auto &em = EffectManager::Get();
   
   auto akey = em.GetVendorName(a->GetID());
   auto bkey = em.GetVendorName(b->GetID());

   if (akey.empty())
      akey = XO("Uncategorized");
   if (bkey.empty())
      bkey = XO("Uncategorized");

   return
      std::make_tuple(
         akey.Translation(), a->GetSymbol().Translation(), a->GetPath() ) <
      std::make_tuple(
         bkey.Translation(), b->GetSymbol().Translation(), b->GetPath() );
}

bool CompareEffectsByPublisherAndName(
   const PluginDescriptor *a, const PluginDescriptor *b)
{
   auto &em = EffectManager::Get();
   auto akey = em.GetVendorName(a->GetID());
   auto bkey = em.GetVendorName(b->GetID());

   if (a->IsEffectDefault())
      akey = {};
   if (b->IsEffectDefault())
      bkey = {};

   return
      std::make_tuple(
         akey.Translation(), a->GetSymbol().Translation(), a->GetPath() ) <
      std::make_tuple(
         bkey.Translation(), b->GetSymbol().Translation(), b->GetPath() );
}

bool CompareEffectsByTypeAndName(
   const PluginDescriptor *a, const PluginDescriptor *b)
{
   auto &em = EffectManager::Get();
   auto akey = em.GetEffectFamilyName(a->GetID());
   auto bkey = em.GetEffectFamilyName(b->GetID());

   if (akey.empty())
      akey = XO("Uncategorized");
   if (bkey.empty())
      bkey = XO("Uncategorized");

   if (a->IsEffectDefault())
      akey = {};
   if (b->IsEffectDefault())
      bkey = {};

   return
      std::make_tuple(
         akey.Translation(), a->GetSymbol().Translation(), a->GetPath() ) <
      std::make_tuple(
         bkey.Translation(), b->GetSymbol().Translation(), b->GetPath() );
}

bool CompareEffectsByType(const PluginDescriptor *a, const PluginDescriptor *b)
{
   auto &em = EffectManager::Get();
   auto akey = em.GetEffectFamilyName(a->GetID());
   auto bkey = em.GetEffectFamilyName(b->GetID());

   if (akey.empty())
      akey = XO("Uncategorized");
   if (bkey.empty())
      bkey = XO("Uncategorized");

   return
      std::make_tuple(
         akey.Translation(), a->GetSymbol().Translation(), a->GetPath() ) <
      std::make_tuple(
         bkey.Translation(), b->GetSymbol().Translation(), b->GetPath() );
}

/// The effects come from a plug in list
/// This code iterates through the list, adding effects into
/// the menu.
MenuTable::BaseItemPtrs PopulateEffectsMenu(
   EffectType type,
   CommandFlag batchflags,
   CommandFlag realflags)
{
   MenuTable::BaseItemPtrs result;
   PluginManager & pm = PluginManager::Get();
   
   const auto groupby = EffectsGroupBy.Read();

   std::vector<MenuSectionBuilder> sections;
   
   auto MakeAddSortedItems = [=](SortBy sortby, bool useSubgroups)
   {
      return [=](MenuTable::BaseItemPtrs& items, std::vector<const PluginDescriptor*>& plugins)
      {
         return AddSortedEffectMenuItems(items, plugins, batchflags, realflags, sortby, useSubgroups);
      };
   };

   auto MakeAddGroupedItems = [=](GroupBy groupBy, bool useSubgroups)
   {
      return [=](MenuTable::BaseItemPtrs& items, std::vector<const PluginDescriptor*>& plugins)
      {
         return AddGroupedEffectMenuItems(items, plugins, batchflags, realflags, groupBy, useSubgroups);
      };
   };

   auto DefaultFilter = [](auto plug) { return IsEnabledPlugin(plug) && IsDefaultPlugin(plug); };
   if(groupby == "default")
   {
      if(type == EffectTypeProcess)
      {
         static auto effectMenuDefaults = [] {
            wxFileName path = wxFileName(FileNames::ResourcesDir(), wxT("EffectsMenuDefaults.xml"));
            return LoadEffectsMenuGroups(path.GetFullPath());
         }();
         static auto groupsFilter = MakeGroupsFilter(effectMenuDefaults);

         sections.emplace_back(
            MenuSectionBuilder {
               {},
               [=](auto plug) { return IsEnabledPlugin(plug) && groupsFilter(plug); },
               nullptr,
               MakeAddGroupItems(effectMenuDefaults, batchflags, realflags)
            });
         sections.emplace_back(
            MenuSectionBuilder {
               {},
               IsEnabledPlugin,
               CompareEffectsByPublisher,
               MakeAddGroupedItems(GroupBy::Publisher, false )
            });
      }
      else//Generators/Analyzers
      {
         sections.emplace_back(
            MenuSectionBuilder {
               {},
               [](auto plug){ return IsEnabledPlugin(plug) && IsBundledPlugin(plug); } ,
               CompareEffectsByName,
               MakeAddSortedItems(SortBy::Name, false )
            });
         sections.emplace_back(
            MenuSectionBuilder {
               {},
               IsEnabledPlugin,
               CompareEffectsByPublisher,
               MakeAddGroupedItems(GroupBy::Publisher, true )
            });
      }
   }
   else if(groupby == "sortby:publisher:name")
   {
      sections.emplace_back(
         MenuSectionBuilder {
            {},
            DefaultFilter,
            CompareEffectsByName,
            MakeAddSortedItems(SortBy::PublisherName, false)
         });
      sections.emplace_back(
         MenuSectionBuilder {
            {},
            IsEnabledPlugin,
            CompareEffectsByPublisherAndName,
            MakeAddSortedItems(SortBy::PublisherName, true )
         });
   }
   else if(groupby == "sortby:type:name")
   {
      sections.emplace_back(
         MenuSectionBuilder {
            {},
            DefaultFilter,
            CompareEffectsByName,
            MakeAddSortedItems(SortBy::TypeName, false)
         });
      sections.emplace_back(
         MenuSectionBuilder {
            {},
            IsEnabledPlugin,
            CompareEffectsByPublisherAndName,
            MakeAddSortedItems(SortBy::TypeName, true )
         });
   }
   else if(groupby == "groupby:publisher")
   {
      sections.emplace_back(
         MenuSectionBuilder {
            {},
            DefaultFilter,
            CompareEffectsByPublisher,
            MakeAddGroupedItems(GroupBy::Publisher, false)
         });
      sections.emplace_back(
         MenuSectionBuilder {
            {},
            IsEnabledPlugin,
            CompareEffectsByPublisher,
            MakeAddGroupedItems(GroupBy::Publisher, true )
         });
   }
   else if(groupby == "groupby:type")
   {
      sections.emplace_back(
         MenuSectionBuilder {
            {},
            DefaultFilter,
            CompareEffectsByType,
            MakeAddGroupedItems(GroupBy::Type, false)
         });
      sections.emplace_back(
         MenuSectionBuilder {
            {},
            IsEnabledPlugin,
            CompareEffectsByType,
            MakeAddGroupedItems(GroupBy::Type, true )
         });
   }
   else //if(groupby == "sortby:name")
   {
      sections.emplace_back(
         MenuSectionBuilder {
            {},
            DefaultFilter,
            CompareEffectsByName,
            MakeAddSortedItems(SortBy::Name, false)
         });
      sections.emplace_back(
         MenuSectionBuilder {
            {},
            IsEnabledPlugin,
            CompareEffectsByName,
            MakeAddSortedItems(SortBy::Name, true)
         });
   }
   for(auto& plugin : pm.EffectsOfType(type))
   {
      for(auto& section : sections)
      {
         if(section.filter(&plugin))
         {
            section.plugins.push_back(&plugin);
            break;
         }
      }
   }

   for(auto& section : sections)
   {
      if(section.compare != nullptr)
         std::sort(section.plugins.begin(), section.plugins.end(), section.compare);

      MenuTable::BaseItemPtrs items;
      section.add(items, section.plugins);

      if(items.empty())
         continue;

      if(result.empty())
         result.push_back(MenuTable::Items( "", std::move( items ) ));
      else
         result.push_back(MenuTable::Section( "", std::move( items ) ));
   }
   
   return result;
}

}

namespace {

// Menu handler functions

void OnResetConfig(const CommandContext &context)
{
   auto &project = context.project;
   auto &menuManager = MenuManager::Get(project);
   menuManager.mLastAnalyzerRegistration = MenuCreator::repeattypenone;
   menuManager.mLastToolRegistration = MenuCreator::repeattypenone;
   menuManager.mLastGenerator = "";
   menuManager.mLastEffect = "";
   menuManager.mLastAnalyzer = "";
   menuManager.mLastTool = "";

   ResetPreferences();

   // Directory will be reset on next restart.
   FileNames::UpdateDefaultPath(FileNames::Operation::Temp, TempDirectory::DefaultTempDir());

   // There are many more things we could reset here.
   // Beeds discussion as to which make sense to.
   // Maybe in future versions?
   // - Reset Effects
   // - Reset Recording and Playback volumes
   // - Reset Selection formats (and for spectral too)
   // - Reset Play-at-speed speed to x1
   // - Stop playback/recording and unapply pause.
   // - Set Zoom sensibly.
   gPrefs->Write("/GUI/SyncLockTracks", 0);
   gPrefs->Write("/AudioIO/SoundActivatedRecord", 0);
   gPrefs->Write("/SelectionToolbarMode", 0);
   gPrefs->Flush();
   DoReloadPreferences(project);

   ProjectWindow::OnResetWindow(context);
   ToolManager::OnResetToolBars(context);

   // These are necessary to preserve the newly correctly laid out toolbars.
   // In particular the Device Toolbar ends up short on next restart, 
   // if they are left out.
   gPrefs->Write(wxT("/PrefsVersion"), wxString(wxT(AUDACITY_PREFS_VERSION_STRING)));

   // write out the version numbers to the prefs file for future checking
   gPrefs->Write(wxT("/Version/Major"), AUDACITY_VERSION);
   gPrefs->Write(wxT("/Version/Minor"), AUDACITY_RELEASE);
   gPrefs->Write(wxT("/Version/Micro"), AUDACITY_REVISION);

   gPrefs->Flush();

   ProjectSelectionManager::Get( project )
      .AS_SetSnapTo(gPrefs->ReadLong("/SnapTo", SNAP_OFF));
   ProjectSelectionManager::Get( project )
      .AS_SetRate(gPrefs->ReadDouble("/DefaultProjectSampleRate", 44100.0));
}

void OnManageGenerators(const CommandContext &context)
{
   auto &project = context.project;
   DoManagePluginsMenu(project);
}

void OnEffect(const CommandContext &context)
{
   // using GET to interpret parameter as a PluginID
   // EffectContext construction
   auto pContext = std::make_shared<EffectContext>();
   EffectUI::DoEffect(context.project, pContext, context.parameter.GET());
}

void OnManageEffects(const CommandContext &context)
{
   auto &project = context.project;
   DoManagePluginsMenu(project);
}

void OnAddRealtimeEffects(const CommandContext& context)
{
   auto& project = context.project;
   DoManageRealtimeEffectsSidePanel(project);
}

void OnAnalyzer2(wxCommandEvent& evt) { return; }

void OnRepeatLastGenerator(const CommandContext &context)
{
   auto& menuManager = MenuManager::Get(context.project);
   auto lastEffect = menuManager.mLastGenerator;
   if (!lastEffect.empty()) {
      // EffectContext construction
      auto pContext = std::make_shared<EffectContext>(
         menuManager.mRepeatGeneratorFlags | EffectManager::kRepeatGen);
      EffectUI::DoEffect(context.project, pContext, lastEffect);
   }
}

void OnRepeatLastEffect(const CommandContext &context)
{
   auto& menuManager = MenuManager::Get(context.project);
   auto lastEffect = menuManager.mLastEffect;
   if (!lastEffect.empty()) {
      // EffectContext construction
      auto pContext =
         std::make_shared<EffectContext>(menuManager.mRepeatEffectFlags);
      EffectUI::DoEffect(context.project, pContext, lastEffect);
   }
}

void OnRepeatLastAnalyzer(const CommandContext& context)
{
   auto& menuManager = MenuManager::Get(context.project);
   switch (menuManager.mLastAnalyzerRegistration) {
   case MenuCreator::repeattypeplugin: {
      auto lastEffect = menuManager.mLastAnalyzer;
      if (!lastEffect.empty()) {
         // EffectContext construction
         auto pContext =
            std::make_shared<EffectContext>(menuManager.mRepeatAnalyzerFlags);
         EffectUI::DoEffect(context.project, pContext, lastEffect);
      }
   }
   break;
   case MenuCreator::repeattypeunique:
      CommandManager::Get(context.project).DoRepeatProcess(context,
         menuManager.mLastAnalyzerRegisteredId);
      break;
   }
}

void OnManageAnalyzers(const CommandContext &context)
{
   auto &project = context.project;
   DoManagePluginsMenu(project);
}

void OnManageTools(const CommandContext &context )
{
   auto &project = context.project;
   DoManagePluginsMenu(project);
}

void OnBenchmark(const CommandContext &context)
{
   auto &project = context.project;
   CommandManager::Get(project).RegisterLastTool(context);  //Register Run Benchmark as Last Tool
   auto &window = GetProjectFrame( project );
   ::RunBenchmark( &window, project);
}

void OnSimulateRecordingErrors(const CommandContext &context)
{
   auto &project = context.project;
   auto &commandManager = CommandManager::Get( project );

   auto gAudioIO = AudioIO::Get();
   bool &setting = gAudioIO->mSimulateRecordingErrors;
   commandManager.Check(wxT("SimulateRecordingErrors"), !setting);
   setting = !setting;
}

void OnDetectUpstreamDropouts(const CommandContext &context)
{
   auto &project = context.project;
   auto &commandManager = CommandManager::Get( project );

   auto gAudioIO = AudioIO::Get();
   auto &setting = gAudioIO->mDetectUpstreamDropouts;
   auto oldValue = setting.load(std::memory_order_relaxed);
   commandManager.Check(wxT("DetectUpstreamDropouts"), !oldValue);
   setting.store(!oldValue, std::memory_order_relaxed);
}

void OnWriteJournal(const CommandContext &)
{
   auto OnMessage =
      /* i18n-hint a "journal" is a text file that records
       the user's interactions with the application */
      XO("A journal will be recorded after Audacity restarts.");
   auto OffMessage =
      /* i18n-hint a "journal" is a text file that records
       the user's interactions with the application */
      XO("No journal will be recorded after Audacity restarts.");

   using namespace Journal;
   bool enabled = RecordEnabled();
   if ( SetRecordEnabled(!enabled) )
      enabled = !enabled;
   if ( enabled )
      AudacityMessageBox( OnMessage );
   else
      AudacityMessageBox( OffMessage );
}

// Menu definitions? ...

// ... buf first some more helper definitions

void AddEffectMenuItemGroup(
   MenuTable::BaseItemPtrs &table,
   const TranslatableStrings & names,
   const PluginIDs & plugs,
   const std::vector<CommandFlag> & flags,
   bool useSubgroups)
{
   const int namesCnt = (int) names.size();
   int perGroup;

#if defined(__WXGTK__)
   gPrefs->Read(wxT("/Effects/MaxPerGroup"), &perGroup, 15);
#else
   gPrefs->Read(wxT("/Effects/MaxPerGroup"), &perGroup, 0);
#endif

   int groupCnt = namesCnt;
   for (int i = 0; i < namesCnt; i++)
   {
      // compare full translations not msgids!
      while (i + 1 < namesCnt && names[i].Translation() == names[i + 1].Translation())
      {
         i++;
         groupCnt--;
      }
   }
   
   if (namesCnt > 0 && !useSubgroups)
   {
      perGroup = 0;
   }

   int max = perGroup;
   int items = perGroup;

   if (max > groupCnt)
   {
      max = 0;
   }

   using namespace MenuTable;
   // This finder scope may be redundant, but harmless
   auto pTable = &table;
   BaseItemPtrs temp1;

   int groupNdx = 0;
   for (int i = 0; i < namesCnt; i++)
   {
      if (max > 0 && items == max)
      {
         // start collecting items for the next submenu
         pTable = &temp1;
      }

      // compare full translations not msgids!
      if (i + 1 < namesCnt && names[i].Translation() == names[i + 1].Translation())
      {
         // collect a sub-menu for like-named items
         const auto name = names[i];
         const auto translation = name.Translation();
         BaseItemPtrs temp2;
         // compare full translations not msgids!
         while (i < namesCnt && names[i].Translation() == translation)
         {
            const PluginDescriptor *plug =
               PluginManager::Get().GetPlugin(plugs[i]);
            if( plug->GetPluginType() == PluginTypeEffect )
               temp2.push_back( Command( plug->GetID(),
                  Verbatim( plug->GetPath() ),
                  OnEffect,
                  flags[i],
                  CommandManager::Options{}
                     .IsEffect()
                     .AllowInMacros()
                     .Parameter( plugs[i] ) ) );

            i++;
         }
         pTable->push_back( Menu( wxEmptyString, name, std::move( temp2 ) ) );
         i--;
      }
      else
      {
         // collect one item
         const PluginDescriptor *plug =
            PluginManager::Get().GetPlugin(plugs[i]);
         if( plug->GetPluginType() == PluginTypeEffect )
            pTable->push_back( Command(
               plug->GetID(),
               names[i],
               OnEffect,
               flags[i],
               CommandManager::Options{}
                  .IsEffect()
                  .AllowInMacros()
                  .Parameter( plugs[i] ) ) );
      }

      if (max > 0)
      {
         items--;
         if (items == 0 || i + 1 == namesCnt)
         {
            int end = groupNdx + max;
            if (end + 1 > groupCnt)
            {
               end = groupCnt;
            }
            // Done collecting
            table.push_back( Menu( wxEmptyString,
               XXO("Plugin %d to %d").Format( groupNdx + 1, end ),
               std::move( temp1 )
            ) );
            items = max;
            pTable = &table;
            groupNdx += max;
         }
      }
   }
}

}

// Menu definitions

// Under /MenuBar
using namespace MenuTable;

namespace {
const ReservedCommandFlag&
   HasLastGeneratorFlag() { static ReservedCommandFlag flag{
      [](const AudacityProject &project){
         return !MenuManager::Get( project ).mLastGenerator.empty();
      }
   }; return flag; }

BaseItemSharedPtr GenerateMenu()
{
   // All of this is a bit hacky until we can get more things connected into
   // the plugin manager...sorry! :-(

   using Options = CommandManager::Options;

   static BaseItemSharedPtr menu{
   Menu( wxT("Generate"), XXO("&Generate"),
      Section( "Manage",
         Command( wxT("ManageGenerators"), XXO("Plugin Manager"),
            OnManageGenerators, AudioIONotBusyFlag() )
      ),

      Section("RepeatLast",
         // Delayed evaluation:
         [](AudacityProject &project)
         {
            const auto &lastGenerator = MenuManager::Get(project).mLastGenerator;
            TranslatableString buildMenuLabel;
            if (!lastGenerator.empty())
               buildMenuLabel = XO("Repeat %s")
                  .Format(EffectManager::Get().GetCommandName(lastGenerator));
            else
               buildMenuLabel = XO("Repeat Last Generator");

            return Command(wxT("RepeatLastGenerator"), buildMenuLabel,
               OnRepeatLastGenerator,
               AudioIONotBusyFlag() |
                   HasLastGeneratorFlag(),
               Options{}.IsGlobal());
         }
      ),

      Section( "Generators",
         // Delayed evaluation:
         [](AudacityProject &)
         { return Items( wxEmptyString, PopulateEffectsMenu(
            EffectTypeGenerate,
            AudioIONotBusyFlag(),
            AudioIONotBusyFlag())
         ); }
      )
   ) };
   return menu;
}

static const ReservedCommandFlag
&IsRealtimeNotActiveFlag() { static ReservedCommandFlag flag{
   [](const AudacityProject &project){
      return !RealtimeEffectManager::Get(project).IsActive();
   }
}; return flag; }  //lll

AttachedItem sAttachment1{
   wxT(""),
   Shared( GenerateMenu() )
};

const ReservedCommandFlag&
   HasLastEffectFlag() { static ReservedCommandFlag flag{
      [](const AudacityProject &project) {
         return !MenuManager::Get(project).mLastEffect.empty();
      }
   }; return flag;
}

static const ReservedCommandFlag&
   HasTrackFocusFlag() { static ReservedCommandFlag flag{
      [](const AudacityProject &project) {
         auto& trackFocus = TrackFocus::Get(const_cast<AudacityProject&>(project));
         return (trackFocus.Get() != nullptr);
      }
   };
   return flag;
}

BaseItemSharedPtr EffectMenu()
{
   // All of this is a bit hacky until we can get more things connected into
   // the plugin manager...sorry! :-(

   static BaseItemSharedPtr menu{
   Menu( wxT("Effect"), XXO("Effe&ct"),
      Section( "Manage",
         Command( wxT("ManageEffects"), XXO("Plugin Manager"),
            OnManageEffects, AudioIONotBusyFlag() )
      ),

      Section( "RealtimeEffects",
         Command ( wxT("AddRealtimeEffects"), XXO("Add Realtime Effects"),
            OnAddRealtimeEffects,  HasTrackFocusFlag(), wxT("E") )
      ),

      Section( "RepeatLast",
         // Delayed evaluation:
         [](AudacityProject &project)
         {
            const auto &lastEffect = MenuManager::Get(project).mLastEffect;
            TranslatableString buildMenuLabel;
            if (!lastEffect.empty())
               buildMenuLabel = XO("Repeat %s")
                  .Format( EffectManager::Get().GetCommandName(lastEffect) );
            else
               buildMenuLabel = XO("Repeat Last Effect");

            return Command( wxT("RepeatLastEffect"), buildMenuLabel,
               OnRepeatLastEffect,
               AudioIONotBusyFlag() | TimeSelectedFlag() |
                  WaveTracksSelectedFlag() | HasLastEffectFlag(),
               wxT("Ctrl+R") );
         }
      ),

      Section( "Effects",
         // Delayed evaluation:
         [](AudacityProject &)
         { return Items( wxEmptyString, PopulateEffectsMenu(
            EffectTypeProcess,
            AudioIONotBusyFlag() | TimeSelectedFlag() | WaveTracksSelectedFlag(),
            IsRealtimeNotActiveFlag() )
         ); }
      )
   ) };
   return menu;
}

AttachedItem sAttachment2{
   wxT(""),
   Shared( EffectMenu() )
};

const ReservedCommandFlag&
   HasLastAnalyzerFlag() { static ReservedCommandFlag flag{
      [](const AudacityProject &project) {
         if (MenuManager::Get(project).mLastAnalyzerRegistration == MenuCreator::repeattypeunique) return true;
         return !MenuManager::Get(project).mLastAnalyzer.empty();
      }
   }; return flag;
}

BaseItemSharedPtr AnalyzeMenu()
{
   // All of this is a bit hacky until we can get more things connected into
   // the plugin manager...sorry! :-(

   using Options = CommandManager::Options;

   static BaseItemSharedPtr menu{
   Menu( wxT("Analyze"), XXO("&Analyze"),
      Section( "Manage",
         Command( wxT("ManageAnalyzers"), XXO("Plugin Manager"),
            OnManageAnalyzers, AudioIONotBusyFlag() )
      ),

      Section("RepeatLast",
         // Delayed evaluation:
         [](AudacityProject &project)
         {
            const auto &lastAnalyzer = MenuManager::Get(project).mLastAnalyzer;
            TranslatableString buildMenuLabel;
            if (!lastAnalyzer.empty())
               buildMenuLabel = XO("Repeat %s")
                  .Format(EffectManager::Get().GetCommandName(lastAnalyzer));
            else
               buildMenuLabel = XO("Repeat Last Analyzer");

            return Command(wxT("RepeatLastAnalyzer"), buildMenuLabel,
               OnRepeatLastAnalyzer,
               AudioIONotBusyFlag() | TimeSelectedFlag() |
                  WaveTracksSelectedFlag() | HasLastAnalyzerFlag(),
               Options{}.IsGlobal());
         }
      ),

      Section( "Analyzers",
         Items( "Windows" ),

         // Delayed evaluation:
         [](AudacityProject&)
         { return Items( wxEmptyString, PopulateEffectsMenu(
            EffectTypeAnalyze,
            AudioIONotBusyFlag() | TimeSelectedFlag() | WaveTracksSelectedFlag(),
            IsRealtimeNotActiveFlag() )
         ); }
      )
   ) };
   return menu;
}

AttachedItem sAttachment3{
   wxT(""),
   Shared( AnalyzeMenu() )
};

BaseItemSharedPtr ToolsMenu()
{
   using Options = CommandManager::Options;

   static BaseItemSharedPtr menu{
   Menu( wxT("Tools"), XXO("T&ools"),
      Section( "Manage",
         Command( wxT("ManageTools"), XXO("Plugin Manager"),
            OnManageTools, AudioIONotBusyFlag() )

         //Separator(),
      ),

      Section( "Other",
         Command( wxT("ConfigReset"), XXO("Reset &Configuration"),
            OnResetConfig,
            AudioIONotBusyFlag() ),

   // PRL: team consensus for 2.2.0 was, we let end users have this diagnostic,
   // as they used to in 1.3.x
   //#ifdef IS_ALPHA
         // TODO: What should we do here?  Make benchmark a plug-in?
         // Easy enough to do.  We'd call it mod-self-test.
         Command( wxT("Benchmark"), XXO("&Run Benchmark..."),
            OnBenchmark, AudioIONotBusyFlag() )
   //#endif
      ),

      Section( "Tools",
         // Delayed evaluation:
         [](AudacityProject&)
         { return Items( wxEmptyString, PopulateEffectsMenu(
            EffectTypeTool,
            AudioIONotBusyFlag(),
            AudioIONotBusyFlag() )
         ); }
      )

#ifdef IS_ALPHA
      ,
      Section( "",
         Command( wxT("SimulateRecordingErrors"),
            XXO("Simulate Recording Errors"),
            OnSimulateRecordingErrors,
            AudioIONotBusyFlag(),
            Options{}.CheckTest(
               [](AudacityProject&){
                  return AudioIO::Get()->mSimulateRecordingErrors; } ) ),
         Command( wxT("DetectUpstreamDropouts"),
            XXO("Detect Upstream Dropouts"),
            OnDetectUpstreamDropouts,
            AudioIONotBusyFlag(),
            Options{}.CheckTest(
               [](AudacityProject&){
                  return AudioIO::Get()->mDetectUpstreamDropouts
                     .load(std::memory_order_relaxed); } ) )
      )
#endif

#if defined(IS_ALPHA) || defined(END_USER_JOURNALLING)
      ,
      Section( "",
         Command( wxT("WriteJournal"),
            /* i18n-hint a "journal" is a text file that records
             the user's interactions with the application */
            XXO("Write Journal"),
            OnWriteJournal,
            AlwaysEnabledFlag,
            Options{}.CheckTest( [](AudacityProject&){
               return Journal::RecordEnabled(); } ) )
      )
#endif

   ) };
   return menu;
}

AttachedItem sAttachment4{
   wxT(""),
   Shared( ToolsMenu() )
};

}
