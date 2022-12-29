#include "MenuHelper.h"
#include "PluginManager.h"
#include "effects/EffectManager.h"
#include "CommonCommandFlags.h"
#include "prefs/EffectsPrefs.h"
#include "BatchCommands.h"
#include "BatchProcessDialog.h"

#include "XMLFileReader.h"

namespace {

using EffectsMenuGroups = std::vector<std::pair<TranslatableString, std::vector<TranslatableString>>>;

struct MenuSectionBuilder
{
   std::vector<const PluginDescriptor*> plugins;

   std::function<bool(const PluginDescriptor*)> filter;
   std::function<bool(const PluginDescriptor*, const PluginDescriptor*)> compare;
   std::function<void(MenuTable::BaseItemPtrs&, std::vector<const PluginDescriptor*>&)> add;
};

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

// Some weird special case stuff just for Noise Reduction so that there is
// more informative help
CommandFlag FixBatchFlags(CommandFlag batchflags, const PluginDescriptor* plug)
{
   if ( plug->GetSymbol().Msgid() == XO( "Noise Reduction" ) )
      return ( batchflags | NoiseReductionTimeSelectedFlag() ) & ~TimeSelectedFlag();
   return batchflags;
}


void AddEffectMenuItemGroup(
   MenuTable::BaseItemPtrs &table,
   const TranslatableStrings & names,
   const PluginIDs & plugs,
   const std::vector<CommandFlag> & flags,
   bool useSubgroups,
   void (*onMenuCommand)(const CommandContext&))
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
                  onMenuCommand,
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
               onMenuCommand,
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

void AddSortedEffectMenuItems(
   MenuTable::BaseItemPtrs &table,
   std::vector<const PluginDescriptor*> & plugs,
   CommandFlag batchflags,
   CommandFlag realflags,
   SortBy sortBy,
   bool useSubgroups,
   void (*onMenuCommand)(const CommandContext&))
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
         table, groupNames, groupPlugs, groupFlags, useSubgroups,
         onMenuCommand);
   }
}

auto MakeAddGroupItems(
   const EffectsMenuGroups& list,
   CommandFlag batchflags,
   CommandFlag realflags,
   void (*onMenuCommand)(const CommandContext&)) -> auto
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
               groupNames, groupPlugs, groupFlags, false,
               onMenuCommand);

            items.push_back( MenuOrItems( wxEmptyString,
               p.first, std::move( temp )
            ) );
         }
      }
   };
}

void AddGroupedEffectMenuItems(
   MenuTable::BaseItemPtrs &table,
   std::vector<const PluginDescriptor*> & plugs,
   CommandFlag batchflags,
   CommandFlag realflags,
   GroupBy groupBy,
   bool useSubgroups,
   void (*onMenuCommand)(const CommandContext&))
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
            groupPlugs, groupFlags, useSubgroups,
            onMenuCommand);

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
         groupNames, groupPlugs, groupFlags, useSubgroups,
         onMenuCommand);

      table.push_back( MenuOrItems( wxEmptyString,
         ( bInSubmenu ? current : TranslatableString{} ), std::move( temp )
      ) );
   }
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

}

MenuTable::BaseItemPtrs MenuHelper::PopulateEffectsMenu(
   EffectType type,
   CommandFlag batchflags,
   CommandFlag realflags,
   void (*onMenuCommand)(const CommandContext&))
{
   MenuTable::BaseItemPtrs result;
   PluginManager & pm = PluginManager::Get();
   
   const auto groupby = EffectsGroupBy.Read();

   std::vector<MenuSectionBuilder> sections;
   
   auto MakeAddSortedItems = [=](SortBy sortby, bool useSubgroups)
   {
      return [=](MenuTable::BaseItemPtrs& items, std::vector<const PluginDescriptor*>& plugins)
      {
         return AddSortedEffectMenuItems(items, plugins, batchflags, realflags, sortby, useSubgroups, onMenuCommand);
      };
   };

   auto MakeAddGroupedItems = [=](GroupBy groupBy, bool useSubgroups)
   {
      return [=](MenuTable::BaseItemPtrs& items, std::vector<const PluginDescriptor*>& plugins)
      {
         return AddGroupedEffectMenuItems(items, plugins, batchflags, realflags, groupBy, useSubgroups, onMenuCommand);
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
               MakeAddGroupItems(effectMenuDefaults, batchflags, realflags, onMenuCommand)
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
