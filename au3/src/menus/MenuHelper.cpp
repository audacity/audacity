#include "MenuHelper.h"
#include "PluginManager.h"
#include "EffectManager.h"
#include "CommonCommandFlags.h"
#include "BatchCommands.h"
#include "BatchProcessDialog.h"

#include "XMLFileReader.h"

namespace {
using EffectsMenuGroups = std::vector<std::pair<TranslatableString, std::vector<TranslatableString> > >;

struct MenuSectionBuilder
{
    std::vector<const PluginDescriptor*> plugins;

    std::function<bool(const PluginDescriptor*)> filter;
    std::function<bool(const PluginDescriptor*, const PluginDescriptor*)> compare;
    std::function<void(MenuHelper::Group&, std::vector<const PluginDescriptor*>&)> add;
};

enum class GroupBy
{
    Publisher,
    Type,
    TypePublisher
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

        EffectsHandler(std::vector<TranslatableString>& effects)
            : effects(effects) { }

        bool HandleXMLTag(const std::string_view& tag, const AttributesList& attrs) override { return true; }
        void HandleXMLContent(const std::string_view& text) override { textContent = text; }
        void HandleXMLEndTag(const std::string_view& tag) override
        {
            if (textContent.has_value() && tag == "Effect") {
                effects.emplace_back(TranslatableString { *textContent, {} });
            }
            textContent.reset();
        }

        XMLTagHandler* HandleXMLChild(const std::string_view& tag) override
        {
            if (tag == "Effect") {
                return this;
            }
            return nullptr;
        }
    };

    struct GroupHandler : XMLTagHandler
    {
        std::optional<std::string> textContent;
        std::unique_ptr<EffectsHandler> effectsHandler;
        std::pair<TranslatableString, std::vector<TranslatableString> >& group;

        GroupHandler(std::pair<TranslatableString, std::vector<TranslatableString> >& group)
            : group(group) { }

        bool HandleXMLTag(const std::string_view& tag, const AttributesList& attrs) override { return true; }
        void HandleXMLContent(const std::string_view& text) override { textContent = text; }
        void HandleXMLEndTag(const std::string_view& tag) override
        {
            if (textContent.has_value() && tag == "Name") {
                group.first = TranslatableString { *textContent, { } }
            }
            textContent.reset();
        }

        XMLTagHandler* HandleXMLChild(const std::string_view& tag) override
        {
            if (tag == "Effects") {
                effectsHandler = std::make_unique<EffectsHandler>(group.second);
                return &*effectsHandler;
            }
            if (tag == "Name") {
                return this;
            }

            return nullptr;
        }
    };

    EffectsMenuGroups& groups;
    std::unique_ptr<GroupHandler> groupHandler;

    EffectsMenuGroupsHandler(EffectsMenuGroups& groups)
        : groups(groups) { }

    bool HandleXMLTag(const std::string_view& tag, const AttributesList& attrs) override { return true; }

    XMLTagHandler* HandleXMLChild(const std::string_view& tag) override
    {
        if (tag == "Group") {
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
    if (plug->GetSymbol().Msgid() == XO("Noise Reduction")) {
        return (batchflags | NoiseReductionTimeSelectedFlag()) & ~TimeSelectedFlag();
    }
    return batchflags;
}

void AddEffectMenuItemGroup(
    MenuHelper::Group& table,
    const TranslatableStrings& names,
    const PluginIDs& plugs,
    const std::vector<CommandFlag>& flags,
    void (*onMenuCommand)(const CommandContext&))
{
    const int namesCnt = (int)names.size();

    int groupCnt = namesCnt;
    for (int i = 0; i < namesCnt; i++) {
        // compare full translations not msgids!
        while (i + 1 < namesCnt && names[i].Translation() == names[i + 1].Translation())
        {
            i++;
            groupCnt--;
        }
    }

    using namespace MenuRegistry;

    for (int i = 0; i < namesCnt; i++) {
        // compare full translations not msgids!
        if (i + 1 < namesCnt && names[i].Translation() == names[i + 1].Translation()) {
            // collect a sub-menu for like-named items
            const auto name = names[i];
            const auto translation = name.Translation();
            auto subMenu = Menu("", name);
            // compare full translations not msgids!
            while (i < namesCnt && names[i].Translation() == translation)
            {
                const PluginDescriptor* plug
                    =PluginManager::Get().GetPlugin(plugs[i]);
                if (plug->GetPluginType() == PluginTypeEffect) {
                    subMenu->push_back(Command(plug->GetID(),
                                               Verbatim(plug->GetPath()),
                                               onMenuCommand,
                                               flags[i],
                                               Options {}
                                               .IsEffect()
                                               .AllowInMacros()
                                               .Parameter(plugs[i])));
                }

                i++;
            }
            table.push_back(move(subMenu));
            i--;
        } else {
            // collect one item
            const PluginDescriptor* plug
                =PluginManager::Get().GetPlugin(plugs[i]);
            if (plug->GetPluginType() == PluginTypeEffect) {
                table.push_back(Command(
                                    plug->GetID(),
                                    names[i],
                                    onMenuCommand,
                                    flags[i],
                                    Options {}
                                    .IsEffect()
                                    .AllowInMacros()
                                    .Parameter(plugs[i])));
            }
        }
    }
}

void AddSortedEffectMenuItems(
    MenuHelper::Group& table,
    std::vector<const PluginDescriptor*>& plugs,
    CommandFlag batchflags,
    SortBy sortBy,
    void (*onMenuCommand)(const CommandContext&))
{
    size_t pluginCnt = plugs.size();

    TranslatableStrings groupNames;
    PluginIDs groupPlugs;
    std::vector<CommandFlag> groupFlags;

    for (size_t i = 0; i < pluginCnt; i++) {
        const PluginDescriptor* plug = plugs[i];

        auto name = plug->GetSymbol().Msgid();

        if (plug->IsEffectInteractive()) {
            name += XO("...");
        }

        TranslatableString group;
        if (sortBy == SortBy::PublisherName /* wxT("sortby:publisher:name")*/) {
            group = EffectManager::Get().GetVendorName(plug->GetID());
        } else if (sortBy == SortBy::TypeName /*wxT("sortby:type:name")*/) {
            group = EffectManager::Get().GetEffectFamilyName(plug->GetID());
        }

        if (plug->IsEffectDefault()) {
            group = {};
        }

        groupNames.push_back(
            group.empty()
            ? name
            : XO("%s: %s").Format(group, name)
            );

        groupPlugs.push_back(plug->GetID());
        groupFlags.push_back(FixBatchFlags(batchflags, plug));
    }

    if (groupNames.size() > 0) {
        AddEffectMenuItemGroup(
            table, groupNames, groupPlugs, groupFlags,
            onMenuCommand);
    }
}

auto MakeAddGroupItems(
    const EffectsMenuGroups& list,
    CommandFlag batchflags,
    void (*onMenuCommand)(const CommandContext&)) -> auto
{
    return [=](MenuHelper::Group& items, std::vector<const PluginDescriptor*>& plugs)
    {
        for (auto& p : list) {
            TranslatableStrings groupNames;
            PluginIDs groupPlugs;
            std::vector<CommandFlag> groupFlags;

            auto srcNames = p.second;
            std::sort(srcNames.begin(), srcNames.end(), TranslationLess);

            for (auto& name : srcNames) {
                auto it = std::find_if(plugs.begin(), plugs.end(), [&name](const PluginDescriptor* other)
                {
                    return name == other->GetSymbol().Msgid();
                });
                if (it == plugs.end()) {
                    continue;
                }

                auto plug = *it;
                if (plug->IsEffectInteractive()) {
                    groupNames.push_back(name + XO("..."));
                } else {
                    groupNames.push_back(name);
                }

                groupPlugs.push_back(plug->GetID());
                groupFlags.push_back(FixBatchFlags(batchflags, plug));
            }

            if (!groupNames.empty()) {
                using namespace MenuRegistry;
                if (p.first.empty()) {
                    auto temp = Items("");
                    AddEffectMenuItemGroup(*temp,
                                           groupNames, groupPlugs, groupFlags,
                                           onMenuCommand);
                    items.push_back(move(temp));
                } else {
                    auto temp = Menu("", p.first);
                    AddEffectMenuItemGroup(*temp,
                                           groupNames, groupPlugs, groupFlags,
                                           onMenuCommand);
                    items.push_back(move(temp));
                }
            }
        }
    };
}

void AddGroupedEffectMenuItems(
    MenuHelper::Group& table,
    std::vector<const PluginDescriptor*>& plugs,
    CommandFlag batchflags,
    GroupBy groupBy,
    void (*onMenuCommand)(const CommandContext&))
{
    using namespace MenuRegistry;

    const auto UnknownGroupName = XO("Unknown");
    auto& effectManager = EffectManager::Get();

    std::vector<TranslatableString> path;

    auto* parentTable = &table;
    std::vector<TranslatableString> names;
    PluginIDs group;
    std::vector<CommandFlag> flags;

    auto doAddGroup = [&]
    {
        using namespace MenuRegistry;
        if (names.empty()) {
            return;
        }

        const auto inSubmenu = !path.empty() && (names.size() > 1);
        const auto label = inSubmenu ? path.back() : TranslatableString{};
        if (label.empty()) {
            auto items = Items("");
            AddEffectMenuItemGroup(*items, names, group, flags, onMenuCommand);
            parentTable->push_back(move(items));
        } else {
            auto items = Menu("", label);
            AddEffectMenuItemGroup(*items, names, group, flags, onMenuCommand);
            parentTable->push_back(move(items));
        }

        names.clear();
        group.clear();
        flags.clear();
    };

    for (auto plug : plugs) {
        if (groupBy == GroupBy::Publisher) {
            const auto vendorName = effectManager.GetVendorName(plug->GetID());
            if (path.empty() || path[0] != vendorName) {
                doAddGroup();
                path = { vendorName };
            }
        } else if (groupBy == GroupBy::Type) {
            const auto effectFamilyName = effectManager.GetEffectFamilyName(plug->GetID());
            if (path.empty() || path[0] != effectFamilyName) {
                doAddGroup();
                path = { effectFamilyName };
            }
        } else if (groupBy == GroupBy::TypePublisher) {
            const auto effectFamilyName = effectManager.GetEffectFamilyName(plug->GetID());
            const auto vendorName = effectManager.GetVendorName(plug->GetID());
            if (path.empty() || path[0] != effectFamilyName) {
                doAddGroup();
                path = { effectFamilyName, vendorName };
                auto menu = Menu("", effectFamilyName);
                parentTable = menu.get();
                table.push_back(move(menu));
            } else if (path[1] != vendorName) {
                doAddGroup();
                path[1] = vendorName;
            }
        }

        group.push_back(plug->GetID());
        names.push_back(plug->GetSymbol().Msgid());
        flags.push_back(FixBatchFlags(batchflags, plug));
    }
    doAddGroup();
}

bool CompareEffectsByName(const PluginDescriptor* a, const PluginDescriptor* b)
{
    return
        std::make_pair(a->GetSymbol().Translation(), a->GetPath())
        < std::make_pair(b->GetSymbol().Translation(), b->GetPath());
}

bool CompareEffectsByPublisher(
    const PluginDescriptor* a, const PluginDescriptor* b)
{
    auto& em = EffectManager::Get();

    auto akey = em.GetVendorName(a->GetID());
    auto bkey = em.GetVendorName(b->GetID());

    if (akey.empty()) {
        akey = XO("Uncategorized");
    }
    if (bkey.empty()) {
        bkey = XO("Uncategorized");
    }

    return
        std::make_tuple(
        akey.Translation(), a->GetSymbol().Translation(), a->GetPath())
        < std::make_tuple(
        bkey.Translation(), b->GetSymbol().Translation(), b->GetPath());
}

bool CompareEffectsByPublisherAndName(
    const PluginDescriptor* a, const PluginDescriptor* b)
{
    auto& em = EffectManager::Get();
    auto akey = em.GetVendorName(a->GetID());
    auto bkey = em.GetVendorName(b->GetID());

    if (a->IsEffectDefault()) {
        akey = {}
    }
    if (b->IsEffectDefault()) {
        bkey = {}
    }

    return
        std::make_tuple(
        akey.Translation(), a->GetSymbol().Translation(), a->GetPath())
        < std::make_tuple(
        bkey.Translation(), b->GetSymbol().Translation(), b->GetPath());
}

bool CompareEffectsByTypeAndName(
    const PluginDescriptor* a, const PluginDescriptor* b)
{
    auto& em = EffectManager::Get();
    auto akey = em.GetEffectFamilyName(a->GetID());
    auto bkey = em.GetEffectFamilyName(b->GetID());

    if (akey.empty()) {
        akey = XO("Uncategorized");
    }
    if (bkey.empty()) {
        bkey = XO("Uncategorized");
    }

    if (a->IsEffectDefault()) {
        akey = {}
    }
    if (b->IsEffectDefault()) {
        bkey = {}
    }

    return
        std::make_tuple(
        akey.Translation(), a->GetSymbol().Translation(), a->GetPath())
        < std::make_tuple(
        bkey.Translation(), b->GetSymbol().Translation(), b->GetPath());
}

bool CompareEffectsByType(const PluginDescriptor* a, const PluginDescriptor* b)
{
    auto& em = EffectManager::Get();
    auto akey = em.GetEffectFamilyName(a->GetID());
    auto bkey = em.GetEffectFamilyName(b->GetID());

    if (akey.empty()) {
        akey = XO("Uncategorized");
    }
    if (bkey.empty()) {
        bkey = XO("Uncategorized");
    }

    return
        std::make_tuple(
        akey.Translation(), a->GetSymbol().Translation(), a->GetPath())
        < std::make_tuple(
        bkey.Translation(), b->GetSymbol().Translation(), b->GetPath());
}

bool ComapareEffectsByTypeAndPublisher(const PluginDescriptor* a, const PluginDescriptor* b)
{
    auto& em = EffectManager::Get();
    auto aType = em.GetEffectFamilyName(a->GetID());
    auto bType = em.GetEffectFamilyName(b->GetID());
    auto aVendor = em.GetVendorName(a->GetID());
    auto bVendor = em.GetVendorName(b->GetID());

    if (aType.empty()) {
        aType = XO("Uncategorized");
    }
    if (bType.empty()) {
        bType = XO("Uncategorized");
    }
    if (aVendor.empty()) {
        aVendor = XO("Unknown");
    }
    if (bVendor.empty()) {
        bVendor = XO("Unknown");
    }

    return
        std::make_tuple(
        aType.Translation(), aVendor.Translation(), a->GetSymbol().Translation(), a->GetPath())
        < std::make_tuple(
        bType.Translation(), bVendor.Translation(), b->GetSymbol().Translation(), b->GetPath());
}

bool IsEnabledPlugin(const PluginDescriptor* plug)
{
    if (PluginManager::Get().IsPluginLoaded(plug->GetID()) && EffectManager::Get().IsHidden(plug->GetID())) {
        return false;
    }
    if (!plug->IsEnabled()) {
        return false;// don't add to menus!
    }
    return true;
}

bool IsDefaultPlugin(const PluginDescriptor* plug)
{
    if (plug->IsEffectDefault()) {
        return true;
    }
    return false;
}

bool IsBundledPlugin(const PluginDescriptor* plug)
{
    if (IsDefaultPlugin(plug)) {
        return true;
    }
    auto applicationResourcePath = wxFileName(FileNames::ResourcesDir());
    auto pluginPath = wxFileName(plug->GetPath());
    pluginPath.MakeAbsolute();
    return pluginPath.GetPath().StartsWith(applicationResourcePath.GetPath());
}

auto MakeGroupsFilter(const EffectsMenuGroups& list) -> auto
{
    return [=](const PluginDescriptor* plug)
    {
        if (!IsEnabledPlugin(plug)) {
            return false;
        }

        for (auto& p : list) {
            for (auto& name : p.second) {
                if (name == plug->GetSymbol().Msgid()) {
                    return true;
                }
            }
        }
        return false;
    };
}
}

void MenuHelper::PopulateEffectsMenu(
    Group& menuItems,
    EffectType type,
    CommandFlag batchflags,
    const wxString& groupby,
    void (*onMenuCommand)(const CommandContext&),
    std::function<bool(const PluginDescriptor&)> pred)
{
    PluginManager& pm = PluginManager::Get();

    std::vector<MenuSectionBuilder> sections;

    auto MakeAddSortedItems = [=](SortBy sortby)
    {
        return [=](Group& items, std::vector<const PluginDescriptor*>& plugins)
        {
            return AddSortedEffectMenuItems(items, plugins, batchflags, sortby, onMenuCommand);
        };
    };

    auto MakeAddGroupedItems = [=](GroupBy groupBy)
    {
        return [=](Group& items, std::vector<const PluginDescriptor*>& plugins)
        {
            return AddGroupedEffectMenuItems(items, plugins, batchflags, groupBy, onMenuCommand);
        };
    };

    auto DefaultFilter = [](auto plug) { return IsEnabledPlugin(plug) && IsDefaultPlugin(plug); };
    if (groupby == "default") {
        if (type == EffectTypeProcess) {
            static auto effectMenuDefaults = [] {
                wxFileName path = wxFileName(FileNames::ResourcesDir(), wxT("EffectsMenuDefaults.xml"));
                return LoadEffectsMenuGroups(path.GetFullPath());
            }();
            static auto groupsFilter = MakeGroupsFilter(effectMenuDefaults);

            sections.emplace_back(
                MenuSectionBuilder {
                {},
                [=](auto plug) { return IsBundledPlugin(plug) && groupsFilter(plug); },
                nullptr,
                MakeAddGroupItems(effectMenuDefaults, batchflags, onMenuCommand)
            });
            sections.emplace_back(
                MenuSectionBuilder {
                {},
                IsEnabledPlugin,
                CompareEffectsByPublisher,
                MakeAddGroupedItems(GroupBy::Publisher)
            });
        } else {//Generators/Analyzers
            sections.emplace_back(
                MenuSectionBuilder {
                {},
                [](auto plug){ return IsEnabledPlugin(plug) && IsBundledPlugin(plug); },
                CompareEffectsByName,
                MakeAddSortedItems(SortBy::Name)
            });
            sections.emplace_back(
                MenuSectionBuilder {
                {},
                IsEnabledPlugin,
                CompareEffectsByPublisher,
                MakeAddGroupedItems(GroupBy::Publisher)
            });
        }
    } else if (groupby == "sortby:publisher:name") {
        sections.emplace_back(
            MenuSectionBuilder {
            {},
            DefaultFilter,
            CompareEffectsByName,
            MakeAddSortedItems(SortBy::PublisherName)
        });
        sections.emplace_back(
            MenuSectionBuilder {
            {},
            IsEnabledPlugin,
            CompareEffectsByPublisherAndName,
            MakeAddSortedItems(SortBy::PublisherName)
        });
    } else if (groupby == "sortby:type:name") {
        sections.emplace_back(
            MenuSectionBuilder {
            {},
            DefaultFilter,
            CompareEffectsByName,
            MakeAddSortedItems(SortBy::TypeName)
        });
        sections.emplace_back(
            MenuSectionBuilder {
            {},
            IsEnabledPlugin,
            CompareEffectsByPublisherAndName,
            MakeAddSortedItems(SortBy::TypeName)
        });
    } else if (groupby == "groupby:publisher") {
        sections.emplace_back(
            MenuSectionBuilder {
            {},
            DefaultFilter,
            CompareEffectsByPublisher,
            MakeAddGroupedItems(GroupBy::Publisher)
        });
        sections.emplace_back(
            MenuSectionBuilder {
            {},
            IsEnabledPlugin,
            CompareEffectsByPublisher,
            MakeAddGroupedItems(GroupBy::Publisher)
        });
    } else if (groupby == "groupby:type") {
        sections.emplace_back(
            MenuSectionBuilder {
            {},
            DefaultFilter,
            CompareEffectsByType,
            MakeAddGroupedItems(GroupBy::Type)
        });
        sections.emplace_back(
            MenuSectionBuilder {
            {},
            IsEnabledPlugin,
            CompareEffectsByType,
            MakeAddGroupedItems(GroupBy::Type)
        });
    } else if (groupby == "groupby:type:publisher") {
        sections.emplace_back(
            MenuSectionBuilder {
            {},
            DefaultFilter,
            CompareEffectsByType,
            MakeAddGroupedItems(GroupBy::Type)
        });
        sections.push_back(
            MenuSectionBuilder {
            {},
            IsEnabledPlugin,
            ComapareEffectsByTypeAndPublisher,
            MakeAddGroupedItems(GroupBy::TypePublisher)
        });
    } else { //if(groupby == "sortby:name")
        sections.emplace_back(
            MenuSectionBuilder {
            {},
            DefaultFilter,
            CompareEffectsByName,
            MakeAddSortedItems(SortBy::Name)
        });
        sections.emplace_back(
            MenuSectionBuilder {
            {},
            IsEnabledPlugin,
            CompareEffectsByName,
            MakeAddSortedItems(SortBy::Name)
        });
    }
    for (auto& plugin : pm.EffectsOfType(type)) {
        if (pred && !pred(plugin)) {
            continue;
        }

        for (auto& section : sections) {
            if (section.filter(&plugin)) {
                section.plugins.push_back(&plugin);
                break;
            }
        }
    }

    for (auto& section : sections) {
        if (section.compare != nullptr) {
            std::sort(section.plugins.begin(), section.plugins.end(), section.compare);
        }

        if (menuItems.empty()) {
            auto group = MenuRegistry::Items("");
            section.add(*group, section.plugins);
            if (group->empty()) {
                continue;
            }
            menuItems.push_back(move(group));
        } else {
            auto group = MenuRegistry::Section("");
            section.add(*group, section.plugins);
            if (group->empty()) {
                continue;
            }
            menuItems.push_back(move(group));
        }
    }
}
