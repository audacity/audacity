#include "ExportPluginRegistry.h"

#include "ExportPlugin.h"

namespace {
const auto PathStart = L"Exporters";
}

auto ExportPluginRegistry::ExportPluginRegistryItem::Registry()
-> Registry::GroupItem<Traits>&
{
    static Registry::GroupItem<Traits> registry{ PathStart };
    return registry;
}

ExportPluginRegistry::ExportPluginRegistryItem::ExportPluginRegistryItem(
    const Identifier& id, Factory factory)
    : SingleItem{id}
    , mFactory{std::move(factory)}
{}

ExportPluginRegistry::ExportPluginRegistry() = default;

ExportPluginRegistry::ConstIterator& ExportPluginRegistry::ConstIterator::operator++()
{
    ++mFormatIndex;
    if (mFormatIndex >= (*mPluginIt)->GetFormatCount()) {
        mFormatIndex = 0;
        ++mPluginIt;
    }
    return *this;
}

ExportPluginRegistry::~ExportPluginRegistry() = default;

ExportPluginRegistry::RegisteredPlugin::RegisteredPlugin(
    const Identifier& id,
    const Factory& factory,
    const Registry::Placement& placement)
    : RegisteredItem{
                     factory ? std::make_unique< ExportPluginRegistryItem >(id, factory) : nullptr,
                     placement
                     }
{
}

ExportPluginRegistry& ExportPluginRegistry::Get()
{
    static ExportPluginRegistry registry;
    return registry;
}

void ExportPluginRegistry::Initialize()
{
    using namespace Registry;
    static OrderingPreferenceInitializer init{
        PathStart,
        { { wxT(""), wxT("PCM,MP3,OGG,Opus,FLAC,WavPack,FFmpeg,MP2,CommandLine") } },
    };

    // visit the registry to collect the plug-ins properly
    // sorted
    GroupItem<Traits> top{ PathStart };
    Registry::Visit(
        [&](const ExportPluginRegistryItem& item, auto&){
        mPlugins.emplace_back(item.mFactory());
    },
        &top, &ExportPluginRegistryItem::Registry());
}

std::tuple<ExportPlugin*, int>
ExportPluginRegistry::FindFormat(const wxString& format, bool compareWithCase) const
{
    for (auto t: *this) {
        const auto [plugin, formatIndex] = t;
        if (plugin->GetFormatInfo(formatIndex).format.IsSameAs(format, compareWithCase)) {
            return t;
        }
    }
    return {};
}
