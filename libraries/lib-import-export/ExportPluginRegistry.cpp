#include "ExportPluginRegistry.h"

#include "ExportPlugin.h"

namespace {
   const auto PathStart = L"Exporters";
}

Registry::GroupItemBase &ExportPluginRegistry::ExportPluginRegistryItem::Registry()
{
   static Registry::GroupItem<Registry::DefaultTraits> registry{ PathStart };
   return registry;
}

ExportPluginRegistry::ExportPluginRegistryItem::ExportPluginRegistryItem(
   const Identifier &id, Factory factory )
   : SingleItem{ id }
   , mFactory{ std::move(factory) }
{}

ExportPluginRegistry::ExportPluginRegistry() = default;

ExportPluginRegistry::ConstIterator& ExportPluginRegistry::ConstIterator::operator++()
{
   ++mFormatIndex;
   if(mFormatIndex >= (*mPluginIt)->GetFormatCount())
   {
      mFormatIndex = 0;
      ++mPluginIt;
   }
   return *this;
}

ExportPluginRegistry::~ExportPluginRegistry() = default;

ExportPluginRegistry::RegisteredPlugin::RegisteredPlugin(
   const Identifier &id,
   const Factory &factory,
   const Registry::Placement &placement )
   : RegisteredItem{
      factory ? std::make_unique< ExportPluginRegistryItem >( id, factory ) : nullptr,
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
      { {wxT(""), wxT("PCM,MP3,OGG,FLAC,MP2,CommandLine,FFmpeg") } },
   };

   struct CreatePluginsVisitor final : Visitor {
      CreatePluginsVisitor(ExportPlugins& plugins)
         : mPlugins(plugins)
      {
      }

      void Visit( SingleItem &item, const Path &path ) override
      {
         mPlugins.emplace_back(
            static_cast<ExportPluginRegistryItem&>( item ).mFactory() );
      }

      ExportPlugins& mPlugins;
   } visitor(mPlugins);
   // visit the registry to collect the plug-ins properly
   // sorted
   GroupItem<Registry::DefaultTraits> top{ PathStart };
   Registry::Visit( visitor, &top, &ExportPluginRegistryItem::Registry() );
}

std::tuple<ExportPlugin*, int>
ExportPluginRegistry::FindFormat(const wxString& format, bool compareWithCase) const
{
   for(auto t: *this)
   {
      const auto [plugin, formatIndex] = t;
      if(plugin->GetFormatInfo(formatIndex).format.IsSameAs(format, compareWithCase))
         return t;
   }
   return {};
}
