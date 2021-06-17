/**********************************************************************

Audacity: A Digital Audio Editor

PrefsPanel.cpp

Paul Licameli split from PrefsDialog.cpp

**********************************************************************/

#include "PrefsPanel.h"
#include <mutex>

namespace {
const auto PathStart = wxT("Preferences");

static Registry::GroupItem &sRegistry()
{
   static Registry::TransparentGroupItem<> registry{ PathStart };
   return registry;
}

struct PrefsItem final : Registry::ConcreteGroupItem<false> {
   PrefsPanel::Factory factory;
   bool expanded{ false };

   PrefsItem( const wxString &name,
      const PrefsPanel::Factory &factory_, bool expanded_ )
         : ConcreteGroupItem<false>{ name }
         , factory{ factory_ }, expanded{ expanded_ }
   {}
};

// Collects registry tree nodes into a vector, in preorder.
struct PrefsItemVisitor final : Registry::Visitor {
   PrefsItemVisitor( PrefsPanel::Factories &factories_ )
      : factories{ factories_ }
   {
      childCounts.push_back( 0 );
   }
   void BeginGroup( Registry::GroupItem &item, const Path & ) override
   {
      auto pItem = dynamic_cast<PrefsItem*>( &item );
      if (!pItem)
         return;
      indices.push_back( factories.size() );
      factories.emplace_back( pItem->factory, 0, pItem->expanded );
      ++childCounts.back();
      childCounts.push_back( 0 );
   }
   void EndGroup( Registry::GroupItem &item, const Path & ) override
   {
      auto pItem = dynamic_cast<PrefsItem*>( &item );
      if (!pItem)
         return;
      auto &factory = factories[ indices.back() ];
      factory.nChildren = childCounts.back();
      childCounts.pop_back();
      indices.pop_back();
   }

   PrefsPanel::Factories &factories;
   std::vector<size_t> childCounts;
   std::vector<size_t> indices;
};
}

PluginPath PrefsPanel::GetPath()
{ return BUILTIN_PREFS_PANEL_PREFIX + GetSymbol().Internal(); }

VendorSymbol PrefsPanel::GetVendor()
{  return XO("Audacity");}

wxString PrefsPanel::GetVersion()
{     return AUDACITY_VERSION_STRING;}

PrefsPanel::Registration::Registration( const wxString &name,
   const Factory &factory, bool expanded,
   const Registry::Placement &placement )
{
   Registry::RegisterItem( sRegistry(), placement,
      std::make_unique< PrefsItem >( name, factory, expanded ) );
}

PrefsPanel::~PrefsPanel()
{
}

void PrefsPanel::Cancel()
{
}

bool PrefsPanel::ShowsPreviewButton()
{
   return false;
}

wxString PrefsPanel::HelpPageName()
{
   return wxEmptyString;
}

PrefsPanel::Factories
&PrefsPanel::DefaultFactories()
{
   // Once only, cause initial population of preferences for the ordering
   // of some preference pages that used to be given in a table but are now
   // separately registered in several .cpp files; the sequence of registration
   // depends on unspecified accidents of static initialization order across
   // compilation units, so we need something specific here to preserve old
   // default appearance of the preference dialog.
   // But this needs only to mention some strings -- there is no compilation or
   // link dependency of this source file on those other implementation files.
   static Registry::OrderingPreferenceInitializer init{
      PathStart,
      {
         {wxT(""),
       wxT("Device,Playback,Recording,Quality,GUI,Tracks,ImportExport,Directories,Warnings,Effects,KeyConfig,Mouse")
         },
         {wxT("/Tracks"), wxT("TracksBehaviors,Spectrum")},
      }
   };

   static Factories factories;
   static std::once_flag flag;

   std::call_once( flag, []{
      PrefsItemVisitor visitor{ factories };
      Registry::TransparentGroupItem<> top{ PathStart };
      Registry::Visit( visitor, &top, &sRegistry() );
   } );
   return factories;
}
