/**********************************************************************

Audacity: A Digital Audio Editor

PrefsPanel.cpp

Paul Licameli split from PrefsDialog.cpp

**********************************************************************/

#include "PrefsPanel.h"
#include <mutex>

static const auto PathStart = L"Preferences";

auto PrefsPanel::PrefsItem::Registry() -> Registry::GroupItem<Traits>&
{
    static Registry::GroupItem<Traits> registry{ PathStart };
    return registry;
}

PrefsPanel::PrefsItem::PrefsItem(const wxString& name,
                                 const PrefsPanel::Factory& factory, bool expanded)
    : GroupItem{name}
    , factory{factory}
    , expanded{expanded}
{}

PluginPath PrefsPanel::GetPath() const
{ return BUILTIN_PREFS_PANEL_PREFIX + GetSymbol().Internal(); }

VendorSymbol PrefsPanel::GetVendor() const
{ return XO("Audacity"); }

wxString PrefsPanel::GetVersion() const
{ return AUDACITY_VERSION_STRING; }

PrefsPanel::Registration::Registration(const wxString& name,
                                       const Factory& factory, bool expanded,
                                       const Registry::Placement& placement)
    : RegisteredItem{
                     std::make_unique< PrefsItem >(name, factory, expanded), placement}
{
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

ManualPageID PrefsPanel::HelpPageName()
{
    return {};
}

PrefsPanel::Factories
& PrefsPanel::DefaultFactories()
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
            { wxT(""),
              wxT("Device,Playback,Recording,Quality,GUI,Tracks,ImportExport,Directories,Warnings,Effects,KeyConfig,Mouse")
            },
            { wxT("/Tracks"), wxT("TracksBehaviors,Spectrum") },
        }
    };

    static Factories sFactories;
    static std::once_flag flag;

    std::call_once(flag, []{
        // Collect registry tree nodes into a vector, in preorder.
        std::vector<size_t> childCounts;
        std::vector<size_t> indices;
        childCounts.push_back(0);
        Factories factories;

        Registry::GroupItem<Traits> top { PathStart };
        Registry::Visit(std::tuple {
            [&](const PrefsItem& item, auto&) {
                if (!item.factory) {
                    return;
                }
                indices.push_back(factories.size());
                factories.emplace_back(item.factory, 0, item.expanded);
                ++childCounts.back();
                childCounts.push_back(0);
            },
            Registry::NoOp,
            [&](const PrefsItem& item, auto&) {
                if (!item.factory) {
                    return;
                }
                auto& factory = factories[indices.back()];
                factory.nChildren = childCounts.back();
                childCounts.pop_back();
                indices.pop_back();
            }
        }, &top, &PrefsItem::Registry());
        sFactories.swap(factories);
    });
    return sFactories;
}
