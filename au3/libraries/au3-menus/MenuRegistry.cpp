/**********************************************************************

  Audacity: A Digital Audio Editor

  MenuRegistry.cpp

  (formerly Menus.cpp)

  Dominic Mazzoni
  Brian Gunlogson
  et al.

*//*******************************************************************/
#include "MenuRegistry.h"
#include "Project.h"
#include "BasicUI.h"
#include <wx/log.h>

namespace MenuRegistry {
auto Options::MakeCheckFn(const wxString key, bool defaultValue) -> CheckFn
{
    return [=](AudacityProject&){ return gPrefs->ReadBool(key, defaultValue); };
}

auto Options::MakeCheckFn(const BoolSetting& setting) -> CheckFn
{
    return MakeCheckFn(setting.GetPath(), setting.GetDefault());
}

std::pair<bool, bool> detail::VisitorBase::ShouldBeginGroup(
    const ItemProperties* pProperties)
{
    const auto properties
        =pProperties ? pProperties->GetProperties() : ItemProperties::None;

    bool inlined = false;
    bool shouldDoSeparator = false;

    switch (properties) {
    case ItemProperties::Inline: {
        inlined = true;
        break;
    }
    case ItemProperties::Section: {
        if (!needSeparator.empty()) {
            needSeparator.back() = true;
        }
        break;
    }
    case ItemProperties::Whole:
    case ItemProperties::Extension: {
        shouldDoSeparator = ShouldDoSeparator();
        break;
    }
    default:
        break;
    }

    return { !inlined, shouldDoSeparator };
}

void detail::VisitorBase::AfterBeginGroup(
    const ItemProperties* pProperties)
{
    const auto properties
        =pProperties ? pProperties->GetProperties() : ItemProperties::None;

    bool isMenu = false;
    bool isExtension = false;

    switch (properties) {
    case ItemProperties::Whole:
    case ItemProperties::Extension: {
        isMenu = true;
        isExtension = (properties == ItemProperties::Extension);
        break;
    }
    default:
        break;
    }

    if (isMenu) {
        needSeparator.push_back(false);
        firstItem.push_back(!isExtension);
    }
}

bool detail::VisitorBase::ShouldEndGroup(
    const ItemProperties* pProperties)
{
    const auto properties
        =pProperties ? pProperties->GetProperties() : ItemProperties::None;

    bool inlined = false;

    switch (properties) {
    case ItemProperties::Inline: {
        inlined = true;
        break;
    }
    case ItemProperties::Section: {
        if (!needSeparator.empty()) {
            needSeparator.back() = true;
        }
        break;
    }
    case ItemProperties::Whole:
    case ItemProperties::Extension: {
        firstItem.pop_back();
        needSeparator.pop_back();
        break;
    }
    default:
        break;
    }

    return !inlined;
}

bool detail::VisitorBase::ShouldDoSeparator()
{
    bool separate = false;
    if (!needSeparator.empty()) {
        separate = needSeparator.back() && !firstItem.back();
        needSeparator.back() = false;
        firstItem.back() = false;
    }
    return separate;
}

MenuItem::~MenuItem() {}
auto MenuItem::GetProperties() const -> Properties { return Whole; }

ConditionalGroupItem::~ConditionalGroupItem() {}

CommandItem::CommandItem(const CommandID& name_,
                         const TranslatableString& label_in_,
                         CommandFunctorPointer callback_,
                         CommandFlag flags_,
                         const Options& options_,
                         CommandHandlerFinder finder_)
    : SingleItem{name_}, label_in{label_in_}
    , finder{finder_}, callback{callback_}
    , flags{flags_}, options{options_}
{}
CommandItem::~CommandItem() {}

CommandGroupItem::CommandGroupItem(const Identifier& name_,
                                   std::vector< ComponentInterfaceSymbol > items_,
                                   CommandFunctorPointer callback_,
                                   CommandFlag flags_,
                                   bool isEffect_,
                                   CommandHandlerFinder finder_)
    : SingleItem{name_}, items{std::move(items_)}
    , finder{finder_}, callback{callback_}
    , flags{flags_}, isEffect{isEffect_}
{}
CommandGroupItem::~CommandGroupItem() {}

SpecialItem::~SpecialItem() {}
MenuPart::~MenuPart() {}
auto MenuPart::GetProperties() const -> Properties { return Section; }

MenuItems::~MenuItems() {}
auto MenuItems::GetOrdering() const -> Ordering
{
    return name.empty() ? Anonymous : Weak;
}

auto MenuItems::GetProperties() const -> Properties { return Inline; }

CommandHandlerFinder FinderScope::sFinder
    =[](AudacityProject& project) -> CommandHandlerObject& {
    // If this default finder function is reached, then FinderScope should
    // have been used somewhere but was not, or an explicit
    // CommandHandlerFinder was not passed to menu item constructors
    wxASSERT(false);
    return project;
};
}

namespace {
using namespace Registry;

const auto MenuPathStart = wxT("MenuBar");
}

auto MenuRegistry::ItemRegistry::Registry() -> Registry::GroupItem<Traits>&
{
    static GroupItem<Traits> registry{ MenuPathStart };
    return registry;
}

void MenuRegistry::Visit(Visitor<Traits>& visitor, AudacityProject& project)
{
    // Once only, cause initial population of preferences for the ordering
    // of some menu items that used to be given in tables but are now separately
    // registered in several .cpp files; the sequence of registration depends
    // on unspecified accidents of static initialization order across
    // compilation units, so we need something specific here to preserve old
    // default appearance of menus.
    // But this needs only to mention some strings -- there is no compilation or
    // link dependency of this source file on those other implementation files.

    static Registry::OrderingPreferenceInitializer init{
        MenuPathStart,
        {
            { wxT(""), wxT(
                  "File,Edit,Select,View,Transport,Tracks,Generate,Effect,Analyze,Tools,Window,Optional,Help"
                  ) },
            { wxT("/Optional/Extra/Part1"), wxT(
                  "Transport,Tools,Mixer,Edit,PlayAtSpeed,Seek,Device,Select"
                  ) },
            { wxT("/Optional/Extra/Part2"), wxT(
                  "Navigation,Focus,Cursor,Track,Scriptables1,Scriptables2"
                  ) },
            { wxT("/View/Windows"), wxT("UndoHistory,MixerBoard") },
            { wxT("/Analyze/Analyzers/Windows"), wxT("ContrastAnalyser,PlotSpectrum") },
            { wxT("/Transport/Basic"), wxT("Play,Record,Scrubbing,Cursor") },
            { wxT("/View/Other/Toolbars/Toolbars/Other"), wxT(
                  "ShowTransportTB,ShowToolsTB,ShowRecordMeterTB,ShowPlayMeterTB,"
//"ShowMeterTB,"
                  "ShowMixerTB,"
                  "ShowEditTB,ShowTranscriptionTB,ShowScrubbingTB,ShowDeviceTB,ShowSelectionTB,"
                  "ShowSpectralSelectionTB") },
            { wxT("/Tracks/Add/Add"), wxT(
                  "NewMonoTrack,NewStereoTrack,NewLabelTrack,NewTimeTrack") },
            { wxT("/Optional/Extra/Part2/Scriptables1"), wxT(
                  "SelectTime,SelectFrequencies,SelectTracks,SetTrackStatus,SetTrackAudio,"
                  "SetTrackVisuals,GetPreference,SetPreference,SetClip,SetEnvelope,SetLabel"
                  "SetProject") },
            { wxT("/Optional/Extra/Part2/Scriptables2"), wxT(
                  "Select,SetTrack,GetInfo,Message,Help,Import2,Export2,OpenProject2,"
                  "SaveProject2,Drag,CompareAudio") },
        }
    };

    static const auto menuTree = MenuRegistry::Items(MenuPathStart);

    wxLogNull nolog;
    Registry::VisitWithFunctions(visitor, menuTree.get(),
                                 &MenuRegistry::ItemRegistry::Registry(), project);
}
