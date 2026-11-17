/*
* Audacity: A Digital Audio Editor
*/
#include "appshortcutsscopes.h"

using namespace au::context;
using namespace muse;
using namespace muse::shortcuts;

static const QString TRACK_VIEW_SECTION_NAME("TrackViewSection");

void AppShortcutsScopes::init()
{
    if (!shortcutsResolver()) {
        return;
    }

    std::weak_ptr<const AppShortcutsScopes> weakSelf = weak_from_this();

    auto when = [weakSelf](Condition condition) {
        return [weakSelf, condition]() {
            std::shared_ptr<const AppShortcutsScopes> self = weakSelf.lock();
            return self && (self.get()->*condition)();
        };
    };

    const auto whenProjectPageShown = when(&AppShortcutsScopes::isProjectPageShown);

    IShortcutsResolver* resolver = shortcutsResolver().get();

    //: Group heading in Preferences > Shortcuts for shortcuts that move the keyboard focus between panels and controls
    resolver->setScope({ "NAVIGATION", TranslatableString("shortcuts", "Navigation"), 1 });
    //: Group heading in Preferences > Shortcuts for shortcuts that control playback
    resolver->setScope({ "PLAYBACK", TranslatableString("shortcuts", "Playback"), 2, whenProjectPageShown });
    //: Group heading in Preferences > Shortcuts for shortcuts that control recording
    resolver->setScope({ "RECORD", TranslatableString("shortcuts", "Recording"), 2, whenProjectPageShown });
    //: Group heading in Preferences > Shortcuts for shortcuts that work only while the timeline has the keyboard focus
    resolver->setScope({ "TIMELINE", TranslatableString("shortcuts", "Timeline"), 3, when(&AppShortcutsScopes::isProjectFocused) });
    //: Group heading in Preferences > Shortcuts for shortcuts that work only while a track or a track item has the keyboard focus
    resolver->setScope({ "TRACK_NAVIGATION_SECTION", TranslatableString("shortcuts", "Track view"), 4,
                         when(&AppShortcutsScopes::isTrackViewFocused) });

    //: Group heading in Preferences > Shortcuts for shortcuts that change how the project is shown
    resolver->setScope({ "PROJECTSCENE", TranslatableString("shortcuts", "Project view"), 0, whenProjectPageShown });
    //: Group heading in Preferences > Shortcuts for shortcuts that edit audio, clips, labels and tracks
    resolver->setScope({ "TRACKEDIT", TranslatableString("shortcuts", "Editing"), 0, whenProjectPageShown });
    //: Group heading in Preferences > Shortcuts for shortcuts that move the keyboard focus between tracks and track items
    resolver->setScope({ "TRACK-NAVIGATION", TranslatableString("shortcuts", "Track navigation"), 0, whenProjectPageShown });
    //: Group heading in Preferences > Shortcuts for shortcuts that open or apply effects
    resolver->setScope({ "EFFECTS", TranslatableString("shortcuts", "Effects"), 0, whenProjectPageShown });
    //: Group heading in Preferences > Shortcuts for shortcuts of the spectrogram view
    resolver->setScope({ "SPECTROGRAM", TranslatableString("shortcuts", "Spectrogram"), 0, whenProjectPageShown });

    //: Group heading in Preferences > Shortcuts for shortcuts that act on the whole application
    resolver->setScope({ "APP", TranslatableString("shortcuts", "App") });
    //: Group heading in Preferences > Shortcuts for shortcuts that manage the project file
    resolver->setScope({ "PROJECT", TranslatableString("shortcuts", "Project") });
    //: Group heading in Preferences > Shortcuts for shortcuts of the cloud service
    resolver->setScope({ "CLOUD", TranslatableString("shortcuts", "Cloud") });
    //: Group heading in Preferences > Shortcuts for shortcuts of the audio engine and audio devices
    resolver->setScope({ "AUDIO", TranslatableString("shortcuts", "Audio") });
    //: Group heading in Preferences > Shortcuts for shortcuts of VST plugins; "VST" is a product name, do not translate it
    resolver->setScope({ "VST", TranslatableString("shortcuts", "VST") });
    //: Group heading in Preferences > Shortcuts for shortcuts that show, hide or move panels and toolbars
    resolver->setScope({ "DOCK", TranslatableString("shortcuts", "Dock") });
    //: Group heading in Preferences > Shortcuts for shortcuts that change the workspace (the layout of panels and toolbars)
    resolver->setScope({ "WORKSPACE", TranslatableString("shortcuts", "Workspace") });
    //: Group heading in Preferences > Shortcuts for shortcuts of extensions (add-ons that users install)
    resolver->setScope({ "EXTENSIONS", TranslatableString("shortcuts", "Extensions") });
    //: Group heading in Preferences > Shortcuts for shortcuts that manage the shortcuts themselves
    resolver->setScope({ "SHORTCUTS", TranslatableString("shortcuts", "Shortcuts") });
    //: Group heading in Preferences > Shortcuts for shortcuts that manage application windows
    resolver->setScope({ "MULTIWINDOWS", TranslatableString("shortcuts", "Windows") });
    //: Group heading in Preferences > Shortcuts for shortcuts that check for or install application updates
    resolver->setScope({ "UPDATE", TranslatableString("shortcuts", "Update") });
    //: Group heading in Preferences > Shortcuts for shortcuts of diagnostic tools for developers
    resolver->setScope({ "DIAGNOSTICS", TranslatableString("shortcuts", "Diagnostics") });
    //: Group heading in Preferences > Shortcuts for shortcuts of the automated test tool for developers; "Testflow" is a tool name, do not translate it
    resolver->setScope({ "TESTFLOW", TranslatableString("shortcuts", "Testflow") });
}

bool AppShortcutsScopes::isProjectPageShown() const
{
    return uiContextResolver()->matchWithCurrent(UiCtxProjectOpened);
}

bool AppShortcutsScopes::isProjectFocused() const
{
    return uiContextResolver()->matchWithCurrent(UiCtxProjectFocused);
}

bool AppShortcutsScopes::isTrackViewFocused() const
{
    const muse::ui::INavigationSection* section = navigationController()->activeSection();
    return section && section->name() == TRACK_VIEW_SECTION_NAME && isProjectFocused();
}
