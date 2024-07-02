/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2021 MuseScore BVBA and others
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
#include "appmenumodel.h"

#include "types/translatablestring.h"

#include "log.h"

using namespace mu;
using namespace au::appshell;
using namespace muse;
using namespace muse::ui;
using namespace muse::uicomponents;
using namespace muse::actions;

using namespace au::project;
//! TODO AU4
// using namespace muse::workspace;
// using namespace muse::extensions;

static QString makeId(const ActionCode& actionCode, int itemIndex)
{
    return QString::fromStdString(actionCode) + QString::number(itemIndex);
}

AppMenuModel::AppMenuModel(QObject* parent)
    : AbstractMenuModel(parent)
{
}

void AppMenuModel::load()
{
    TRACEFUNC;

    AbstractMenuModel::load();

    MenuItemList items {
        makeFileMenu(),
        makeEditMenu(),
        makeSelectMenu(),
        makeViewMenu(),
        makeRecordMenu(),
        makeTracksMenu(),
        makeGenerateMenu(),
        makeEffectMenu(),
        makeAnalyzeMenu(),
        makeToolsMenu(),
        makeExtraMenu(),
        makeHelpMenu()
    };

    setItems(items);

    setupConnections();

    //! NOTE: removes some undesired platform-specific items
    //! (such as "Start Dictation" and "Special Characters" on macOS)
    appMenuModelHook()->onAppMenuInited();
}

bool AppMenuModel::isGlobalMenuAvailable()
{
    return uiConfiguration()->isGlobalMenuAvailable();
}

void AppMenuModel::setupConnections()
{
    recentFilesController()->recentFilesListChanged().onNotify(this, [this]() {
        MenuItem& recentScoreListItem = findMenu("menu-file-open");

        MenuItemList recentScoresList = makeRecentProjectsItems();
        bool openRecentEnabled = !recentScoresList.empty();

        if (!recentScoresList.empty()) {
            recentScoresList = appendClearRecentSection(recentScoresList);
        }

        UiActionState state = recentScoreListItem.state();
        state.enabled = openRecentEnabled;
        recentScoreListItem.setState(state);

        recentScoreListItem.setSubitems(recentScoresList);
    });

    //! TODO AU4
    // workspacesManager()->currentWorkspaceChanged().onNotify(this, [this]() {
    //     MenuItem& workspacesItem = findMenu("menu-workspaces");
    //     workspacesItem.setSubitems(makeWorkspacesItems());
    // });

    // workspacesManager()->workspacesListChanged().onNotify(this, [this]() {
    //     MenuItem& workspacesItem = findMenu("menu-workspaces");
    //     workspacesItem.setSubitems(makeWorkspacesItems());
    // });

    // extensionsProvider()->manifestListChanged().onNotify(this, [this]() {
    //     MenuItem& pluginsMenu = findMenu("menu-plugins");
    //     pluginsMenu.setSubitems(makePluginsMenuSubitems());
    // });

    // extensionsProvider()->manifestChanged().onReceive(this, [this](const Manifest&) {
    //     MenuItem& pluginsItem = findMenu("menu-plugins");
    //     pluginsItem.setSubitems(makePluginsMenuSubitems());
    // });
}

MenuItem* AppMenuModel::makeMenuItem(const actions::ActionCode& actionCode, MenuItemRole menuRole)
{
    MenuItem* item = makeMenuItem(actionCode);
    item->setRole(menuRole);
    return item;
}

MenuItem* AppMenuModel::makeFileMenu()
{
    MenuItemList recentScoresList = makeRecentProjectsItems();
    bool openRecentEnabled = !recentScoresList.isEmpty();

    if (!recentScoresList.empty()) {
        recentScoresList = appendClearRecentSection(recentScoresList);
    }

    MenuItemList fileItems {
        makeMenuItem("file-new"),
        makeMenuItem("file-open"),
        makeMenu(TranslatableString("appshell/menu/file", "Open &recent"), recentScoresList, "menu-file-open", openRecentEnabled),

        makeSeparator(),

        makeMenuItem("project-import"),

        makeSeparator(),

        makeMenuItem("file-save"),
        makeMenuItem("file-save-as"),
        makeMenuItem("file-save-backup"),

        makeSeparator(),

        makeMenuItem("export-audio"),
        makeMenu(TranslatableString("appshell/menu/export-other", "&Export other"), makeExportItems(), "menu-export-other"),

        makeSeparator(),

        makeMenuItem("file-close"),
        makeMenuItem("quit", MenuItemRole::QuitRole)
    };

    return makeMenu(TranslatableString("appshell/menu/file", "&File"), fileItems, "menu-file");
}

MenuItem* AppMenuModel::makeEditMenu()
{
    MenuItemList editItems {
        makeMenuItem("undo"),
        makeMenuItem("redo"),
        makeSeparator(),
        makeMenuItem("cut"),
        makeMenuItem("copy"),
        makeMenuItem("paste"),
        makeMenuItem("delete"),
        makeSeparator(),
        makeMenuItem("cut-and-close-gap"),
        makeMenuItem("duplicate"),
        makeMenuItem("insert"),
        makeMenuItem("delete-and-close-gap"),
        makeSeparator(),
        makeMenu(TranslatableString("appshell/menu/clip", "Clip"), makeClipItems(), "menu-clip"),
        makeMenuItem("silence-audio"),
        makeSeparator(),
        makeMenuItem("add-label"),
        makeMenuItem("paste-new-label"),
        makeMenu(TranslatableString("appshell/menu/audio-actions", "Audio actions across labels"),
                 makeAudioActionsItems(), "menu-audio-actions"),
        makeSeparator(),
        makeMenuItem("manage-labels"),
        makeMenuItem("manage-metadata"),
        makeSeparator(),
        makeMenuItem("preference-dialog", MenuItemRole::PreferencesRole)
    };

    return makeMenu(TranslatableString("appshell/menu/edit", "&Edit"), editItems, "menu-edit");
}

MenuItem* AppMenuModel::makeSelectMenu()
{
    MenuItemList selectItems {
        makeMenuItem("select-all"),
        makeMenuItem("select-none"),
        makeSeparator(),
        makeMenuItem("select-entire-track"),
        makeMenuItem("select-from-start-to-cursor"),
        makeMenuItem("select-from-cursor-end"),
        makeSeparator(),
        makeMenuItem("select-previous-item"),
        makeMenuItem("select-next-item"),
        makeSeparator(),
        makeMenu(TranslatableString("appshell/menu/looping", "Looping"), makeLoopingItems(), "menu-looping"),
        makeMenuItem("select-near-zero-crossings"),
    };

    return makeMenu(TranslatableString("appshell/menu/select", "&Select"), selectItems, "menu-select");
}

MenuItem* AppMenuModel::makeViewMenu()
{
    MenuItemList viewItems {
        makeMenu(TranslatableString("appshell/menu/zoom", "Zoom"), makeZoomItems(), "menu-zoom"),
        makeMenu(TranslatableString("appshell/menu/skip", "Skip to "), makeSkipToItems(), "menu-skip"),
        makeSeparator(),
        makeMenuItem("toggle-mixer"),
        makeMenuItem("toggle-effects"),
        makeSeparator(),
        makeMenuItem("toggle-label-editor"),
        makeMenuItem("toggle-metadata-editor"),
        makeMenuItem("toggle-undo-history"),
        makeSeparator(),
#ifndef Q_OS_MAC
        makeMenuItem("fullscreen"),
        makeSeparator(),
#endif
        makeMenuItem("toggle-clipping-in-waveform"),
        makeMenuItem("toggle-vertical-rulers"),
        makeSeparator(),
        makeMenuItem("dock-restore-default-layout")
    };

    return makeMenu(TranslatableString("appshell/menu/view", "&View"), viewItems, "menu-view");
}

MenuItem* AppMenuModel::makeRecordMenu()
{
    MenuItemList recordItems {
        makeMenuItem("record-on-current-track"),
        makeMenuItem("record-on-new-track"),
        makeMenuItem("set-up-timed-recording"),
        makeMenuItem("punch-and-roll-record"),
        makeSeparator(),
        makeMenuItem("toggle-sound-activated-recording"),
        makeMenuItem("set-sound-activation-level"),
    };

    return makeMenu(TranslatableString("appshell/menu/record", "&Record"), recordItems, "menu-record");
}

MenuItem* AppMenuModel::makeTracksMenu()
{
    MenuItemList tracksItems {
        makeMenuItem("new-mono-track"),
        makeMenuItem("new-stereo-track"),
        makeMenuItem("new-label-track"),
        makeSeparator(),
        makeMenuItem("duplicate-track"),
        makeMenuItem("remove-tracks"),
        makeSeparator(),
        makeMenuItem("mixdown-to"),
        makeSeparator(),
        makeMenu(TranslatableString("appshell/menu/align", "Align content"), makeAlignItems(), "menu-align"),
        makeMenu(TranslatableString("appshell/menu/sort", "Sort tracks"), makeSortItems(), "menu-sort"),
        makeSeparator(),
        makeMenuItem("keep-tracks-synchronised")
    };

    return makeMenu(TranslatableString("appshell/menu/tracks", "&Tracks"), tracksItems, "menu-tracks");
}

MenuItem* AppMenuModel::makeGenerateMenu()
{
    MenuItemList generateItems {
        makeMenuItem("generate-plugin-manager"),
        makeSeparator(),
        makeMenuItem("repeat-last-generator"),
        makeSeparator(),
        makeMenuItem("generate-omitted"),
    };

    return makeMenu(TranslatableString("appshell/menu/generate", "&Generate"), generateItems, "menu-generate");
}

MenuItem* AppMenuModel::makeEffectMenu()
{
    MenuItemList effectItems {
        makeMenuItem("effect-plugin-manager"),
        makeMenuItem("add-realtime-effects"),
        makeSeparator(),
        makeMenuItem("repeat-last-effect"),
        makeSeparator(),
        makeMenuItem("favourite-effect-1"),
        makeMenuItem("favourite-effect-2"),
        makeMenuItem("favourite-effect-3"),
        makeSeparator(),
        //! TODO AU4 - expand effects
        makeMenu(TranslatableString("appshell/menu/volumecomp", "Volume and compression"),
                 makeVolumeAndCompressionItems(), "menu-volumecomp"),
        makeMenu(TranslatableString("appshell/menu/fading", "Fading"), makeVolumeAndCompressionItems(), "menu-fading"),
        makeMenu(TranslatableString("appshell/menu/pitchtempo", "Pitch and tempo"), makeVolumeAndCompressionItems(), "menu-pitchtempo"),
        makeMenu(TranslatableString("appshell/menu/eq", "EQ & filters"), makeVolumeAndCompressionItems(), "menu-eq"),
        makeMenu(TranslatableString("appshell/menu/noiserepair", "Noise removal & repair"),
                 makeVolumeAndCompressionItems(), "menu-noiserepair"),
        makeMenu(TranslatableString("appshell/menu/delayreverb", "Delay & reverb"), makeVolumeAndCompressionItems(), "menu-delayreverb"),
        makeMenu(TranslatableString("appshell/menu/distmod", "Distortion & modulation"), makeVolumeAndCompressionItems(), "menu-distmod"),
        makeMenu(TranslatableString("appshell/menu/special", "Special"), makeVolumeAndCompressionItems(), "menu-special"),
        makeMenu(TranslatableString("appshell/menu/spectral", "Spectral tools"), makeVolumeAndCompressionItems(), "menu-spectral"),
        makeSeparator(),
        makeMenuItem("plugins-omitted")
    };

    return makeMenu(TranslatableString("appshell/menu/effect", "&Effect"), effectItems, "menu-effect");
}

MenuItem* AppMenuModel::makeAnalyzeMenu()
{
    MenuItemList analyzeItems {
        makeMenuItem("analyze-plugin-manager"),
        makeSeparator(),
        makeMenuItem("repeat-last-analyzer"),
        makeSeparator(),
        makeMenuItem("contrast-analyzer"),
        makeMenuItem("plot-spectrum"),
        makeMenuItem("analyzer-omitted"),
    };

    return makeMenu(TranslatableString("appshell/menu/analyze", "&Analyze"), analyzeItems, "menu-analyze");
}

MenuItem* AppMenuModel::makeToolsMenu()
{
    MenuItemList toolsItems {
        makeMenuItem("tools-plugin-manager"),
        makeSeparator(),
        makeMenuItem("repeat-last-tool"),
        makeSeparator(),
        makeMenuItem("manage-macros"),
        makeMenu(TranslatableString("appshell/menu/macros", "&Macros"), makeMacrosItems(), "menu-macros"),
        makeSeparator(),
        makeMenuItem("nyquist-plugin-installer"),
        makeMenuItem("nyquist-prompt"),
        makeSeparator(),
        makeMenuItem("sample-data-export"),
        makeMenuItem("sample-data-import"),
        makeMenuItem("raw-data-import"),
        makeSeparator(),
        makeMenuItem("reset-configuration")
    };

    return makeMenu(TranslatableString("appshell/menu/tools", "&Tools"), toolsItems, "menu-tools");
}

MenuItemList AppMenuModel::makePluginsMenuSubitems()
{
    MenuItemList subitems {
        makeMenuItem("manage-plugins"),
    };

    MenuItemList enabledPlugins = makePluginsItems();

    if (!enabledPlugins.empty()) {
        subitems << makeSeparator();
    }

    subitems << enabledPlugins;

    return subitems;
}

MenuItem* AppMenuModel::makeExtraMenu()
{
    MenuItemList extraItems {
        //! TODO AU4
        makeMenu(TranslatableString("appshell/menu/play", "Play"), makeVolumeAndCompressionItems(), "menu-play"),
        makeMenu(TranslatableString("appshell/menu/scrubbing", "Scrubbing"), makeVolumeAndCompressionItems(), "menu-scrubbing"),
        makeMenu(TranslatableString("appshell/menu/extratools", "Tools"), makeVolumeAndCompressionItems(), "menu-extra-tools"),
        makeMenu(TranslatableString("appshell/menu/mixer", "Mixer"), makeVolumeAndCompressionItems(), "menu-mixer"),
        makeMenu(TranslatableString("appshell/menu/extraedit", "Edit"), makeVolumeAndCompressionItems(), "menu-extra-edit"),
        makeMenu(TranslatableString("appshell/menu/playatspeed", "Play at speed"), makeVolumeAndCompressionItems(), "menu-play-at-speed"),
        makeMenu(TranslatableString("appshell/menu/device", "Device"), makeVolumeAndCompressionItems(), "menu-device"),
        makeMenu(TranslatableString("appshell/menu/extraselect", "Select"), makeVolumeAndCompressionItems(), "menu-extraselect"),
        makeSeparator(),
        makeMenuItem("prev-window"),
        makeMenuItem("next-window"),
        makeMenu(TranslatableString("appshell/menu/focus", "Focus"), makeVolumeAndCompressionItems(), "menu-focus"),
        makeMenu(TranslatableString("appshell/menu/cursor", "Cursor"), makeVolumeAndCompressionItems(), "menu-cursor"),
        makeMenu(TranslatableString("appshell/menu/track", "Track"), makeVolumeAndCompressionItems(), "menu-track"),
        makeMenu(TranslatableString("appshell/menu/scriptables1", "Scriptables1"), makeVolumeAndCompressionItems(), "menu-scriptables1"),
        makeMenu(TranslatableString("appshell/menu/scriptables2", "Scriptables2"), makeVolumeAndCompressionItems(), "menu-scriptables2"),
        makeMenu(TranslatableString("appshell/menu/images", "Images"), makeVolumeAndCompressionItems(), "menu-images"),

#ifndef Q_OS_MAC
        makeMenu(TranslatableString("appshell/menu/settings", "Settings"), makeVolumeAndCompressionItems(), "menu-settings"),
#endif
        makeMenuItem("karaoke"),
        makeMenuItem("benchmark"),
        makeMenuItem("regular-interval-labels"),
    };

    //! TODO AU4
    // put on top
    // if (updateConfiguration()->isAppUpdatable()) {
    //     helpItems.push_front(makeSeparator());
    //     helpItems.push_front(makeMenuItem("check-update"));
    // }

    return makeMenu(TranslatableString("appshell/menu/extra", "&Extra"), extraItems, "menu-extra");
}

MenuItem* AppMenuModel::makeHelpMenu()
{
    MenuItemList helpItems {
        makeMenuItem("tutorials"),
        makeMenuItem("online-handbook"),
        makeSeparator(),
        makeMenu(TranslatableString("appshell/menu/diagnostics", "Diagnostics"), makeDiagnosticsItems(), "menu-diagnostics"),
        makeSeparator(),
        makeMenuItem("link-account"),
        makeMenuItem("updates"),
        makeMenuItem("about-audacity"),
        makeMenuItem("about-qt", MenuItemRole::AboutQtRole),
    };

    //! TODO AU4
    // put on top
    // if (updateConfiguration()->isAppUpdatable()) {
    //     helpItems.push_front(makeSeparator());
    //     helpItems.push_front(makeMenuItem("check-update"));
    // }

    return makeMenu(TranslatableString("appshell/menu/help", "&Help"), helpItems, "menu-help");
}

MenuItemList AppMenuModel::makeRecentProjectsItems()
{
    MenuItemList items;
    //! TODO AU4
    const RecentFilesList& recentFiles = recentFilesController()->recentFilesList();

    int index = 0;
    for (const RecentFile& file : recentFiles) {
        MenuItem* item = new MenuItem(this);

        UiAction action;
        action.code = "file-open";
        action.title = TranslatableString::untranslatable(file.displayName(/*includingExtension*/ true));
        item->setAction(action);

        item->setId(makeId(item->action().code, index++));

        UiActionState state;
        state.enabled = true;
        item->setState(state);

        item->setSelectable(true);
        item->setArgs(ActionData::make_arg2<QUrl, QString>(file.path.toQUrl(), file.displayNameOverride));

        items << item;
    }

    return items;
}

MenuItemList AppMenuModel::appendClearRecentSection(const uicomponents::MenuItemList& recentScores)
{
    MenuItemList result = recentScores;
    result << makeSeparator()
           << makeMenuItem("clear-recent");

    return result;
}

MenuItemList AppMenuModel::makeExportItems()
{
    MenuItemList items {
        makeMenuItem("export-labels"),
        makeMenuItem("export-midi")
    };

    return items;
}

MenuItemList AppMenuModel::makeClipItems()
{
    MenuItemList items {
        makeMenuItem("rename-clip"),
        makeMenuItem("trim-clip"),
        makeSeparator(),
        makeMenuItem("split-clip"),
        makeMenuItem("split-into-new-track"),
        makeMenuItem("split-clips-at-silences"),
        makeMenuItem("merge-selected-clips"),
        makeSeparator(),
        makeMenuItem("group-clips"),
        makeMenuItem("ungroup-clips")
    };

    return items;
}

MenuItemList AppMenuModel::makeAudioActionsItems()
{
    MenuItemList items {
        makeMenuItem("cut-labels"),
        makeMenuItem("split-cut-labels"),
        makeMenuItem("copy-labels"),
        makeMenuItem("delete-labels"),
        makeMenuItem("split-delete-labels"),
        makeSeparator(),
        makeMenuItem("split-labels"),
        makeMenuItem("join-labels"),
        makeSeparator(),
        makeMenuItem("silence-labels"),
        makeMenuItem("disjoin-labels"),
    };

    return items;
}

MenuItemList AppMenuModel::makeLoopingItems()
{
    MenuItemList items {
        makeMenuItem("toggle-loop-region"),
        makeMenuItem("clear-loop-region"),
        makeMenuItem("set-loop-region-to-selection"),
        makeMenuItem("set-loop-region-in"),
        makeMenuItem("set-loop-region-out")
    };

    return items;
}

MenuItemList AppMenuModel::makeZoomItems()
{
    MenuItemList items {
        makeMenuItem("zoom-in"),
        makeMenuItem("zoom-out"),
        makeMenuItem("zoom-to-selection"),
        makeMenuItem("zoom-toggle"),
        makeMenuItem("zoom-reset"),
        makeSeparator(),
        makeMenuItem("fit-project-to-window"),
        makeMenuItem("fit-view-to-project"),
        makeMenuItem("collapse-all-tracks"),
        makeMenuItem("expand-all-tracks")
    };

    return items;
}

MenuItemList AppMenuModel::makeSkipToItems()
{
    MenuItemList items {
        makeMenuItem("skip-to-selection-start"),
        makeMenuItem("skip-to-selection-end")
    };

    return items;
}

MenuItemList AppMenuModel::makeAlignItems()
{
    MenuItemList items {
        makeMenuItem("align-end-to-end"),
        makeMenuItem("align-together"),
        makeSeparator(),
        makeMenuItem("align-start-to-zero"),
        makeMenuItem("align-start-to-playhead"),
        makeMenuItem("align-start-to-selection-end"),
        makeMenuItem("align-end-to-playhead"),
        makeMenuItem("align-end-to-selection-end")
    };

    return items;
}

MenuItemList AppMenuModel::makeSortItems()
{
    MenuItemList items {
        makeMenuItem("sort-by-time"),
        makeMenuItem("sort-by-name")
    };

    return items;
}

//! TODO AU4
MenuItemList AppMenuModel::makeVolumeAndCompressionItems()
{
    MenuItemList items {
        makeMenuItem("sort-by-time")
    };

    return items;
}

MenuItemList AppMenuModel::makeMacrosItems()
{
    MenuItemList items {
        makeMenuItem("apply-macros-palette"),
        makeSeparator(),
        makeMenuItem("macro-fade-ends"),
        makeMenuItem("macro-mp3-conversion")
    };

    return items;
}

MenuItemList AppMenuModel::makeDiagnosticsItems()
{
    MenuItemList items {
        makeMenuItem("device-info"),
        makeMenuItem("midi-device-info"),
        makeMenuItem("log"),
        makeMenuItem("crash-report"),
        makeMenuItem("raise-segfault"),
        makeMenuItem("throw-exception"),
        makeMenuItem("violate-assertion"),
        makeMenuItem("menu-tree"),
        makeMenuItem("frame-statistics")
    };

    return items;
}

MenuItemList AppMenuModel::makeFramesItems()
{
    MenuItemList items {
        makeMenuItem("insert-hbox"),
        makeMenuItem("insert-vbox"),
        makeMenuItem("insert-textframe"),
        makeSeparator(),
        makeMenuItem("append-hbox"),
        makeMenuItem("append-vbox"),
        makeMenuItem("append-textframe")
    };

    return items;
}

MenuItemList AppMenuModel::makeWorkspacesItems()
{
    MenuItemList items;

    //! TODO AU4
    // IWorkspacePtrList workspaces = workspacesManager()->workspaces();
    // IWorkspacePtr currentWorkspace = workspacesManager()->currentWorkspace();

    // std::sort(workspaces.begin(), workspaces.end(), [](const IWorkspacePtr& workspace1, const IWorkspacePtr& workspace2) {
    //     return workspace1->name() < workspace2->name();
    // });

    // int index = 0;
    // for (const IWorkspacePtr& workspace : workspaces) {
    //     MenuItem* item = new MenuItem(uiActionsRegister()->action("select-workspace"), this);
    //     item->setId(makeId(item->action().code, index++));

    //     UiAction action = item->action();
    //     action.title = TranslatableString::untranslatable(String::fromStdString(workspace->title()));

    //     item->setAction(action);
    //     item->setArgs(ActionData::make_arg1<std::string>(workspace->name()));
    //     item->setSelectable(true);
    //     item->setSelected(workspace == currentWorkspace);

    //     UiActionState state;
    //     state.enabled = true;
    //     state.checked = item->selected();
    //     item->setState(state);

    //     items << item;
    // }

    items << makeSeparator()
          << makeMenuItem("configure-workspaces");

    return items;
}

MenuItemList AppMenuModel::makeShowItems()
{
    MenuItemList items {
        makeMenuItem("show-invisible"),
        makeMenuItem("show-unprintable"),
        makeMenuItem("show-frames"),
        makeMenuItem("show-pageborders"),
        makeMenuItem("show-irregular"),
        makeMenuItem("show-soundflags"),
    };

    return items;
}

MenuItemList AppMenuModel::makePluginsItems()
{
    MenuItemList result;

    //! TODO AU4
    // KnownCategories categories = extensionsProvider()->knownCategories();
    // ManifestList enabledExtensions = extensionsProvider()->manifestList(Filter::Enabled);

    // auto addMenuItems = [this](MenuItemList& items, const Manifest& m) {
    //     if (m.actions.size() == 1) {
    //         const extensions::Action& a = m.actions.at(0);
    //         items << makeMenuItem(makeUriQuery(m.uri, a.code).toString(), TranslatableString::untranslatable(a.title));
    //     } else {
    //         MenuItemList sub;
    //         for (const extensions::Action& a : m.actions) {
    //             sub << makeMenuItem(makeUriQuery(m.uri, a.code).toString(), TranslatableString::untranslatable(a.title));
    //         }
    //         items << makeMenu(TranslatableString::untranslatable(m.title), sub);
    //     }
    // };

    // std::map<std::string, MenuItemList> categoriesMap;
    // MenuItemList pluginsWithoutCategories;
    // for (const Manifest& m : enabledExtensions) {
    //     std::string categoryStr = m.category.toStdString();
    //     if (muse::contains(categories, categoryStr)) {
    //         MenuItemList& items = categoriesMap[categoryStr];
    //         addMenuItems(items, m);
    //     } else {
    //         addMenuItems(pluginsWithoutCategories, m);
    //     }
    // }

    // for (const auto& it : categoriesMap) {
    //     TranslatableString categoryTitle = muse::value(categories, it.first, {});
    //     result << makeMenu(categoryTitle, it.second);
    // }

    // std::sort(result.begin(), result.end(), [](const MenuItem& l, const MenuItem& r) {
    //     return l.translatedTitle() < r.translatedTitle();
    // });

    // std::sort(pluginsWithoutCategories.begin(), pluginsWithoutCategories.end(), [](const MenuItem& l, const MenuItem& r) {
    //     return l.translatedTitle() < r.translatedTitle();
    // });

    // for (MenuItem* plugin : pluginsWithoutCategories) {
    //     result << plugin;
    // }

    return result;
}
