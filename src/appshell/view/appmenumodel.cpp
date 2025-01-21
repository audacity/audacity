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

#include "muse_framework_config.h"
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
        makeHelpMenu(),
        makeDiagnosticMenu()
    };

    setItems(items);

    setupConnections();

    //! NOTE: removes some undesired platform-specific items
    //! (such as "Start Dictation" and "Special Characters" on macOS)
    appMenuModelHook()->onAppMenuInited();

    muse::ValCh<bool> isEffectsPanelVisible = configuration()->isEffectsPanelVisible();
    isEffectsPanelVisible.ch.onReceive(this, [this](bool visible)
    {
        setItemIsChecked("toggle-effects", visible);
    });
    setItemIsChecked("toggle-effects", isEffectsPanelVisible.val);
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

    uiActionsRegister()->actionsChanged().onReceive(this, [this](const ui::UiActionList& acts) {
        for (const UiAction& act : acts) {
            MenuItem& item = findItem(act.code);
            if (item.isValid()) {
                item.setAction(act);
            }
        }
    });

    effectsProvider()->effectMetaListChanged().onNotify(this, [this]() {
        MenuItem& effectsItem = findMenu("menu-effect");
        effectsItem.setSubitems(makeEffectsItems());
    });
}

void AppMenuModel::setItemIsChecked(const QString& itemId, bool checked)
{
    MenuItem& item = findMenu(itemId);
    auto state = item.state();
    state.checked = checked;
    item.setState(state);
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
        makeMenuItem("group-clips"),
        makeMenuItem("ungroup-clips"),
        makeSeparator(),
        makeMenuItem("split-cut"),
        makeMenuItem("duplicate"),
        makeMenuItem("insert"),
        makeMenuItem("split-delete"),
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
        makeMenuItem("generate-omitted"),
    };

    return makeMenu(TranslatableString("appshell/menu/generate", "&Generate"), generateItems, "menu-generate");
}

MenuItem* AppMenuModel::makeEffectMenu()
{
    return makeMenu(TranslatableString("appshell/menu/effect", "&Effect"), makeEffectsItems(), "menu-effect");
}

MenuItem* AppMenuModel::makeAnalyzeMenu()
{
    MenuItemList analyzeItems {
        makeMenuItem("analyze-plugin-manager"),
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
        makeSeparator(),
        makeMenuItem("revert-factory")
    };

    //! TODO AU4
    // put on top
    // if (updateConfiguration()->isAppUpdatable()) {
    //     helpItems.push_front(makeSeparator());
    //     helpItems.push_front(makeMenuItem("check-update"));
    // }

    return makeMenu(TranslatableString("appshell/menu/help", "&Help"), helpItems, "menu-help");
}

muse::uicomponents::MenuItem* AppMenuModel::makeDiagnosticMenu()
{
    MenuItemList systemItems {
        makeMenuItem("diagnostic-show-paths"),
        makeMenuItem("diagnostic-show-graphicsinfo"),
        makeMenuItem("diagnostic-show-profiler"),
    };

    MenuItemList items {
        makeMenuItem("diagnostic-save-diagnostic-files"),
        makeMenu(TranslatableString("appshell/menu/diagnostics", "&System"), systemItems, "menu-system")
    };

    if (globalConfiguration()->devModeEnabled()) {
        MenuItemList actionsItems {
            makeMenuItem("diagnostic-show-actions")
        };

        MenuItemList accessibilityItems {
            makeMenuItem("diagnostic-show-navigation-tree"),
            makeMenuItem("diagnostic-show-accessible-tree"),
            makeMenuItem("diagnostic-accessible-tree-dump"),
        };

        //! TODO AU4
        // MenuItemList autobotItems {
        //     makeMenuItem("autobot-show-scripts"),
        // };

        items << makeMenu(TranslatableString("appshell/menu/diagnostics", "A&ctions"), actionsItems, "menu-actions")
              << makeMenu(TranslatableString("appshell/menu/diagnostics", "&Accessibility"), accessibilityItems, "menu-accessibility");
        //! TODO AU4
        // << makeMenu(TranslatableString("appshell/menu/diagnostics", "Auto&bot"), autobotItems, "menu-autobot")
        // << makeMenuItem("multiinstances-dev-show-info");
    }

    return makeMenu(TranslatableString("appshell/menu/diagnostics", "&Diagnostics"), items, "menu-diagnostic");
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
        makeMenuItem("split"),
        makeMenuItem("split-into-new-track"),
        makeMenuItem("split-clips-at-silences"),
        makeMenuItem("join"),
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
    MenuItemList systemItems {
        makeMenuItem("diagnostic-show-paths"),
        makeMenuItem("diagnostic-show-profiler"),
    };

    MenuItemList items {
        makeMenuItem("diagnostic-save-diagnostic-files"),
        makeMenu(TranslatableString("appshell/menu/diagnostic", "&System"), systemItems, "menu-system")
    };

    if (globalConfiguration()->devModeEnabled()) {
#ifdef MUSE_MODULE_ACCESSIBILITY
        MenuItemList accessibilityItems {
            makeMenuItem("diagnostic-show-navigation-tree"),
            makeMenuItem("diagnostic-show-accessible-tree"),
            makeMenuItem("diagnostic-accessible-tree-dump"),
        };
        items << makeMenu(TranslatableString("appshell/menu/diagnostic", "&Accessibility"), accessibilityItems, "menu-accessibility");
#endif

#ifdef MUSE_MODULE_AUTOBOT
        MenuItemList autobotItems {
            makeMenuItem("autobot-show-scripts"),
        };
        items << makeMenu(TranslatableString("appshell/menu/diagnostic", "Auto&bot"), autobotItems, "menu-autobot");
#endif

#ifdef MUSE_MODULE_MULTIINSTANCES
        items << makeMenuItem("multiinstances-dev-show-info");
#endif
    }

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

MenuItemList AppMenuModel::makeEffectsItems()
{
    MenuItemList items {
        makeMenuItem("effect-plugin-manager"),
        makeMenuItem("add-realtime-effects"),
        makeSeparator(),
        makeMenuItem("repeat-last-effect"),
        makeSeparator(),
        makeMenuItem("favourite-effect-1"),
        makeMenuItem("favourite-effect-2"),
        makeMenuItem("favourite-effect-3"),
        makeMenuItem("insert-silence"),
        makeSeparator(),
    };

    std::map<muse::String, MenuItemList> effectsToCategoryMap;

    effects::EffectMetaList metaList = effectsProvider()->effectMetaList();

    for (const effects::EffectMeta& meta : metaList) {
        MenuItem* item = makeMenuItem(effects::makeEffectOpenAction(meta.id).toString());
        effectsToCategoryMap[meta.categoryId].push_back(item);
    }

    for (const effects::EffectCategory& category : effectsProvider()->effectsCategoryList()) {
        items << makeMenu(TranslatableString::untranslatable(category.title), effectsToCategoryMap[category.id]);
    }

    return items;
}
