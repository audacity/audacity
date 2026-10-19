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

#include "framework/global/containers.h"
#include "framework/global/types/translatablestring.h"
#include "framework/diagnostics/diagnosticscommands.h"
#include "framework/dockwindow/dockcommands.h"
#include "framework/extensions/extensionscommands.h"
#include "framework/testflow/testflowcommands.h"
#include "framework/update/updatecommands.h"

#include "shared/types/workspacetitles.h"
#include "appshell/appshellcommands.h"
#include "effects/effects_base/effectscommands.h"
#include "playback/playbackcommands.h"
#include "project/projectcommands.h"
#include "projectscene/projectscenecommands.h"
#include "record/recordcommands.h"
#include "trackedit/trackeditcommands.h"

#include "muse_framework_config.h"

#ifdef MUSE_MODULE_WORKSPACE
#include "workspace/qml/Muse/Workspace/workspacesmenumodel.h"
#endif

#include "log.h"

using namespace au::appshell;
using namespace muse;
using namespace muse::ui;
using namespace muse::uicomponents;
using namespace muse::rcommand;

using namespace au::project;
//! TODO AU4
// using namespace muse::workspace;
using namespace muse::extensions;

static QString makeId(const Command& command, int itemIndex)
{
    return QString::fromStdString(command.toString()) + QString::number(itemIndex);
}

static const QString RENAME_CLIP_ITEM_ID("rename-clip-item");
static const QString RENAME_LABEL_ITEM_ID("rename-label-item");

AppMenuModel::AppMenuModel(QObject* parent)
    : AbstractMenuModel(parent)
{
#ifdef MUSE_MODULE_WORKSPACE
    m_workspacesMenuModel = std::make_shared<workspace::WorkspacesMenuModel>(this);
#endif
}

void AppMenuModel::load()
{
    TRACEFUNC;

    AbstractMenuModel::load();

#ifdef MUSE_MODULE_WORKSPACE
    m_workspacesMenuModel->load();
#endif

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
        makeHelpMenu()
    };

    if (globalConfiguration()->devModeEnabled()) {
        const auto diagnosticMenu = makeDiagnosticMenu();
        items << diagnosticMenu;
    }

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

        recentScoreListItem.setEnabled(openRecentEnabled);
        recentScoreListItem.setSubitems(recentScoresList);
    });

#ifdef MUSE_MODULE_WORKSPACE
    connect(m_workspacesMenuModel.get(), &workspace::WorkspacesMenuModel::itemsChanged, this, [this](){
        MenuItem& workspacesItem = findMenu("menu-workspaces");
        au::shared::translateWorkspaceTitles(m_workspacesMenuModel->items());
        workspacesItem.setSubitems(m_workspacesMenuModel->items());
    });
#endif

    effectsProvider()->effectMetaListChanged().onNotify(this, [this]() {
        onEffectsChanged();
    });

    effectsConfiguration()->effectMenuOrganizationChanged().onNotify(this, [this]() {
        onEffectsChanged();
    });

    effectExecutionScenario()->lastProcessorIdChanged().onReceive(this, [this](const effects::EffectId& effectId) {
        m_lastProcessorId = effectId;
        updateRepeatLastEffectItem();
    });

    projectHistory()->historyChanged().onReceive(this, [this](auto) {
        updateUndoRedoItems();
    });

    extensionsProvider()->manifestListChanged().onNotify(this, [this]() {
        findMenu("menu-tools").setSubitems(makeToolItems());
    });

    extensionsProvider()->enabledChanged().onReceive(this, [this](const ExtensionUri&) {
        findMenu("menu-tools").setSubitems(makeToolItems());
    });

    updateRenameItemsState();
}

void AppMenuModel::onCommandStateChanged(const Command& command, const CommandState& state)
{
    AbstractMenuModel::onCommandStateChanged(command, state);

    //! NOTE "rename-item" drives both the "Rename clip" and "Rename label" items; split its
    //! state between them depending on whether a clip or a label is selected
    if (command == trackedit::TRACKEDIT_RENAME_ITEM_COMMAND) {
        updateRenameItemsState();
    }
}

void AppMenuModel::updateUndoRedoItems()
{
    MenuItem& undoItem = findItem(trackedit::TRACKEDIT_UNDO_COMMAND);
    const TranslatableString undoActionName = projectHistory()->topMostUndoActionName();
    undoItem.setTitle(undoActionName.isEmpty()
                      ? TranslatableString("action", "Undo")
                      : TranslatableString("action", "Undo ‘%1’").arg(undoActionName));

    MenuItem& redoItem = findItem(trackedit::TRACKEDIT_REDO_COMMAND);
    const TranslatableString redoActionName = projectHistory()->topMostRedoActionName();
    redoItem.setTitle(redoActionName.isEmpty()
                      ? TranslatableString("action", "Redo")
                      : TranslatableString("action", "Redo ‘%1’").arg(redoActionName));
}

void AppMenuModel::updateRenameItemsState()
{
    const CommandState state = commandsState()->commandState(trackedit::TRACKEDIT_RENAME_ITEM_COMMAND);

    MenuItem& renameClipItem = findItem(RENAME_CLIP_ITEM_ID);
    if (renameClipItem.isValid()) {
        renameClipItem.setEnabled(state.enabled && selectionController()->selectedClips().size() == 1);
    }

    MenuItem& renameLabelItem = findItem(RENAME_LABEL_ITEM_ID);
    if (renameLabelItem.isValid()) {
        renameLabelItem.setEnabled(state.enabled && selectionController()->selectedLabels().size() == 1);
    }
}

void AppMenuModel::updateRepeatLastEffectItem()
{
    MenuItem& item = findItem(effects::EFFECTS_REPEAT_LAST_EFFECT_COMMAND);
    if (!item.isValid() || !m_lastProcessorId) {
        return;
    }

    const CommandInfo& effectInfo = commandsRegister()->commandInfo(effects::makeEffectOpenCommand(*m_lastProcessorId));
    item.setTitle(TranslatableString("action", "Repeat %1").arg(String::fromQString(effectInfo.title.qTranslatedWithoutMnemonic())));
}

MenuItem* AppMenuModel::makeMenuItem(const Command& command, MenuItemRole menuRole)
{
    MenuItem* item = makeMenuItem(command);
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
        makeMenuItem(PROJECT_NEW_COMMAND),
        makeMenuItem(PROJECT_OPEN_COMMAND),
        makeMenu(TranslatableString("appshell-menu-file", "Open &recent"), recentScoresList, "menu-file-open", openRecentEnabled),

        makeSeparator(),

        makeMenuItem(PROJECT_IMPORT_COMMAND),

        makeSeparator(),

        makeMenuItem(PROJECT_SAVE_COMMAND),
        makeMenuItem(PROJECT_SAVE_TO_CLOUD_COMMAND),
        makeMenuItem(PROJECT_SAVE_AS_COMMAND),
        makeMenuItem(PROJECT_UPDATE_CLOUD_AUDIO_PREVIEW_COMMAND),

        makeSeparator(),

        makeMenuItem(PROJECT_EXPORT_AUDIO_COMMAND),
        makeMenuItem(PROJECT_EXPORT_LABELS_COMMAND),
        makeMenuItem(PROJECT_SHARE_AUDIO_COMMAND),

        makeSeparator(),

        makeMenuItem(PROJECT_CLOSE_COMMAND),
        makeMenuItem(APP_QUIT_COMMAND, MenuItemRole::QuitRole)
    };

    return makeMenu(TranslatableString("appshell-menu-file", "&File"), fileItems, "menu-file");
}

MenuItem* AppMenuModel::makeEditMenu()
{
    MenuItemList editItems {
        makeMenuItem(trackedit::TRACKEDIT_UNDO_COMMAND),
        makeMenuItem(trackedit::TRACKEDIT_REDO_COMMAND),
        makeSeparator(),
        makeMenuItem(APP_CUT_COMMAND),
        makeMenuItem(APP_COPY_COMMAND),
        makeMenuItem(APP_PASTE_COMMAND),
        makeMenuItem(APP_DELETE_COMMAND),
        makeSeparator(),
        makeMenuItem(trackedit::TRACKEDIT_DUPLICATE_COMMAND),
        // TODO: we should only show those trackedit actions when in the trackedit context,
        // this need some framework update to support contextual shortcuts and dynamic actions based on context
        // makeMenuItem(trackedit::TRACKEDIT_PASTE_OVERLAP_COMMAND),
        // makeMenuItem(trackedit::TRACKEDIT_PASTE_INSERT_COMMAND),
        // makeMenuItem(trackedit::TRACKEDIT_PASTE_INSERT_ALL_TRACKS_RIPPLE_COMMAND),
        makeMenuItem(trackedit::TRACKEDIT_DELETE_PER_TRACK_RIPPLE_COMMAND),
        makeSeparator(),
        makeMenu(TranslatableString("appshell-menu-clip", "Clip"), makeClipItems(), "menu-clip"),
        makeMenu(TranslatableString("appshell-menu-label", "Label"), makeLabelItems(), "menu-label"),
        makeMenuItem(trackedit::TRACKEDIT_SILENCE_AUDIO_SELECTION_COMMAND, TranslatableString("action", "Silence audio")),
        makeSeparator(),
        makeMenuItem(PROJECT_OPEN_METADATA_DIALOG_COMMAND, TranslatableString("action", "Metadata editor")),
#ifndef Q_OS_MAC
        makeSeparator(),
#endif
        makeMenuItem(APP_PREFERENCES_COMMAND, MenuItemRole::PreferencesRole)
    };

    return makeMenu(TranslatableString("appshell-menu-edit", "&Edit"), editItems, "menu-edit");
}

MenuItem* AppMenuModel::makeSelectMenu()
{
    //! NOTE: audio sub-menu and spectral tools are not implemented yet

    MenuItemList selectItems {
        makeMenuItem(trackedit::TRACKEDIT_SELECT_ALL_COMMAND),
        makeMenuItem(trackedit::TRACKEDIT_CLEAR_SELECTION_COMMAND),
        makeMenuItem(trackedit::TRACKEDIT_SELECT_ALL_TRACKS_COMMAND),
        makeSeparator(),
        makeMenu(TranslatableString("appshell-menu-select", "Region"), makeRegionSelectionItems(), "menu-selection-region"),
        makeSeparator(),
        makeMenu(TranslatableString("appshell-menu-select", "Looping"), makeLoopingItems(), "menu-looping"),
        makeMenuItem(trackedit::TRACKEDIT_ZERO_CROSS_COMMAND),
    };

    return makeMenu(TranslatableString("appshell-menu-select", "&Select"), selectItems, "menu-select");
}

MenuItem* AppMenuModel::makeViewMenu()
{
    MenuItem* effectsItem = makeMenuItem(projectscene::PROJECTSCENE_TOGGLE_EFFECTS_PANEL_COMMAND);

    MenuItemList viewItems {
        makeMenu(TranslatableString("appshell-menu-zoom", "Zoom"), makeZoomItems(), "menu-zoom"),
    };

    if (effectsItem) {
        viewItems << makeSeparator();
        viewItems << effectsItem;
    }

    viewItems << makeSeparator()
              << makeMenuItem(projectscene::PROJECTSCENE_OPEN_LABEL_EDITOR_COMMAND)
              << makeMenuItem(PROJECT_OPEN_METADATA_DIALOG_COMMAND)
              << makeMenuItem(APP_TOGGLE_HISTORY_PANEL_COMMAND)
              << makeSeparator()
#ifdef MUSE_MODULE_WORKSPACE
        << makeMenu(TranslatableString("appshell-menu-view", "W&orkspaces"),
                    au::shared::translateWorkspaceTitles(m_workspacesMenuModel->items()), "menu-workspaces")
        << makeSeparator()
#endif
#ifndef Q_OS_MAC
        << makeMenuItem(APP_TOGGLE_FULLSCREEN_COMMAND)
        << makeSeparator()
#endif
        << makeMenuItem(projectscene::PROJECTSCENE_TOGGLE_CLIPPING_IN_WAVEFORM_COMMAND)
        << makeMenuItem(projectscene::PROJECTSCENE_TOGGLE_RMS_IN_WAVEFORM_COMMAND)
        << makeMenuItem(projectscene::PROJECTSCENE_TOGGLE_VERTICAL_RULERS_COMMAND)
        << makeSeparator()
        << makeMenuItem(dock::DOCK_RESTORE_DEFAULT_LAYOUT_COMMAND);

    return makeMenu(TranslatableString("appshell-menu-view", "&View"), viewItems, "menu-view");
}

MenuItem* AppMenuModel::makeRecordMenu()
{
    MenuItemList recordItems {
        makeMenuItem(record::RECORD_ON_CURRENT_TRACK_COMMAND),
        makeMenuItem(record::RECORD_ON_NEW_TRACK_COMMAND),
        //! makeMenuItem("set-up-timed-recording"),
        makeMenuItem(record::RECORD_LEAD_IN_RECORDING_COMMAND),
        //! makeMenuItem("toggle-sound-activated-recording"),
        //! makeMenuItem("set-sound-activation-level"),
    };

    return makeMenu(TranslatableString("appshell-menu-record", "&Record"), recordItems, "menu-record");
}

MenuItem* AppMenuModel::makeTracksMenu()
{
    MenuItemList tracksItems {
        makeMenuItem(trackedit::TRACKEDIT_NEW_MONO_TRACK_COMMAND),
        makeMenuItem(trackedit::TRACKEDIT_NEW_STEREO_TRACK_COMMAND),
        makeMenuItem(trackedit::TRACKEDIT_NEW_LABEL_TRACK_COMMAND),
        makeSeparator(),
        makeMenuItem(trackedit::TRACKEDIT_TRACK_DUPLICATE_COMMAND),
        // makeMenuItem("remove-tracks"),
        // makeSeparator(),
        // makeMenuItem("mixdown-to"),
    };

    return makeMenu(TranslatableString("appshell-menu-tracks", "&Tracks"), tracksItems, "menu-tracks");
}

MenuItem* AppMenuModel::makeGenerateMenu()
{
    return makeMenu(TranslatableString("appshell-menu-generate", "&Generate"), makeGeneratorItems(), "menu-generate");
}

MenuItem* AppMenuModel::makeEffectMenu()
{
    return makeMenu(TranslatableString("appshell-menu-effect", "&Effect"), makeEffectsItems(), "menu-effect");
}

MenuItem* AppMenuModel::makeAnalyzeMenu()
{
    return makeMenu(TranslatableString("appshell-menu-analyze", "&Analyze"), makeAnalyzeItems(), "menu-analyze");
}

MenuItem* AppMenuModel::makeToolsMenu()
{
    return makeMenu(TranslatableString("appshell-menu-tools", "&Tools"), makeToolItems(), "menu-tools");
}

MenuItemList AppMenuModel::makeExtensionItems()
{
    MenuItemList items;
    for (const Manifest& manifest : extensionsProvider()->manifestList(Filter::Enabled)) {
        if (manifest.actions.size() == 1) {
            const muse::extensions::Action& action = manifest.actions.front();
            if (action.showOnAppmenu) {
                items << makeMenuItem(makeCommand(manifest.uri, action.code),
                                      TranslatableString::untranslatable(action.title.empty() ? manifest.title : action.title));
            }
            continue;
        }

        MenuItemList actions;
        for (const muse::extensions::Action& action : manifest.actions) {
            if (action.showOnAppmenu) {
                actions << makeMenuItem(makeCommand(manifest.uri, action.code),
                                        TranslatableString::untranslatable(action.title));
            }
        }
        if (!actions.empty()) {
            items << makeMenu(TranslatableString::untranslatable(manifest.title), actions);
        }
    }

    std::sort(items.begin(), items.end(), [](const MenuItem& left, const MenuItem& right) {
        return left.translatedTitle() < right.translatedTitle();
    });
    return items;
}

MenuItem* AppMenuModel::makeHelpMenu()
{
    MenuItemList helpItems {
        // makeMenuItem("tutorials"),
        makeMenuItem(APP_ONLINE_HANDBOOK_COMMAND),
        makeSeparator(),
        // makeMenuItem("link-account"),
        makeMenuItem(APP_ABOUT_COMMAND, MenuItemRole::AboutRole),
        makeMenuItem(APP_ABOUT_QT_COMMAND, MenuItemRole::AboutQtRole),
        makeSeparator(),
        makeMenuItem(APP_REVERT_FACTORY_COMMAND)
    };

    if (updateConfiguration()->isAppUpdatable()) {
        helpItems.push_front(makeSeparator());
        helpItems.push_front(makeMenuItem(update::UPDATE_CHECK_COMMAND));
    }

    return makeMenu(TranslatableString("appshell-menu-help", "&Help"), helpItems, "menu-help");
}

muse::uicomponents::MenuItem* AppMenuModel::makeDiagnosticMenu()
{
    MenuItemList systemItems {
        makeMenuItem(diagnostics::DIAGNOSTICS_SHOW_PATHS_COMMAND),
        makeMenuItem(diagnostics::DIAGNOSTICS_SHOW_GRAPHICSINFO_COMMAND),
        makeMenuItem(diagnostics::DIAGNOSTICS_SHOW_PROFILER_COMMAND),
    };

    MenuItemList items {
        makeMenuItem(diagnostics::DIAGNOSTICS_SAVE_FILES_COMMAND),
        makeMenu(TranslatableString("appshell-menu-diagnostics", "&System"), systemItems, "menu-system")
    };

    if (globalConfiguration()->devModeEnabled()) {
        MenuItemList actionsItems {
            makeMenuItem(diagnostics::DIAGNOSTICS_SHOW_ACTIONS_COMMAND)
        };

        MenuItemList accessibilityItems {
            makeMenuItem(diagnostics::DIAGNOSTICS_SHOW_NAVIGATION_TREE_COMMAND),
            makeMenuItem(diagnostics::DIAGNOSTICS_SHOW_ACCESSIBLE_TREE_COMMAND),
            makeMenuItem(diagnostics::DIAGNOSTICS_DUMP_ACCESSIBLE_TREE_COMMAND),
        };

        MenuItemList testflowItems {
            makeMenuItem(testflow::TESTFLOW_OPEN_SCRIPTS_COMMAND),
        };

        MenuItemList extensionsItems {
            makeMenuItem(extensions::OPEN_APIDUMP_COMMAND),
        };

        items << makeMenu(TranslatableString("appshell-menu-diagnostics", "A&ctions"), actionsItems, "menu-actions")
              << makeMenu(TranslatableString("appshell-menu-diagnostics", "&Accessibility"), accessibilityItems, "menu-accessibility")
              << makeMenu(TranslatableString("appshell-menu-diagnostics", "E&xtensions"), extensionsItems, "menu-extensions")
              << makeMenu(TranslatableString("appshell-menu-diagnostics", "Test&flow"), testflowItems, "menu-testflow");
        //! TODO AU4
        // << makeMenuItem("multiwindows-dev-show-info");
    }

    return makeMenu(TranslatableString("appshell-menu-diagnostics", "&Diagnostics"), items, "menu-diagnostic");
}

MenuItemList AppMenuModel::makeRecentProjectsItems()
{
    MenuItemList items;
    const RecentFilesList& recentFiles = recentFilesController()->recentFilesList();
    const CommandInfo& openInfo = commandsRegister()->commandInfo(PROJECT_OPEN_COMMAND);

    int index = 0;
    for (const RecentFile& file : recentFiles) {
        MenuItem* item = new MenuItem(openInfo, this);
        item->setCommandQuery(make_query(PROJECT_OPEN_COMMAND, {
            { PROJECT_URL_PARAM, Val(file.path.toQUrl().toString()) },
            { PROJECT_DISPLAY_NAME_PARAM, Val(file.displayNameOverride) },
        }));
        item->setId(makeId(PROJECT_OPEN_COMMAND, index++));
        item->setTitle(TranslatableString::untranslatable(file.displayName(/*includingExtension*/ true)));
        item->setEnabled(true);
        item->setSelectable(true);

        items << item;
    }

    return items;
}

MenuItemList AppMenuModel::appendClearRecentSection(const muse::uicomponents::MenuItemList& recentScores)
{
    MenuItemList result = recentScores;
    result << makeSeparator()
           << makeMenuItem(PROJECT_CLEAR_RECENT_COMMAND);

    return result;
}

MenuItemList AppMenuModel::makeClipItems()
{
    MenuItem* renameClipItem = makeMenuItem(trackedit::TRACKEDIT_RENAME_ITEM_COMMAND,
                                            TranslatableString("appshell-menu-clip", "Rename clip"));
    renameClipItem->setId(RENAME_CLIP_ITEM_ID);

    MenuItemList items {
        renameClipItem,
        // makeMenuItem("trim-clip"),
        makeSeparator(),
        makeMenuItem(trackedit::TRACKEDIT_SPLIT_COMMAND),
        makeMenuItem(trackedit::TRACKEDIT_SPLIT_INTO_NEW_TRACK_COMMAND),
        makeMenuItem(trackedit::TRACKEDIT_DISJOIN_COMMAND),
        makeMenuItem(trackedit::TRACKEDIT_JOIN_COMMAND),
        makeSeparator(),
        makeMenuItem(trackedit::TRACKEDIT_GROUP_ITEMS_COMMAND),
        makeMenuItem(trackedit::TRACKEDIT_UNGROUP_ITEMS_COMMAND)
    };

    return items;
}

MenuItemList AppMenuModel::makeLabelItems()
{
    MenuItem* renameLabelItem = makeMenuItem(trackedit::TRACKEDIT_RENAME_ITEM_COMMAND,
                                             TranslatableString("appshell-menu-label", "Rename label"));
    renameLabelItem->setId(RENAME_LABEL_ITEM_ID);

    MenuItemList items {
        makeMenuItem(trackedit::TRACKEDIT_LABEL_ADD_COMMAND),
        renameLabelItem,
        // makeMenuItem("paste-new-label"),
        makeSeparator(),
        makeMenuItem(projectscene::PROJECTSCENE_OPEN_LABEL_EDITOR_COMMAND, TranslatableString("action", "Manage labels")),
    };

    return items;
}

MenuItemList AppMenuModel::makeRegionSelectionItems()
{
    MenuItemList items {
        makeMenuItem(trackedit::TRACKEDIT_SELECT_LEFT_OF_PLAYBACK_POSITION_COMMAND),
        makeMenuItem(trackedit::TRACKEDIT_SELECT_RIGHT_OF_PLAYBACK_POSITION_COMMAND),
        makeMenuItem(trackedit::TRACKEDIT_SELECT_TRACK_START_TO_CURSOR_COMMAND),
        makeMenuItem(trackedit::TRACKEDIT_SELECT_CURSOR_TO_TRACK_END_COMMAND),
        makeMenuItem(trackedit::TRACKEDIT_SELECT_TRACK_START_TO_END_COMMAND),
    };

    return items;
}

MenuItemList AppMenuModel::makeLoopingItems()
{
    MenuItemList items {
        makeMenuItem(playback::PLAYBACK_TOGGLE_LOOP_REGION_COMMAND),
        makeMenuItem(playback::PLAYBACK_CLEAR_LOOP_REGION_COMMAND),
        makeSeparator(),
        makeMenuItem(playback::PLAYBACK_SET_LOOP_REGION_TO_SELECTION_COMMAND),
        makeSeparator(),
        makeMenuItem(playback::PLAYBACK_SET_LOOP_REGION_IN_OUT_COMMAND),
    };

    return items;
}

MenuItemList AppMenuModel::makeZoomItems()
{
    MenuItemList items {
        makeMenuItem(projectscene::PROJECTSCENE_ZOOM_IN_COMMAND),
        makeMenuItem(projectscene::PROJECTSCENE_ZOOM_OUT_COMMAND),
        makeMenuItem(projectscene::PROJECTSCENE_ZOOM_DEFAULT_COMMAND),
        makeMenuItem(projectscene::PROJECTSCENE_ZOOM_TO_SELECTION_COMMAND),
        makeMenuItem(projectscene::PROJECTSCENE_ZOOM_TOGGLE_COMMAND),
        makeSeparator(),
        makeMenuItem(projectscene::PROJECTSCENE_ZOOM_TO_FIT_PROJECT_COMMAND),
        // makeMenuItem("collapse-all-tracks"),
        // makeMenuItem("expand-all-tracks")
    };

    return items;
}

muse::uicomponents::MenuItem* AppMenuModel::makeMenuEffectItem(const effects::EffectId& effectId)
{
    return makeMenuItem(effects::makeEffectOpenCommand(effectId));
}

muse::uicomponents::MenuItem* AppMenuModel::makeMenuEffect(const muse::String& title, const muse::uicomponents::MenuItemList& items)
{
    return makeMenu(TranslatableString::untranslatable(title), items);
}

MenuItemList AppMenuModel::makeEffectsItems()
{
    MenuItemList items {
        makeMenuItem(effects::EFFECTS_PLUGIN_MANAGER_COMMAND),
        makeMenuItem(projectscene::PROJECTSCENE_ADD_REALTIME_EFFECTS_COMMAND),
        makeSeparator(),
        makeMenuItem(effects::EFFECTS_REPEAT_LAST_EFFECT_COMMAND),
    };

    const muse::uicomponents::MenuItemList effectMenus = effectsMenuProvider()->destructiveEffectMenu(*this,
                                                                                                      effects::EffectFilter::ProcessorsOnly);
    if (!effectMenus.empty()) {
        items << makeSeparator() << effectMenus;
    }

    return items;
}

MenuItemList AppMenuModel::makeGeneratorItems()
{
    MenuItemList items {
        makeMenuItem(effects::EFFECTS_PLUGIN_MANAGER_COMMAND),
        makeSeparator(),
    };

    const muse::uicomponents::MenuItemList effectMenus = effectsMenuProvider()->destructiveEffectMenu(*this,
                                                                                                      effects::EffectFilter::GeneratorsOnly);

    items << effectMenus;

    return items;
}

MenuItemList AppMenuModel::makeToolItems()
{
    MenuItemList items {
        makeMenuItem(effects::EFFECTS_PLUGIN_MANAGER_COMMAND),
        makeSeparator(),
        // makeMenuItem("manage-macros"),
    };

    const muse::uicomponents::MenuItemList toolMenus = effectsMenuProvider()->destructiveEffectMenu(*this,
                                                                                                    effects::EffectFilter::ToolsOnly);

    items << toolMenus;

    if (!toolMenus.empty()) {
        items << makeSeparator();
    }

    const muse::uicomponents::MenuItemList extensionItems = makeExtensionItems();
    items << extensionItems;

    // if (!extensionItems.empty()) {
    //     items << makeSeparator();
    // }

    // items << makeMenuItem("raw-data-import")
    //       << makeSeparator();
    // items << makeMenuItem("reset-configuration");

    return items;
}

MenuItemList AppMenuModel::makeAnalyzeItems()
{
    MenuItemList items {
        makeMenuItem(effects::EFFECTS_PLUGIN_MANAGER_COMMAND),
        makeSeparator(),
        // makeMenuItem("contrast-analyzer"),
        // makeMenuItem("plot-spectrum"),
        // makeSeparator(),
    };

    const muse::uicomponents::MenuItemList analyzeMenus = effectsMenuProvider()->destructiveEffectMenu(*this,
                                                                                                       effects::EffectFilter::AnalyzersOnly);

    items << analyzeMenus;

    if (!analyzeMenus.empty()) {
        items << makeSeparator();
    }

    return items;
}

void AppMenuModel::onEffectsChanged()
{
    MenuItem& effectsItem = findMenu("menu-effect");
    effectsItem.setSubitems(makeEffectsItems());
    MenuItem& generateItem = findMenu("menu-generate");
    generateItem.setSubitems(makeGeneratorItems());
    MenuItem& analyzeItem = findMenu("menu-analyze");
    analyzeItem.setSubitems(makeAnalyzeItems());
    MenuItem& toolsItem = findMenu("menu-tools");
    toolsItem.setSubitems(makeToolItems());

    updateRepeatLastEffectItem();
}
