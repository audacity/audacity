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

#include "notationstatusbarmodel.h"

#include "types/translatablestring.h"

#include "log.h"

using namespace au::appshell;
using namespace mu::actions;
using namespace mu::ui;
using namespace mu::uicomponents;

static const QString TITLE_KEY("title");
static const QString ICON_KEY("icon");
static const QString SELECTABLE_KEY("selectable");
static const QString SELECTED_KEY("selected");
static const QString TYPE_KEY("type");
static const QString ID_KEY("id");
static const QString VALUE_KEY("value");

static const ActionCode TOGGLE_CONCERT_PITCH_CODE("concert-pitch");
static const ActionCode SELECT_WORKSPACE_CODE("configure-workspaces");

static constexpr int MIN_DISPLAYED_ZOOM_PERCENTAGE = 25;

static const QMap<ViewMode, ActionCode> ALL_MODE_MAP {
    { ViewMode::PAGE, "view-mode-page" },
    { ViewMode::LINE, "view-mode-continuous" },
    { ViewMode::SYSTEM, "view-mode-single" },
    { ViewMode::FLOAT, "view-mode-float" },
};

static ActionCode zoomTypeToActionCode(ZoomType type)
{
    switch (type) {
    case ZoomType::Percentage: return "zoom-x-percent";
    case ZoomType::PageWidth: return "zoom-page-width";
    case ZoomType::WholePage: return "zoom-whole-page";
    case ZoomType::TwoPages: return "zoom-two-pages";
    }

    return "";
}

NotationStatusBarModel::NotationStatusBarModel(QObject* parent)
    : QObject(parent)
{
}

QString NotationStatusBarModel::accessibilityInfo() const
{
    return accessibility() ? QString::fromStdString(accessibility()->accessibilityInfo().val) : QString();
}

QVariant NotationStatusBarModel::concertPitchItem()
{
    MenuItem* item = makeMenuItem(TOGGLE_CONCERT_PITCH_CODE);
    UiActionState state;
    state.enabled = notation() ? true : false;
    state.checked = notation() ? notation()->style()->styleValue(StyleId::concertPitch).toBool() : false;
    item->setState(state);
    return QVariant::fromValue(item);
}

QVariant NotationStatusBarModel::currentWorkspaceItem()
{
    MenuItem* item = makeMenuItem(SELECT_WORKSPACE_CODE);
    item->setId(QString::fromStdString(item->action().code));

    UiAction action;
    action.title
        = TranslatableString::untranslatable("%1 %2").arg(TranslatableString("workspace", "Workspace:"),
                                                          String::fromStdString(workspaceConfiguration()->currentWorkspaceName()));
    item->setAction(action);

    return QVariant::fromValue(item);
}

MenuItem* NotationStatusBarModel::makeMenuItem(const actions::ActionCode& actionCode)
{
    MenuItem* item = new MenuItem(actionsRegister()->action(actionCode), this);
    item->setId(QString::fromStdString(item->action().code));
    item->setState(actionsRegister()->actionState(actionCode));

    return item;
}

QVariant NotationStatusBarModel::currentViewMode()
{
    ViewMode viewMode = notation() ? notation()->viewMode() : ViewMode::PAGE;

    for (MenuItem* modeItem : makeAvailableViewModeList()) {
        if (ALL_MODE_MAP.key(modeItem->id().toStdString()) == viewMode) {
            if (viewMode == ViewMode::LINE || viewMode == ViewMode::SYSTEM) {
                // In continuous view, we don't want to see "horizontal" or "vertical" (those should only be visible in the menu)
                modeItem->setTitle(TranslatableString("notation", "Continuous view"));
            }

            return QVariant::fromValue(modeItem);
        }
    }

    return QVariant();
}

MenuItemList NotationStatusBarModel::makeAvailableViewModeList()
{
    if (!notation()) {
        return {};
    }

    MenuItemList result;

    ViewMode currentViewMode = notation()->viewMode();

    for (const ViewMode& viewMode: ALL_MODE_MAP.keys()) {
        ActionCode code = ALL_MODE_MAP[viewMode];
        if (viewMode == ViewMode::FLOAT && !globalConfiguration()->devModeEnabled()) {
            continue;
        }
        UiAction action = actionsRegister()->action(code);

        MenuItem* viewModeItem = new MenuItem(action, this);

        UiActionState state;
        state.enabled = true;
        viewModeItem->setState(state);

        viewModeItem->setId(QString::fromStdString(code));
        viewModeItem->setSelectable(true);
        viewModeItem->setSelected(currentViewMode == viewMode);

        result << viewModeItem;
    }

    return result;
}

bool NotationStatusBarModel::zoomEnabled() const
{
    return notation() != nullptr;
}

int NotationStatusBarModel::currentZoomPercentage() const
{
    if (!notation()) {
        return 100;
    }

    return notation()->viewState()->zoomPercentage().val;
}

void NotationStatusBarModel::setCurrentZoomPercentage(int zoomPercentage)
{
    if (zoomPercentage == currentZoomPercentage()) {
        return;
    }

    dispatch(zoomTypeToActionCode(ZoomType::Percentage), ActionData::make_arg1<int>(zoomPercentage));
}

ZoomType NotationStatusBarModel::currentZoomType() const
{
    if (!notation()) {
        return ZoomType::Percentage;
    }

    return notation()->viewState()->zoomType().val;
}

void NotationStatusBarModel::load()
{
    TRACEFUNC;

    onCurrentNotationChanged();
    context()->currentNotationChanged().onNotify(this, [this]() {
        onCurrentNotationChanged();
    });

    workspaceConfiguration()->currentWorkspaceNameChanged().onReceive(this, [this](const std::string&) {
        emit currentWorkspaceActionChanged();
    });

    actionsRegister()->actionStateChanged().onReceive(this, [this](const ActionCodeList& codeList) {
        for (const ActionCode& code : codeList) {
            if (code == SELECT_WORKSPACE_CODE) {
                emit currentWorkspaceActionChanged();
            }
        }
    });
}

void NotationStatusBarModel::onCurrentNotationChanged()
{
    emit currentViewModeChanged();
    emit availableViewModeListChanged();
    emit currentZoomPercentageChanged();
    emit availableZoomListChanged();
    emit zoomEnabledChanged();
    emit concertPitchActionChanged();

    if (!notation()) {
        return;
    }

    notation()->undoStack()->changesChannel().onReceive(this, [this](const mu::engraving::ScoreChangesRange& range) {
        if (mu::contains(range.changedStyleIdSet, mu::engraving::Sid::concertPitch)) {
            emit concertPitchActionChanged();
        }
    });

    notation()->viewModeChanged().onNotify(this, [this]() {
        emit currentViewModeChanged();
        emit availableViewModeListChanged();
    });

    notation()->viewState()->zoomPercentage().ch.onReceive(this, [this](int) {
        emit currentZoomPercentageChanged();
        emit availableZoomListChanged();
    });

    notation()->viewState()->zoomType().ch.onReceive(this, [this](ZoomType) {
        emit availableZoomListChanged();
    });

    listenChangesInAccessibility();
}

void NotationStatusBarModel::listenChangesInAccessibility()
{
    if (!accessibility()) {
        return;
    }

    emit accessibilityInfoChanged();

    accessibility()->accessibilityInfo().ch.onReceive(this, [this](const std::string&) {
        emit accessibilityInfoChanged();
    });
}

void NotationStatusBarModel::selectWorkspace()
{
    dispatch(SELECT_WORKSPACE_CODE);
}

void NotationStatusBarModel::toggleConcertPitch()
{
    dispatch(TOGGLE_CONCERT_PITCH_CODE);
}

void NotationStatusBarModel::setCurrentViewMode(const QString& modeCode)
{
    dispatch(codeFromQString(modeCode));
}

MenuItemList NotationStatusBarModel::makeAvailableZoomList()
{
    if (!notation()) {
        return {};
    }

    MenuItemList result;

    int currZoomPercentage = currentZoomPercentage();
    ZoomType currZoomType = currentZoomType();

    auto zoomPercentageTitle = [](int percentage) {
        return TranslatableString::untranslatable("%1%").arg(percentage);
    };

    auto buildZoomItem = [=](ZoomType type, const TranslatableString& title = {}, int value = 0) {
        MenuItem* menuItem = new MenuItem(this);
        menuItem->setId(QString::number(static_cast<int>(type)) + QString::number(value));

        UiAction action;
        action.title = title.isEmpty() ? zoomTypeTitle(type) : title;
        menuItem->setAction(action);

        UiActionState state;
        state.enabled = true;
        menuItem->setState(state);

        menuItem->setSelectable(true);
        if (currZoomType == type) {
            menuItem->setSelected(type == ZoomType::Percentage ? value == currZoomPercentage : true);
        }

        menuItem->setArgs(ActionData::make_arg2<ZoomType, int>(type, value));

        return menuItem;
    };

    QList<int> possibleZoomList = possibleZoomPercentageList();

    for (int zoom : possibleZoomList) {
        result << buildZoomItem(ZoomType::Percentage, zoomPercentageTitle(zoom), zoom);
    }

    result << buildZoomItem(ZoomType::PageWidth);
    result << buildZoomItem(ZoomType::WholePage);
    result << buildZoomItem(ZoomType::TwoPages);

    bool isCustomZoom = currZoomType == ZoomType::Percentage && !possibleZoomList.contains(currZoomPercentage);
    if (isCustomZoom) {
        MenuItem* customZoom = buildZoomItem(ZoomType::Percentage, zoomPercentageTitle(currZoomPercentage), currZoomPercentage);
        customZoom->setSelected(true);
        result << customZoom;
    }

    return result;
}

void NotationStatusBarModel::setCurrentZoom(const QString& zoomId)
{
    MenuItemList zoomList = makeAvailableZoomList();
    int zoomIndex = -1;
    for (int i = 0; i < zoomList.count(); ++i) {
        MenuItem* zoomItem = zoomList[i];
        if (zoomItem->id() == zoomId) {
            zoomIndex = i;
            break;
        }
    }

    if (zoomIndex < 0 || zoomIndex >= zoomList.size()) {
        return;
    }

    MenuItem* zoom = zoomList[zoomIndex];
    ZoomType type = zoom->args().arg<ZoomType>(0);
    int value = zoom->args().arg<int>(1);

    emit availableZoomListChanged();

    dispatch(zoomTypeToActionCode(type), ActionData::make_arg1<int>(value));
}

int NotationStatusBarModel::minZoomPercentage() const
{
    return 5;
}

int NotationStatusBarModel::maxZoomPercentage() const
{
    return possibleZoomPercentageList().last();
}

void NotationStatusBarModel::zoomIn()
{
    dispatch("zoomin");
}

void NotationStatusBarModel::zoomOut()
{
    dispatch("zoomout");
}

void NotationStatusBarModel::handleAction(const QString& actionCode)
{
    dispatch(codeFromQString(actionCode));
}

INotationPtr NotationStatusBarModel::notation() const
{
    return context()->currentNotation();
}

INotationAccessibilityPtr NotationStatusBarModel::accessibility() const
{
    return notation() ? notation()->accessibility() : nullptr;
}

void NotationStatusBarModel::dispatch(const actions::ActionCode& code, const actions::ActionData& args)
{
    dispatcher()->dispatch(code, args);
}

QList<int> NotationStatusBarModel::possibleZoomPercentageList() const
{
    QList<int> result;

    for (int zoom : notationConfiguration()->possibleZoomPercentageList()) {
        if (zoom >= MIN_DISPLAYED_ZOOM_PERCENTAGE) {
            result << zoom;
        }
    }

    return result;
}

QVariantList NotationStatusBarModel::availableViewModeList_property()
{
    return menuItemListToVariantList(makeAvailableViewModeList());
}

QVariantList NotationStatusBarModel::availableZoomList_property()
{
    return menuItemListToVariantList(makeAvailableZoomList());
}

QVariantList NotationStatusBarModel::menuItemListToVariantList(const MenuItemList& list) const
{
    QVariantList result;
    for (MenuItem* item: list) {
        result << QVariant::fromValue(item);
    }

    return result;
}
