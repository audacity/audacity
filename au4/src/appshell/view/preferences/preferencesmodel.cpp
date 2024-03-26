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
#include <QApplication>

#include "preferencesmodel.h"

#include "log.h"
#include "translation.h"
#include "ui/view/iconcodes.h"

using namespace au::appshell;
using namespace mu::ui;

PreferencesModel::PreferencesModel(QObject* parent)
    : QAbstractItemModel(parent)
{
}

PreferencesModel::~PreferencesModel()
{
    cancel();

    delete m_rootItem;
    m_rootItem = nullptr;
}

QModelIndex PreferencesModel::index(int row, int column, const QModelIndex& parent) const
{
    if (!hasIndex(row, column, parent)) {
        return QModelIndex();
    }

    PreferencePageItem* parentItem = nullptr;

    if (!parent.isValid()) {
        parentItem = m_rootItem;
    } else {
        parentItem = modelIndexToItem(parent);
    }

    if (!parentItem) {
        return QModelIndex();
    }

    PreferencePageItem* childItem = parentItem->childAtRow(row);

    if (childItem) {
        return createIndex(row, column, childItem);
    }

    return QModelIndex();
}

QModelIndex PreferencesModel::parent(const QModelIndex& child) const
{
    PreferencePageItem* childItem = modelIndexToItem(child);
    if (!childItem) {
        return QModelIndex();
    }

    PreferencePageItem* parentItem = qobject_cast<PreferencePageItem*>(childItem->parentItem());

    if (parentItem == m_rootItem) {
        return QModelIndex();
    }

    return createIndex(parentItem->row(), 0, parentItem);
}

int PreferencesModel::rowCount(const QModelIndex& parent) const
{
    PreferencePageItem* parentItem = nullptr;

    if (!parent.isValid()) {
        parentItem = m_rootItem;
    } else {
        parentItem = modelIndexToItem(parent);
    }

    if (!parentItem) {
        return 0;
    }

    return parentItem->childCount();
}

int PreferencesModel::columnCount(const QModelIndex&) const
{
    return 1;
}

QVariant PreferencesModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid() && role != ItemRole) {
        return QVariant();
    }

    PreferencePageItem* item = modelIndexToItem(index);

    if (!item) {
        return QVariant();
    }

    return QVariant::fromValue(qobject_cast<QObject*>(item));
}

QHash<int, QByteArray> PreferencesModel::roleNames() const
{
    return { { ItemRole, "itemRole" } };
}

QString PreferencesModel::currentPageId() const
{
    return m_currentPageId;
}

void PreferencesModel::load(const QString& currentPageId)
{
    configuration()->startEditSettings();

    beginResetModel();

    if (!currentPageId.isEmpty()) {
        setCurrentPageId(currentPageId);
    } else {
        setCurrentPageId("general");
    }

    m_rootItem = new PreferencePageItem();

    QList<PreferencePageItem*> items {
        makeItem("general", QT_TRANSLATE_NOOP("appshell/preferences", "General"), IconCode::Code::SETTINGS_COG,
                 "Preferences/GeneralPreferencesPage.qml"),

        makeItem("appearance", QT_TRANSLATE_NOOP("appshell/preferences", "Appearance"), IconCode::Code::EYE_OPEN,
                 "Preferences/AppearancePreferencesPage.qml"),

        makeItem("canvas", QT_TRANSLATE_NOOP("appshell/preferences", "Canvas"), IconCode::Code::NEW_FILE,
                 "Preferences/CanvasPreferencesPage.qml"),

        makeItem("cloud", QT_TRANSLATE_NOOP("appshell/preferences", "Save & publish"), IconCode::Code::CLOUD_FILE,
                 "Preferences/SaveAndPublishPreferencesPage.qml"),

        makeItem("note-input", QT_TRANSLATE_NOOP("appshell/preferences", "Note input"), IconCode::Code::EDIT,
                 "Preferences/NoteInputPreferencesPage.qml"),

        makeItem("midi-device-mapping", QT_TRANSLATE_NOOP("appshell/preferences", "MIDI mappings"), IconCode::Code::MIDI_INPUT,
                 "Preferences/MidiDeviceMappingPreferencesPage.qml"),

        makeItem("score", QT_TRANSLATE_NOOP("appshell/preferences", "Score"), IconCode::Code::SCORE,
                 "Preferences/ScorePreferencesPage.qml"),

        makeItem("playback", QT_TRANSLATE_NOOP("appshell/preferences", "Playback"), IconCode::Code::AUDIO,
                 "Preferences/PlaybackPreferencesPage.qml"),

        makeItem("import", QT_TRANSLATE_NOOP("appshell/preferences", "Import"), IconCode::Code::IMPORT,
                 "Preferences/ImportPreferencesPage.qml"),

        makeItem("shortcuts", QT_TRANSLATE_NOOP("appshell/preferences", "Shortcuts"), IconCode::Code::SHORTCUTS,
                 "Preferences/ShortcutsPreferencesPage.qml"),

        makeItem("update", QT_TRANSLATE_NOOP("appshell/preferences", "Update"), IconCode::Code::UPDATE,
                 "Preferences/UpdatePreferencesPage.qml"),

        makeItem("general-folders", QT_TRANSLATE_NOOP("appshell/preferences", "Folders"), IconCode::Code::OPEN_FILE,
                 "Preferences/FoldersPreferencesPage.qml"),

        makeItem("advanced", QT_TRANSLATE_NOOP("appshell/preferences", "Advanced"), IconCode::Code::CONFIGURE,
                 "Preferences/AdvancedPreferencesPage.qml"),

        makeItem("braille", QT_TRANSLATE_NOOP("appshell/preferences", "Braille"), IconCode::Code::BRAILLE,
                 "Preferences/BraillePreferencesPage.qml")
    };

    for (PreferencePageItem* item: items) {
        m_rootItem->appendChild(item);
    }

    endResetModel();
}

void PreferencesModel::resetFactorySettings()
{
    static constexpr bool KEEP_DEFAULT_SETTINGS = true;
    QApplication::setOverrideCursor(Qt::WaitCursor);
    QApplication::processEvents();
    configuration()->revertToFactorySettings(KEEP_DEFAULT_SETTINGS);
    configuration()->startEditSettings();
    QApplication::restoreOverrideCursor();
}

void PreferencesModel::apply()
{
    configuration()->applySettings();
}

void PreferencesModel::cancel()
{
    configuration()->rollbackSettings();
}

void PreferencesModel::selectRow(const QModelIndex& rowIndex)
{
    QModelIndex parentItemIndex = parent(rowIndex);
    PreferencePageItem* parentItem = nullptr;
    if (!parentItemIndex.isValid()) {
        parentItem = m_rootItem;
    } else {
        parentItem = modelIndexToItem(parentItemIndex);
    }

    QList<PreferencePageItem*> children = parentItem->childrenItems();
    for (PreferencePageItem* child: children) {
        child->setExpanded(false);
    }

    PreferencePageItem* selectedItem = parentItem->childAtRow(rowIndex.row());
    if (!selectedItem) {
        return;
    }

    selectedItem->setExpanded(true);
    setCurrentPageId(selectedItem->id());
}

QVariantList PreferencesModel::availablePages() const
{
    std::function<QVariantList(const PreferencePageItem*)> childPages;
    childPages = [&childPages](const PreferencePageItem* item) {
        QVariantList result;

        for (int i = 0; i < item->childCount(); ++i) {
            PreferencePageItem* child = item->childAtRow(i);
            QVariantMap childObj;
            childObj["id"] = child->id();
            childObj["path"] = child->path();
            result << childObj;

            QVariantList pages = childPages(child);
            for (const QVariant& page: pages) {
                result << page;
            }
        }

        return result;
    };

    return childPages(m_rootItem);
}

void PreferencesModel::setCurrentPageId(QString currentPageId)
{
    if (m_currentPageId == currentPageId) {
        return;
    }

    m_currentPageId = currentPageId;
    emit currentPageIdChanged(m_currentPageId);
}

PreferencePageItem* PreferencesModel::makeItem(const QString& id, const QString& title, mu::ui::IconCode::Code icon,
                                               const QString& path,
                                               const QList<PreferencePageItem*>& children) const
{
    PreferencePageItem* item = new PreferencePageItem();
    item->setId(id);
    item->setTitle(title);
    item->setIcon(icon);
    item->setPath(path);
    item->setExpanded(id == currentPageId());

    for (PreferencePageItem* child: children) {
        item->appendChild(child);

        if (child->id() == currentPageId()) {
            item->setExpanded(true);
        }
    }

    return item;
}

PreferencePageItem* PreferencesModel::modelIndexToItem(const QModelIndex& index) const
{
    return static_cast<PreferencePageItem*>(index.internalPointer());
}
