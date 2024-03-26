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
#include "scorepreferencesmodel.h"

#include "log.h"
#include "translation.h"

using namespace au::appshell;

ScorePreferencesModel::ScorePreferencesModel(QObject* parent)
    : QAbstractListModel(parent)
{
}

int ScorePreferencesModel::rowCount(const QModelIndex&) const
{
    return m_defaultFiles.count();
}

QVariant ScorePreferencesModel::data(const QModelIndex& index, int role) const
{
    const DefaultFileInfo& file = m_defaultFiles.at(index.row());
    switch (role) {
    case TitleRole: return file.title;
    case PathRole: return file.path;
    case PathFilterRole: return file.pathFilter;
    case ChooseTitleRole: return file.chooseTitle;
    case DirectoryRole: return fileDirectory(file.path);
    }

    return QVariant();
}

bool ScorePreferencesModel::setData(const QModelIndex& index, const QVariant& value, int role)
{
    const DefaultFileInfo file = m_defaultFiles.at(index.row());

    switch (role) {
    case PathRole:
        if (file.path == value.toString()) {
            return false;
        }

        savePath(file.type, value.toString());
        emit dataChanged(index, index, { PathRole });
        return true;
    default:
        break;
    }

    return false;
}

QHash<int, QByteArray> ScorePreferencesModel::roleNames() const
{
    static const QHash<int, QByteArray> roles = {
        { TitleRole, "title" },
        { PathRole, "path" },
        { PathFilterRole, "pathFilter" },
        { ChooseTitleRole, "chooseTitle" },
        { DirectoryRole, "directory" }
    };

    return roles;
}

void ScorePreferencesModel::load()
{
    beginResetModel();

    m_defaultFiles = {
        { DefaultFileType::FirstScoreOrderList, qtrc("appshell/preferences", "Score order list 1"), firstScoreOrderListPath(),
          scoreOrderPathFilter(), scoreOrderChooseTitle() },
        { DefaultFileType::SecondScoreOrderList, qtrc("appshell/preferences", "Score order list 2"), secondScoreOrderListPath(),
          scoreOrderPathFilter(), scoreOrderChooseTitle() },
        { DefaultFileType::Style, qtrc("appshell/preferences", "Style"), stylePath(),
          stylePathFilter(), styleChooseTitle() },
        { DefaultFileType::PartStyle, qtrc("appshell/preferences", "Style for part"), partStylePath(),
          stylePathFilter(), partStyleChooseTitle() },
    };

    endResetModel();
}

void ScorePreferencesModel::savePath(ScorePreferencesModel::DefaultFileType fileType, const QString& path)
{
    switch (fileType) {
    case DefaultFileType::FirstScoreOrderList:
        setFirstScoreOrderListPath(path);
        break;
    case DefaultFileType::SecondScoreOrderList:
        setSecondScoreOrderListPath(path);
        break;
    case DefaultFileType::Style:
        notationConfiguration()->setDefaultStyleFilePath(path.toStdString());
        break;
    case DefaultFileType::PartStyle:
        notationConfiguration()->setPartStyleFilePath(path.toStdString());
        break;
    case DefaultFileType::Undefined:
        break;
    }

    setPath(fileType, path);
}

QString ScorePreferencesModel::firstScoreOrderListPath() const
{
    io::paths_t scoreOrderListPaths = notationConfiguration()->userScoreOrderListPaths();
    if (scoreOrderListPaths.empty()) {
        return QString();
    }

    return scoreOrderListPaths[0].toQString();
}

void ScorePreferencesModel::setFirstScoreOrderListPath(const QString& path)
{
    io::paths_t scoreOrderListPaths = notationConfiguration()->userScoreOrderListPaths();
    if (scoreOrderListPaths.empty()) {
        return;
    }

    scoreOrderListPaths[0] = path.toStdString();
    notationConfiguration()->setUserScoreOrderListPaths(scoreOrderListPaths);
}

QString ScorePreferencesModel::secondScoreOrderListPath() const
{
    io::paths_t scoreOrderListPaths = notationConfiguration()->userScoreOrderListPaths();
    if (scoreOrderListPaths.size() < 1) {
        return QString();
    }

    return scoreOrderListPaths[1].toQString();
}

void ScorePreferencesModel::setSecondScoreOrderListPath(const QString& path)
{
    io::paths_t scoreOrderListPaths = notationConfiguration()->userScoreOrderListPaths();
    if (scoreOrderListPaths.size() < 1) {
        return;
    }

    scoreOrderListPaths[1] = path.toStdString();
    notationConfiguration()->setUserScoreOrderListPaths(scoreOrderListPaths);
}

QString ScorePreferencesModel::stylePath() const
{
    return notationConfiguration()->defaultStyleFilePath().toQString();
}

QString ScorePreferencesModel::partStylePath() const
{
    return notationConfiguration()->partStyleFilePath().toQString();
}

QStringList ScorePreferencesModel::scoreOrderPathFilter() const
{
    return { qtrc("appshell/preferences", "Score order list") + " (*.xml)" };
}

QStringList ScorePreferencesModel::stylePathFilter() const
{
    return { qtrc("appshell/preferences", "MuseScore style file") + " (*.mss)" };
}

QString ScorePreferencesModel::scoreOrderChooseTitle() const
{
    return qtrc("appshell/preferences", "Choose score order list");
}

QString ScorePreferencesModel::styleChooseTitle() const
{
    return qtrc("appshell/preferences", "Choose default style");
}

QString ScorePreferencesModel::partStyleChooseTitle() const
{
    return qtrc("appshell/preferences", "Choose default style for parts");
}

void ScorePreferencesModel::setPath(ScorePreferencesModel::DefaultFileType fileType, const QString& path)
{
    QModelIndex index = fileIndex(fileType);
    if (!index.isValid()) {
        return;
    }

    m_defaultFiles[index.row()].path = path;
    emit dataChanged(index, index, { PathRole, DirectoryRole });
}

QModelIndex ScorePreferencesModel::fileIndex(ScorePreferencesModel::DefaultFileType fileType)
{
    for (int i = 0; i < m_defaultFiles.count(); ++i) {
        DefaultFileInfo& fileInfo = m_defaultFiles[i];
        if (fileInfo.type == fileType) {
            return index(i, 0);
        }
    }

    return QModelIndex();
}

QString ScorePreferencesModel::fileDirectory(const QString& filePath) const
{
    return io::dirpath(filePath.toStdString()).toQString();
}
