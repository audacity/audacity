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

#include "filepickermodel.h"

using namespace mu::uicomponents;

FilePickerModel::FilePickerModel(QObject* parent)
    : QObject(parent)
{
}

QString FilePickerModel::title() const
{
    return m_title;
}

QString FilePickerModel::dir() const
{
    return m_dir;
}

QStringList FilePickerModel::filter() const
{
    return m_filter;
}

QString FilePickerModel::selectFile()
{
    std::vector<std::string> filter;
    for (const QString& nameFilter : m_filter) {
        filter.push_back(nameFilter.toStdString());
    }

    io::path_t file = interactive()->selectOpeningFile(m_title, m_dir, filter);

    if (!file.empty()) {
        m_dir = io::dirpath(file).toQString();
    }

    return file.toQString();
}

QString FilePickerModel::selectDirectory()
{
    io::path_t directory = interactive()->selectDirectory(m_title, m_dir);

    if (!directory.empty()) {
        m_dir = directory.toQString();
    }

    return directory.toQString();
}

QString FilePickerModel::selectMultipleDirectories(const QString& selectedDirectoriesStr)
{
    io::paths_t selectedDirectories = io::pathsFromString(selectedDirectoriesStr.toStdString());
    io::paths_t directories = interactive()->selectMultipleDirectories(m_title, m_dir, selectedDirectories);

    QStringList result;
    for (const io::path_t& dir: directories) {
        result << dir.toQString();
    }

    return result.join(";");
}

void FilePickerModel::setTitle(const QString& title)
{
    if (title == m_title) {
        return;
    }

    m_title = title;
    emit titleChanged(title);
}

void FilePickerModel::setDir(const QString& dir)
{
    if (dir == m_dir) {
        return;
    }

    m_dir = dir;
    emit dirChanged(dir);
}

void FilePickerModel::setFilter(const QStringList& filter)
{
    if (filter == m_filter) {
        return;
    }

    m_filter = filter;
    emit filterChanged(filter);
}
