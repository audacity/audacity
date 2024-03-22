//=============================================================================
//  MuseScore
//  Music Composition & Notation
//
//  Copyright (C) 2021 MuseScore BVBA and others
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License version 2.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
//=============================================================================
#ifndef MU_UICOMPONENTS_SELECTMULTIPLEDIRECTORIESMODEL_H
#define MU_UICOMPONENTS_SELECTMULTIPLEDIRECTORIESMODEL_H

#include <QAbstractListModel>
#include <QItemSelection>

#include "modularity/ioc.h"
#include "iinteractive.h"

namespace mu::uicomponents {
class ItemMultiSelectionModel;
class SelectMultipleDirectoriesModel : public QAbstractListModel
{
    Q_OBJECT

    INJECT(IInteractive, interactive)

    Q_PROPERTY(bool isRemovingAvailable READ isRemovingAvailable NOTIFY selectionChanged)

public:
    explicit SelectMultipleDirectoriesModel(QObject* parent = nullptr);

    QVariant data(const QModelIndex& index, int role) const override;
    bool setData(const QModelIndex& index, const QVariant& value, int role) override;
    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    QHash<int, QByteArray> roleNames() const override;

    QItemSelectionModel* selection() const;

    Q_INVOKABLE void load(const QString& startDir, const QString& directoriesStr);

    Q_INVOKABLE void selectRow(int row);
    Q_INVOKABLE void removeSelectedDirectories();
    Q_INVOKABLE void addDirectory();

    Q_INVOKABLE QString directories() const;

    bool isRemovingAvailable() const;

signals:
    void selectionChanged();
    void directoryAdded(int index);

private:
    enum Roles {
        TitleRole = Qt::UserRole + 1,
        SelectedRole
    };

    bool isIndexValid(int index) const;
    int indexOf(const io::path_t& path) const;

    void doRemoveDirectory(int index);

    io::paths_t m_directories;
    io::path_t m_dir;

    uicomponents::ItemMultiSelectionModel* m_selectionModel = nullptr;
};
}

#endif // MU_UICOMPONENTS_SELECTMULTIPLEDIRECTORIESMODEL_H
