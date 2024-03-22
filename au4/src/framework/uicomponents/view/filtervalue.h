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
#ifndef MU_UICOMPONENTS_FILTERVALUE_H
#define MU_UICOMPONENTS_FILTERVALUE_H

#include <QObject>
#include <QVariant>

namespace mu::uicomponents {
class CompareType
{
    Q_GADGET
public:
    enum Type
    {
        Equal,
        NotEqual,
        Contains
    };
    Q_ENUM(Type)
};

class FilterValue : public QObject
{
    Q_OBJECT

    Q_PROPERTY(QString roleName READ roleName WRITE setRoleName NOTIFY dataChanged)
    Q_PROPERTY(QVariant roleValue READ roleValue WRITE setRoleValue NOTIFY dataChanged)
    Q_PROPERTY(mu::uicomponents::CompareType::Type compareType READ compareType WRITE setCompareType NOTIFY dataChanged)
    Q_PROPERTY(bool enabled READ enabled WRITE setEnabled NOTIFY dataChanged)

    /// Determines whether the SortFilterProxyModel should react asynchronously to the `dataChanged` signal.
    Q_PROPERTY(bool async READ async WRITE setAsync NOTIFY dataChanged)

public:
    explicit FilterValue(QObject* parent = nullptr);

    QString roleName() const;
    QVariant roleValue() const;
    CompareType::Type compareType() const;
    bool enabled() const;

    bool async() const;
    void setAsync(bool async);

public slots:
    void setRoleName(QString roleName);
    void setRoleValue(QVariant roleValue);
    void setCompareType(mu::uicomponents::CompareType::Type compareType);
    void setEnabled(bool enabled);

signals:
    void dataChanged();

private:
    QString m_roleName;
    QVariant m_roleValue;
    CompareType::Type m_compareType = CompareType::Equal;
    bool m_enabled = true;
    bool m_async = false;
};
}

#endif // MU_UICOMPONENTS_FILTERVALUE_H
