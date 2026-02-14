/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-Studio-CLA-applies
 *
 * MuseScore Studio
 * Music Composition & Notation
 *
 * Copyright (C) 2025 MuseScore Limited
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

#pragma once

#include <QObject>
#include <qqmlintegration.h>

#include "modularity/ioc.h"
#include "iappshellconfiguration.h"

namespace au::appshell {
class WelcomeDialogModel : public QObject, public muse::Contextable
{
    Q_OBJECT

    Q_PROPERTY(int count READ count NOTIFY itemsChanged)

    Q_PROPERTY(QVariant currentItem READ currentItem NOTIFY currentItemChanged)
    Q_PROPERTY(int currentIndex READ currentIndex NOTIFY currentItemChanged)

    Q_PROPERTY(bool showOnStartup READ showOnStartup WRITE setShowOnStartup NOTIFY showOnStartupChanged)

    QML_ELEMENT

    muse::GlobalInject<IAppShellConfiguration> configuration;

public:
    WelcomeDialogModel();

    Q_INVOKABLE void init();

    int count() const { return static_cast<int>(m_items.size()); }

    QVariantMap currentItem() const;
    int currentIndex() const { return static_cast<int>(m_currentIndex); }

    Q_INVOKABLE void nextItem();
    Q_INVOKABLE void prevItem();

    bool showOnStartup() const;
    void setShowOnStartup(bool show);

signals:
    void itemsChanged();
    void currentItemChanged();
    void showOnStartupChanged();

private:
    bool hasPrev() const { return m_currentIndex > 0; }
    bool hasNext() const { return m_currentIndex < m_items.size() - 1; }

    std::vector<QVariantMap> m_items;
    size_t m_currentIndex = 0;
};
}
