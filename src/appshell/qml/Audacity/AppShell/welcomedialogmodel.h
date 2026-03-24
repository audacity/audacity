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

#include <functional>

#include <QObject>
#include <qcontainerfwd.h>
#include <qqmlintegration.h>

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"
#include "framework/actions/iactionsdispatcher.h"
#include "framework/interactive/iplatforminteractive.h"
#include "framework/ui/iuiconfiguration.h"

#include "iappshellconfiguration.h"

namespace au::appshell {
class WelcomeDialogModel : public QObject, public muse::Contextable, public muse::async::Asyncable
{
    Q_OBJECT

    Q_PROPERTY(int count READ count NOTIFY itemsChanged)

    Q_PROPERTY(QVariant currentItem READ currentItem NOTIFY currentItemChanged)
    Q_PROPERTY(int currentIndex READ currentIndex NOTIFY currentItemChanged)

    Q_PROPERTY(bool showOnStartup READ showOnStartup WRITE setShowOnStartup NOTIFY showOnStartupChanged)

    QML_ELEMENT

    muse::GlobalInject<IAppShellConfiguration> configuration;
    muse::GlobalInject<muse::ui::IUiConfiguration> uiConfiguration;
    muse::GlobalInject<muse::IPlatformInteractive> platformInteractive;

    muse::Inject<muse::actions::IActionsDispatcher> dispatcher { this };

public:
    WelcomeDialogModel();

    Q_INVOKABLE void init();

    int count() const { return static_cast<int>(m_items.size()); }

    QVariantMap currentItem() const;
    int currentIndex() const { return static_cast<int>(m_currentIndex); }

    Q_INVOKABLE void nextItem();
    Q_INVOKABLE void prevItem();

    Q_INVOKABLE void activateCurrentItem();

    bool showOnStartup() const;
    void setShowOnStartup(bool show);

signals:
    void itemsChanged();
    void currentItemChanged();
    void showOnStartupChanged();

private:
    struct Item {
        QString title;
        QString imageUrl;
        QString description;
        QString buttonText;
        std::function<void()> action;

        QVariantMap toQVariantMap() const
        {
            QVariantMap map;
            map.insert("title", title);
            map.insert("imageUrl", imageUrl);
            map.insert("description", description);
            map.insert("buttonText", buttonText);
            return map;
        }
    };

    bool hasPrev() const { return m_currentIndex > 0; }
    bool hasNext() const { return m_currentIndex < m_items.size() - 1; }

    std::vector<Item> buildItems();

    std::vector<Item> m_items;
    size_t m_currentIndex = 0;
};
}
