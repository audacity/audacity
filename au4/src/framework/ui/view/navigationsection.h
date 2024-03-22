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
#ifndef MU_UI_NAVIGATIONSECTION_H
#define MU_UI_NAVIGATIONSECTION_H

#include <QObject>
#include <QList>

#include "abstractnavigation.h"
#include "navigationpanel.h"

#include "modularity/ioc.h"
#include "global/iapplication.h"

#include "../inavigationcontroller.h"

namespace mu::ui {
class NavigationSection : public AbstractNavigation, public INavigationSection
{
    Q_OBJECT
    Q_PROPERTY(QmlType type READ type_property WRITE setType NOTIFY typeChanged)

    INJECT(IApplication, application)
    INJECT(INavigationController, navigationController)

public:
    explicit NavigationSection(QObject* parent = nullptr);
    ~NavigationSection() override;

    //! NOTE Please sync with INavigationSection::Type
    enum QmlType {
        Regular = 0,
        Exclusive
    };
    Q_ENUM(QmlType)

    QmlType type_property() const;
    INavigationSection::Type type() const override;
    QString name() const override;

    const Index& index() const override;
    void setIndex(const INavigation::Index& index) override;
    async::Channel<Index> indexChanged() const override;

    bool enabled() const override;
    async::Channel<bool> enabledChanged() const override;

    bool active() const override;
    void setActive(bool arg) override;
    async::Channel<bool> activeChanged() const override;

    void onEvent(EventPtr e) override;

    QWindow* window() const override;

    const std::set<INavigationPanel*>& panels() const override;
    async::Notification panelsListChanged() const override;

    void componentComplete() override;

    void addPanel(NavigationPanel* panel);
    void removePanel(NavigationPanel* panel);

    void setOnActiveRequested(const OnActiveRequested& func) override;

    //! NOTE Can be called from QML without args
    Q_INVOKABLE void requestActive(INavigationPanel* panel = nullptr, INavigationControl* control = nullptr, bool enableHighlight = false,
                                   ActivationType activationType = ActivationType::None) override;

public slots:
    void setType(QmlType type);

signals:
    void typeChanged(QmlType type);

private:
    std::set<INavigationPanel*> m_panels;
    async::Notification m_panelsListChanged;
    QmlType m_type = Regular;
    OnActiveRequested m_onActiveRequested;
};
}

#endif // MU_UI_NAVIGATIONSECTION_H
