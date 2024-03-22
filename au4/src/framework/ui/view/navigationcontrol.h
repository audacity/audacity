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
#ifndef MU_UI_NAVIGATIONCONTROL_H
#define MU_UI_NAVIGATIONCONTROL_H

#include <QObject>
#include <QQuickItem>

#include "abstractnavigation.h"

#ifndef MU_QT5_COMPAT
Q_MOC_INCLUDE("ui/view/navigationpanel.h")
#endif

namespace mu::ui {
class NavigationPanel;
class NavigationControl : public AbstractNavigation, public INavigationControl
{
    Q_OBJECT
    Q_PROPERTY(mu::ui::NavigationPanel * panel READ panel_property WRITE setPanel NOTIFY panelChanged)

public:
    explicit NavigationControl(QObject* parent = nullptr);
    ~NavigationControl() override;

    NavigationPanel* panel_property() const;
    INavigationPanel* panel() const override;

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

    void trigger() override;

    Q_INVOKABLE void requestActive(bool enableHighlight = false) override;
    Q_INVOKABLE void requestActiveByInteraction(bool enableHighlight = false);

public slots:
    void setPanel(NavigationPanel* panel);

signals:
    void panelChanged(NavigationPanel* panel);
    void triggered();

private slots:
    void onPanelDestroyed();

private:
    NavigationPanel* m_panel = nullptr;
};
}

#endif // MU_UI_NAVIGATIONCONTROL_H
