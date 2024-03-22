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
#ifndef MU_UI_NAVIGATIONPANEL_H
#define MU_UI_NAVIGATIONPANEL_H

#include <QObject>
#include <QList>

#include "abstractnavigation.h"
#include "navigationcontrol.h"

#ifndef MU_QT5_COMPAT
Q_MOC_INCLUDE("ui/view/navigationsection.h")
#endif

namespace mu::ui {
class NavigationSection;
class NavigationPanel : public AbstractNavigation, public INavigationPanel
{
    Q_OBJECT
    Q_PROPERTY(mu::ui::NavigationSection * section READ section_property WRITE setSection_property NOTIFY sectionChanged)
    Q_PROPERTY(QmlDirection direction READ direction_property WRITE setDirection NOTIFY directionChanged)
    Q_PROPERTY(QString directionInfo READ directionInfo NOTIFY directionChanged)

public:
    explicit NavigationPanel(QObject* parent = nullptr);
    ~NavigationPanel() override;

    //! NOTE Please sync with INavigationPanel::Direction
    enum QmlDirection {
        Horizontal = 0,
        Vertical,
        Both
    };
    Q_ENUM(QmlDirection)

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

    QmlDirection direction_property() const;
    QString directionInfo() const;
    Direction direction() const override;

    const std::set<INavigationControl*>& controls() const override;
    async::Notification controlsListChanged() const override;

    INavigationSection* section() const override;
    NavigationSection* section_property() const;

    void addControl(NavigationControl* control);
    void removeControl(NavigationControl* control);

    //! NOTE Can be called from QML without args
    Q_INVOKABLE void requestActive(INavigationControl* control = nullptr, bool enableHighlight = false,
                                   ActivationType activationType = ActivationType::None) override;

public slots:
    void setSection_property(NavigationSection* section);
    void setSection(INavigationSection* section);
    void setDirection(QmlDirection direction);

signals:
    void sectionChanged(NavigationSection* section);
    void directionChanged();

private slots:
    void onSectionDestroyed();

private:
    NavigationSection* m_section = nullptr;
    std::set<INavigationControl*> m_controls;
    async::Notification m_controlsListChanged;
    QmlDirection m_direction = QmlDirection::Horizontal;
};
}

#endif // MU_UI_NAVIGATIONPANEL_H
