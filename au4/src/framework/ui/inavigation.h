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
#ifndef MU_UI_INAVIGATION_H
#define MU_UI_INAVIGATION_H

#include <tuple>
#include <memory>
#include <functional>
#include <QString>
#include <QList>
#include <QVariantMap>

#include "async/channel.h"
#include "async/notification.h"

class QWindow;

namespace mu::ui {
class INavigationSection;
class INavigationPanel;
class INavigationControl;

class INavigation
{
public:
    virtual ~INavigation() = default;

    struct Event
    {
        //! NOTE Please sync with view/NavigationEvent::Type
        enum Type {
            Undefined = 0,
            Left,
            Right,
            Up,
            Down,
            Trigger,
            Escape,

            // Internal events
            AboutActive // sending before activation
        };

        Type type = Undefined;
        bool accepted = false;
        QVariantMap data; // additional data

        Event(Type t)
            : type(t) {}

        static std::shared_ptr<Event> make(Type t) { return std::make_shared<Event>(t); }
    };
    using EventPtr = std::shared_ptr<Event>;

    struct Index
    {
        int column = -1;
        int row = -1;

        void setOrder(int n) { column = n; }
        int order() const { return column; }

        inline bool operator ==(const Index& idx) const { return column == idx.column && row == idx.row; }

        std::string to_string() const { return std::string("[") + std::to_string(row) + "," + std::to_string(column) + "]"; }
    };

    enum class ActivationType {
        None,
        ByMouse
    };

    virtual QString name() const = 0;

    virtual const Index& index() const = 0;
    virtual void setIndex(const Index& index) = 0;
    virtual async::Channel<Index> indexChanged() const = 0;

    virtual bool enabled() const = 0;
    virtual async::Channel<bool> enabledChanged() const = 0;

    virtual bool active() const = 0;
    virtual void setActive(bool arg) = 0;
    virtual async::Channel<bool> activeChanged() const = 0;

    virtual QWindow* window() const = 0;

    virtual void onEvent(EventPtr e) = 0;
};

class INavigationPanel;
class INavigationControl : public INavigation
{
public:
    virtual ~INavigationControl() = default;

    virtual INavigationPanel* panel() const = 0;

    virtual void trigger() = 0;
    virtual void requestActive(bool enableHighlight = false) = 0;
};

class INavigationSection;
class INavigationPanel : public INavigation
{
public:
    virtual ~INavigationPanel() = default;

    //! NOTE Please sync with view/NavigationPanel::Direction
    enum class Direction {
        Horizontal = 0,
        Vertical,
        Both
    };

    virtual INavigationSection* section() const = 0;
    virtual Direction direction() const = 0;
    virtual const std::set<INavigationControl*>& controls() const = 0;
    virtual async::Notification controlsListChanged() const = 0;
    virtual void requestActive(INavigationControl* control = nullptr, bool enableHighlight = false,
                               INavigation::ActivationType activationType = INavigation::ActivationType::None) = 0;
};

using OnActiveRequested = std::function<void (INavigationSection* sec, INavigationPanel* panel, INavigationControl* ctrl,
                                              bool enableHighlight, INavigation::ActivationType activationType)>;

class INavigationSection : public INavigation
{
public:
    virtual ~INavigationSection() = default;

    //! NOTE Please sync with view/NavigationSection::Type
    enum class Type {
        Regular = 0,
        //! NOTE If activated exclusive section, we shouldn't navigate to another section.
        //! Typically exclusive section - this is dialog
        Exclusive
    };

    virtual Type type() const = 0;
    virtual const std::set<INavigationPanel*>& panels() const = 0;
    virtual async::Notification panelsListChanged() const = 0;

    virtual void setOnActiveRequested(const OnActiveRequested& func) = 0;
    virtual void requestActive(INavigationPanel* panel = nullptr, INavigationControl* control = nullptr, bool enableHighlight = false,
                               INavigation::ActivationType activationType = INavigation::ActivationType::None) = 0;
};
}

#endif // MU_UI_INAVIGATION_H
