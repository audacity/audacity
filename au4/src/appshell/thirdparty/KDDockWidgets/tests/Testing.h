/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

// We don't care about performance related checks in the tests
// clazy:excludeall=ctor-missing-parent-argument,missing-qobject-macro,range-loop,missing-typeinfo,detaching-member,function-args-by-ref,non-pod-global-static,reserve-candidates,qstring-allocations

#ifndef KDDOCKWIDGETS_TESTING_H
#define KDDOCKWIDGETS_TESTING_H

#include "KDDockWidgets.h"
#include "MainWindowBase.h"
#include "DockWidgetBase.h"
#include "DockRegistry_p.h"

#include <QSize>
#include <QRect>
#include <QVector>
#include <QEvent>
#include <QVariant>

/**
 * @file
 * @brief Namespace for tests related enums and classes
 */

namespace KDDockWidgets {

namespace Testing {

    class WarningObserver
    {
        Q_DISABLE_COPY(WarningObserver)
    public:
        WarningObserver() = default;
        virtual ~WarningObserver();
        virtual void onFatal() = 0;
    };

    struct AddDockWidgetParams {
        QString mainWindowName;
        QString dockWidgetName;
        QString relativeToName;
        KDDockWidgets::Location location;
        KDDockWidgets::InitialVisibilityOption addingOption;

        bool isNull() const
        {
            return mainWindowName.isEmpty();
        }

        QVariantMap toVariantMap() const
        {
            QVariantMap map;

            map["mainWindowName"] = mainWindowName;
            map["dockWidgetName"] = dockWidgetName;
            if (!relativeToName.isEmpty())
                map["relativeToName"] = relativeToName;
            map["location"] = location;
            map["addingOption"] = QVariant::fromValue(addingOption);

            return map;
        }

        static AddDockWidgetParams fillFromVariantMap(const QVariantMap &map)
        {
            AddDockWidgetParams params;

            params.mainWindowName = map["mainWindowName"].toString();
            params.dockWidgetName = map["dockWidgetName"].toString();
            params.relativeToName = map["relativeToName"].toString();
            params.location = KDDockWidgets::Location(map["location"].toInt());
            params.addingOption = map["addingOption"].value<KDDockWidgets::InitialVisibilityOption>();

            return params;
        }

        KDDockWidgets::MainWindowBase *mainWindow() const
        {
            return KDDockWidgets::DockRegistry::self()->mainWindowByName(mainWindowName);
        }

        KDDockWidgets::DockWidgetBase *dockWidget() const
        {
            return KDDockWidgets::DockRegistry::self()->dockByName(dockWidgetName);
        }

        KDDockWidgets::DockWidgetBase *relativeTo() const
        {
            return KDDockWidgets::DockRegistry::self()->dockByName(relativeToName);
        }
    };

    void setWarningObserver(WarningObserver *);

    void installFatalMessageHandler();
    void setExpectedWarning(const QString &);

    bool waitForEvent(QObject *w, QEvent::Type type, int timeout = 2000);
    bool waitForDeleted(QObject *o, int timeout = 2000);
    bool waitForResize(QWidgetOrQuick *w, int timeout = 2000);

    class HostedWidget : public QWidgetOrQuick
    {
    public:

        explicit HostedWidget(QSize minSz = QSize(1,1))
            : m_minSz(minSz)
        {
        }

        ~HostedWidget() override;

        QSize sizeHint() const override
        {
            return m_minSz;
        }

        QSize minimumSizeHint() const override
        {
            return m_minSz;
        }

        void setMinSize(QSize s)
        {
            m_minSz = s;
            updateGeometry();
        }

        QSize m_minSz;
    };
}

struct SetExpectedWarning
{
    explicit SetExpectedWarning(const QString &s)
    {
        if (!s.isEmpty())
            Testing::setExpectedWarning(s);
    }

    ~SetExpectedWarning()
    {
        Testing::setExpectedWarning({});
    }
    Q_DISABLE_COPY(SetExpectedWarning)
};

}

#endif
