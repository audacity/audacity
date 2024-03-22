/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#ifndef KD_DOCKWIDGET_INSTANTIATOR_P_H
#define KD_DOCKWIDGET_INSTANTIATOR_P_H

#include "DockWidgetQuick.h"

#include <QQmlParserStatus>
#include <QQuickItem>
#include <QString>

#include <optional>

namespace KDDockWidgets {

class DockWidgetQuick;

/**
 * @brief Indirection helper to instantiate dock widgets from QML
 *
 * "DockWidget {}" in QML won't create a KDDockWidget::DockWidget directly, but instead an
 * DockWidgetInstantiator. DockWidgetInstantiator will then create the DockWidget instance only
 * when the QML parsing ends (and all propreties are set)
 *
 * This allows to pass the correct uniqueName to DockWidget's ctor.
 */
class DockWidgetInstantiator : public QQuickItem
{
    Q_OBJECT
    Q_PROPERTY(QString uniqueName READ uniqueName WRITE setUniqueName NOTIFY uniqueNameChanged)
    Q_PROPERTY(QString source READ source WRITE setSource NOTIFY sourceChanged)
    Q_PROPERTY(KDDockWidgets::DockWidgetQuick *dockWidget READ dockWidget NOTIFY dockWidgetChanged)
    Q_PROPERTY(
        KDDockWidgets::TitleBar *actualTitleBar READ actualTitleBar NOTIFY actualTitleBarChanged)
    Q_PROPERTY(QString title READ title WRITE setTitle NOTIFY titleChanged)
    Q_PROPERTY(bool isFocused READ isFocused NOTIFY isFocusedChanged)
    Q_PROPERTY(bool isFloating READ isFloating WRITE setFloating NOTIFY isFloatingChanged)
public:
    QString uniqueName() const;
    void setUniqueName(const QString &);

    QString source() const;
    void setSource(const QString &);

    DockWidgetQuick *dockWidget() const;
    KDDockWidgets::TitleBar *actualTitleBar() const;

    QString title() const;
    void setTitle(const QString &title);

    bool isFocused() const;
    bool isFloating() const;
    void setFloating(bool);

    Q_INVOKABLE void addDockWidgetAsTab(KDDockWidgets::DockWidgetInstantiator *other,
                                        KDDockWidgets::InitialVisibilityOption = {});
    Q_INVOKABLE void addDockWidgetAsTab(KDDockWidgets::DockWidgetBase *other,
                                        KDDockWidgets::InitialVisibilityOption = {});

    Q_INVOKABLE void addDockWidgetToContainingWindow(KDDockWidgets::DockWidgetBase *other,
                                                     KDDockWidgets::Location location,
                                                     KDDockWidgets::DockWidgetBase *relativeTo = nullptr,
                                                     QSize initialSize = {},
                                                     KDDockWidgets::InitialVisibilityOption = {});

    Q_INVOKABLE void addDockWidgetToContainingWindow(KDDockWidgets::DockWidgetInstantiator *other,
                                                     KDDockWidgets::Location location,
                                                     KDDockWidgets::DockWidgetInstantiator *relativeTo = nullptr,
                                                     QSize initialSize = {},
                                                     KDDockWidgets::InitialVisibilityOption = {});

    Q_INVOKABLE void setAsCurrentTab();
    Q_INVOKABLE void forceClose();
    Q_INVOKABLE bool close();
    Q_INVOKABLE void show();
    Q_INVOKABLE void raise();
    Q_INVOKABLE void moveToSideBar();

protected:
    void classBegin() override;
    void componentComplete() override;

Q_SIGNALS:
    void uniqueNameChanged();
    void sourceChanged();
    void dockWidgetChanged();
    void actualTitleBarChanged();
    void titleChanged(const QString &title);
    void shown();
    void hidden();
    void iconChanged();
    void widgetChanged(KDDockWidgets::QWidgetOrQuick *);
    void optionsChanged(KDDockWidgets::DockWidgetBase::Options);
    void isFocusedChanged(bool);
    void isOverlayedChanged(bool);
    void isFloatingChanged(bool);
    void removedFromSideBar();
    void windowActiveAboutToChange(bool activated);

private:
    std::optional<bool> m_isFloating;
    QString m_uniqueName;
    QString m_sourceFilename;
    QString m_title;
    DockWidgetQuick *m_dockWidget = nullptr;
};
}

#endif
