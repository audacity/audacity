/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

/**
 * @file
 * @brief Represents a dock widget.
 *
 * @author Sérgio Martins \<sergio.martins@kdab.com\>
 */

#ifndef KD_DOCKWIDGET_QUICK_H
#define KD_DOCKWIDGET_QUICK_H

#include "DockWidgetBase.h"

QT_BEGIN_NAMESPACE
class QCloseEvent;
class QQmlEngine;
QT_END_NAMESPACE

#if (defined(_MSCVER) || defined(_MSC_VER))
// unreferenced function with internal linkage has been removed
#pragma warning(disable: 4505)
#endif

namespace KDDockWidgets {

class Frame;
class TitleBar;

/**
 * @brief Represents a dock widget.
 *
 * Most of the interface lives in DockWidgetBase, to facilitate sharing with QtQuick.
 */
class DOCKS_EXPORT DockWidgetQuick : public DockWidgetBase
{
    Q_OBJECT
    Q_PROPERTY(QObject *actualTitleBar READ actualTitleBarObj NOTIFY actualTitleBarChanged)
public:
    /**
     * @brief constructs a new DockWidget
     * @param uniqueName the name of the dockwidget, should be unique. Use title for user visible text.
     * @param options optional options controlling behaviour
     * @param layoutSaverOptions options regarding LayoutSaver behaviour
     * @param engine the QML engine this dock widget will be created on. If not specified then
     * Config::self().qmlEngine() will be used
     *
     * There's no parent argument. The DockWidget is either parented to FloatingWindow or MainWindow
     * when visible, or stays without a parent when hidden.
     */
    explicit DockWidgetQuick(const QString &uniqueName, Options options = {},
                             LayoutSaverOptions layoutSaverOptions = LayoutSaverOptions(),
                             QQmlEngine *engine = nullptr);

    ///@brief destructor
    ~DockWidgetQuick() override;

    /// Sets the DockWidget's guest item
    /// Similar to DockWidgetBase::setWidget(QQuickItem*)
    void setWidget(const QString &qmlFilename);

    /// @reimp
    void setWidget(QWidgetAdapter *widget) override;

    /// @reimp
    Q_INVOKABLE void setWidget(QQuickItem *widget);

    /// @reimp
    QSize minimumSize() const override;

    /// @reimp
    QSize maximumSize() const override;

    /// @brief Returns the title bar
    TitleBar *actualTitleBar() const;

    /// @brief Returns the title bar
    /// Qt6 requires us to include TitleBar_p.h, so instead the Q_PROPERTY uses
    /// QObject so we don't include private headers in public headers
    QObject *actualTitleBarObj() const;

    /// @brief Returns the visual item which represents Frame in the screen
    /// Equivalent to Frame::visualItem().
    QQuickItem *frameVisualItem() const;

    ///@internal
    Q_INVOKABLE KDDockWidgets::Frame *frame() const;

    /// @brief Called by QtQuick when min-size changes
    Q_INVOKABLE void onGeometryUpdated();

Q_SIGNALS:
    /// @brief The geometry of the frame container this dock widget is in changed
    /// For example, when dragging a dockwidget
    void frameGeometryChanged(QRect);

protected:
    bool event(QEvent *e) override;

private:
    class Private;
    Private *const d;
};

}

#endif
