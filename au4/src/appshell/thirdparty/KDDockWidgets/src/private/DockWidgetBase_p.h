/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#ifndef KD_DOCKWIDGET_BASE_P_H
#define KD_DOCKWIDGET_BASE_P_H

#include "DockWidgetBase.h"
#include "SideBar_p.h"
#include "DockRegistry_p.h"
#include "Position_p.h"
#include "FloatingWindow_p.h"

#include <QCoreApplication>
#include <QString>
#include <QSize>

QT_BEGIN_NAMESPACE
class QAction;
QT_END_NAMESPACE

namespace KDDockWidgets {

class DOCKS_EXPORT_FOR_UNIT_TESTS DockWidgetBase::Private : public QObject /// clazy:exclude=missing-qobject-macro
{
public:
    Private(const QString &dockName, DockWidgetBase::Options options_,
            LayoutSaverOptions layoutSaverOptions_, DockWidgetBase *qq);

    void init()
    {
        updateTitle();
    }

    /**
     * @brief returns the FloatingWindow this dock widget is in. If nullptr then it's in a
     * MainWindow.
     *
     * Note: Being in a FloatingWindow doesn't necessarily mean @ref isFloating() returns true, as
     * the dock widget might be in a floating window with other dock widgets side by side.
     */
    FloatingWindow *floatingWindow() const
    {
        return qobject_cast<FloatingWindow *>(q->window());
    }

    MainWindowBase *mainWindow() const
    {
        if (q->isWindow())
            return nullptr;

        // Note: Don't simply use window(), as the MainWindow might be embedded into something else
        QWidgetOrQuick *p = q->parentWidget();
        while (p) {
            if (auto window = qobject_cast<MainWindowBase *>(p))
                return window;

            if (p->isWindow())
                return nullptr;

            p = p->parentWidget();
        }

        return nullptr;
    }

    SideBar *sideBar() const
    {
        return DockRegistry::self()->sideBarForDockWidget(q);
    }

    ///@brief adds the current layout item containing this dock widget
    void addPlaceholderItem(Layouting::Item *);

    ///@brief returns the last position, just for tests.
    LastPositions &lastPositions();

    void forceClose();
    QPoint defaultCenterPosForFloating();

    bool eventFilter(QObject *watched, QEvent *event) override;

    void updateTitle();
    void toggle(bool enabled);
    void updateToggleAction();
    void updateFloatAction();
    void onDockWidgetShown();
    void onDockWidgetHidden();
    void show();
    void close();
    bool restoreToPreviousPosition();
    void maybeRestoreToPreviousPosition();
    int currentTabIndex() const;

    /**
     * @brief Serializes this dock widget into an intermediate form
     */
    std::shared_ptr<LayoutSaver::DockWidget> serialize() const;

    /**
     * @brief the Frame which contains this dock widgets.
     *
     * A frame wraps a docked DockWidget, giving it a TabWidget so it can accept other dock widgets.
     * Frame is also the actual class that goes into a LayoutWidget.
     *
     * It's nullptr immediately after creation.
     */
    Frame *frame() const;

    ///@brief If this dock widget is floating, then it saves its geometry
    void saveLastFloatingGeometry();

    /**
     * Before floating a dock widget we save its position. So it can be restored when calling
     * DockWidget::setFloating(false)
     */
    void saveTabIndex();

    /**
     * @brief Creates a FloatingWindow and adds itself into it
     * @return the created FloatingWindow
     */
    KDDockWidgets::FloatingWindow *morphIntoFloatingWindow();

    /// @brief calls morphIntoFloatingWindow() if the dock widget is visible and is a top-level
    /// This is called delayed whenever we show a floating dock widget, so we get a FloatingWindow
    void maybeMorphIntoFloatingWindow();

    /// @brief Returns the mdi layout this dock widget is in, if any.
    MDILayoutWidget *mdiLayout() const;

    const QString name;
    QStringList affinities;
    QString title;
    QIcon titleBarIcon;
    QIcon tabBarIcon;
    QWidgetOrQuick *widget = nullptr;
    DockWidgetBase *const q;
    DockWidgetBase::Options options;
    const LayoutSaverOptions layoutSaverOptions;
    QAction *const toggleAction;
    QAction *const floatAction;
    LastPositions m_lastPositions;
    bool m_isPersistentCentralDockWidget = false;
    bool m_processingToggleAction = false;
    bool m_updatingToggleAction = false;
    bool m_updatingFloatAction = false;
    bool m_isForceClosing = false;
    bool m_isMovingToSideBar = false;
    QSize m_lastOverlayedSize = QSize(0, 0);
    int m_userType = 0;
};
}

#if defined(QT_WIDGETS_LIB)
#include <QAction>
#else
// A QAction for QtQuick. Just so it compiles, for now
class QAction : public QObject
{
    Q_OBJECT
public:
    using QObject::QObject;

    bool isChecked() const
    {
        return m_isChecked;
    }

    void setCheckable(bool is)
    {
        m_isCheckable = is;
    }

    void setText(const QString &text)
    {
        m_text = text;
    }

    void setToolTip(const QString &text)
    {
        m_toolTip = text;
    }

    QString toolTip() const
    {
        return m_toolTip;
    }

    bool enabled() const
    {
        return m_enabled;
    }

    void setEnabled(bool enabled)
    {
        m_enabled = enabled;
    }

    bool checked() const
    {
        return m_checked;
    }

    void setChecked(bool checked)
    {
        m_checked = checked;
    }

    bool isEnabled() const
    {
        return m_enabled;
    }

    void toggle()
    {
        m_enabled = !m_enabled;
        Q_EMIT toggled(m_enabled);
    }

Q_SIGNALS:
    bool toggled(bool);

private:
    QString m_text;
    QString m_toolTip;

    bool m_isChecked = false;
    bool m_isCheckable = false;
    bool m_enabled = false;
    bool m_checked = false;
};

#endif

#endif
