/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#ifndef KD_TITLEBAR_P_H
#define KD_TITLEBAR_P_H

#include "kddockwidgets/docks_export.h"

#include "kddockwidgets/QWidgetAdapter.h"
#include "kddockwidgets/DockWidgetBase.h"
#include "Draggable_p.h"
#include "Frame_p.h"

#include <QVector>
#include <QIcon>

QT_BEGIN_NAMESPACE
class QHBoxLayout;
class QLabel;
QT_END_NAMESPACE

class TestDocks;

namespace KDDockWidgets {

class DockWidgetBase;
class Frame;
class Button;

class DOCKS_EXPORT TitleBar : public QWidgetAdapter, public Draggable
{
    Q_OBJECT
    Q_PROPERTY(QString title READ title NOTIFY titleChanged)
    Q_PROPERTY(bool hasIcon READ hasIcon NOTIFY iconChanged)
    Q_PROPERTY(bool closeButtonEnabled READ closeButtonEnabled WRITE setCloseButtonEnabled NOTIFY closeButtonEnabledChanged)
    Q_PROPERTY(bool floatButtonVisible READ floatButtonVisible WRITE setFloatButtonVisible NOTIFY floatButtonVisibleChanged)
    Q_PROPERTY(QString floatButtonToolTip READ floatButtonToolTip NOTIFY floatButtonToolTipChanged)
    Q_PROPERTY(bool isFocused READ isFocused NOTIFY isFocusedChanged)
public:
    typedef QVector<TitleBar *> List;

    explicit TitleBar(Frame *parent);
    explicit TitleBar(FloatingWindow *parent);
    ~TitleBar() override;

    void setTitle(const QString &title);
    QString title() const
    {
        return m_title;
    }

    void setIcon(const QIcon &icon);
    std::unique_ptr<WindowBeingDragged> makeWindow() override;
    bool isWindow() const override;

    DockWidgetBase *singleDockWidget() const override;

    ///@brief Returns true if the dock widget which has this title bar is floating
    bool isFloating() const;

    ///@brief the list of dockwidgets under this TitleBar.
    /// There should always be at least 1. If more than 1 then they are tabbed.
    DockWidgetBase::List dockWidgets() const;

    ///@brief returns whether this title bar supports a floating/docking button
    bool supportsFloatingButton() const;

    ///@brief returns whether this title bar supports a maximize/restore button
    bool supportsMaximizeButton() const;

    ///@brief returns whether this title bar supports a minimize button
    bool supportsMinimizeButton() const;

    ///@brief returns whether this title bar supports the auto-hide button
    bool supportsAutoHideButton() const;

    ///@brief returns whether this title bar has an icon
    bool hasIcon() const;

    ///@brief returns whether any of the DockWidgets this TitleBar controls has a child focus
    ///Not to be confused with QWidget::hasFocus(), which just refers to 1 widget. This works more
    /// like QtQuick's FocusScope
    bool isFocused() const;

    ///@brief the icon
    QIcon icon() const;

    ///@brief toggle floating
    Q_INVOKABLE bool onDoubleClicked();

    ///@brief getter for m_frame
    Frame *frame() const
    {
        return m_frame;
    }

    ///@brief getter for m_floatingWindow
    FloatingWindow *floatingWindow() const
    {
        return m_floatingWindow;
    }

    ///@brief If this title bar belongs to a dock widget docked into the main window, returns the main window
    ///Returns nullptr otherwise
    MainWindowBase *mainWindow() const;

    /// @brief Returns if this title bar is in a main window in MDI mode
    /// By default false. Only relevant if your main window was constructed with the
    /// MainWindowOption_MDI option
    bool isMDI() const override;

    /// @brief updates the close button enabled state
    void updateButtons();

Q_SIGNALS:
    void titleChanged();
    void iconChanged();
    void isFocusedChanged();
    void closeButtonEnabledChanged(bool);
    void floatButtonVisibleChanged(bool);
    void floatButtonToolTipChanged(const QString &);

protected:
    Q_INVOKABLE void onCloseClicked();
    Q_INVOKABLE void onFloatClicked();
    Q_INVOKABLE void onMaximizeClicked();
    Q_INVOKABLE void onMinimizeClicked();
    Q_INVOKABLE void toggleMaximized();
    Q_INVOKABLE void onAutoHideClicked();

    bool closeButtonEnabled() const;
    bool floatButtonVisible() const;
    QString floatButtonToolTip() const;

    virtual void updateMaximizeButton()
    {
    }
    virtual void updateMinimizeButton()
    {
    }
    virtual void updateAutoHideButton()
    {
    }

#ifdef DOCKS_DEVELOPER_MODE
    // The following are needed for the unit-tests
    virtual bool isCloseButtonVisible() const = 0;
    virtual bool isCloseButtonEnabled() const = 0;
    virtual bool isFloatButtonVisible() const = 0;
    virtual bool isFloatButtonEnabled() const = 0;
#endif

    void focusInEvent(QFocusEvent *event) override;
    bool isOverlayed() const;

private:
    friend class ::TestDocks;
    friend class FloatingWindowWidget;
    friend class TabWidgetWidget;
    void updateFloatButton();
    void updateCloseButton();
    void setCloseButtonEnabled(bool);
    void setFloatButtonVisible(bool);
    void setFloatButtonToolTip(const QString &);

    void init();

    QPoint m_pressPos;
    QString m_title;
    QIcon m_icon;

    Frame *const m_frame;
    FloatingWindow *const m_floatingWindow;
    const bool m_supportsAutoHide;
    bool m_closeButtonEnabled = true;
    bool m_floatButtonVisible = true;
    QString m_floatButtonToolTip;
};


}

#endif
