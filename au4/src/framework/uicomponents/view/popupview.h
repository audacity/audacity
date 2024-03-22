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

#ifndef MU_UICOMPONENTS_POPUPVIEW_H
#define MU_UICOMPONENTS_POPUPVIEW_H

#include <QQuickItem>
#include <QQmlParserStatus>

#include "types/ret.h"
#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "ui/imainwindow.h"
#include "ui/iuiconfiguration.h"
#include "ui/inavigationcontroller.h"
#include "ui/view/navigationcontrol.h"

#include "popupwindow/ipopupwindow.h"
#include "internal/popupviewclosecontroller.h"

#ifndef MU_QT5_COMPAT
Q_MOC_INCLUDE(< QWindow >)
#endif

class QQuickCloseEvent;

namespace mu::uicomponents {
class PopupView : public QObject, public QQmlParserStatus, async::Asyncable
{
    Q_OBJECT
    Q_INTERFACES(QQmlParserStatus)

    Q_PROPERTY(QQuickItem * parent READ parentItem WRITE setParentItem NOTIFY parentItemChanged)

    Q_PROPERTY(QQuickItem * contentItem READ contentItem WRITE setContentItem NOTIFY contentItemChanged)
    Q_PROPERTY(int contentWidth READ contentWidth WRITE setContentWidth NOTIFY contentWidthChanged)
    Q_PROPERTY(int contentHeight READ contentHeight WRITE setContentHeight NOTIFY contentHeightChanged)

    Q_PROPERTY(QWindow * window READ window NOTIFY windowChanged)

    //! NOTE Local, related parent
    Q_PROPERTY(qreal x READ localX WRITE setLocalX NOTIFY xChanged)
    Q_PROPERTY(qreal y READ localY WRITE setLocalY NOTIFY yChanged)

    Q_PROPERTY(bool showArrow READ showArrow WRITE setShowArrow NOTIFY showArrowChanged)
    Q_PROPERTY(int padding READ padding WRITE setPadding NOTIFY paddingChanged)

    Q_PROPERTY(QQuickItem * anchorItem READ anchorItem WRITE setAnchorItem NOTIFY anchorItemChanged)
    Q_PROPERTY(bool opensUpward READ opensUpward NOTIFY opensUpwardChanged)
    Q_PROPERTY(int arrowX READ arrowX WRITE setArrowX NOTIFY arrowXChanged)

    Q_PROPERTY(bool isOpened READ isOpened NOTIFY isOpenedChanged)
    Q_PROPERTY(OpenPolicies openPolicies READ openPolicies WRITE setOpenPolicies NOTIFY openPoliciesChanged)

    Q_PROPERTY(bool isContentReady READ isContentReady WRITE setIsContentReady NOTIFY isContentReadyChanged)

    Q_PROPERTY(ClosePolicies closePolicies READ closePolicies WRITE setClosePolicies NOTIFY closePoliciesChanged)

    Q_PROPERTY(
        bool activateParentOnClose READ activateParentOnClose WRITE setActivateParentOnClose NOTIFY activateParentOnCloseChanged)

    //! NOTE Used for dialogs, but be here so that dialogs and just popups have one api
    Q_PROPERTY(QString title READ title WRITE setTitle NOTIFY titleChanged)
    Q_PROPERTY(QString objectId READ objectId WRITE setObjectId NOTIFY objectIdChanged)
    Q_PROPERTY(bool modal READ modal WRITE setModal NOTIFY modalChanged)
    Q_PROPERTY(bool frameless READ frameless WRITE setFrameless NOTIFY framelessChanged)
    Q_PROPERTY(bool resizable READ resizable WRITE setResizable NOTIFY resizableChanged)
    Q_PROPERTY(bool alwaysOnTop READ alwaysOnTop WRITE setAlwaysOnTop NOTIFY alwaysOnTopChanged)
    Q_PROPERTY(QVariantMap ret READ ret WRITE setRet NOTIFY retChanged)

    INJECT(ui::IMainWindow, mainWindow)
    INJECT(ui::IUiConfiguration, uiConfiguration)
    INJECT(ui::INavigationController, navigationController)

public:

    explicit PopupView(QQuickItem* parent = nullptr);
    ~PopupView() override;

    enum OpenPolicy {
        Default = 0x00000000,
        NoActivateFocus = 0x00000001,
        OpenOnContentReady = 0x00000002
    };
    Q_DECLARE_FLAGS(OpenPolicies, OpenPolicy)
    Q_FLAG(OpenPolicies)

    enum ClosePolicy {
        NoAutoClose = 0x00000000,
        CloseOnPressOutsideParent = 0x00000001,
    };
    Q_DECLARE_FLAGS(ClosePolicies, ClosePolicy)
    Q_FLAG(ClosePolicies)

    QQuickItem* parentItem() const;

    QQuickItem* contentItem() const;
    int contentWidth() const;
    int contentHeight() const;

    QWindow* window() const;

    qreal localX() const;
    qreal localY() const;
    QRect geometry() const;

    Q_INVOKABLE void forceActiveFocus();

    void init();

    Q_INVOKABLE void open();
    Q_INVOKABLE void close(bool force = false);
    Q_INVOKABLE void toggleOpened();

    Q_INVOKABLE void setParentWindow(QWindow* window);

    Q_INVOKABLE QRectF anchorGeometry() const;

    OpenPolicies openPolicies() const;
    ClosePolicies closePolicies() const;

    bool activateParentOnClose() const;

    ui::INavigationControl* navigationParentControl() const;

    bool isOpened() const;

    QString objectId() const;
    QString title() const;
    bool modal() const;
    bool frameless() const;
    bool resizable() const;
    bool alwaysOnTop() const;
    QVariantMap ret() const;

    bool opensUpward() const;
    int arrowX() const;
    int padding() const;
    bool showArrow() const;
    QQuickItem* anchorItem() const;

    bool isContentReady() const;
    void setIsContentReady(bool ready);

public slots:
    void setParentItem(QQuickItem* parent);
    void setEngine(QQmlEngine* engine);
    void setComponent(QQmlComponent* component);
    void setContentItem(QQuickItem* content);
    void setContentWidth(int contentWidth);
    void setContentHeight(int contentHeight);
    void setLocalX(qreal x);
    void setLocalY(qreal y);
    void setOpenPolicies(mu::uicomponents::PopupView::OpenPolicies openPolicies);
    void setClosePolicies(mu::uicomponents::PopupView::ClosePolicies closePolicies);
    void setNavigationParentControl(ui::INavigationControl* parentNavigationControl);
    void setObjectId(QString objectId);
    void setTitle(QString title);
    void setModal(bool modal);
    void setFrameless(bool frameless);
    void setResizable(bool resizable);
    void setAlwaysOnTop(bool alwaysOnTop);
    void setRet(QVariantMap ret);

    void setOpensUpward(bool opensUpward);
    void setArrowX(int arrowX);
    void setPadding(int padding);
    void setShowArrow(bool showArrow);
    void setAnchorItem(QQuickItem* anchorItem);

    void setActivateParentOnClose(bool activateParentOnClose);

signals:
    void parentItemChanged();
    void contentItemChanged();
    void contentWidthChanged();
    void contentHeightChanged();
    void windowChanged();
    void xChanged(qreal x);
    void yChanged(qreal y);
    void openPoliciesChanged(mu::uicomponents::PopupView::OpenPolicies openPolicies);
    void closePoliciesChanged(mu::uicomponents::PopupView::ClosePolicies closePolicies);
    void navigationParentControlChanged(ui::INavigationControl* navigationParentControl);
    void objectIdChanged(QString objectId);
    void titleChanged(QString title);
    void modalChanged(bool modal);
    void framelessChanged(bool frameless);
    void resizableChanged(bool resizable);
    void alwaysOnTopChanged();
    void retChanged(QVariantMap ret);

    void isOpenedChanged();
    void opened();
    void aboutToClose(QQuickCloseEvent* closeEvent);
    void closed(bool force);

    void opensUpwardChanged(bool opensUpward);
    void arrowXChanged(int arrowX);
    void paddingChanged(int padding);
    void showArrowChanged(bool showArrow);
    void anchorItemChanged(QQuickItem* anchorItem);

    void activateParentOnCloseChanged(bool activateParentOnClose);

    void isContentReadyChanged();

protected:
    virtual bool isDialog() const;
    void classBegin() override;
    void componentComplete() override;
    bool eventFilter(QObject* watched, QEvent* event) override;

    void initCloseController();

    void doFocusOut();
    void windowMoveEvent();

    bool isMouseWithinBoundaries(const QPoint& mousePos) const;

    void doOpen();

    QWindow* qWindow() const;
    virtual void onHidden();

    void repositionWindowIfNeed();

    void setErrCode(Ret::Code code);

    virtual QScreen* resolveScreen() const;
    QRect currentScreenGeometry() const;
    virtual void updateGeometry();
    void updateContentPosition();

    virtual QRect viewGeometry() const;

    void resolveNavigationParentControl();
    void activateNavigationParentControl();

    QQmlEngine* engine() const;

    IPopupWindow* m_window = nullptr;

    QQmlComponent* m_component = nullptr;
    QQmlEngine* m_engine = nullptr;

    QQuickItem* m_contentItem = nullptr;
    int m_contentWidth = 0;
    int m_contentHeight = 0;

    QQuickItem* m_anchorItem = nullptr;

    QPointF m_localPos;
    QPointF m_globalPos;

    OpenPolicies m_openPolicies = { OpenPolicy::Default };
    bool m_shouldOpenOnReady = false;
    bool m_isContentReady = false;

    ClosePolicies m_closePolicies = { ClosePolicy::CloseOnPressOutsideParent };

    bool m_activateParentOnClose = true;
    ui::INavigationControl* m_navigationParentControl = nullptr;
    QString m_objectId;
    QString m_title;
    bool m_modal = true;
    bool m_frameless = false;
    bool m_resizable = false;
    bool m_alwaysOnTop = false;
    QVariantMap m_ret;
    bool m_opensUpward = false;
    int m_arrowX = 0;
    int m_padding = 0;
    bool m_showArrow = false;

    PopupViewCloseController* m_closeController = nullptr;
    bool m_forceClosed = false;
};
}

#endif // MU_UICOMPONENTS_POPUPVIEW_H
