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

#ifndef MU_NOTATION_NOTATIONSTATUSBARMODEL_H
#define MU_NOTATION_NOTATIONSTATUSBARMODEL_H

#include <QObject>

#include "async/asyncable.h"
#include "actions/actionable.h"
#include "uicomponents/view/menuitem.h"

#include "modularity/ioc.h"
#include "actions/iactionsdispatcher.h"
#include "ui/iuiactionsregister.h"
#include "workspace/iworkspaceconfiguration.h"
#include "notation/inotationconfiguration.h"
#include "context/iglobalcontext.h"

#include "notation/notationtypes.h"

#include "global/iglobalconfiguration.h"

namespace au::appshell {
class NotationStatusBarModel : public QObject, public async::Asyncable, public actions::Actionable
{
    Q_OBJECT

    INJECT(context::IGlobalContext, context)
    INJECT(actions::IActionsDispatcher, dispatcher)
    INJECT(ui::IUiActionsRegister, actionsRegister)
    INJECT(workspace::IWorkspaceConfiguration, workspaceConfiguration)
    INJECT(notation::INotationConfiguration, notationConfiguration)
    INJECT(IGlobalConfiguration, globalConfiguration)

    Q_PROPERTY(QString accessibilityInfo READ accessibilityInfo NOTIFY accessibilityInfoChanged)
    Q_PROPERTY(QVariant currentWorkspaceItem READ currentWorkspaceItem NOTIFY currentWorkspaceActionChanged)
    Q_PROPERTY(QVariant concertPitchItem READ concertPitchItem NOTIFY concertPitchActionChanged)
    Q_PROPERTY(QVariant currentViewMode READ currentViewMode NOTIFY currentViewModeChanged)
    Q_PROPERTY(bool zoomEnabled READ zoomEnabled NOTIFY zoomEnabledChanged)
    Q_PROPERTY(QVariantList availableViewModeList READ availableViewModeList_property NOTIFY availableViewModeListChanged)
    Q_PROPERTY(QVariantList availableZoomList READ availableZoomList_property NOTIFY availableZoomListChanged)
    Q_PROPERTY(int currentZoomPercentage READ currentZoomPercentage WRITE setCurrentZoomPercentage NOTIFY currentZoomPercentageChanged)

public:
    explicit NotationStatusBarModel(QObject* parent = nullptr);

    QString accessibilityInfo() const;
    QVariant currentWorkspaceItem();
    QVariant concertPitchItem();
    QVariant currentViewMode();
    bool zoomEnabled() const;
    int currentZoomPercentage() const;
    uicomponents::MenuItemList makeAvailableViewModeList();
    uicomponents::MenuItemList makeAvailableZoomList();

    Q_INVOKABLE void load();

    Q_INVOKABLE void selectWorkspace();
    Q_INVOKABLE void toggleConcertPitch();
    Q_INVOKABLE void setCurrentViewMode(const QString& modeCode);

    Q_INVOKABLE int minZoomPercentage() const;
    Q_INVOKABLE int maxZoomPercentage() const;
    Q_INVOKABLE void setCurrentZoom(const QString& zoomId);
    Q_INVOKABLE void zoomIn();
    Q_INVOKABLE void zoomOut();

    Q_INVOKABLE void handleAction(const QString& actionCode);

public slots:
    void setCurrentZoomPercentage(int zoomPercentage);

signals:
    void accessibilityInfoChanged();
    void currentWorkspaceActionChanged();
    void concertPitchActionChanged();
    void currentViewModeChanged();
    void availableViewModeListChanged();
    void zoomEnabledChanged();
    void availableZoomListChanged();
    void currentZoomPercentageChanged();

private:
    notation::INotationPtr notation() const;
    notation::INotationAccessibilityPtr accessibility() const;

    uicomponents::MenuItem* makeMenuItem(const actions::ActionCode& actionCode);

    void dispatch(const actions::ActionCode& code, const actions::ActionData& args = actions::ActionData());

    void onCurrentNotationChanged();

    notation::ZoomType currentZoomType() const;

    void listenChangesInAccessibility();

    QList<int> possibleZoomPercentageList() const;

    QVariantList availableViewModeList_property();
    QVariantList availableZoomList_property();

    QVariantList menuItemListToVariantList(const uicomponents::MenuItemList& list) const;
};
}

#endif // MU_NOTATION_NOTATIONSTATUSBARMODEL_H
