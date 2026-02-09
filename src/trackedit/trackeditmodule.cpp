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
#include "trackeditmodule.h"

#include <QtQml>

#include "framework/global/modularity/ioc.h"

#include "framework/ui/iuiactionsregister.h"
#include "framework/interactive/iinteractiveuriregister.h"

#include "internal/trackedituiactions.h"
#include "internal/trackeditactionscontroller.h"
#include "internal/trackeditinteraction.h"
#include "internal/trackeditconfiguration.h"
#include "internal/trackeditoperationcontroller.h"
#include "internal/tracknavigationcontroller.h"
#include "internal/trackspectrogramsettingsupdater.h"
#include "internal/undomanager.h"

#include "view/deletebehaviorpanelmodel.h"
#include "view/pastebehaviorpanelmodel.h"
#include "view/tracknavigationmodel.h"
#include "view/trackspectrogramsettingsmodel.h"

#include "internal/au3/au3trackeditproject.h"
#include "internal/au3/au3selectioncontroller.h"
#include "internal/au3/au3projecthistory.h"
#include "internal/au3/au3trackeditclipboard.h"

#include "internal/au3/au3tracksinteraction.h"
#include "internal/au3/au3clipsinteraction.h"
#include "internal/au3/au3labelsinteraction.h"

using namespace au::trackedit;
using namespace muse;
using namespace muse::modularity;
using namespace muse::ui;
using namespace muse::actions;

TrackeditModule::TrackeditModule()
{
}

static void trackedit_init_qrc()
{
    Q_INIT_RESOURCE(trackedit);
}

std::string TrackeditModule::moduleName() const
{
    return "trackedit";
}

void TrackeditModule::registerExports()
{
    m_trackeditController = std::make_shared<TrackeditActionsController>(iocContext());
    m_trackeditUiActions = std::make_shared<TrackeditUiActions>(iocContext(), m_trackeditController);
    m_selectionController = std::make_shared<Au3SelectionController>(iocContext());
    m_configuration = std::make_shared<TrackeditConfiguration>();
    m_trackNavigationController = std::make_shared<TrackNavigationController>(iocContext());
    m_trackSpectrogramSettingsUpdater = std::make_shared<TrackSpectrogramSettingsUpdater>(iocContext());

    ioc()->registerExport<ITrackeditProjectCreator>(moduleName(), new Au3TrackeditProjectCreator(iocContext()));
    ioc()->registerExport<ITrackeditInteraction>(moduleName(),
                                                 new TrackeditInteraction(iocContext(), std::make_unique<TrackeditOperationController>(
                                                                              iocContext(), std::make_unique<UndoManager>(iocContext()))));
    ioc()->registerExport<ISelectionController>(moduleName(), m_selectionController);
    ioc()->registerExport<IProjectHistory>(moduleName(), new Au3ProjectHistory(iocContext()));
    ioc()->registerExport<ITrackeditClipboard>(moduleName(), new Au3TrackeditClipboard(iocContext()));
    ioc()->registerExport<ITrackeditConfiguration>(moduleName(), m_configuration);
    ioc()->registerExport<ITrackNavigationController>(moduleName(), m_trackNavigationController);

    ioc()->registerExport<ITracksInteraction>(moduleName(), new Au3TracksInteraction(iocContext()));
    ioc()->registerExport<IClipsInteraction>(moduleName(), new Au3ClipsInteraction(iocContext()));
    ioc()->registerExport<ILabelsInteraction>(moduleName(), new Au3LabelsInteraction(iocContext()));
}

void TrackeditModule::registerUiTypes()
{
    qmlRegisterType<DeleteBehaviorPanelModel>("Audacity.TrackEdit", 1, 0, "DeleteBehaviorPanelModel");
    qmlRegisterType<PasteBehaviorPanelModel>("Audacity.TrackEdit", 1, 0, "PasteBehaviorPanelModel");
    qmlRegisterType<TrackNavigationModel>("Audacity.TrackEdit", 1, 0, "TrackNavigationModel");
    qmlRegisterType<TrackSpectrogramSettingsModel>("Audacity.TrackEdit", 1, 0, "TrackSpectrogramSettingsModel");
}

void TrackeditModule::resolveImports()
{
    auto ir = ioc()->resolve<muse::interactive::IInteractiveUriRegister>(moduleName());
    if (ir) {
        ir->registerQmlUri(muse::Uri("audacity://trackedit/custom_rate"), "Audacity/TrackEdit/CustomRateDialog.qml");
        ir->registerQmlUri(muse::Uri("audacity://trackedit/custom_time"), "Audacity/TrackEdit/CustomTimeDialog.qml");
        ir->registerQmlUri(muse::Uri("audacity://trackedit/delete_behavior"), "Audacity/TrackEdit/DeleteBehaviorOnboardingDialog.qml");
        ir->registerQmlUri(muse::Uri(
                               "audacity://trackedit/delete_behavior_followup"),
                           "Audacity/TrackEdit/DeleteBehaviorOnboardingFollowupDialog.qml");
        ir->registerQmlUri(muse::Uri(
                               "audacity://trackedit/track_spectrogram_settings"), "Audacity/TrackEdit/TrackSpectrogramSettingsDialog.qml");
    }
}

void TrackeditModule::registerResources()
{
    trackedit_init_qrc();
}

void TrackeditModule::onInit(const muse::IApplication::RunMode&)
{
    m_trackeditController->init();
    m_selectionController->init();
    m_configuration->init();
    m_trackNavigationController->init();
    m_trackSpectrogramSettingsUpdater->init();

    TimeSignatureRestorer::reg();

    m_trackeditUiActions->init();
    auto ar = ioc()->resolve<muse::ui::IUiActionsRegister>(moduleName());
    if (ar) {
        ar->reg(m_trackeditUiActions);
    }
}

void TrackeditModule::onDeinit()
{
}
