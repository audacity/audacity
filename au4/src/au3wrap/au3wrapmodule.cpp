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
#include "au3wrapmodule.h"

#include <wx/log.h>

#include "libraries/lib-preferences/Prefs.h"
#include "libraries/lib-audio-io/AudioIO.h"
#include "libraries/lib-project-file-io/ProjectFileIO.h"

#include "mocks/au3settingsmock.h"

#include "modularity/ioc.h"

#include "internal/wxlogwrap.h"
#include "internal/processinginteraction.h"
#include "internal/au3wavepainter.h"
#include "internal/au3playback.h"
#include "internal/au3record.h"
#include "internal/au3audiodevicesmanager.h"

#include "log.h"

using namespace au::au3;
using namespace muse::modularity;

std::string Au3WrapModule::moduleName() const
{
    return "au3wrap";
}

void Au3WrapModule::registerExports()
{
    m_playback = std::make_shared<Au3Playback>();
    m_record = std::make_shared<Au3Record>();

    m_audioDevicesManager = std::make_shared<Au3AudioDevicesManager>();

    ioc()->registerExport<playback::IPlayback>(moduleName(), m_playback);
    ioc()->registerExport<IAu3Record>(moduleName(), m_record);
    ioc()->registerExport<processing::IProcessingInteraction>(moduleName(), new ProcessingInteraction());
    ioc()->registerExport<IAu3WavePainter>(moduleName(), new Au3WavePainter());
}

void Au3WrapModule::onInit(const muse::IApplication::RunMode&)
{
    m_wxLog = new WxLogWrap();
    wxLog::SetActiveTarget(m_wxLog);

    std::unique_ptr<Au3SettingsMock> auset = std::make_unique<Au3SettingsMock>();
    InitPreferences(std::move(auset));

    AudioIO::Init();

    bool ok = ProjectFileIO::InitializeSQL();
    if (!ok) {
        LOGE() << "failed init sql";
    }

    m_record->init();

    m_audioDevicesManager->init();
}

void Au3WrapModule::onDeinit()
{
    wxLog::SetActiveTarget(nullptr);
    delete m_wxLog;
}
