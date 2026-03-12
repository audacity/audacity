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
#include "firstlaunchsetupmodel.h"

#include "global/translation.h"
#include "global/async/async.h"

namespace {
const char* THEMES_PAGE = "ThemesPage.qml";
const char* CLIP_VISUALIZATION_PAGE = "ClipVisualizationPage.qml";
const char* WORKSPACE_LAYOUT_PAGE = "WorkspaceLayoutPage.qml";
const char* SIGNIN_AUDIO_COM_PAGE = "SigninAudiocomPage.qml";
const char* APP_UPDATES_AND_USAGE_INFO_PAGE = "AppUpdatesAndUsageInfoPage.qml";
}

using namespace muse;
using namespace au::appshell;

FirstLaunchSetupModel::FirstLaunchSetupModel(QObject* parent)
    : QObject(parent), muse::Injectable(muse::iocCtxForQmlObject(this))
{
    m_pages = {
        Page { THEMES_PAGE, "audacity://project" },
        Page { CLIP_VISUALIZATION_PAGE, "audacity://project" },
        Page { WORKSPACE_LAYOUT_PAGE, "audacity://project" },
        Page { SIGNIN_AUDIO_COM_PAGE, "audacity://project" },
        Page { APP_UPDATES_AND_USAGE_INFO_PAGE, "audacity://project" },
    };
}

void FirstLaunchSetupModel::load()
{
    setCurrentPageIndex(0);
}

int FirstLaunchSetupModel::numberOfPages() const
{
    return m_pages.size();
}

int FirstLaunchSetupModel::currentPageIndex() const
{
    return m_currentPageIndex;
}

QVariantMap FirstLaunchSetupModel::Page::toMap() const
{
    return {
        { "url", m_url },
    };
}

QVariantMap FirstLaunchSetupModel::currentPage() const
{
    if (m_currentPageIndex < 0 || m_currentPageIndex >= m_pages.size()) {
        return {};
    }

    return m_pages.at(m_currentPageIndex).toMap();
}

bool FirstLaunchSetupModel::canGoBack() const
{
    return m_currentPageIndex > 0;
}

bool FirstLaunchSetupModel::canGoForward() const
{
    return m_currentPageIndex < m_pages.size() - 1;
}

bool FirstLaunchSetupModel::canFinish() const
{
    return m_currentPageIndex == m_pages.size() - 1;
}

void FirstLaunchSetupModel::setCurrentPageIndex(int index)
{
    if (index == m_currentPageIndex || index < 0 || index >= m_pages.size()) {
        return;
    }

    m_currentPageIndex = index;
    emit currentPageChanged();
    emit nextButtonTextChanged();

    async::Async::call(this, [this]() {
        interactive()->open(m_pages.at(m_currentPageIndex).m_backgroundUri);
    });
}

QString FirstLaunchSetupModel::dialogTitle() const
{
    return muse::qtrc("appshell/gettingstarted", "Getting started");
}

QString FirstLaunchSetupModel::backButtonText() const
{
    return muse::qtrc("global", "Back");
}

QString FirstLaunchSetupModel::nextButtonText() const
{
    if (m_pages.at(m_currentPageIndex).m_url.contains(SIGNIN_AUDIO_COM_PAGE)) {
        return muse::qtrc("global", "Skip");
    }
    return !canFinish() ? muse::qtrc("global", "Next") : muse::qtrc("appshell/gettingstarted", "Accept & continue");
}

QString FirstLaunchSetupModel::formatPageProgress(int current, int total) const
{
    //: %1 is the current page number, %2 is the total number of pages
    return muse::qtrc("appshell/gettingstarted", "%1 of %2").arg(current).arg(total);
}

void FirstLaunchSetupModel::finish()
{
    configuration()->setHasCompletedFirstLaunchSetup(true);
}
