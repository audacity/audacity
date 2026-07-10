/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-Studio-CLA-applies
 *
 * MuseScore Studio
 * Music Composition & Notation
 *
 * Copyright (C) 2025 MuseScore Limited
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

#include "welcomedialogmodel.h"

#include "framework/actions/actiontypes.h"
#include "translation.h"
#include "log.h"

using namespace au::appshell;

namespace {
const char* AU4_VIDEO_URL {
    "https://youtu.be/QYM3TWf_G38?utm_source=au-app-au4-video&utm_medium=au-app-au4-video&utm_campaign=au-app-au4-video" };
const char* FEATURE_SURVEY_URL {"https://audacityteam.org/survey"};
}

std::vector<WelcomeDialogModel::Item> WelcomeDialogModel::buildItems()
{
    const char* audiocomUrl = muse::ui::isDarkTheme(uiConfiguration()->currentTheme().codeKey)
                              ? "qrc:/resources/welcomedialog/AudioDotCom_Dark.png"
                              : "qrc:/resources/welcomedialog/AudioDotCom_Light.png";

    return {
        {
            muse::qtrc("appshell/welcome", "Video: find out what’s new in Audacity 4"),
            "qrc:/resources/welcomedialog/Audacity40Video.png",
            {},
            muse::qtrc("appshell/welcome", "Watch video"),
            [this]() {
                platformInteractive()->openUrl(AU4_VIDEO_URL);
            }
        },
        {
            muse::qtrc("appshell/welcome", "Complete your Audacity cloud setup with Audio.com"),
            audiocomUrl,
            {},
            muse::qtrc("appshell/welcome", "Continue"),
            [this]() {
                muse::actions::ActionQuery query("audacity://cloud/show-tour-page");
                dispatcher()->dispatch(query);
            }
        },
        {
            muse::qtrc("appshell/welcome", "Explore free plugins for sculpting your audio"),
            "qrc:/resources/welcomedialog/MuseHubPromo.jpg",
            {},
            muse::qtrc("appshell/welcome", "View free plugins"),
            [this]() {
                dispatcher()->dispatch("get-effects");
            }
        },
        {
            muse::qtrc("appshell/welcome", "Help us decide the future of Audacity"),
            "qrc:/resources/welcomedialog/Audacity_Feature_Survey.png",
            {},
            muse::qtrc("appshell/welcome", "Take part in survey"),
            [this]() {
                platformInteractive()->openUrl(FEATURE_SURVEY_URL);
            }
        },
    };
}

WelcomeDialogModel::WelcomeDialogModel()
    : muse::Contextable(muse::iocCtxForQmlObject(this))
{
}

void WelcomeDialogModel::init()
{
    IF_ASSERT_FAILED(configuration()) {
        return;
    }

    m_items = buildItems();

    m_currentIndex = configuration()->welcomeDialogLastShownIndex();
    nextItem();

    IF_ASSERT_FAILED(m_currentIndex != muse::nidx) {
        m_currentIndex = 0;
    }
    configuration()->setWelcomeDialogLastShownIndex(static_cast<int>(m_currentIndex));

    uiConfiguration()->currentThemeChanged().onNotify(this, [this]() {
        m_items = buildItems();

        emit itemsChanged();
        emit currentItemChanged();
    });

    emit itemsChanged();
    emit currentItemChanged();
}

QVariantMap WelcomeDialogModel::currentItem() const
{
    if (m_items.empty()) {
        return QVariantMap();
    }

    const Item& item = m_items.at(m_currentIndex);
    return item.toQVariantMap();
}

void WelcomeDialogModel::activateCurrentItem()
{
    if (m_items.empty()) {
        return;
    }

    const auto& action = m_items.at(m_currentIndex).action;
    if (action) {
        action();
    }
}

void WelcomeDialogModel::nextItem()
{
    IF_ASSERT_FAILED(!m_items.empty()) {
        return;
    }

    if (hasNext()) {
        ++m_currentIndex;
    } else {
        // Cycle to first...
        m_currentIndex = 0;
    }
    configuration()->setWelcomeDialogLastShownIndex(static_cast<int>(m_currentIndex));

    emit currentItemChanged();
}

void WelcomeDialogModel::prevItem()
{
    IF_ASSERT_FAILED(!m_items.empty()) {
        return;
    }

    if (hasPrev()) {
        --m_currentIndex;
    } else {
        // Cycle to last....
        m_currentIndex = count() - 1;
    }
    configuration()->setWelcomeDialogLastShownIndex(static_cast<int>(m_currentIndex));

    emit currentItemChanged();
}

bool WelcomeDialogModel::showOnStartup() const
{
    return configuration()->welcomeDialogShowOnStartup();
}

void WelcomeDialogModel::setShowOnStartup(bool show)
{
    if (show == showOnStartup()) {
        return;
    }

    configuration()->setWelcomeDialogShowOnStartup(show);
    emit showOnStartupChanged();
}
