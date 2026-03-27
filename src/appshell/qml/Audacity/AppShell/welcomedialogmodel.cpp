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
const char* SHOP_URL {
    "https://audacity-shop.fourthwall.com/en-gbp/?utm_source=au-app-merch-store&utm_medium=merch-25y&utm_campaign=au-app-welcome-au-app-merch-store-merch-25y&utm_id=au-app-welcome" };
}

std::vector<WelcomeDialogModel::Item> WelcomeDialogModel::buildItems()
{
    const char* audiocomUrl = muse::ui::isDarkTheme(uiConfiguration()->currentTheme().codeKey)
                              ? "qrc:/resources/welcomedialog/AudioDotCom_Dark.png"
                              : "qrc:/resources/welcomedialog/AudioDotCom_Light.png";

    return {
        {
            muse::qtrc("appshell/welcome", "Tutorial: what’s different in Audacity 4?"),
            "qrc:/resources/welcomedialog/Audacity40Video.png",
            muse::qtrc("appshell/welcome",
                       "In this video, we walk you through the most important differences between version 3 and version 4."),
            muse::qtrc("appshell/welcome", "Watch video"),
            [this]() {
                platformInteractive()->openUrl(AU4_VIDEO_URL);
            }
        },
        {
            muse::qtrc("appshell/welcome", "Complete your Audacity cloud setup with Audio.com"),
            audiocomUrl,
            muse::qtrc("appshell/welcome",
                       "This integration allows you to save and access your Audacity projects on any device"),
            muse::qtrc("appshell/welcome", "Continue"),
            [this]() {
                muse::actions::ActionQuery query("audacity://cloud/open-signin-dialog");
                query.addParam("showTourPage", muse::Val(true));

                dispatcher()->dispatch(query);
            }
        },
        {
            muse::qtrc("appshell/welcome", "Explore free plugins for sculpting your audio"),
            "qrc:/resources/welcomedialog/MuseHubPromo.jpg",
            muse::qtrc("appshell/welcome",
                       "There are tons of powerful plugins available that you can install for free on MuseHub"),
            muse::qtrc("appshell/welcome", "View free plugins"),
            [this]() {
                dispatcher()->dispatch("get-effects");
            }
        },
        {
            muse::qtrc("appshell/welcome", "Get 25th anniversary merchandise!"),
            "qrc:/resources/welcomedialog/Audacity_Merch_Store.png",
            muse::qtrc("appshell/welcome",
                       "A collection of merchandise that commemorates Audacity’s original appearance and branding"),
            muse::qtrc("appshell/welcome", "Visit now"),
            [this]() {
                platformInteractive()->openUrl(SHOP_URL);
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
