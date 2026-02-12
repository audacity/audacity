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

#include "translation.h"
#include "log.h"

using namespace au::appshell;

static std::vector<QVariantMap> welcomeDialogData()
{
    QVariantMap item1;
    item1.insert("title", muse::qtrc("appshell/welcome", "Tutorial: what's different in Audacity 4?"));
    item1.insert("imageUrl", "qrc:/resources/welcomedialog/Audacity40Video.png");
    item1.insert("description", muse::qtrc("appshell/welcome",
                                           "In this video, we walk you through the most important differences between version 3 and version 4."));
    item1.insert("buttonText", muse::qtrc("appshell/welcome", "Watch video"));
    item1.insert("destinationUrl",
                 "https://youtu.be/QYM3TWf_G38?utm_source=au-app-au4-video&utm_medium=au-app-au4-video&utm_campaign=au-app-au4-video");

    QVariantMap item2;
    item2.insert("title", muse::qtrc("appshell/welcome", "Complete your Audacity cloud setup with audio.com"));
    item2.insert("imageUrl", "qrc:/resources/welcomedialog/AudioDotCom_Promo.png");
    item2.insert("description", muse::qtrc("appshell/welcome",
                                           "This integration allows you to save and access your Audacity projects on any device"));
    item2.insert("buttonText", muse::qtrc("appshell/welcome", "Continue"));
    item2.insert("destinationUrl",
                 "https://audio.com/audacity/auth/sign-in?mtm_campaign=audacitydesktop&mtm_content=app_launch_popup");

    QVariantMap item3;
    item3.insert("title", muse::qtrc("appshell/welcome", "Explore free plugins for sculpting your audio"));
    item3.insert("imageUrl", "qrc:/resources/welcomedialog/MuseHubPromo.jpg");
    item3.insert("description", muse::qtrc("appshell/welcome",
                                           "There are tons of powerful plugins available that you can install for free on MuseHub"));
    item3.insert("buttonText", muse::qtrc("appshell/welcome", "View free plugins"));
    item3.insert("destinationUrl",
                 "");
    // TODO: button should lead to 'Get effects' panel

    QVariantMap item4;
    item4.insert("title", muse::qtrc("appshell/welcome", "Get 25th anniversary merchandise!"));
    item4.insert("imageUrl", "qrc:/resources/welcomedialog/Audacity_Merch_Store.png");
    item4.insert("description", muse::qtrc("appshell/welcome",
                                           "A collection of merchandise that commemorates Audacity's original appearance and branding"));
    item4.insert("buttonText", muse::qtrc("appshell/welcome", "Visit now"));
    item4.insert("destinationUrl",
                 "https://audacity-shop.fourthwall.com/en-gbp/?utm_source=au-app-merch-store&utm_medium=merch-25y&utm_campaign=au-app-welcome-au-app-merch-store-merch-25y&utm_id=au-app-welcome");

    return { item1, item2, item3, item4 };
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

    m_items = welcomeDialogData();

    m_currentIndex = configuration()->welcomeDialogLastShownIndex();
    nextItem();

    IF_ASSERT_FAILED(m_currentIndex != muse::nidx) {
        m_currentIndex = 0;
    }
    configuration()->setWelcomeDialogLastShownIndex(static_cast<int>(m_currentIndex));

    emit itemsChanged();
    emit currentItemChanged();
}

QVariantMap WelcomeDialogModel::currentItem() const
{
    if (m_items.empty()) {
        return QVariantMap();
    }
    return m_items.at(m_currentIndex);
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
