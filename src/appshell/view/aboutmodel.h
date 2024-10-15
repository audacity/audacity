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
#ifndef MU_APPSHELL_ABOUTMODEL_H
#define MU_APPSHELL_ABOUTMODEL_H

#include <QObject>

#include "modularity/ioc.h"
#include "iappshellconfiguration.h"
#include "global/iglobalconfiguration.h"
#include "global/iapplication.h"

//! TODO AU4
//#include "update/iupdateconfiguration.h"

class QUrl;

namespace au::appshell {
class AboutModel : public QObject
{
    Q_OBJECT

    muse::Inject<IAppShellConfiguration> configuration;
    muse::Inject<muse::IGlobalConfiguration> globalConfiguration;
    muse::Inject<muse::IApplication> application;

//! TODO AU4
//     muse::Inject<update::IUpdateConfiguration> updateConfiguration;

public:
    explicit AboutModel(QObject* parent = nullptr);

    Q_INVOKABLE QString museScoreVersion() const;
    Q_INVOKABLE QString museScoreRevision() const;
    Q_INVOKABLE QVariantMap museScoreUrl() const;
    Q_INVOKABLE QVariantMap museScoreForumUrl() const;
    Q_INVOKABLE QVariantMap museScoreContributionUrl() const;
    Q_INVOKABLE QVariantMap museScorePrivacyPolicyUrl() const;

    Q_INVOKABLE QVariantMap musicXMLLicenseUrl() const;
    Q_INVOKABLE QVariantMap musicXMLLicenseDeedUrl() const;

    Q_INVOKABLE void copyRevisionToClipboard() const;

    Q_INVOKABLE void toggleDevMode();

private:
    QVariantMap makeUrl(const QUrl& url, bool showPath = true) const;
};
}

#endif // MU_APPSHELL_ABOUTMODEL_H
