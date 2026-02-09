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
#ifndef AU_APPSHELL_FIRSTLAUNCHSETUPMODEL_H
#define AU_APPSHELL_FIRSTLAUNCHSETUPMODEL_H

#include <QObject>
#include <QtQml/qqmlregistration.h>

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"
#include "framework/interactive/iinteractive.h"

#include "appshell/iappshellconfiguration.h"

namespace au::appshell {
class FirstLaunchSetupModel : public QObject, public muse::async::Asyncable, public muse::Injectable
{
    Q_OBJECT
    QML_ELEMENT

    Q_PROPERTY(int numberOfPages READ numberOfPages CONSTANT)
    Q_PROPERTY(int currentPageIndex READ currentPageIndex WRITE setCurrentPageIndex NOTIFY currentPageChanged)
    Q_PROPERTY(QVariantMap currentPage READ currentPage NOTIFY currentPageChanged)

    Q_PROPERTY(bool canGoBack READ canGoBack NOTIFY currentPageChanged)
    Q_PROPERTY(bool canGoForward READ canGoForward NOTIFY currentPageChanged)
    Q_PROPERTY(bool canFinish READ canFinish NOTIFY currentPageChanged)
    Q_PROPERTY(QString dialogTitle READ dialogTitle CONSTANT)
    Q_PROPERTY(QString backButtonText READ backButtonText CONSTANT)
    Q_PROPERTY(QString nextButtonText READ nextButtonText CONSTANT)
    Q_PROPERTY(QString doneButtonText READ doneButtonText CONSTANT)

    muse::GlobalInject<IAppShellConfiguration> configuration;

    muse::Inject<muse::IInteractive> interactive { this };

public:
    explicit FirstLaunchSetupModel(QObject* parent = nullptr);

    Q_INVOKABLE void load();

    int numberOfPages() const;
    int currentPageIndex() const;
    QVariantMap currentPage() const;

    bool canGoBack() const;
    bool canGoForward() const;
    bool canFinish() const;

    static QString dialogTitle();
    static QString backButtonText();
    static QString nextButtonText();
    static QString doneButtonText();
    Q_INVOKABLE QString formatPageProgress(int current, int total) const;

    Q_INVOKABLE void finish();

public slots:
    void setCurrentPageIndex(int index);

signals:
    void currentPageChanged();

private:
    struct Page {
        QString m_url;
        std::string m_backgroundUri;

        [[nodiscard]] QVariantMap toMap() const;
    };

    QList<Page> m_pages;
    int m_currentPageIndex = -1;
};
}

#endif // AU_APPSHELL_FIRSTLAUNCHSETUPMODEL_H
