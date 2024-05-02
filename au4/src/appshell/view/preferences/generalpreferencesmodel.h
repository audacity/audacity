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
#ifndef AU_APPSHELL_GENERALPREFERENCESMODEL_H
#define AU_APPSHELL_GENERALPREFERENCESMODEL_H

#include <QObject>

#include "progress.h"

#include "modularity/ioc.h"
#include "iappshellconfiguration.h"
#include "async/asyncable.h"

#include "global/iinteractive.h"
#include "languages/ilanguagesconfiguration.h"
#include "languages/ilanguagesservice.h"
#include "shortcuts/ishortcutsconfiguration.h"
#include "project/iprojectconfiguration.h"

namespace au::appshell {
class GeneralPreferencesModel : public QObject, public async::Asyncable
{
    Q_OBJECT

    INJECT(IAppShellConfiguration, configuration)
    INJECT(IInteractive, interactive)
    INJECT(languages::ILanguagesConfiguration, languagesConfiguration)
    INJECT(languages::ILanguagesService, languagesService)
    INJECT(shortcuts::IShortcutsConfiguration, shortcutsConfiguration)

    Q_PROPERTY(QVariantList languages READ languages NOTIFY languagesChanged)
    Q_PROPERTY(QString currentLanguageCode READ currentLanguageCode WRITE setCurrentLanguageCode NOTIFY currentLanguageCodeChanged)

    Q_PROPERTY(QStringList keyboardLayouts READ keyboardLayouts CONSTANT)
    Q_PROPERTY(QString currentKeyboardLayout READ currentKeyboardLayout WRITE setCurrentKeyboardLayout NOTIFY currentKeyboardLayoutChanged)

    Q_PROPERTY(bool isOSCRemoteControl READ isOSCRemoteControl WRITE setIsOSCRemoteControl NOTIFY isOSCRemoteControlChanged)
    Q_PROPERTY(int oscPort READ oscPort WRITE setOscPort NOTIFY oscPortChanged)

    Q_PROPERTY(bool isNeedRestart READ isNeedRestart WRITE setIsNeedRestart NOTIFY isNeedRestartChanged)

    Q_PROPERTY(QVariantList startupModes READ startupModes NOTIFY startupModesChanged)

public:
    explicit GeneralPreferencesModel(QObject* parent = nullptr);

    Q_INVOKABLE void load();
    Q_INVOKABLE void checkUpdateForCurrentLanguage();

    Q_INVOKABLE void setCurrentStartupMode(int modeIndex);
    Q_INVOKABLE void setStartupScorePath(const QString& scorePath);

    Q_INVOKABLE QStringList scorePathFilter() const;

    QVariantList languages() const;
    QString currentLanguageCode() const;

    QStringList keyboardLayouts() const;
    QString currentKeyboardLayout() const;

    QVariantList startupModes() const;

    bool isOSCRemoteControl() const;
    int oscPort() const;
    bool isNeedRestart() const;

public slots:
    void setCurrentLanguageCode(const QString& currentLanguageCode);
    void setCurrentKeyboardLayout(const QString& keyboardLayout);
    void setIsOSCRemoteControl(bool isOSCRemoteControl);
    void setOscPort(int oscPort);
    void setIsNeedRestart(bool newIsNeedRestart);

signals:
    void languagesChanged(QVariantList languages);
    void currentLanguageCodeChanged(QString currentLanguageCode);
    void currentKeyboardLayoutChanged();
    void isOSCRemoteControlChanged(bool isOSCRemoteControl);
    void oscPortChanged(int oscPort);

    void receivingUpdateForCurrentLanguage(int current, int total, QString status);
    void isNeedRestartChanged();

    void startupModesChanged();

private:
    mu::Progress m_languageUpdateProgress;

    bool m_isNeedRestart = false;

    struct StartMode
    {
        StartupModeType type = StartupModeType::StartWithNewScore;
        QString title;
        bool checked = false;
        bool canSelectScorePath = false;
        QString scorePath;
    };

    using StartModeList = QList<StartMode>;

    StartModeList allStartupModes() const;
};
}

#endif // AU_APPSHELL_GENERALPREFERENCESMODEL_H
