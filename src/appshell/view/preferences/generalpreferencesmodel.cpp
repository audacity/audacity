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
#include "generalpreferencesmodel.h"

#include "languages/languageserrors.h"

#include <QStorageInfo>

#include "log.h"
#include "translation.h"

using namespace au::appshell;
using namespace muse::languages;

GeneralPreferencesModel::GeneralPreferencesModel(QObject* parent)
    : QObject(parent)
{
}

void GeneralPreferencesModel::load()
{
    languagesConfiguration()->currentLanguageCode().ch.onReceive(this, [this](const QString& languageCode) {
        emit currentLanguageCodeChanged(languageCode);
    });

    setIsNeedRestart(languagesService()->needRestartToApplyLanguageChange());
    languagesService()->needRestartToApplyLanguageChangeChanged().onReceive(this, [this](bool need) {
        setIsNeedRestart(need);
    });

    projectConfiguration()->temporaryDirChanged().onReceive(this, [this](muse::io::path_t) {
        emit temporaryDirChanged();
    });
}

void GeneralPreferencesModel::checkUpdateForCurrentLanguage()
{
    QString languageCode = currentLanguageCode();

    m_languageUpdateProgress = languagesService()->update(languageCode);

    m_languageUpdateProgress.progressChanged().onReceive(this, [this](int64_t current, int64_t total, const std::string& status) {
        emit receivingUpdateForCurrentLanguage(current, total, QString::fromStdString(status));
    });

    m_languageUpdateProgress.finished().onReceive(this, [this, languageCode](const muse::ProgressResult& res) {
        if (res.ret.code() == static_cast<int>(Err::AlreadyUpToDate)) {
            QString msg = muse::qtrc("appshell/preferences", "Your version of %1 is up to date.")
                          .arg(languagesService()->language(languageCode).name);
            interactive()->info(msg.toStdString(), std::string());
        }
    });
}

QVariantList GeneralPreferencesModel::languages() const
{
    QList<Language> languages = languagesService()->languages().values();

    std::sort(languages.begin(), languages.end(), [](const Language& l, const Language& r) {
        return l.code < r.code;
    });

    QVariantList result;

    for (const Language& language : languages) {
        QVariantMap languageObj;
        languageObj["code"] = language.code;
        languageObj["name"] = language.name;
        result << languageObj;
    }

    if (languagesService()->hasPlaceholderLanguage()) {
        QVariantMap placeholderLanguageObj;
        placeholderLanguageObj["code"] = PLACEHOLDER_LANGUAGE_CODE;
        placeholderLanguageObj["name"] = "«Placeholder translations»";
        result.prepend(placeholderLanguageObj);
    }

    QVariantMap systemLanguageObj;
    systemLanguageObj["code"] = SYSTEM_LANGUAGE_CODE;
    systemLanguageObj["name"] = muse::qtrc("appshell/preferences", "System default");
    result.prepend(systemLanguageObj);

    return result;
}

QString GeneralPreferencesModel::currentLanguageCode() const
{
    return languagesConfiguration()->currentLanguageCode().val;
}

QStringList GeneralPreferencesModel::keyboardLayouts() const
{
    NOT_IMPLEMENTED;
    return { "US-QWERTY", "UK-QWERTY", "QWERTZ", "AZERTY" };
}

QString GeneralPreferencesModel::currentKeyboardLayout() const
{
    return shortcutsConfiguration()->currentKeyboardLayout();
}

bool GeneralPreferencesModel::isOSCRemoteControl() const
{
    return false;
}

int GeneralPreferencesModel::oscPort() const
{
    return 0;
}

void GeneralPreferencesModel::setCurrentLanguageCode(const QString& currentLanguageCode)
{
    if (currentLanguageCode == this->currentLanguageCode()) {
        return;
    }

    languagesConfiguration()->setCurrentLanguageCode(currentLanguageCode);
    emit currentLanguageCodeChanged(currentLanguageCode);
}

void GeneralPreferencesModel::setCurrentKeyboardLayout(const QString& keyboardLayout)
{
    if (keyboardLayout == this->currentKeyboardLayout()) {
        return;
    }

    shortcutsConfiguration()->setCurrentKeyboardLayout(keyboardLayout);
    emit currentKeyboardLayoutChanged();
}

void GeneralPreferencesModel::setIsOSCRemoteControl(bool isOSCRemoteControl)
{
    NOT_IMPLEMENTED;
    emit isOSCRemoteControlChanged(isOSCRemoteControl);
}

void GeneralPreferencesModel::setOscPort(int oscPort)
{
    NOT_IMPLEMENTED;
    emit oscPortChanged(oscPort);
}

bool GeneralPreferencesModel::isNeedRestart() const
{
    return m_isNeedRestart;
}

QString GeneralPreferencesModel::availableSpace() const
{
    QString path = projectConfiguration()->temporaryDir().toQString();
    QStorageInfo storage(path);

    QString msg = muse::qtrc("appshell/preferences", "%1 GB")
                  .arg(QString::number(storage.bytesAvailable() / (1024 * 1024 * 1024)));

    return msg;
}

void GeneralPreferencesModel::setIsNeedRestart(bool newIsNeedRestart)
{
    if (m_isNeedRestart == newIsNeedRestart) {
        return;
    }
    m_isNeedRestart = newIsNeedRestart;
    emit isNeedRestartChanged();
}

QString GeneralPreferencesModel::temporaryDir() const
{
    return projectConfiguration()->temporaryDir().toQString();
}

void GeneralPreferencesModel::setTemporaryDir(const QString& path)
{
    if (path == projectConfiguration()->temporaryDir().toQString()) {
        return;
    }

    auto newPath = muse::io::path_t(path);
    projectConfiguration()->setTemporaryDir(newPath);

    QString title = muse::qtrc("appshell/preferences", "Temp directory update");
    QString msg = muse::qtrc("appshell/preferences", "Changes to temporary directory will not take effect until Audacity is restarted");
    interactive()->info(title.toStdString(), msg.toStdString());
}

void GeneralPreferencesModel::setNumberFormat(int format)
{
    Q_UNUSED(format);
    NOT_IMPLEMENTED;
}
