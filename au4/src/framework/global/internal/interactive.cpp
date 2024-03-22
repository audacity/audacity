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
#include "interactive.h"

#include <QUrl>

#include <QFileDialog>
#include <QColorDialog>
#include <QMainWindow>

#include <QMessageBox>
#include <QPushButton>
#include <QMap>
#include <QSpacerItem>
#include <QGridLayout>
#include <QDesktopServices>

#ifdef Q_OS_MAC
#include "platform/macos/macosinteractivehelper.h"
#elif defined(Q_OS_WIN)
#include <QDir>
#include <QProcess>
#endif

#include "translation.h"
#include "io/path.h"

#include "log.h"

using namespace mu;

static IInteractive::Result standardDialogResult(const RetVal<Val>& retVal)
{
    if (!retVal.ret) {
        return IInteractive::Result(static_cast<int>(IInteractive::Button::Cancel));
    }

    QVariantMap resultMap = retVal.val.toQVariant().toMap();

    int btn = resultMap["buttonId"].toInt();
    bool showAgain = resultMap["showAgain"].toBool();
    return IInteractive::Result(btn, showAgain);
}

#ifndef Q_OS_LINUX
static QString filterToString(const std::vector<std::string>& filter)
{
    QStringList result;
    for (const std::string& nameFilter : filter) {
        result << QString::fromStdString(nameFilter);
    }

    return result.join(";;");
}

#endif

IInteractive::Result Interactive::question(const std::string& title, const std::string& text,
                                           const Buttons& buttons,
                                           const Button& def,
                                           const Options& options) const
{
    return question(title, Text(text), buttonDataList(buttons), int(def), options);
}

IInteractive::Result Interactive::question(const std::string& title, const Text& text, const ButtonDatas& btns, int defBtn,
                                           const Options& options) const
{
    return standardDialogResult(provider()->question(title, text, btns, defBtn, options));
}

IInteractive::ButtonData Interactive::buttonData(Button b) const
{
    constexpr bool accent = true;

    switch (b) {
    case IInteractive::Button::NoButton:    return ButtonData(int(b), "");
    case IInteractive::Button::Ok:          return ButtonData(int(b), trc("global", "OK"), accent);
    case IInteractive::Button::Save:        return ButtonData(int(b), trc("global", "Save"), accent);
    case IInteractive::Button::SaveAll:     return ButtonData(int(b), trc("global", "Save all"));
    case IInteractive::Button::DontSave:    return ButtonData(int(b), trc("global", "Don’t save"));
    case IInteractive::Button::Open:        return ButtonData(int(b), trc("global", "Open"));
    case IInteractive::Button::Yes:         return ButtonData(int(b), trc("global", "Yes"), accent);
    case IInteractive::Button::YesToAll:    return ButtonData(int(b), trc("global", "Yes to all"), accent);
    case IInteractive::Button::No:          return ButtonData(int(b), trc("global", "No"));
    case IInteractive::Button::NoToAll:     return ButtonData(int(b), trc("global", "No to all"));
    case IInteractive::Button::Abort:       return ButtonData(int(b), trc("global", "Abort"));
    case IInteractive::Button::Retry:       return ButtonData(int(b), trc("global", "Retry"));
    case IInteractive::Button::Ignore:      return ButtonData(int(b), trc("global", "Ignore"));
    case IInteractive::Button::Close:       return ButtonData(int(b), trc("global", "Close"));
    case IInteractive::Button::Cancel:      return ButtonData(int(b), trc("global", "Cancel"));
    case IInteractive::Button::Discard:     return ButtonData(int(b), trc("global", "Discard"));
    case IInteractive::Button::Help:        return ButtonData(int(b), trc("global", "Help"));
    case IInteractive::Button::Apply:       return ButtonData(int(b), trc("global", "Apply"));
    case IInteractive::Button::Reset:       return ButtonData(int(b), trc("global", "Reset"));
    case IInteractive::Button::Continue:    return ButtonData(int(b), trc("global", "Continue"));
    case IInteractive::Button::Next:
    case IInteractive::Button::Back:
    case IInteractive::Button::Select:
    case IInteractive::Button::Clear:
    case IInteractive::Button::Done:
    case IInteractive::Button::RestoreDefaults:
    case IInteractive::Button::CustomButton: break;
    }

    return ButtonData(int(b), "");
}

IInteractive::Result Interactive::info(const std::string& title, const std::string& text, const Buttons& buttons,
                                       int defBtn,
                                       const Options& options) const
{
    return standardDialogResult(provider()->info(title, text, buttonDataList(buttons), defBtn, options));
}

IInteractive::Result Interactive::info(const std::string& title, const Text& text, const ButtonDatas& buttons, int defBtn,
                                       const Options& options) const
{
    return standardDialogResult(provider()->info(title, text, buttons, defBtn, options));
}

Interactive::Result Interactive::warning(const std::string& title, const std::string& text, const Buttons& buttons, const Button& defBtn,
                                         const Options& options) const
{
    return standardDialogResult(provider()->warning(title, text, {}, buttonDataList(buttons), int(defBtn), options));
}

IInteractive::Result Interactive::warning(const std::string& title, const Text& text, const ButtonDatas& buttons,
                                          int defBtn,
                                          const Options& options) const
{
    return standardDialogResult(provider()->warning(title, text, {}, buttons, defBtn, options));
}

IInteractive::Result Interactive::warning(const std::string& title, const Text& text, const std::string& detailedText,
                                          const ButtonDatas& buttons, int defBtn,
                                          const Options& options) const
{
    return standardDialogResult(provider()->warning(title, text, detailedText, buttons, defBtn, options));
}

IInteractive::Result Interactive::error(const std::string& title, const std::string& text,
                                        const Buttons& buttons, const Button& defBtn,
                                        const Options& options) const
{
    return standardDialogResult(provider()->error(title, text, {}, buttonDataList(buttons), int(defBtn), options));
}

IInteractive::Result Interactive::error(const std::string& title, const Text& text,
                                        const ButtonDatas& buttons, int defBtn,
                                        const Options& options) const
{
    return standardDialogResult(provider()->error(title, text, {}, buttons, defBtn, options));
}

IInteractive::Result Interactive::error(const std::string& title, const Text& text, const std::string& detailedText,
                                        const ButtonDatas& buttons, int defBtn,
                                        const Options& options) const
{
    return standardDialogResult(provider()->error(title, text, detailedText, buttons, defBtn, options));
}

Ret Interactive::showProgress(const std::string& title, mu::Progress* progress) const
{
    return provider()->showProgress(title, progress);
}

mu::io::path_t Interactive::selectOpeningFile(const QString& title, const io::path_t& dir, const std::vector<std::string>& filter)
{
#ifndef Q_OS_LINUX
    QString result = QFileDialog::getOpenFileName(nullptr, title, dir.toQString(), filterToString(filter));
    return result;
#else
    return provider()->selectOpeningFile(title.toStdString(), dir, filter).val;
#endif
}

io::path_t Interactive::selectSavingFile(const QString& title, const io::path_t& path, const std::vector<std::string>& filter,
                                         bool confirmOverwrite)
{
#ifndef Q_OS_LINUX
    QFileDialog::Options options;
    options.setFlag(QFileDialog::DontConfirmOverwrite, !confirmOverwrite);
    QString result = QFileDialog::getSaveFileName(nullptr, title, path.toQString(), filterToString(filter), nullptr, options);
    return result;
#else
    return provider()->selectSavingFile(title.toStdString(), path, filter, confirmOverwrite).val;
#endif
}

io::path_t Interactive::selectDirectory(const QString& title, const io::path_t& dir)
{
#ifndef Q_OS_LINUX
    QString result = QFileDialog::getExistingDirectory(nullptr, title, dir.toQString());
    return result;
#else
    return provider()->selectDirectory(title.toStdString(), dir).val;
#endif
}

io::paths_t Interactive::selectMultipleDirectories(const QString& title, const io::path_t& dir, const io::paths_t& selectedDirectories)
{
    QString directoriesStr = QString::fromStdString(io::pathsToString(selectedDirectories));
    QStringList params = {
        "title=" + title,
        "selectedDirectories=" + directoriesStr,
        "startDir=" + dir.toQString()
    };

    RetVal<Val> result = open("musescore://interactive/selectMultipleDirectories?" + params.join("&").toStdString());
    if (!result.ret) {
        return selectedDirectories;
    }

    return io::pathsFromString(result.val.toQString().toStdString());
}

QColor Interactive::selectColor(const QColor& color, const QString& title)
{
    QColor selectedColor = QColorDialog::getColor(color, nullptr, title);
    return selectedColor.isValid() ? selectedColor : color;
}

RetVal<Val> Interactive::open(const std::string& uri) const
{
    return open(UriQuery(uri));
}

RetVal<Val> Interactive::open(const Uri& uri) const
{
    return open(UriQuery(uri));
}

RetVal<Val> Interactive::open(const UriQuery& uri) const
{
    UriQuery newQuery = uri;
    if (!newQuery.contains("sync")) {
        newQuery.addParam("sync", Val(true));
    }

    return provider()->open(newQuery);
}

RetVal<bool> Interactive::isOpened(const std::string& uri) const
{
    return provider()->isOpened(Uri(uri));
}

RetVal<bool> Interactive::isOpened(const Uri& uri) const
{
    return provider()->isOpened(uri);
}

RetVal<bool> Interactive::isOpened(const UriQuery& uri) const
{
    return provider()->isOpened(uri);
}

async::Channel<Uri> Interactive::opened() const
{
    return provider()->opened();
}

void Interactive::raise(const UriQuery& uri)
{
    provider()->raise(uri);
}

void Interactive::close(const std::string& uri)
{
    provider()->close(Uri(uri));
}

void Interactive::close(const Uri& uri)
{
    provider()->close(uri);
}

void Interactive::close(const UriQuery& uri)
{
    provider()->close(uri);
}

void Interactive::closeAllDialogs()
{
    provider()->closeAllDialogs();
}

ValCh<Uri> Interactive::currentUri() const
{
    return provider()->currentUri();
}

std::vector<Uri> Interactive::stack() const
{
    return provider()->stack();
}

Ret Interactive::openUrl(const std::string& url) const
{
    return openUrl(QUrl(QString::fromStdString(url)));
}

Ret Interactive::openUrl(const QUrl& url) const
{
    return QDesktopServices::openUrl(url);
}

Ret Interactive::revealInFileBrowser(const io::path_t& filePath) const
{
#ifdef Q_OS_MACOS
    if (MacOSInteractiveHelper::revealInFinder(filePath)) {
        return true;
    }
#elif defined(Q_OS_WIN)
    QString command = QLatin1String("explorer /select,%1").arg(QDir::toNativeSeparators(filePath.toQString()));
    if (QProcess::startDetached(command, QStringList())) {
        return true;
    }
#endif
    io::path_t dirPath = io::dirpath(filePath);
    return openUrl(QUrl::fromLocalFile(dirPath.toQString()));
}

IInteractive::ButtonDatas Interactive::buttonDataList(const Buttons& buttons) const
{
    ButtonDatas result;
    result.reserve(buttons.size());

    for (Button b : buttons) {
        result.push_back(buttonData(b));
    }

    return result;
}
