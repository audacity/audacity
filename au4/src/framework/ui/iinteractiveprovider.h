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
#ifndef MU_UI_IINTERACTIVEPROVIDER_H
#define MU_UI_IINTERACTIVEPROVIDER_H

#include "global/modularity/imoduleinterface.h"
#include "global/iinteractive.h"
#include "global/types/uri.h"
#include "global/types/retval.h"
#include "global/progress.h"

class QWindow;

namespace mu::ui {
class IInteractiveProvider : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(ILaunchProvider)

public:
    virtual ~IInteractiveProvider() = default;

    virtual RetVal<Val> question(const std::string& title, const IInteractive::Text& text, const IInteractive::ButtonDatas& buttons,
                                 int defBtn = int(IInteractive::Button::NoButton), const IInteractive::Options& options = {}) = 0;

    virtual RetVal<Val> info(const std::string& title, const IInteractive::Text& text, const IInteractive::ButtonDatas& buttons,
                             int defBtn = int(IInteractive::Button::NoButton), const IInteractive::Options& options = {}) = 0;

    virtual RetVal<Val> warning(const std::string& title, const IInteractive::Text& text, const std::string& detailedText = {},
                                const IInteractive::ButtonDatas& buttons = {}, int defBtn = int(IInteractive::Button::NoButton),
                                const IInteractive::Options& options = {}) = 0;

    virtual RetVal<Val> error(const std::string& title, const IInteractive::Text& text, const std::string& detailedText = {},
                              const IInteractive::ButtonDatas& buttons = {}, int defBtn = int(IInteractive::Button::NoButton),
                              const IInteractive::Options& options = {}) = 0;

    virtual Ret showProgress(const std::string& title, mu::Progress* progress) = 0;

    virtual RetVal<io::path_t> selectOpeningFile(const std::string& title, const io::path_t& dir,
                                                 const std::vector<std::string>& filter) = 0;
    virtual RetVal<io::path_t> selectSavingFile(const std::string& title, const io::path_t& path, const std::vector<std::string>& filter,
                                                bool confirmOverwrite) = 0;
    virtual RetVal<io::path_t> selectDirectory(const std::string& title, const io::path_t& dir) = 0;

    virtual RetVal<Val> open(const UriQuery& uri) = 0;
    virtual RetVal<bool> isOpened(const Uri& uri) const = 0;
    virtual RetVal<bool> isOpened(const UriQuery& uri) const = 0;
    virtual async::Channel<Uri> opened() const = 0;

    virtual void raise(const UriQuery& uri) = 0;

    virtual void close(const Uri& uri) = 0;
    virtual void close(const UriQuery& uri) = 0;
    virtual void closeAllDialogs() = 0;

    virtual ValCh<Uri> currentUri() const = 0;
    virtual async::Notification currentUriAboutToBeChanged() const = 0;
    virtual std::vector<Uri> stack() const = 0;

    virtual QWindow* topWindow() const = 0;
    virtual bool topWindowIsWidget() const = 0;
};
}

#endif // MU_UI_IINTERACTIVEPROVIDER_H
