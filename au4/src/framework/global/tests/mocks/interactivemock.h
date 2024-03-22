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
#ifndef MU_GLOBAL_INTERACTIVEMOCK_H
#define MU_GLOBAL_INTERACTIVEMOCK_H

#include <gmock/gmock.h>

#include "iinteractive.h"

namespace mu {
class InteractiveMock : public IInteractive
{
public:
    MOCK_METHOD(Result, question, (const std::string&, const std::string&, const Buttons&, const Button&, const Options&),
                (const, override));
    MOCK_METHOD(Result, question, (const std::string&, const Text&, const ButtonDatas&, int, const Options&), (const, override));

    MOCK_METHOD(ButtonData, buttonData, (Button), (const, override));

    MOCK_METHOD(Result, info, (const std::string&, const std::string&, const Buttons&, int, const Options&), (const, override));
    MOCK_METHOD(Result, info, (const std::string&, const Text&, const ButtonDatas&, int, const Options&), (const, override));

    MOCK_METHOD(Result, warning, (const std::string&, const std::string&, const Buttons&, const Button&, const Options&),
                (const, override));
    MOCK_METHOD(Result, warning, (const std::string&, const Text&, const ButtonDatas&, int, const Options&), (const, override));
    MOCK_METHOD(Result, warning, (const std::string&, const Text&, const std::string&, const ButtonDatas&, int, const Options&),
                (const, override));

    MOCK_METHOD(Result, error, (const std::string&, const std::string&, const Buttons&, const Button&, const Options&), (const, override));
    MOCK_METHOD(Result, error, (const std::string&, const Text&, const ButtonDatas&, int, const Options&), (const, override));
    MOCK_METHOD(Result, error, (const std::string&, const Text&, const std::string&, const ButtonDatas&, int, const Options&),
                (const, override));

    MOCK_METHOD(Ret, showProgress, (const std::string&, mu::Progress*), (const, override));

    MOCK_METHOD(io::path_t, selectOpeningFile, (const QString&, const io::path_t&, const std::vector<std::string>&), (override));
    MOCK_METHOD(io::path_t, selectSavingFile, (const QString&, const io::path_t&, const std::vector<std::string>&, bool), (override));
    MOCK_METHOD(io::path_t, selectDirectory, (const QString&, const io::path_t&), (override));
    MOCK_METHOD(io::paths_t, selectMultipleDirectories, (const QString&, const io::path_t&, const io::paths_t&), (override));
    MOCK_METHOD(QColor, selectColor, (const QColor&, const QString&), (override));

    MOCK_METHOD(RetVal<Val>, open, (const std::string&), (const, override));
    MOCK_METHOD(RetVal<Val>, open, (const Uri&), (const, override));
    MOCK_METHOD(RetVal<Val>, open, (const UriQuery&), (const, override));

    MOCK_METHOD(RetVal<bool>, isOpened, (const std::string&), (const, override));
    MOCK_METHOD(RetVal<bool>, isOpened, (const Uri&), (const, override));
    MOCK_METHOD(RetVal<bool>, isOpened, (const UriQuery&), (const, override));
    MOCK_METHOD(async::Channel<Uri>, opened, (), (const, override));

    MOCK_METHOD(void, raise, (const UriQuery&), (override));

    MOCK_METHOD(void, close, (const std::string&), (override));
    MOCK_METHOD(void, close, (const Uri&), (override));
    MOCK_METHOD(void, close, (const UriQuery&), (override));
    MOCK_METHOD(void, closeAllDialogs, (), (override));

    MOCK_METHOD(ValCh<Uri>, currentUri, (), (const, override));
    MOCK_METHOD(std::vector<Uri>, stack, (), (const, override));

    MOCK_METHOD(Ret, openUrl, (const std::string&), (const, override));
    MOCK_METHOD(Ret, openUrl, (const QUrl&), (const, override));

    MOCK_METHOD(Ret, revealInFileBrowser, (const io::path_t&), (const, override));
};
}

#endif // MU_GLOBAL_INTERACTIVEMOCK_H
