/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <gmock/gmock.h>

#include "iinteractive.h"

namespace au::trackedit {
class InteractiveMock : public muse::IInteractive
{
public:
    MOCK_METHOD(muse::IInteractive::Result, question,
                (const std::string&, const std::string&, const muse::IInteractive::Buttons&,
                 const muse::IInteractive::Button&, const muse::IInteractive::Options&, const std::string&),
                (const, override));
    MOCK_METHOD(muse::IInteractive::Result, question,
                (const std::string&, const muse::IInteractive::Text&, const muse::IInteractive::ButtonDatas&,
                 int, const muse::IInteractive::Options&, const std::string&),
                (const, override));
    MOCK_METHOD(muse::IInteractive::ButtonData, buttonData, (muse::IInteractive::Button), (const, override));

    MOCK_METHOD(muse::IInteractive::Result, info,
                (const std::string&, const std::string&, const muse::IInteractive::Buttons&,
                 int, const muse::IInteractive::Options&, const std::string&),
                (const, override));
    MOCK_METHOD(muse::IInteractive::Result, info,
                (const std::string&, const muse::IInteractive::Text&, const muse::IInteractive::ButtonDatas&,
                 int, const muse::IInteractive::Options&, const std::string&),
                (const, override));

    MOCK_METHOD(muse::IInteractive::Result, warning,
                (const std::string&, const std::string&, const muse::IInteractive::Buttons&,
                 const muse::IInteractive::Button&, const muse::IInteractive::Options&, const std::string&),
                (const, override));
    MOCK_METHOD(muse::IInteractive::Result, warning,
                (const std::string&, const muse::IInteractive::Text&, const muse::IInteractive::ButtonDatas&,
                 int, const muse::IInteractive::Options&, const std::string&),
                (const, override));
    MOCK_METHOD(muse::IInteractive::Result, warning,
                (const std::string&, const muse::IInteractive::Text&, const std::string&,
                 const muse::IInteractive::ButtonDatas&, int, const muse::IInteractive::Options&, const std::string&),
                (const, override));

    MOCK_METHOD(muse::IInteractive::Result, error,
                (const std::string&, const std::string&, const muse::IInteractive::Buttons&,
                 const muse::IInteractive::Button&, const muse::IInteractive::Options&, const std::string&),
                (const, override));
    MOCK_METHOD(muse::IInteractive::Result, error,
                (const std::string&, const muse::IInteractive::Text&, const muse::IInteractive::ButtonDatas&,
                 int, const muse::IInteractive::Options&, const std::string&),
                (const, override));
    MOCK_METHOD(muse::IInteractive::Result, error,
                (const std::string&, const muse::IInteractive::Text&, const std::string&,
                 const muse::IInteractive::ButtonDatas&, int, const muse::IInteractive::Options&, const std::string&),
                (const, override));

    MOCK_METHOD(muse::Ret, showProgress, (const std::string&, muse::Progress*), (const, override));

    MOCK_METHOD(muse::io::path_t, selectOpeningFile, (const QString&, const muse::io::path_t&, const std::vector<std::string>&),
                (override));
    MOCK_METHOD(muse::io::path_t, selectSavingFile, (const QString&, const muse::io::path_t&, const std::vector<std::string>&, bool),
                (override));

    MOCK_METHOD(muse::io::path_t, selectDirectory, (const QString&, const muse::io::path_t&), (override));
    MOCK_METHOD(muse::io::paths_t, selectMultipleDirectories, (const QString&, const muse::io::path_t&, const muse::io::paths_t&),
                (override));

    MOCK_METHOD(QColor, selectColor, (const QColor&, const QString&), (override));
    MOCK_METHOD(bool, isSelectColorOpened, (), (const, override));

    MOCK_METHOD(muse::RetVal<muse::Val>, open, (const std::string&), (const, override));
    MOCK_METHOD(muse::RetVal<muse::Val>, open, (const muse::Uri&), (const, override));
    MOCK_METHOD(muse::RetVal<muse::Val>, open, (const muse::UriQuery&), (const, override));

    MOCK_METHOD(muse::RetVal<bool>, isOpened, (const std::string&), (const, override));
    MOCK_METHOD(muse::RetVal<bool>, isOpened, (const muse::Uri&), (const, override));
    MOCK_METHOD(muse::RetVal<bool>, isOpened, (const muse::UriQuery&), (const, override));
    MOCK_METHOD(muse::async::Channel<muse::Uri>, opened, (), (const, override));

    MOCK_METHOD(void, raise, (const muse::UriQuery&), (override));

    MOCK_METHOD(void, close, (const std::string&), (override));
    MOCK_METHOD(void, close, (const muse::Uri&), (override));
    MOCK_METHOD(void, close, (const muse::UriQuery& uri), (override));
    MOCK_METHOD(void, closeAllDialogs, (), (override));

    MOCK_METHOD(muse::ValCh<muse::Uri>, currentUri, (), (const, override));
    MOCK_METHOD(muse::RetVal<bool>, isCurrentUriDialog, (), (const, override));
    MOCK_METHOD(std::vector<muse::Uri>, stack, (), (const, override));

    MOCK_METHOD(muse::Ret, openUrl, (const std::string&), (const, override));
    MOCK_METHOD(muse::Ret, openUrl, (const QUrl&), (const, override));

    MOCK_METHOD(muse::Ret, isAppExists, (const std::string&), (const, override));
    MOCK_METHOD(muse::Ret, canOpenApp, (const muse::Uri&), (const, override));
    MOCK_METHOD(muse::async::Promise<muse::Ret>, openApp, (const muse::Uri&), (const, override));

    MOCK_METHOD(muse::Ret, revealInFileBrowser, (const muse::io::path_t&), (const, override));
};
}
