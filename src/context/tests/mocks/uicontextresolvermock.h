/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <gmock/gmock.h>

#include "context/iuicontextresolver.h"

namespace au::context {
class UiContextResolverMock : public IUiContextResolver
{
public:
    MOCK_METHOD(const muse::ui::UiContext&, currentUiContext, (), (const, override));
    MOCK_METHOD(muse::async::Notification, currentUiContextChanged, (), (const, override));

    MOCK_METHOD(bool, match, (const muse::ui::UiContext&, const muse::ui::UiContext&), (const, override));
    MOCK_METHOD(bool, matchWithCurrent, (const muse::ui::UiContext&), (const, override));

    MOCK_METHOD(bool, isShortcutContextAllowed, (const std::string&), (const, override));
};
}
