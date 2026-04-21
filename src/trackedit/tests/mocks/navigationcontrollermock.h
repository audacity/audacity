/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <gmock/gmock.h>

#include "ui/inavigationcontroller.h"

namespace muse::ui {
class NavigationControllerMock : public INavigationController
{
public:
    MOCK_METHOD(void, reg, (INavigationSection*), (override));
    MOCK_METHOD(void, unreg, (INavigationSection*), (override));

    MOCK_METHOD(bool, requestActivateByName, (const std::string&, const std::string&, const std::string&), (override));
    MOCK_METHOD(bool, requestActivateByIndex, (const std::string&, const std::string&, const INavigation::Index&), (override));

    MOCK_METHOD(void, resetNavigation, (), (override));

    MOCK_METHOD(INavigationSection*, activeSection, (), (const, override));
    MOCK_METHOD(INavigationPanel*, activePanel, (), (const, override));
    MOCK_METHOD(INavigationControl*, activeControl, (), (const, override));

    MOCK_METHOD((const std::set<INavigationSection*>&), sections, (), (const, override));
    MOCK_METHOD(const INavigationSection*, findSection, (const std::string&), (const, override));
    MOCK_METHOD(const INavigationPanel*, findPanel, (const std::string&, const std::string&), (const, override));
    MOCK_METHOD(const INavigationControl*, findControl, (const std::string&, const std::string&, const std::string&), (const, override));

    MOCK_METHOD(void, setDefaultNavigationControl, (INavigationControl*), (override));

    MOCK_METHOD(async::Notification, navigationChanged, (), (const, override));

    MOCK_METHOD(bool, isHighlight, (), (const, override));
    MOCK_METHOD(void, setIsHighlight, (bool), (override));
    MOCK_METHOD(async::Notification, highlightChanged, (), (const, override));

    MOCK_METHOD(void, setIsResetOnMousePress, (bool), (override));

    MOCK_METHOD(void, dump, (), (const, override));
};
}
