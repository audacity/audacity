/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <gmock/gmock.h>

#include "trackedit/itrackeditconfiguration.h"

namespace au::trackedit {
class TrackeditConfigurationMock : public ITrackeditConfiguration
{
public:
    MOCK_METHOD(bool, pasteAsNewClip, (), (const, override));
    MOCK_METHOD(void, setPasteAsNewClip, (bool), (override));
    MOCK_METHOD(muse::async::Notification, pasteAsNewClipChanged, (), (const, override));

    MOCK_METHOD(bool, askBeforeConvertingToMonoOrStereo, (), (const, override));
    MOCK_METHOD(void, setAskBeforeConvertingToMonoOrStereo, (bool), (override));
    MOCK_METHOD(muse::async::Notification, askBeforeConvertingToMonoOrStereoChanged, (), (const, override));

    MOCK_METHOD(DeleteBehavior, deleteBehavior, (), (const, override));
    MOCK_METHOD(void, setDeleteBehavior, (DeleteBehavior), (override));
    MOCK_METHOD(muse::async::Notification, deleteBehaviorChanged, (), (const, override));

    MOCK_METHOD(CloseGapBehavior, closeGapBehavior, (), (const, override));
    MOCK_METHOD(void, setCloseGapBehavior, (CloseGapBehavior), (override));
    MOCK_METHOD(muse::async::Notification, closeGapBehaviorChanged, (), (const, override));

    MOCK_METHOD(PasteBehavior, pasteBehavior, (), (const, override));
    MOCK_METHOD(void, setPasteBehavior, (PasteBehavior), (override));
    MOCK_METHOD(muse::async::Notification, pasteBehaviorChanged, (), (const, override));

    MOCK_METHOD(PasteInsertBehavior, pasteInsertBehavior, (), (const, override));
    MOCK_METHOD(void, setPasteInsertBehavior, (PasteInsertBehavior), (override));
    MOCK_METHOD(muse::async::Notification, pasteInsertBehaviorChanged, (), (const, override));
};
}

