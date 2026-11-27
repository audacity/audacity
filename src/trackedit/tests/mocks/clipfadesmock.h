/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <gmock/gmock.h>

#include "trackedit/iclipfades.h"

namespace au::trackedit {
class ClipFadesMock : public IClipFades
{
public:
    MOCK_METHOD(bool, startFadeDrag, (const ClipKeyList&, const ClipKey&, ClipSide), (override));
    MOCK_METHOD(bool, changeFade, (secs_t), (override));
    MOCK_METHOD(bool, commitFadeDrag, (), (override));
    MOCK_METHOD(void, cancelFadeDrag, (), (override));
    MOCK_METHOD(std::optional<DraggedFade>, draggedFade, (), (const, override));
    MOCK_METHOD(muse::async::Notification, draggedFadeChanged, (), (const, override));
    MOCK_METHOD(FadeEnds, snappableFadeEnds, (const Clip&), (const, override));
};
}
