/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <gmock/gmock.h>

#include "automation/iclipgaininteraction.h"

namespace au::automation {
class ClipGainInteractionMock : public IClipGainInteraction
{
public:
    MOCK_METHOD(std::optional<AutomationInfo>, clipGainInfo, (const trackedit::ClipKey&), (const, override));
    MOCK_METHOD(AutomationPoints, clipGainPoints, (const trackedit::ClipKey&), (const, override));
    MOCK_METHOD(bool, setClipGainPoint, (const trackedit::ClipKey&, double, double, bool), (override));
    MOCK_METHOD(bool, removeClipGainPoint, (const trackedit::ClipKey&, int, bool), (override));
    MOCK_METHOD(bool, setClipGainPointAtIndex, (const trackedit::ClipKey&, int, double, double, bool), (override));
    MOCK_METHOD(bool, beginClipGainPointDrag, (const trackedit::ClipKey&, int), (override));
    MOCK_METHOD(bool, updateClipGainPointDrag, (const trackedit::ClipKey&, double, double), (override));
    MOCK_METHOD(bool, endClipGainPointDrag, (const trackedit::ClipKey&, bool), (override));
    MOCK_METHOD((muse::async::Channel<trackedit::ClipKey, bool>), clipGainChanged, (), (const, override));
};
}
