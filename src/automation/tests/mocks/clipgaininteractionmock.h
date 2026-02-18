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
    MOCK_METHOD(std::optional<ClipEnvelopeInfo>, clipEnvelopeInfo, (const trackedit::ClipKey&), (const, override));
    MOCK_METHOD(ClipEnvelopePoints, clipEnvelopePoints, (const trackedit::ClipKey&), (const, override));
    MOCK_METHOD(bool, setClipEnvelopePoint, (const trackedit::ClipKey&, double, double, bool), (override));
    MOCK_METHOD(bool, removeClipEnvelopePoint, (const trackedit::ClipKey&, int, bool), (override));
    MOCK_METHOD(bool, setClipEnvelopePointAtIndex, (const trackedit::ClipKey&, int, double, double, bool), (override));
    MOCK_METHOD(bool, beginClipEnvelopePointDrag, (const trackedit::ClipKey&, int), (override));
    MOCK_METHOD(bool, updateClipEnvelopePointDrag, (const trackedit::ClipKey&, double, double), (override));
    MOCK_METHOD(bool, endClipEnvelopePointDrag, (const trackedit::ClipKey&, bool), (override));
    MOCK_METHOD((muse::async::Channel<trackedit::ClipKey, bool>), clipEnvelopeChanged, (), (const, override));
};
}
