/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <gmock/gmock.h>

#include "spectrogram/ifrequencyselectioncontroller.h"

namespace au::spectrogram {
class FrequencySelectionControllerMock : public IFrequencySelectionController
{
public:
    MOCK_METHOD(FrequencySelection, frequencySelection, (), (const, override));

    MOCK_METHOD(bool, showsSpectrogram, (int), (const, override));
    MOCK_METHOD(void, setShowsSpectrogram, (int, bool), (override));

    MOCK_METHOD(uintptr_t, beginSelection, (int, double), (override));
    MOCK_METHOD(void, resetFrequencySelection, (), (override));
    MOCK_METHOD(void, restoreFrequencySelection, (), (override));

    MOCK_METHOD(uintptr_t, startFrequencyHandle, (), (const, override));
    MOCK_METHOD(uintptr_t, endFrequencyHandle, (), (const, override));
    MOCK_METHOD(void, setHandleFrequency, (double, bool, uintptr_t), (override));
    MOCK_METHOD(double, handleFrequency, (uintptr_t), (const, override));

    MOCK_METHOD(void, setCenterFrequency, (double, bool), (override));

    MOCK_METHOD(muse::async::Channel<bool>, frequencySelectionChanged, (), (const, override));
    MOCK_METHOD((muse::async::Channel<uintptr_t, bool>), handleDragged, (), (const, override));
};
}
