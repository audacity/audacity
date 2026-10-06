/*
 * Audacity: A Digital Audio Editor
 */
#include <algorithm>
#include <future>
#include <string>
#include <thread>

#include <gtest/gtest.h>

#include <portaudio.h>

#include "au3-audio-devices/AudioIOBase.h"
#include "au3-audio-io/AudioIO.h"
#include "au3-project/Project.h"

#include "au3wrap/internal/au3project.h"
#include "au3wrap/au3types.h"
#include "project/tests/testtools.h"

using namespace std::chrono_literals;

namespace au::au3audio {
/**
 * @brief Fixture for testing AudioIO monitoring against a real PortAudio stream.
 *
 * @details Runs on any platform with a capture device; on headless Linux CI, ALSA's `null` device fills that role.
 * No audio needs to flow, the only hardware dependency is that Pa_OpenStream must succeed.
 */
class AudioIOMonitoringTest : public ::testing::Test
{
protected:
    void SetUp() override
    {
        // StartPortAudioStream refuses to run without an owning project
        // (AudioIO.cpp: `if (mOwningProject.expired()) return false;`),
        // so load a real (empty) one, like au3record_tests does.
        m_au3ProjectAccessor = std::make_shared<au::au3::Au3ProjectAccessor>(muse::modularity::globalCtx());
        const std::string source = std::string(au3audio_tests_DATA_ROOT) + "/../../trackedit/tests/data/empty.aup4";
        m_workingProjectPath = std::string(au3audio_tests_DATA_ROOT) + "/monitoring_working.aup4";
        testtools::removeProjectIfExists(m_workingProjectPath);
        ASSERT_TRUE(testtools::copyFile(source, m_workingProjectPath));
        constexpr auto discardAutosave = false;
        ASSERT_TRUE(m_au3ProjectAccessor->load(muse::io::path_t(m_workingProjectPath), discardAutosave));
    }

    void TearDown() override
    {
        AudioIO::Deinit();

        if (m_au3ProjectAccessor && m_au3ProjectAccessor->au3ProjectPtr()) {
            m_au3ProjectAccessor->clearSavedState();
            m_au3ProjectAccessor->close();
        }
        testtools::removeProjectIfExists(m_workingProjectPath);
    }

    au::au3::Au3Project& projectRef() const
    {
        return *reinterpret_cast<au::au3::Au3Project*>(m_au3ProjectAccessor->au3ProjectPtr());
    }

    //! Points the recording-device prefs at a usable capture device.
    //! Selects ALSA "null" if available (headless CI), else tries default input device first, because likely less flaky than other random devices.
    //! Returns false if none exists.
    bool selectCaptureDevice()
    {
        const PaDeviceIndex deviceCount = Pa_GetDeviceCount();
        const PaDeviceInfo* chosen = nullptr;
        for (PaDeviceIndex i = 0; i < deviceCount; ++i) {
            const PaDeviceInfo* info = Pa_GetDeviceInfo(i);
            if (info && info->maxInputChannels > 0 && std::string(info->name) == "null") {
                chosen = info;
                break;
            }
        }
        if (!chosen) {
            const PaDeviceIndex defaultInput = Pa_GetDefaultInputDevice();
            if (defaultInput != paNoDevice) {
                chosen = Pa_GetDeviceInfo(defaultInput);
            }
        }
        for (PaDeviceIndex i = 0; !chosen && i < deviceCount; ++i) {
            const PaDeviceInfo* info = Pa_GetDeviceInfo(i);
            if (info && info->maxInputChannels > 0) {
                chosen = info;
            }
        }
        if (!chosen) {
            return false;
        }
        const PaHostApiInfo* host = Pa_GetHostApiInfo(chosen->hostApi);
        AudioIOHost.Write(host->name);
        AudioIORecordingDevice.Write(chosen->name);
        AudioIORecordChannels.Write(std::min(2, chosen->maxInputChannels));
        return true;
    }

    std::shared_ptr<au::au3::Au3ProjectAccessor> m_au3ProjectAccessor;
    std::string m_workingProjectPath;
};

//! https://github.com/audacity/audacity/issues/11571 and https://github.com/audacity/audacity/issues/11825
//! are caused by `StopMonitoring` waiting for an acknowledgement from the `AudioThread`, which may never come
//! if `StartMonitoring` - `StopMonitoring` are called in quick succession.
//!
//! (In #11825 the calls are fast because due to repeated track-focus toggling (and hence monitoring) upon track creation.
//! In #11571 it was for something similar.)
//!
//! Monitoring does not involve the audio thread, so we replace it with one that does nothing:
//! `StopMonitoring` must still return.
TEST_F(AudioIOMonitoringTest, MonitoringDoesNotNeedAudioThread)
{
    AudioIO::Init();
    if (!selectCaptureDevice()) {
        GTEST_SKIP() << "no capture device available";
    }
    AudioIO* audioIO = AudioIO::Get();

    audioIO->mFinishAudioThread.store(true, std::memory_order_release);
    audioIO->mAudioThread.join();
    audioIO->mAudioThread = std::thread([] {});

    const AudioIOStartStreamOptions options(projectRef().shared_from_this(), 44100.0);
    audioIO->StartMonitoring(options);
    ASSERT_TRUE(audioIO->IsMonitoring());

    std::promise<void> stopped;
    std::future<void> result = stopped.get_future();
    std::thread stopper([audioIO, &stopped] {
        audioIO->StopMonitoring();
        stopped.set_value();
    });

    const bool hung = result.wait_for(5s) == std::future_status::timeout;
    if (hung) {
        // unblock StopMonitoring if it hung
        audioIO->mBufferExchangeAcknowledge.store(Acknowledge::eStop, std::memory_order_release);
    }
    stopper.join();

    EXPECT_FALSE(hung) << "StopMonitoring waits for the audio thread";
}
} // namespace au::au3audio
