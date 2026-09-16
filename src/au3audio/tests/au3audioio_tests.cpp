/*
 * Audacity: A Digital Audio Editor
 */
#include <algorithm>
#include <string>

#include <gtest/gtest.h>

#include <portaudio.h>

#include "au3-audio-devices/AudioIOBase.h"
#include "au3-audio-io/AudioIO.h"
#include "au3-project/Project.h"

#include "au3wrap/internal/au3project.h"
#include "au3wrap/au3types.h"
#include "project/tests/testtools.h"
#include "mocks/audiothreadloopcontroller.h"

using namespace std::chrono_literals;

namespace au::au3audio {
/**
 * @brief Fixture allowing to test concurrency between the main thread and the audio thread.
 *
 * @details Uses a pacer for the loop in `::AudioThread`.
 * Runs on any platform with a capture device; on headless Linux CI, a `test_null` ALSA device
 * (an alias of `null`, which PortAudio ignores) is defined in the workflow to fill that role.
 * No audio needs to flow, the only hardware dependency is that Pa_OpenStream must succeed.
 */
class MainAndAudioThreadConcurrencyTest : public ::testing::Test
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
        m_pacer->release(); // Deinit joins the audio thread
        AudioIO::Deinit();
        AudioIoCallback::SetAudioThreadPacerForTests(nullptr);

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
    //! Selects "test_null" if available (headless CI), else tries default input device first, because likely less flaky than other random devices.
    //! Returns false if none exists.
    bool selectCaptureDevice()
    {
        const PaDeviceIndex deviceCount = Pa_GetDeviceCount();
        const PaDeviceInfo* chosen = nullptr;
        for (PaDeviceIndex i = 0; i < deviceCount; ++i) {
            const PaDeviceInfo* info = Pa_GetDeviceInfo(i);
            if (info && info->maxInputChannels > 0 && std::string(info->name) == "test_null") {
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

    std::shared_ptr<test::AudioThreadLoopController> m_pacer;
    std::shared_ptr<au::au3::Au3ProjectAccessor> m_au3ProjectAccessor;
    std::string m_workingProjectPath;
};

//! https://github.com/audacity/audacity/issues/11571 and https://github.com/audacity/audacity/issues/11825
//! are caused by `StartMonitoring` - `StopMonitoring` calls sufficiently fast one after the other so that
//! the `AudioThread` doesn't have time to complete the otherwise expected iterations.
//!
//! (In #11825 the calls are fast because due to repeated track-focus toggling (and hence monitoring) upon track creation.
//! In #11571 it was for something similar.)
//!
//! We simulate this situation by not letting the `AudioThreadLoopController` any iteration of the `AudioThread` loop complete.
TEST_F(MainAndAudioThreadConcurrencyTest, ImmediateStopAfterStartDoesNotDeadlock)
{
    // Three iterations are normally sufficient for the handshake to complete.
    constexpr auto maxLoopIterations = 3;
    m_pacer = std::make_shared<test::AudioThreadLoopController>(maxLoopIterations);

    AudioIoCallback::SetAudioThreadPacerForTests(m_pacer);
    AudioIO::Init();

    if (!selectCaptureDevice()) {
        GTEST_SKIP() << "no capture device available";
    }

    AudioIO* gAudioIO = AudioIO::Get();
    const AudioIOStartStreamOptions options(projectRef().shared_from_this(), 44100.0);

    gAudioIO->StartMonitoring(options);
    ASSERT_TRUE(gAudioIO->IsMonitoring());
    gAudioIO->StopMonitoring();

    EXPECT_FALSE(m_pacer->waitGaveUp()) << "acknowledge handshake never completed";
}
} // namespace au::au3audio
