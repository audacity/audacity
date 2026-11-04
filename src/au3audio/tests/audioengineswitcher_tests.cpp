/*
 * Audacity: A Digital Audio Editor
 */
#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include <memory>

#include "framework/global/settings.h"

#include "audio/internal/audioengineswitcher.h"
#include "audio/tests/mocks/audiodrivercontrollermock.h"
#include "mocks/audioenginemock.h"

using ::testing::NiceMock;
using ::testing::Return;

namespace {
const muse::Settings::Key TEST_BACKEND_KEY("audio", "audio/backend");
const std::string TEST_FIRST = "first";
const std::string TEST_SECOND = "second";
const std::string TEST_FIRST_TITLE = "First";
constexpr float TEST_PLAYBACK_VOLUME = 0.25f;
constexpr float TEST_INPUT_VOLUME = 0.75f;
}

namespace au::audio {
class AudioEngineSwitcherTests : public ::testing::Test
{
protected:
    struct Backend {
        std::shared_ptr<NiceMock<AudioEngineMock> > engine = std::make_shared<NiceMock<AudioEngineMock> >();
        std::shared_ptr<NiceMock<AudioDriverControllerMock> > driver = std::make_shared<NiceMock<AudioDriverControllerMock> >();
        //! What suspending the backend's stream returns
        AudioStreamRestorer restorer = [this] { ++restorations; return true; };
        int suspensions = 0;
        int restorations = 0;

        AudioBackend backend(const std::string& id, const std::string& title = {})
        {
            return { id, engine, driver, [this] { ++suspensions; return restorer; }, title };
        }
    };

    void SetUp() override
    {
        m_savedChoice = muse::settings()->value(TEST_BACKEND_KEY);
        muse::settings()->setSharedValue(TEST_BACKEND_KEY, muse::Val());
    }

    void TearDown() override
    {
        muse::settings()->setSharedValue(TEST_BACKEND_KEY, m_savedChoice);
    }

    std::unique_ptr<AudioEngineSwitcher> makeSwitcher()
    {
        auto switcher = std::make_unique<AudioEngineSwitcher>();
        switcher->registerBackend(m_first.backend(TEST_FIRST, TEST_FIRST_TITLE));
        switcher->registerBackend(m_second.backend(TEST_SECOND));
        switcher->init();
        return switcher;
    }

    Backend m_first;
    Backend m_second;
    muse::Val m_savedChoice;
};

TEST_F(AudioEngineSwitcherTests, TheFirstBackendIsActiveUnlessAnotherWasChosen)
{
    EXPECT_EQ(makeSwitcher()->activeBackend(), TEST_FIRST);

    makeSwitcher()->setActiveBackend(TEST_SECOND);
    EXPECT_EQ(makeSwitcher()->activeBackend(), TEST_SECOND);
}

TEST_F(AudioEngineSwitcherTests, SwitchingCarriesTheStreamAndVolumesOver)
{
    //! [GIVEN] the first backend streams
    const auto switcher = makeSwitcher();
    ON_CALL(*m_first.engine, currentStream()).WillByDefault(Return(AudioStreamDescriptor {}));
    ON_CALL(*m_first.engine, getPlaybackVolume()).WillByDefault(Return(TEST_PLAYBACK_VOLUME));
    ON_CALL(*m_first.engine, getInputVolume()).WillByDefault(Return(TEST_INPUT_VOLUME));
    auto notified = false;
    switcher->activeBackendChanged().onNotify(nullptr, [&notified] { notified = true; });

    //! [THEN] the second gets the volumes ...
    EXPECT_CALL(*m_second.engine, setPlaybackVolume(TEST_PLAYBACK_VOLUME));
    EXPECT_CALL(*m_second.engine, setInputVolume(TEST_INPUT_VOLUME));

    //! [WHEN] switching to the second
    const auto result = switcher->setActiveBackend(TEST_SECOND);

    //! ... and the stream, suspended on the first, is restored
    EXPECT_EQ(result.status, ApplyStatus::Applied);
    EXPECT_EQ(m_first.suspensions, 1);
    EXPECT_EQ(m_first.restorations, 1);
    EXPECT_EQ(switcher->activeBackend(), TEST_SECOND);
    EXPECT_TRUE(notified);

    //! ... and the engine is the second's
    EXPECT_CALL(*m_second.engine, isBusy()).WillOnce(Return(true));
    EXPECT_TRUE(switcher->isBusy());
}

TEST_F(AudioEngineSwitcherTests, ABackendWhoseStreamCantBeSuspendedStaysActive)
{
    const auto switcher = makeSwitcher();
    ON_CALL(*m_first.engine, currentStream()).WillByDefault(Return(AudioStreamDescriptor {}));
    m_first.restorer = nullptr;

    const auto result = switcher->setActiveBackend(TEST_SECOND);

    EXPECT_EQ(result.status, ApplyStatus::OwnerUnavailable);
    EXPECT_EQ(switcher->activeBackend(), TEST_FIRST);
    // Nor will be the other at the next start
    EXPECT_EQ(makeSwitcher()->activeBackend(), TEST_FIRST);
}

TEST_F(AudioEngineSwitcherTests, UnknownBackendsAreRefused)
{
    const auto switcher = makeSwitcher();
    EXPECT_EQ(switcher->setActiveBackend("unknown").status, ApplyStatus::InvalidConfiguration);
    EXPECT_EQ(switcher->activeBackend(), TEST_FIRST);
}

TEST_F(AudioEngineSwitcherTests, BackendsAreTitledAsRegisteredOrByTheirIds)
{
    const auto switcher = makeSwitcher();
    EXPECT_EQ(switcher->backendTitle(TEST_FIRST), TEST_FIRST_TITLE);
    EXPECT_EQ(switcher->backendTitle(TEST_SECOND), TEST_SECOND);
}
}
