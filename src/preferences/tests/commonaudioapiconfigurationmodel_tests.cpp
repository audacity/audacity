/*
* Audacity: A Digital Audio Editor
*/

#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include <QQmlContext>
#include <QQmlEngine>

#include "global/modularity/ioc.h"

#include "audio/tests/mocks/audiodevicesprovidermock.h"

#include "preferences/qml/Audacity/Preferences/commonaudioapiconfigurationmodel.h"

using ::testing::NiceMock;
using ::testing::Return;

using namespace au::audio;

namespace au::appshell {
constexpr const char* SYSTEM_DEFAULT = "System default";

class CommonAudioApiConfigurationModelTests : public ::testing::Test
{
protected:
    void SetUp() override
    {
        //! NOTE The model resolves its dependencies through its QML object context,
        //! and IAudioDevicesProvider is a contextual interface, so the mock is
        //! registered in a context IoC that the engine exposes as "ioc_context"
        m_iocContext = std::make_shared<muse::modularity::Context>(1);

        m_provider = std::make_shared<NiceMock<AudioDevicesProviderMock> >();
        muse::modularity::ioc(m_iocContext)->registerExport<IAudioDevicesProvider>("utests", m_provider);

        m_engine = std::make_unique<QQmlEngine>();
        auto* qmlIoc = new muse::QmlIoCContext(m_engine.get());
        qmlIoc->ctx = m_iocContext;
        m_engine->rootContext()->setContextProperty("ioc_context", QVariant::fromValue(qmlIoc));

        m_model = std::make_unique<CommonAudioApiConfigurationModel>();
        QQmlEngine::setContextForObject(m_model.get(), m_engine->rootContext());
    }

    void TearDown() override
    {
        m_model.reset();
        m_engine.reset();
        muse::modularity::removeIoC(m_iocContext);
    }

    void setOutputDevices(const std::vector<std::string>& devices)
    {
        ON_CALL(*m_provider, outputDevices()).WillByDefault(Return(devices));
    }

    void setCurrentOutputDevice(const std::optional<std::string>& device)
    {
        ON_CALL(*m_provider, currentOutputDevice()).WillByDefault(Return(device));
    }

    void setInputDevices(const std::vector<std::string>& devices)
    {
        ON_CALL(*m_provider, inputDevices()).WillByDefault(Return(devices));
    }

    void setCurrentInputDevice(const std::optional<std::string>& device)
    {
        ON_CALL(*m_provider, currentInputDevice()).WillByDefault(Return(device));
    }

    muse::modularity::ContextPtr m_iocContext;
    std::shared_ptr<NiceMock<AudioDevicesProviderMock> > m_provider;
    std::unique_ptr<QQmlEngine> m_engine;
    std::unique_ptr<CommonAudioApiConfigurationModel> m_model;
};

TEST_F(CommonAudioApiConfigurationModelTests, OutputDeviceList_NoDevices_IsEmpty)
{
    setOutputDevices({});

    EXPECT_TRUE(m_model->outputDeviceList().isEmpty());
    EXPECT_EQ(m_model->currentOutputDeviceIndex(), -1);
}

TEST_F(CommonAudioApiConfigurationModelTests, OutputDeviceList_StartsWithSystemDefaultEntry)
{
    setOutputDevices({ "Speakers", "Headphones" });

    const QVariantList list = m_model->outputDeviceList();
    ASSERT_EQ(list.size(), 3);
    EXPECT_EQ(list.at(0).toString(), QString(SYSTEM_DEFAULT));
    EXPECT_EQ(list.at(1).toString(), QString("Speakers"));
    EXPECT_EQ(list.at(2).toString(), QString("Headphones"));
}

TEST_F(CommonAudioApiConfigurationModelTests, CurrentOutputDeviceIndex_NoDeviceSelected_IsSystemDefaultEntry)
{
    setOutputDevices({ "Speakers", "Headphones" });
    setCurrentOutputDevice(std::nullopt);

    EXPECT_EQ(m_model->currentOutputDeviceIndex(), 0);
}

TEST_F(CommonAudioApiConfigurationModelTests, CurrentOutputDeviceIndex_DeviceSelected_IsShiftedByDefaultEntry)
{
    setOutputDevices({ "Speakers", "Headphones" });
    setCurrentOutputDevice("Headphones");

    EXPECT_EQ(m_model->currentOutputDeviceIndex(), 2);
}

TEST_F(CommonAudioApiConfigurationModelTests, CurrentOutputDeviceIndex_UnknownDevice_FallsBackToSystemDefaultEntry)
{
    setOutputDevices({ "Speakers", "Headphones" });
    setCurrentOutputDevice("Unplugged device");

    EXPECT_EQ(m_model->currentOutputDeviceIndex(), 0);
}

TEST_F(CommonAudioApiConfigurationModelTests, OutputDeviceSelected_SystemDefaultEntry_ResetsDevice)
{
    setOutputDevices({ "Speakers", "Headphones" });
    setCurrentOutputDevice("Speakers");

    EXPECT_CALL(*m_provider, setOutputDevice(std::optional<std::string>(std::nullopt)));

    m_model->outputDeviceSelected(0);
}

TEST_F(CommonAudioApiConfigurationModelTests, OutputDeviceSelected_DeviceEntry_SetsDevice)
{
    setOutputDevices({ "Speakers", "Headphones" });
    setCurrentOutputDevice(std::nullopt);

    EXPECT_CALL(*m_provider, setOutputDevice(std::optional<std::string>("Headphones")));

    m_model->outputDeviceSelected(2);
}

TEST_F(CommonAudioApiConfigurationModelTests, OutputDeviceSelected_CurrentEntry_DoesNothing)
{
    setOutputDevices({ "Speakers", "Headphones" });
    setCurrentOutputDevice("Speakers");

    EXPECT_CALL(*m_provider, setOutputDevice(::testing::_)).Times(0);

    m_model->outputDeviceSelected(1);
}

TEST_F(CommonAudioApiConfigurationModelTests, OutputDeviceSelected_OutOfRange_DoesNothing)
{
    setOutputDevices({ "Speakers", "Headphones" });
    setCurrentOutputDevice("Speakers");

    EXPECT_CALL(*m_provider, setOutputDevice(::testing::_)).Times(0);

    m_model->outputDeviceSelected(3);
    m_model->outputDeviceSelected(-1);
}

//! NOTE A real device may carry the same name as the "System default" entry;
//! index-based selection must keep them distinguishable
TEST_F(CommonAudioApiConfigurationModelTests, OutputDevice_NamedSystemDefault_IsDistinctFromDefaultEntry)
{
    setOutputDevices({ SYSTEM_DEFAULT });
    setCurrentOutputDevice(SYSTEM_DEFAULT);

    EXPECT_EQ(m_model->currentOutputDeviceIndex(), 1);

    setCurrentOutputDevice(std::nullopt);

    EXPECT_CALL(*m_provider, setOutputDevice(std::optional<std::string>(SYSTEM_DEFAULT)));

    m_model->outputDeviceSelected(1);
}

TEST_F(CommonAudioApiConfigurationModelTests, InputDeviceList_NoDevices_IsEmpty)
{
    setInputDevices({});

    EXPECT_TRUE(m_model->inputDeviceList().isEmpty());
    EXPECT_EQ(m_model->currentInputDeviceIndex(), -1);
}

TEST_F(CommonAudioApiConfigurationModelTests, InputDeviceList_StartsWithSystemDefaultEntry)
{
    setInputDevices({ "Microphone" });

    const QVariantList list = m_model->inputDeviceList();
    ASSERT_EQ(list.size(), 2);
    EXPECT_EQ(list.at(0).toString(), QString(SYSTEM_DEFAULT));
    EXPECT_EQ(list.at(1).toString(), QString("Microphone"));
}

TEST_F(CommonAudioApiConfigurationModelTests, CurrentInputDeviceIndex_MapsLikeOutput)
{
    setInputDevices({ "Microphone", "Line In" });

    setCurrentInputDevice(std::nullopt);
    EXPECT_EQ(m_model->currentInputDeviceIndex(), 0);

    setCurrentInputDevice("Line In");
    EXPECT_EQ(m_model->currentInputDeviceIndex(), 2);

    setCurrentInputDevice("Unplugged device");
    EXPECT_EQ(m_model->currentInputDeviceIndex(), 0);
}

TEST_F(CommonAudioApiConfigurationModelTests, InputDeviceSelected_SystemDefaultEntry_ResetsDevice)
{
    setInputDevices({ "Microphone", "Line In" });
    setCurrentInputDevice("Microphone");

    EXPECT_CALL(*m_provider, setInputDevice(std::optional<std::string>(std::nullopt)));

    m_model->inputDeviceSelected(0);
}

TEST_F(CommonAudioApiConfigurationModelTests, InputDeviceSelected_DeviceEntry_SetsDevice)
{
    setInputDevices({ "Microphone", "Line In" });
    setCurrentInputDevice(std::nullopt);

    EXPECT_CALL(*m_provider, setInputDevice(std::optional<std::string>("Line In")));

    m_model->inputDeviceSelected(2);
}

TEST_F(CommonAudioApiConfigurationModelTests, Load_ForwardsProviderNotificationsToSignals)
{
    muse::async::Notification outputDeviceChanged;
    muse::async::Notification inputDeviceChanged;
    ON_CALL(*m_provider, outputDeviceChanged()).WillByDefault(Return(outputDeviceChanged));
    ON_CALL(*m_provider, inputDeviceChanged()).WillByDefault(Return(inputDeviceChanged));

    m_model->load();

    int outputChangedCount = 0;
    int inputChangedCount = 0;
    QObject::connect(m_model.get(), &CommonAudioApiConfigurationModel::currentOutputDeviceIndexChanged,
                     m_model.get(), [&outputChangedCount]() { ++outputChangedCount; });
    QObject::connect(m_model.get(), &CommonAudioApiConfigurationModel::currentInputDeviceIndexChanged,
                     m_model.get(), [&inputChangedCount]() { ++inputChangedCount; });

    outputDeviceChanged.notify();
    inputDeviceChanged.notify();

    EXPECT_EQ(outputChangedCount, 1);
    EXPECT_EQ(inputChangedCount, 1);
}
}
