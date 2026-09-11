/*
 * Audacity: A Digital Audio Editor
 */
#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include <QAbstractItemModel>
#include <QFontMetricsF>
#include <QPointer>
#include <QQmlComponent>
#include <QQmlContext>
#include <QQmlEngine>
#include <QQmlIncubationController>
#include <QQuickItem>
#include <QQuickWindow>
#include <QtGui/qtestsupport_gui.h>

#include "actions/internal/actionsdispatcher.h"
#include "audio/tests/mocks/audiodrivercontrollermock.h"
#include "interactive/tests/mocks/interactivemock.h"
#include "rcommand/internal/commanddispatcher.h"
#include "stubs/accessibility/accessibilitycontrollerstub.h"
#include "stubs/languages/languagesservicestub.h"
#include "ui/api/themeapi.h"
#include "ui/internal/navigationcontroller.h"
#include "ui/internal/uiengine.h"
#include "ui/navigationcommands.h"
#include "ui/tests/mocks/mainwindowmock.h"
#include "ui/tests/mocks/uiconfigurationmock.h"
#include "uicomponents/qml/Muse/UiComponents/popupview.h"
#include "uicomponents/uicomponentsmodule.h"

#include "../qml/Audacity/Preferences/commonaudioapiconfigurationmodel.h"

using ::testing::_;
using ::testing::NiceMock;
using ::testing::Return;
using ::testing::ReturnRef;

namespace au::appshell {
constexpr const char* SYSTEM_DEFAULT = "System default";

class CommonAudioApiConfigurationModelTests : public ::testing::Test
{
public:
    void SetUp() override
    {
        m_context = std::make_shared<muse::modularity::Context>(201);
        m_model = std::make_unique<CommonAudioApiConfigurationModel>();
        m_model->setContext(m_context);
        m_controller = std::make_shared<NiceMock<audio::AudioDriverControllerMock> >();
        m_model->audioDriverController.set(m_controller);

        m_applied.api = "Core Audio";
        m_applied.outputDevice = "Built-in Output";
        m_applied.inputDevice = "Built-in Mic";
        m_applied.inputChannelSelection = { { { 0 } } };
        m_applied.bufferLength = 100.0;
        m_applied.defaultSampleRate = 44100;
        m_applied.defaultSampleFormat = "32-bit float";

        ON_CALL(*m_controller, configuration()).WillByDefault([this]() { return m_applied; });
        ON_CALL(*m_controller, configurationChanged())
        .WillByDefault(Return(m_configurationChanged));
        ON_CALL(*m_controller, audioDeviceListChanged())
        .WillByDefault(Return(m_deviceListChanged));
        ON_CALL(*m_controller, apis())
        .WillByDefault(Return(std::vector<std::string> { "Core Audio", "JACK" }));
        ON_CALL(*m_controller, outputDevices())
        .WillByDefault(Return(std::vector<std::string> { "Built-in Output", "Headphones" }));
        ON_CALL(*m_controller, inputDevices())
        .WillByDefault(Return(std::vector<std::string> { "Built-in Mic", "USB Mic" }));
        ON_CALL(*m_controller, inputChannelsAvailable()).WillByDefault(Return(2));
        ON_CALL(*m_controller, outputDevices("Core Audio"))
        .WillByDefault(Return(std::vector<std::string> { "Built-in Output", "Headphones" }));
        ON_CALL(*m_controller, inputDevices("Core Audio"))
        .WillByDefault(Return(std::vector<std::string> { "Built-in Mic", "USB Mic" }));
        ON_CALL(*m_controller, outputDevices("JACK"))
        .WillByDefault(Return(std::vector<std::string> { "JACK Out 1", "JACK Out 2" }));
        ON_CALL(*m_controller, inputDevices("JACK"))
        .WillByDefault(Return(std::vector<std::string> { "JACK In" }));
        ON_CALL(*m_controller, inputChannelsAvailable("JACK", _)).WillByDefault(Return(4));
        ON_CALL(*m_controller, sampleRates())
        .WillByDefault(Return(std::vector<uint64_t> { 44100, 48000 }));

        m_model->load();
    }

    void TearDown() override
    {
        m_model.reset();
        muse::modularity::removeIoC(m_context);
    }

    std::unique_ptr<CommonAudioApiConfigurationModel> m_model;
    muse::modularity::ContextPtr m_context;
    std::shared_ptr<NiceMock<audio::AudioDriverControllerMock> > m_controller;
    audio::AudioConfiguration m_applied;
    muse::async::Channel<audio::AudioConfigurationDelta> m_configurationChanged;
    muse::async::Notification m_deviceListChanged;
};

class RecordingChannelListModelTests : public CommonAudioApiConfigurationModelTests
{
public:
    std::unique_ptr<QObject> createView()
    {
        m_engine.rootContext()->setContextProperty("apiModel", m_model.get());
        QQmlComponent component(&m_engine, QUrl::fromLocalFile(
                                    QStringLiteral(preferences_tests_DATA_ROOT "/data/RecordingChannelListView.qml")));
        auto view = std::unique_ptr<QObject>(component.create());
        EXPECT_FALSE(component.isError()) << component.errorString().toStdString();
        return view;
    }

    QQuickItem* scrollToGroup(QObject* view, int index)
    {
        QVariant result;
        EXPECT_TRUE(QMetaObject::invokeMethod(view, "scrollToGroup", Q_RETURN_ARG(QVariant, result), Q_ARG(QVariant, index)));
        return qobject_cast<QQuickItem*>(result.value<QObject*>());
    }

    QQmlEngine m_engine;
};

class RecordingChannelPopupTests : public CommonAudioApiConfigurationModelTests
{
public:
    static void SetUpTestSuite()
    {
        uicomponents::UiComponentsModule module;
        module.registerResources();
        module.registerUiTypes();
    }

    void SetUp() override
    {
        CommonAudioApiConfigurationModelTests::SetUp();
#ifdef Q_OS_MAC
        // Muse popups observe the parent NSWindow, which requires the Cocoa platform plugin.
        if (QGuiApplication::platformName() != "cocoa") {
            GTEST_SKIP() << "Popup navigation tests require QT_QPA_PLATFORM=cocoa on macOS";
        }
#endif

        m_uiConfiguration = std::make_shared<NiceMock<muse::ui::UiConfigurationMock> >();
        ON_CALL(*m_uiConfiguration, currentTheme()).WillByDefault(ReturnRef(m_themeInfo));
        ON_CALL(*m_uiConfiguration, fontFamily()).WillByDefault(Return(QGuiApplication::font().family().toStdString()));
        ON_CALL(*m_uiConfiguration, fontSize(_)).WillByDefault(Return(12));
        ON_CALL(*m_uiConfiguration, iconsFontSize(_)).WillByDefault(Return(16));
        ON_CALL(*m_uiConfiguration, musicalFontSize()).WillByDefault(Return(16));
        ON_CALL(*m_uiConfiguration, musicalTextFontSize()).WillByDefault(Return(16));
        ON_CALL(*m_uiConfiguration, flickableMaxVelocity()).WillByDefault(Return(2500));
        auto* globalIoc = muse::modularity::globalIoc();
        globalIoc->registerExport<muse::ui::IUiConfiguration>("preferences_tests", m_uiConfiguration);
        globalIoc->registerExport<muse::languages::ILanguagesService>("preferences_tests", new muse::languages::LanguagesServiceStub());

        auto* ioc = muse::modularity::ioc(m_context);
        auto mainWindow = std::make_shared<NiceMock<muse::ui::MainWindowMock> >();
        ON_CALL(*mainWindow, qWindow()).WillByDefault(Return(&m_window));
        ioc->registerExport<muse::ui::IMainWindow>("preferences_tests", mainWindow);
        auto interactive = std::make_shared<NiceMock<muse::InteractiveMock> >();
        ON_CALL(*interactive, topWindow()).WillByDefault(Return(&m_window));
        ioc->registerExport<muse::IInteractive>("preferences_tests", interactive);
        ioc->registerExport<muse::accessibility::IAccessibilityController>(
            "preferences_tests", new muse::accessibility::AccessibilityControllerStub());
        ioc->registerExport<muse::actions::IActionsDispatcher>(
            "preferences_tests", new muse::actions::ActionsDispatcher(m_context));
        m_dispatcher = std::make_shared<muse::rcommand::CommandDispatcher>();
        ioc->registerExport<muse::rcommand::ICommandDispatcher>("preferences_tests", m_dispatcher);
        m_navigation = std::make_shared<muse::ui::NavigationController>(m_context);
        ioc->registerExport<muse::ui::INavigationController>("preferences_tests", m_navigation);
        m_navigation->init();

        m_theme = std::make_unique<muse::api::ThemeApi>(nullptr);
        m_theme->init();
        m_uiEngine = std::make_shared<muse::ui::UiEngine>(m_context);
        ioc->registerExport<muse::ui::IUiEngine>("preferences_tests", m_uiEngine);
        m_uiEngine->setTheme(m_theme.get());
        m_uiEngine->init();
        m_uiEngine->qmlEngine()->rootContext()->setContextProperty("recordingApiModel", m_model.get());
    }

    void TearDown() override
    {
        if (m_popup) {
            m_popup->close();
        }
        m_view.reset();
        m_window.close();
        if (m_uiEngine) {
            m_uiEngine->quit();
        }
        m_uiEngine.reset();
        m_theme.reset();
        CommonAudioApiConfigurationModelTests::TearDown();
        m_navigation.reset();
        m_dispatcher.reset();
        if (m_uiConfiguration) {
            muse::modularity::globalIoc()->unregister<muse::ui::IUiConfiguration>("preferences_tests");
            muse::modularity::globalIoc()->unregister<muse::languages::ILanguagesService>("preferences_tests");
        }
    }

    void createView()
    {
        QQmlComponent component(m_uiEngine->qmlEngine(), QUrl::fromLocalFile(
                                    QStringLiteral(preferences_tests_DATA_ROOT "/data/RecordingChannelPopupView.qml")));
        m_view.reset(qobject_cast<QQuickItem*>(component.create()));
        ASSERT_FALSE(component.isError()) << component.errorString().toStdString();
        ASSERT_TRUE(m_view);
        m_popup = m_view->findChild<muse::uicomponents::PopupView*>("RecordingChannelsPopup");
        ASSERT_TRUE(m_popup);
        m_list = m_view->findChild<QQuickItem*>("RecordingChannelsList");
        ASSERT_TRUE(m_list);

        m_window.resize(m_view->width(), m_view->height());
        m_view->setParentItem(m_window.contentItem());
        m_uiEngine->setRootItem(m_view.get());
        m_window.show();
        ASSERT_TRUE(QTest::qWaitForWindowExposed(&m_window));
    }

    void dispatch(const muse::rcommand::Command& command)
    {
        m_dispatcher->dispatch(command);
        QCoreApplication::processEvents();
        // Finish the ListView's cached delegates before sending the next command.
        ASSERT_TRUE(QMetaObject::invokeMethod(m_list, "forceLayout"));
        if (auto* incubation = m_uiEngine->qmlEngine()->incubationController()) {
            ASSERT_TRUE(QTest::qWaitFor([incubation]() {
                incubation->incubateFor(10);
                return incubation->incubatingObjectCount() == 0;
            }));
        }
    }

    void openPopup()
    {
        ASSERT_TRUE(m_navigation->requestActivateByName("PreferencesTestWindow", "AudioApiSection", "RecordingChannelsBox"));
        dispatch(muse::ui::TRIGGER_CONTROL_COMMAND);
        ASSERT_TRUE(m_popup->isOpened());
        ASSERT_NO_FATAL_FAILURE(expectChannelFocus(0, "1"));
    }

    void expectChannelFocus(int row, const QString& title)
    {
        ASSERT_NE(m_navigation->activeSection(), nullptr);
        ASSERT_EQ(m_navigation->activeSection()->name(), "RecordingChannelsPopup");
        ASSERT_NE(m_navigation->activeControl(), nullptr);
        ASSERT_EQ(m_navigation->activeControl()->name().toStdString(), ("RecordingChannel" + title).toStdString());
        ASSERT_EQ(m_navigation->activeControl()->index().row, row);
        // Check focus within the QML scope even when the test window is not the foreground app.
        EXPECT_TRUE(m_navigation->activeControl()->visualItem()->hasFocus());
    }

    muse::ui::ThemeInfo m_themeInfo;
    std::shared_ptr<NiceMock<muse::ui::UiConfigurationMock> > m_uiConfiguration;
    std::unique_ptr<muse::api::ThemeApi> m_theme;
    std::shared_ptr<muse::ui::UiEngine> m_uiEngine;
    std::shared_ptr<muse::ui::NavigationController> m_navigation;
    std::shared_ptr<muse::rcommand::CommandDispatcher> m_dispatcher;
    QQuickWindow m_window;
    std::unique_ptr<QQuickItem> m_view;
    QPointer<muse::uicomponents::PopupView> m_popup;
    QPointer<QQuickItem> m_list;
};

TEST_F(RecordingChannelPopupTests, OpeningAndDirectionalNavigationStayInsidePopup)
{
    ASSERT_NO_FATAL_FAILURE(createView());
    ASSERT_NO_FATAL_FAILURE(openPopup());

    dispatch(muse::ui::DOWN_COMMAND);
    ASSERT_NO_FATAL_FAILURE(expectChannelFocus(1, "2"));
    dispatch(muse::ui::DOWN_COMMAND);
    ASSERT_NO_FATAL_FAILURE(expectChannelFocus(2, "1+2"));
    dispatch(muse::ui::UP_COMMAND);
    ASSERT_NO_FATAL_FAILURE(expectChannelFocus(1, "2"));
    dispatch(muse::ui::UP_COMMAND);
    ASSERT_NO_FATAL_FAILURE(expectChannelFocus(0, "1"));

    for (const auto& command : { muse::ui::NEXT_PANEL_COMMAND, muse::ui::NEXT_SECTION_COMMAND }) {
        dispatch(command);
        ASSERT_NO_FATAL_FAILURE(expectChannelFocus(0, "1"));
        EXPECT_TRUE(m_popup->isOpened());
    }
}

TEST_F(RecordingChannelPopupTests, TriggerTogglesPendingSelectionWithoutClosing)
{
    EXPECT_CALL(*m_controller, apply(_, _)).Times(0);
    ASSERT_NO_FATAL_FAILURE(createView());
    ASSERT_NO_FATAL_FAILURE(openPopup());
    dispatch(muse::ui::DOWN_COMMAND);
    ASSERT_NO_FATAL_FAILURE(expectChannelFocus(1, "2"));

    for (const bool checked : { true, false }) {
        dispatch(muse::ui::TRIGGER_CONTROL_COMMAND);
        ASSERT_NO_FATAL_FAILURE(expectChannelFocus(1, "2"));
        EXPECT_TRUE(m_popup->isOpened());
        EXPECT_EQ(m_navigation->activeControl()->visualItem()->property("checked").toBool(), checked);
        EXPECT_EQ(m_model->inputChannelGroups().at(1).toMap().value("checked").toBool(), checked);
        EXPECT_EQ(m_applied.inputChannelSelection, (audio::InputChannelSelection { { { 0 } } }));
    }
}

TEST_F(RecordingChannelPopupTests, DeviceChangeKeepsReusedStereoLabelVisible)
{
    ON_CALL(*m_controller, inputChannelsAvailable()).WillByDefault(Return(4));
    ON_CALL(*m_controller, inputChannelsAvailable("Core Audio", _)).WillByDefault(Return(2));
    m_applied.inputChannelSelection = { { { 2, 3 } } };
    ASSERT_EQ(m_model->inputChannelSelectionSummary(), "3+4");
    ASSERT_NO_FATAL_FAILURE(createView());
    ASSERT_NO_FATAL_FAILURE(openPopup());
    dispatch(muse::ui::DOWN_COMMAND);
    dispatch(muse::ui::DOWN_COMMAND);
    ASSERT_NO_FATAL_FAILURE(expectChannelFocus(2, "3"));

    const QPointer<QQuickItem> checkBox = m_navigation->activeControl()->visualItem();
    ASSERT_TRUE(QTest::qWaitFor([checkBox]() { return checkBox->width() > 20; }));

    // The third row changes from mono 3 to stereo 1+2 without recreating its checkbox.
    m_model->inputDeviceSelected(2);
    ASSERT_TRUE(QMetaObject::invokeMethod(m_list, "forceLayout"));
    ASSERT_FALSE(checkBox.isNull());
    ASSERT_EQ(checkBox->property("text").toString(), "1+2");

    QQuickItem* label = nullptr;
    for (auto* item : checkBox->findChildren<QQuickItem*>()) {
        if (item->property("text").toString() == "1+2"
            && item->property("truncated").isValid()) {
            label = item;
            break;
        }
    }
    ASSERT_NE(label, nullptr);
    const qreal textWidth = QFontMetricsF(label->property("font").value<QFont>()).horizontalAdvance("1+2");
    EXPECT_TRUE(QTest::qWaitFor([label, textWidth]() {
        return label->isVisible() && label->width() >= textWidth
               && !label->property("truncated").toBool();
    })) << "label width=" << label->width() << ", text width=" << textWidth;
}

TEST_F(RecordingChannelPopupTests, KeyboardNavigationScrollsToOffscreenMonoAndStereoGroups)
{
    ON_CALL(*m_controller, inputChannelsAvailable()).WillByDefault(Return(20));
    ASSERT_NO_FATAL_FAILURE(createView());
    ASSERT_NO_FATAL_FAILURE(openPopup());
    int currentRow = 0;

    for (const int targetRow : { 16, 28 }) {
        SCOPED_TRACE(targetRow);
        while (currentRow < targetRow) {
            dispatch(muse::ui::DOWN_COMMAND);
            ++currentRow;
            const auto group = m_model->inputChannelGroups().at(currentRow).toMap();
            ASSERT_NO_FATAL_FAILURE(expectChannelFocus(currentRow, group.value("title").toString()));
        }

        const QPointer<QQuickItem> checkBox = m_navigation->activeControl()->visualItem();
        const qreal contentY = m_list->property("contentY").toReal();
        ASSERT_GT(contentY, 0.0);
        const qreal top = checkBox->mapToItem(m_list, QPointF()).y();
        EXPECT_GE(top, 0.0);
        EXPECT_LE(top + checkBox->height(), m_list->height());

        for (const bool checked : { true, false }) {
            dispatch(muse::ui::TRIGGER_CONTROL_COMMAND);
            EXPECT_TRUE(m_popup->isOpened());
            ASSERT_FALSE(checkBox.isNull());
            EXPECT_EQ(m_navigation->activeControl()->visualItem(), checkBox.data());
            EXPECT_TRUE(checkBox->hasFocus());
            EXPECT_EQ(checkBox->property("checked").toBool(), checked);
            EXPECT_DOUBLE_EQ(m_list->property("contentY").toReal(), contentY);
        }
    }
}

TEST_F(RecordingChannelPopupTests, EscapeRestoresButtonFocusAndPopupCanReopen)
{
    ASSERT_NO_FATAL_FAILURE(createView());
    ASSERT_NO_FATAL_FAILURE(openPopup());

    for (int attempt = 0; attempt < 2; ++attempt) {
        dispatch(muse::ui::DOWN_COMMAND);
        ASSERT_NO_FATAL_FAILURE(expectChannelFocus(1, "2"));
        dispatch(muse::ui::ESCAPE_COMMAND);
        ASSERT_FALSE(m_popup->isOpened());
        EXPECT_TRUE(m_window.isVisible());
        EXPECT_FALSE(m_view->property("closeRequested").toBool());
        ASSERT_NE(m_navigation->activeControl(), nullptr);
        EXPECT_EQ(m_navigation->activeSection()->name(), "PreferencesTestWindow");
        EXPECT_EQ(m_navigation->activeControl()->name(), "RecordingChannelsBox");

        dispatch(muse::ui::TRIGGER_CONTROL_COMMAND);
        ASSERT_TRUE(m_popup->isOpened());
        ASSERT_NO_FATAL_FAILURE(expectChannelFocus(0, "1"));
    }
}

TEST_F(RecordingChannelListModelTests, TogglingScrolledGroupsPreservesScrollPositionAndDelegates)
{
    ON_CALL(*m_controller, inputChannelsAvailable()).WillByDefault(Return(20));
    const auto view = createView();
    ASSERT_TRUE(view);
    auto* listModel = qobject_cast<QAbstractItemModel*>(view->property("model").value<QObject*>());
    ASSERT_NE(listModel, nullptr);
    int resets = 0;
    QObject::connect(listModel, &QAbstractItemModel::modelReset, view.get(), [&resets]() { ++resets; });

    for (const int channelCount : { 1, 2 }) {
        SCOPED_TRACE(channelCount);
        // Mono 17 and stereo 17+18 are both below the initial viewport.
        const int row = channelCount == 1 ? 16 : 28;
        const QPointer<QQuickItem> delegate = scrollToGroup(view.get(), row);
        ASSERT_FALSE(delegate.isNull());
        const double contentY = view->property("contentY").toDouble();
        ASSERT_GT(contentY, 0.0);
        delegate->forceActiveFocus();
        ASSERT_TRUE(delegate->hasFocus());

        for (const bool checked : { true, false }) {
            m_model->toggleInputChannelGroup(16, channelCount);
            ASSERT_TRUE(QMetaObject::invokeMethod(view.get(), "forceLayout"));

            EXPECT_DOUBLE_EQ(view->property("contentY").toDouble(), contentY);
            EXPECT_EQ(view->property("model").value<QObject*>(), listModel);
            EXPECT_EQ(resets, 0);
            ASSERT_FALSE(delegate.isNull());
            EXPECT_EQ(scrollToGroup(view.get(), row), delegate.data());
            EXPECT_EQ(delegate->property("checked").toBool(), checked);
            EXPECT_TRUE(delegate->hasFocus());
        }
    }
}

TEST_F(RecordingChannelListModelTests, OverlapReplacementUpdatesAllAffectedCheckmarksInPlace)
{
    ON_CALL(*m_controller, inputChannelsAvailable()).WillByDefault(Return(20));
    m_applied.inputChannelSelection = { { { 16 } }, { { 17 } } };
    const auto view = createView();
    ASSERT_TRUE(view);
    auto* listModel = qobject_cast<QAbstractItemModel*>(view->property("model").value<QObject*>());
    ASSERT_NE(listModel, nullptr);
    const int checkedRole = listModel->roleNames().key("checked");
    ASSERT_TRUE(listModel->data(listModel->index(16, 0), checkedRole).toBool());
    ASSERT_TRUE(listModel->data(listModel->index(17, 0), checkedRole).toBool());

    const QPointer<QQuickItem> delegate = scrollToGroup(view.get(), 28);
    ASSERT_FALSE(delegate.isNull());
    const double contentY = view->property("contentY").toDouble();
    m_model->toggleInputChannelGroup(16, 2);
    ASSERT_TRUE(QMetaObject::invokeMethod(view.get(), "forceLayout"));

    EXPECT_FALSE(listModel->data(listModel->index(16, 0), checkedRole).toBool());
    EXPECT_FALSE(listModel->data(listModel->index(17, 0), checkedRole).toBool());
    EXPECT_TRUE(listModel->data(listModel->index(28, 0), checkedRole).toBool());
    EXPECT_DOUBLE_EQ(view->property("contentY").toDouble(), contentY);
    ASSERT_FALSE(delegate.isNull());
    EXPECT_EQ(scrollToGroup(view.get(), 28), delegate.data());
}

TEST_F(RecordingChannelListModelTests, DeviceCapacityChangesResizeTheListAndRefreshSections)
{
    const auto view = createView();
    ASSERT_TRUE(view);
    auto* listModel = qobject_cast<QAbstractItemModel*>(view->property("model").value<QObject*>());
    ASSERT_NE(listModel, nullptr);
    ASSERT_EQ(listModel->rowCount(), 3);
    const int titleRole = listModel->roleNames().key("title");
    const int sectionRole = listModel->roleNames().key("sectionStart");

    for (const int capacity : { 20, 4, 1, 0, 2 }) {
        SCOPED_TRACE(capacity);
        ON_CALL(*m_controller, inputChannelsAvailable()).WillByDefault(Return(capacity));
        m_deviceListChanged.notify();

        EXPECT_EQ(listModel->rowCount(), capacity + capacity / 2);
        for (int row = 0; row < listModel->rowCount(); ++row) {
            EXPECT_EQ(listModel->data(listModel->index(row, 0), sectionRole).toBool(), row == 0 || row == capacity);
        }
        if (capacity >= 2) {
            EXPECT_EQ(listModel->data(listModel->index(capacity, 0), titleRole).toString(), "1+2");
        }
    }
}

TEST_F(CommonAudioApiConfigurationModelTests, EditingValues_IsPendingUntilApply)
{
    EXPECT_CALL(*m_controller, apply(_, _)).Times(0);

    m_model->outputDeviceSelected(2);
    m_model->toggleInputChannelGroup(0, 2);
    m_model->bufferLengthSelected("50");

    EXPECT_EQ(m_model->currentOutputDeviceIndex(), 2);
    EXPECT_EQ(m_model->inputChannelSelectionSummary(), "1+2");
    EXPECT_DOUBLE_EQ(m_model->bufferLength(), 50.0);
}

TEST_F(CommonAudioApiConfigurationModelTests, InputChannelGroupsExposeSectionAndSelectionRoles)
{
    ON_CALL(*m_controller, inputChannelsAvailable()).WillByDefault(Return(4));
    m_applied.inputChannelSelection = { { { 0 } }, { { 2, 3 } } };

    const QVariantList groups = m_model->inputChannelGroups();

    ASSERT_EQ(groups.size(), 6);
    const QStringList expectedTitles { "1", "2", "3", "4", "1+2", "3+4" };
    for (int index = 0; index < groups.size(); ++index) {
        const QVariantMap group = groups.at(index).toMap();
        EXPECT_EQ(group.value("title").toString(), expectedTitles.at(index));
        EXPECT_EQ(group.value("sectionStart").toBool(), index == 0 || index == 4);
        EXPECT_EQ(group.value("channelCount").toInt(), index < 4 ? 1 : 2);
    }
    EXPECT_TRUE(groups.at(0).toMap().value("checked").toBool());
    EXPECT_FALSE(groups.at(1).toMap().value("checked").toBool());
    EXPECT_FALSE(groups.at(2).toMap().value("checked").toBool());
    EXPECT_FALSE(groups.at(3).toMap().value("checked").toBool());
    EXPECT_FALSE(groups.at(4).toMap().value("checked").toBool());
    EXPECT_TRUE(groups.at(5).toMap().value("checked").toBool());
}

TEST_F(CommonAudioApiConfigurationModelTests, MixedSelectionSummaryAndApplyPreserveGroups)
{
    ON_CALL(*m_controller, inputChannelsAvailable()).WillByDefault(Return(4));

    m_model->toggleInputChannelGroup(2, 2);

    EXPECT_EQ(m_model->inputChannelSelectionSummary(), "1, 3+4");
    EXPECT_CALL(*m_controller, apply(_, _))
    .WillOnce([](const muse::modularity::ContextPtr&,
                 const audio::AudioConfigurationChange& change) {
        EXPECT_EQ(change.inputChannelSelection,
                  std::optional<audio::InputChannelSelection>(
                      { { { 0 } }, { { 2, 3 } } }));
        return audio::ApplyResult { audio::ApplyStatus::Applied };
    });
    EXPECT_TRUE(m_model->apply());
}

TEST_F(CommonAudioApiConfigurationModelTests, StereoToggleAtomicallyReplacesOverlappingMonos)
{
    ON_CALL(*m_controller, inputChannelsAvailable()).WillByDefault(Return(4));
    m_applied.inputChannelSelection = { { { 0 } }, { { 1 } }, { { 2, 3 } } };

    m_model->toggleInputChannelGroup(0, 2);

    EXPECT_EQ(m_model->inputChannelSelectionSummary(), "1+2, 3+4");
    EXPECT_CALL(*m_controller, apply(_, _))
    .WillOnce([](const muse::modularity::ContextPtr&,
                 const audio::AudioConfigurationChange& change) {
        EXPECT_EQ(change.inputChannelSelection,
                  std::optional<audio::InputChannelSelection>(
                      { { { 0, 1 } }, { { 2, 3 } } }));
        return audio::ApplyResult { audio::ApplyStatus::Applied };
    });
    EXPECT_TRUE(m_model->apply());
}

TEST_F(CommonAudioApiConfigurationModelTests, InvalidTogglesAreIgnoredAndLastGroupCannotBeRemoved)
{
    m_model->toggleInputChannelGroup(-1, 1);
    m_model->toggleInputChannelGroup(0, 3);
    m_model->toggleInputChannelGroup(1, 2);
    m_model->toggleInputChannelGroup(0, 1);

    EXPECT_EQ(m_model->inputChannelSelectionSummary(), "1");
    EXPECT_CALL(*m_controller, apply(_, _))
    .WillOnce([](const muse::modularity::ContextPtr&,
                 const audio::AudioConfigurationChange& change) {
        EXPECT_FALSE(change.inputChannelSelection);
        return audio::ApplyResult { audio::ApplyStatus::NoChange };
    });
    EXPECT_TRUE(m_model->apply());
}

TEST_F(CommonAudioApiConfigurationModelTests, Apply_SubmitsAllPendingFieldsInOneChangeSet)
{
    m_model->outputDeviceSelected(2);
    m_model->toggleInputChannelGroup(0, 2);
    m_model->bufferLengthSelected("50");

    EXPECT_CALL(*m_controller, apply(_, _))
    .WillOnce([](const muse::modularity::ContextPtr&, const audio::AudioConfigurationChange& change) {
        EXPECT_EQ(change.outputDevice, std::optional<std::string>("Headphones"));
        EXPECT_EQ(change.inputChannelSelection,
                  std::optional<audio::InputChannelSelection>({ { { 0, 1 } } }));
        EXPECT_EQ(change.bufferLength, std::optional<double>(50.0));
        EXPECT_FALSE(change.api.has_value());
        EXPECT_FALSE(change.inputDevice.has_value());
        return audio::ApplyResult { audio::ApplyStatus::Applied };
    });

    EXPECT_TRUE(m_model->apply());
}

TEST_F(CommonAudioApiConfigurationModelTests, ApplyFailure_KeepsPendingValuesAndDialogOpen)
{
    m_model->bufferLengthSelected("50");
    EXPECT_CALL(*m_controller, apply(_, _))
    .WillOnce(Return(audio::ApplyResult { audio::ApplyStatus::InternalError }));

    EXPECT_FALSE(m_model->apply());
    EXPECT_DOUBLE_EQ(m_model->bufferLength(), 50.0);
}

TEST_F(CommonAudioApiConfigurationModelTests, ApplySuccess_ClearsFieldsThatNormalizedToTheCurrentValue)
{
    m_model->outputDeviceSelected(2);
    m_model->bufferLengthSelected("50");
    EXPECT_CALL(*m_controller, apply(_, _))
    .WillOnce(Return(audio::ApplyResult { audio::ApplyStatus::Applied }))
    .WillOnce([](const muse::modularity::ContextPtr&, const audio::AudioConfigurationChange& change) {
        EXPECT_FALSE(change.api);
        EXPECT_FALSE(change.outputDevice);
        EXPECT_FALSE(change.inputDevice);
        EXPECT_FALSE(change.inputChannelSelection);
        EXPECT_FALSE(change.bufferLength);
        EXPECT_FALSE(change.automaticLatencyCompensation);
        EXPECT_FALSE(change.latencyCompensation);
        EXPECT_FALSE(change.defaultSampleRate);
        EXPECT_FALSE(change.defaultSampleFormat);
        EXPECT_FALSE(change.asioUseDeviceSampleRate);
        return audio::ApplyResult { audio::ApplyStatus::NoChange };
    });

    EXPECT_TRUE(m_model->apply());
    EXPECT_TRUE(m_model->apply());
}

TEST_F(CommonAudioApiConfigurationModelTests, ApiEdit_PreviewsTheSelectedApisDeviceList)
{
    EXPECT_CALL(*m_controller, apply(_, _)).Times(0);

    m_model->setCurrentAudioApiIndex(1);

    EXPECT_EQ(m_model->currentAudioApiIndex(), 1);
    // The applied devices do not exist under JACK, so both fall back to the system default.
    EXPECT_EQ(m_model->currentOutputDeviceIndex(), 0);
    EXPECT_EQ(m_model->currentInputDeviceIndex(), 0);
    EXPECT_EQ(m_model->outputDeviceList().size(), 3);
    EXPECT_EQ(m_model->inputChannelGroups().size(), 6);
}

TEST_F(CommonAudioApiConfigurationModelTests, InputDeviceEditPreviewsItsChannelCapacity)
{
    ON_CALL(*m_controller, inputChannelsAvailable()).WillByDefault(Return(4));
    ON_CALL(*m_controller, inputChannelsAvailable("Core Audio", _))
    .WillByDefault(Return(2));
    m_applied.inputChannelSelection = { { { 2, 3 } } };

    m_model->inputDeviceSelected(2);

    EXPECT_EQ(m_model->currentInputDeviceIndex(), 2);
    EXPECT_EQ(m_model->inputChannelGroups().size(), 3);
    EXPECT_EQ(m_model->inputChannelSelectionSummary(), "1");
    EXPECT_CALL(*m_controller, apply(_, _))
    .WillOnce([](const muse::modularity::ContextPtr&,
                 const audio::AudioConfigurationChange& change) {
        EXPECT_EQ(change.inputDevice,
                  std::optional<audio::AudioDeviceSelection>("USB Mic"));
        EXPECT_FALSE(change.inputChannelSelection);
        return audio::ApplyResult { audio::ApplyStatus::Applied };
    });
    EXPECT_TRUE(m_model->apply());
}

TEST_F(CommonAudioApiConfigurationModelTests, CapacityReductionPreviewsStereoPresetAndCancelRestoresSelection)
{
    ON_CALL(*m_controller, inputChannelsAvailable()).WillByDefault(Return(4));
    ON_CALL(*m_controller, inputChannelsAvailable("Core Audio", _)).WillByDefault(Return(2));
    m_applied.inputChannelSelection = audio::legacyInputChannelSelection(4);
    EXPECT_CALL(*m_controller, apply(_, _)).Times(0);

    m_model->inputDeviceSelected(2);

    EXPECT_EQ(m_model->inputChannelSelectionSummary(), "1+2");
    const auto groups = m_model->inputChannelGroups();
    ASSERT_EQ(groups.size(), 3);
    EXPECT_FALSE(groups[0].toMap()["checked"].toBool());
    EXPECT_FALSE(groups[1].toMap()["checked"].toBool());
    EXPECT_TRUE(groups[2].toMap()["checked"].toBool());

    m_model->reset();

    EXPECT_EQ(m_model->currentInputDeviceIndex(), 1);
    EXPECT_EQ(m_model->inputChannelSelectionSummary(), "1, 2, 3, 4");
    EXPECT_EQ(m_applied.inputChannelSelection, audio::legacyInputChannelSelection(4));
}

TEST_F(CommonAudioApiConfigurationModelTests, EditingBackToAppliedStateProducesAnEmptyChange)
{
    m_model->outputDeviceSelected(2);
    m_model->outputDeviceSelected(1);

    EXPECT_CALL(*m_controller, apply(_, _))
    .WillOnce([](const muse::modularity::ContextPtr&, const audio::AudioConfigurationChange& change) {
        EXPECT_FALSE(change.api);
        EXPECT_FALSE(change.outputDevice);
        EXPECT_FALSE(change.inputDevice);
        EXPECT_FALSE(change.inputChannelSelection);
        EXPECT_FALSE(change.bufferLength);
        EXPECT_FALSE(change.automaticLatencyCompensation);
        EXPECT_FALSE(change.latencyCompensation);
        EXPECT_FALSE(change.defaultSampleRate);
        EXPECT_FALSE(change.defaultSampleFormat);
        EXPECT_FALSE(change.asioUseDeviceSampleRate);
        return audio::ApplyResult { audio::ApplyStatus::NoChange };
    });

    EXPECT_TRUE(m_model->apply());
}

TEST_F(CommonAudioApiConfigurationModelTests,
       ExternalChangeDoesNotDiscardPendingEdit)
{
    m_model->outputDeviceSelected(2);
    m_applied.outputDevice = "External Output";
    audio::AudioConfigurationDelta delta;
    delta.fields = audio::fieldMask(audio::AudioConfigurationField::OutputDevice);

    m_configurationChanged.send(delta);

    EXPECT_EQ(m_model->currentOutputDeviceIndex(), 2);
    EXPECT_CALL(*m_controller, apply(_, _))
    .WillOnce([](const muse::modularity::ContextPtr&,
                 const audio::AudioConfigurationChange& change) {
        EXPECT_EQ(change.outputDevice,
                  std::optional<std::string>("Headphones"));
        return audio::ApplyResult { audio::ApplyStatus::Applied };
    });
    EXPECT_TRUE(m_model->apply());
}

TEST_F(CommonAudioApiConfigurationModelTests, ResetDiscardsPendingEdits)
{
    m_model->outputDeviceSelected(2);

    m_model->reset();

    EXPECT_EQ(m_model->currentOutputDeviceIndex(), 1);
    EXPECT_CALL(*m_controller, apply(_, _))
    .WillOnce([](const muse::modularity::ContextPtr&,
                 const audio::AudioConfigurationChange& change) {
        EXPECT_FALSE(change.outputDevice);
        return audio::ApplyResult { audio::ApplyStatus::NoChange };
    });
    EXPECT_TRUE(m_model->apply());
}

TEST_F(CommonAudioApiConfigurationModelTests, ResetDiscardsPendingInputSelection)
{
    m_model->toggleInputChannelGroup(0, 2);

    m_model->reset();

    EXPECT_EQ(m_model->inputChannelSelectionSummary(), "1");
    EXPECT_CALL(*m_controller, apply(_, _))
    .WillOnce([](const muse::modularity::ContextPtr&,
                 const audio::AudioConfigurationChange& change) {
        EXPECT_FALSE(change.inputChannelSelection);
        return audio::ApplyResult { audio::ApplyStatus::NoChange };
    });
    EXPECT_TRUE(m_model->apply());
}

TEST_F(CommonAudioApiConfigurationModelTests, ExternalSelectionChangeUpdatesTheSummary)
{
    ON_CALL(*m_controller, inputChannelsAvailable()).WillByDefault(Return(4));
    m_applied.inputChannelSelection = { { { 2, 3 } } };
    audio::AudioConfigurationDelta delta;
    delta.fields
        = audio::fieldMask(audio::AudioConfigurationField::InputChannelSelection);

    m_configurationChanged.send(delta);

    EXPECT_EQ(m_model->inputChannelSelectionSummary(), "3+4");
}

TEST_F(CommonAudioApiConfigurationModelTests, ExternalSampleRateChangeUpdatesOtherRateState)
{
    audio::AudioConfigurationDelta delta;
    delta.fields = audio::fieldMask(audio::AudioConfigurationField::DefaultSampleRate);

    m_applied.defaultSampleRate = 12345;
    m_configurationChanged.send(delta);
    EXPECT_TRUE(m_model->otherSampleRate());

    m_applied.defaultSampleRate = 48000;
    m_configurationChanged.send(delta);
    EXPECT_FALSE(m_model->otherSampleRate());
}

TEST_F(CommonAudioApiConfigurationModelTests, RestoreFailureIsAnAppliedResult)
{
    m_model->bufferLengthSelected("50");
    EXPECT_CALL(*m_controller, apply(_, _))
    .WillOnce(Return(audio::ApplyResult {
            audio::ApplyStatus::Applied,
            true }));

    EXPECT_TRUE(m_model->apply());
}

TEST_F(CommonAudioApiConfigurationModelTests, DeviceLists_StartWithTheSystemDefaultEntry)
{
    const QVariantList outputs = m_model->outputDeviceList();
    ASSERT_EQ(outputs.size(), 3);
    EXPECT_EQ(outputs.at(0).toString(), QString(SYSTEM_DEFAULT));
    EXPECT_EQ(outputs.at(1).toString(), QString("Built-in Output"));
    EXPECT_EQ(outputs.at(2).toString(), QString("Headphones"));

    const QVariantList inputs = m_model->inputDeviceList();
    ASSERT_EQ(inputs.size(), 3);
    EXPECT_EQ(inputs.at(0).toString(), QString(SYSTEM_DEFAULT));
    EXPECT_EQ(inputs.at(1).toString(), QString("Built-in Mic"));
    EXPECT_EQ(inputs.at(2).toString(), QString("USB Mic"));
}

TEST_F(CommonAudioApiConfigurationModelTests, DeviceLists_NoDevices_AreEmptyAndHaveNoCurrentIndex)
{
    ON_CALL(*m_controller, outputDevices("Core Audio"))
    .WillByDefault(Return(std::vector<std::string> {}));
    ON_CALL(*m_controller, inputDevices("Core Audio"))
    .WillByDefault(Return(std::vector<std::string> {}));

    EXPECT_TRUE(m_model->outputDeviceList().isEmpty());
    EXPECT_EQ(m_model->currentOutputDeviceIndex(), -1);
    EXPECT_TRUE(m_model->inputDeviceList().isEmpty());
    EXPECT_EQ(m_model->currentInputDeviceIndex(), -1);
}

TEST_F(CommonAudioApiConfigurationModelTests, CurrentDeviceIndex_IsShiftedByTheSystemDefaultEntry)
{
    EXPECT_EQ(m_model->currentOutputDeviceIndex(), 1);
    EXPECT_EQ(m_model->currentInputDeviceIndex(), 1);

    m_applied.outputDevice = std::nullopt;
    EXPECT_EQ(m_model->currentOutputDeviceIndex(), 0);

    m_applied.outputDevice = "Unplugged device";
    EXPECT_EQ(m_model->currentOutputDeviceIndex(), 0);
}

TEST_F(CommonAudioApiConfigurationModelTests, DeviceSelected_SystemDefaultEntry_StagesTheDefaultSelection)
{
    m_model->outputDeviceSelected(0);

    EXPECT_EQ(m_model->currentOutputDeviceIndex(), 0);
    EXPECT_CALL(*m_controller, apply(_, _))
    .WillOnce([](const muse::modularity::ContextPtr&, const audio::AudioConfigurationChange& change) {
        EXPECT_EQ(change.outputDevice, std::optional<audio::AudioDeviceSelection>(audio::AudioDeviceSelection {}));
        return audio::ApplyResult { audio::ApplyStatus::Applied };
    });

    EXPECT_TRUE(m_model->apply());
}

TEST_F(CommonAudioApiConfigurationModelTests, DeviceSelected_OutOfRange_IsIgnored)
{
    m_model->outputDeviceSelected(3);
    m_model->outputDeviceSelected(-1);
    m_model->inputDeviceSelected(3);
    m_model->inputDeviceSelected(-1);

    EXPECT_EQ(m_model->currentOutputDeviceIndex(), 1);
    EXPECT_EQ(m_model->currentInputDeviceIndex(), 1);
    EXPECT_CALL(*m_controller, apply(_, _))
    .WillOnce([](const muse::modularity::ContextPtr&, const audio::AudioConfigurationChange& change) {
        EXPECT_FALSE(change.outputDevice);
        EXPECT_FALSE(change.inputDevice);
        return audio::ApplyResult { audio::ApplyStatus::NoChange };
    });

    EXPECT_TRUE(m_model->apply());
}

//! NOTE A real device may carry the same name as the "System default" entry;
//! index-based selection must keep the two distinguishable
TEST_F(CommonAudioApiConfigurationModelTests, DeviceNamedSystemDefault_IsDistinctFromTheDefaultEntry)
{
    ON_CALL(*m_controller, outputDevices("Core Audio"))
    .WillByDefault(Return(std::vector<std::string> { SYSTEM_DEFAULT }));
    m_applied.outputDevice = SYSTEM_DEFAULT;

    EXPECT_EQ(m_model->currentOutputDeviceIndex(), 1);

    m_model->outputDeviceSelected(0);

    EXPECT_EQ(m_model->currentOutputDeviceIndex(), 0);
    EXPECT_CALL(*m_controller, apply(_, _))
    .WillOnce([](const muse::modularity::ContextPtr&, const audio::AudioConfigurationChange& change) {
        EXPECT_EQ(change.outputDevice, std::optional<audio::AudioDeviceSelection>(audio::AudioDeviceSelection {}));
        return audio::ApplyResult { audio::ApplyStatus::Applied };
    });

    EXPECT_TRUE(m_model->apply());
}

TEST_F(CommonAudioApiConfigurationModelTests, DeviceLists_ShowTheResolvedSystemDefaultName)
{
    ON_CALL(*m_controller, systemDefaultOutputDevice("Core Audio"))
    .WillByDefault(Return(std::string("Headphones")));
    ON_CALL(*m_controller, systemDefaultInputDevice("Core Audio"))
    .WillByDefault(Return(std::string("USB Mic")));

    EXPECT_EQ(m_model->outputDeviceList().at(0).toString(), QString("System default: Headphones"));
    EXPECT_EQ(m_model->inputDeviceList().at(0).toString(), QString("System default: USB Mic"));
}

TEST_F(CommonAudioApiConfigurationModelTests, DeviceLists_ResolvedSystemDefaultFollowsThePreviewedApi)
{
    ON_CALL(*m_controller, systemDefaultOutputDevice("JACK"))
    .WillByDefault(Return(std::string("JACK Out 2")));

    m_model->setCurrentAudioApiIndex(1);

    EXPECT_EQ(m_model->outputDeviceList().at(0).toString(), QString("System default: JACK Out 2"));
}

TEST_F(CommonAudioApiConfigurationModelTests, ExternalDeviceChangeIsForwardedAsAnIndexChangeSignal)
{
    int outputChangedCount = 0;
    int inputChangedCount = 0;
    QObject::connect(m_model.get(), &CommonAudioApiConfigurationModel::currentOutputDeviceIndexChanged,
                     m_model.get(), [&outputChangedCount]() { ++outputChangedCount; });
    QObject::connect(m_model.get(), &CommonAudioApiConfigurationModel::currentInputDeviceIndexChanged,
                     m_model.get(), [&inputChangedCount]() { ++inputChangedCount; });

    audio::AudioConfigurationDelta delta;
    delta.fields = audio::fieldMask(audio::AudioConfigurationField::OutputDevice);
    m_configurationChanged.send(delta);

    EXPECT_EQ(outputChangedCount, 1);
    EXPECT_EQ(inputChangedCount, 1);
}
}
