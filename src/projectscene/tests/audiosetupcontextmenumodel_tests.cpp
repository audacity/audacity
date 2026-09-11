/*
 * Audacity: A Digital Audio Editor
 */
#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include "actions/tests/mocks/actionsdispatchermock.h"
#include "audio/tests/mocks/audiodrivercontrollermock.h"
#include "projectscene/view/toolbars/audiosetupcontextmenumodel.h"
#include "testing/testcontext.h"

using ::testing::NiceMock;
using ::testing::Return;
using ::testing::ReturnRef;

namespace au::projectscene {
class ShortcutsRegisterMock : public muse::shortcuts::IShortcutsRegister
{
public:
    MOCK_METHOD(const muse::shortcuts::ShortcutList&, shortcuts, (), (const, override));
    MOCK_METHOD(muse::Ret, setShortcuts, (const muse::shortcuts::ShortcutList&), (override));
    MOCK_METHOD(void, resetShortcuts, (), (override));
    MOCK_METHOD(muse::async::Notification, shortcutsChanged, (), (const, override));
    MOCK_METHOD(muse::Ret, setAdditionalShortcuts, (const std::string&, const muse::shortcuts::ShortcutList&), (override));
    MOCK_METHOD(const muse::shortcuts::Shortcut&, shortcut, (const std::string&), (const, override));
    MOCK_METHOD(const muse::shortcuts::Shortcut&, defaultShortcut, (const std::string&), (const, override));
    MOCK_METHOD(bool, isRegistered, (const std::string&), (const, override));
    MOCK_METHOD(muse::shortcuts::ShortcutList, shortcutsForSequence, (const std::string&), (const, override));
    MOCK_METHOD(muse::Ret, importFromFile, (const muse::io::path_t&), (override));
    MOCK_METHOD(muse::Ret, exportToFile, (const muse::io::path_t&), (const, override));
    MOCK_METHOD(bool, active, (), (override));
    MOCK_METHOD(void, setActive, (bool), (override));
    MOCK_METHOD(muse::async::Notification, activeChanged, (), (const, override));
    MOCK_METHOD(void, reload, (bool), (override));
};

class UiActionsRegisterMock : public muse::ui::IUiActionsRegister
{
public:
    MOCK_METHOD(void, reg, (const muse::ui::IUiActionsModulePtr&), (override));
    MOCK_METHOD(void, unreg, (const muse::ui::IUiActionsModulePtr&), (override));
    MOCK_METHOD(std::vector<muse::ui::UiAction>, actionList, (), (const, override));
    MOCK_METHOD(const muse::ui::UiAction&, action,
                (const muse::actions::ActionCode&), (const, override));
    MOCK_METHOD(const muse::actions::ActionCode&, parentActionCode,
                (const muse::actions::ActionCode&), (const, override));
    MOCK_METHOD(muse::async::Channel<muse::ui::UiActionList>, actionsChanged,
                (), (const, override));
    MOCK_METHOD(muse::ui::UiActionState, actionState,
                (const muse::actions::ActionCode&), (const, override));
    MOCK_METHOD(muse::async::Channel<muse::actions::ActionCodeList>,
                actionStateChanged, (), (const, override));
};

class AudioSetupContextMenuModelTests : public ::testing::Test
{
public:
    void SetUp() override
    {
        m_context = au::testutils::makeTestContext();
        m_model.setContext(m_context);
        m_controller
            = std::make_shared<NiceMock<audio::AudioDriverControllerMock> >();
        m_uiActionsRegister
            = std::make_shared<NiceMock<UiActionsRegisterMock> >();
        m_shortcutsRegister = std::make_shared<NiceMock<ShortcutsRegisterMock> >();
        m_dispatcher = std::make_shared<NiceMock<muse::actions::ActionsDispatcherMock> >();
        m_model.audioDriverController.set(m_controller);
        m_model.uiActionsRegister.set(m_uiActionsRegister);
        m_model.shortcutsRegister.set(m_shortcutsRegister);
        m_model.dispatcher.set(m_dispatcher);

        ON_CALL(*m_shortcutsRegister, shortcut(testing::_))
        .WillByDefault(ReturnRef(m_shortcut));

        m_inputChannelsAction.code
            = "action://playback/change-input-channels";
        m_audioSettingsAction.code = "audio-settings";
        ON_CALL(*m_uiActionsRegister, action(testing::_))
        .WillByDefault(ReturnRef(m_inputChannelsAction));
        ON_CALL(*m_uiActionsRegister, action("audio-settings"))
        .WillByDefault(ReturnRef(m_audioSettingsAction));
        ON_CALL(*m_uiActionsRegister, actionState(testing::_))
        .WillByDefault(Return(muse::ui::UiActionState::make_enabled()));

        setSelection({ { { 0 } }, { { 2, 3 } } });
    }

    void TearDown() override
    {
        muse::modularity::removeIoC(m_context);
    }

    void setSelection(const audio::InputChannelSelection& selection, int availableChannels = 4)
    {
        audio::AudioConfiguration configuration;
        configuration.inputChannelSelection = selection;
        ON_CALL(*m_controller, configuration()).WillByDefault(Return(configuration));
        ON_CALL(*m_controller, inputChannelsAvailable()).WillByDefault(Return(availableChannels));
    }

    muse::uicomponents::MenuItem* inputChannelsMenu()
    {
        auto* menu = m_model.makeInputChannelsMenu();
        m_model.setItems({ menu });
        return menu;
    }

    AudioSetupContextMenuModel m_model;
    muse::modularity::ContextPtr m_context;
    std::shared_ptr<NiceMock<audio::AudioDriverControllerMock> > m_controller;
    std::shared_ptr<NiceMock<UiActionsRegisterMock> > m_uiActionsRegister;
    std::shared_ptr<NiceMock<ShortcutsRegisterMock> > m_shortcutsRegister;
    std::shared_ptr<NiceMock<muse::actions::ActionsDispatcherMock> > m_dispatcher;
    muse::ui::UiAction m_inputChannelsAction;
    muse::ui::UiAction m_audioSettingsAction;
    muse::shortcuts::Shortcut m_shortcut;
};

TEST_F(AudioSetupContextMenuModelTests, InputChannelsMenuOffersCountPresetsAndCustomCommand)
{
    const auto* menu = inputChannelsMenu();
    const auto items = menu->subitems();

    EXPECT_EQ(menu->translatedTitle(), "Recording channels: Custom");
    ASSERT_EQ(items.size(), 6);
    const QStringList expectedTitles { "1 (Mono) Recording channel", "2 (Stereo) Recording channels", "3", "4", "", "Custom..." };
    for (int index = 0; index < items.size(); ++index) {
        EXPECT_EQ(items.at(index)->translatedTitle(), expectedTitles.at(index));
        EXPECT_FALSE(items.at(index)->checked());
        EXPECT_EQ(items.at(index)->checkable(), index < 4);
    }

    EXPECT_FALSE(items.at(4)->isValid());
    EXPECT_EQ(items.last()->actionCode(), "audio-settings");
    EXPECT_EQ(items.last()->id(), "customInputChannels");
}

TEST_F(AudioSetupContextMenuModelTests, InputChannelsMenuEncodesOneBasedCountInActionQuery)
{
    const auto items = inputChannelsMenu()->subitems();

    for (int index = 0; index < 4; ++index) {
        const auto query = items.at(index)->query();
        EXPECT_EQ(query.uri().toString(), "action://playback/change-input-channels");
        EXPECT_EQ(query.param("input-channels_index").toInt(), index + 1);
        EXPECT_FALSE(query.contains("first-channel-index"));
        EXPECT_FALSE(query.contains("channel-count"));
        EXPECT_EQ(items.at(index)->id(), QString::fromStdString(query.toString()));
    }
}

TEST_F(AudioSetupContextMenuModelTests, ExactPresetSelectionChecksOnlyTheMatchingCount)
{
    const std::vector<audio::InputChannelSelection> presets {
        { { { 0 } } },
        { { { 0, 1 } } },
        { { { 0 } }, { { 1 } }, { { 2 } } },
        { { { 0 } }, { { 1 } }, { { 2 } }, { { 3 } } }
    };
    for (size_t presetIndex = 0; presetIndex < presets.size(); ++presetIndex) {
        SCOPED_TRACE(presetIndex);
        setSelection(presets[presetIndex]);
        const auto* menu = inputChannelsMenu();
        EXPECT_EQ(menu->translatedTitle(), "Recording channels");
        const auto items = menu->subitems();
        for (int index = 0; index < items.size(); ++index) {
            EXPECT_EQ(items.at(index)->checked(), index == static_cast<int>(presetIndex));
        }
    }
}

TEST_F(AudioSetupContextMenuModelTests, NonPresetChannelsOrGroupingShowCustomState)
{
    const std::vector<audio::InputChannelSelection> selections {
        { { { 2 } } },
        { { { 2, 3 } } },
        { { { 0 } }, { { 1 } } },
        { { { 0, 1 } }, { { 2 } } },
        { { { 0, 1 } }, { { 2, 3 } } }
    };
    for (const auto& selection : selections) {
        SCOPED_TRACE(::testing::PrintToString(selection));
        setSelection(selection);
        const auto* menu = inputChannelsMenu();
        EXPECT_EQ(menu->translatedTitle(), "Recording channels: Custom");
        for (const auto* item : menu->subitems()) {
            EXPECT_FALSE(item->checked());
        }
    }
}

TEST_F(AudioSetupContextMenuModelTests, NoInputsOffersOnlyCustomWithoutASeparator)
{
    setSelection({}, 0);
    const auto* menu = inputChannelsMenu();

    EXPECT_EQ(menu->translatedTitle(), "Recording channels");
    ASSERT_EQ(menu->subitems().size(), 1);
    const auto* item = menu->subitems().first();
    EXPECT_EQ(item->translatedTitle(), "Custom...");
    EXPECT_EQ(item->actionCode(), "audio-settings");
    EXPECT_TRUE(item->isValid());
    EXPECT_TRUE(item->enabled());
    EXPECT_FALSE(item->checkable());
    EXPECT_FALSE(item->checked());
}

TEST_F(AudioSetupContextMenuModelTests, NoInputsDoesNotDescribeAnUnavailableSelectionAsCustom)
{
    setSelection({ { { 2, 3 } } }, 0);
    const auto* menu = inputChannelsMenu();

    EXPECT_EQ(menu->translatedTitle(), "Recording channels");
    ASSERT_EQ(menu->subitems().size(), 1);
    EXPECT_EQ(menu->subitems().first()->actionCode(), "audio-settings");
}

TEST_F(AudioSetupContextMenuModelTests, EmptySelectionDoesNotCheckAPresetOrShowCustomState)
{
    setSelection({});
    const auto* menu = inputChannelsMenu();

    EXPECT_EQ(menu->translatedTitle(), "Recording channels");
    for (const auto* item : menu->subitems()) {
        EXPECT_FALSE(item->checked());
    }
}

TEST_F(AudioSetupContextMenuModelTests, MonoDeviceOffersOnlyMonoPresetAndCustom)
{
    setSelection({ { { 0 } } }, 1);
    const auto* menu = inputChannelsMenu();

    EXPECT_EQ(menu->translatedTitle(), "Recording channels");
    ASSERT_EQ(menu->subitems().size(), 3);
    EXPECT_TRUE(menu->subitems().first()->checked());
    EXPECT_FALSE(menu->subitems().at(1)->isValid());
    EXPECT_EQ(menu->subitems().last()->translatedTitle(), "Custom...");
}

TEST_F(AudioSetupContextMenuModelTests, CustomDispatchesAudioSettingsWithoutChangingSelection)
{
    EXPECT_CALL(*m_dispatcher, dispatch(testing::Matcher<const muse::actions::ActionQuery&>(testing::_))).Times(0);
    EXPECT_CALL(*m_dispatcher, dispatch(testing::Matcher<const muse::actions::ActionCode&>(testing::_))).Times(0);
    for (const auto& selection : { audio::InputChannelSelection { { { 0, 1 } } }, audio::InputChannelSelection { { { 2, 3 } } } }) {
        setSelection(selection);
        const auto* item = inputChannelsMenu()->subitems().last();

        EXPECT_FALSE(item->checkable());
        EXPECT_FALSE(item->checked());
        EXPECT_CALL(*m_controller, apply(testing::_, testing::_)).Times(0);
        EXPECT_CALL(*m_dispatcher, dispatch(muse::actions::ActionCode("audio-settings"), testing::_)).Times(1);

        m_model.handleMenuItem(item->id());

        EXPECT_EQ(m_controller->configuration().inputChannelSelection, selection);
    }
}

TEST_F(AudioSetupContextMenuModelTests, PresetDispatchesTheCountQuery)
{
    const auto* item = inputChannelsMenu()->subitems().at(1);
    EXPECT_CALL(*m_dispatcher, dispatch(testing::Matcher<const muse::actions::ActionQuery&>(
                                            testing::Property(&muse::actions::ActionQuery::toString,
                                                              "action://playback/change-input-channels?input-channels_index=2"))))
    .Times(1);

    m_model.handleMenuItem(item->id());
}
}
