/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>

#include "context/internal/shortcutresolver.h"

#include "framework/ui/tests/mocks/navigationmocks.h"

#include "mocks/uicontextresolvermock.h"

using ::testing::NiceMock;
using ::testing::Return;

using namespace muse;
using namespace muse::shortcuts;

static const std::string MODULE_NAME("context_tests");

namespace au::context {
class ShortcutResolverTests : public ::testing::Test
{
public:
    void SetUp() override
    {
        m_uiContextResolver = std::make_shared<NiceMock<UiContextResolverMock> >();
        modularity::ioc(m_iocCtx)->registerExport<IUiContextResolver>(MODULE_NAME, m_uiContextResolver);

        m_navigationController = std::make_shared<NiceMock<muse::ui::NavigationControllerMock> >();
        modularity::ioc(m_iocCtx)->registerExport<muse::ui::INavigationController>(MODULE_NAME, m_navigationController);

        m_resolver = std::make_unique<ShortcutResolver>(m_iocCtx);

        setProjectPageShown(true);
    }

    void TearDown() override
    {
        m_resolver.reset();
        modularity::removeIoC(m_iocCtx);
    }

    void setActiveSection(const QString& name)
    {
        ON_CALL(m_section, name()).WillByDefault(Return(name));
        ON_CALL(*m_navigationController, activeSection()).WillByDefault(Return(&m_section));
    }

    void setProjectPageShown(bool shown)
    {
        ON_CALL(*m_uiContextResolver, matchWithCurrent(UiCtxProjectOpened)).WillByDefault(Return(shown));
    }

    void setProjectFocused(bool focused)
    {
        ON_CALL(*m_uiContextResolver, matchWithCurrent(UiCtxProjectFocused)).WillByDefault(Return(focused));
    }

    static Shortcut make(const std::string& scope, const std::string& command)
    {
        Shortcut sc;
        sc.scope = scope;
        sc.command = rcommand::Command(command);
        return sc;
    }

    const modularity::ContextPtr m_iocCtx = std::make_shared<modularity::Context>(1);
    std::unique_ptr<ShortcutResolver> m_resolver;
    std::shared_ptr<NiceMock<UiContextResolverMock> > m_uiContextResolver;
    std::shared_ptr<NiceMock<muse::ui::NavigationControllerMock> > m_navigationController;
    NiceMock<muse::ui::NavigationSectionMock> m_section;

    const ShortcutList m_shiftEnter = {
        make("NAVIGATION", "command://navigation/trigger-control"),
        make("TRACK_NAVIGATION_SECTION", "command://track-navigation/range-selection"),
    };
};

TEST_F(ShortcutResolverTests, TimelineWinsOverNavigationWhenProjectFocused)
{
    setProjectFocused(true);

    const ShortcutList list = {
        make("NAVIGATION", "command://navigation/left"),
        make("TIMELINE", "command://projectscene/play-position-decrease"),
    };

    EXPECT_EQ(m_resolver->selectActive(list).command, rcommand::Command("command://projectscene/play-position-decrease"));
}

TEST_F(ShortcutResolverTests, NavigationWinsWhenProjectNotFocused)
{
    setProjectFocused(false);

    const ShortcutList list = {
        make("NAVIGATION", "command://navigation/left"),
        make("TIMELINE", "command://projectscene/play-position-decrease"),
    };

    EXPECT_EQ(m_resolver->selectActive(list).command, rcommand::Command("command://navigation/left"));
}

TEST_F(ShortcutResolverTests, TimelineWinsOverModuleScopeWhenProjectFocused)
{
    setProjectFocused(true);

    const ShortcutList list = {
        make("TIMELINE", "command://trackedit/track-view-item-extend-left"),
        make("PROJECTSCENE", "command://projectscene/selection-extend-left"),
    };

    EXPECT_EQ(m_resolver->selectActive(list).command, rcommand::Command("command://trackedit/track-view-item-extend-left"));
}

TEST_F(ShortcutResolverTests, ModuleScopeWinsWhenProjectNotFocused)
{
    setProjectFocused(false);

    const ShortcutList list = {
        make("TIMELINE", "command://trackedit/track-view-item-extend-left"),
        make("PROJECTSCENE", "command://projectscene/selection-extend-left"),
    };

    EXPECT_EQ(m_resolver->selectActive(list).command, rcommand::Command("command://projectscene/selection-extend-left"));
}

TEST_F(ShortcutResolverTests, NoActiveShortcutWhenOnlyTimelineAndProjectNotFocused)
{
    setProjectFocused(false);

    const ShortcutList list = {
        make("TIMELINE", "command://projectscene/toggle-split-tool"),
    };

    EXPECT_FALSE(m_resolver->selectActive(list).isValid());
}

TEST_F(ShortcutResolverTests, PlaybackWinsOverNavigationTrigger)
{
    setProjectFocused(false);

    const ShortcutList list = {
        make("NAVIGATION", "command://navigation/trigger-control"),
        make("PLAYBACK", "command://playback/toggle-play-stop"),
    };

    EXPECT_EQ(m_resolver->selectActive(list).command, rcommand::Command("command://playback/toggle-play-stop"));
}

TEST_F(ShortcutResolverTests, RecordWinsOverNavigationTrigger)
{
    setProjectFocused(true);

    const ShortcutList list = {
        make("NAVIGATION", "command://navigation/trigger-control"),
        make("RECORD", "command://record/stop"),
    };

    EXPECT_EQ(m_resolver->selectActive(list).command, rcommand::Command("command://record/stop"));
}

TEST_F(ShortcutResolverTests, CommandWinsOverNoneCommandInSameScope)
{
    setProjectFocused(false);

    const ShortcutList list = {
        make("APP", rcommand::NONE_COMMAND.toString()),
        make("APP", "command://app/quit"),
    };

    EXPECT_EQ(m_resolver->selectActive(list).command, rcommand::Command("command://app/quit"));
}

TEST_F(ShortcutResolverTests, EmptyListHasNoActiveShortcut)
{
    EXPECT_FALSE(m_resolver->selectActive({}).isValid());
}

TEST_F(ShortcutResolverTests, ScopeTitleIsCaseInsensitive)
{
    EXPECT_EQ(m_resolver->scopeTitle("timeline").str, m_resolver->scopeTitle("TIMELINE").str);
}

TEST_F(ShortcutResolverTests, TrackNavigationSectionWinsWhenTrackViewFocused)
{
    setProjectFocused(true);
    setActiveSection("TrackViewSection");

    EXPECT_EQ(m_resolver->selectActive(m_shiftEnter).command, rcommand::Command("command://track-navigation/range-selection"));
}

TEST_F(ShortcutResolverTests, NavigationWinsWhenTimelineRulerFocused)
{
    setProjectFocused(true);
    setActiveSection("TimelineSection");

    EXPECT_EQ(m_resolver->selectActive(m_shiftEnter).command, rcommand::Command("command://navigation/trigger-control"));
}

TEST_F(ShortcutResolverTests, NavigationWinsWhenTrackHeaderControlsFocused)
{
    setProjectFocused(false);
    setActiveSection("TrackViewSection");

    EXPECT_EQ(m_resolver->selectActive(m_shiftEnter).command, rcommand::Command("command://navigation/trigger-control"));
}

TEST_F(ShortcutResolverTests, NavigationWinsWhenNoSectionIsActive)
{
    setProjectFocused(false);

    EXPECT_EQ(m_resolver->selectActive(m_shiftEnter).command, rcommand::Command("command://navigation/trigger-control"));
}

TEST_F(ShortcutResolverTests, SpaceTriggersControlOnHomePage)
{
    setProjectPageShown(false);
    setProjectFocused(false);

    const ShortcutList list = {
        make("NAVIGATION", "command://navigation/trigger-control"),
        make("PLAYBACK", "command://playback/toggle-play-stop"),
        make("RECORD", "command://record/stop"),
    };

    EXPECT_EQ(m_resolver->selectActive(list).command, rcommand::Command("command://navigation/trigger-control"));
}

TEST_F(ShortcutResolverTests, ProjectPageShortcutsDoNothingOnHomePage)
{
    setProjectPageShown(false);
    setProjectFocused(false);

    for (const std::string scope : { "TRACKEDIT", "PLAYBACK", "RECORD", "PROJECTSCENE", "EFFECTS", "SPECTROGRAM", "TRACK-NAVIGATION" }) {
        EXPECT_FALSE(m_resolver->selectActive({ make(scope, "command://test/command") }).isValid()) << scope;
    }
}

TEST_F(ShortcutResolverTests, ProjectAndAppShortcutsWorkOnHomePage)
{
    setProjectPageShown(false);
    setProjectFocused(false);

    EXPECT_EQ(m_resolver->selectActive({ make("PROJECT", "command://project/new") }).command, rcommand::Command("command://project/new"));
    EXPECT_EQ(m_resolver->selectActive({ make("APP", "command://app/preferences") }).command,
              rcommand::Command("command://app/preferences"));
}
}
