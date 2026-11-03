/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>

#include "context/internal/shortcutresolver.h"

#include "mocks/uicontextresolvermock.h"

using ::testing::NiceMock;
using ::testing::Return;

using namespace muse;
using namespace muse::shortcuts;

namespace au::context {
class ShortcutResolverTests : public ::testing::Test
{
public:
    void SetUp() override
    {
        m_resolver = std::make_unique<ShortcutResolver>(muse::modularity::globalCtx());

        m_uiContextResolver = std::make_shared<NiceMock<UiContextResolverMock> >();
        m_resolver->uiContextResolver.set(m_uiContextResolver);
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

    std::unique_ptr<ShortcutResolver> m_resolver;
    std::shared_ptr<NiceMock<UiContextResolverMock> > m_uiContextResolver;
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

TEST_F(ShortcutResolverTests, PlaybackWinsOverAppTrigger)
{
    setProjectFocused(false);

    const ShortcutList list = {
        make("APP", "command://app/trigger"),
        make("PLAYBACK", "command://playback/toggle-play-stop"),
    };

    EXPECT_EQ(m_resolver->selectActive(list).command, rcommand::Command("command://playback/toggle-play-stop"));
}

TEST_F(ShortcutResolverTests, RecordWinsOverAppTrigger)
{
    setProjectFocused(true);

    const ShortcutList list = {
        make("APP", "command://app/trigger"),
        make("RECORD", "command://record/stop"),
    };

    EXPECT_EQ(m_resolver->selectActive(list).command, rcommand::Command("command://record/stop"));
}

TEST_F(ShortcutResolverTests, CommandWinsOverNoneCommandInSameScope)
{
    setProjectFocused(false);

    const ShortcutList list = {
        make("APP", rcommand::NONE_COMMAND.toString()),
        make("APP", "command://app/trigger"),
    };

    EXPECT_EQ(m_resolver->selectActive(list).command, rcommand::Command("command://app/trigger"));
}

TEST_F(ShortcutResolverTests, EmptyListHasNoActiveShortcut)
{
    EXPECT_FALSE(m_resolver->selectActive({}).isValid());
}

TEST_F(ShortcutResolverTests, ScopeTitleIsCaseInsensitive)
{
    EXPECT_EQ(m_resolver->scopeTitle("timeline").str, m_resolver->scopeTitle("TIMELINE").str);
}
}
