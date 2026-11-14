/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>

#include "trackedit/internal/trackeditcommandsstate.h"
#include "trackedit/trackeditcommands.h"

#include "mocks/selectioncontrollermock.h"
#include "mocks/tracknavigationcontrollermock.h"
#include "mocks/trackeditinteractionmock.h"
#include "context/tests/mocks/globalcontextmock.h"
#include "project/tests/mocks/audacityprojectmock.h"

using ::testing::NiceMock;
using ::testing::Return;

namespace au::trackedit {
class TrackeditCommandsStateTests : public ::testing::Test
{
public:
    void SetUp() override
    {
        m_globalContext = std::make_shared<NiceMock<context::GlobalContextMock> >();
        m_project = std::make_shared<NiceMock<project::AudacityProjectMock> >();
        m_selectionController = std::make_shared<NiceMock<SelectionControllerMock> >();
        m_trackNavigationController = std::make_shared<NiceMock<TrackNavigationControllerMock> >();
        m_trackeditInteraction = std::make_shared<NiceMock<TrackeditInteractionMock> >();

        m_testCtx = std::make_shared<muse::modularity::Context>(999);
        auto ioc = muse::modularity::ioc(m_testCtx);
        ioc->registerExportNoDelete<context::IGlobalContext>("utests", m_globalContext.get());
        ioc->registerExportNoDelete<ISelectionController>("utests", m_selectionController.get());
        ioc->registerExportNoDelete<ITrackNavigationController>("utests", m_trackNavigationController.get());
        ioc->registerExportNoDelete<ITrackeditInteraction>("utests", m_trackeditInteraction.get());

        m_state = std::make_shared<TrackeditCommandsState>(m_testCtx);

        ON_CALL(*m_globalContext, currentProject()).WillByDefault(Return(m_project));
        ON_CALL(*m_trackNavigationController, focus()).WillByDefault(Return(TrackFocus::track(INVALID_TRACK)));
        ON_CALL(*m_selectionController, selectedClips()).WillByDefault(Return(ClipKeyList {}));
        ON_CALL(*m_selectionController, selectedLabels()).WillByDefault(Return(LabelKeyList {}));
    }

    void TearDown() override
    {
        m_state.reset();

        auto ioc = muse::modularity::ioc(m_testCtx);
        ioc->unregister<context::IGlobalContext>("utests");
        ioc->unregister<ISelectionController>("utests");
        ioc->unregister<ITrackNavigationController>("utests");
        ioc->unregister<ITrackeditInteraction>("utests");

        muse::modularity::removeIoC(m_testCtx);
    }

    bool enabled(const muse::rcommand::Command& command) const
    {
        return m_state->commandState(command).enabled;
    }

    std::shared_ptr<muse::modularity::Context> m_testCtx;
    std::shared_ptr<TrackeditCommandsState> m_state;

    std::shared_ptr<context::GlobalContextMock> m_globalContext;
    std::shared_ptr<project::AudacityProjectMock> m_project;
    std::shared_ptr<SelectionControllerMock> m_selectionController;
    std::shared_ptr<TrackNavigationControllerMock> m_trackNavigationController;
    std::shared_ptr<TrackeditInteractionMock> m_trackeditInteraction;
};

TEST_F(TrackeditCommandsStateTests, UngroupIsAvailableForASingleGroupedItem)
{
    //! [GIVEN] One selected clip that still carries a group id, the rest of its group having been removed
    const ClipKey clipKey { 1, 10 };
    ON_CALL(*m_selectionController, selectedClips()).WillByDefault(Return(ClipKeyList { clipKey }));
    ON_CALL(*m_trackeditInteraction, itemGroupId(clipKey)).WillByDefault(Return(int64_t(7)));

    //! [THEN] Ungroup can clear that id, while Group needs more than one item
    EXPECT_TRUE(enabled(TRACKEDIT_UNGROUP_ITEMS_COMMAND));
    EXPECT_FALSE(enabled(TRACKEDIT_GROUP_ITEMS_COMMAND));

    //! [WHEN] The clip is not grouped
    ON_CALL(*m_trackeditInteraction, itemGroupId(clipKey)).WillByDefault(Return(int64_t(-1)));

    //! [THEN] Neither command applies to it alone
    EXPECT_FALSE(enabled(TRACKEDIT_UNGROUP_ITEMS_COMMAND));
    EXPECT_FALSE(enabled(TRACKEDIT_GROUP_ITEMS_COMMAND));
}
}
