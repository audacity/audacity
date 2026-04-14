/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include "projectscene/view/tracksitemsview/trackclipslistmodel.h"
#include "projectscene/view/tracksitemsview/tracklabelslistmodel.h"

#include "trackedit/tests/mocks/selectioncontrollermock.h"
#include "trackedit/tests/mocks/tracknavigationcontrollermock.h"
#include "trackedit/tests/mocks/trackeditinteractionmock.h"

#include "testing/testcontext.h"

using namespace ::testing;

namespace au::projectscene {
static TrackItemKey makeKey(trackedit::TrackId trackId, trackedit::TrackItemId itemId)
{
    return TrackItemKey(trackedit::TrackItemKey(trackId, itemId));
}

// =====================================================================
// Clip selection tests
// =====================================================================

class TrackClipsSelectionTests : public ::testing::Test
{
protected:
    void SetUp() override
    {
        m_model = new TrackClipsListModel();
        m_selectionController = std::make_shared<NiceMock<trackedit::SelectionControllerMock> >();
        m_trackNavController = std::make_shared<NiceMock<trackedit::TrackNavigationControllerMock> >();
        m_trackeditInteraction = std::make_shared<NiceMock<trackedit::TrackeditInteractionMock> >();

        m_model->selectionController.set(m_selectionController);
        m_model->trackNavigationController.set(m_trackNavController);
        m_model->trackeditInteraction.set(m_trackeditInteraction);

        ON_CALL(*m_trackeditInteraction, clipGroupId(_))
        .WillByDefault(Return(int64_t(-1)));
    }

    void TearDown() override
    {
        delete m_model;
    }

    TrackClipsListModel* m_model = nullptr;
    std::shared_ptr<NiceMock<trackedit::SelectionControllerMock> > m_selectionController;
    std::shared_ptr<NiceMock<trackedit::TrackNavigationControllerMock> > m_trackNavController;
    std::shared_ptr<NiceMock<trackedit::TrackeditInteractionMock> > m_trackeditInteraction;
};

TEST_F(TrackClipsSelectionTests, SelectClip_SelectsClip_WhenNothingSelected)
{
    //! CASE Selecting a clip should select it
    auto key = makeKey(1, 42);

    ON_CALL(*m_selectionController, selectedClips())
    .WillByDefault(Return(trackedit::ClipKeyList {}));

    EXPECT_CALL(*m_selectionController, setSelectedClips(trackedit::ClipKeyList { key.key }, true))
    .Times(1);

    m_model->selectClip(key);
}

TEST_F(TrackClipsSelectionTests, SelectClip_SetsFocusedItem)
{
    //! CASE Selecting a clip should update focus
    auto key = makeKey(1, 42);

    ON_CALL(*m_selectionController, selectedClips())
    .WillByDefault(Return(trackedit::ClipKeyList {}));

    EXPECT_CALL(*m_trackNavController, setFocusedItem(key.key, _)).Times(1);
    m_model->selectClip(key);
}

TEST_F(TrackClipsSelectionTests, SelectClip_SetsFocusedItem_WhenAlreadySelected)
{
    //! CASE Selecting an already selected clip should also update focus
    auto key = makeKey(1, 42);

    ON_CALL(*m_selectionController, selectedClips())
    .WillByDefault(Return(trackedit::ClipKeyList { key.key }));

    EXPECT_CALL(*m_trackNavController, setFocusedItem(key.key, _)).Times(1);
    m_model->selectClip(key);
}

TEST_F(TrackClipsSelectionTests, SelectClip_SetsFocusedItem_GroupedClip)
{
    //! CASE Selecting a grouped clip should update focus
    auto key = makeKey(1, 42);
    auto other = makeKey(1, 43);
    int64_t groupId = 7;

    ON_CALL(*m_trackeditInteraction, clipGroupId(_))
    .WillByDefault(Return(groupId));
    ON_CALL(*m_trackeditInteraction, clipsInGroup(groupId))
    .WillByDefault(Return(trackedit::ClipKeyList { key.key, other.key }));

    EXPECT_CALL(*m_trackNavController, setFocusedItem(key.key, _)).Times(1);
    m_model->selectClip(key);
}

// =====================================================================
// Label selection tests
// =====================================================================

class TrackLabelsSelectionTests : public ::testing::Test
{
protected:
    void SetUp() override
    {
        m_model = new TrackLabelsListModel();
        m_model->m_trackId = 1;

        m_selectionController = std::make_shared<NiceMock<trackedit::SelectionControllerMock> >();
        m_trackNavController = std::make_shared<NiceMock<trackedit::TrackNavigationControllerMock> >();

        m_model->selectionController.set(m_selectionController);
        m_model->trackNavigationController.set(m_trackNavController);
    }

    void TearDown() override
    {
        delete m_model;
    }

    TrackLabelsListModel* m_model = nullptr;
    std::shared_ptr<NiceMock<trackedit::SelectionControllerMock> > m_selectionController;
    std::shared_ptr<NiceMock<trackedit::TrackNavigationControllerMock> > m_trackNavController;
};

TEST_F(TrackLabelsSelectionTests, SelectLabel_SelectsLabel_WhenNothingSelected)
{
    //! CASE Selecting a label should select it
    //!
    auto key = makeKey(1, 10);

    ON_CALL(*m_selectionController, selectedLabels())
    .WillByDefault(Return(trackedit::LabelKeyList {}));

    EXPECT_CALL(*m_selectionController, setSelectedLabels(trackedit::LabelKeyList { key.key }, true))
    .Times(1);

    m_model->selectLabel(key);
}

TEST_F(TrackLabelsSelectionTests, SelectLabel_SetsFocusedItem)
{
    //! CASE Selecting a label should update focus
    auto key = makeKey(1, 10);

    ON_CALL(*m_selectionController, selectedLabels())
    .WillByDefault(Return(trackedit::LabelKeyList {}));

    EXPECT_CALL(*m_trackNavController, setFocusedItem(key.key, _)).Times(1);
    m_model->selectLabel(key);
}

TEST_F(TrackLabelsSelectionTests, SelectLabel_SetsFocusedItem_WhenAlreadySelected)
{
    //! CASE Selecting an already selected label should also update focus
    auto key = makeKey(1, 10);

    ON_CALL(*m_selectionController, selectedLabels())
    .WillByDefault(Return(trackedit::LabelKeyList { key.key }));

    EXPECT_CALL(*m_trackNavController, setFocusedItem(key.key, _)).Times(1);
    m_model->selectLabel(key);
}
}
