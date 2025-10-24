/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include "../internal/au3/au3labelsinteraction.h"

#include "au3interactiontestbase.h"
#include "mocks/selectioncontrollermock.h"

#include "libraries/lib-label-track/LabelTrack.h"
#include "au3wrap/internal/domaccessor.h"
#include "au3wrap/internal/wxtypes_convert.h"

using ::testing::Truly;
using ::testing::_;
using ::testing::AtLeast;

namespace au::trackedit {
/*******************************************************************************
 * AU3 LABELS INTERACTION TESTS
 *
 * This test suite verifies the functionality of Au3LabelsInteraction class.
 * Labels interaction includes operations such as adding labels to selection,
 * changing label titles, and other label-related operations.
 *
 ******************************************************************************************/

class Au3LabelsInteractionsTests : public Au3InteractionTestBase
{
public:
    void SetUp() override
    {
        m_labelsInteraction = std::make_shared<Au3LabelsInteraction>();

        m_globalContext = std::make_shared<NiceMock<context::GlobalContextMock> >();
        m_selectionController = std::make_shared<NiceMock<SelectionControllerMock> >();
        m_playbackState = std::make_shared<NiceMock<context::PlaybackStateMock> >();

        m_labelsInteraction->globalContext.set(m_globalContext);
        m_labelsInteraction->selectionController.set(m_selectionController);

        m_trackEditProject = std::make_shared<NiceMock<TrackeditProjectMock> >();
        ON_CALL(*m_globalContext, currentTrackeditProject())
        .WillByDefault(Return(m_trackEditProject));

        m_currentProject = std::make_shared<NiceMock<project::AudacityProjectMock> >();
        ON_CALL(*m_globalContext, currentProject())
        .WillByDefault(Return(m_currentProject));
        ON_CALL(*m_globalContext, playbackState())
        .WillByDefault(Return(m_playbackState));

        ON_CALL(*m_currentProject, trackeditProject())
        .WillByDefault(Return(m_trackEditProject));

        initTestProject();
    }

    std::shared_ptr<Au3LabelsInteraction> m_labelsInteraction;
    std::shared_ptr<SelectionControllerMock> m_selectionController;
};

TEST_F(Au3LabelsInteractionsTests, AddLabelToSelectionCreatesLabelTrackWhenNoneExists)
{
    //! [GIVEN] There is a project without any label tracks
    const Au3TrackList& projectTracks = Au3TrackList::Get(projectRef());
    ASSERT_EQ(projectTracks.Size(), 0) << "Precondition failed: The project should be empty";

    //! [GIVEN] There is a selection from 1.0 to 3.0 seconds
    const double selectionStart = 1.0;
    const double selectionEnd = 3.0;
    ON_CALL(*m_selectionController, dataSelectedStartTime())
    .WillByDefault(Return(selectionStart));
    ON_CALL(*m_selectionController, dataSelectedEndTime())
    .WillByDefault(Return(selectionEnd));
    ON_CALL(*m_selectionController, focusedTrack())
    .WillByDefault(Return(INVALID_TRACK));

    //! [EXPECT] The project is notified about a new track and a new label being added
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackAdded(_)).Times(1);
    EXPECT_CALL(*m_trackEditProject, notifyAboutLabelAdded(_)).Times(1);
    EXPECT_CALL(*m_selectionController, setFocusedTrack(_)).Times(1);

    //! [WHEN] Add a label to the selection
    bool result = m_labelsInteraction->addLabelToSelection();

    //! [THEN] The operation is successful
    ASSERT_TRUE(result) << "Adding label to selection should succeed";

    //! [THEN] A new label track is created
    ASSERT_EQ(projectTracks.Size(), 1) << "A label track should have been created";

    //! [THEN] The label track contains one label
    const Au3LabelTrack* labelTrack = DomAccessor::findLabelTrackByIndex(projectRef(), 0);
    ASSERT_NE(labelTrack, nullptr) << "Label track should exist";
    ASSERT_EQ(labelTrack->GetNumLabels(), 1) << "Label track should contain one label";

    //! [THEN] The label has the correct time range
    const Au3Label* label = labelTrack->GetLabel(0);
    ASSERT_NE(label, nullptr) << "Label should exist";
    ASSERT_DOUBLE_EQ(label->getT0(), selectionStart) << "Label start time should match selection start";
    ASSERT_DOUBLE_EQ(label->getT1(), selectionEnd) << "Label end time should match selection end";
}

TEST_F(Au3LabelsInteractionsTests, AddLabelToSelectionUsesExistingLabelTrack)
{
    //! [GIVEN] There is a project with an existing label track
    Au3TrackList& tracks = Au3TrackList::Get(projectRef());
    Au3LabelTrack* existingLabelTrack = ::LabelTrack::Create(tracks);
    ASSERT_NE(existingLabelTrack, nullptr) << "Failed to create initial label track";
    ASSERT_EQ(tracks.Size(), 1) << "Precondition failed: Should have one track";

    //! [GIVEN] There is a selection from 2.0 to 4.0 seconds
    const double selectionStart = 2.0;
    const double selectionEnd = 4.0;
    ON_CALL(*m_selectionController, dataSelectedStartTime())
    .WillByDefault(Return(selectionStart));
    ON_CALL(*m_selectionController, dataSelectedEndTime())
    .WillByDefault(Return(selectionEnd));
    ON_CALL(*m_selectionController, focusedTrack())
    .WillByDefault(Return(INVALID_TRACK));

    //! [EXPECT] The project is notified about a new label being added but NOT about a new track
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackAdded(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutLabelAdded(_)).Times(1);
    EXPECT_CALL(*m_selectionController, setFocusedTrack(_)).Times(1);

    //! [WHEN] Add a label to the selection
    bool result = m_labelsInteraction->addLabelToSelection();

    //! [THEN] The operation is successful
    ASSERT_TRUE(result) << "Adding label to selection should succeed";

    //! [THEN] No new track is created
    ASSERT_EQ(tracks.Size(), 1) << "No new track should have been created";

    //! [THEN] The existing label track now contains one label
    ASSERT_EQ(existingLabelTrack->GetNumLabels(), 1) << "Label track should contain one label";

    //! [THEN] The label has the correct time range
    const Au3Label* label = existingLabelTrack->GetLabel(0);
    ASSERT_NE(label, nullptr) << "Label should exist";
    ASSERT_DOUBLE_EQ(label->getT0(), selectionStart) << "Label start time should match selection start";
    ASSERT_DOUBLE_EQ(label->getT1(), selectionEnd) << "Label end time should match selection end";
}

TEST_F(Au3LabelsInteractionsTests, AddLabelToSelectionUsesFocusedLabelTrack)
{
    //! [GIVEN] There is a project with two label tracks
    Au3TrackList& tracks = Au3TrackList::Get(projectRef());
    Au3LabelTrack* firstLabelTrack = ::LabelTrack::Create(tracks);
    Au3LabelTrack* secondLabelTrack = ::LabelTrack::Create(tracks);
    ASSERT_NE(firstLabelTrack, nullptr) << "Failed to create first label track";
    ASSERT_NE(secondLabelTrack, nullptr) << "Failed to create second label track";
    ASSERT_EQ(tracks.Size(), 2) << "Precondition failed: Should have two tracks";

    //! [GIVEN] The second label track is focused
    const TrackId focusedTrackId = secondLabelTrack->GetId();
    ON_CALL(*m_selectionController, focusedTrack())
    .WillByDefault(Return(focusedTrackId));

    //! [GIVEN] There is a selection from 0.5 to 1.5 seconds
    const double selectionStart = 0.5;
    const double selectionEnd = 1.5;
    ON_CALL(*m_selectionController, dataSelectedStartTime())
    .WillByDefault(Return(selectionStart));
    ON_CALL(*m_selectionController, dataSelectedEndTime())
    .WillByDefault(Return(selectionEnd));

    //! [EXPECT] The project is notified about a new label being added
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackAdded(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutLabelAdded(_)).Times(1);
    EXPECT_CALL(*m_selectionController, setFocusedTrack(focusedTrackId)).Times(1);

    //! [WHEN] Add a label to the selection
    bool result = m_labelsInteraction->addLabelToSelection();

    //! [THEN] The operation is successful
    ASSERT_TRUE(result) << "Adding label to selection should succeed";

    //! [THEN] The label is added to the focused (second) track, not the first
    ASSERT_EQ(firstLabelTrack->GetNumLabels(), 0) << "First label track should remain empty";
    ASSERT_EQ(secondLabelTrack->GetNumLabels(), 1) << "Second (focused) label track should contain one label";

    //! [THEN] The label has the correct time range
    const Au3Label* label = secondLabelTrack->GetLabel(0);
    ASSERT_NE(label, nullptr) << "Label should exist";
    ASSERT_DOUBLE_EQ(label->getT0(), selectionStart) << "Label start time should match selection start";
    ASSERT_DOUBLE_EQ(label->getT1(), selectionEnd) << "Label end time should match selection end";
}

TEST_F(Au3LabelsInteractionsTests, AddLabelToSelectionWithZeroLengthSelection)
{
    //! [GIVEN] There is a project without any label tracks
    const Au3TrackList& projectTracks = Au3TrackList::Get(projectRef());
    ASSERT_EQ(projectTracks.Size(), 0) << "Precondition failed: The project should be empty";

    //! [GIVEN] There is a zero-length selection at 2.5 seconds (cursor position)
    const double cursorPosition = 2.5;
    ON_CALL(*m_selectionController, dataSelectedStartTime())
    .WillByDefault(Return(cursorPosition));
    ON_CALL(*m_selectionController, dataSelectedEndTime())
    .WillByDefault(Return(cursorPosition));
    ON_CALL(*m_selectionController, focusedTrack())
    .WillByDefault(Return(INVALID_TRACK));

    //! [EXPECT] The project is notified about a new track and a new label being added
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackAdded(_)).Times(1);
    EXPECT_CALL(*m_trackEditProject, notifyAboutLabelAdded(_)).Times(1);
    EXPECT_CALL(*m_selectionController, setFocusedTrack(_)).Times(1);

    //! [WHEN] Add a label at the cursor position
    bool result = m_labelsInteraction->addLabelToSelection();

    //! [THEN] The operation is successful
    ASSERT_TRUE(result) << "Adding label at cursor position should succeed";

    //! [THEN] A new label track is created
    ASSERT_EQ(projectTracks.Size(), 1) << "A label track should have been created";

    //! [THEN] The label track contains one label with zero length
    const Au3LabelTrack* labelTrack = DomAccessor::findLabelTrackByIndex(projectRef(), 0);
    ASSERT_NE(labelTrack, nullptr) << "Label track should exist";
    ASSERT_EQ(labelTrack->GetNumLabels(), 1) << "Label track should contain one label";

    //! [THEN] The label is a point label (same start and end time)
    const Au3Label* label = labelTrack->GetLabel(0);
    ASSERT_NE(label, nullptr) << "Label should exist";
    ASSERT_DOUBLE_EQ(label->getT0(), cursorPosition) << "Label start should be at cursor position";
    ASSERT_DOUBLE_EQ(label->getT1(), cursorPosition) << "Label end should be at cursor position";
}

TEST_F(Au3LabelsInteractionsTests, AddMultipleLabelsToSameLabelTrack)
{
    //! [GIVEN] There is a project without any label tracks
    const Au3TrackList& projectTracks = Au3TrackList::Get(projectRef());
    ASSERT_EQ(projectTracks.Size(), 0) << "Precondition failed: The project should be empty";

    ON_CALL(*m_selectionController, focusedTrack())
    .WillByDefault(Return(INVALID_TRACK));

    //! [EXPECT] Notifications for track and labels
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackAdded(_)).Times(1);
    EXPECT_CALL(*m_trackEditProject, notifyAboutLabelAdded(_)).Times(3);
    EXPECT_CALL(*m_selectionController, setFocusedTrack(_)).Times(AtLeast(1));

    //! [WHEN] Add first label from 0.0 to 1.0
    ON_CALL(*m_selectionController, dataSelectedStartTime()).WillByDefault(Return(0.0));
    ON_CALL(*m_selectionController, dataSelectedEndTime()).WillByDefault(Return(1.0));
    bool result1 = m_labelsInteraction->addLabelToSelection();
    ASSERT_TRUE(result1) << "Adding first label should succeed";

    //! [WHEN] Add second label from 2.0 to 3.0
    ON_CALL(*m_selectionController, dataSelectedStartTime()).WillByDefault(Return(2.0));
    ON_CALL(*m_selectionController, dataSelectedEndTime()).WillByDefault(Return(3.0));
    bool result2 = m_labelsInteraction->addLabelToSelection();
    ASSERT_TRUE(result2) << "Adding second label should succeed";

    //! [WHEN] Add third label from 4.0 to 5.0
    ON_CALL(*m_selectionController, dataSelectedStartTime()).WillByDefault(Return(4.0));
    ON_CALL(*m_selectionController, dataSelectedEndTime()).WillByDefault(Return(5.0));
    bool result3 = m_labelsInteraction->addLabelToSelection();
    ASSERT_TRUE(result3) << "Adding third label should succeed";

    //! [THEN] Only one label track was created
    ASSERT_EQ(projectTracks.Size(), 1) << "Only one label track should exist";

    //! [THEN] The label track contains all three labels
    const Au3LabelTrack* labelTrack = DomAccessor::findLabelTrackByIndex(projectRef(), 0);
    ASSERT_NE(labelTrack, nullptr) << "Label track should exist";
    ASSERT_EQ(labelTrack->GetNumLabels(), 3) << "Label track should contain three labels";

    //! [THEN] Verify the time ranges of all labels
    const Au3Label* label1 = labelTrack->GetLabel(0);
    ASSERT_NE(label1, nullptr) << "First label should exist";
    ASSERT_DOUBLE_EQ(label1->getT0(), 0.0);
    ASSERT_DOUBLE_EQ(label1->getT1(), 1.0);

    const Au3Label* label2 = labelTrack->GetLabel(1);
    ASSERT_NE(label2, nullptr) << "Second label should exist";
    ASSERT_DOUBLE_EQ(label2->getT0(), 2.0);
    ASSERT_DOUBLE_EQ(label2->getT1(), 3.0);

    const Au3Label* label3 = labelTrack->GetLabel(2);
    ASSERT_NE(label3, nullptr) << "Third label should exist";
    ASSERT_DOUBLE_EQ(label3->getT0(), 4.0);
    ASSERT_DOUBLE_EQ(label3->getT1(), 5.0);
}

TEST_F(Au3LabelsInteractionsTests, AddLabelToSelectionWithAudioTrackPresent)
{
    //! [GIVEN] There is a project with an audio track
    const TrackId audioTrackId = createTrack(TestTrackID::TRACK_SMALL_SILENCE);
    ASSERT_NE(audioTrackId, INVALID_TRACK) << "Failed to create audio track";

    const Au3TrackList& projectTracks = Au3TrackList::Get(projectRef());
    ASSERT_EQ(projectTracks.Size(), 1) << "Precondition failed: Should have one audio track";

    //! [GIVEN] There is a selection from 0.0 to 1.0 seconds
    const double selectionStart = 0.0;
    const double selectionEnd = 1.0;
    ON_CALL(*m_selectionController, dataSelectedStartTime())
    .WillByDefault(Return(selectionStart));
    ON_CALL(*m_selectionController, dataSelectedEndTime())
    .WillByDefault(Return(selectionEnd));
    ON_CALL(*m_selectionController, focusedTrack())
    .WillByDefault(Return(INVALID_TRACK));

    //! [EXPECT] The project is notified about a new label track and a new label being added
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackAdded(_)).Times(1);
    EXPECT_CALL(*m_trackEditProject, notifyAboutLabelAdded(_)).Times(1);
    EXPECT_CALL(*m_selectionController, setFocusedTrack(_)).Times(1);

    //! [WHEN] Add a label to the selection
    bool result = m_labelsInteraction->addLabelToSelection();

    //! [THEN] The operation is successful
    ASSERT_TRUE(result) << "Adding label to selection should succeed";

    //! [THEN] A new label track is created alongside the audio track
    ASSERT_EQ(projectTracks.Size(), 2) << "Should have both audio track and label track";

    //! [THEN] The label track contains one label with the correct time range
    const Au3LabelTrack* labelTrack = DomAccessor::findLabelTrackByIndex(projectRef(), 0);
    ASSERT_NE(labelTrack, nullptr) << "Label track should exist";
    ASSERT_EQ(labelTrack->GetNumLabels(), 1) << "Label track should contain one label";

    const Au3Label* label = labelTrack->GetLabel(0);
    ASSERT_NE(label, nullptr) << "Label should exist";
    ASSERT_DOUBLE_EQ(label->getT0(), selectionStart) << "Label start time should match selection start";
    ASSERT_DOUBLE_EQ(label->getT1(), selectionEnd) << "Label end time should match selection end";

    // Cleanup
    removeTrack(audioTrackId);
}

TEST_F(Au3LabelsInteractionsTests, ChangeLabelTitle)
{
    //! [GIVEN] There is a project with a label track containing one label
    Au3TrackList& tracks = Au3TrackList::Get(projectRef());
    Au3LabelTrack* labelTrack = ::LabelTrack::Create(tracks);
    ASSERT_NE(labelTrack, nullptr) << "Failed to create label track";

    //! [GIVEN] Add a label directly (not through the interaction being tested)
    SelectedRegion region;
    region.setTimes(1.0, 2.0);
    int labelIndex = labelTrack->AddLabel(region, wxString());
    ASSERT_EQ(labelIndex, 0) << "Label should be added at index 0";
    ASSERT_EQ(labelTrack->GetNumLabels(), 1) << "Label track should contain one label";

    //! [EXPECT] The project is notified about label change
    EXPECT_CALL(*m_trackEditProject, notifyAboutLabelChanged(Truly([&](const Label& label) {
        return label.key.trackId == TrackId(labelTrack->GetId()) && label.key.itemId == 0;
    }))).Times(1);

    //! [WHEN] Change the label title
    const muse::String newTitle = u"Test Label";
    const LabelKey labelKey { labelTrack->GetId(), 0 };
    bool result = m_labelsInteraction->changeLabelTitle(labelKey, newTitle);

    //! [THEN] The operation is successful
    ASSERT_TRUE(result) << "Changing label title should succeed";

    //! [THEN] The label title is updated
    const Au3Label* updatedLabel = labelTrack->GetLabel(0);
    ASSERT_NE(updatedLabel, nullptr) << "Label should still exist";
    EXPECT_EQ(updatedLabel->title, wxFromString(newTitle)) << "Label title should be updated";
}
}
