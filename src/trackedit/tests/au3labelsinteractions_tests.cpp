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
    EXPECT_CALL(*m_selectionController, setSelectedLabels(_, _)).Times(1);

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
    EXPECT_CALL(*m_selectionController, setSelectedLabels(_, _)).Times(1);

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
    EXPECT_CALL(*m_selectionController, setSelectedLabels(_, _)).Times(1);

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
    EXPECT_CALL(*m_selectionController, setSelectedLabels(_, _)).Times(1);

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
    EXPECT_CALL(*m_selectionController, setSelectedLabels(_, _)).Times(AtLeast(1));

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
    EXPECT_CALL(*m_selectionController, setSelectedLabels(_, _)).Times(1);

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
    TrackItemId labelId = labelTrack->AddLabel(region, wxString());
    ASSERT_EQ(labelTrack->GetNumLabels(), 1) << "Label track should contain one label";

    //! [EXPECT] The project is notified about label change
    EXPECT_CALL(*m_trackEditProject, notifyAboutLabelChanged(Truly([&](const Label& label) {
        return label.key.trackId == TrackId(labelTrack->GetId()) && label.key.itemId == labelId;
    }))).Times(1);

    //! [WHEN] Change the label title
    const muse::String newTitle = u"Test Label";
    const LabelKey labelKey { labelTrack->GetId(), labelId };
    bool result = m_labelsInteraction->changeLabelTitle(labelKey, newTitle);

    //! [THEN] The operation is successful
    ASSERT_TRUE(result) << "Changing label title should succeed";

    //! [THEN] The label title is updated
    const Au3Label* updatedLabel = labelTrack->GetLabel(0);
    ASSERT_NE(updatedLabel, nullptr) << "Label should still exist";
    EXPECT_EQ(updatedLabel->title, wxFromString(newTitle)) << "Label title should be updated";
}

TEST_F(Au3LabelsInteractionsTests, RemoveSingleLabel)
{
    //! [GIVEN] There is a project with a label track containing two labels
    Au3TrackList& tracks = Au3TrackList::Get(projectRef());
    Au3LabelTrack* labelTrack = ::LabelTrack::Create(tracks);
    ASSERT_NE(labelTrack, nullptr) << "Failed to create label track";

    //! [GIVEN] Add two labels directly
    SelectedRegion region1;
    region1.setTimes(1.0, 2.0);
    TrackItemId labelId1 = labelTrack->AddLabel(region1, wxString("Label 1"));

    SelectedRegion region2;
    region2.setTimes(3.0, 4.0);
    TrackItemId labelId2 = labelTrack->AddLabel(region2, wxString("Label 2"));

    //! [THEN] The label track contains two labels
    ASSERT_EQ(labelTrack->GetNumLabels(), 2) << "Precondition failed: The number of labels is not 2";
    ASSERT_EQ(labelTrack->GetLabel(0)->GetId(), labelId1) << "First label should be Label 1";
    ASSERT_EQ(labelTrack->GetLabel(1)->GetId(), labelId2) << "Second label should be Label 2";

    //! [EXPECT] The project is notified about track changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackChanged(Truly([&](const Track& track) {
        return track.id == TrackId(labelTrack->GetId());
    }))).Times(1);

    //! [WHEN] Remove the first label
    const LabelKey labelKey { labelTrack->GetId(), labelId1 };
    bool result = m_labelsInteraction->removeLabel(labelKey);

    //! [THEN] The operation is successful
    ASSERT_TRUE(result) << "Removing label should succeed";

    //! [THEN] The number of labels is 1
    ASSERT_EQ(labelTrack->GetNumLabels(), 1) << "The number of labels after the remove operation is not 1";

    //! [THEN] The removed label is no longer in the track
    ASSERT_EQ(labelTrack->GetLabelIndex(labelId1), -1) << "labelId1 should be removed";

    //! [THEN] The remaining label keeps the same properties
    const Au3Label* remainingLabel = labelTrack->GetLabel(0);
    ASSERT_NE(remainingLabel, nullptr) << "Remaining label should exist";
    ASSERT_EQ(remainingLabel->GetId(), labelId2) << "Remaining label should have correct ID";
    ASSERT_DOUBLE_EQ(remainingLabel->getT0(), 3.0) << "Remaining label start time should be 3.0";
    ASSERT_DOUBLE_EQ(remainingLabel->getT1(), 4.0) << "Remaining label end time should be 4.0";
    EXPECT_EQ(remainingLabel->title, wxString("Label 2")) << "Remaining label title should be 'Label 2'";
}

TEST_F(Au3LabelsInteractionsTests, RemoveAllLabelsFromTrack)
{
    //! [GIVEN] There is a project with a label track containing three labels
    Au3TrackList& tracks = Au3TrackList::Get(projectRef());
    Au3LabelTrack* labelTrack = ::LabelTrack::Create(tracks);
    ASSERT_NE(labelTrack, nullptr) << "Failed to create label track";

    //! [GIVEN] Add three labels directly
    SelectedRegion region1;
    region1.setTimes(0.0, 1.0);
    TrackItemId labelId1 = labelTrack->AddLabel(region1, wxString("Label 1"));

    SelectedRegion region2;
    region2.setTimes(2.0, 3.0);
    TrackItemId labelId2 = labelTrack->AddLabel(region2, wxString("Label 2"));

    SelectedRegion region3;
    region3.setTimes(4.0, 5.0);
    TrackItemId labelId3 = labelTrack->AddLabel(region3, wxString("Label 3"));

    //! [THEN] The label track contains three labels
    ASSERT_EQ(labelTrack->GetNumLabels(), 3) << "Precondition failed: The number of labels is not 3";
    ASSERT_EQ(labelTrack->GetLabel(0)->GetId(), labelId1) << "First label should be Label 1";
    ASSERT_EQ(labelTrack->GetLabel(1)->GetId(), labelId2) << "Second label should be Label 2";
    ASSERT_EQ(labelTrack->GetLabel(2)->GetId(), labelId3) << "Third label should be Label 3";

    //! [EXPECT] The project is notified about track changed three times
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackChanged(_)).Times(3);

    //! [WHEN] Remove all three labels one by one
    bool result1 = m_labelsInteraction->removeLabel({ labelTrack->GetId(), labelId1 });
    ASSERT_TRUE(result1) << "Removing first label should succeed";
    ASSERT_EQ(labelTrack->GetNumLabels(), 2) << "Should have 2 labels after removing first";
    ASSERT_EQ(labelTrack->GetLabel(0)->GetId(), labelId2) << "First remaining label should be labelId2";
    ASSERT_EQ(labelTrack->GetLabel(1)->GetId(), labelId3) << "Second remaining label should be labelId3";

    bool result2 = m_labelsInteraction->removeLabel({ labelTrack->GetId(), labelId2 });
    ASSERT_TRUE(result2) << "Removing second label should succeed";
    ASSERT_EQ(labelTrack->GetNumLabels(), 1) << "Should have 1 label after removing second";
    ASSERT_EQ(labelTrack->GetLabel(0)->GetId(), labelId3) << "Remaining label should be labelId3";

    bool result3 = m_labelsInteraction->removeLabel({ labelTrack->GetId(), labelId3 });
    ASSERT_TRUE(result3) << "Removing third label should succeed";

    //! [THEN] The number of labels is 0
    ASSERT_EQ(labelTrack->GetNumLabels(), 0) << "The number of labels after removing all should be 0";
}

TEST_F(Au3LabelsInteractionsTests, RemoveLabelWithInvalidKey)
{
    //! [GIVEN] There is a project with a label track containing one label
    Au3TrackList& tracks = Au3TrackList::Get(projectRef());
    Au3LabelTrack* labelTrack = ::LabelTrack::Create(tracks);
    ASSERT_NE(labelTrack, nullptr) << "Failed to create label track";

    SelectedRegion region;
    region.setTimes(1.0, 2.0);
    TrackItemId labelId = labelTrack->AddLabel(region, wxString("Label"));
    ASSERT_EQ(labelTrack->GetNumLabels(), 1) << "Label track should contain one label";
    ASSERT_EQ(labelTrack->GetLabel(0)->GetId(), labelId) << "First label should be Label";

    //! [EXPECT] No notification should be sent for invalid operations
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackChanged(_)).Times(0);

    //! [WHEN] Try to remove a label with an invalid label ID
    const LabelKey invalidLabelKey { labelTrack->GetId(), 999999 };
    bool result = m_labelsInteraction->removeLabel(invalidLabelKey);

    //! [THEN] The operation fails
    ASSERT_FALSE(result) << "Removing label with invalid ID should fail";

    //! [THEN] The original label is still present
    ASSERT_EQ(labelTrack->GetNumLabels(), 1) << "Original label should still be present";
    ASSERT_EQ(labelTrack->GetLabels().front().GetId(), labelId) << "Original label should still be present by id";
}

TEST_F(Au3LabelsInteractionsTests, RemoveTwoLabels)
{
    //! [GIVEN] There is a project with a label track containing three labels
    Au3TrackList& tracks = Au3TrackList::Get(projectRef());
    Au3LabelTrack* labelTrack = ::LabelTrack::Create(tracks);
    ASSERT_NE(labelTrack, nullptr) << "Failed to create label track";

    //! [GIVEN] Add three labels directly
    SelectedRegion region1;
    region1.setTimes(0.0, 1.0);
    TrackItemId labelId1 = labelTrack->AddLabel(region1, wxString("Label 1"));

    SelectedRegion region2;
    region2.setTimes(2.0, 3.0);
    TrackItemId labelId2 = labelTrack->AddLabel(region2, wxString("Label 2"));

    SelectedRegion region3;
    region3.setTimes(4.0, 5.0);
    TrackItemId labelId3 = labelTrack->AddLabel(region3, wxString("Label 3"));

    //! [THEN] The label track contains three labels
    ASSERT_EQ(labelTrack->GetNumLabels(), 3) << "Precondition failed: The number of labels is not 3";
    ASSERT_EQ(labelTrack->GetLabel(0)->GetId(), labelId1) << "First label should be Label 1";
    ASSERT_EQ(labelTrack->GetLabel(1)->GetId(), labelId2) << "Second label should be Label 2";
    ASSERT_EQ(labelTrack->GetLabel(2)->GetId(), labelId3) << "Third label should be Label 3";

    //! [EXPECT] The project is notified about track changed once
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackChanged(Truly([&](const Track& track) {
        return track.id == TrackId(labelTrack->GetId());
    }))).Times(1);

    //! [WHEN] Remove the first and third labels
    LabelKeyList labelsToRemove = {
        { labelTrack->GetId(), labelId1 },
        { labelTrack->GetId(), labelId3 }
    };
    bool result = m_labelsInteraction->removeLabels(labelsToRemove);

    //! [THEN] The operation is successful
    ASSERT_TRUE(result) << "Removing labels should succeed";

    //! [THEN] The number of labels is 1
    ASSERT_EQ(labelTrack->GetNumLabels(), 1) << "The number of labels after the remove operation is not 1";

    //! [THEN] The removed labels are no longer in the track
    ASSERT_EQ(labelTrack->GetLabelIndex(labelId1), -1) << "labelId1 should be removed";
    ASSERT_EQ(labelTrack->GetLabelIndex(labelId3), -1) << "labelId3 should be removed";

    //! [THEN] The remaining label keeps the same properties
    const Au3Label* remainingLabel = labelTrack->GetLabel(0);
    ASSERT_NE(remainingLabel, nullptr) << "Remaining label should exist";
    ASSERT_EQ(remainingLabel->GetId(), labelId2) << "Remaining label should have correct ID";
    ASSERT_DOUBLE_EQ(remainingLabel->getT0(), 2.0) << "Remaining label start time should be 2.0";
    ASSERT_DOUBLE_EQ(remainingLabel->getT1(), 3.0) << "Remaining label end time should be 3.0";
    EXPECT_EQ(remainingLabel->title, wxString("Label 2")) << "Remaining label title should be 'Label 2'";
}

TEST_F(Au3LabelsInteractionsTests, RemoveAllLabelsAtOnce)
{
    //! [GIVEN] There is a project with a label track containing three labels
    Au3TrackList& tracks = Au3TrackList::Get(projectRef());
    Au3LabelTrack* labelTrack = ::LabelTrack::Create(tracks);
    ASSERT_NE(labelTrack, nullptr) << "Failed to create label track";

    //! [GIVEN] Add three labels directly
    SelectedRegion region1;
    region1.setTimes(0.0, 1.0);
    TrackItemId labelId1 = labelTrack->AddLabel(region1, wxString("Label 1"));

    SelectedRegion region2;
    region2.setTimes(2.0, 3.0);
    TrackItemId labelId2 = labelTrack->AddLabel(region2, wxString("Label 2"));

    SelectedRegion region3;
    region3.setTimes(4.0, 5.0);
    TrackItemId labelId3 = labelTrack->AddLabel(region3, wxString("Label 3"));

    //! [THEN] The label track contains three labels
    ASSERT_EQ(labelTrack->GetNumLabels(), 3) << "Precondition failed: The number of labels is not 3";
    ASSERT_EQ(labelTrack->GetLabel(0)->GetId(), labelId1) << "First label should be Label 1";
    ASSERT_EQ(labelTrack->GetLabel(1)->GetId(), labelId2) << "Second label should be Label 2";
    ASSERT_EQ(labelTrack->GetLabel(2)->GetId(), labelId3) << "Third label should be Label 3";

    //! [EXPECT] The project is notified about track changed once
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackChanged(_)).Times(1);

    //! [WHEN] Remove all three labels at once
    LabelKeyList labelsToRemove = {
        { labelTrack->GetId(), labelId1 },
        { labelTrack->GetId(), labelId2 },
        { labelTrack->GetId(), labelId3 }
    };
    bool result = m_labelsInteraction->removeLabels(labelsToRemove);

    //! [THEN] The operation is successful
    ASSERT_TRUE(result) << "Removing all labels should succeed";

    //! [THEN] The number of labels is 0
    ASSERT_EQ(labelTrack->GetNumLabels(), 0) << "The number of labels after the remove operation is not 0";
}

TEST_F(Au3LabelsInteractionsTests, RemoveLabelsFromMultipleTracks)
{
    //! [GIVEN] There is a project with two label tracks
    Au3TrackList& tracks = Au3TrackList::Get(projectRef());
    Au3LabelTrack* labelTrack1 = ::LabelTrack::Create(tracks);
    Au3LabelTrack* labelTrack2 = ::LabelTrack::Create(tracks);
    ASSERT_NE(labelTrack1, nullptr) << "Failed to create first label track";
    ASSERT_NE(labelTrack2, nullptr) << "Failed to create second label track";
    ASSERT_EQ(tracks.Size(), 2) << "Precondition failed: Should have two tracks";

    //! [GIVEN] Add labels to both tracks
    SelectedRegion region1;
    region1.setTimes(0.0, 1.0);
    TrackItemId label1Track1 = labelTrack1->AddLabel(region1, wxString("Track1 Label1"));

    SelectedRegion region2;
    region2.setTimes(2.0, 3.0);
    TrackItemId label2Track1 = labelTrack1->AddLabel(region2, wxString("Track1 Label2"));

    SelectedRegion region3;
    region3.setTimes(0.5, 1.5);
    TrackItemId label1Track2 = labelTrack2->AddLabel(region3, wxString("Track2 Label1"));

    SelectedRegion region4;
    region4.setTimes(2.5, 3.5);
    TrackItemId label2Track2 = labelTrack2->AddLabel(region4, wxString("Track2 Label2"));

    //! [THEN] Both tracks contain two labels each
    ASSERT_EQ(labelTrack1->GetNumLabels(), 2) << "First track should have 2 labels";
    ASSERT_EQ(labelTrack2->GetNumLabels(), 2) << "Second track should have 2 labels";
    ASSERT_EQ(labelTrack1->GetLabel(0)->GetId(), label1Track1) << "First label should be Track1 Label1";
    ASSERT_EQ(labelTrack1->GetLabel(1)->GetId(), label2Track1) << "Second label should be Track1 Label2";
    ASSERT_EQ(labelTrack2->GetLabel(0)->GetId(), label1Track2) << "First label should be Track2 Label1";
    ASSERT_EQ(labelTrack2->GetLabel(1)->GetId(), label2Track2) << "Second label should be Track2 Label2";

    //! [EXPECT] The project is notified about both tracks changing
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackChanged(_)).Times(2);

    //! [WHEN] Remove one label from each track
    LabelKeyList labelsToRemove = {
        { labelTrack1->GetId(), label1Track1 },
        { labelTrack2->GetId(), label2Track2 }
    };
    bool result = m_labelsInteraction->removeLabels(labelsToRemove);

    //! [THEN] The operation is successful
    ASSERT_TRUE(result) << "Removing labels from multiple tracks should succeed";

    //! [THEN] Each track has one label remaining
    ASSERT_EQ(labelTrack1->GetNumLabels(), 1) << "First track should have 1 label remaining";
    ASSERT_EQ(labelTrack2->GetNumLabels(), 1) << "Second track should have 1 label remaining";
    ASSERT_EQ(labelTrack1->GetLabel(0)->GetId(), label2Track1) << "First track remaining label should have correct ID";
    ASSERT_EQ(labelTrack2->GetLabel(0)->GetId(), label1Track2) << "Second track remaining label should have correct ID";

    //! [THEN] The removed labels are no longer in the tracks
    ASSERT_EQ(labelTrack1->GetLabelIndex(label1Track1), -1) << "label1Track1 should be removed";
    ASSERT_EQ(labelTrack2->GetLabelIndex(label2Track2), -1) << "label2Track2 should be removed";

    //! [THEN] The remaining labels are correct
    const Au3Label* remainingLabel1 = labelTrack1->GetLabel(0);
    ASSERT_NE(remainingLabel1, nullptr) << "First track remaining label should exist";
    ASSERT_EQ(remainingLabel1->GetId(), label2Track1) << "First track remaining label should have correct ID";
    EXPECT_EQ(remainingLabel1->title, wxString("Track1 Label2")) << "First track remaining label should be 'Track1 Label2'";

    const Au3Label* remainingLabel2 = labelTrack2->GetLabel(0);
    ASSERT_NE(remainingLabel2, nullptr) << "Second track remaining label should exist";
    ASSERT_EQ(remainingLabel2->GetId(), label1Track2) << "Second track remaining label should have correct ID";
    EXPECT_EQ(remainingLabel2->title, wxString("Track2 Label1")) << "Second track remaining label should be 'Track2 Label1'";
}

TEST_F(Au3LabelsInteractionsTests, RemoveLabelsWithEmptyList)
{
    //! [GIVEN] There is a project with a label track containing labels
    Au3TrackList& tracks = Au3TrackList::Get(projectRef());
    Au3LabelTrack* labelTrack = ::LabelTrack::Create(tracks);
    ASSERT_NE(labelTrack, nullptr) << "Failed to create label track";

    SelectedRegion region;
    region.setTimes(1.0, 2.0);
    TrackItemId labelId = labelTrack->AddLabel(region, wxString("Label"));
    ASSERT_EQ(labelTrack->GetNumLabels(), 1) << "Label track should contain one label";
    ASSERT_EQ(labelTrack->GetLabel(0)->GetId(), labelId) << "First label should be Label";

    //! [EXPECT] No notification should be sent for empty list
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackChanged(_)).Times(0);

    //! [WHEN] Try to remove labels with an empty list
    LabelKeyList emptyList;
    bool result = m_labelsInteraction->removeLabels(emptyList);

    //! [THEN] The operation fails
    ASSERT_FALSE(result) << "Removing with empty list should fail";

    //! [THEN] The original label is still present
    ASSERT_EQ(labelTrack->GetNumLabels(), 1) << "Original label should still be present";
    ASSERT_EQ(labelTrack->GetLabels().front().GetId(), labelTrack->GetLabel(0)->GetId()) << "Original label should still be present by id";
}

TEST_F(Au3LabelsInteractionsTests, RemoveLabelsWithSomeInvalidKeys)
{
    //! [GIVEN] There is a project with a label track containing two labels
    Au3TrackList& tracks = Au3TrackList::Get(projectRef());
    Au3LabelTrack* labelTrack = ::LabelTrack::Create(tracks);
    ASSERT_NE(labelTrack, nullptr) << "Failed to create label track";

    //! [GIVEN] Add two labels
    SelectedRegion region1;
    region1.setTimes(0.0, 1.0);
    TrackItemId labelId1 = labelTrack->AddLabel(region1, wxString("Label 1"));

    SelectedRegion region2;
    region2.setTimes(2.0, 3.0);
    TrackItemId labelId2 = labelTrack->AddLabel(region2, wxString("Label 2"));

    ASSERT_EQ(labelTrack->GetNumLabels(), 2) << "Label track should contain two labels";
    ASSERT_EQ(labelTrack->GetLabel(0)->GetId(), labelId1) << "First label should be Label 1";
    ASSERT_EQ(labelTrack->GetLabel(1)->GetId(), labelId2) << "Second label should be Label 2";

    //! [EXPECT] The project is notified about track changed once
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackChanged(_)).Times(1);

    //! [WHEN] Try to remove one valid and one invalid label
    LabelKeyList labelsToRemove = {
        { labelTrack->GetId(), labelId1 },
        { labelTrack->GetId(), 999999 }  // Invalid ID
    };
    bool result = m_labelsInteraction->removeLabels(labelsToRemove);

    //! [THEN] The operation succeeds (valid labels are removed)
    ASSERT_TRUE(result) << "Operation should succeed when at least one valid label is present";

    //! [THEN] Only the valid label was removed
    ASSERT_EQ(labelTrack->GetNumLabels(), 1) << "Should have 1 label remaining";

    //! [THEN] The removed label is no longer in the track
    ASSERT_EQ(labelTrack->GetLabelIndex(labelId1), -1) << "labelId1 should be removed";

    //! [THEN] The remaining label is the second one
    const Au3Label* remainingLabel = labelTrack->GetLabel(0);
    ASSERT_NE(remainingLabel, nullptr) << "Remaining label should exist";
    ASSERT_EQ(remainingLabel->GetId(), labelId2) << "Remaining label should have correct ID";
    EXPECT_EQ(remainingLabel->title, wxString("Label 2")) << "Remaining label should be 'Label 2'";
}

TEST_F(Au3LabelsInteractionsTests, CutLabel)
{
    //! [GIVEN] There is a project with a label track containing three labels
    Au3TrackList& tracks = Au3TrackList::Get(projectRef());
    Au3LabelTrack* labelTrack = ::LabelTrack::Create(tracks);
    ASSERT_NE(labelTrack, nullptr) << "Failed to create label track";

    //! [GIVEN] Add three labels directly
    SelectedRegion region1;
    region1.setTimes(0.0, 1.0);
    TrackItemId labelId1 = labelTrack->AddLabel(region1, wxString("Label 1"));

    SelectedRegion region2;
    region2.setTimes(2.0, 3.0);
    TrackItemId labelId2 = labelTrack->AddLabel(region2, wxString("Label 2"));

    SelectedRegion region3;
    region3.setTimes(4.0, 5.0);
    TrackItemId labelId3 = labelTrack->AddLabel(region3, wxString("Label 3"));

    //! [GIVEN] The number of labels is 3
    ASSERT_EQ(labelTrack->GetNumLabels(), 3) << "Precondition failed: The number of labels is not 3";
    ASSERT_EQ(labelTrack->GetLabel(0)->GetId(), labelId1) << "First label should be Label 1";
    ASSERT_EQ(labelTrack->GetLabel(1)->GetId(), labelId2) << "Second label should be Label 2";
    ASSERT_EQ(labelTrack->GetLabel(2)->GetId(), labelId3) << "Third label should be Label 3";

    const LabelKey labelKey { labelTrack->GetId(), labelId2 };

    //! [EXPECT] The project is notified about label removed and track changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutLabelRemoved(Truly([&](const Label& label) {
        return label.key.trackId == TrackId(labelTrack->GetId()) && label.key.itemId == labelId2;
    }))).Times(1);
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackChanged(Truly([&](const Track& track) {
        return track.id == TrackId(labelTrack->GetId());
    }))).Times(1);

    //! [WHEN] Cut the middle label into the clipboard
    ITrackDataPtr data = m_labelsInteraction->cutLabel(labelKey);

    //! [THEN] The operation returns valid data
    ASSERT_NE(data, nullptr) << "Cut label should return valid track data";

    //! [THEN] The number of labels is 2
    ASSERT_EQ(labelTrack->GetNumLabels(), 2) << "The number of labels after the cut operation is not 2";

    //! [THEN] The cut label is no longer in the track
    ASSERT_EQ(labelTrack->GetLabelIndex(labelId2), -1) << "labelId2 should be removed";

    //! [THEN] The remaining labels are correct
    const Au3Label* firstLabel = labelTrack->GetLabel(0);
    ASSERT_NE(firstLabel, nullptr) << "First label should exist";
    ASSERT_EQ(firstLabel->GetId(), labelId1) << "First label should have correct ID";
    EXPECT_EQ(firstLabel->title, wxString("Label 1")) << "First label should be 'Label 1'";

    const Au3Label* lastLabel = labelTrack->GetLabel(1);
    ASSERT_NE(lastLabel, nullptr) << "Last label should exist";
    ASSERT_EQ(lastLabel->GetId(), labelId3) << "Last label should have correct ID";
    EXPECT_EQ(lastLabel->title, wxString("Label 3")) << "Last label should be 'Label 3'";
}

TEST_F(Au3LabelsInteractionsTests, CopyLabel)
{
    //! [GIVEN] There is a project with a label track containing two labels
    Au3TrackList& tracks = Au3TrackList::Get(projectRef());
    Au3LabelTrack* labelTrack = ::LabelTrack::Create(tracks);
    ASSERT_NE(labelTrack, nullptr) << "Failed to create label track";

    //! [GIVEN] Add two labels
    SelectedRegion region1;
    region1.setTimes(1.0, 2.0);
    TrackItemId labelId1 = labelTrack->AddLabel(region1, wxString("Test Label 1"));

    SelectedRegion region2;
    region2.setTimes(3.0, 4.0);
    TrackItemId labelId2 = labelTrack->AddLabel(region2, wxString("Test Label 2"));

    //! [GIVEN] The label track contains two labels
    ASSERT_EQ(labelTrack->GetNumLabels(), 2) << "Precondition failed: The number of labels is not 2";
    ASSERT_EQ(labelTrack->GetLabel(0)->GetId(), labelId1) << "First label should be Label 1";
    ASSERT_EQ(labelTrack->GetLabel(1)->GetId(), labelId2) << "Second label should be Label 2";

    const LabelKey labelKey { labelTrack->GetId(), labelId1 };

    //! [WHEN] Copy the first label
    ITrackDataPtr data = m_labelsInteraction->copyLabel(labelKey);

    //! [THEN] The operation returns valid data
    ASSERT_NE(data, nullptr) << "Copy label should return valid track data";

    //! [THEN] Both original labels are still in the track
    ASSERT_EQ(labelTrack->GetNumLabels(), 2) << "Both original labels should still be present";

    //! [THEN] The labels have correct IDs and properties
    const Au3Label* firstLabel = labelTrack->GetLabel(0);
    ASSERT_NE(firstLabel, nullptr) << "First label should exist";
    ASSERT_EQ(firstLabel->GetId(), labelId1) << "First label should have correct ID";
    ASSERT_DOUBLE_EQ(firstLabel->getT0(), 1.0) << "First label start time should be 1.0";
    ASSERT_DOUBLE_EQ(firstLabel->getT1(), 2.0) << "First label end time should be 2.0";
    EXPECT_EQ(firstLabel->title, wxString("Test Label 1")) << "First label title should be unchanged";

    const Au3Label* secondLabel = labelTrack->GetLabel(1);
    ASSERT_NE(secondLabel, nullptr) << "Second label should exist";
    ASSERT_EQ(secondLabel->GetId(), labelId2) << "Second label should have correct ID";
    ASSERT_DOUBLE_EQ(secondLabel->getT0(), 3.0) << "Second label start time should be 3.0";
    ASSERT_DOUBLE_EQ(secondLabel->getT1(), 4.0) << "Second label end time should be 4.0";
    EXPECT_EQ(secondLabel->title, wxString("Test Label 2")) << "Second label title should be unchanged";
}

TEST_F(Au3LabelsInteractionsTests, CopyLabelWithInvalidKey)
{
    //! [GIVEN] There is a project with a label track containing one label
    Au3TrackList& tracks = Au3TrackList::Get(projectRef());
    Au3LabelTrack* labelTrack = ::LabelTrack::Create(tracks);
    ASSERT_NE(labelTrack, nullptr) << "Failed to create label track";

    //! [GIVEN] Add a label
    SelectedRegion region;
    region.setTimes(1.0, 2.0);
    TrackItemId labelId = labelTrack->AddLabel(region, wxString("Test Label"));
    ASSERT_EQ(labelTrack->GetNumLabels(), 1) << "Label track should contain one label";
    ASSERT_EQ(labelTrack->GetLabel(0)->GetId(), labelId) << "Label should have correct ID";

    //! [WHEN] Try to copy a label with an invalid ID
    const LabelKey invalidLabelKey { labelTrack->GetId(), 999999 };
    ITrackDataPtr data = m_labelsInteraction->copyLabel(invalidLabelKey);

    //! [THEN] The operation returns nullptr
    ASSERT_EQ(data, nullptr) << "Copy label with invalid key should return nullptr";

    //! [THEN] The original label is still in the track and unchanged
    ASSERT_EQ(labelTrack->GetNumLabels(), 1) << "Original label should still be present";
    ASSERT_EQ(labelTrack->GetLabel(0)->GetId(), labelId) << "Original label should still have correct ID";
}

TEST_F(Au3LabelsInteractionsTests, CutLabelWithInvalidKey)
{
    //! [GIVEN] There is a project with a label track containing one label
    Au3TrackList& tracks = Au3TrackList::Get(projectRef());
    Au3LabelTrack* labelTrack = ::LabelTrack::Create(tracks);
    ASSERT_NE(labelTrack, nullptr) << "Failed to create label track";

    //! [GIVEN] Add a label
    SelectedRegion region;
    region.setTimes(1.0, 2.0);
    TrackItemId labelId = labelTrack->AddLabel(region, wxString("Test Label"));
    ASSERT_EQ(labelTrack->GetNumLabels(), 1) << "Label track should contain one label";
    ASSERT_EQ(labelTrack->GetLabel(0)->GetId(), labelId) << "Label should have correct ID";

    //! [EXPECT] No notifications should be sent for invalid operations
    EXPECT_CALL(*m_trackEditProject, notifyAboutLabelRemoved(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackChanged(_)).Times(0);

    //! [WHEN] Try to cut a label with an invalid ID
    const LabelKey invalidLabelKey { labelTrack->GetId(), 999999 };
    ITrackDataPtr data = m_labelsInteraction->cutLabel(invalidLabelKey);

    //! [THEN] The operation returns nullptr
    ASSERT_EQ(data, nullptr) << "Cut label with invalid key should return nullptr";

    //! [THEN] The original label is still in the track and unchanged
    ASSERT_EQ(labelTrack->GetNumLabels(), 1) << "Original label should still be present";
    ASSERT_EQ(labelTrack->GetLabel(0)->GetId(), labelId) << "Original label should still have correct ID";
}

TEST_F(Au3LabelsInteractionsTests, MoveLabelsRight)
{
    //! [GIVEN] There is a project with a label track containing three labels
    Au3TrackList& tracks = Au3TrackList::Get(projectRef());
    Au3LabelTrack* labelTrack = ::LabelTrack::Create(tracks);
    ASSERT_NE(labelTrack, nullptr) << "Failed to create label track";

    //! [GIVEN] Add three labels directly
    SelectedRegion region1;
    region1.setTimes(1.0, 2.0);
    TrackItemId labelId1 = labelTrack->AddLabel(region1, wxString("Label 1"));

    SelectedRegion region2;
    region2.setTimes(3.0, 4.0);
    TrackItemId labelId2 = labelTrack->AddLabel(region2, wxString("Label 2"));

    SelectedRegion region3;
    region3.setTimes(5.0, 6.0);
    TrackItemId labelId3 = labelTrack->AddLabel(region3, wxString("Label 3"));

    //! [GIVEN] The label track contains three labels
    ASSERT_EQ(labelTrack->GetNumLabels(), 3) << "Precondition failed: The number of labels is not 3";
    ASSERT_EQ(labelTrack->GetLabel(0)->GetId(), labelId1) << "First label should be Label 1";
    ASSERT_EQ(labelTrack->GetLabel(1)->GetId(), labelId2) << "Second label should be Label 2";
    ASSERT_EQ(labelTrack->GetLabel(2)->GetId(), labelId3) << "Third label should be Label 3";

    //! [GIVEN] All three labels are selected
    LabelKeyList selectedLabels = {
        { labelTrack->GetId(), labelId1 },
        { labelTrack->GetId(), labelId2 },
        { labelTrack->GetId(), labelId3 }
    };
    ON_CALL(*m_selectionController, selectedLabels()).WillByDefault(Return(selectedLabels));

    //! [EXPECT] The project is notified about each label change
    EXPECT_CALL(*m_trackEditProject, notifyAboutLabelChanged(_)).Times(3);

    const double timeOffset = 2.0;

    //! [WHEN] Move the labels right
    bool result = m_labelsInteraction->moveLabels(timeOffset, true);

    //! [THEN] The operation is successful
    ASSERT_TRUE(result) << "Moving labels should succeed";

    //! [THEN] All labels are moved by the offset
    const Au3Label* label1 = labelTrack->GetLabel(0);
    ASSERT_NE(label1, nullptr) << "First label should exist";
    ASSERT_EQ(label1->GetId(), labelId1) << "First label should have correct ID";
    ASSERT_DOUBLE_EQ(label1->getT0(), 3.0) << "First label start time should be 3.0";
    ASSERT_DOUBLE_EQ(label1->getT1(), 4.0) << "First label end time should be 4.0";

    const Au3Label* label2 = labelTrack->GetLabel(1);
    ASSERT_NE(label2, nullptr) << "Second label should exist";
    ASSERT_EQ(label2->GetId(), labelId2) << "Second label should have correct ID";
    ASSERT_DOUBLE_EQ(label2->getT0(), 5.0) << "Second label start time should be 5.0";
    ASSERT_DOUBLE_EQ(label2->getT1(), 6.0) << "Second label end time should be 6.0";

    const Au3Label* label3 = labelTrack->GetLabel(2);
    ASSERT_NE(label3, nullptr) << "Third label should exist";
    ASSERT_EQ(label3->GetId(), labelId3) << "Third label should have correct ID";
    ASSERT_DOUBLE_EQ(label3->getT0(), 7.0) << "Third label start time should be 7.0";
    ASSERT_DOUBLE_EQ(label3->getT1(), 8.0) << "Third label end time should be 8.0";
}

TEST_F(Au3LabelsInteractionsTests, MoveLabelsLeftWhenLabelIsAtZero)
{
    //! [GIVEN] There is a project with a label track containing labels starting at zero
    Au3TrackList& tracks = Au3TrackList::Get(projectRef());
    Au3LabelTrack* labelTrack = ::LabelTrack::Create(tracks);
    ASSERT_NE(labelTrack, nullptr) << "Failed to create label track";

    //! [GIVEN] Add three labels, first one starting at 0.0
    SelectedRegion region1;
    region1.setTimes(0.0, 1.0);
    TrackItemId labelId1 = labelTrack->AddLabel(region1, wxString("Label 1"));

    SelectedRegion region2;
    region2.setTimes(2.0, 3.0);
    TrackItemId labelId2 = labelTrack->AddLabel(region2, wxString("Label 2"));

    SelectedRegion region3;
    region3.setTimes(4.0, 5.0);
    TrackItemId labelId3 = labelTrack->AddLabel(region3, wxString("Label 3"));

    //! [GIVEN] The label track contains three labels
    ASSERT_EQ(labelTrack->GetNumLabels(), 3) << "Precondition failed: The number of labels is not 3";
    ASSERT_EQ(labelTrack->GetLabel(0)->GetId(), labelId1) << "First label should be Label 1";
    ASSERT_EQ(labelTrack->GetLabel(1)->GetId(), labelId2) << "Second label should be Label 2";
    ASSERT_EQ(labelTrack->GetLabel(2)->GetId(), labelId3) << "Third label should be Label 3";

    //! [GIVEN] All three labels are selected
    LabelKeyList selectedLabels = {
        { labelTrack->GetId(), labelId1 },
        { labelTrack->GetId(), labelId2 },
        { labelTrack->GetId(), labelId3 }
    };
    ON_CALL(*m_selectionController, selectedLabels()).WillByDefault(Return(selectedLabels));

    //! [EXPECT] The project is notified about each label change
    EXPECT_CALL(*m_trackEditProject, notifyAboutLabelChanged(_)).Times(3);

    const double timeOffset = -1.0;

    //! [WHEN] Move the labels left
    bool result = m_labelsInteraction->moveLabels(timeOffset, true);

    //! [THEN] The operation is successful
    ASSERT_TRUE(result) << "Moving labels should succeed";

    //! [THEN] Labels are moved but clamped to not go below zero
    const Au3Label* label1 = labelTrack->GetLabel(0);
    ASSERT_NE(label1, nullptr) << "First label should exist";
    ASSERT_EQ(label1->GetId(), labelId1) << "First label should have correct ID";
    ASSERT_DOUBLE_EQ(label1->getT0(), 0.0) << "First label start time should be clamped to 0.0";
    ASSERT_DOUBLE_EQ(label1->getT1(), 0.0) << "First label end time should be clamped to 0.0";

    const Au3Label* label2 = labelTrack->GetLabel(1);
    ASSERT_NE(label2, nullptr) << "Second label should exist";
    ASSERT_EQ(label2->GetId(), labelId2) << "Second label should have correct ID";
    ASSERT_DOUBLE_EQ(label2->getT0(), 1.0) << "Second label start time should be 1.0";
    ASSERT_DOUBLE_EQ(label2->getT1(), 2.0) << "Second label end time should be 2.0";

    const Au3Label* label3 = labelTrack->GetLabel(2);
    ASSERT_NE(label3, nullptr) << "Third label should exist";
    ASSERT_EQ(label3->GetId(), labelId3) << "Third label should have correct ID";
    ASSERT_DOUBLE_EQ(label3->getT0(), 3.0) << "Third label start time should be 3.0";
    ASSERT_DOUBLE_EQ(label3->getT1(), 4.0) << "Third label end time should be 4.0";
}

TEST_F(Au3LabelsInteractionsTests, MoveLabelsWithZeroOffset)
{
    //! [GIVEN] There is a project with a label track containing one label
    Au3TrackList& tracks = Au3TrackList::Get(projectRef());
    Au3LabelTrack* labelTrack = ::LabelTrack::Create(tracks);
    ASSERT_NE(labelTrack, nullptr) << "Failed to create label track";

    //! [GIVEN] Add a label
    SelectedRegion region;
    region.setTimes(1.0, 2.0);
    TrackItemId labelId = labelTrack->AddLabel(region, wxString("Test Label"));
    ASSERT_EQ(labelTrack->GetNumLabels(), 1) << "Label track should contain one label";
    ASSERT_EQ(labelTrack->GetLabel(0)->GetId(), labelId) << "Label should have correct ID";

    //! [GIVEN] The label is selected
    LabelKeyList selectedLabels = { { labelTrack->GetId(), labelId } };
    ON_CALL(*m_selectionController, selectedLabels()).WillByDefault(Return(selectedLabels));

    //! [EXPECT] No notifications should be sent when moving by zero
    EXPECT_CALL(*m_trackEditProject, notifyAboutLabelChanged(_)).Times(0);

    //! [WHEN] Move the label by zero offset
    bool result = m_labelsInteraction->moveLabels(0.0, true);

    //! [THEN] The operation succeeds but does nothing
    ASSERT_TRUE(result) << "Moving by zero should succeed";

    //! [THEN] The label position is unchanged
    const Au3Label* label = labelTrack->GetLabel(0);
    ASSERT_NE(label, nullptr) << "Label should exist";
    ASSERT_EQ(label->GetId(), labelId) << "Label should have correct ID";
    ASSERT_DOUBLE_EQ(label->getT0(), 1.0) << "Label start time should be unchanged";
    ASSERT_DOUBLE_EQ(label->getT1(), 2.0) << "Label end time should be unchanged";
}

TEST_F(Au3LabelsInteractionsTests, MoveLabelsWithNoSelection)
{
    //! [GIVEN] There is a project with a label track containing labels
    Au3TrackList& tracks = Au3TrackList::Get(projectRef());
    Au3LabelTrack* labelTrack = ::LabelTrack::Create(tracks);
    ASSERT_NE(labelTrack, nullptr) << "Failed to create label track";

    //! [GIVEN] Add a label
    SelectedRegion region;
    region.setTimes(1.0, 2.0);
    TrackItemId labelId = labelTrack->AddLabel(region, wxString("Test Label"));
    ASSERT_EQ(labelTrack->GetNumLabels(), 1) << "Label track should contain one label";
    ASSERT_EQ(labelTrack->GetLabel(0)->GetId(), labelId) << "Label should have correct ID";

    //! [GIVEN] No labels are selected
    LabelKeyList emptySelection;
    ON_CALL(*m_selectionController, selectedLabels()).WillByDefault(Return(emptySelection));

    //! [EXPECT] No notifications should be sent
    EXPECT_CALL(*m_trackEditProject, notifyAboutLabelChanged(_)).Times(0);

    //! [WHEN] Try to move labels with no selection
    bool result = m_labelsInteraction->moveLabels(2.0, true);

    //! [THEN] The operation fails
    ASSERT_FALSE(result) << "Moving with no selection should fail";

    //! [THEN] The label position is unchanged
    const Au3Label* label = labelTrack->GetLabel(0);
    ASSERT_NE(label, nullptr) << "Label should exist";
    ASSERT_EQ(label->GetId(), labelId) << "Label should have correct ID";
    ASSERT_DOUBLE_EQ(label->getT0(), 1.0) << "Label start time should be unchanged";
    ASSERT_DOUBLE_EQ(label->getT1(), 2.0) << "Label end time should be unchanged";
}

TEST_F(Au3LabelsInteractionsTests, StretchSingleLabelLeft)
{
    //! [GIVEN] There is a project with a label track containing one label
    Au3TrackList& tracks = Au3TrackList::Get(projectRef());
    Au3LabelTrack* labelTrack = ::LabelTrack::Create(tracks);
    ASSERT_NE(labelTrack, nullptr) << "Failed to create label track";

    //! [GIVEN] Add a label
    SelectedRegion region;
    region.setTimes(2.0, 5.0);
    TrackItemId labelId = labelTrack->AddLabel(region, wxString("Test Label"));
    ASSERT_EQ(labelTrack->GetNumLabels(), 1) << "Label track should contain one label";
    ASSERT_EQ(labelTrack->GetLabel(0)->GetId(), labelId) << "Label should have correct ID";

    const LabelKey labelKey { labelTrack->GetId(), labelId };

    //! [EXPECT] The project is notified about label changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutLabelChanged(Truly([&](const Label& label) {
        return label.key.trackId == TrackId(labelTrack->GetId()) && label.key.itemId == labelId;
    }))).Times(1);

    //! [WHEN] Stretch the label from the left to new start time
    const secs_t newStartTime = 3.0;
    bool result = m_labelsInteraction->stretchLabelLeft(labelKey, newStartTime, true);

    //! [THEN] The operation is successful
    ASSERT_TRUE(result) << "Stretching label left should succeed";

    //! [THEN] The label is stretched (start time changed, end time unchanged)
    const Au3Label* stretchedLabel = labelTrack->GetLabel(0);
    ASSERT_NE(stretchedLabel, nullptr) << "Label should exist";
    ASSERT_EQ(stretchedLabel->GetId(), labelId) << "Label should have correct ID";
    ASSERT_DOUBLE_EQ(stretchedLabel->getT0(), 3.0) << "Label start time should be 3.0";
    ASSERT_DOUBLE_EQ(stretchedLabel->getT1(), 5.0) << "Label end time should be unchanged at 5.0";
}

TEST_F(Au3LabelsInteractionsTests, StretchSingleLabelRight)
{
    //! [GIVEN] There is a project with a label track containing one label
    Au3TrackList& tracks = Au3TrackList::Get(projectRef());
    Au3LabelTrack* labelTrack = ::LabelTrack::Create(tracks);
    ASSERT_NE(labelTrack, nullptr) << "Failed to create label track";

    //! [GIVEN] Add a label
    SelectedRegion region;
    region.setTimes(2.0, 5.0);
    TrackItemId labelId = labelTrack->AddLabel(region, wxString("Test Label"));
    ASSERT_EQ(labelTrack->GetNumLabels(), 1) << "Label track should contain one label";
    ASSERT_EQ(labelTrack->GetLabel(0)->GetId(), labelId) << "Label should have correct ID";

    const LabelKey labelKey { labelTrack->GetId(), labelId };

    //! [EXPECT] The project is notified about label changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutLabelChanged(Truly([&](const Label& label) {
        return label.key.trackId == TrackId(labelTrack->GetId()) && label.key.itemId == labelId;
    }))).Times(1);

    //! [WHEN] Stretch the label from the right to new end time
    const secs_t newEndTime = 7.0;
    bool result = m_labelsInteraction->stretchLabelRight(labelKey, newEndTime, true);

    //! [THEN] The operation is successful
    ASSERT_TRUE(result) << "Stretching label right should succeed";

    //! [THEN] The label is stretched (end time changed, start time unchanged)
    const Au3Label* stretchedLabel = labelTrack->GetLabel(0);
    ASSERT_NE(stretchedLabel, nullptr) << "Label should exist";
    ASSERT_EQ(stretchedLabel->GetId(), labelId) << "Label should have correct ID";
    ASSERT_DOUBLE_EQ(stretchedLabel->getT0(), 2.0) << "Label start time should be unchanged at 2.0";
    ASSERT_DOUBLE_EQ(stretchedLabel->getT1(), 7.0) << "Label end time should be 7.0";
}

TEST_F(Au3LabelsInteractionsTests, StretchLabelLeftBeyondEndTime)
{
    //! [GIVEN] There is a project with a label track containing one label
    Au3TrackList& tracks = Au3TrackList::Get(projectRef());
    Au3LabelTrack* labelTrack = ::LabelTrack::Create(tracks);
    ASSERT_NE(labelTrack, nullptr) << "Failed to create label track";

    //! [GIVEN] Add a label
    SelectedRegion region;
    region.setTimes(2.0, 5.0);
    TrackItemId labelId = labelTrack->AddLabel(region, wxString("Test Label"));
    ASSERT_EQ(labelTrack->GetNumLabels(), 1) << "Label track should contain one label";
    ASSERT_EQ(labelTrack->GetLabel(0)->GetId(), labelId) << "Label should have correct ID";

    const LabelKey labelKey { labelTrack->GetId(), labelId };

    //! [EXPECT] The project is notified about label changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutLabelChanged(_)).Times(1);

    //! [WHEN] Stretch the label from the left beyond the end time
    const secs_t newStartTime = 6.0;  // Beyond the end time of 5.0
    bool result = m_labelsInteraction->stretchLabelLeft(labelKey, newStartTime, true);

    //! [THEN] The operation succeeds
    ASSERT_TRUE(result) << "Stretching label left beyond end time should succeed";

    //! [THEN] The label has inverted times (start > end)
    const Au3Label* stretchedLabel = labelTrack->GetLabel(0);
    ASSERT_NE(stretchedLabel, nullptr) << "Label should exist";
    ASSERT_EQ(stretchedLabel->GetId(), labelId) << "Label should have correct ID";
    ASSERT_DOUBLE_EQ(stretchedLabel->getT0(), 5.0) << "Label start time should be 5.0";
    ASSERT_DOUBLE_EQ(stretchedLabel->getT1(), 6.0) << "Label end time should be 6.0";
}

TEST_F(Au3LabelsInteractionsTests, StretchLabelRightBeforeStartTime)
{
    //! [GIVEN] There is a project with a label track containing one label
    Au3TrackList& tracks = Au3TrackList::Get(projectRef());
    Au3LabelTrack* labelTrack = ::LabelTrack::Create(tracks);
    ASSERT_NE(labelTrack, nullptr) << "Failed to create label track";

    //! [GIVEN] Add a label
    SelectedRegion region;
    region.setTimes(3.0, 6.0);
    TrackItemId labelId = labelTrack->AddLabel(region, wxString("Test Label"));
    ASSERT_EQ(labelTrack->GetNumLabels(), 1) << "Label track should contain one label";
    ASSERT_EQ(labelTrack->GetLabel(0)->GetId(), labelId) << "Label should have correct ID";

    const LabelKey labelKey { labelTrack->GetId(), labelId };

    //! [EXPECT] The project is notified about label changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutLabelChanged(_)).Times(1);

    //! [WHEN] Stretch the label from the right before the start time
    const secs_t newEndTime = 2.0;  // Before the start time of 3.0
    bool result = m_labelsInteraction->stretchLabelRight(labelKey, newEndTime, true);

    //! [THEN] The operation succeeds
    ASSERT_TRUE(result) << "Stretching label right before start time should succeed";

    //! [THEN] The label has inverted times (end < start)
    const Au3Label* stretchedLabel = labelTrack->GetLabel(0);
    ASSERT_NE(stretchedLabel, nullptr) << "Label should exist";
    ASSERT_EQ(stretchedLabel->GetId(), labelId) << "Label should have correct ID";
    ASSERT_DOUBLE_EQ(stretchedLabel->getT0(), 2.0) << "Label start time should be 2.0";
    ASSERT_DOUBLE_EQ(stretchedLabel->getT1(), 3.0) << "Label end time should be 3.0";
}

TEST_F(Au3LabelsInteractionsTests, StretchLabelLeftWithInvalidKey)
{
    //! [GIVEN] There is a project with a label track containing one label
    Au3TrackList& tracks = Au3TrackList::Get(projectRef());
    Au3LabelTrack* labelTrack = ::LabelTrack::Create(tracks);
    ASSERT_NE(labelTrack, nullptr) << "Failed to create label track";

    //! [GIVEN] Add a label
    SelectedRegion region;
    region.setTimes(2.0, 5.0);
    TrackItemId labelId = labelTrack->AddLabel(region, wxString("Test Label"));
    ASSERT_EQ(labelTrack->GetNumLabels(), 1) << "Label track should contain one label";
    ASSERT_EQ(labelTrack->GetLabel(0)->GetId(), labelId) << "Label should have correct ID";

    //! [EXPECT] No notifications should be sent for invalid operations
    EXPECT_CALL(*m_trackEditProject, notifyAboutLabelChanged(_)).Times(0);

    //! [WHEN] Try to stretch a label with an invalid ID
    const LabelKey invalidLabelKey { labelTrack->GetId(), 999999 };
    bool result = m_labelsInteraction->stretchLabelLeft(invalidLabelKey, 3.0, true);

    //! [THEN] The operation fails
    ASSERT_FALSE(result) << "Stretching label with invalid key should fail";

    //! [THEN] The original label is unchanged
    const Au3Label* label = labelTrack->GetLabel(0);
    ASSERT_NE(label, nullptr) << "Label should exist";
    ASSERT_EQ(label->GetId(), labelId) << "Label should have correct ID";
    ASSERT_DOUBLE_EQ(label->getT0(), 2.0) << "Label start time should be unchanged";
    ASSERT_DOUBLE_EQ(label->getT1(), 5.0) << "Label end time should be unchanged";
}

TEST_F(Au3LabelsInteractionsTests, StretchLabelRightWithInvalidKey)
{
    //! [GIVEN] There is a project with a label track containing one label
    Au3TrackList& tracks = Au3TrackList::Get(projectRef());
    Au3LabelTrack* labelTrack = ::LabelTrack::Create(tracks);
    ASSERT_NE(labelTrack, nullptr) << "Failed to create label track";

    //! [GIVEN] Add a label
    SelectedRegion region;
    region.setTimes(2.0, 5.0);
    TrackItemId labelId = labelTrack->AddLabel(region, wxString("Test Label"));
    ASSERT_EQ(labelTrack->GetNumLabels(), 1) << "Label track should contain one label";
    ASSERT_EQ(labelTrack->GetLabel(0)->GetId(), labelId) << "Label should have correct ID";

    //! [EXPECT] No notifications should be sent for invalid operations
    EXPECT_CALL(*m_trackEditProject, notifyAboutLabelChanged(_)).Times(0);

    //! [WHEN] Try to stretch a label with an invalid ID
    const LabelKey invalidLabelKey { labelTrack->GetId(), 999999 };
    bool result = m_labelsInteraction->stretchLabelRight(invalidLabelKey, 7.0, true);

    //! [THEN] The operation fails
    ASSERT_FALSE(result) << "Stretching label with invalid key should fail";

    //! [THEN] The original label is unchanged
    const Au3Label* label = labelTrack->GetLabel(0);
    ASSERT_NE(label, nullptr) << "Label should exist";
    ASSERT_EQ(label->GetId(), labelId) << "Label should have correct ID";
    ASSERT_DOUBLE_EQ(label->getT0(), 2.0) << "Label start time should be unchanged";
    ASSERT_DOUBLE_EQ(label->getT1(), 5.0) << "Label end time should be unchanged";
}
}
