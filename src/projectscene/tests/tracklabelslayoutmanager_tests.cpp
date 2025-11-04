/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include "projectscene/view/tracksitemsview/tracklabelslayoutmanager.h"
#include "projectscene/view/tracksitemsview/tracklabelslistmodel.h"

#include "context/tests/mocks/globalcontextmock.h"
#include "trackedit/tests/mocks/trackeditprojectmock.h"

#include "trackedit/dom/label.h"

namespace au::projectscene {
class TrackLabelsLayoutManagerTests : public ::testing::Test
{
protected:
    void SetUp() override
    {
        m_layoutManager = new TrackLabelsLayoutManager();

        m_labelsModel = new TrackLabelsListModel();

        m_globalContext = std::make_shared<context::GlobalContextMock>();
        m_labelsModel->globalContext.set(m_globalContext);

        m_trackEditProject = std::make_shared<trackedit::TrackeditProjectMock>();

        ON_CALL(*m_globalContext, currentTrackeditProject())
        .WillByDefault(testing::Return(m_trackEditProject));

        createTimelineContext();
    }

    void TearDown() override
    {
        delete m_layoutManager;
        m_layoutManager = nullptr;

        delete m_labelsModel;
        m_labelsModel = nullptr;
    }

    void addItem(trackedit::TrackItemId itemId, const muse::String& title, double startTime, double endTime, int visualWidth = -1)
    {
        trackedit::TrackItemKey key(1, itemId);
        m_labelsModel->m_allLabelList.push_back(au::trackedit::Label { key,
                                                                       title,
                                                                       muse::draw::Color(255, 255, 255), startTime, endTime });

        ON_CALL(*m_trackEditProject, label(key))
        .WillByDefault(testing::Return(m_labelsModel->m_allLabelList.back()));

        m_labelsModel->update();

        TrackLabelItem* item = m_labelsModel->labelItemByKey(key);
        item->setVisualWidth(visualWidth == -1 ? endTime - startTime : visualWidth);
        item->setVisualHeight(14);
    }

    TrackLabelItem* item(int index) const
    {
        return static_cast<TrackLabelItem*>(m_labelsModel->m_items.at(index));
    }

    void relayout()
    {
        m_layoutManager->relayout();
    }

    void relink()
    {
        m_layoutManager->relink();
    }

    LabelKey leftLinkedLabelKey(const LabelKey& key) const
    {
        return m_layoutManager->m_leftLinkedLabels[key];
    }

    LabelKey rightLinkedLabelKey(const LabelKey& key) const
    {
        return m_layoutManager->m_rightLinkedLabels[key];
    }

    void createTimelineContext()
    {
        m_timelineContext = std::make_shared<projectscene::TimelineContext>();

        m_timelineContext->setFrameStartTime(0);
        m_timelineContext->setFrameEndTime(100);

        m_labelsModel->setTimelineContext(m_timelineContext.get());
    }

    TrackLabelsLayoutManager* m_layoutManager = nullptr;
    TrackLabelsListModel* m_labelsModel = nullptr;

    std::shared_ptr<context::GlobalContextMock> m_globalContext;
    std::shared_ptr<trackedit::TrackeditProjectMock> m_trackEditProject;
    std::shared_ptr<projectscene::TimelineContext> m_timelineContext;
};

TEST_F(TrackLabelsLayoutManagerTests, ItemsOnSameLine)
{
    //! [GIVEN] A layout manager with a model set
    m_layoutManager->setLabelsModel(m_labelsModel);

    //! [WHEN] An item is added
    addItem(1, u"Label 1", 0.0, 10.0);
    addItem(2, u"Label 2", 20.0, 30.0);

    m_layoutManager->init();

    //! [THEN] The items should be on the same levels
    TrackLabelItem* item1 = item(0);
    TrackLabelItem* item2 = item(1);
    ASSERT_EQ(item1->level(), item2->level()) << "Items should be on the same level";
}

TEST_F(TrackLabelsLayoutManagerTests, ItemsOnDifferentLines)
{
    //! [GIVEN] A layout manager with a model set
    m_layoutManager->setLabelsModel(m_labelsModel);

    //! [WHEN] Two items are added with different start times
    addItem(1, u"Label 1", 0.0, 10.0);
    addItem(2, u"Label 2", 5.0, 15.0);

    m_layoutManager->init();

    //! [THEN] The items should be on different levels
    TrackLabelItem* item1 = item(0);
    TrackLabelItem* item2 = item(1);
    ASSERT_NE(item1->level(), item2->level()) << "Items should be on different levels";
    ASSERT_EQ(item1->level(), 0) << "Item 1 should be on level 0";
    ASSERT_EQ(item2->level(), 1) << "Item 2 should be on level 1";
}

TEST_F(TrackLabelsLayoutManagerTests, Points)
{
    //! [GIVEN] A layout manager with a model set
    m_layoutManager->setLabelsModel(m_labelsModel);

    //! [WHEN] Two points are added with different times
    addItem(1, u"Label 1", 10.0, 10.0, 10);
    addItem(2, u"Label 2", 30.0, 30.0, 10);

    m_layoutManager->init();

    //! [THEN] The items should be on same levels
    TrackLabelItem* item1 = item(0);
    TrackLabelItem* item2 = item(1);
    ASSERT_EQ(item1->level(), item2->level()) << "Items should be on same levels";
    ASSERT_EQ(item1->level(), 0) << "Item 1 should be on level 0";
    ASSERT_EQ(item2->level(), 0) << "Item 2 should be on level 0";
}

TEST_F(TrackLabelsLayoutManagerTests, PointsOverlap)
{
    //! [GIVEN] A layout manager with a model set
    m_layoutManager->setLabelsModel(m_labelsModel);

    //! [WHEN] Two points are added with different times
    addItem(1, u"Label 1", 10.0, 10.0, 10);
    addItem(2, u"Label 2", 15.0, 15.0, 10);

    m_layoutManager->init();

    //! [THEN] The items should be on different levels
    TrackLabelItem* item1 = item(0);
    TrackLabelItem* item2 = item(1);
    ASSERT_NE(item1->level(), item2->level()) << "Items should be on different levels";
    ASSERT_EQ(item1->level(), 0) << "Item 1 should be on level 0";
    ASSERT_EQ(item2->level(), 1) << "Item 2 should be on level 1";
}

TEST_F(TrackLabelsLayoutManagerTests, PointAndRegionOverlap_PointOnHigherLevel)
{
    //! [GIVEN] A layout manager with a model set
    m_layoutManager->setLabelsModel(m_labelsModel);

    //! [WHEN] A region is added first, then a point that overlaps with it
    addItem(1, u"Region", 0.0, 20.0);
    addItem(2, u"Point", 10.0, 10.0, 10);

    m_layoutManager->init();

    //! [THEN] The point should be on a higher level than the region
    TrackLabelItem* regionItem = item(0);
    TrackLabelItem* pointItem = item(1);
    ASSERT_EQ(regionItem->level(), 0) << "Region should be on level 0";
    ASSERT_EQ(pointItem->level(), 1) << "Point should be on level 1 (higher level)";
}

TEST_F(TrackLabelsLayoutManagerTests, RegionAndPointOverlap_PointOnHigherLevel)
{
    //! [GIVEN] A layout manager with a model set
    m_layoutManager->setLabelsModel(m_labelsModel);

    //! [WHEN] A point is added first, then a region that overlaps with it
    addItem(1, u"Point", 10.0, 10.0, 10);
    addItem(2, u"Region", 0.0, 20.0);

    m_layoutManager->init();

    //! [THEN] The point should be on a higher level than the region
    TrackLabelItem* pointItem = item(0);
    TrackLabelItem* regionItem = item(1);
    ASSERT_EQ(regionItem->level(), 0) << "Region should be on level 0";
    ASSERT_EQ(pointItem->level(), 1) << "Point should be on level 1 (higher level)";
}

TEST_F(TrackLabelsLayoutManagerTests, TwoRegionsOverlap_LeftmostOnLowerLevel)
{
    //! [GIVEN] A layout manager with a model set
    m_layoutManager->setLabelsModel(m_labelsModel);

    //! [WHEN] Two overlapping regions are added, first one starts earlier
    addItem(1, u"Region 1", 0.0, 15.0);
    addItem(2, u"Region 2", 10.0, 25.0);

    m_layoutManager->init();

    //! [THEN] The region with smaller x should be on level 0
    TrackLabelItem* item1 = item(0);
    TrackLabelItem* item2 = item(1);
    ASSERT_EQ(item1->level(), 0) << "Region 1 (smaller x) should be on level 0";
    ASSERT_EQ(item2->level(), 1) << "Region 2 should be on level 1";
}

TEST_F(TrackLabelsLayoutManagerTests, TwoRegionsOverlap_ReverseOrder)
{
    //! [GIVEN] A layout manager with a model set
    m_layoutManager->setLabelsModel(m_labelsModel);

    //! [WHEN] Two overlapping regions are added, second one starts earlier
    addItem(1, u"Region 1", 10.0, 25.0);
    addItem(2, u"Region 2", 0.0, 15.0);

    m_layoutManager->init();

    //! [THEN] The region with smaller x should be on level 0
    TrackLabelItem* item1 = item(0);
    TrackLabelItem* item2 = item(1);
    ASSERT_EQ(item1->level(), 1) << "Region 1 should be on level 1";
    ASSERT_EQ(item2->level(), 0) << "Region 2 (smaller x) should be on level 0";
}

TEST_F(TrackLabelsLayoutManagerTests, AdjacentRegions_ShouldBeOnSameLevel)
{
    //! [GIVEN] A layout manager with a model set
    m_layoutManager->setLabelsModel(m_labelsModel);

    //! [WHEN] Two regions are added where one starts exactly where another ends
    addItem(1, u"Region 1", 0.0, 10.0);
    addItem(2, u"Region 2", 10.0, 20.0);

    m_layoutManager->init();

    //! [THEN] Both regions should be on the same level (no overlap)
    TrackLabelItem* item1 = item(0);
    TrackLabelItem* item2 = item(1);
    ASSERT_EQ(item1->level(), 0) << "Region 1 should be on level 0";
    ASSERT_EQ(item2->level(), 0) << "Region 2 should be on level 0 (no overlap)";
}

TEST_F(TrackLabelsLayoutManagerTests, TwoPointsOverlap_LeftmostOnLowerLevel)
{
    //! [GIVEN] A layout manager with a model set
    m_layoutManager->setLabelsModel(m_labelsModel);

    //! [WHEN] Two overlapping points are added
    addItem(1, u"Point 1", 5.0, 5.0, 10);
    addItem(2, u"Point 2", 10.0, 10.0, 10);

    m_layoutManager->init();

    //! [THEN] Points should be on different levels, leftmost on lower level
    TrackLabelItem* item1 = item(0);
    TrackLabelItem* item2 = item(1);
    ASSERT_EQ(item1->level(), 0) << "Point 1 (smaller x) should be on level 0";
    ASSERT_EQ(item2->level(), 1) << "Point 2 should be on level 1";
}

TEST_F(TrackLabelsLayoutManagerTests, ThreeRegionsOverlap_LevelsByPosition)
{
    //! [GIVEN] A layout manager with a model set
    m_layoutManager->setLabelsModel(m_labelsModel);

    //! [WHEN] Three overlapping regions are added in non-sorted order
    addItem(1, u"Region 1", 5.0, 20.0);
    addItem(2, u"Region 2", 0.0, 15.0);
    addItem(3, u"Region 3", 10.0, 25.0);

    m_layoutManager->init();

    //! [THEN] Regions should be arranged by position
    TrackLabelItem* item1 = item(0);
    TrackLabelItem* item2 = item(1);
    TrackLabelItem* item3 = item(2);

    ASSERT_EQ(item2->level(), 0) << "Region 2 (x=0) should be on level 0";
    ASSERT_EQ(item1->level(), 1) << "Region 1 (x=5) should be on level 1";
    ASSERT_EQ(item3->level(), 2) << "Region 3 (x=10) should be on level 2";
}

TEST_F(TrackLabelsLayoutManagerTests, MultiplePointsAndRegions_MixedLevels)
{
    //! [GIVEN] A layout manager with a model set
    m_layoutManager->setLabelsModel(m_labelsModel);

    //! [WHEN] Points and regions are mixed with overlaps
    addItem(1, u"Region 1", 0.0, 20.0);
    addItem(2, u"Point 1", 10.0, 10.0, 10);
    addItem(3, u"Region 2", 5.0, 15.0);
    addItem(4, u"Point 2", 12.0, 12.0, 10);

    m_layoutManager->init();

    //! [THEN] Points should be on higher levels than regions they overlap with
    TrackLabelItem* region1 = item(0);
    TrackLabelItem* point1 = item(1);
    TrackLabelItem* point2 = item(3);

    ASSERT_EQ(region1->level(), 0) << "Region 1 should be on level 0";
    ASSERT_GT(point1->level(), region1->level()) << "Point 1 should be on higher level than Region 1";
    ASSERT_GT(point2->level(), region1->level()) << "Point 2 should be on higher level than Region 1";
}

TEST_F(TrackLabelsLayoutManagerTests, LinkedLabels)
{
    //! [GIVEN] A layout manager with a model set
    m_layoutManager->setLabelsModel(m_labelsModel);

    //! [WHEN] Two labels are added where one ends where another begins
    addItem(1, u"Label 1", 0.0, 10.0);
    addItem(2, u"Label 2", 10.0, 20.0);

    m_layoutManager->init();

    //! [THEN] The labels should be linked
    TrackLabelItem* item1 = item(0);
    TrackLabelItem* item2 = item(1);

    LabelKey key1 = item1->key();
    LabelKey key2 = item2->key();
    LabelKey leftLinkedLabel = leftLinkedLabelKey(key1);
    LabelKey rightLinkedLabel = rightLinkedLabelKey(key1);

    ASSERT_EQ(leftLinkedLabel, LabelKey()) << "No left link should be present";
    ASSERT_EQ(rightLinkedLabel, key2) << "Right link should point to Label 2";
}

TEST_F(TrackLabelsLayoutManagerTests, UnlinkedLabels)
{
    //! [GIVEN] A layout manager with a model set
    m_layoutManager->setLabelsModel(m_labelsModel);

    //! [WHEN] Two labels are added with a gap between them
    addItem(1, u"Label 1", 0.0, 10.0);
    addItem(2, u"Label 2", 15.0, 25.0);

    m_layoutManager->init();

    //! [THEN] The labels should not be linked
    TrackLabelItem* item1 = item(0);
    TrackLabelItem* item2 = item(1);

    LabelKey key1 = item1->key();
    LabelKey leftLinkedLabel = leftLinkedLabelKey(key1);
    LabelKey key2 = item2->key();
    LabelKey rightLinkedLabel = rightLinkedLabelKey(key2);

    ASSERT_EQ(leftLinkedLabel, LabelKey()) << "Label 1 should not have a left link";
    ASSERT_EQ(rightLinkedLabel, LabelKey()) << "Label 2 should not have a right link";
}

TEST_F(TrackLabelsLayoutManagerTests, ChainOfLinkedLabels)
{
    //! [GIVEN] A layout manager with a model set
    m_layoutManager->setLabelsModel(m_labelsModel);

    //! [WHEN] Three labels are added in a chain
    addItem(1, u"Label 1", 0.0, 10.0);
    addItem(2, u"Label 2", 10.0, 20.0);
    addItem(3, u"Label 3", 20.0, 30.0);

    m_layoutManager->init();

    //! [THEN] All labels should be linked in a chain
    TrackLabelItem* item1 = item(0);
    TrackLabelItem* item2 = item(1);
    TrackLabelItem* item3 = item(2);

    LabelKey key1 = item1->key();
    LabelKey key2 = item2->key();
    LabelKey key3 = item3->key();

    // Label 1 -> Label 2
    LabelKey rightLinkedLabel = rightLinkedLabelKey(key1);
    ASSERT_EQ(rightLinkedLabel, key2) << "Label 1's right link should point to Label 2";

    // Label 2 -> Label 3
    rightLinkedLabel = rightLinkedLabelKey(key2);
    ASSERT_EQ(rightLinkedLabel, key3) << "Label 2's right link should point to Label 3";

    // Label 2 <- Label 1
    LabelKey leftLinkedLabel = leftLinkedLabelKey(key2);
    ASSERT_EQ(leftLinkedLabel, key1) << "Label 2's left link should point to Label 1";

    // Label 3 <- Label 2
    leftLinkedLabel = leftLinkedLabelKey(key3);
    ASSERT_EQ(leftLinkedLabel, key2) << "Label 3's left link should point to Label 2";
}
}
