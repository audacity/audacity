/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include "../view/timeline/snaptimeformatter.h"

namespace au::projectscene {
class SnapTimeFormatterTests : public ::testing::Test
{
public:

    void SetUp() override
    {
        m_formatter = std::make_shared<SnapTimeFormatter>();
        m_timeSignature = { 120, 4, 4 };
    }

protected:
    std::shared_ptr<SnapTimeFormatter> m_formatter = nullptr;
    trackedit::TimeSignature m_timeSignature;
};

TEST_F(SnapTimeFormatterTests, Snap_Time)
{
    Snap snap = { SnapType::Seconds, true, false };
    EXPECT_EQ(m_formatter->snapTime(0.1, snap, m_timeSignature), 0.0);
    EXPECT_EQ(m_formatter->snapTime(0.49, snap, m_timeSignature), 0.0);
    EXPECT_EQ(m_formatter->snapTime(0.5, snap, m_timeSignature), 1.0);
    EXPECT_EQ(m_formatter->snapTime(0.6, snap, m_timeSignature), 1.0);

    snap = { SnapType::Deciseconds, true, false };
    EXPECT_EQ(m_formatter->snapTime(0.01, snap, m_timeSignature), 0.0);
    EXPECT_EQ(m_formatter->snapTime(0.049, snap, m_timeSignature), 0.0);
    EXPECT_EQ(m_formatter->snapTime(0.05, snap, m_timeSignature), 0.1);
    EXPECT_EQ(m_formatter->snapTime(0.06, snap, m_timeSignature), 0.1);

    snap = { SnapType::Centiseconds, true, false };
    EXPECT_EQ(m_formatter->snapTime(0.001, snap, m_timeSignature), 0.0);
    EXPECT_EQ(m_formatter->snapTime(0.0049, snap, m_timeSignature), 0.0);
    EXPECT_EQ(m_formatter->snapTime(0.005, snap, m_timeSignature), 0.01);
    EXPECT_EQ(m_formatter->snapTime(0.006, snap, m_timeSignature), 0.01);

    snap = { SnapType::Milliseconds, true, false };
    EXPECT_EQ(m_formatter->snapTime(0.0001, snap, m_timeSignature), 0.0);
    EXPECT_EQ(m_formatter->snapTime(0.00049, snap, m_timeSignature), 0.0);
    EXPECT_EQ(m_formatter->snapTime(0.0005, snap, m_timeSignature), 0.001);
    EXPECT_EQ(m_formatter->snapTime(0.0006, snap, m_timeSignature), 0.001);
}

TEST_F(SnapTimeFormatterTests, Snap_Beats)
{
    Snap snap = { SnapType::Bar, true, false };
    EXPECT_EQ(m_formatter->snapTime(0.9, snap, m_timeSignature), 0.0);
    EXPECT_EQ(m_formatter->snapTime(1.1, snap, m_timeSignature), 2.0);

    snap = { SnapType::Half, true, false };
    EXPECT_EQ(m_formatter->snapTime(0.9, snap, m_timeSignature), 1.0);
    EXPECT_EQ(m_formatter->snapTime(1.1, snap, m_timeSignature), 1.0);
    EXPECT_EQ(m_formatter->snapTime(1.4, snap, m_timeSignature), 1.0);

    snap = { SnapType::Quarter, true, false };
    EXPECT_EQ(m_formatter->snapTime(1.1, snap, m_timeSignature), 1.0);
    EXPECT_EQ(m_formatter->snapTime(1.4, snap, m_timeSignature), 1.5);

    snap = { SnapType::Eighth, true, false };
    EXPECT_EQ(m_formatter->snapTime(1.2, snap, m_timeSignature), 1.25);
    EXPECT_EQ(m_formatter->snapTime(1.3, snap, m_timeSignature), 1.25);

    snap = { SnapType::Sixteenth, true, false };
    EXPECT_EQ(m_formatter->snapTime(1.15, snap, m_timeSignature), 1.125);
    EXPECT_EQ(m_formatter->snapTime(1.11, snap, m_timeSignature), 1.125);

    snap = { SnapType::ThirtySecond, true, false };
    EXPECT_EQ(m_formatter->snapTime(1.07, snap, m_timeSignature), 1.0625);
    EXPECT_EQ(m_formatter->snapTime(1.05, snap, m_timeSignature), 1.0625);

    snap = { SnapType::SixtyFourth, true, false };
    EXPECT_EQ(m_formatter->snapTime(1.033, snap, m_timeSignature), 1.03125);
    EXPECT_EQ(m_formatter->snapTime(1.03, snap, m_timeSignature), 1.03125);

    snap = { SnapType::HundredTwentyEighth, true, false };
    EXPECT_EQ(m_formatter->snapTime(1.016, snap, m_timeSignature), 1.015625);
    EXPECT_EQ(m_formatter->snapTime(1.014, snap, m_timeSignature), 1.015625);
}

TEST_F(SnapTimeFormatterTests, SingleStep_Time)
{
    Snap snap = { SnapType::Seconds, true, false };
    EXPECT_EQ(m_formatter->singleStep(0.0, snap, Direction::Right, m_timeSignature), 1.0);
    EXPECT_EQ(m_formatter->singleStep(0.4, snap, Direction::Right, m_timeSignature), 1.0);
    EXPECT_EQ(m_formatter->singleStep(1.0, snap, Direction::Left, m_timeSignature), 0.0);

    snap = { SnapType::Deciseconds, true, false };
    EXPECT_EQ(m_formatter->singleStep(0.4, snap, Direction::Left, m_timeSignature), 0.3);
    EXPECT_EQ(m_formatter->singleStep(0.4, snap, Direction::Right, m_timeSignature), 0.5);

    snap = { SnapType::Centiseconds, true, false };
    EXPECT_EQ(m_formatter->singleStep(0.4, snap, Direction::Right, m_timeSignature), 0.41);
    EXPECT_EQ(m_formatter->singleStep(0.4, snap, Direction::Left, m_timeSignature), 0.39);

    snap = { SnapType::Milliseconds, true, false };
    EXPECT_EQ(m_formatter->singleStep(0.4, snap, Direction::Left, m_timeSignature), 0.399);
    EXPECT_EQ(m_formatter->singleStep(0.4, snap, Direction::Right, m_timeSignature), 0.401);
}

TEST_F(SnapTimeFormatterTests, SingleStep_Beats)
{
    Snap snap = { SnapType::Bar, true, false };
    EXPECT_EQ(m_formatter->singleStep(0.0, snap, Direction::Right, m_timeSignature), 2.0);
    EXPECT_EQ(m_formatter->singleStep(2.0, snap, Direction::Left, m_timeSignature), 0.0);

    snap = { SnapType::Half, true, false };
    EXPECT_EQ(m_formatter->singleStep(0.0, snap, Direction::Right, m_timeSignature), 1.0);
    EXPECT_EQ(m_formatter->singleStep(2.0, snap, Direction::Left, m_timeSignature), 1.0);

    snap = { SnapType::Quarter, true, false };
    EXPECT_EQ(m_formatter->singleStep(0.0, snap, Direction::Right, m_timeSignature), 0.5);
    EXPECT_EQ(m_formatter->singleStep(2.0, snap, Direction::Left, m_timeSignature), 1.5);

    snap = { SnapType::Eighth, true, false };
    EXPECT_EQ(m_formatter->singleStep(0.0, snap, Direction::Right, m_timeSignature), 0.25);
    EXPECT_EQ(m_formatter->singleStep(2.0, snap, Direction::Left, m_timeSignature), 1.75);

    snap = { SnapType::Sixteenth, true, false };
    EXPECT_EQ(m_formatter->singleStep(0.0, snap, Direction::Right, m_timeSignature), 0.125);
    EXPECT_EQ(m_formatter->singleStep(2.0, snap, Direction::Left, m_timeSignature), 1.875);

    snap = { SnapType::ThirtySecond, true, false };
    EXPECT_EQ(m_formatter->singleStep(0.0, snap, Direction::Right, m_timeSignature), 0.0625);
    EXPECT_EQ(m_formatter->singleStep(2.0, snap, Direction::Left, m_timeSignature), 1.9375);

    snap = { SnapType::SixtyFourth, true, false };
    EXPECT_EQ(m_formatter->singleStep(0.0, snap, Direction::Right, m_timeSignature), 0.03125);
    EXPECT_EQ(m_formatter->singleStep(2.0, snap, Direction::Left, m_timeSignature), 1.96875);

    snap = { SnapType::HundredTwentyEighth, true, false };
    EXPECT_EQ(m_formatter->singleStep(0.0, snap, Direction::Right, m_timeSignature), 0.015625);
    EXPECT_EQ(m_formatter->singleStep(2.0, snap, Direction::Left, m_timeSignature), 1.984375);
}
}
