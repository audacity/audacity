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
    EXPECT_EQ(m_formatter->snapTime(0.1, SnapType::Seconds, false, m_timeSignature), 0.0);
    EXPECT_EQ(m_formatter->snapTime(0.49, SnapType::Seconds, false, m_timeSignature), 0.0);
    EXPECT_EQ(m_formatter->snapTime(0.5, SnapType::Seconds, false, m_timeSignature), 1.0);
    EXPECT_EQ(m_formatter->snapTime(0.6, SnapType::Seconds, false, m_timeSignature), 1.0);

    EXPECT_EQ(m_formatter->snapTime(0.01, SnapType::Deciseconds, false, m_timeSignature), 0.0);
    EXPECT_EQ(m_formatter->snapTime(0.049, SnapType::Deciseconds, false, m_timeSignature), 0.0);
    EXPECT_EQ(m_formatter->snapTime(0.05, SnapType::Deciseconds, false, m_timeSignature), 0.1);
    EXPECT_EQ(m_formatter->snapTime(0.06, SnapType::Deciseconds, false, m_timeSignature), 0.1);

    EXPECT_EQ(m_formatter->snapTime(0.001, SnapType::Centiseconds, false, m_timeSignature), 0.0);
    EXPECT_EQ(m_formatter->snapTime(0.0049, SnapType::Centiseconds, false, m_timeSignature), 0.0);
    EXPECT_EQ(m_formatter->snapTime(0.005, SnapType::Centiseconds, false, m_timeSignature), 0.01);
    EXPECT_EQ(m_formatter->snapTime(0.006, SnapType::Centiseconds, false, m_timeSignature), 0.01);

    EXPECT_EQ(m_formatter->snapTime(0.0001, SnapType::Milliseconds, false, m_timeSignature), 0.0);
    EXPECT_EQ(m_formatter->snapTime(0.00049, SnapType::Milliseconds, false, m_timeSignature), 0.0);
    EXPECT_EQ(m_formatter->snapTime(0.0005, SnapType::Milliseconds, false, m_timeSignature), 0.001);
    EXPECT_EQ(m_formatter->snapTime(0.0006, SnapType::Milliseconds, false, m_timeSignature), 0.001);
}

TEST_F(SnapTimeFormatterTests, Snap_Beats)
{
    EXPECT_EQ(m_formatter->snapTime(0.9, SnapType::Bar, false, m_timeSignature), 0.0);
    EXPECT_EQ(m_formatter->snapTime(1.1, SnapType::Bar, false, m_timeSignature), 2.0);

    EXPECT_EQ(m_formatter->snapTime(0.9, SnapType::Half, false, m_timeSignature), 1.0);
    EXPECT_EQ(m_formatter->snapTime(1.1, SnapType::Half, false, m_timeSignature), 1.0);
    EXPECT_EQ(m_formatter->snapTime(1.4, SnapType::Half, false, m_timeSignature), 1.0);

    EXPECT_EQ(m_formatter->snapTime(1.1, SnapType::Quarter, false, m_timeSignature), 1.0);
    EXPECT_EQ(m_formatter->snapTime(1.4, SnapType::Quarter, false, m_timeSignature), 1.5);

    EXPECT_EQ(m_formatter->snapTime(1.2, SnapType::Eighth, false, m_timeSignature), 1.25);
    EXPECT_EQ(m_formatter->snapTime(1.3, SnapType::Eighth, false, m_timeSignature), 1.25);

    EXPECT_EQ(m_formatter->snapTime(1.15, SnapType::Sixteenth, false, m_timeSignature), 1.125);
    EXPECT_EQ(m_formatter->snapTime(1.11, SnapType::Sixteenth, false, m_timeSignature), 1.125);

    EXPECT_EQ(m_formatter->snapTime(1.07, SnapType::ThirtySecond, false, m_timeSignature), 1.0625);
    EXPECT_EQ(m_formatter->snapTime(1.05, SnapType::ThirtySecond, false, m_timeSignature), 1.0625);

    EXPECT_EQ(m_formatter->snapTime(1.033, SnapType::SixtyFourth, false, m_timeSignature), 1.03125);
    EXPECT_EQ(m_formatter->snapTime(1.03, SnapType::SixtyFourth, false, m_timeSignature), 1.03125);

    EXPECT_EQ(m_formatter->snapTime(1.016, SnapType::HundredTwentyEighth, false, m_timeSignature), 1.015625);
    EXPECT_EQ(m_formatter->snapTime(1.014, SnapType::HundredTwentyEighth, false, m_timeSignature), 1.015625);
}

TEST_F(SnapTimeFormatterTests, SingleStep_Time)
{
    EXPECT_EQ(m_formatter->singleStep(0.0, SnapType::Seconds, false, Direction::Right, m_timeSignature), 1.0);
    EXPECT_EQ(m_formatter->singleStep(0.4, SnapType::Seconds, false, Direction::Right, m_timeSignature), 1.0);
    EXPECT_EQ(m_formatter->singleStep(1.0, SnapType::Seconds, false, Direction::Left, m_timeSignature), 0.0);

    EXPECT_EQ(m_formatter->singleStep(0.4, SnapType::Deciseconds, false, Direction::Left, m_timeSignature), 0.3);
    EXPECT_EQ(m_formatter->singleStep(0.4, SnapType::Deciseconds, false, Direction::Right, m_timeSignature), 0.5);

    EXPECT_EQ(m_formatter->singleStep(0.4, SnapType::Centiseconds, false, Direction::Right, m_timeSignature), 0.41);
    EXPECT_EQ(m_formatter->singleStep(0.4, SnapType::Centiseconds, false, Direction::Left, m_timeSignature), 0.39);

    EXPECT_EQ(m_formatter->singleStep(0.4, SnapType::Milliseconds, false, Direction::Left, m_timeSignature), 0.399);
    EXPECT_EQ(m_formatter->singleStep(0.4, SnapType::Milliseconds, false, Direction::Right, m_timeSignature), 0.401);
}

TEST_F(SnapTimeFormatterTests, SingleStep_Beats)
{
    EXPECT_EQ(m_formatter->singleStep(0.0, SnapType::Bar, false, Direction::Right, m_timeSignature), 2.0);
    EXPECT_EQ(m_formatter->singleStep(2.0, SnapType::Bar, false, Direction::Left, m_timeSignature), 0.0);

    EXPECT_EQ(m_formatter->singleStep(0.0, SnapType::Half, false, Direction::Right, m_timeSignature), 1.0);
    EXPECT_EQ(m_formatter->singleStep(2.0, SnapType::Half, false, Direction::Left, m_timeSignature), 1.0);

    EXPECT_EQ(m_formatter->singleStep(0.0, SnapType::Quarter, false, Direction::Right, m_timeSignature), 0.5);
    EXPECT_EQ(m_formatter->singleStep(2.0, SnapType::Quarter, false, Direction::Left, m_timeSignature), 1.5);

    EXPECT_EQ(m_formatter->singleStep(0.0, SnapType::Eighth, false, Direction::Right, m_timeSignature), 0.25);
    EXPECT_EQ(m_formatter->singleStep(2.0, SnapType::Eighth, false, Direction::Left, m_timeSignature), 1.75);

    EXPECT_EQ(m_formatter->singleStep(0.0, SnapType::Sixteenth, false, Direction::Right, m_timeSignature), 0.125);
    EXPECT_EQ(m_formatter->singleStep(2.0, SnapType::Sixteenth, false, Direction::Left, m_timeSignature), 1.875);

    EXPECT_EQ(m_formatter->singleStep(0.0, SnapType::ThirtySecond, false, Direction::Right, m_timeSignature), 0.0625);
    EXPECT_EQ(m_formatter->singleStep(2.0, SnapType::ThirtySecond, false, Direction::Left, m_timeSignature), 1.9375);

    EXPECT_EQ(m_formatter->singleStep(0.0, SnapType::SixtyFourth, false, Direction::Right, m_timeSignature), 0.03125);
    EXPECT_EQ(m_formatter->singleStep(2.0, SnapType::SixtyFourth, false, Direction::Left, m_timeSignature), 1.96875);

    EXPECT_EQ(m_formatter->singleStep(0.0, SnapType::HundredTwentyEighth, false, Direction::Right, m_timeSignature), 0.015625);
    EXPECT_EQ(m_formatter->singleStep(2.0, SnapType::HundredTwentyEighth, false, Direction::Left, m_timeSignature), 1.984375);
}
}
