/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include <vector>

#include "../internal/playheadmapper.h"

namespace {
constexpr int64_t TEST_LOOP_START = 100;
constexpr int64_t TEST_LOOP_END = 200;
constexpr int64_t TEST_LOOP_LENGTH = TEST_LOOP_END - TEST_LOOP_START;
constexpr int64_t TEST_POSITION_IN_LOOP = TEST_LOOP_START + TEST_LOOP_LENGTH / 2;
constexpr int64_t TEST_POSITION_BEFORE_LOOP = TEST_LOOP_START / 2;
constexpr int64_t TEST_POSITION_AFTER_LOOP = TEST_LOOP_END + TEST_LOOP_LENGTH;
//! Wraps around the loop several times
constexpr int64_t TEST_BLOCK_SIZE = 5 * TEST_LOOP_LENGTH + TEST_LOOP_LENGTH / 3;
constexpr int64_t TEST_DEVICE_ADVANCE = TEST_LOOP_LENGTH / 4;
}

namespace au::au4audio {
class PlayheadMapperTests : public ::testing::Test
{
protected:
    PlayheadMapper m_mapper;

    //! The timeline positions of `length` indices from `index`, segment by segment
    std::vector<int64_t> positions(int64_t index, int64_t length) const
    {
        std::vector<int64_t> positions;
        for (int64_t done = 0; done < length;) {
            const auto segment = m_mapper.Map(index + done, static_cast<size_t>(length - done));
            const auto segmentLength = static_cast<int64_t>(segment.len);
            EXPECT_GT(segmentLength, 0);
            for (int64_t i = 0; i < segmentLength; ++i) {
                positions.push_back(segment.position.as_long_long() + i);
            }
            done += segmentLength;
        }
        return positions;
    }

    //! The positions the mapper should give, looping from `from`
    static std::vector<int64_t> looped(int64_t from, int64_t length)
    {
        std::vector<int64_t> positions;
        for (auto position = from; static_cast<int64_t>(positions.size()) < length; ++position) {
            if (position == TEST_LOOP_END) {
                position = TEST_LOOP_START;
            }
            positions.push_back(position);
        }
        return positions;
    }

    static std::vector<int64_t> linear(int64_t from, int64_t length)
    {
        std::vector<int64_t> positions;
        for (int64_t i = 0; i < length; ++i) {
            positions.push_back(from + i);
        }
        return positions;
    }
};

TEST_F(PlayheadMapperTests, PlaysLinearlyWithoutLoop)
{
    m_mapper.start(TEST_POSITION_IN_LOOP, std::nullopt);

    EXPECT_EQ(positions(0, TEST_BLOCK_SIZE), linear(TEST_POSITION_IN_LOOP, TEST_BLOCK_SIZE));
    EXPECT_EQ(m_mapper.linearPosition(TEST_BLOCK_SIZE), TEST_POSITION_IN_LOOP + TEST_BLOCK_SIZE);
    EXPECT_FALSE(m_mapper.loop().has_value());
}

TEST_F(PlayheadMapperTests, LoopsFromWithinTheLoop)
{
    m_mapper.start(TEST_POSITION_IN_LOOP, PlayheadMapper::Range { TEST_LOOP_START, TEST_LOOP_END });

    EXPECT_EQ(positions(0, TEST_BLOCK_SIZE), looped(TEST_POSITION_IN_LOOP, TEST_BLOCK_SIZE));
}

TEST_F(PlayheadMapperTests, RollsIntoTheLoop)
{
    m_mapper.start(TEST_POSITION_BEFORE_LOOP, PlayheadMapper::Range { TEST_LOOP_START, TEST_LOOP_END });

    auto expected = linear(TEST_POSITION_BEFORE_LOOP, TEST_LOOP_START - TEST_POSITION_BEFORE_LOOP);
    const auto rest = looped(TEST_LOOP_START, TEST_BLOCK_SIZE - static_cast<int64_t>(expected.size()));
    expected.insert(expected.end(), rest.begin(), rest.end());
    EXPECT_EQ(positions(0, TEST_BLOCK_SIZE), expected);
}

TEST_F(PlayheadMapperTests, StartsRightOfTheLoopAtItsStart)
{
    m_mapper.start(TEST_POSITION_AFTER_LOOP, PlayheadMapper::Range { TEST_LOOP_START, TEST_LOOP_END });

    EXPECT_EQ(m_mapper.position(), TEST_LOOP_START);
}

TEST_F(PlayheadMapperTests, SeeksWhereTheDeviceIs)
{
    m_mapper.start(TEST_POSITION_BEFORE_LOOP, std::nullopt);
    m_mapper.advance(TEST_DEVICE_ADVANCE);

    m_mapper.setPosition(TEST_POSITION_AFTER_LOOP);

    EXPECT_EQ(m_mapper.position(), TEST_POSITION_AFTER_LOOP);
    EXPECT_EQ(positions(TEST_DEVICE_ADVANCE, TEST_BLOCK_SIZE), linear(TEST_POSITION_AFTER_LOOP, TEST_BLOCK_SIZE));
}

TEST_F(PlayheadMapperTests, SeeksWithinTheLoopWhileLooping)
{
    m_mapper.start(TEST_POSITION_IN_LOOP, PlayheadMapper::Range { TEST_LOOP_START, TEST_LOOP_END });
    m_mapper.advance(TEST_DEVICE_ADVANCE);

    m_mapper.setPosition(TEST_POSITION_BEFORE_LOOP);
    EXPECT_EQ(m_mapper.position(), TEST_LOOP_START);

    m_mapper.setPosition(TEST_POSITION_AFTER_LOOP);
    EXPECT_EQ(m_mapper.position(), TEST_LOOP_END - 1);
}

TEST_F(PlayheadMapperTests, RollsOnWhenSeekingWhileRollingIn)
{
    m_mapper.start(0, PlayheadMapper::Range { TEST_LOOP_START, TEST_LOOP_END });
    m_mapper.advance(TEST_DEVICE_ADVANCE);

    m_mapper.setPosition(TEST_POSITION_BEFORE_LOOP);

    EXPECT_EQ(m_mapper.position(), TEST_POSITION_BEFORE_LOOP);
}

TEST_F(PlayheadMapperTests, PlaysOnWhenTheLoopIsTurnedOff)
{
    m_mapper.start(TEST_POSITION_IN_LOOP, PlayheadMapper::Range { TEST_LOOP_START, TEST_LOOP_END });
    m_mapper.advance(TEST_LOOP_LENGTH);

    m_mapper.setLoop(std::nullopt);

    EXPECT_EQ(positions(TEST_LOOP_LENGTH, TEST_BLOCK_SIZE), linear(TEST_POSITION_IN_LOOP, TEST_BLOCK_SIZE));
}

TEST_F(PlayheadMapperTests, KeepsItsPositionWhenTheLoopEndMoves)
{
    constexpr int64_t newLoopEnd = TEST_LOOP_END + TEST_LOOP_LENGTH;
    m_mapper.start(TEST_POSITION_IN_LOOP, PlayheadMapper::Range { TEST_LOOP_START, TEST_LOOP_END });
    m_mapper.advance(TEST_DEVICE_ADVANCE);

    m_mapper.setLoop(PlayheadMapper::Range { TEST_LOOP_START, newLoopEnd });

    EXPECT_EQ(m_mapper.position(), TEST_POSITION_IN_LOOP + TEST_DEVICE_ADVANCE);
    EXPECT_EQ(static_cast<int64_t>(m_mapper.Map(TEST_DEVICE_ADVANCE, TEST_BLOCK_SIZE).len), newLoopEnd - m_mapper.position());
}

TEST_F(PlayheadMapperTests, JumpsIntoALoopMovedAway)
{
    m_mapper.start(TEST_POSITION_IN_LOOP, PlayheadMapper::Range { TEST_LOOP_START, TEST_LOOP_END });

    m_mapper.setLoop(PlayheadMapper::Range { TEST_POSITION_AFTER_LOOP, TEST_POSITION_AFTER_LOOP + TEST_LOOP_LENGTH });

    EXPECT_EQ(m_mapper.position(), TEST_POSITION_AFTER_LOOP);
}
}
