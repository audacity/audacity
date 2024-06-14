/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>

#include "containers.h"

using namespace muse;

class ProjectScene_SampleTests : public ::testing::Test
{
public:
};

TEST_F(ProjectScene_SampleTests, Move)
{
    // [GIVEN]
    std::vector<int> v { 1, 2, 3, 4, 5, 6, 7, 8, 9 };

    // [WHEN]
    moveItem(v, 1, 4);
    moveItem(v, 5, 1);
    moveItem(v, 3, 3);
    moveItem(v, 6, 12);

    // [THEN]
    std::vector<int> expected { 1, 6, 3, 4, 5, 2, 8, 9, 7 };
    EXPECT_EQ(v, expected);
}
