/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>

#include "trackedit/internal/au3/au3interaction.h"

#include "log.h"

using namespace au::au3;

class au3interactionTests : public ::testing::Test
{
public:
};

TEST_F(au3interactionTests, FirstTest)
{
    bool ok = true;
    EXPECT_TRUE(ok);
}