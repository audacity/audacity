/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>

#include "trackedit/dom/trackeditproject.h"

#include "log.h"

using namespace muse;
using namespace au::trackedit;

class ProjectScene_SampleTests : public ::testing::Test
{
public:
};

TEST_F(ProjectScene_SampleTests, Sample)
{
    TrackeditProject prj;
    UNUSED(prj);
}
