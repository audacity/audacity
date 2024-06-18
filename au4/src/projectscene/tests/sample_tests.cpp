/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>

#include "processing/dom/processingproject.h"

#include "log.h"

using namespace muse;
using namespace au::processing;

class ProjectScene_SampleTests : public ::testing::Test
{
public:
};

TEST_F(ProjectScene_SampleTests, Sample)
{
    ProcessingProject prj;
    UNUSED(prj);
}
