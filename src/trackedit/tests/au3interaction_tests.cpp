/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>

#include "../project/internal/audacityproject.h"
#include "trackedit/internal/au3/au3interaction.h"
#include "au3wrap/internal/au3project.h"

// TODO: What is this?
#include "log.h"

using namespace au::au3;
using namespace au::trackedit;

static const std::string TEST_DATA_DIR("/data/");
static const std::string SAMPLE_FILE("test.aup3");

class au3interactionTests : public ::testing::Test
{
public:
    void SetUp() override
    {
        m_au3Interaction = std::make_unique<Au3Interaction>();
        m_au3ProjectCreator = std::make_unique<Au3ProjectCreator>();
    }

    std::shared_ptr<IAu3ProjectCreator> m_au3ProjectCreator;
    std::unique_ptr<Au3Interaction> m_au3Interaction;
};

TEST_F(au3interactionTests, FirstTest)
{
    auto project = std::make_shared<au::project::Audacity4Project>();

    project->au3ProjectCreator.set(m_au3ProjectCreator);

    auto filePath = trackedit_tests_DATA_ROOT + TEST_DATA_DIR + SAMPLE_FILE;
    muse::io::path_t loadPath = filePath;
    std::string format = muse::io::suffix(filePath);

    muse::Ret ret = project->load(loadPath, false /*forceMode*/, format);

    EXPECT_EQ(ret, make_ret(muse::Ret::Code::Ok));
}