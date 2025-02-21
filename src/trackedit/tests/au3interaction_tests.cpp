/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>

#include "../project/internal/audacityproject.h"
#include "trackedit/internal/au3/au3interaction.h"
#include "au3wrap/internal/au3project.h"
#include "libraries/lib-project-file-io/ProjectFileIO.h"
#include "../au3/tests/MockedPrefs.h"
#include "../au3/tests/MockedAudio.h"

#include "projectscene/internal/projectsceneconfiguration.h"

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

        m_project = std::make_shared<au::project::Audacity4Project>();

        m_configuration = std::make_shared<au::projectscene::ProjectSceneConfiguration>();

        m_project->au3ProjectCreator.set(m_au3ProjectCreator);

        m_configuration->init();

        bool ok = ProjectFileIO::InitializeSQL();

        EXPECT_TRUE(ok);
    }

    std::shared_ptr<IAu3ProjectCreator> m_au3ProjectCreator;
    std::shared_ptr<au::projectscene::ProjectSceneConfiguration> m_configuration;

    std::unique_ptr<Au3Interaction> m_au3Interaction;
    std::shared_ptr<au::project::Audacity4Project> m_project;
};

TEST_F(au3interactionTests, FirstTest)
{
    MockedPrefs mockedPrefs;

    // Right now on this branch I've just removed an assert in
    //MockedAudio mockedAudio;

    auto filePath = trackedit_tests_DATA_ROOT + TEST_DATA_DIR + SAMPLE_FILE;
    muse::io::path_t loadPath = filePath;
    std::string format = muse::io::suffix(filePath);

    // TODO:
    // I got as far as the project actually loading, and hitting the TrackColor constructor,
    // where it crashes because projectSceneConfiguration() is not set.
    // I'm not yet sure how that is injected, when setting up mocks like this, to TrackColor.
    // Is it because I haven't instantiated an ioc()?

    muse::Ret ret = m_project->load(loadPath, false /*forceMode*/, format);

    EXPECT_EQ(ret, make_ret(muse::Ret::Code::Ok));
}
