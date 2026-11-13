/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include "projectrecovery/internal/projectrecoveryservice.h"

#include "testprojectbuilder.h"

using namespace au::projectrecovery;
using namespace au::projectrecovery::tests;

TEST(ProjectRecoveryServiceTests, StopWaitsForTheJobs)
{
    //! [GIVEN] An inspection of a project that the service runs
    const auto dir = std::filesystem::temp_directory_path() / "au_projectrecovery_tests" / "StopWaitsForTheJobs";
    std::filesystem::remove_all(dir);
    std::filesystem::create_directories(dir);
    const auto path = dir / "project.aup4";
    TestProject project;
    project.tracks.push_back({ "Voice", 44100, { { "Take 1", 0, { { 1, 1000 } } } } });
    writeProjectFile(path, project);
    ProjectRecoveryService service;
    const QFuture<muse::RetVal<Inspection> > future = service.inspect(muse::io::path_t(path.string()));

    //! [WHEN] The service stops
    service.stop();

    //! [THEN] The job has its result
    ASSERT_TRUE(future.isFinished());
    EXPECT_TRUE(future.result().ret) << future.result().ret.toString();
    std::filesystem::remove_all(dir);
}
