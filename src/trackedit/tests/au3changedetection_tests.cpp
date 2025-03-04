/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include "../internal/au3/au3changedetection.h"

#include "context/tests/mocks/globalcontextmock.h"
#include "project/tests/mocks/audacityprojectmock.h"
#include "mocks/trackeditprojectmock.h"

#include "au3wrap/internal/au3project.h"
#include "au3wrap/internal/domaccessor.h"

using ::testing::Return;
using ::testing::Truly;
using ::testing::_;

using namespace au;
using namespace au::au3;

namespace au::trackedit {
class Au3ChangeDetectionTests : public ::testing::Test
{
protected:
    void SetUp() override
    {
        m_au3ChangeDetection = std::make_shared<Au3ChangeDetection>();

        m_globalContext = std::make_shared<context::GlobalContextMock>();
        m_au3ChangeDetection->globalContext.set(m_globalContext);

        m_trackEditProject = std::make_shared<TrackeditProjectMock>();
        ON_CALL(*m_globalContext, currentTrackeditProject())
        .WillByDefault(Return(m_trackEditProject));

        m_currentProject = std::make_shared<project::AudacityProjectMock>();
        ON_CALL(*m_globalContext, currentProject())
        .WillByDefault(Return(m_currentProject));

        initTestProject();
    }

    void initTestProject()
    {
        m_au3ProjectAccessor = std::make_shared<au3::Au3ProjectAccessor>();
        const muse::io::path_t TEST_PROJECT_PATH = muse::String::fromUtf8(trackedit_tests_DATA_ROOT) + "/data/test.aup3";
        muse::Ret ret = m_au3ProjectAccessor->load(TEST_PROJECT_PATH);

        ON_CALL(*m_currentProject, au3ProjectPtr())
        .WillByDefault(Return(m_au3ProjectAccessor->au3ProjectPtr()));
    }

    Au3Project& projectRef() const
    {
        auto project = reinterpret_cast<Au3Project*>(m_au3ProjectAccessor->au3ProjectPtr());
        return *project;
    }

    void TearDown() override
    {
        m_au3ProjectAccessor->close();
    }

    std::shared_ptr<Au3ChangeDetection> m_au3ChangeDetection;

    std::shared_ptr<context::GlobalContextMock> m_globalContext;
    std::shared_ptr<project::AudacityProjectMock> m_currentProject;
    std::shared_ptr<TrackeditProjectMock> m_trackEditProject;

    std::shared_ptr<au3::Au3ProjectAccessor> m_au3ProjectAccessor;
};

TEST_F(Au3ChangeDetectionTests, TestTrackNotifications)
{
}
}
