/*
* Audacity: A Digital Audio Editor
*/

#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include "context/tests/mocks/globalcontextmock.h"
#include "project/tests/mocks/audacityprojectmock.h"

#include "project/internal/au3/au3metadata.h"

#include "au3-project/Project.h"
#include "au3-tags/Tags.h"

using ::testing::NiceMock;
using ::testing::Return;

namespace au::project {
class Au3MetadataTests : public ::testing::Test
{
public:
    void SetUp() override
    {
        m_ctx = std::make_shared<muse::modularity::Context>(302);
        muse::modularity::ioc(m_ctx)->registerExport<context::IGlobalContext>("utests", m_globalContext);

        ON_CALL(*m_globalContext, currentProject())
        .WillByDefault(Return(m_currentProject));
        ON_CALL(*m_currentProject, au3ProjectPtr())
        .WillByDefault(Return(reinterpret_cast<uintptr_t>(m_au3Project.get())));

        m_metadata = std::make_unique<Au3Metadata>(m_ctx);
    }

    void TearDown() override
    {
        m_metadata.reset();
        muse::modularity::removeIoC(m_ctx);
    }

protected:
    Tags& projectTags() { return Tags::Get(*m_au3Project); }

    muse::modularity::ContextPtr m_ctx;
    const std::shared_ptr<NiceMock<context::GlobalContextMock> > m_globalContext
        = std::make_shared<NiceMock<context::GlobalContextMock> >();
    const std::shared_ptr<NiceMock<AudacityProjectMock> > m_currentProject
        = std::make_shared<NiceMock<AudacityProjectMock> >();
    const std::shared_ptr<AudacityProject> m_au3Project = AudacityProject::Create();
    std::unique_ptr<Au3Metadata> m_metadata;
};

TEST_F(Au3MetadataTests, WritesEachFieldToItsStandardTag)
{
    //! [GIVEN] Metadata with a distinct value in each standard field
    ProjectMeta meta;
    meta.trackTitle = "Title";
    meta.artist = "Artist";
    meta.album = "Album";
    meta.trackNumber = "3";
    meta.year = "2026";
    meta.comments = "Comment";

    //! [WHEN] Writing it to the project
    m_metadata->setTags(meta);

    //! [THEN] Each value is under its tag
    EXPECT_EQ(projectTags().GetTag(TAG_TITLE).ToStdString(), "Title");
    EXPECT_EQ(projectTags().GetTag(TAG_ARTIST).ToStdString(), "Artist");
    EXPECT_EQ(projectTags().GetTag(TAG_ALBUM).ToStdString(), "Album");
    EXPECT_EQ(projectTags().GetTag(TAG_TRACK).ToStdString(), "3");
    EXPECT_EQ(projectTags().GetTag(TAG_YEAR).ToStdString(), "2026");
    EXPECT_EQ(projectTags().GetTag(TAG_COMMENTS).ToStdString(), "Comment");
}
}
