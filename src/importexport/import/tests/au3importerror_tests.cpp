/*
 * Audacity: A Digital Audio Editor
 */

#include <gmock/gmock.h>
#include <QFile>
#include <QTemporaryDir>

#include "au3wrap/au3wrapmodule.h"
#include "au3wrap/internal/au3project.h"
#include "importexport/import/importermodule.h"
#include "importexport/import/internal/au3/au3importer.h"
#include "importexport/labels/internal/au3/au3labelsimporter.h"
#include "interactive/tests/mocks/interactivemock.h"
#include "project/tests/mocks/audacityprojectmock.h"
#include "projectscene/tests/mocks/globalcontextmock.h"
#include "testing/environment.h"
#include "testing/testcontext.h"

namespace au::importexport {
namespace {
using namespace ::testing;

static auto suiteEnvironment = muse::testing::SuiteEnvironment()
                               .setDependencyModules({ new au3::Au3WrapModule(), new ImporterModule() });

class Au3ImportErrorTests : public Test
{
protected:
    void SetUp() override
    {
        ASSERT_TRUE(m_directory.isValid());
        m_path = m_directory.filePath("invalid.mp3");
        QFile file(m_path.toQString());
        ASSERT_TRUE(file.open(QIODevice::WriteOnly));
        ASSERT_GT(file.write("This file contains no audio."), 0);
        file.close();

        auto project = std::make_shared<NiceMock<project::AudacityProjectMock> >();
        ON_CALL(*project, au3ProjectPtr()).WillByDefault(Return(m_project.au3ProjectPtr()));

        auto context = std::make_shared<NiceMock<projectscene::GlobalContextMock> >();
        ON_CALL(*context, currentProject()).WillByDefault(Return(project));

        auto* ioc = muse::modularity::ioc(m_context);
        ioc->registerExport<context::IGlobalContext>("utests", context);
        ioc->registerExport<ILabelsImporter>("utests", std::make_shared<Au3LabelsImporter>(m_context));
        ioc->registerExport<muse::IInteractive>("utests", m_interactive);
    }

    void TearDown() override
    {
        muse::modularity::removeIoC(m_context);
    }

    void expectImportError()
    {
        using Result = muse::IInteractive::Result;
        EXPECT_CALL(*m_interactive, error("Error Importing", Field(&muse::IInteractive::Text::text,
                                                                  AllOf(HasSubstr("did not recognize the type"), HasSubstr("invalid.mp3"))),
                                          _, _, _, _))
        .WillOnce(Return(muse::async::make_promise<Result>([](auto resolve) { return resolve(Result {}); })));
    }

    QTemporaryDir m_directory;
    muse::io::path_t m_path;
    muse::modularity::ContextPtr m_context = testutils::makeTestContext();
    au3::Au3ProjectAccessor m_project { m_context };
    Au3Importer m_importer { m_context };
    std::shared_ptr<NiceMock<muse::InteractiveMock> > m_interactive = std::make_shared<NiceMock<muse::InteractiveMock> >();
};

TEST_F(Au3ImportErrorTests, FileImportShowsImporterError)
{
    expectImportError();
    EXPECT_FALSE(m_importer.import(m_path));
}

TEST_F(Au3ImportErrorTests, TrackImportShowsImporterError)
{
    expectImportError();
    EXPECT_FALSE(m_importer.importIntoTrack(m_path, 1, 0.0));
}
}
}
