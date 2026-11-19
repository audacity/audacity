/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>

#include <QQmlContext>
#include <QQmlEngine>
#include <QStringList>

#include "global/modularity/ioc.h"

#include "importexport/export/view/exportaudiostepmodel.h"

#include "mocks/exportermock.h"

using namespace au::importexport;
using ::testing::NiceMock;

namespace {
QStringList titles(const ExportAudioStepModel& model)
{
    QStringList list;
    for (int row = 0; row < model.rowCount(); ++row) {
        list << model.data(model.index(row), ExportAudioStepModel::TitleRole).toString();
    }
    return list;
}

int rowOf(const ExportAudioStepModel& model, const QString& title)
{
    return static_cast<int>(titles(model).indexOf(title));
}
}

class ExportAudioStepModelTests : public ::testing::Test
{
protected:
    void SetUp() override
    {
        m_exporter = std::make_shared<NiceMock<ExporterMock> >();
        m_testCtx = std::make_shared<muse::modularity::Context>(999);
        muse::modularity::ioc(m_testCtx)->registerExportNoDelete<IExporter>("utests", m_exporter.get());

        m_qmlIoc = new muse::QmlIoCContext(&m_engine);
        m_qmlIoc->ctx = m_testCtx;
        m_engine.rootContext()->setContextProperty("ioc_context", QVariant::fromValue(m_qmlIoc));

        QQmlEngine::setContextForObject(&m_model, m_engine.rootContext());
    }

    void TearDown() override
    {
        muse::modularity::ioc(m_testCtx)->unregister<IExporter>("utests");
        muse::modularity::removeIoC(m_testCtx);
    }

    QQmlEngine m_engine;
    muse::QmlIoCContext* m_qmlIoc = nullptr;
    muse::modularity::ContextPtr m_testCtx;
    std::shared_ptr<ExporterMock> m_exporter;
    ExportAudioStepModel m_model;
};

TEST_F(ExportAudioStepModelTests, StepDataGivesBackTheLoadedData)
{
    const QVariantMap stored {
        { "format", "mp3" },
        { "quality", "cbr-192" },
        { "sampleRate", "44100" },
        { "channels", "Mono" },
        { "folder", "/out" },
        { "fileName", "{ProjectName}-x" },
    };

    m_model.load(stored);

    EXPECT_EQ(m_model.stepData(), stored);
}

TEST_F(ExportAudioStepModelTests, FormatChangeRebuildsTheRows)
{
    m_model.load({});
    EXPECT_EQ(titles(m_model), QStringList({ "Format", "Encoding", "Channels", "Sample rate" }));

    m_model.setValue(rowOf(m_model, "Format"), "flac");

    EXPECT_EQ(titles(m_model), QStringList({ "Format", "Bit depth", "Level", "Channels", "Sample rate" }));
    EXPECT_EQ(m_model.stepData().value("format"), "flac");
    EXPECT_EQ(m_model.stepData().value("bitDepth"), "16");
    EXPECT_EQ(m_model.stepData().value("level"), "5");
}

TEST_F(ExportAudioStepModelTests, Mp3ModeChangeReplacesTheValueList)
{
    m_model.load({ { "format", "mp3" } });
    EXPECT_EQ(titles(m_model), QStringList({ "Format", "Bit rate mode", "Quality", "Channels", "Sample rate" }));

    m_model.setValue(rowOf(m_model, "Bit rate mode"), "cbr");

    const int bitRateRow = rowOf(m_model, "Bit rate");
    ASSERT_GE(bitRateRow, 0);
    EXPECT_EQ(m_model.data(m_model.index(bitRateRow), ExportAudioStepModel::OptionsRole).toList().size(),
              static_cast<qsizetype>(Mp3BitRateContract::table.size()));
    EXPECT_EQ(m_model.stepData().value("quality"), "cbr-192");
}

TEST_F(ExportAudioStepModelTests, Mp3FormatResetsASampleRateMp3DoesNotSupport)
{
    m_model.load({ { "format", "wav" }, { "sampleRate", "96000" } });

    m_model.setValue(rowOf(m_model, "Format"), "mp3");

    EXPECT_EQ(m_model.stepData().value("sampleRate"), "project");
}
