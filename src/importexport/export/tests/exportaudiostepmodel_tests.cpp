/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>

#include <QStringList>

#include "importexport/export/view/exportaudiostepmodel.h"

using namespace au::importexport;

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

TEST(ExportAudioStepModelTests, StepDataGivesBackTheLoadedData)
{
    const QVariantMap stored {
        { "format", "mp3" },
        { "quality", "cbr-192" },
        { "sampleRate", "44100" },
        { "channels", "Mono" },
        { "folder", "/out" },
        { "fileName", "{ProjectName}-x" },
    };

    ExportAudioStepModel model;
    model.load(stored);

    EXPECT_EQ(model.stepData(), stored);
}

TEST(ExportAudioStepModelTests, FormatChangeRebuildsTheRows)
{
    ExportAudioStepModel model;
    model.load({});
    EXPECT_EQ(titles(model), QStringList({ "Format", "Encoding", "Channels", "Sample rate" }));

    model.setValue(rowOf(model, "Format"), "flac");

    EXPECT_EQ(titles(model), QStringList({ "Format", "Bit depth", "Level", "Channels", "Sample rate" }));
    EXPECT_EQ(model.stepData().value("format"), "flac");
    EXPECT_EQ(model.stepData().value("bitDepth"), "16");
    EXPECT_EQ(model.stepData().value("level"), "5");
}

TEST(ExportAudioStepModelTests, Mp3ModeChangeReplacesTheValueList)
{
    ExportAudioStepModel model;
    model.load({ { "format", "mp3" } });
    EXPECT_EQ(titles(model), QStringList({ "Format", "Bit rate mode", "Quality", "Channels", "Sample rate" }));

    model.setValue(rowOf(model, "Bit rate mode"), "cbr");

    const int bitRateRow = rowOf(model, "Bit rate");
    ASSERT_GE(bitRateRow, 0);
    EXPECT_EQ(model.data(model.index(bitRateRow), ExportAudioStepModel::OptionsRole).toList().size(),
              static_cast<qsizetype>(Mp3BitRateContract::table.size()));
    EXPECT_EQ(model.stepData().value("quality"), "cbr-192");
}

TEST(ExportAudioStepModelTests, Mp3FormatResetsASampleRateMp3DoesNotSupport)
{
    ExportAudioStepModel model;
    model.load({ { "format", "wav" }, { "sampleRate", "96000" } });

    model.setValue(rowOf(model, "Format"), "mp3");

    EXPECT_EQ(model.stepData().value("sampleRate"), "project");
}
