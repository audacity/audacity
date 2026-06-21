#include <gtest/gtest.h>

#include <QFile>
#include <QFileInfo>
#include <QScopeGuard>
#include <QTemporaryDir>

#include <filesystem>
#include <memory>
#include <vector>

#ifdef Q_OS_LINUX
#include <QtEndian>
#include <linux/posix_acl.h>
#include <linux/posix_acl_xattr.h>
#include <sys/xattr.h>
#include <unistd.h>
#endif

#include "au3-project/Project.h"
#include "au3-project-history/ProjectHistory.h"
#include "au3-project-history/UndoManager.h"
#include "au3wrap/internal/au3project.h"
#include "importexport/export/OriginalFileInfo.h"

namespace {
struct FormatCodecCase
{
    const char* extension;
    const char* formatID;
    int sampleRate;
    int channels;
    int bitDepth;
    int bitRate;
};

AudacityProject& freshProject()
{
    static std::vector<std::shared_ptr<AudacityProject> > projects;
    projects.push_back(AudacityProject::Create());
    return *projects.back();
}

au::importexport::ExportParameters makeExportParameters()
{
    return {
        { 1, 256000 },
        { 2, 24 },
        { 3, std::string("joint-stereo") }
    };
}
}

class OriginalFileInfoFormatCodecTests : public ::testing::TestWithParam<FormatCodecCase>
{
};

TEST(OriginalFileInfoTests, InitiallyHasNoOriginalFile)
{
    AudacityProject& project = freshProject();
    auto& info = OriginalFileInfo::Get(project);

    EXPECT_FALSE(info.HasOriginalFile());
    EXPECT_TRUE(info.GetOriginalFilePath().isEmpty());
    EXPECT_TRUE(info.GetOriginalFileName().isEmpty());
    EXPECT_TRUE(info.GetExportFormatID().isEmpty());
    EXPECT_TRUE(info.GetCodecSettings().isEmpty());
    EXPECT_TRUE(info.GetExportParameters().empty());
    EXPECT_EQ(info.GetImportedFileCount(), 0);
}

TEST(OriginalFileInfoTests, RemembersImportedFilePathFormatAndCodecSettings)
{
    AudacityProject& project = freshProject();
    auto& info = OriginalFileInfo::Get(project);

    QVariantMap codecSettings;
    codecSettings.insert("sampleRate", 48000);
    codecSettings.insert("channels", 2);
    codecSettings.insert("bitRate", 256000);
    codecSettings.insert("bitDepth", 24);

    const auto exportParameters = makeExportParameters();

    info.SetOriginalFile("C:/audio/original.wav", "original.wav");
    info.SetExportFormatID("WAV");
    info.SetCodecSettings(codecSettings);
    info.SetExportParameters(exportParameters);
    info.IncrementImportedFileCount();

    EXPECT_TRUE(info.HasOriginalFile());
    EXPECT_EQ(info.GetOriginalFilePath(), "C:/audio/original.wav");
    EXPECT_EQ(info.GetOriginalFileName(), "original.wav");
    EXPECT_EQ(info.GetExportFormatID(), "WAV");
    EXPECT_EQ(info.GetCodecSettings().value("sampleRate").toInt(), 48000);
    EXPECT_EQ(info.GetCodecSettings().value("channels").toInt(), 2);
    EXPECT_EQ(info.GetCodecSettings().value("bitRate").toInt(), 256000);
    EXPECT_EQ(info.GetCodecSettings().value("bitDepth").toInt(), 24);
    EXPECT_EQ(info.GetExportParameters(), exportParameters);
}

TEST_P(OriginalFileInfoFormatCodecTests, RemembersFormatBitRateBitDepthAndChannelLayout)
{
    const auto testCase = GetParam();
    AudacityProject& project = freshProject();
    auto& info = OriginalFileInfo::Get(project);

    QVariantMap codecSettings;
    codecSettings.insert("sampleRate", testCase.sampleRate);
    codecSettings.insert("channels", testCase.channels);
    codecSettings.insert("bitRate", testCase.bitRate);
    codecSettings.insert("bitDepth", testCase.bitDepth);

    const QString filename = QString("original.") + testCase.extension;
    info.SetOriginalFile(QString("C:/audio/") + filename, filename);
    info.SetExportFormatID(QString(testCase.formatID));
    info.SetCodecSettings(codecSettings);
    info.SetExportParameters(makeExportParameters());
    info.IncrementImportedFileCount();

    EXPECT_TRUE(info.HasOriginalFile());
    EXPECT_EQ(info.GetOriginalFileName(), filename);
    EXPECT_EQ(info.GetExportFormatID(), QString(testCase.formatID));
    EXPECT_EQ(info.GetCodecSettings().value("sampleRate").toInt(), testCase.sampleRate);
    EXPECT_EQ(info.GetCodecSettings().value("channels").toInt(), testCase.channels);
    EXPECT_EQ(info.GetCodecSettings().value("bitRate").toInt(), testCase.bitRate);
    EXPECT_EQ(info.GetCodecSettings().value("bitDepth").toInt(), testCase.bitDepth);
    EXPECT_FALSE(info.GetExportParameters().empty());
}

INSTANTIATE_TEST_SUITE_P(
    OriginalFileInfoTests,
    OriginalFileInfoFormatCodecTests,
    ::testing::Values(
        FormatCodecCase { "wav", "WAV", 44100, 1, 16, 705600 },
        FormatCodecCase { "wav", "WAV", 48000, 2, 24, 2304000 },
        FormatCodecCase { "flac", "FLAC", 96000, 1, 24, 900000 },
        FormatCodecCase { "flac", "FLAC", 44100, 2, 16, 650000 },
        FormatCodecCase { "mp3", "MP3", 44100, 1, 0, 128000 },
        FormatCodecCase { "mp3", "MP3", 48000, 2, 0, 229000 },
        FormatCodecCase { "ogg", "OGG", 44100, 1, 0, 96000 },
        FormatCodecCase { "ogg", "OGG", 48000, 2, 0, 160000 }));

TEST(OriginalFileInfoTests, MultipleImportsKeepMetadataButDisableOverwriteOriginal)
{
    AudacityProject& project = freshProject();
    auto& info = OriginalFileInfo::Get(project);

    QVariantMap codecSettings;
    codecSettings.insert("sampleRate", 96000);
    codecSettings.insert("bitDepth", 24);

    info.SetOriginalFile("C:/audio/first.flac", "first.flac");
    info.SetExportFormatID("FLAC");
    info.SetCodecSettings(codecSettings);
    info.SetExportParameters(makeExportParameters());

    info.IncrementImportedFileCount();
    info.IncrementImportedFileCount();

    EXPECT_FALSE(info.HasOriginalFile());
    EXPECT_EQ(info.GetImportedFileCount(), 2);
    EXPECT_EQ(info.GetOriginalFilePath(), "C:/audio/first.flac");
    EXPECT_EQ(info.GetExportFormatID(), "FLAC");
    EXPECT_EQ(info.GetCodecSettings().value("sampleRate").toInt(), 96000);
}

TEST(OriginalFileInfoTests, ClearDropsRememberedOverwriteState)
{
    AudacityProject& project = freshProject();
    auto& info = OriginalFileInfo::Get(project);

    QVariantMap codecSettings;
    codecSettings.insert("sampleRate", 44100);
    codecSettings.insert("quality", 7);

    info.SetOriginalFile("C:/audio/original.ogg", "original.ogg");
    info.SetExportFormatID("OGG");
    info.SetCodecSettings(codecSettings);
    info.SetExportParameters(makeExportParameters());
    info.IncrementImportedFileCount();

    info.Clear();

    EXPECT_FALSE(info.HasOriginalFile());
    EXPECT_TRUE(info.GetOriginalFilePath().isEmpty());
    EXPECT_TRUE(info.GetOriginalFileName().isEmpty());
    EXPECT_TRUE(info.GetExportFormatID().isEmpty());
    EXPECT_TRUE(info.GetCodecSettings().isEmpty());
    EXPECT_TRUE(info.GetExportParameters().empty());
    EXPECT_EQ(info.GetImportedFileCount(), 0);
}

TEST(OriginalFileInfoTests, StateIsScopedPerProject)
{
    AudacityProject& firstProject = freshProject();
    AudacityProject& secondProject = freshProject();

    auto& firstInfo = OriginalFileInfo::Get(firstProject);
    auto& secondInfo = OriginalFileInfo::Get(secondProject);

    QVariantMap firstSettings;
    firstSettings.insert("bitRate", 192000);

    QVariantMap secondSettings;
    secondSettings.insert("sampleRate", 44100);
    secondSettings.insert("bitDepth", 16);

    firstInfo.SetOriginalFile("C:/audio/first.mp3", "first.mp3");
    firstInfo.SetExportFormatID("MP3");
    firstInfo.SetCodecSettings(firstSettings);
    firstInfo.IncrementImportedFileCount();

    secondInfo.SetOriginalFile("C:/audio/second.wav", "second.wav");
    secondInfo.SetExportFormatID("WAV");
    secondInfo.SetCodecSettings(secondSettings);
    secondInfo.IncrementImportedFileCount();

    EXPECT_EQ(firstInfo.GetOriginalFilePath(), "C:/audio/first.mp3");
    EXPECT_EQ(firstInfo.GetExportFormatID(), "MP3");
    EXPECT_EQ(firstInfo.GetCodecSettings().value("bitRate").toInt(), 192000);

    EXPECT_EQ(secondInfo.GetOriginalFilePath(), "C:/audio/second.wav");
    EXPECT_EQ(secondInfo.GetExportFormatID(), "WAV");
    EXPECT_EQ(secondInfo.GetCodecSettings().value("sampleRate").toInt(), 44100);
    EXPECT_EQ(secondInfo.GetCodecSettings().value("bitDepth").toInt(), 16);
}

TEST(OriginalFileInfoTests, UndoAndRedoRestoreTheImportCountAndTarget)
{
    au::au3::Au3ProjectAccessor projectAccessor(muse::modularity::globalCtx());
    ASSERT_TRUE(projectAccessor.open());
    const auto cleanup = qScopeGuard([&] { projectAccessor.close(); });
    auto& project = *reinterpret_cast<AudacityProject*>(projectAccessor.au3ProjectPtr());
    auto& undo = UndoManager::Get(project);
    auto& history = ProjectHistory::Get(project);
    auto& info = OriginalFileInfo::Get(project);
    undo.PushState({}, {});
    info.SetOriginalFile("first.wav", "first.wav");
    info.SetExportFormatID("WAV");
    info.SetExportParameters(makeExportParameters());
    info.IncrementImportedFileCount();
    undo.PushState({}, {});
    info.IncrementImportedFileCount();
    undo.PushState({}, {});
    EXPECT_FALSE(info.HasOriginalFile());

    const auto restore = [&](const UndoStackElem& state) {
        history.PopState(state.state, false);
    };
    ASSERT_TRUE(undo.UndoAvailable());
    undo.Undo(restore);
    EXPECT_TRUE(info.HasOriginalFile());
    EXPECT_EQ(info.GetOriginalFilePath(), "first.wav");
    EXPECT_EQ(info.GetExportParameters(), makeExportParameters());

    ASSERT_TRUE(undo.UndoAvailable());
    undo.Undo(restore);
    EXPECT_FALSE(info.HasOriginalFile());
    EXPECT_TRUE(info.GetOriginalFilePath().isEmpty());
    EXPECT_EQ(info.GetImportedFileCount(), 0);

    ASSERT_TRUE(undo.RedoAvailable());
    undo.Redo(restore);
    EXPECT_TRUE(info.HasOriginalFile());
    ASSERT_TRUE(undo.RedoAvailable());
    undo.Redo(restore);
    EXPECT_FALSE(info.HasOriginalFile());
    EXPECT_EQ(info.GetImportedFileCount(), 2);
}

TEST(OriginalFileInfoTests, ChecksTheSourceContentsAndRefreshesTheVersionSharedWithHistory)
{
    const QTemporaryDir dir;
    ASSERT_TRUE(dir.isValid());
    const auto path = dir.filePath("original.wav");
    QFile source(path);
    ASSERT_TRUE(source.open(QIODevice::WriteOnly));
    ASSERT_EQ(source.write("original"), 8);
    source.close();
#ifdef Q_OS_UNIX
    const auto originalPath = QFileInfo(source).filesystemFilePath();
    const auto permissions = std::filesystem::perms::owner_read | std::filesystem::perms::owner_write;
    std::filesystem::permissions(originalPath, permissions);
#endif

    OriginalFileInfo info;
    const auto version = OriginalFileInfo::ReadFileVersion(path);
    ASSERT_TRUE(version);
    info.SetOriginalFile(path, "original.wav", version);
    EXPECT_TRUE(info.OriginalFileUnchanged());

    ASSERT_TRUE(source.open(QIODevice::ReadWrite | QIODevice::Truncate));
    ASSERT_EQ(source.write("modified"), 8);
    ASSERT_TRUE(source.flush());
    ASSERT_TRUE(source.setFileTime(version->modified, QFileDevice::FileModificationTime));
    source.close();
    EXPECT_FALSE(info.OriginalFileUnchanged());

    QFile staged(dir.filePath("staged.wav"));
    ASSERT_TRUE(staged.open(QIODevice::WriteOnly));
    ASSERT_EQ(staged.write("exported"), 8);
    staged.close();
#ifdef Q_OS_UNIX
    std::filesystem::permissions(QFileInfo(staged).filesystemFilePath(),
                                 permissions | std::filesystem::perms::group_read | std::filesystem::perms::others_read);
#endif
    const auto conflict = info.ReplaceWithExport(staged.fileName());
    EXPECT_EQ(conflict.result, OriginalFileInfo::ReplaceResult::SourceChanged);
    EXPECT_TRUE(conflict.recoveryPath.isEmpty());
    ASSERT_TRUE(source.open(QIODevice::ReadOnly));
    EXPECT_EQ(source.readAll(), QByteArray("modified"));
    source.close();

    const auto changedVersion = OriginalFileInfo::ReadFileVersion(path);
    ASSERT_TRUE(changedVersion);
    auto mismatchedDigest = *changedVersion;
    mismatchedDigest.digest[0] ^= 1;
    info.SetOriginalFile(path, "original.wav", mismatchedDigest);
    EXPECT_FALSE(info.OriginalFileUnchanged());

    info.SetOriginalFile(path, "original.wav", changedVersion);
    const auto historical = info;
    const auto result = info.ReplaceWithExport(staged.fileName());
    ASSERT_EQ(result.result, OriginalFileInfo::ReplaceResult::Success);
    EXPECT_TRUE(result.recoveryPath.isEmpty());
    EXPECT_TRUE(historical.OriginalFileUnchanged());
#ifdef Q_OS_UNIX
    EXPECT_EQ(std::filesystem::status(originalPath).permissions(), permissions);
#endif
    ASSERT_TRUE(source.remove());
    EXPECT_FALSE(info.OriginalFileUnchanged());
}

#ifdef Q_OS_LINUX
TEST(OriginalFileInfoTests, PreservesNamedUserAccessOnReplacement)
{
    const QTemporaryDir dir;
    ASSERT_TRUE(dir.isValid());
    QFile source(dir.filePath("original.wav"));
    ASSERT_TRUE(source.open(QIODevice::WriteOnly));
    ASSERT_EQ(source.write("original"), 8);
    const auto excludedUser = getuid() == 65534 ? 65533u : 65534u;
    const struct {
        posix_acl_xattr_header header;
        posix_acl_xattr_entry entries[5];
    } acl {
        { qToLittleEndian<quint32>(POSIX_ACL_XATTR_VERSION) },
        {
            { qToLittleEndian<quint16>(ACL_USER_OBJ), qToLittleEndian<quint16>(6), qToLittleEndian<quint32>(ACL_UNDEFINED_ID) },
            { qToLittleEndian<quint16>(ACL_USER), 0, qToLittleEndian<quint32>(excludedUser) },
            { qToLittleEndian<quint16>(ACL_GROUP_OBJ), qToLittleEndian<quint16>(4), qToLittleEndian<quint32>(ACL_UNDEFINED_ID) },
            { qToLittleEndian<quint16>(ACL_MASK), qToLittleEndian<quint16>(4), qToLittleEndian<quint32>(ACL_UNDEFINED_ID) },
            { qToLittleEndian<quint16>(ACL_OTHER), qToLittleEndian<quint16>(4), qToLittleEndian<quint32>(ACL_UNDEFINED_ID) }
        }
    };
    constexpr auto attribute = "system.posix_acl_access";
    ASSERT_EQ(fsetxattr(source.handle(), attribute, &acl, sizeof(acl), 0), 0);
    source.close();

    QFile output(dir.filePath("staged.wav"));
    ASSERT_TRUE(output.open(QIODevice::WriteOnly));
    ASSERT_EQ(output.write("exported"), 8);
    output.close();

    OriginalFileInfo info;
    const auto version = OriginalFileInfo::ReadFileVersion(source.fileName());
    ASSERT_TRUE(version);
    info.SetOriginalFile(source.fileName(), "original.wav", version);
    const auto result = info.ReplaceWithExport(output.fileName());
    ASSERT_EQ(result.result, OriginalFileInfo::ReplaceResult::Success);
    ASSERT_TRUE(source.open(QIODevice::ReadOnly));
    EXPECT_EQ(source.readAll(), QByteArray("exported"));
    QByteArray preserved(sizeof(acl), Qt::Uninitialized);
    ASSERT_EQ(fgetxattr(source.handle(), attribute, preserved.data(), preserved.size()), sizeof(acl));
    EXPECT_EQ(preserved, QByteArray(reinterpret_cast<const char*>(&acl), sizeof(acl)));
}
#endif
