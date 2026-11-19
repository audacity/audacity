/**********************************************************************

Audacity: A Digital Audio Editor

OriginalFileInfo.cpp

**********************************************************************/

#include "OriginalFileInfo.h"

#include <QCryptographicHash>
#include <QFile>
#include <QFileInfo>

#include "au3-import-export/ExportPluginRegistry.h"
#include "au3-import-export/ExportUtils.h"
#include "au3-mixer/MixerOptions.h"
#include "au3-project/Project.h"
#include "au3-project-history/UndoManager.h"
#include "au3-wave-track/WaveTrack.h"

#include <algorithm>
#include <filesystem>
#include <iterator>
#include <memory>

#if defined(Q_OS_WIN)
#include <windows.h>
#include <aclapi.h>
#elif defined(Q_OS_LINUX) || defined(Q_OS_MACOS)
#include <sys/stat.h>
#include <unistd.h>
#if defined(Q_OS_LINUX)
#include <fcntl.h>
#include <linux/fs.h>
#include <sys/syscall.h>
#include <sys/xattr.h>
#include <map>
#include <string>
#elif defined(Q_OS_MACOS)
#include <copyfile.h>
#include <stdio.h>
#include <sys/acl.h>
#endif
#endif

namespace {
#ifdef Q_OS_WIN
bool hasSameOwnerAndGroup(const std::filesystem::path& original, const std::filesystem::path& staged)
{
    PSID sourceOwner = nullptr, sourceGroup = nullptr, outputOwner = nullptr, outputGroup = nullptr;
    PSECURITY_DESCRIPTOR sourceDescriptor = nullptr, outputDescriptor = nullptr;
    constexpr auto information = OWNER_SECURITY_INFORMATION | GROUP_SECURITY_INFORMATION;
    const auto sourceStatus = GetNamedSecurityInfoW(original.c_str(), SE_FILE_OBJECT, information,
                                                    &sourceOwner, &sourceGroup, nullptr, nullptr, &sourceDescriptor);
    const auto outputStatus = GetNamedSecurityInfoW(staged.c_str(), SE_FILE_OBJECT, information,
                                                    &outputOwner, &outputGroup, nullptr, nullptr, &outputDescriptor);
    const std::unique_ptr<void, decltype(& LocalFree)> sourceSecurity(sourceDescriptor, LocalFree);
    const std::unique_ptr<void, decltype(& LocalFree)> outputSecurity(outputDescriptor, LocalFree);
    return sourceStatus == ERROR_SUCCESS && outputStatus == ERROR_SUCCESS
           && sourceOwner && sourceGroup && outputOwner && outputGroup
           && EqualSid(sourceOwner, outputOwner) && EqualSid(sourceGroup, outputGroup);
}

#elif defined(Q_OS_LINUX) || defined(Q_OS_MACOS)
#ifdef Q_OS_LINUX
using AccessAttributes = std::map<std::string, std::string>;

std::optional<AccessAttributes> readAccessAttributes(int fd)
{
    const auto namesSize = flistxattr(fd, nullptr, 0);
    if (namesSize < 0) {
        return {};
    }
    AccessAttributes attributes;
    if (namesSize == 0) {
        return attributes;
    }
    std::string names(namesSize, '\0');
    if (flistxattr(fd, names.data(), namesSize) != namesSize) {
        return {};
    }
    for (size_t offset = 0; offset < names.size();) {
        const std::string name(names.c_str() + offset);
        offset += name.size() + 1;
        if (name.rfind("system.", 0) != 0 && name.rfind("security.", 0) != 0) {
            continue;
        }
        const auto valueSize = fgetxattr(fd, name.c_str(), nullptr, 0);
        if (valueSize < 0) {
            return {};
        }
        std::string value(valueSize, '\0');
        if (fgetxattr(fd, name.c_str(), value.data(), valueSize) != valueSize) {
            return {};
        }
        attributes.emplace(name, std::move(value));
    }
    return attributes;
}

#endif

bool copyAccessControls(int source, int output)
{
#ifdef Q_OS_MACOS
    struct stat original {}, staged {};
    return fstat(source, &original) == 0
           && fcopyfile(source, output, nullptr, COPYFILE_SECURITY) == 0
           && fstat(output, &staged) == 0
           && original.st_uid == staged.st_uid && original.st_gid == staged.st_gid
           && (original.st_mode & 07777) == (staged.st_mode & 07777);
#else
    struct stat original {}, staged {};
    if (fstat(source, &original) != 0 || fstat(output, &staged) != 0) {
        return false;
    }
    if ((original.st_uid != staged.st_uid || original.st_gid != staged.st_gid)
        && fchown(output, original.st_uid, original.st_gid) != 0) {
        return false;
    }
    const auto originalAttributes = readAccessAttributes(source);
    const auto outputAttributes = readAccessAttributes(output);
    if (!originalAttributes || !outputAttributes) {
        return false;
    }
    for (const auto& entry : *outputAttributes) {
        const auto& name = entry.first;
        if (originalAttributes->find(name) == originalAttributes->end() && fremovexattr(output, name.c_str()) != 0) {
            return false;
        }
    }
    for (const auto& [name, value] : *originalAttributes) {
        const auto existing = outputAttributes->find(name);
        if ((existing == outputAttributes->end() || existing->second != value)
            && fsetxattr(output, name.c_str(), value.data(), value.size(), 0) != 0) {
            return false;
        }
    }
    return fchmod(output, original.st_mode & 07777) == 0;
#endif
}

bool makeReplacementPrivate(int output)
{
#ifdef Q_OS_MACOS
    const auto acl = acl_init(0);
    if (!acl) {
        return false;
    }
    const bool cleared = acl_set_fd(output, acl) == 0;
    acl_free(acl);
    if (!cleared) {
        return false;
    }
#endif
    return fchmod(output, S_IRUSR | S_IWUSR) == 0;
}

bool exchangeFiles(const std::filesystem::path& original, const std::filesystem::path& staged)
{
#ifdef Q_OS_MACOS
    return renamex_np(original.c_str(), staged.c_str(), RENAME_SWAP) == 0;
#elif defined(SYS_renameat2)
    return syscall(SYS_renameat2, AT_FDCWD, original.c_str(), AT_FDCWD, staged.c_str(), RENAME_EXCHANGE) == 0;
#else
    return false;
#endif
}

#endif

class OriginalFileInfoAttachment final : public ClientData::Base, public OriginalFileInfo
{
};

static const AudacityProject::AttachedObjects::RegisteredFactory key {
    [](AudacityProject&) {
        return std::make_shared<OriginalFileInfoAttachment>();
    }
};

struct OriginalFileInfoRestorer final : UndoStateExtension {
    explicit OriginalFileInfoRestorer(AudacityProject& project)
        : info(OriginalFileInfo::Get(project))
    {
    }

    void RestoreUndoRedoState(AudacityProject& project) override
    {
        OriginalFileInfo::Get(project) = info;
    }

    const OriginalFileInfo info;
};

const UndoRedoExtensionRegistry::Entry<OriginalFileInfoRestorer> originalFileInfoUndo {
    [](AudacityProject& project) {
        return std::make_shared<OriginalFileInfoRestorer>(project);
    }
};
}

OriginalFileInfo& OriginalFileInfo::Get(AudacityProject& project)
{
    return project.AttachedObjects::Get<OriginalFileInfoAttachment>(key);
}

void OriginalFileInfo::SetOriginalFile(const QString& filePath, const QString& displayName, std::optional<FileVersion> version)
{
    mOriginalFilePath = version ? QFileInfo(filePath).absoluteFilePath() : filePath;
    mOriginalFileName = displayName;
    mFileVersion = std::make_shared<std::optional<FileVersion> >(std::move(version));
}

std::optional<OriginalFileInfo::FileVersion> OriginalFileInfo::ReadFileStamp(const QString& path)
{
    const QFileInfo info(path);
    if (info.isSymLink() || !info.isFile()) {
        return {};
    }
    return FileVersion {
        info.canonicalFilePath(), info.size(), info.lastModified(),
        info.metadataChangeTime(), info.birthTime(), {}
    };
}

std::optional<OriginalFileInfo::FileVersion> OriginalFileInfo::ReadFileVersion(const QString& path,
                                                                               const std::optional<FileVersion>& expectedStamp)
{
    auto version = ReadFileStamp(path);
    if (!version || (expectedStamp && version != expectedStamp)) {
        return {};
    }

    QFile file(path);
    QCryptographicHash hash(QCryptographicHash::Sha256);
    if (!file.open(QIODevice::ReadOnly) || !hash.addData(&file) || ReadFileStamp(path) != version) {
        return {};
    }
    version->digest = hash.result();
    return version;
}

bool OriginalFileInfo::OriginalFileUnchanged() const
{
    if (!mFileVersion || !*mFileVersion) {
        return false;
    }
    auto expectedStamp = mFileVersion->value();
    expectedStamp.digest.clear();
    return ReadFileVersion(mOriginalFilePath, expectedStamp) == *mFileVersion;
}

OriginalFileInfo::Replacement OriginalFileInfo::ReplaceWithExport(const QString& exportPath) const
{
    if (!OriginalFileUnchanged()) {
        return { ReplaceResult::SourceChanged, {} };
    }
    const auto original = QFileInfo(mOriginalFilePath).filesystemFilePath();
    const auto exported = QFileInfo(exportPath).filesystemFilePath();
    auto expectedStamp = mFileVersion->value();
    expectedStamp.digest.clear();

#if defined(Q_OS_LINUX) || defined(Q_OS_MACOS)
    QFile originalFile(mOriginalFilePath);
    QFile outputFile(exportPath);
    if (!originalFile.open(QIODevice::ReadOnly) || !outputFile.open(QIODevice::ReadOnly)
        || !copyAccessControls(originalFile.handle(), outputFile.handle())
        || !makeReplacementPrivate(outputFile.handle())) {
        return { ReplaceResult::Error, {} };
    }
#elif defined(Q_OS_WIN)
    if (!hasSameOwnerAndGroup(original, exported)) {
        return { ReplaceResult::Error, {} };
    }
#else
    return { ReplaceResult::Error, {} };
#endif

    const auto exportedVersion = ReadFileVersion(exportPath);
    if (!exportedVersion) {
        return { ReplaceResult::Error, {} };
    }
    if (ReadFileStamp(mOriginalFilePath) != expectedStamp) {
        return { ReplaceResult::SourceChanged, {} };
    }

    QString displacedPath;
#if defined(Q_OS_WIN)
    displacedPath = exportPath + ".previous";
    const auto backup = QFileInfo(displacedPath).filesystemFilePath();
    if (!ReplaceFileW(original.c_str(), exported.c_str(), backup.c_str(), 0, nullptr, nullptr)) {
        return { ReplaceResult::Error, QFileInfo::exists(displacedPath) ? displacedPath : QString {} };
    }
#elif defined(Q_OS_LINUX) || defined(Q_OS_MACOS)
    if (!exchangeFiles(original, exported)) {
        return { ReplaceResult::Error, {} };
    }
    displacedPath = exportPath;
#endif

    const auto displaced = ReadFileVersion(displacedPath);
    const auto& previous = mFileVersion->value();
    if (!displaced || displaced->size != previous.size || displaced->modified != previous.modified
        || displaced->digest != previous.digest) {
        mFileVersion->reset();
        return { ReplaceResult::SourceChanged, displacedPath };
    }
#if defined(Q_OS_LINUX) || defined(Q_OS_MACOS)
    // The live replacement only becomes accessible to others after applying the
    // access controls from the exact file displaced by the exchange.
    QFile displacedFile(displacedPath);
    if (!displacedFile.open(QIODevice::ReadOnly) || !copyAccessControls(displacedFile.handle(), outputFile.handle())) {
        mFileVersion->reset();
        return { ReplaceResult::Error, displacedPath };
    }
#endif
    RefreshOriginalFileVersion(*exportedVersion);
    return { ReplaceResult::Success, {} };
}

void OriginalFileInfo::SetSourceChannelTracks(std::vector<int64_t> trackIds)
{
    mSourceChannelTracks = std::move(trackIds);
}

std::unique_ptr<MixerOptions::Downmix> OriginalFileInfo::MakeChannelMapping(const TrackList& tracks) const
{
    std::vector<size_t> outputChannels;
    for (const auto* track : ExportUtils::FindExportWaveTracks(tracks, false)) {
        const auto source = std::find(mSourceChannelTracks.begin(), mSourceChannelTracks.end(), track->GetId().raw());
        if (source == mSourceChannelTracks.end() || track->NChannels() != 1) {
            return nullptr;
        }
        outputChannels.push_back(std::distance(mSourceChannelTracks.begin(), source));
    }
    const auto channelCount = outputChannels.size();
    if (channelCount == 0 || channelCount != mSourceChannelTracks.size()) {
        return nullptr;
    }
    const auto channels = static_cast<unsigned>(channelCount);
    auto mapping = std::make_unique<MixerOptions::Downmix>(channels, channels);
    for (size_t input = 0; input < channelCount; ++input) {
        for (size_t output = 0; output < channelCount; ++output) {
            mapping->mMap[input][output] = outputChannels[input] == output;
        }
    }
    return mapping;
}

void OriginalFileInfo::RefreshOriginalFileVersion(const FileVersion& exportedVersion) const
{
    auto version = ReadFileStamp(mOriginalFilePath);
    if (version && version->size == exportedVersion.size) {
        version->digest = exportedVersion.digest;
        *mFileVersion = std::move(version);
    } else {
        mFileVersion->reset();
    }
}

const QString& OriginalFileInfo::GetOriginalFilePath() const
{
    return mOriginalFilePath;
}

const QString& OriginalFileInfo::GetOriginalFileName() const
{
    return mOriginalFileName;
}

void OriginalFileInfo::SetExportFormatID(const QString& formatID)
{
    mExportFormatID = formatID;
}

const QString& OriginalFileInfo::GetExportFormatID() const
{
    return mExportFormatID;
}

void OriginalFileInfo::SetCodecSettings(const QVariantMap& settings)
{
    mCodecSettings = settings;
}

const QVariantMap& OriginalFileInfo::GetCodecSettings() const
{
    return mCodecSettings;
}

void OriginalFileInfo::SetExportParameters(const au::importexport::ExportParameters& parameters)
{
    mExportParameters = parameters;
}

const au::importexport::ExportParameters& OriginalFileInfo::GetExportParameters() const
{
    return mExportParameters;
}

void OriginalFileInfo::IncrementImportedFileCount()
{
    ++mImportedFileCount;
}

int OriginalFileInfo::GetImportedFileCount() const
{
    return mImportedFileCount;
}

void OriginalFileInfo::Clear()
{
    mOriginalFilePath.clear();
    mOriginalFileName.clear();
    mExportFormatID.clear();
    mCodecSettings.clear();
    mExportParameters.clear();
    mSourceChannelTracks.clear();
    mFileVersion.reset();
    mImportedFileCount = 0;
}

bool OriginalFileInfo::HasOriginalFile() const
{
    return !mOriginalFilePath.isEmpty() && mImportedFileCount == 1;
}

bool OriginalFileInfo::CanOverwriteOriginalFile(const TrackList& tracks) const
{
    if (!HasOriginalFile() || mExportFormatID.isEmpty() || !mFileVersion || !*mFileVersion) {
        return false;
    }

    const auto audibleTracks = ExportUtils::FindExportWaveTracks(tracks, false);
    if (std::none_of(audibleTracks.begin(), audibleTracks.end(), [this](const WaveTrack* track) {
        return !track->IsEmpty()
               && std::find(mSourceChannelTracks.begin(), mSourceChannelTracks.end(), track->GetId().raw()) != mSourceChannelTracks.end();
    })) {
        return false;
    }

    const QFileInfo file(mOriginalFilePath);
    const auto extension = file.suffix().toLower();
    if (extension == "aup" || extension == "aup3" || extension == "aup4") {
        return false;
    }

    if ((file.exists() && (!file.isFile() || !file.isWritable())) || !QFileInfo(file.absolutePath()).isWritable()) {
        return false;
    }

    return std::get<0>(ExportPluginRegistry::Get().FindFormat(wxString(mExportFormatID.toStdWString()))) != nullptr;
}
