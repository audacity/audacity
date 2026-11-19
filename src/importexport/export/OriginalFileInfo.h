/**********************************************************************

Audacity: A Digital Audio Editor

OriginalFileInfo.h

Stores information about the original file that was imported,
including the file path and export codec/format parameters.
This allows the "Overwrite Original" feature to re-export using
the original file's codec and format settings.

**********************************************************************/

#ifndef __AUDACITY_ORIGINAL_FILE_INFO__
#define __AUDACITY_ORIGINAL_FILE_INFO__

#include <QByteArray>
#include <QDateTime>
#include <QString>
#include <QVariantMap>

#include <cstdint>
#include <memory>
#include <optional>
#include <vector>

#include "types/exporttypes.h"

class AudacityProject;
class TrackList;
namespace MixerOptions {
class Downmix;
}

/// Stores metadata about the original imported file and its export settings
/// This is session-scoped data (not saved to project file) and cleared when
/// the project closes or a new project is created.
/// This is NOT part of the project file; it lives purely in the app state during a session.
class OriginalFileInfo
{
public:
    struct FileVersion {
        QString canonicalPath;
        qint64 size;
        QDateTime modified;
        QDateTime metadataChanged;
        QDateTime created;
        QByteArray digest;

        bool operator==(const FileVersion&) const = default;
    };

    enum class ReplaceResult {
        Success,
        SourceChanged,
        Error
    };

    struct Replacement {
        ReplaceResult result;
        QString recoveryPath;
    };

    /// Get or create the OriginalFileInfo for a project
    static OriginalFileInfo& Get(AudacityProject& project);

    OriginalFileInfo() = default;
    ~OriginalFileInfo() = default;

    /// Set information about the originally imported file
    /// Called when the first file is imported into the project
    void SetOriginalFile(const QString& filePath, const QString& displayName, std::optional<FileVersion> version = {});

    static std::optional<FileVersion> ReadFileStamp(const QString& path);
    static std::optional<FileVersion> ReadFileVersion(const QString& path, const std::optional<FileVersion>& expectedStamp = {});
    bool OriginalFileUnchanged() const;
    Replacement ReplaceWithExport(const QString& exportPath) const;

    void SetSourceChannelTracks(std::vector<int64_t> trackIds);
    std::unique_ptr<MixerOptions::Downmix> MakeChannelMapping(const TrackList& tracks) const;

    /// Get the path to the original file
    const QString& GetOriginalFilePath() const;

    /// Get the display name of the original file
    const QString& GetOriginalFileName() const;

    /// Set the export format ID (e.g., "WAV", "MP3")
    void SetExportFormatID(const QString& formatID);

    /// Get the export format ID
    const QString& GetExportFormatID() const;

    /// Set codec-specific properties read from the source file.
    /// Examples include sampleRate, bitRate, bitDepth, quality, or any
    /// format-specific export option needed to reproduce the original file.
    void SetCodecSettings(const QVariantMap& settings);

    /// Get codec-specific properties read from the source file.
    const QVariantMap& GetCodecSettings() const;

    /// Set the exact export option values to use when overwriting.
    void SetExportParameters(const au::importexport::ExportParameters& parameters);

    /// Get the exact export option values to use when overwriting.
    const au::importexport::ExportParameters& GetExportParameters() const;

    /// Increment the count of imported files
    /// If more than 1 file has been imported, the "Overwrite Original" menu
    /// entry should be disabled/hidden
    void IncrementImportedFileCount();

    /// Get the count of files that have been imported into this project
    int GetImportedFileCount() const;

    /// Clear all stored information
    void Clear();

    /// Check if we have valid original file information
    bool HasOriginalFile() const;
    bool CanOverwriteOriginalFile(const TrackList& tracks) const;

private:
    void RefreshOriginalFileVersion(const FileVersion& exportedVersion) const;

    QString mOriginalFilePath;
    QString mOriginalFileName;
    QString mExportFormatID;
    QVariantMap mCodecSettings;
    au::importexport::ExportParameters mExportParameters;
    std::vector<int64_t> mSourceChannelTracks;
    // The on-disk version is shared by undo states describing the same import.
    std::shared_ptr<std::optional<FileVersion> > mFileVersion;
    int mImportedFileCount{ 0 };
};

#endif
