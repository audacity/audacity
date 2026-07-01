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

#include <QString>
#include <QVariantMap>

#include "types/exporttypes.h"

class AudacityProject;

/// Stores metadata about the original imported file and its export settings
/// This is session-scoped data (not saved to project file) and cleared when
/// the project closes or a new project is created.
/// This is NOT part of the project file; it lives purely in the app state during a session.
class OriginalFileInfo
{
public:
    /// Get or create the OriginalFileInfo for a project
    static OriginalFileInfo& Get(AudacityProject& project);

    OriginalFileInfo() = default;
    OriginalFileInfo(const OriginalFileInfo&) = delete;
    OriginalFileInfo& operator=(const OriginalFileInfo&) = delete;
    ~OriginalFileInfo() = default;

    /// Set information about the originally imported file
    /// Called when the first file is imported into the project
    void SetOriginalFile(const QString& filePath, const QString& displayName);

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

private:
    QString mOriginalFilePath;
    QString mOriginalFileName;
    QString mExportFormatID;
    QVariantMap mCodecSettings;
    au::importexport::ExportParameters mExportParameters;
    int mImportedFileCount{ 0 };
};

#endif
