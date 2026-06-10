/**********************************************************************

Audacity: A Digital Audio Editor

OriginalFileInfo.cpp

**********************************************************************/

#include "OriginalFileInfo.h"
#include <map>
#include <memory>

namespace {
    // Simple map-based storage for OriginalFileInfo per project (identified by pointer)
    // Note: Uses pointer address as key - this is safe as long as projects exist
    // This is session-scoped and cleared when the process exits.
    std::map<void*, std::shared_ptr<OriginalFileInfo>> gOriginalFileInfoMap;
}

OriginalFileInfo& OriginalFileInfo::Get(AudacityProject& project)
{
    void* projectPtr = &project;
    auto it = gOriginalFileInfoMap.find(projectPtr);
    if (it == gOriginalFileInfoMap.end()) {
        auto info = std::make_shared<OriginalFileInfo>(project);
        gOriginalFileInfoMap[projectPtr] = info;
        return *info;
    }
    return *it->second;
}

OriginalFileInfo::OriginalFileInfo(AudacityProject& project)
    : mProject(project)
{
}

OriginalFileInfo::~OriginalFileInfo()
{
    // The static map owns these instances. Do not erase from it here: during
    // static destruction the map is already erasing its nodes.
}

void OriginalFileInfo::SetOriginalFile(const QString& filePath, const QString& displayName)
{
    mOriginalFilePath = filePath;
    mOriginalFileName = displayName;
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
    mImportedFileCount = 0;
}

bool OriginalFileInfo::HasOriginalFile() const
{
    return !mOriginalFilePath.isEmpty() && mImportedFileCount == 1;
}
