/**********************************************************************

Audacity: A Digital Audio Editor

OriginalFileInfo.cpp

**********************************************************************/

#include "OriginalFileInfo.h"

#include "au3-project/Project.h"

#include <memory>

namespace {
class OriginalFileInfoAttachment final : public ClientData::Base, public OriginalFileInfo
{
};

static const AudacityProject::AttachedObjects::RegisteredFactory key {
    [](AudacityProject&) {
        return std::make_shared<OriginalFileInfoAttachment>();
    }
};
}

OriginalFileInfo& OriginalFileInfo::Get(AudacityProject& project)
{
    return project.AttachedObjects::Get<OriginalFileInfoAttachment>(key);
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
