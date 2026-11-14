/**********************************************************************

Audacity: A Digital Audio Editor

ImportPlugin.cpp

Paul Licameli split from Import.cpp

**********************************************************************/

#include "ImportPlugin.h"

#include <wx/filename.h>

ImportPlugin::ImportPlugin(FileExtensions supportedExtensions)
    : mExtensions(std::move(supportedExtensions))
{
}

ImportPlugin::~ImportPlugin() = default;

FileExtensions ImportPlugin::GetSupportedExtensions()
{
    return mExtensions;
}

bool ImportPlugin::SupportsExtension(const FileExtension& extension)
{
    // Case-insensitive check if extension is supported
    return mExtensions.Index(extension, false) != wxNOT_FOUND;
}

TranslatableString ImportPlugin::FailureHint() const
{
    return {};
}

ImportFileHandle::~ImportFileHandle() = default;

ImportFileHandleEx::ImportFileHandleEx(const FilePath& filename)
    :  mFilename(filename)
{
}

FilePath ImportFileHandleEx::GetFilename() const
{
    return mFilename;
}

void ImportFileHandleEx::Cancel()
{
    if (!mStopped) {
        mCancelled = true;
    }
}

void ImportFileHandleEx::Stop()
{
    if (!mCancelled) {
        mStopped = true;
    }
}

void ImportFileHandleEx::BeginImport()
{
    mCancelled = false;
    mStopped = false;
}

bool ImportFileHandleEx::IsCancelled() const noexcept
{
    return mCancelled;
}

bool ImportFileHandleEx::IsStopped() const noexcept
{
    return mStopped;
}

TranslatableString ImportFileHandle::GetErrorMessage() const
{
    return {};
}
