/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportPlugin.cpp

  Dominic Mazzoni

**********************************************************************/

#include "ExportPlugin.h"
#include "wxFileNameWrapper.h"

ExportException::ExportException(const wxString& msg)
    : mMessage(msg)
{
}

const wxString& ExportException::What() const noexcept
{
    return mMessage;
}

ExportDiskFullError::ExportDiskFullError(const wxFileNameWrapper& filename)
    : mFileName(filename)
{
}

const wxFileNameWrapper& ExportDiskFullError::GetFileName() const noexcept
{
    return mFileName;
}

// Untranslated ErrorCodes like "MP3:1882" are used since we don't yet have
// a good user facing error message.  They allow us to
// distinguish where the error occurred, and we can update the landing
// page as we learn more about when (if ever) these errors actually happen.
// The number happens to at one time have been a line number, but all
// we need from them is that they be distinct.
ExportErrorException::ExportErrorException(const wxString& code)
    : mMessage(XO("Unable to export.\nError %s").Format(code))
    , mHelpPageId("Error:_Unable_to_export")
{
}

ExportErrorException::ExportErrorException(TranslatableString message, const wxString& helpPage)
    : mMessage(std::move(message))
    , mHelpPageId(helpPage)
{
}

const wxString& ExportErrorException::GetHelpPageId() const noexcept
{
    return mHelpPageId;
}

const TranslatableString& ExportErrorException::GetMessage() const noexcept
{
    return mMessage;
}

ExportProcessorDelegate::~ExportProcessorDelegate() = default;

ExportProcessor::~ExportProcessor() = default;

ExportPlugin::ExportPlugin() = default;
ExportPlugin::~ExportPlugin() = default;

std::vector<std::string> ExportPlugin::GetMimeTypes(int formatIndex) const
{
    return {};
}

bool ExportPlugin::ParseConfig(int, const rapidjson::Value&, ExportProcessor::Parameters&) const
{
    return false;
}

bool ExportPlugin::CheckFileName(wxFileName&, int) const
{
    return true;
}
