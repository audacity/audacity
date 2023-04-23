/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportPlugin.cpp

  Dominic Mazzoni

**********************************************************************/

#include "ExportPlugin.h"

ExportException::ExportException(const wxString& msg)
   : mMessage(msg)
{
}

const wxString& ExportException::What() const noexcept
{
   return mMessage;
}

ExportErrorException::ExportErrorException(const wxString& message, const wxString& helpPage)
   : ExportException(message), mHelpPage(helpPage)
{
}

const wxString& ExportErrorException::GetHelpPage() const noexcept
{
   return mHelpPage;
}

ExportPluginDelegate::~ExportPluginDelegate() = default;

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
