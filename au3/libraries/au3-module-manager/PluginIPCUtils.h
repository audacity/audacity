/**********************************************************************

  Audacity: A Digital Audio Editor

  @file PluginIPCUtils.h

  @author Vitaly Sverchinsky

  @brief Provides a set of internal commonly used functions/structures

  Part of lib-module-manager library.

**********************************************************************/

#pragma once

#include <vector>
#include <wx/string.h>
#include "XMLTagHandler.h"

class IPCChannel;
class PluginDescriptor;
class XMLWriter;

namespace detail {
///Helper function that extracts plugin provider id and plugin path from the item string
///return true if item string was successfully parsed
bool ParseRequestString(const wxString& req, wxString& providerId, wxString& pluginPath);

///Helper function that builds item string from given plugin provider id and plugin path strings
///return Item string that can be passed as argument to wxConnection::Request
wxString MakeRequestString(const wxString& providerId, const wxString& pluginPath);

///Writes the length of the string and string bytes into the channel.
///Message can be then extracted with InputMessageReader
void PutMessage(IPCChannel& channel, const wxString& value);

///Stores consumed bytes into buffer,
///so that individual messages can be extracted later.
class InputMessageReader
{
    std::vector<char> mBuffer;
public:
    ///fills internal buffer
    void ConsumeBytes(const void* bytes, size_t length);

    ///@returns true if buffer contains at least one complete message
    bool CanPop() const noexcept;
    ///extracts message from the buffer, use CanPop() to check if message
    ///is present first
    wxString Pop();
};

///Host uses this structure to build a response and store it
///into xml form before sending to the other side. Main application
///uses this structure to restore response from the xml document received
///as a message
class PluginValidationResult final : public XMLTagHandler
{
    std::vector<PluginDescriptor> mDescriptors;
    wxString mErrorMessage;
    bool mHasError{ false };
public:

    ///Result is valid if error flag was not set and
    ///contains at least one plugin descriptor.
    bool IsValid() const noexcept;

    bool HasError() const noexcept;
    const wxString& GetErrorMessage() const noexcept;

    void Add(PluginDescriptor&& desc);
    void SetError(const wxString& msg);

    const std::vector<PluginDescriptor>& GetDescriptors() const noexcept;

    bool HandleXMLTag(const std::string_view& tag, const AttributesList& attrs) override;
    void HandleXMLEndTag(const std::string_view&) override;
    XMLTagHandler* HandleXMLChild(const std::string_view& tag) override;

    void WriteXML(XMLWriter& writer) const;
};
}
