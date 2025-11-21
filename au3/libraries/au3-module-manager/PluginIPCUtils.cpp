/**********************************************************************

  Audacity: A Digital Audio Editor

  @file PluginIPCUtils.cpp

  @author Vitaly Sverchinsky

  Part of lib-module-manager library.

**********************************************************************/

#include "PluginIPCUtils.h"

#include <cstring>

#include "PluginDescriptor.h"

#include "IPCChannel.h"
#include "XMLWriter.h"

using namespace detail;

namespace {
using HeaderBlock = size_t;
constexpr auto HeaderBlockSize = sizeof(HeaderBlock);

constexpr auto NodePlugin = "Plugin";
constexpr auto NodeError = "Error";
constexpr auto AttrErrorMessage = "msg";
}

bool detail::ParseRequestString(const wxString& req, wxString& providerId, wxString& pluginPath)
{
    auto strings = wxSplit(req, ';');
    if (strings.size() == 2) {
        providerId = strings[0];
        pluginPath = strings[1];
        return true;
    }
    return false;
}

wxString detail::MakeRequestString(const wxString& providerId, const wxString& pluginPath)
{
    return wxJoin(wxArrayStringEx { providerId, pluginPath }, ';');
}

void detail::PutMessage(IPCChannel& channel, const wxString& value)
{
    auto utf8 = value.ToUTF8();
    const HeaderBlock length = utf8.length();
    channel.Send(&length, HeaderBlockSize);
    if (length > 0) {
        channel.Send(utf8.data(), length);
    }
}

void InputMessageReader::ConsumeBytes(const void* bytes, size_t length)
{
    auto from = mBuffer.size();
    mBuffer.resize(from + length);
    std::memcpy(&mBuffer[from], bytes, length);
}

bool InputMessageReader::CanPop() const noexcept
{
    if (mBuffer.size() >= HeaderBlockSize) {
        auto expectedSize = *reinterpret_cast<const HeaderBlock*>(mBuffer.data());
        return mBuffer.size() >= expectedSize + HeaderBlockSize;
    }
    return false;
}

wxString InputMessageReader::Pop()
{
    wxString message;
    const auto length = *reinterpret_cast<const HeaderBlock*>(mBuffer.data());
    assert(mBuffer.size() >= length + HeaderBlockSize);
    if (length > 0) {
        message = wxString::FromUTF8(mBuffer.data() + HeaderBlockSize, length);
    }
    mBuffer.erase(mBuffer.begin(), mBuffer.begin() + HeaderBlockSize + length);
    return message;
}

bool PluginValidationResult::IsValid() const noexcept
{
    return !mHasError && !mDescriptors.empty();
}

bool PluginValidationResult::HasError() const noexcept
{
    return mHasError;
}

const wxString& PluginValidationResult::GetErrorMessage() const noexcept
{
    return mErrorMessage;
}

void PluginValidationResult::Add(PluginDescriptor&& desc)
{
    mDescriptors.push_back(std::move(desc));
}

void PluginValidationResult::SetError(const wxString& msg)
{
    mHasError = true;
    mErrorMessage = msg;
}

const std::vector<PluginDescriptor>& PluginValidationResult::GetDescriptors() const noexcept
{
    return mDescriptors;
}

bool PluginValidationResult::HandleXMLTag(const std::string_view& tag, const AttributesList& attrs)
{
    if (tag == NodeError) {
        mHasError = true;
        for (auto& p : attrs) {
            auto key = wxString(p.first.data(), p.first.length());
            auto& attr = p.second;
            if (key == AttrErrorMessage) {
                mErrorMessage = attr.ToWString();
            }
        }
    }
    return true;
}

void PluginValidationResult::HandleXMLEndTag(const std::string_view&) { }

XMLTagHandler* PluginValidationResult::HandleXMLChild(const std::string_view& tag)
{
    if (tag == PluginDescriptor::XMLNodeName) {
        mDescriptors.resize(mDescriptors.size() + 1);
        return &mDescriptors.back();
    }
    return nullptr;
}

void PluginValidationResult::WriteXML(XMLWriter& writer) const
{
    if (mHasError) {
        writer.StartTag(NodeError);
        writer.WriteAttr(AttrErrorMessage, mErrorMessage);
        writer.EndTag(NodeError);
    }
    if (!mDescriptors.empty()) {
        writer.StartTag(NodePlugin);
        for (auto& desc : mDescriptors) {
            desc.WriteXML(writer);
        }
        writer.EndTag(NodePlugin);
    }
}
