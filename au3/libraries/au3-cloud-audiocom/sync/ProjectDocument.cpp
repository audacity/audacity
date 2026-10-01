/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ProjectDocument.cpp

**********************************************************************/
#include "ProjectDocument.h"

#include <algorithm>
#include <cstring>
#include <memory>

#include "au3-project-file-io/ProjectSerializer.h"
#include "au3-utility/BufferedStreamReader.h"
#include "au3-utility/MemoryX.h"
#include "au3-xml/XMLTagHandler.h"

namespace audacity::cloud::audiocom::sync {
namespace {
class MemoryStreamReader final : public BufferedStreamReader
{
public:
    MemoryStreamReader(const uint8_t* data, size_t size)
        : mData(data), mSize(size)
    {
    }

protected:
    bool HasMoreData() const override
    {
        return mOffset < mSize;
    }

    size_t ReadData(void* buffer, size_t maxBytes) override
    {
        const auto bytes = std::min(maxBytes, mSize - mOffset);
        std::memcpy(buffer, mData + mOffset, bytes);
        mOffset += bytes;
        return bytes;
    }

private:
    const uint8_t* const mData;
    const size_t mSize;
    size_t mOffset { 0 };
};

//! Builds one element; children get handlers of their own
class ElementBuilder final : public XMLTagHandler
{
public:
    explicit ElementBuilder(DocumentElement& element)
        : mElement(element)
    {
    }

    bool HandleXMLTag(const std::string_view& tag, const AttributesList& attrs) override
    {
        mElement.name = tag;
        for (const auto& [name, value] : attrs) {
            mElement.attributes.emplace_back(std::string(name), ToDocumentValue(value));
        }
        return true;
    }

    XMLTagHandler* HandleXMLChild(const std::string_view&) override
    {
        // The previous child is complete by now, so reallocating is fine
        mElement.children.emplace_back();
        mChildBuilder = std::make_unique<ElementBuilder>(mElement.children.back());
        return mChildBuilder.get();
    }

    void HandleXMLContent(const std::string_view& content) override
    {
        mElement.content += content;
    }

    void HandleXMLBlob(const std::string_view& name, const void* data, size_t len) override
    {
        const auto bytes = static_cast<const uint8_t*>(data);
        mElement.blobs.emplace_back(std::string(name), std::vector<uint8_t>(bytes, bytes + len));
    }

private:
    static DocumentValue ToDocumentValue(const XMLAttributeValueView& value)
    {
        switch (value.GetType()) {
        case XMLAttributeValueView::Type::SignedInteger: {
            long long v {};
            value.TryGet(v);
            return v;
        }
        case XMLAttributeValueView::Type::UnsignedInteger: {
            unsigned long long v {};
            value.TryGet(v);
            return v;
        }
        case XMLAttributeValueView::Type::Float: {
            float v {};
            value.TryGet(v);
            return v;
        }
        case XMLAttributeValueView::Type::Double: {
            double v {};
            value.TryGet(v);
            return v;
        }
        default:
            return value.ToString();
        }
    }

    DocumentElement& mElement;
    std::unique_ptr<ElementBuilder> mChildBuilder;
};

void Write(ProjectSerializer& serializer, const DocumentElement& element)
{
    const wxString name = wxString::FromUTF8(element.name);
    serializer.StartTag(name);
    for (const auto& [attrName, value] : element.attributes) {
        const wxString wxAttrName = wxString::FromUTF8(attrName);
        std::visit([&](const auto& v) {
            using T = std::decay_t<decltype(v)>;
            if constexpr (std::is_same_v<T, std::string>) {
                serializer.WriteAttr(wxAttrName, wxString::FromUTF8(v));
            } else if constexpr (std::is_same_v<T, unsigned long long>) {
                serializer.WriteAttr(wxAttrName, static_cast<size_t>(v));
            } else {
                serializer.WriteAttr(wxAttrName, v);
            }
        }, value);
    }
    for (const auto& [blobName, data] : element.blobs) {
        serializer.WriteBlob(wxString::FromUTF8(blobName), data.data(), data.size());
    }
    if (!element.content.empty()) {
        serializer.WriteData(wxString::FromUTF8(element.content));
    }
    for (const auto& child : element.children) {
        Write(serializer, child);
    }
    serializer.EndTag(name);
}
} // namespace

const DocumentValue* DocumentElement::Attribute(std::string_view attributeName) const
{
    const auto it = std::find_if(attributes.begin(), attributes.end(),
                                 [&](const auto& attr) { return attr.first == attributeName; });
    return it == attributes.end() ? nullptr : &it->second;
}

void DocumentElement::SetAttribute(std::string_view attributeName, DocumentValue value)
{
    const auto it = std::find_if(attributes.begin(), attributes.end(),
                                 [&](const auto& attr) { return attr.first == attributeName; });
    if (it == attributes.end()) {
        attributes.emplace_back(std::string(attributeName), std::move(value));
    } else {
        it->second = std::move(value);
    }
}

std::optional<long long> DocumentElement::IntAttribute(std::string_view attributeName) const
{
    const auto value = Attribute(attributeName);
    if (!value) {
        return {};
    }
    if (const auto v = std::get_if<long long>(value)) {
        return *v;
    }
    if (const auto v = std::get_if<unsigned long long>(value)) {
        return static_cast<long long>(*v);
    }
    if (const auto v = std::get_if<std::string>(value)) {
        try {
            size_t pos = 0;
            const long long parsed = std::stoll(*v, &pos);
            if (pos == v->size()) {
                return parsed;
            }
        } catch (...) {
        }
    }
    return {};
}

std::optional<std::string> DocumentElement::StringAttribute(std::string_view attributeName) const
{
    const auto value = Attribute(attributeName);
    if (const auto v = value ? std::get_if<std::string>(value) : nullptr) {
        return *v;
    }
    return {};
}

std::optional<DocumentElement> DecodeProjectBlob(const std::vector<uint8_t>& blob)
{
    if (blob.size() < sizeof(uint64_t)) {
        return {};
    }
    // The dict size header isn't needed: the decoder reads the dict and the doc
    // as one stream, as from the `project` table
    MemoryStreamReader reader(blob.data() + sizeof(uint64_t), blob.size() - sizeof(uint64_t));
    // The base handler receives the root element itself
    DocumentElement root;
    ElementBuilder builder(root);
    if (!ProjectSerializer::Decode(reader, &builder) || root.name.empty()) {
        return {};
    }
    return root;
}

std::vector<uint8_t> EncodeProjectBlob(const DocumentElement& root)
{
    ProjectSerializer serializer;
    Write(serializer, root);
    return PackProjectBlob(serializer);
}

std::vector<uint8_t> PackProjectBlob(const ProjectSerializer& serializer)
{
    const uint64_t dictSize = serializer.GetDict().GetSize();
    const uint64_t docSize = serializer.GetData().GetSize();

    std::vector<uint8_t> data(sizeof(uint64_t) + dictSize + docSize);
    const uint64_t dictSizeData = IsLittleEndian() ? dictSize : SwapIntBytes(dictSize);
    std::memcpy(data.data(), &dictSizeData, sizeof(uint64_t));

    size_t offset = sizeof(uint64_t);
    for (const auto [chunkData, size] : serializer.GetDict()) {
        std::memcpy(data.data() + offset, chunkData, size);
        offset += size;
    }
    for (const auto [chunkData, size] : serializer.GetData()) {
        std::memcpy(data.data() + offset, chunkData, size);
        offset += size;
    }
    return data;
}
} // namespace audacity::cloud::audiocom::sync
