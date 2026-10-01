/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ProjectDocument.h

**********************************************************************/
#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

class ProjectSerializer;

namespace audacity::cloud::audiocom::sync {
//! An attribute value as stored in a project document, keeping its type so
//! that it can be written back unchanged
using DocumentValue = std::variant<long long, unsigned long long, float, double, std::string>;

//! A project document (the content of the `project` table's `dict` and `doc`
//! columns, as exchanged with the server) as an editable tree
struct DocumentElement final
{
    std::string name;
    std::vector<std::pair<std::string, DocumentValue> > attributes;
    std::vector<DocumentElement> children;
    std::vector<std::pair<std::string, std::vector<uint8_t> > > blobs;
    std::string content;

    const DocumentValue* Attribute(std::string_view attributeName) const;
    void SetAttribute(std::string_view attributeName, DocumentValue value);

    //! Integer value of an attribute, if present and integral
    std::optional<long long> IntAttribute(std::string_view attributeName) const;
    //! String value of an attribute, if present and a string
    std::optional<std::string> StringAttribute(std::string_view attributeName) const;
};

//! Decodes a project blob as uploaded to the server: `[u64 dict size][dict][doc]`
CLOUD_AUDIOCOM_API std::optional<DocumentElement> DecodeProjectBlob(const std::vector<uint8_t>& blob);

//! Inverse of DecodeProjectBlob
CLOUD_AUDIOCOM_API std::vector<uint8_t> EncodeProjectBlob(const DocumentElement& root);

//! A serialized project in the form uploaded to the server
CLOUD_AUDIOCOM_API std::vector<uint8_t> PackProjectBlob(const ProjectSerializer& serializer);
} // namespace audacity::cloud::audiocom::sync
