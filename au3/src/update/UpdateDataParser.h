/*!********************************************************************
 Audacity: A Digital Audio Editor

 @file UpdateDataParser.h
 @brief Declare a class that parses update server data format.

 Anton Gerasimov
 **********************************************************************/
#pragma once

#include "VersionPatch.h"

#include "XMLTagHandler.h"

#include <wx/arrstr.h>
#include <map>

/// A class that parses update server data format.
class UpdateDataParser final : public XMLTagHandler
{
public:
    UpdateDataParser();
    ~UpdateDataParser();

    //! Parsing from update data format to VersionPatch fields.
    /*!
       @param updateData InputData.
       @param versionPath Parsed output data.
       @return True if success.
    */
    bool Parse(const VersionPatch::UpdateDataFormat& updateData, VersionPatch* versionPatch);

private:
    enum class XmlParsedTags : int {
        kNotUsedTag,
        kUpdateTag,
        kDescriptionTag,
        kOsTag,
        kWin32Tag,
        kWin64Tag,
        kMacosTag,
        kLinuxTag,
        kVersionTag,
        kLinkTag
    };
    XmlParsedTags mXmlParsingState{ XmlParsedTags::kNotUsedTag };

    std::map<XmlParsedTags, const char*> mXmlTagNames{
        { XmlParsedTags::kUpdateTag, "Updates" },
        { XmlParsedTags::kDescriptionTag, "Description" },
        { XmlParsedTags::kOsTag, "OS" },
        { XmlParsedTags::kWin32Tag, "Win32" },
        { XmlParsedTags::kWin64Tag, "Win64" },
        { XmlParsedTags::kMacosTag, "Macos" },
        { XmlParsedTags::kLinuxTag, "Linux" },
        { XmlParsedTags::kVersionTag, "Version" },
        { XmlParsedTags::kLinkTag, "Link" },
    };

    bool HandleXMLTag(
        const std::string_view& tag, const AttributesList& attrs) override;
    void HandleXMLEndTag(const std::string_view& tag) override;
    void HandleXMLContent(const std::string_view& content) override;
    XMLTagHandler* HandleXMLChild(const std::string_view& tag) override;

    wxArrayString SplitChangelogSentences(const wxString& changelogContent);

    VersionPatch* mVersionPatch{ nullptr };
};
