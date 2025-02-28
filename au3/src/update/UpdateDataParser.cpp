/*!********************************************************************
 Audacity: A Digital Audio Editor

 @file UpdateDataParser.cpp
 @brief Declare a class that parses update server data format.

 Anton Gerasimov
 **********************************************************************/

#include "UpdateDataParser.h"

#include "XMLFileReader.h"
#include "MemoryX.h"
#include <wx/platinfo.h>

UpdateDataParser::UpdateDataParser()
{}

UpdateDataParser::~UpdateDataParser()
{}

bool UpdateDataParser::Parse(const VersionPatch::UpdateDataFormat& updateData, VersionPatch* versionPatch)
{
    XMLFileReader xmlReader;

    ValueRestorer<VersionPatch*> setter{ mVersionPatch, versionPatch };

    return xmlReader.ParseString(this, updateData);
}

wxArrayString UpdateDataParser::SplitChangelogSentences(const wxString& changelogContent)
{
    wxArrayString changelogSentenceList;

    size_t pos = 0;
    std::string s(changelogContent.ToStdString());
    std::string token;
    const std::string delimiter(". ");

    while ((pos = s.find(delimiter)) != std::string::npos)
    {
        token = s.substr(0, pos + 1);
        changelogSentenceList.Add(wxString(token).Trim());

        s.erase(0, pos + delimiter.length());
    }
    changelogSentenceList.Add(wxString(s).Trim());

    return changelogSentenceList;
}

bool UpdateDataParser::HandleXMLTag(const std::string_view& tag, const AttributesList& attrs)
{
    if (tag == mXmlTagNames[XmlParsedTags::kDescriptionTag]) {
        mXmlParsingState = XmlParsedTags::kDescriptionTag;
        return true;
    }

    const wxPlatformInfo& info = wxPlatformInfo::Get();

    constexpr bool is32Bit = sizeof(void*) == 4;
    constexpr bool is64Bit = sizeof(void*) == 8;

    if (is32Bit) {
        if (tag == mXmlTagNames[XmlParsedTags::kWin32Tag]) {
            if (info.GetOperatingSystemId() & wxOS_WINDOWS) {
                mXmlParsingState = XmlParsedTags::kOsTag;
            }
            return true;
        }
    }

    if (is64Bit) {
        if (tag == mXmlTagNames[XmlParsedTags::kWin64Tag]) {
            if (info.GetOperatingSystemId() & wxOS_WINDOWS) {
                mXmlParsingState = XmlParsedTags::kOsTag;
            }
            return true;
        }
    }

    if (tag == mXmlTagNames[XmlParsedTags::kMacosTag]) {
        if (info.GetOperatingSystemId() & wxOS_MAC) {
            mXmlParsingState = XmlParsedTags::kOsTag;
        }
        return true;
    }

    if (tag == mXmlTagNames[XmlParsedTags::kLinuxTag]) {
        if (info.GetOperatingSystemId() & wxOS_UNIX_LINUX) {
            mXmlParsingState = XmlParsedTags::kOsTag;
        }
        return true;
    }

    if (tag == mXmlTagNames[XmlParsedTags::kVersionTag]) {
        if (mXmlParsingState == XmlParsedTags::kOsTag) {
            mXmlParsingState = XmlParsedTags::kVersionTag;
        }
        return true;
    }

    if (tag == mXmlTagNames[XmlParsedTags::kLinkTag]) {
        if (mXmlParsingState == XmlParsedTags::kOsTag) {
            mXmlParsingState = XmlParsedTags::kLinkTag;
        }
        return true;
    }

    for (auto& xmlTag : mXmlTagNames) {
        if (tag == xmlTag.second) {
            return true;
        }
    }

    return false;
}

void UpdateDataParser::HandleXMLEndTag(const std::string_view& tag)
{
    if (mXmlParsingState == XmlParsedTags::kDescriptionTag
        || mXmlParsingState == XmlParsedTags::kLinkTag) {
        mXmlParsingState = XmlParsedTags::kNotUsedTag;
    }

    // If it is our working OS, using "kOsTag" for keeping ready for parse state for both tags:
    // <Version> and <Link>, that ordered one after another.
    if (mXmlParsingState == XmlParsedTags::kVersionTag) {
        mXmlParsingState = XmlParsedTags::kOsTag;
    }
}

void UpdateDataParser::HandleXMLContent(const std::string_view& content)
{
    if (mVersionPatch == nullptr) {
        return;
    }

    wxString trimmedContent = std::string(content);

    switch (mXmlParsingState) {
    case XmlParsedTags::kDescriptionTag:
        trimmedContent.Trim(true).Trim(false);
        mVersionPatch->changelog = SplitChangelogSentences(trimmedContent);
        break;

    case XmlParsedTags::kVersionTag:
        trimmedContent.Trim(true).Trim(false);
        mVersionPatch->version = VersionId::ParseFromString(trimmedContent);
        break;

    case XmlParsedTags::kLinkTag:
        trimmedContent.Trim(true).Trim(false);
        mVersionPatch->download = trimmedContent;
        break;

    default:
        break;
    }
}

XMLTagHandler* UpdateDataParser::HandleXMLChild(const std::string_view& tag)
{
    for (auto& xmlTag : mXmlTagNames) {
        if (tag == xmlTag.second) {
            return this;
        }
    }

    return NULL;
}
