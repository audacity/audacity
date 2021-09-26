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

bool UpdateDataParser::HandleXMLTag(const wxChar* tag, const wxChar** attrs)
{
    if (wxStrcmp(tag, mXmlTagNames[XmlParsedTags::kDescriptionTag]) == 0)
    {
        mXmlParsingState = XmlParsedTags::kDescriptionTag;
        return true;
    }

    const wxPlatformInfo& info = wxPlatformInfo::Get();

    constexpr bool is32Bit = sizeof(void*) == 4;
    constexpr bool is64Bit = sizeof(void*) == 8;

    if (is32Bit)
    {
       if (wxStrcmp(tag, mXmlTagNames[XmlParsedTags::kWin32Tag]) == 0)
       {
          if (info.GetOperatingSystemId() & wxOS_WINDOWS)
             mXmlParsingState = XmlParsedTags::kOsTag;
          return true;
       }
    }

    if (is64Bit)
    {
       if (wxStrcmp(tag, mXmlTagNames[XmlParsedTags::kWin64Tag]) == 0)
       {
          if (info.GetOperatingSystemId() & wxOS_WINDOWS)
             mXmlParsingState = XmlParsedTags::kOsTag;
          return true;
       }
    }

    if (wxStrcmp(tag, mXmlTagNames[XmlParsedTags::kMacosTag]) == 0)
    {
        if (info.GetOperatingSystemId() & wxOS_MAC)
            mXmlParsingState = XmlParsedTags::kOsTag;
        return true;
    }

    if (wxStrcmp(tag, mXmlTagNames[XmlParsedTags::kLinuxTag]) == 0)
    {
        if (info.GetOperatingSystemId() & wxOS_UNIX_LINUX)
            mXmlParsingState = XmlParsedTags::kOsTag;
        return true;
    }

    if (wxStrcmp(tag, mXmlTagNames[XmlParsedTags::kVersionTag]) == 0)
    {
        if (mXmlParsingState == XmlParsedTags::kOsTag)
            mXmlParsingState = XmlParsedTags::kVersionTag;
        return true;
    }

    if (wxStrcmp(tag, mXmlTagNames[XmlParsedTags::kLinkTag]) == 0)
    {
        if (mXmlParsingState == XmlParsedTags::kOsTag)
            mXmlParsingState = XmlParsedTags::kLinkTag;
        return true;
    }

    for (auto& xmlTag : mXmlTagNames)
    {
        if (wxStrcmp(tag, xmlTag.second) == 0)
            return true;
    }

    return false;
}

void UpdateDataParser::HandleXMLEndTag(const wxChar* tag)
{
    if (mXmlParsingState == XmlParsedTags::kDescriptionTag ||
        mXmlParsingState == XmlParsedTags::kLinkTag)
        mXmlParsingState = XmlParsedTags::kNotUsedTag;

    // If it is our working OS, using "kOsTag" for keeping ready for parse state for both tags:
    // <Version> and <Link>, that ordered one after another.
    if (mXmlParsingState == XmlParsedTags::kVersionTag)
        mXmlParsingState = XmlParsedTags::kOsTag;
}

void UpdateDataParser::HandleXMLContent(const wxString& content)
{
    if (mVersionPatch == nullptr)
        return;

    wxString trimmedContent(content);

    switch (mXmlParsingState)
    {
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

XMLTagHandler* UpdateDataParser::HandleXMLChild(const wxChar* tag)
{
    for (auto& xmlTag : mXmlTagNames)
    {
        if (wxStrcmp(tag, xmlTag.second) == 0)
            return this;
    }

    return NULL;
}
