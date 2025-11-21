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

bool UpdateDataParser::Parse(const UpdateDataFeed::UpdateDataFormat& updateData, UpdateDataFeed* feed)
{
    XMLFileReader xmlReader;

    ValueRestorer<UpdateDataFeed*> setter{ mUpdateDataFeed, feed };

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
    switch (mContext)
    {
    case ParserContext::Unset:
         mContext = ParserContext::Root;
         return true;

    case ParserContext::Root:
        return HandleRootTags(tag);

    case ParserContext::Changelog:
        return HandleChangelogTags(tag, attrs);

    case ParserContext::Notifications:
        return HandleNotificationsTags(tag);

    case ParserContext::Notification:
        return HandleNotificationTags(tag);

    case ParserContext::NotificationAction:
        return HandleNotificationActionTags(tag);

    case ParserContext::OS:
        return HandleOSTags(tag);
    }

    return false;
}

bool UpdateDataParser::HandleRootTags(const std::string_view& tag)
{
    if (tag == mXmlTagNames[XmlParsedTags::kNotificationsTag])
    {
        mContext = ParserContext::Notifications;
        return true;
    }

    if (tag == mXmlTagNames[XmlParsedTags::kChangelogTag])
    {
        mContext = ParserContext::Changelog;
        mCurrentContentTag = XmlParsedTags::kChangelogTag;
        return true;
    }

    if (tag == mXmlTagNames[XmlParsedTags::kOsTag])
    {
        mContext = ParserContext::OS;
        return true;
    }

    if (tag == mXmlTagNames[XmlParsedTags::kReleaseNotesUrlTag])
    {
        mCurrentContentTag = XmlParsedTags::kReleaseNotesUrlTag;
        return true;
    }

    if (tag == mXmlTagNames[XmlParsedTags::kDescriptionTag])
    {
        // Skip root-level Description (legacy)
        return true;
    }

    return false;
}

bool UpdateDataParser::HandleChangelogTags(const std::string_view& tag, const AttributesList& attrs)
{
    if (tag == mXmlTagNames[XmlParsedTags::kDescriptionTag])
    {
        mCurrentContentTag = XmlParsedTags::kDescriptionTag;
        return true;
    }

    if (tag == mXmlTagNames[XmlParsedTags::kChangelogItemTag])
    {
        mCurrentContentTag = XmlParsedTags::kChangelogItemTag;
        mCurrentChangelogItem = ChangelogItem();

        // Capture version attribute if present
        for (const auto& attr : attrs)
        {
            if (attr.first == "version")
            {
                mCurrentChangelogItem.version = attr.second.ToWString();
                break;
            }
        }
        return true;
    }

    return false;
}

bool UpdateDataParser::HandleNotificationsTags(const std::string_view& tag)
{
    if (tag == mXmlTagNames[XmlParsedTags::kNotificationTag])
    {
        mContext = ParserContext::Notification;
        mCurrentNotification = Notification();
        return true;
    }

    return false;
}

bool UpdateDataParser::HandleNotificationTags(const std::string_view& tag)
{
    if (tag == mXmlTagNames[XmlParsedTags::kNotificationActionTag])
    {
        mContext = ParserContext::NotificationAction;
        mCurrentButton = NotificationAction();
        return true;
    }

    if (tag == mXmlTagNames[XmlParsedTags::kUuidTag])
    {
        mCurrentContentTag = XmlParsedTags::kUuidTag;
        return true;
    }

    if (tag == mXmlTagNames[XmlParsedTags::kTitleTag])
    {
        mCurrentContentTag = XmlParsedTags::kTitleTag;
        return true;
    }

    if (tag == mXmlTagNames[XmlParsedTags::kMessageTag])
    {
        mCurrentContentTag = XmlParsedTags::kMessageTag;
        return true;
    }

    if (tag == mXmlTagNames[XmlParsedTags::kStartTag])
    {
        mCurrentContentTag = XmlParsedTags::kStartTag;
        return true;
    }

    if (tag == mXmlTagNames[XmlParsedTags::kExpiresTag])
    {
        mCurrentContentTag = XmlParsedTags::kExpiresTag;
        return true;
    }

    return false;
}

bool UpdateDataParser::HandleNotificationActionTags(const std::string_view& tag)
{
    if (tag == mXmlTagNames[XmlParsedTags::kLabelTag])
    {
        mCurrentContentTag = XmlParsedTags::kLabelTag;
        return true;
    }

    if (tag == mXmlTagNames[XmlParsedTags::kLinkTag])
    {
        mCurrentContentTag = XmlParsedTags::kLinkTag;
        return true;
    }

    return false;
}

bool UpdateDataParser::HandleOSTags(const std::string_view& tag)
{
    const wxPlatformInfo& info = wxPlatformInfo::Get();

    // Check if this is a platform tag matching our current platform
    if (tag == mXmlTagNames[XmlParsedTags::kWin32Tag])
    {
        mIsCorrectPlatform = (info.GetArchitecture() == wxARCH_32 && (info.GetOperatingSystemId() & wxOS_WINDOWS));
        return true;
    }
    else if (tag == mXmlTagNames[XmlParsedTags::kWin64Tag])
    {
        mIsCorrectPlatform = (info.GetArchitecture() == wxARCH_64 && (info.GetOperatingSystemId() & wxOS_WINDOWS));
        return true;
    }
    else if (tag == mXmlTagNames[XmlParsedTags::kMacosTag])
    {
        mIsCorrectPlatform = (info.GetOperatingSystemId() & wxOS_MAC);
        return true;
    }
    else if (tag == mXmlTagNames[XmlParsedTags::kLinuxTag])
    {
        mIsCorrectPlatform = (info.GetOperatingSystemId() & wxOS_UNIX_LINUX);
        return true;
    }

    // Only parse Version and Link tags for the correct platform
    if (tag == mXmlTagNames[XmlParsedTags::kVersionTag])
    {
        if (mIsCorrectPlatform)
            mCurrentContentTag = XmlParsedTags::kVersionTag;
        return true;
    }

    if (tag == mXmlTagNames[XmlParsedTags::kLinkTag])
    {
        if (mIsCorrectPlatform)
            mCurrentContentTag = XmlParsedTags::kLinkTag;
        return true;
    }

    return false;
}

void UpdateDataParser::HandleXMLEndTag(const std::string_view& tag)
{
    if (tag == mXmlTagNames[XmlParsedTags::kUpdateTag])
    {
        mContext = ParserContext::Unset;
    }

    else if (tag == mXmlTagNames[XmlParsedTags::kNotificationActionTag])
    {
        if (mContext == ParserContext::NotificationAction)
        {
            mCurrentNotification.notificationAction = mCurrentButton;
            mContext = ParserContext::Notification;
        }
    }
    else if (tag == mXmlTagNames[XmlParsedTags::kNotificationTag])
    {
        if (mContext == ParserContext::Notification && mUpdateDataFeed)
        {
            mUpdateDataFeed->notifications.push_back(mCurrentNotification);
        }
        mContext = ParserContext::Notifications;
    }
    else if (tag == mXmlTagNames[XmlParsedTags::kNotificationsTag])
    {
        mContext = ParserContext::Root;
    }
    else if (tag == mXmlTagNames[XmlParsedTags::kChangelogTag])
    {
        mContext = ParserContext::Root;
    }
    else if (tag == mXmlTagNames[XmlParsedTags::kOsTag])
    {
        mContext = ParserContext::Root;
        mIsCorrectPlatform = false;
    }
    else if (tag == mXmlTagNames[XmlParsedTags::kWin32Tag] ||
             tag == mXmlTagNames[XmlParsedTags::kWin64Tag] ||
             tag == mXmlTagNames[XmlParsedTags::kMacosTag] ||
             tag == mXmlTagNames[XmlParsedTags::kLinuxTag])
    {
        mIsCorrectPlatform = false;
    }

    mCurrentContentTag = XmlParsedTags::kNotUsedTag;
}

void UpdateDataParser::HandleXMLContent(const std::string_view& content)
{
    if (mUpdateDataFeed == nullptr)
        return;

    wxString trimmedContent = std::string(content);
    trimmedContent.Trim(true).Trim(false);

    switch (mCurrentContentTag)
    {
    case XmlParsedTags::kDescriptionTag:
        mUpdateDataFeed->versionPatch.description = trimmedContent;
        break;

    case XmlParsedTags::kChangelogItemTag:
        mCurrentChangelogItem.text = trimmedContent;
        mUpdateDataFeed->versionPatch.changelog.push_back(mCurrentChangelogItem);
        break;

    case XmlParsedTags::kReleaseNotesUrlTag:
        mUpdateDataFeed->versionPatch.releaseNotesUrl = trimmedContent;
        break;

    case XmlParsedTags::kVersionTag:
        mUpdateDataFeed->versionPatch.version = VersionId::ParseFromString(trimmedContent);
        break;

    case XmlParsedTags::kLinkTag:
        if (mContext == ParserContext::NotificationAction)
        {
            mCurrentButton.link = trimmedContent;
        }
        else if (mContext == ParserContext::OS)
        {
            mUpdateDataFeed->versionPatch.download = trimmedContent;
        }
        break;

    case XmlParsedTags::kUuidTag:
        if (mContext == ParserContext::Notification)
            mCurrentNotification.uuid = trimmedContent;
        break;

    case XmlParsedTags::kTitleTag:
        if (mContext == ParserContext::Notification)
            mCurrentNotification.title = trimmedContent;
        break;

    case XmlParsedTags::kMessageTag:
        if (mContext == ParserContext::Notification)
            mCurrentNotification.message = trimmedContent;
        break;

    case XmlParsedTags::kStartTag:
        if (mContext == ParserContext::Notification)
        {
            mCurrentNotification.start.ParseISOCombined(trimmedContent);
        }
        break;

    case XmlParsedTags::kExpiresTag:
        if (mContext == ParserContext::Notification)
        {
            mCurrentNotification.expires.ParseISOCombined(trimmedContent);
        }
        break;

    case XmlParsedTags::kLabelTag:
        if (mContext == ParserContext::NotificationAction)
            mCurrentButton.label = trimmedContent;
        break;

    default:
        break;
    }
}

XMLTagHandler* UpdateDataParser::HandleXMLChild(const std::string_view& tag)
{
    for (auto& xmlTag : mXmlTagNames)
    {
        if (tag == xmlTag.second)
            return this;
    }

    return NULL;
}
