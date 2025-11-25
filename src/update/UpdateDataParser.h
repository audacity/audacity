/*!********************************************************************
 Audacity: A Digital Audio Editor

 @file UpdateDataParser.h
 @brief Declare a class that parses update server data format.

 Anton Gerasimov
 **********************************************************************/
#pragma once

#include "UpdateDataStructures.h"

#include "XMLTagHandler.h"

#include <wx/arrstr.h>
#include <map>

/// A class that parses update server data format.
class UpdateDataParser final : public XMLTagHandler
{
public:
    UpdateDataParser();
    ~UpdateDataParser();

    //! Parsing from update data format to UpdateDataFeed.
    /*!
       @param updateData Input XML data from update server.
       @param feed Parsed output data containing new version info and notifications
       @return True if success.
    */
    bool Parse(const UpdateDataFeed::UpdateDataFormat& updateData, UpdateDataFeed* feed);

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
        kLinkTag,
        kChangelogTag,
        kChangelogItemTag,
        kReleaseNotesUrlTag,
        kNotificationsTag,
        kNotificationTag,
        kUuidTag,
        kTitleTag,
        kMessageTag,
        kStartTag,
        kExpiresTag,
        kNotificationActionTag,
        kLabelTag
    };

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
        { XmlParsedTags::kChangelogTag, "Changelog" },
        { XmlParsedTags::kChangelogItemTag, "Item" },
        { XmlParsedTags::kReleaseNotesUrlTag, "ReleaseNotesUrl" },
        { XmlParsedTags::kNotificationsTag, "Notifications" },
        { XmlParsedTags::kNotificationTag, "Notification" },
        { XmlParsedTags::kUuidTag, "Uuid" },
        { XmlParsedTags::kTitleTag, "Title" },
        { XmlParsedTags::kMessageTag, "Message" },
        { XmlParsedTags::kStartTag, "Start" },
        { XmlParsedTags::kExpiresTag, "Expires" },
        { XmlParsedTags::kNotificationActionTag, "Action" },
        { XmlParsedTags::kLabelTag, "Label" }
    };

    // Track which tag's content we're currently reading
    XmlParsedTags mCurrentContentTag{ XmlParsedTags::kNotUsedTag };

    // Track if we're in the correct platform section for OS-specific data
    bool mIsCorrectPlatform{ false };

    bool HandleXMLTag(
       const std::string_view& tag, const AttributesList& attrs) override;
    void HandleXMLEndTag(const std::string_view& tag) override;
    void HandleXMLContent(const std::string_view& content) override;
    XMLTagHandler* HandleXMLChild(const std::string_view& tag) override;

    // Context-specific tag handlers
    bool HandleRootTags(const std::string_view& tag);
    bool HandleChangelogTags(const std::string_view& tag, const AttributesList& attrs);
    bool HandleNotificationsTags(const std::string_view& tag);
    bool HandleNotificationTags(const std::string_view& tag);
    bool HandleNotificationActionTags(const std::string_view& tag);
    bool HandleOSTags(const std::string_view& tag);

    wxArrayString SplitChangelogSentences(const wxString& changelogContent);

    UpdateDataFeed* mUpdateDataFeed{ nullptr };

    enum class ParserContext {
        Unset,
        Root,
        Changelog,
        Notifications,
        Notification,
        NotificationAction,
        OS
    };
    ParserContext mContext{ ParserContext::Unset };

    Notification mCurrentNotification;
    NotificationAction mCurrentButton;
    ChangelogItem mCurrentChangelogItem;
};
