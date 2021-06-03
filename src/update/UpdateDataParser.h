#pragma once

#include "ServerCommunication.h"
#include "VersionPatch.h"

#include "xml/XMLTagHandler.h"

#include <wx/arrstr.h>
#include <map>

class UpdateDataParser final : public XMLTagHandler
{
public:
	UpdateDataParser();
	~UpdateDataParser();

	bool Parse(const ServerCommunication::UpdateDataFormat& updateData, VersionPatch* versionPatch);

private:
	enum class XmlParsedTags : int
	{
		kNotUsedTag,
		kUpdateTag,
		kDescriptionTag,
		kOsTag,
		kWindowsTag,
		kMacosTag,
		kLinuxTag,
		kVersionTag,
		kLinkTag
	};
	XmlParsedTags mXmlParsingState{ XmlParsedTags::kNotUsedTag };
	std::map<XmlParsedTags, wxString> mXmlTagNames
	{
		{XmlParsedTags::kUpdateTag, wxT("Updates")},
		{XmlParsedTags::kDescriptionTag, wxT("Description")},
		{XmlParsedTags::kOsTag, wxT("OS")},
		{XmlParsedTags::kWindowsTag, wxT("Windows")},
		{XmlParsedTags::kMacosTag, wxT("Macos")},
		{XmlParsedTags::kLinuxTag, wxT("Linux")},
		{XmlParsedTags::kVersionTag, wxT("Version")},
		{XmlParsedTags::kLinkTag, wxT("Link")},
	};

	bool HandleXMLTag(const wxChar* tag, const wxChar** attrs) override;
	void HandleXMLEndTag(const wxChar* tag) override;
	void HandleXMLContent(const wxString& content) override;
	XMLTagHandler* HandleXMLChild(const wxChar* tag) override;

	wxArrayString splitChangelogSentences(const wxString& changelogContent);

	VersionPatch* mVersionPatch{ nullptr };
};
