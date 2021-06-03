#pragma once

#include "UpdateDataParser.h"

#include "xml/XMLFileReader.h"

#include <wx/tokenzr.h>

UpdateDataParser::UpdateDataParser()
{}

UpdateDataParser::~UpdateDataParser()
{}

bool UpdateDataParser::Parse(const ServerCommunication::UpdateDataFormat& updateData, VersionPatch* versionPatch)
{
	XMLFileReader xmlReader;

	mVersionPatch = versionPatch;
	auto ok = xmlReader.ParseString(this, updateData);
	mVersionPatch = nullptr;

	return ok;
}

wxArrayString UpdateDataParser::splitChangelogSentences(const wxString& changelogContent)
{
	wxArrayString changelogSentenceList;

	const wxString separator(". ");
	wxStringTokenizer tokenizer(changelogContent, separator);

	while (tokenizer.HasMoreTokens())
	{
		changelogSentenceList.Add(tokenizer.GetNextToken());
	}

	return changelogSentenceList;
}

bool UpdateDataParser::HandleXMLTag(const wxChar* tag, const wxChar** attrs)
{
	if (wxStrcmp(tag, mXmlTagNames[XmlParsedTags::kDescriptionTag]) == 0)
	{
		mXmlParsingState = XmlParsedTags::kDescriptionTag;
		return true;
	}

	if (wxStrcmp(tag, mXmlTagNames[XmlParsedTags::kWindowsTag]) == 0)
	{
		if (wxPlatformInfo::Get().GetOperatingSystemId() & wxOS_WINDOWS)
			mXmlParsingState = XmlParsedTags::kOsTag;
		return true;
	}

	if (wxStrcmp(tag, mXmlTagNames[XmlParsedTags::kMacosTag]) == 0)
	{
		if (wxPlatformInfo::Get().GetOperatingSystemId() & wxOS_MAC)
			mXmlParsingState = XmlParsedTags::kOsTag;
		return true;
	}

	if (wxStrcmp(tag, mXmlTagNames[XmlParsedTags::kLinuxTag]) == 0)
	{
		if (wxPlatformInfo::Get().GetOperatingSystemId() & wxOS_UNIX_LINUX)
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

	if (mXmlParsingState == XmlParsedTags::kVersionTag)
		mXmlParsingState = XmlParsedTags::kOsTag;
}

void UpdateDataParser::HandleXMLContent(const wxString& content)
{
	if (mVersionPatch == nullptr)
		return;

	wxString trimedContent(content);

	switch (mXmlParsingState)
	{
	case XmlParsedTags::kDescriptionTag:
		trimedContent.Trim(true).Trim(false);
		// TODO: need current spliting by ". "
		//mVersionPatch->changelog = wxSplit(trimedContent, '.');
		mVersionPatch->changelog = splitChangelogSentences(trimedContent);
		break;

	case XmlParsedTags::kVersionTag:
		trimedContent.Trim(true).Trim(false);
		mVersionPatch->version = VersionId::ParseFromString(trimedContent);
		break;

	case XmlParsedTags::kLinkTag:
		trimedContent.Trim(true).Trim(false);
		mVersionPatch->download = trimedContent;
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
