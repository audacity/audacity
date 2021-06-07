#pragma once

#include "VersionId.h"

struct VersionPatch final
{
	VersionPatch() = default;
	
	using UpdateDataFormat = std::string;

	VersionId version;
	wxArrayString changelog;
	wxString download;
};
