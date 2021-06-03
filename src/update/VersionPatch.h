#pragma once

#include "VersionId.h"

struct VersionPatch final
{
	VersionPatch() = default;

	VersionId version;
	wxArrayString changelog;
	wxString download;
};
