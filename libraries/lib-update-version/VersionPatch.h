#pragma once

#include "VersionId.h"

struct VersionPatch final
{
#if 0
	VersionPatch (VersionId version, wxArrayString changelog, wxString download)
		: version (version),
		  changelog (changelog),
		  download (download)
	{}
#endif
	//VersionPatch() = default;

	VersionId version;
	wxArrayString changelog;
	wxString download;
#if 0
public:
	VersionPatch (VersionId version, wxArrayString changelog);


	VersionId getVersion() const { return mVersion; }
	wxArrayString getChangelog() const { return mChangelog; }

private:
	const VersionId mVersion;
	const wxArrayString mChangelog;
#endif
};
