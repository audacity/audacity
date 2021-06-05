#pragma once

#include <wx/arrstr.h>

// Initialized of current build version by default.
class VersionId final
{
public:
	VersionId() = default;
	VersionId(int version, int release, int revision);

	static wxString MakeString(int version, int release, int revision);
	static VersionId ParseFromString(wxString& versionString);

	wxString getString() const;
	bool isZero();

	bool operator== (const VersionId& other);
	bool operator!= (const VersionId& other);

	bool operator< (const VersionId& other);
	bool operator> (const VersionId& other);

private:
	int mVersion{ AUDACITY_VERSION };
	int mRelease{ AUDACITY_RELEASE };
	int mRevision{ AUDACITY_REVISION };
};

static inline VersionId CurrentBuildVersion() { return VersionId{}; }
