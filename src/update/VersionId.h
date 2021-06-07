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
	int mVersion{ 0 };
	int mRelease{ 0 };
	int mRevision{ 0 };
};

static inline VersionId CurrentBuildVersion()
{
	return VersionId{ AUDACITY_VERSION, AUDACITY_RELEASE, AUDACITY_REVISION };
}
