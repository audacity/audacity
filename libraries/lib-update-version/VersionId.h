#pragma once

#include <wx/arrstr.h>

// Initialized of current build version by default.
class VersionId final
{
public:
    VersionId() = default;
    VersionId (size_t version, size_t release, size_t revision);

    static wxString makeString (size_t version, size_t release, size_t revision);
    static VersionId ParseFromString (wxString& versionString);

    wxString getString() const;
    bool isZero();

    bool operator== (const VersionId& other);
    bool operator!= (const VersionId& other);

    bool operator< (const VersionId& other);
    bool operator> (const VersionId& other);

private:
    size_t mVersion {AUDACITY_VERSION};
    size_t mRelease {AUDACITY_RELEASE};
    size_t mRevision {AUDACITY_REVISION};
};
