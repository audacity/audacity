#include "VersionId.h"

VersionId::VersionId (size_t version, size_t release, size_t revision)
    : mVersion (version),
      mRelease (release),
      mRevision (revision)
{}

wxString VersionId::makeString (size_t version, size_t release, size_t revision)
{
    return std::to_string (version)
        + "." + std::to_string (release)
        + "." + std::to_string (revision);
}

VersionId VersionId::ParseFromString (wxString& versionString)
{
    auto versionStringParts = wxSplit (versionString, '.');

    // If we have corrupted version string,
    // then return the zero version number, that not allow us to update.
    if (versionStringParts.size() != 3)
        return VersionId (0, 0, 0);

    for (auto& v : versionStringParts)
    {
        if (v.empty() || !v.IsNumber())
            return VersionId (0, 0, 0);
    }

    return VersionId (
        std::stoi (versionStringParts[0].ToStdString()),
        std::stoi (versionStringParts[1].ToStdString()),
        std::stoi (versionStringParts[2].ToStdString())
    );
}

wxString VersionId::getString() const
{
    return makeString (mVersion, mRelease, mRevision);
}

bool VersionId::isZero()
{
    return mVersion == 0 && mRelease == 0 && mRevision == 0;
}

bool VersionId::operator== (const VersionId& other)
{
    return mVersion == other.mVersion &&
           mRelease == other.mRelease &&
           mRevision == other.mRevision;
}

bool VersionId::operator!= (const VersionId& other)
{
    return !(*this == other);
}

bool VersionId::operator< (const VersionId& other)
{
    if (mVersion < other.mVersion)
        return true;

    if (mRelease < other.mRelease &&
        mVersion == other.mVersion)
        return true;

    if (mRevision < other.mRevision &&
        mVersion == other.mVersion &&
        mRelease == other.mRelease)
        return true;

    return false;
}

bool VersionId::operator> (const VersionId& other)
{
    if (*this == other) return false;

    return !(*this < other);
}
