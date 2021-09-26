/*!********************************************************************
 Audacity: A Digital Audio Editor

 @file VersionId.h
 @brief Declare a class with version number manipulation.

 Anton Gerasimov
 **********************************************************************/
#include "VersionId.h"

VersionId::VersionId(int version, int release, int revision)
    : mVersion(version),
      mRelease(release),
      mRevision(revision)
{}

wxString VersionId::MakeString(int version, int release, int revision)
{
    return std::to_string(version)
        + "." + std::to_string(release)
        + "." + std::to_string(revision);
}

VersionId VersionId::ParseFromString(wxString& versionString)
{
    auto versionStringParts = wxSplit(versionString, '.');

    // If we have corrupted version string,
    // then return the zero version number, that not allow us to update.
    if (versionStringParts.size() != 3)
        return VersionId{};

    for (auto& v : versionStringParts)
    {
        if (v.empty() || !v.IsNumber())
            return VersionId{};
    }

    return VersionId(
        std::stoi(versionStringParts[0].ToStdString()),
        std::stoi(versionStringParts[1].ToStdString()),
        std::stoi(versionStringParts[2].ToStdString())
    );
}

wxString VersionId::GetString() const
{
    return MakeString(mVersion, mRelease, mRevision);
}

bool VersionId::operator== (const VersionId& other)
{
    return std::tie(mVersion, mRelease, mRevision) ==
           std::tie(other.mVersion, other.mRelease, other.mRevision);
}

bool VersionId::operator!= (const VersionId& other)
{
    return !(*this == other);
}

bool VersionId::operator< (const VersionId& other)
{
    return std::tie(mVersion, mRelease, mRevision) <
           std::tie(other.mVersion, other.mRelease, other.mRevision);
}

bool VersionId::operator> (const VersionId& other)
{
    return !(*this < other) && (*this != other);
}
