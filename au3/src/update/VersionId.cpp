/*!********************************************************************
 Audacity: A Digital Audio Editor

 @file VersionId.h
 @brief Declare a class with version number manipulation.

 Anton Gerasimov
 **********************************************************************/
#include "VersionId.h"

VersionId::VersionId(int version, int release, int revision, int patch)
    : mVersion(version),
    mRelease(release),
    mRevision(revision),
    mPatch(patch)
{}

wxString VersionId::MakeString(int version, int release, int revision, int patch)
{
    if (patch == 0) {
        return std::to_string(version)
               + "." + std::to_string(release)
               + "." + std::to_string(revision);
    } else {
        return std::to_string(version)
               + "." + std::to_string(release)
               + "." + std::to_string(revision)
               + "." + std::to_string(patch);
    }
}

VersionId VersionId::ParseFromString(wxString& versionString)
{
    auto versionStringParts = wxSplit(versionString, '.');

    // If we have corrupted version string,
    // then return the zero version number, that not allow us to update.
    const auto size = versionStringParts.size();
    if (size < 2 || size > 4) {
        return VersionId{}
    }

    VersionId versionId;

    const auto fields = { &versionId.mVersion, &versionId.mRelease,
                          &versionId.mRevision, &versionId.mPatch };

    auto currentField = fields.begin();

    for (auto& v : versionStringParts) {
        if (v.empty() || !v.IsNumber()) {
            return VersionId{}
        }

        **currentField = std::stoi(v.ToStdString());
        ++currentField;
    }

    return versionId;
}

wxString VersionId::GetString() const
{
    return MakeString(mVersion, mRelease, mRevision, mPatch);
}

bool VersionId::operator==(const VersionId& other)
{
    return std::tie(mVersion, mRelease, mRevision, mPatch)
           == std::tie(other.mVersion, other.mRelease, other.mRevision, other.mPatch);
}

bool VersionId::operator!=(const VersionId& other)
{
    return !(*this == other);
}

bool VersionId::operator<(const VersionId& other)
{
    return std::tie(mVersion, mRelease, mRevision, mPatch)
           < std::tie(other.mVersion, other.mRelease, other.mRevision, other.mPatch);
}

bool VersionId::operator>(const VersionId& other)
{
    return !(*this < other) && (*this != other);
}
