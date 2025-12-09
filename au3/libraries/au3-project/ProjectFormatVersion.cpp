/**********************************************************************

  Audacity: A Digital Audio Editor

  ProjectFormatVersion.cpp

  Dmitry Vedenko

**********************************************************************/

#include "ProjectFormatVersion.h"

#include <tuple>

bool operator ==(ProjectFormatVersion lhs, ProjectFormatVersion rhs) noexcept
{
    return std::tie(lhs.Major, lhs.Minor, lhs.Revision, lhs.ModLevel)
           == std::tie(rhs.Major, rhs.Minor, rhs.Revision, rhs.ModLevel);
}

bool operator !=(ProjectFormatVersion lhs, ProjectFormatVersion rhs) noexcept
{
    return !(lhs == rhs);
}

bool operator <(ProjectFormatVersion lhs, ProjectFormatVersion rhs) noexcept
{
    return std::tie(lhs.Major, lhs.Minor, lhs.Revision, lhs.ModLevel)
           < std::tie(rhs.Major, rhs.Minor, rhs.Revision, rhs.ModLevel);
}

ProjectFormatVersion ProjectFormatVersion::FromPacked(uint32_t packedVersion) noexcept
{
    return {
        static_cast<uint8_t>((packedVersion >> 24) & 0xFF),
        static_cast<uint8_t>((packedVersion >> 16) & 0xFF),
        static_cast<uint8_t>((packedVersion >> 8) & 0xFF),
        static_cast<uint8_t>((packedVersion) & 0xFF),
    };
}

uint32_t ProjectFormatVersion::GetPacked() const noexcept
{
    return (Major << 24) | (Minor << 16) | (Revision << 8) | ModLevel;
}

bool ProjectFormatVersion::IsValid() const noexcept
{
    return Major != 0;
}

const ProjectFormatVersion SupportedProjectFormatVersion = {
    AUDACITY_VERSION, AUDACITY_RELEASE, AUDACITY_REVISION, AUDACITY_MODLEVEL
};

const ProjectFormatVersion BaseProjectFormatVersion = { AUDACITY_VERSION, AUDACITY_RELEASE, 0, 0 };
