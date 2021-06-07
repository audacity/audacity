#pragma once

#include "VersionId.h"

struct VersionPatch final
{
    using UpdateDataFormat = std::string;

    VersionPatch() = default;	

    VersionId version;
    wxArrayString changelog;
    wxString download;
};
