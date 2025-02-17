/*!********************************************************************
 Audacity: A Digital Audio Editor

 @file VersionPatch.h
 @brief Declare a structure that describes patch fields.

 Anton Gerasimov
 **********************************************************************/
#pragma once

#include "VersionId.h"

/// A structure that describes patch fields.
struct VersionPatch final
{
    using UpdateDataFormat = std::string;

    VersionPatch() = default;

    VersionId version;
    wxArrayString changelog;
    wxString download;
};
