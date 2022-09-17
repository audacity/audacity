/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  MimeTypesList.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <string>
#include <vector>

namespace cloud
{
using MimeType = std::string;
//! Ordered list of mime types.
using MimeTypesList = std::vector<std::string>;
} // namespace cloud
