/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudSettings.cpp

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "CloudSettings.h"

#include "FileNames.h"

namespace cloud::audiocom
{
StringSetting CloudProjectsSavePath { "/cloud/audiocom/CloudProjectsSavePath",
                                      [] {
                                         return FileNames::DataDir() + "/cloud";
                                      } };
} // namespace cloud::audiocom
