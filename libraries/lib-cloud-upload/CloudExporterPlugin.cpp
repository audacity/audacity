#include "CloudExporterPlugin.h"
/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudExporterPlugin.cpp

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "CloudExporterPlugin.h"

namespace cloud
{
CloudExporterPlugin::~CloudExporterPlugin() noexcept
{
}
void CloudExporterPlugin::OnAfterExport()
{
}
} // namespace cloud
