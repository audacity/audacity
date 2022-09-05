/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudExportersRegistry.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <functional>
#include <memory>

#include "MimeTypesList.h"

class AudacityProject;

namespace cloud
{
class CloudExporterPlugin;

using CloudExporterPluginFactory = std::function<std::unique_ptr<CloudExporterPlugin>(const AudacityProject&)>;

//! Registers a factory for a specific mime type.
CLOUD_UPLOAD_API bool RegisterCloudExporter(MimeType mimeType, CloudExporterPluginFactory factory);

//! Returns a best matching exporter provided a list of mime types. The list is
//! ordered by priority.
CLOUD_UPLOAD_API std::unique_ptr<CloudExporterPlugin>
CreatePreferredExporter(const MimeTypesList& mimeTypes, const AudacityProject& project);

} // namespace cloud
