/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudExportersRegistry.cpp

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "CloudExportersRegistry.h"

#include <unordered_map>

#include "CloudExporterPlugin.h"

namespace cloud
{
namespace
{
std::unordered_map<MimeType, CloudExporterPluginFactory> Exporters;
}

bool RegisterCloudExporter(
   MimeType mimeType, CloudExporterPluginFactory factory)
{
   if (Exporters.end() == Exporters.find(mimeType))
   {
      Exporters.emplace(std::move(mimeType), std::move(factory));
      return true;
   }

   return false;
}

std::unique_ptr<cloud::CloudExporterPlugin> CreatePreferredExporter(
   const MimeTypesList& mimeTypes, const AudacityProject& project)
{
   for (const auto& mimeType : mimeTypes)
   {
      auto it = Exporters.find(mimeType);
      
      if (Exporters.end() != it)
         return it->second(project);
   }

   return {};
}

} // namespace cloud
