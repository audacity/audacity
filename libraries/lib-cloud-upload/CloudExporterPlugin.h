/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudExporterPlugin.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <wx/string.h>

#include "Identifier.h"

namespace cloud
{
//! Helper interface, that allows to setup the desired export format on the ExportPlugin.
/*!
 * Audacity exporting framework relies heavily on using the global state.
 * This interface attempts to overcome this problem by allowing to setup the desired
 * export format on the ExportPlugin before exporting.
 *
 * It is expected, that exporting happens inside a SettingScope, however a plugin may
 * perform additional clean ups after exporting.
 */
class CLOUD_UPLOAD_API CloudExporterPlugin /* not final */
{
public:
   virtual ~CloudExporterPlugin() noexcept;

   //! Identifier of the ExportPlugin to be used
   virtual wxString GetExporterID() const = 0;
   //! File extension that is expected with this plugin
   virtual FileExtension GetFileExtension() const = 0;
   //! Setup the preferred format for the export
   virtual void OnBeforeExport() = 0;
   //! Cleanup after the exporting is done, if needed
   virtual void OnAfterExport();

}; // class CloudExporterPlugin
} // namespace cloud
