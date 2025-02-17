/* SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportExport.h

  Vitaly Sverchinsky

**********************************************************************/

#pragma once

#include "ClientData.h"

class AudacityProject;

class IMPORT_EXPORT_API ImportExport final : public ClientData::Base
{
public:
    static constexpr double InvalidRate = 0;

    static ImportExport& Get(AudacityProject& project);
    static const ImportExport& Get(const AudacityProject& project);

    ImportExport();
    ImportExport(const ImportExport&) = delete;
    ImportExport& operator=(const ImportExport&) = delete;

    double GetPreferredExportRate() const;
    void SetPreferredExportRate(double rate);

private:
    double mExportRate{ InvalidRate };
};
