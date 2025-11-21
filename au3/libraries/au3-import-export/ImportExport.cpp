/* SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportExport.cpp

  Vitaly Sverchinsky

**********************************************************************/

#include "ImportExport.h"

#include "Project.h"
#include "XMLWriter.h"
#include "XMLAttributeValueView.h"

static const AudacityProject::AttachedObjects::RegisteredFactory
    sKey{
    []( AudacityProject&){
        auto result = std::make_shared< ImportExport >();
        return result;
    }
};

ImportExport& ImportExport::Get(AudacityProject& project)
{
    return project.AttachedObjects::Get<ImportExport>(sKey);
}

const ImportExport& ImportExport::Get(const AudacityProject& project)
{
    return Get(const_cast<AudacityProject&>(project));
}

ImportExport::ImportExport() = default;

double ImportExport::GetPreferredExportRate() const
{
    return mExportRate;
}

void ImportExport::SetPreferredExportRate(double rate)
{
    assert(rate > 0 || rate == InvalidRate);
    if (rate > 0 || rate == InvalidRate) {
        mExportRate = rate;
    }
}

static ProjectFileIORegistry::AttributeWriterEntry entry {
    [](const AudacityProject& project, XMLWriter& xmlFile){
        xmlFile.WriteAttr(wxT("preferred_export_rate"), ImportExport::Get(project).GetPreferredExportRate());
    }
};

static ProjectFileIORegistry::AttributeReaderEntries entries {
// Just a pointer to function, but needing overload resolution as non-const:
    (ImportExport & (*)(AudacityProject&)) & ImportExport::Get, {
        { "preferred_export_rate", [](auto& settings, auto value){
                const double rate = value.Get(settings.GetPreferredExportRate());
                settings.SetPreferredExportRate(rate);
            } }
    } };
