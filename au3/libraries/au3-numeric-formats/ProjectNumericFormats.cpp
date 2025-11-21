/**********************************************************************

 Audacity: A Digital Audio Editor

 @file ProjectNumericFormats.cpp

 Paul Licameli split from ProjectNumericFormats.cpp

 **********************************************************************/
#include "ProjectNumericFormats.h"
#include "Prefs.h"
#include "Project.h"

#include "NumericConverterFormats.h"
#include "NumericConverterFormatterContext.h"

#include "XMLAttributeValueView.h"
#include "XMLWriter.h"

static const AttachedProjectObjects::RegisteredFactory key
{
    [](AudacityProject& project)
    {
        return std::make_shared<ProjectNumericFormats>(project);
    }
};

ProjectNumericFormats& ProjectNumericFormats::Get(AudacityProject& project)
{
    return project.AttachedObjects::Get<ProjectNumericFormats&>(key);
}

const ProjectNumericFormats& ProjectNumericFormats::Get(
    const AudacityProject& project)
{
    return Get(const_cast<AudacityProject&>(project));
}

ProjectNumericFormats::ProjectNumericFormats(const AudacityProject& project)
    : mProject{project}
    , mSelectionFormat{
                       gPrefs->Read(wxT("/SelectionFormat"), wxT(""))
                       }
    , mFrequencySelectionFormatName{
                                    gPrefs->Read(wxT("/FrequencySelectionFormatName"), wxT(""))
                                    }
    , mBandwidthSelectionFormatName{
                                    gPrefs->Read(wxT("/BandwidthSelectionFormatName"), wxT(""))
                                    }
    , mAudioTimeFormat{
                       gPrefs->Read(wxT("/AudioTimeFormat"), wxT("hh:mm:ss"))
                       }
{}

ProjectNumericFormats::~ProjectNumericFormats() = default;

NumericFormatID
ProjectNumericFormats::GetFrequencySelectionFormatName() const
{
    return mFrequencySelectionFormatName;
}

void ProjectNumericFormats::SetFrequencySelectionFormatName(
    const NumericFormatID& formatName)
{
    if (mFrequencySelectionFormatName != formatName) {
        ProjectNumericFormatsEvent e{
            ProjectNumericFormatsEvent::ChangedFrequencyFormat,
            mFrequencySelectionFormatName, formatName
        };
        mFrequencySelectionFormatName = formatName;
        Publish(e);
    }
}

NumericFormatID
ProjectNumericFormats::GetBandwidthSelectionFormatName() const
{
    return mBandwidthSelectionFormatName;
}

NumericFormatSymbol ProjectNumericFormats::LookupFormat(
    const NumericConverterType& type, const wxString& identifier)
{
    return NumericConverterFormats::Lookup(
        FormatterContext::ProjectContext(mProject), type, identifier);
}

void ProjectNumericFormats::SetBandwidthSelectionFormatName(
    const NumericFormatID& formatName)
{
    if (mBandwidthSelectionFormatName != formatName) {
        ProjectNumericFormatsEvent e{
            ProjectNumericFormatsEvent::ChangedBandwidthFormat,
            mBandwidthSelectionFormatName, formatName
        };
        mBandwidthSelectionFormatName = formatName;
        Publish(e);
    }
}

void ProjectNumericFormats::SetSelectionFormat(const NumericFormatID& format)
{
    if (mSelectionFormat != format) {
        ProjectNumericFormatsEvent e{
            ProjectNumericFormatsEvent::ChangedSelectionFormat,
            mSelectionFormat, format
        };
        mSelectionFormat = format;
        Publish(e);
    }
}

NumericFormatID ProjectNumericFormats::GetSelectionFormat() const
{
    return mSelectionFormat;
}

void ProjectNumericFormats::SetAudioTimeFormat(const NumericFormatID& format)
{
    if (mAudioTimeFormat != format) {
        ProjectNumericFormatsEvent e{
            ProjectNumericFormatsEvent::ChangedAudioTimeFormat,
            mAudioTimeFormat, format
        };
        mAudioTimeFormat = format;
        Publish(e);
    }
}

NumericFormatID ProjectNumericFormats::GetAudioTimeFormat() const
{
    return mAudioTimeFormat;
}

static ProjectFileIORegistry::AttributeWriterEntry entry {
    [](const AudacityProject& project, XMLWriter& xmlFile){
        auto& formats = ProjectNumericFormats::Get(project);
        xmlFile.WriteAttr(wxT("selectionformat"),
                          formats.GetSelectionFormat().GET());
        xmlFile.WriteAttr(wxT("frequencyformat"),
                          formats.GetFrequencySelectionFormatName().GET());
        xmlFile.WriteAttr(wxT("bandwidthformat"),
                          formats.GetBandwidthSelectionFormatName().GET());
    }
};

static ProjectFileIORegistry::AttributeReaderEntries entries {
// Just a pointer to function, but needing overload resolution as non-const:
    (ProjectNumericFormats & (*)(AudacityProject&)) & ProjectNumericFormats::Get, {
        // PRL:  The following have persisted as per-project settings for long.
        // Maybe that should be abandoned.  Enough to save changes in the user
        // preference file.
        { "selectionformat", [](auto& formats, auto value){
                formats.SetSelectionFormat(value.ToWString());
            } },
        { "frequencyformat", [](auto& formats, auto value){
                formats.SetFrequencySelectionFormatName(value.ToWString());
            } },
        { "bandwidthformat", [](auto& formats, auto value){
                formats.SetBandwidthSelectionFormatName(value.ToWString());
            } },
    } };
