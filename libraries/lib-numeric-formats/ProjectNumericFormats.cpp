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
   [](AudacityProject &project)
   {
      return std::make_shared<ProjectNumericFormats>(project);
   }
};

ProjectNumericFormats &ProjectNumericFormats::Get(AudacityProject &project)
{
   return project.AttachedObjects::Get<ProjectNumericFormats&>(key);
}

const ProjectNumericFormats &ProjectNumericFormats::Get(
   const AudacityProject &project)
{
   return Get(const_cast<AudacityProject &>(project));
}

ProjectNumericFormats::ProjectNumericFormats(const AudacityProject& project)
   : mProject { project }
   , mSelectionFormat{ NumericConverterFormats::Lookup(
      FormatterContext::ProjectContext(project),
      NumericConverterType_TIME(),
      gPrefs->Read(wxT("/SelectionFormat"), wxT("")))
   }
   , mFrequencySelectionFormatName{ NumericConverterFormats::Lookup(
      FormatterContext::ProjectContext(project),
      NumericConverterType_FREQUENCY(),
      gPrefs->Read(wxT("/FrequencySelectionFormatName"), wxT("")) )
   }
   , mBandwidthSelectionFormatName{ NumericConverterFormats::Lookup(
      FormatterContext::ProjectContext(project),
      NumericConverterType_BANDWIDTH(),
      gPrefs->Read(wxT("/BandwidthSelectionFormatName"), wxT("")) )
   }
   , mAudioTimeFormat{ NumericConverterFormats::Lookup(
      FormatterContext::ProjectContext(project),
      NumericConverterType_TIME(),
      gPrefs->Read(wxT("/AudioTimeFormat"), wxT("hh:mm:ss")))
   }
{}

ProjectNumericFormats::~ProjectNumericFormats() = default;

const NumericFormatSymbol &
ProjectNumericFormats::GetFrequencySelectionFormatName() const
{
   return mFrequencySelectionFormatName;
}

void ProjectNumericFormats::SetFrequencySelectionFormatName(
   const NumericFormatSymbol & formatName)
{
   mFrequencySelectionFormatName = formatName;
}

const NumericFormatSymbol &
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
   const NumericFormatSymbol & formatName)
{
   mBandwidthSelectionFormatName = formatName;
}

void ProjectNumericFormats::SetSelectionFormat(
   const NumericFormatSymbol & format)
{
   mSelectionFormat = format;
}

const NumericFormatSymbol & ProjectNumericFormats::GetSelectionFormat() const
{
   return mSelectionFormat;
}

void ProjectNumericFormats::SetAudioTimeFormat(const NumericFormatSymbol & format)
{
   mAudioTimeFormat = format;
}

const NumericFormatSymbol & ProjectNumericFormats::GetAudioTimeFormat() const
{
   return mAudioTimeFormat;
}

static ProjectFileIORegistry::AttributeWriterEntry entry {
[](const AudacityProject &project, XMLWriter &xmlFile){
   auto &formats = ProjectNumericFormats::Get(project);
   xmlFile.WriteAttr(wxT("selectionformat"),
                     formats.GetSelectionFormat().Internal());
   xmlFile.WriteAttr(wxT("frequencyformat"),
                     formats.GetFrequencySelectionFormatName().Internal());
   xmlFile.WriteAttr(wxT("bandwidthformat"),
                     formats.GetBandwidthSelectionFormatName().Internal());
}
};

static ProjectFileIORegistry::AttributeReaderEntries entries {
// Just a pointer to function, but needing overload resolution as non-const:
(ProjectNumericFormats& (*)(AudacityProject &)) &ProjectNumericFormats::Get, {
   // PRL:  The following have persisted as per-project settings for long.
   // Maybe that should be abandoned.  Enough to save changes in the user
   // preference file.
   { "selectionformat", [](auto &formats, auto value){
      formats.SetSelectionFormat(formats.LookupFormat(
              NumericConverterType_TIME(), value.ToWString()));
   } },
   { "frequencyformat", [](auto &formats, auto value){
           formats.SetFrequencySelectionFormatName(formats.LookupFormat(
              NumericConverterType_FREQUENCY(), value.ToWString()));
   } },
   { "bandwidthformat", [](auto &formats, auto value){
           formats.SetBandwidthSelectionFormatName(formats.LookupFormat(
              NumericConverterType_BANDWIDTH(), value.ToWString()));
   } },
} };
