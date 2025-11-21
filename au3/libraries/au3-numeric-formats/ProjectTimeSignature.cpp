/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

 Audacity: A Digital Audio Editor

 @file ProjectTimeSignature.cpp

 Dmitry Vedenko

 **********************************************************************/
#include "ProjectTimeSignature.h"

#include "Beats.h"
#include "Project.h"
#include "XMLAttributeValueView.h"
#include "XMLWriter.h"

static const AttachedProjectObjects::RegisteredFactory key
{
    [](AudacityProject& project)
    { return std::make_shared<ProjectTimeSignature>(); }
};

ProjectTimeSignature& ProjectTimeSignature::Get(AudacityProject& project)
{
    return project.AttachedObjects::Get<ProjectTimeSignature&>(key);
}

const ProjectTimeSignature&
ProjectTimeSignature::Get(const AudacityProject& project)
{
    return Get(const_cast<AudacityProject&>(project));
}

ProjectTimeSignature::ProjectTimeSignature()
    : mTempo{BeatsPerMinute.Read()}
    , mUpperTimeSignature{UpperTimeSignature.Read()}
    , mLowerTimeSignature{LowerTimeSignature.Read()}
{}

ProjectTimeSignature::~ProjectTimeSignature() = default;

double ProjectTimeSignature::GetTempo() const
{
    return mTempo;
}

void ProjectTimeSignature::SetTempo(double tempo)
{
    if (mTempo != tempo) {
        mTempo = tempo;

        BeatsPerMinute.Write(tempo);
        gPrefs->Flush();

        PublishSignatureChange();
    }
}

int ProjectTimeSignature::GetUpperTimeSignature() const
{
    return mUpperTimeSignature;
}

void ProjectTimeSignature::SetUpperTimeSignature(int upperTimeSignature)
{
    if (mUpperTimeSignature != upperTimeSignature) {
        mUpperTimeSignature = upperTimeSignature;

        UpperTimeSignature.Write(upperTimeSignature);
        gPrefs->Flush();

        PublishSignatureChange();
    }
}

int ProjectTimeSignature::GetLowerTimeSignature() const
{
    return mLowerTimeSignature;
}

void ProjectTimeSignature::SetLowerTimeSignature(int lowerTimeSignature)
{
    if (mLowerTimeSignature != lowerTimeSignature) {
        mLowerTimeSignature = lowerTimeSignature;

        LowerTimeSignature.Write(lowerTimeSignature);
        gPrefs->Flush();

        PublishSignatureChange();
    }
}

double ProjectTimeSignature::GetQuarterDuration() const
{
    return 60.0 / mTempo;
}

double ProjectTimeSignature::GetBeatDuration() const
{
    return GetQuarterDuration() * 4.0 / mLowerTimeSignature;
}

double ProjectTimeSignature::GetBarDuration() const
{
    return GetBeatDuration() * mUpperTimeSignature;
}

void ProjectTimeSignature::PublishSignatureChange()
{
    Publish(TimeSignatureChangedMessage { mTempo, mUpperTimeSignature,
                                          mLowerTimeSignature });
}

static ProjectFileIORegistry::AttributeWriterEntry entry {
    [](const AudacityProject& project, XMLWriter& xmlFile){
        auto& formats = ProjectTimeSignature::Get(project);
        xmlFile.WriteAttr(wxT("time_signature_tempo"), formats.GetTempo());
        xmlFile.WriteAttr(wxT("time_signature_upper"), formats.GetUpperTimeSignature());
        xmlFile.WriteAttr(wxT("time_signature_lower"), formats.GetLowerTimeSignature());
    }
};

static ProjectFileIORegistry::AttributeReaderEntries entries {
    // Just a pointer to function, but needing overload resolution as non-const:
    (ProjectTimeSignature & (*)(AudacityProject&)) & ProjectTimeSignature::Get,
    {
        { "time_signature_tempo", [](auto& signature, auto value)
            { signature.SetTempo(value.Get(BeatsPerMinute.Read())); } },
        { "time_signature_upper", [](auto& signature, auto value)
            { signature.SetUpperTimeSignature(value.Get(UpperTimeSignature.Read())); } },
        { "time_signature_lower", [](auto& signature, auto value)
            { signature.SetLowerTimeSignature(value.Get(LowerTimeSignature.Read())); } },
    }
};
