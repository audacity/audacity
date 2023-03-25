/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ProjectSnap.cpp

  Dmitry Vedenko

**********************************************************************/
#include "ProjectSnap.h"
#include "Project.h"
#include "XMLWriter.h"
#include "XMLAttributeValueView.h"

#include "ProjectRate.h"
#include "ProjectTimeSignature.h"

static const AudacityProject::AttachedObjects::RegisteredFactory sKey {
   [](AudacityProject& project)
   {
      auto result = std::make_shared<ProjectSnap>(project);
      return result;
   }
};

ProjectSnap& ProjectSnap::Get(AudacityProject& project)
{
   return project.AttachedObjects::Get<ProjectSnap>(sKey);
}

const ProjectSnap& ProjectSnap::Get(const AudacityProject& project)
{
   return Get(const_cast<AudacityProject&>(project));
}

ProjectSnap::ProjectSnap(const AudacityProject& project)
    : mProject(project)
{}

void ProjectSnap::SetSnapMode(SnapMode mode)
{
   if (mSnapMode != mode)
   {
      mSnapMode = mode;
      
      SnapModeSetting.WriteEnum(mSnapMode);
      gPrefs->Flush();

      Publish(SnapChangedMessage { mode, mSnapTo });
   }
}

SnapMode ProjectSnap::GetSnapMode() const
{
   return mSnapMode;
}

void ProjectSnap::SetSnapTo(Identifier snap)
{
   if (mSnapTo != snap)
   {
      mSnapTo = snap;

      SnapToSetting.Write(mSnapTo.GET());
      gPrefs->Flush();

      Publish(SnapChangedMessage { mSnapMode, snap });
   }
}

Identifier ProjectSnap::GetSnapTo() const
{
   return mSnapTo;
}

SnapResult ProjectSnap::SnapTime(double time) const
{
   if (mSnapMode == SnapMode::SNAP_OFF)
      return { time, false };

   auto& timeSignature = ProjectTimeSignature::Get(mProject);
   
   const SnapConfig config { ProjectRate::Get(mProject).GetRate(),
                       timeSignature.GetTempo(),
                       { timeSignature.GetUpperTimeSignature(),
                         timeSignature.GetLowerTimeSignature() } };
   
   return SnapFunctionsRegistry::Snap(mSnapTo, config, time, mSnapMode == SnapMode::SNAP_NEAREST);
}

static ProjectFileIORegistry::AttributeWriterEntry entry {
   [](const AudacityProject& project, XMLWriter& xmlFile)
   {
      // Keep this attr for compatibility
      auto& snapSettings = ProjectSnap::Get(project);
      xmlFile.WriteAttr(
         wxT("snapto"), snapSettings.GetSnapMode() != SnapMode::SNAP_OFF ?
                           wxT("on") :
                           wxT("off"));
   }
};

static ProjectFileIORegistry::AttributeReaderEntries entries {
   // Just a pointer to function, but needing overload resolution as non-const:
   (ProjectSnap & (*)(AudacityProject&)) & ProjectSnap::Get,
   {
      // PRL:  The following has persisted as a per-project setting for long.
      // Maybe that should be abandoned.  Enough to save changes in the user
      // preference file.
      // DV: Keep the old behavior here: project only stored *off* and *nearest*
      { "snapto",
        [](auto& snapSettings, auto value)
        {
           snapSettings.SetSnapMode(
              value.ToWString() == wxT("on") ? SnapMode::SNAP_NEAREST :
                                               SnapMode::SNAP_OFF);
        } },
   }
};
