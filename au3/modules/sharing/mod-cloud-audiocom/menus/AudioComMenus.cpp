/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  AudioComMenus.cpp

  Dmitry Vedenko

**********************************************************************/

#include "BasicUI.h"
#include "CloudProjectFileIOExtensions.h"
#include "CloudProjectMixdownUtils.h"
#include "CommandContext.h"
#include "ExportUtils.h"
#include "MenuRegistry.h"

#include "sync/MixdownUploader.h"
#include "sync/ProjectCloudExtension.h"

#include "ProjectWindow.h"

#include "ui/dialogs/ProjectsListDialog.h"
#include "ui/dialogs/ShareAudioDialog.h"

#include "CommonCommandFlags.h"

namespace {
using namespace audacity::cloud::audiocom;
using namespace audacity::cloud::audiocom::sync;

void OnSaveToCloud(const CommandContext& context)
{
    SaveToCloud(context.project, UploadMode::Normal);
}

void OnOpenFromCloud(const CommandContext& context)
{
    ProjectsListDialog dialog { ProjectWindow::Find(&context.project),
                                &context.project };

    dialog.ShowModal();
}

void OnUpdateMixdown(const CommandContext& context)
{
    UploadMixdown(
        context.project,
        [](auto& project, auto state)
    {
        if (state != MixdownState::Succeeded) {
            return;
        }

        auto& projectCloudExtension = ProjectCloudExtension::Get(project);

        BasicUI::OpenInDefaultBrowser(
            projectCloudExtension.GetCloudProjectPage(
                AudiocomTrace::UpdateCloudAudioPreviewMenu));
    });
}

void OnShareAudio(const CommandContext& context)
{
    ShareAudioDialog dialog {
        context.project,
        AudiocomTrace::ShareAudioMenu,
        ProjectWindow::Find(&context.project),
    };

    dialog.ShowModal();
}

const ReservedCommandFlag& IsCloudProjectFlag()
{
    static ReservedCommandFlag flag {
        [](const AudacityProject& project)
        { return ProjectCloudExtension::Get(project).IsCloudProject(); },
        CommandFlagOptions { [](const TranslatableString&) {
                return XO("Previews can be updated only for Cloud projects");
            } }.QuickTest()
        .Priority(1)
    };
    return flag;
}

using namespace MenuRegistry;

AttachedItem sSaveAttachment { Command(
                                   wxT("SaveToCloud"), XXO("Save &To Cloud..."),
                                   OnSaveToCloud, AlwaysEnabledFlag),
                               wxT("File/Save") };

AttachedItem sMixdownAttachment { Command(
                                      wxT("UpdateMixdown"),
                                      XXO("&Update Cloud Audio Preview"),
                                      OnUpdateMixdown, IsCloudProjectFlag()),
                                  wxT("File/Save") };

AttachedItem sOpenAttachment { Command(
                                   wxT("OpenFromCloud"),
                                   XXO("Open Fro&m Cloud..."), OnOpenFromCloud,
                                   AlwaysEnabledFlag),
                               wxT("File/Basic") };

AttachedItem sShareAttachment { Command(
                                    wxT("ShareAudio"), XXO("S&hare Audio..."),
                                    OnShareAudio, WaveTracksExistFlag()),
                                wxT("File/Import-Export") };
} // namespace
