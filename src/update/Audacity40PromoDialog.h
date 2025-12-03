/*!********************************************************************
 Audacity: A Digital Audio Editor

 @file Audacity40PromoDialog.h
 @brief Promotional dialog for Audacity 4.0 release.

 **********************************************************************/
#pragma once

#include "UpdateDataStructures.h"
#include "ReleaseInfo.h"
#include "wxPanelWrapper.h"
#include "BasicUI.h"

#include <wx/dialog.h>

#include <fstream>
#include <memory>

namespace audacity::network_manager
{
class IResponse;
}

/// Custom dialog for Audacity 4.0 release announcement
class Audacity40PromoDialog final : public wxDialogWrapper
{
public:
    Audacity40PromoDialog(wxWindow* parent, const Notification& notification);
    ~Audacity40PromoDialog();

private:
    void OnInstallClicked();
    void FetchReleaseInfo();
    void StartDownload();
    void OnDownloadFinished(bool success);
    void LaunchInstaller();

    std::string GetUpdatesUrl() const;

    ReleaseInfo mReleaseInfo;
    std::unique_ptr<BasicUI::ProgressDialog> mProgressDialog;
    std::string mInstallerPath;
    std::ofstream mInstallerFile;
    std::shared_ptr<audacity::network_manager::IResponse> mCurrentResponse;
};
