/*!********************************************************************
 Audacity: A Digital Audio Editor

 @file Audacity40PromoDialog.cpp
 @brief Promotional dialog for Audacity 4.0 release.

 **********************************************************************/

#include "Audacity40PromoDialog.h"

#include "HyperLink.h"
#include "UpdateFeedMigrationParser.h"

#include "NetworkManager.h"
#include "IResponse.h"
#include "Request.h"

#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/statbmp.h>
#include <wx/button.h>
#include <wx/utils.h>
#include <wx/image.h>
#include <wx/mstream.h>
#include <wx/log.h>
#include <wx/filename.h>
#include <wx/stdpaths.h>
#include <wx/platinfo.h>
#include <wx/app.h>

#include "../../images/Audacity40Promo.h"

namespace
{
    constexpr auto FeatureBreakdownLink = "https://www.audacityteam.org/au4/";
    StringSetting perfAudioComUserId { wxT("/cloud/audiocom/userId"), wxEmptyString };

    wxBitmap LoadEmbeddedPNG(const unsigned char* data, size_t len)
    {
        wxMemoryInputStream stream(data, len);
        wxImage image;
        if (!image.LoadFile(stream, wxBITMAP_TYPE_PNG))
        {
            wxLogError("Failed to load embedded PNG image.");
            return wxBitmap();
        }
        return wxBitmap(image);
    }
}

Audacity40PromoDialog::Audacity40PromoDialog(wxWindow* parent, const Notification& notification)
    : wxDialogWrapper(parent, wxID_ANY, XO("Install Audacity 4"),
                     wxDefaultPosition, wxDefaultSize,
                     wxDEFAULT_DIALOG_STYLE)
{
    auto mainSizer = safenew wxBoxSizer(wxVERTICAL);

    auto descText = safenew wxStaticText(this, wxID_ANY,
        _("Audacity 4 is here! Packed with hundreds of upgrades that make it faster and\neasier to use than ever."));
    mainSizer->Add(descText, 0, wxALL, 15);

    wxBitmap bitmap = LoadEmbeddedPNG(Audacity_4_0_Promo_png, Audacity_4_0_Promo_png_len);
    if (bitmap.IsOk())
    {
        auto bitmapCtrl = safenew wxStaticBitmap(this, wxID_ANY, bitmap);
        mainSizer->Add(bitmapCtrl, 0, wxLEFT | wxRIGHT, 15);
        mainSizer->AddSpacer(20);
    }

    auto footerText = safenew wxStaticText(this, wxID_ANY,
        _("Installation does not affect Audacity 3, so your existing setup remains untouched."));
    mainSizer->Add(footerText, 0, wxLEFT | wxRIGHT, 15);

    mainSizer->AddSpacer(5);
    auto link = safenew HyperLink(this, wxID_ANY,
        _("Discover what's new in our full feature breakdown."),
        FeatureBreakdownLink);
    mainSizer->Add(link, 0, wxLEFT | wxRIGHT, 15);

    mainSizer->AddSpacer(40);

    auto buttonSizer = safenew wxBoxSizer(wxHORIZONTAL);
    buttonSizer->AddStretchSpacer();

    auto noThanksBtn = safenew wxButton(this, wxID_CANCEL, _("No thanks"));
    buttonSizer->Add(noThanksBtn, 0, wxRIGHT, 5);

    auto remindLaterBtn = safenew wxButton(this, wxID_APPLY, _("Remind me later"));
    buttonSizer->Add(remindLaterBtn, 0, wxRIGHT, 5);

    auto installBtn = safenew wxButton(this, wxID_OK, _("Install Audacity 4"));
    installBtn->SetDefault();
    buttonSizer->Add(installBtn, 0);

    mainSizer->Add(buttonSizer, 0, wxEXPAND | wxLEFT | wxRIGHT | wxBOTTOM, 15);

    SetSizerAndFit(mainSizer);
    Center();

    Bind(wxEVT_BUTTON, [this](wxCommandEvent& evt) {
        if (evt.GetId() == wxID_OK)
        {
            OnInstallClicked();
            return;
        }
        EndModal(evt.GetId());
    });
}

Audacity40PromoDialog::~Audacity40PromoDialog()
{
    if (mInstallerFile.is_open())
        mInstallerFile.close();

    if (mCurrentResponse)
        mCurrentResponse->abort();
}

std::string Audacity40PromoDialog::GetUpdatesUrl() const
{
   #if AUDACITY_BUILD_LEVEL == 0
      std::string url = "https://updates.audacityteam.org/builds/alpha.json";
   #elif AUDACITY_BUILD_LEVEL == 1
      std::string url = "https://updates.audacityteam.org/builds/beta.json";
   #else
      std::string url = "https://updates.audacityteam.org/feed/latest.json";
   #endif

   if (SendAnonymousUsageInfo->Read())
   {
      url += "?audacity-instance-id=" + InstanceId->Read().ToStdString();

      if (!perfAudioComUserId.Read().IsEmpty())
      {
         url += "&user_id=" + perfAudioComUserId.Read().ToStdString();
      }
   }

   return url;
}

void Audacity40PromoDialog::OnInstallClicked()
{
    FetchReleaseInfo();
}

void Audacity40PromoDialog::FetchReleaseInfo()
{
    using namespace audacity::network_manager;

    const Request request(GetUpdatesUrl());
    mCurrentResponse = NetworkManager::GetInstance().doGet(request);

    mCurrentResponse->setRequestFinishedCallback([this](IResponse* response) {
        if (response->getError() != NetworkError::NoError)
        {
            BasicUI::CallAfter([this] {
                wxLogError("Failed to fetch Audacity 4 release info");
                BasicUI::ShowErrorDialog({},
                    XO("Error"),
                    XO("Unable to fetch Audacity 4 release information. Please try again later."),
                    wxString(),
                    BasicUI::ErrorDialogOptions{ BasicUI::ErrorDialogType::ModalError });
                EndModal(wxID_CANCEL);
            });
            return;
        }

        std::string jsonData = response->readAll<std::string>();
        UpdateFeedMigrationParser parser;

        if (!parser.ParseRelease(jsonData, mReleaseInfo) || !mReleaseInfo.IsValid())
        {
            BasicUI::CallAfter([this] {
                wxLogError("Failed to parse Audacity 4 release info");
                BasicUI::ShowErrorDialog({},
                    XO("Error"),
                    XO("Unable to parse Audacity 4 release information. Please try again later."),
                    wxString(),
                    BasicUI::ErrorDialogOptions{ BasicUI::ErrorDialogType::ModalError });
                EndModal(wxID_CANCEL);
            });
            return;
        }

        BasicUI::CallAfter([this] {
            StartDownload();
        });
    });
}

void Audacity40PromoDialog::StartDownload()
{
    using namespace audacity::network_manager;

    if (mReleaseInfo.fileUrl.empty())
    {
        wxLogError("No download URL available for current platform");
        BasicUI::ShowErrorDialog({},
            XO("Error"),
            XO("No Audacity 4 installer available for your platform."),
            wxString(),
            BasicUI::ErrorDialogOptions{ BasicUI::ErrorDialogType::ModalError });
        EndModal(wxID_CANCEL);
        return;
    }

    wxString installerExtension;
    const wxPlatformInfo& info = wxPlatformInfo::Get();
    if (info.GetOperatingSystemId() & wxOS_WINDOWS)
        installerExtension = "msi";
    else if (info.GetOperatingSystemId() & wxOS_MAC)
        installerExtension = "dmg";
    else
    {
        wxFileName fn(mReleaseInfo.fileName);
        installerExtension = fn.GetExt();
    }

    wxString filename = wxFileName(mReleaseInfo.fileName).GetName();

    mInstallerPath = wxFileName(
        wxStandardPaths::Get().GetUserDir(wxStandardPaths::Dir_Downloads),
        filename, installerExtension)
        .GetFullPath()
        .ToStdString();

    mInstallerFile.open(mInstallerPath, std::ios::binary);
    if (!mInstallerFile.is_open())
    {
        wxLogError("Failed to create installer file: %s", mInstallerPath);
        BasicUI::ShowErrorDialog({},
            XO("Error"),
            XO("Unable to create installer file. Please check write permissions."),
            wxString(),
            BasicUI::ErrorDialogOptions{ BasicUI::ErrorDialogType::ModalError });
        EndModal(wxID_CANCEL);
        return;
    }

    mProgressDialog = BasicUI::MakeProgress(XO("Audacity update"), XO("Downloading %s").Format(filename));
    wxASSERT(mProgressDialog);

    const Request downloadRequest(mReleaseInfo.fileUrl);
    mCurrentResponse = NetworkManager::GetInstance().doGet(downloadRequest);

    mCurrentResponse->setDownloadProgressCallback(
        [this](int64_t current, int64_t expected) {
            CallAfter([this, current, expected] {
                if (mProgressDialog)
                    mProgressDialog->Poll(current, expected);
            });
        });

    mCurrentResponse->setOnDataReceivedCallback(
        [this](IResponse* response) {
            if (response->getError() == NetworkError::NoError)
            {
                std::vector<char> buffer(response->getBytesAvailable());
                size_t bytes = response->readData(buffer.data(), buffer.size());

                if (mInstallerFile.is_open())
                    mInstallerFile.write(buffer.data(), bytes);
            }
        });

    mCurrentResponse->setRequestFinishedCallback(
        [this](IResponse* response) {
            bool success = (response->getError() == NetworkError::NoError);

            CallAfter([this, success] {
                OnDownloadFinished(success);
            });
        });
}

void Audacity40PromoDialog::OnDownloadFinished(bool success)
{
    mProgressDialog.reset();

    if (mInstallerFile.is_open())
        mInstallerFile.close();

    if (!success)
    {
        wxLogError("Failed to download Audacity 4 installer");
        BasicUI::ShowErrorDialog({},
            XO("Download Error"),
            XO("Failed to download Audacity 4 installer. Please try again later."),
            wxString(),
            BasicUI::ErrorDialogOptions{ BasicUI::ErrorDialogType::ModalError });
        EndModal(wxID_CANCEL);
        return;
    }

    LaunchInstaller();
    EndModal(wxID_OK);
}

void Audacity40PromoDialog::LaunchInstaller()
{
    const wxPlatformInfo& info = wxPlatformInfo::Get();

    if (!wxFileName(mInstallerPath).Exists())
    {
        wxLogError("Installer file not found: %s", mInstallerPath);
        return;
    }

    if (info.GetOperatingSystemId() & wxOS_WINDOWS)
    {
        wxFileName fn(mInstallerPath);
        if (fn.GetExt().Lower() == "msi")
        {
            // MSI files need to be launched with msiexec
            wxExecute("msiexec /i \"" + mInstallerPath + "\"", wxEXEC_ASYNC);
        }
        else
        {
            wxExecute("\"" + mInstallerPath + "\"", wxEXEC_ASYNC);
        }
    }
    else if (info.GetOperatingSystemId() & wxOS_MAC)
    {
        wxExecute("open \"" + mInstallerPath + "\"", wxEXEC_ASYNC);
    }
    else
    {
        // Linux - make executable and run
        wxExecute("chmod +x \"" + mInstallerPath + "\"", wxEXEC_SYNC);
        wxExecute("\"" + mInstallerPath + "\"", wxEXEC_ASYNC);
    }
}
