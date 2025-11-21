/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  UserService.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <wx/string.h>

#include "Observer.h"

namespace audacity::cloud::audiocom {
//! Message that is sent when user data changes.
//! This message is always sent on UI thread.
struct CLOUD_AUDIOCOM_API UserDataChanged final
{
};

//! Service for providing information about the user profile
class CLOUD_AUDIOCOM_API UserService final : public Observer::Publisher <UserDataChanged>
{
public:
    //! Request the service to update the data
    void UpdateUserData();
    //! Reset the user profile data
    void ClearUserData();

    //! Gets user id
    wxString GetUserId() const;
    //! "Slug" used to construct shareable URLs
    wxString GetUserSlug() const;
    //! Get the user name to display in the dialog
    wxString GetDisplayName() const;
    //! Gets a path to the avatar
    wxString GetAvatarPath() const;

private:
    void DownloadAvatar(std::string_view url);
}; // class UserService

CLOUD_AUDIOCOM_API UserService& GetUserService();
} // namespace audacity::cloud::audiocom
