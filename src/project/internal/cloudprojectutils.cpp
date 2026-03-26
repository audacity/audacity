/*
* Audacity: A Digital Audio Editor
*/

#include "cloudprojectutils.h"

#include "framework/global/translation.h"

#include "au3cloud/au3clouderrors.h"

namespace {
const char* PRJ_SIZE_EXCEEDED_TITLE = "Project size limit exceeded";
const char* PRJ_SIZE_EXCEEDED_TEXT
    = "Your project exceeds the maximum size of 10GB. Please consider reducing its size or save it to your computer";

const char* PRJ_LIMIT_REACHED_TITLE = "Your project limit has been reached";
const char* PRJ_LIMIT_REACHED_TEXT
    =
        "You have used up all of your available projects. Visit the project page on audio.com to make room for new creations.\n\nYou can also save this project on your computer to avoid losing changes.";

const char* FILE_SYNC_ISSUE_TITLE = "We encountered an issue syncing your file";
const char* FILE_SYNC_ISSUE_TEXT
    =
        "Don't worry, your changes will be saved to a temporary location and will be synchronised to your cloud copy when your internet connection resumes.";

const char* CLOUD_SAVE_UNAVAILABLE_TITLE = "Cloud save unavailable";
const char* CLOUD_SAVE_UNAVAILABLE_TEXT
    =
        "Your project is no longer linked to its previous cloud save. This may happen if the cloud version was deleted.\n\nSave to audio.com to re-upload this project to the cloud.";

const char* DEFAULT_CLOUD_ERROR_TITLE = "Cloud error";
const char* DEFAULT_CLOUD_ERROR_TEXT = "An error occurred while syncing with the cloud. Please try again later.";
}

namespace au::project {
std::string cloudErrorTitle(int code)
{
    using Err = au::au3cloud::Err;
    switch (static_cast<Err>(code)) {
    case Err::ProjectLimitReached:
        return muse::trc("cloud", PRJ_LIMIT_REACHED_TITLE);
    case Err::ProjectStorageLimitReached:
        return muse::trc("cloud", PRJ_SIZE_EXCEEDED_TITLE);
    case Err::ProjectNotFound:
        return muse::trc("cloud", CLOUD_SAVE_UNAVAILABLE_TITLE);
    default:
        return muse::trc("cloud", DEFAULT_CLOUD_ERROR_TITLE);
    }
}

std::string cloudErrorMessage(int code)
{
    using Err = au::au3cloud::Err;
    switch (static_cast<Err>(code)) {
    case Err::ProjectLimitReached:
        return muse::trc("cloud", PRJ_LIMIT_REACHED_TEXT);
    case Err::ProjectStorageLimitReached:
        return muse::trc("cloud", PRJ_SIZE_EXCEEDED_TEXT);
    case Err::ProjectNotFound:
        return muse::trc("cloud", CLOUD_SAVE_UNAVAILABLE_TEXT);
    default:
        return muse::trc("cloud", DEFAULT_CLOUD_ERROR_TEXT);
    }
}
}
