/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-Studio-CLA-applies
 *
 * MuseScore Studio
 * Music Composition & Notation
 *
 * Copyright (C) 2022 MuseScore Limited
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#include "opensaveprojectscenario.h"

#include "cloud/clouderrors.h"
#include "engraving/infrastructure/mscio.h"
#include "projecterrors.h"

using namespace muse;
using namespace mu::project;

static constexpr int RET_CODE_CHANGE_SAVE_LOCATION_TYPE = 1234;

RetVal<SaveLocation> OpenSaveProjectScenario::askSaveLocation(INotationProjectPtr project, SaveMode mode,
                                                              SaveLocationType preselectedType) const
{
    SaveLocationType type = preselectedType;

    if (type == SaveLocationType::Undefined) {
        RetVal<SaveLocationType> askedType = saveLocationType();
        if (!askedType.ret) {
            return askedType.ret;
        }

        type = askedType.val;
    }

    IF_ASSERT_FAILED(type != SaveLocationType::Undefined) {
        return make_ret(Ret::Code::UnknownError);
    }

    // The user may switch between Local and Cloud as often as they want
    for (;;) {
        configuration()->setLastUsedSaveLocationType(type);

        switch (type) {
        case SaveLocationType::Undefined:
            return make_ret(Ret::Code::UnknownError);

        case SaveLocationType::Local: {
            RetVal<muse::io::path_t> path = askLocalPath(project, mode);
            switch (path.ret.code()) {
            case int(Ret::Code::Ok): {
                return RetVal<SaveLocation>::make_ok(SaveLocation(path.val));
            }
            case RET_CODE_CHANGE_SAVE_LOCATION_TYPE:
                type = SaveLocationType::Cloud;
                continue;
            default:
                return path.ret;
            }
        }

        case SaveLocationType::Cloud: {
            RetVal<CloudProjectInfo> info = askCloudLocation(project, mode);
            switch (info.ret.code()) {
            case int(Ret::Code::Ok):
                return RetVal<SaveLocation>::make_ok(SaveLocation(info.val));
            case RET_CODE_CHANGE_SAVE_LOCATION_TYPE:
                type = SaveLocationType::Local;
                continue;
            default:
                return info.ret;
            }
        }
        }
    }
}

RetVal<muse::io::path_t> OpenSaveProjectScenario::askLocalPath(INotationProjectPtr project, SaveMode saveMode) const
{
    QString dialogTitle = muse::qtrc("project/save", "Save score");
    std::string filenameAddition;

    if (saveMode == SaveMode::SaveCopy) {
        //: used to form a filename suggestion, like "originalFile - copy"
        filenameAddition = " - " + muse::trc("project/save", "copy", "a copy of a file");
    } else if (saveMode == SaveMode::SaveSelection) {
        //: used to form a filename suggestion, like "originalFile - selection"
        filenameAddition = " - " + muse::trc("project/save", "selection");
    }

    muse::io::path_t defaultPath = configuration()->defaultSavingFilePath(project, filenameAddition);

    std::vector<std::string> filter {
        muse::trc("project", "MuseScore file") + " (*.mscz)",
        muse::trc("project", "Uncompressed MuseScore folder (experimental)")
#ifdef Q_OS_MAC
        + " (*)"
#else
        + " (*.)"
#endif
    };

    muse::io::path_t selectedPath = interactive()->selectSavingFile(dialogTitle, defaultPath, filter);

    if (selectedPath.empty()) {
        return make_ret(Ret::Code::Cancel);
    }

    if (!engraving::isMuseScoreFile(io::suffix(selectedPath))) {
        // Then it must be that the user is trying to save a mscx file.
        // At the selected path, a folder will be created,
        // and inside the folder, a mscx file will be created.
        // We should return the path to the mscx file.
        selectedPath = selectedPath.appendingComponent(io::filename(selectedPath)).appendingSuffix(engraving::MSCX);
    }

    configuration()->setLastSavedProjectsPath(io::dirpath(selectedPath));

    return RetVal<muse::io::path_t>::make_ok(selectedPath);
}

RetVal<SaveLocationType> OpenSaveProjectScenario::saveLocationType() const
{
    bool shouldAsk = configuration()->shouldAskSaveLocationType();
    SaveLocationType lastUsed = configuration()->lastUsedSaveLocationType();
    if (!shouldAsk && lastUsed != SaveLocationType::Undefined) {
        return RetVal<SaveLocationType>::make_ok(lastUsed);
    }

    return askSaveLocationType();
}

RetVal<SaveLocationType> OpenSaveProjectScenario::askSaveLocationType() const
{
    UriQuery query("musescore://project/asksavelocationtype");
    bool shouldAsk = configuration()->shouldAskSaveLocationType();
    query.addParam("askAgain", Val(shouldAsk));

    RetVal<Val> rv = interactive()->open(query);
    if (!rv.ret) {
        return rv.ret;
    }

    QVariantMap vals = rv.val.toQVariant().toMap();

    bool askAgain = vals["askAgain"].toBool();
    configuration()->setShouldAskSaveLocationType(askAgain);

    SaveLocationType type = static_cast<SaveLocationType>(vals["saveLocationType"].toInt());
    return RetVal<SaveLocationType>::make_ok(type);
}

RetVal<CloudProjectInfo> OpenSaveProjectScenario::askCloudLocation(INotationProjectPtr project, SaveMode mode) const
{
    return doAskCloudLocation(project, mode, false);
}

RetVal<CloudProjectInfo> OpenSaveProjectScenario::askPublishLocation(INotationProjectPtr project) const
{
    return doAskCloudLocation(project, SaveMode::Save, true);
}

RetVal<CloudAudioInfo> OpenSaveProjectScenario::askShareAudioLocation(INotationProjectPtr project) const
{
    bool isCloudAvailable = audioComService()->authorization()->checkCloudIsAvailable();
    if (!isCloudAvailable) {
        return warnCloudNotAvailableForSharingAudio();
    }

    std::string dialogText = muse::trc("project/save", "Log in or create a new account on Audio.com to share your music.");
    Ret ret = audioComService()->authorization()->ensureAuthorization(false, dialogText).ret;
    if (!ret) {
        return ret;
    }

    QString defaultName = project->displayName();
    QUrl uploadUrl = project->cloudAudioInfo().url;
    cloud::Visibility defaultVisibility = cloud::Visibility::Public;

    UriQuery query("musescore://project/savetocloud");
    query.addParam("isPublishShare", Val(true));
    query.addParam("name", Val(defaultName));
    query.addParam("visibility", Val(defaultVisibility));
    query.addParam("cloudCode", Val(cloud::AUDIO_COM_CLOUD_CODE));

    if (!uploadUrl.isEmpty()) {
        query.addParam("existingScoreOrAudioUrl", Val(uploadUrl.toString()));
    }

    RetVal<Val> rv = interactive()->open(query);
    if (!rv.ret) {
        return rv.ret;
    }

    QVariantMap vals = rv.val.toQVariant().toMap();
    using Response = cloud::QMLSaveToCloudResponse::SaveToCloudResponse;
    auto response = static_cast<Response>(vals["response"].toInt());
    switch (response) {
    case Response::Cancel:
    case Response::SaveLocallyInstead:
        return make_ret(Ret::Code::Cancel);
    case Response::Ok:
        break;
    }

    CloudAudioInfo result;
    result.name = vals["name"].toString();
    result.visibility = static_cast<cloud::Visibility>(vals["visibility"].toInt());
    result.replaceExisting = vals["replaceExisting"].toBool() && !uploadUrl.isEmpty();

    return RetVal<CloudAudioInfo>::make_ok(result);
}

RetVal<CloudProjectInfo> OpenSaveProjectScenario::doAskCloudLocation(INotationProjectPtr project, SaveMode mode, bool isPublishShare) const
{
    bool isCloudAvailable = museScoreComService()->authorization()->checkCloudIsAvailable();
    if (!isCloudAvailable) {
        return warnCloudNotAvailableForUploading(isPublishShare);
    }

    std::string dialogText = isPublishShare
                             ? muse::trc("project/save", "Log in to musescore.com to save this score to the cloud.")
                             : muse::trc("project/save", "Log in to musescore.com to publish this score.");
    RetVal<Val> retVal = museScoreComService()->authorization()->ensureAuthorization(true, dialogText);
    if (!retVal.ret) {
        return retVal.ret;
    }

    using Response = cloud::QMLSaveToCloudResponse::SaveToCloudResponse;
    if (static_cast<Response>(retVal.val.toInt()) == Response::SaveLocallyInstead) {
        RetVal<muse::io::path_t> rv = askLocalPath(project, mode);
        if (!rv.ret) {
            LOGE() << rv.ret.toString();
            return rv.ret;
        }

        projectFilesController()->saveProjectLocally(rv.val, mode);
        configuration()->setLastUsedSaveLocationType(SaveLocationType::Local);

        return make_ret(Ret::Code::Cancel);
    }

    QString defaultName = project->displayName();
    cloud::Visibility defaultVisibility = isPublishShare ? cloud::Visibility::Public : cloud::Visibility::Private;
    const CloudProjectInfo existingProjectInfo = project->cloudInfo();

    if (!existingProjectInfo.sourceUrl.isEmpty()) {
        RetVal<cloud::ScoreInfo> scoreInfo = museScoreComService()->downloadScoreInfo(existingProjectInfo.sourceUrl);

        switch (scoreInfo.ret.code()) {
        case int(Ret::Code::Ok):
            defaultName = scoreInfo.val.title;
            if (!isPublishShare) {
                defaultVisibility = scoreInfo.val.visibility;
            }
            break;

        case int(cloud::Err::Status400_InvalidRequest):
        case int(cloud::Err::Status403_AccountNotActivated):
        case int(cloud::Err::Status422_ValidationFailed):
        case int(cloud::Err::Status429_RateLimitExceeded):
        case int(cloud::Err::Status500_InternalServerError):
        case int(cloud::Err::UnknownStatusCode):
        case int(cloud::Err::NetworkError):
            return showCloudSaveError(scoreInfo.ret, project->cloudInfo(), isPublishShare, false);

        // It's possible the source URL is invalid or points to a score on a different user's account.
        // In this situation we shouldn't show an error.
        default: break;
        }
    }

    UriQuery query("musescore://project/savetocloud");
    query.addParam("isPublishShare", Val(isPublishShare));
    query.addParam("name", Val(defaultName));
    query.addParam("visibility", Val(defaultVisibility));
    query.addParam("existingScoreOrAudioUrl", Val(existingProjectInfo.sourceUrl.toString()));
    query.addParam("cloudCode", Val(cloud::MUSESCORE_COM_CLOUD_CODE));

    RetVal<Val> rv = interactive()->open(query);
    if (!rv.ret) {
        return rv.ret;
    }

    QVariantMap vals = rv.val.toQVariant().toMap();
    using Response = cloud::QMLSaveToCloudResponse::SaveToCloudResponse;
    auto response = static_cast<Response>(vals["response"].toInt());
    switch (response) {
    case Response::Cancel:
        return make_ret(Ret::Code::Cancel);
    case Response::SaveLocallyInstead:
        return Ret(RET_CODE_CHANGE_SAVE_LOCATION_TYPE);
    case Response::Ok:
        break;
    }

    CloudProjectInfo result;

    if ((mode == SaveMode::Save || isPublishShare) && vals["replaceExisting"].toBool()) {
        result = existingProjectInfo;
    }

    result.name = vals["name"].toString();
    result.visibility = static_cast<cloud::Visibility>(vals["visibility"].toInt());

    if (!warnBeforePublishing(isPublishShare, result.visibility)) {
        return make_ret(Ret::Code::Cancel);
    }

    return RetVal<CloudProjectInfo>::make_ok(result);
}

bool OpenSaveProjectScenario::warnBeforePublishing(bool isPublishShare, cloud::Visibility visibility) const
{
    if (isPublishShare) {
        if (!configuration()->shouldWarnBeforePublish()) {
            return true;
        }
    } else {
        if (!configuration()->shouldWarnBeforeSavingPubliclyToCloud()) {
            return true;
        }
    }

    std::string title, message;

    IInteractive::ButtonDatas buttons = {
        IInteractive::ButtonData(IInteractive::Button::Cancel, muse::trc("global", "Cancel")),
        IInteractive::ButtonData(IInteractive::Button::Ok, muse::trc("project/save", "Publish"), true)
    };

    IInteractive::Options options = IInteractive::Option::WithIcon | IInteractive::Option::WithDontShowAgainCheckBox;

    if (isPublishShare) {
        title = muse::trc("project/save", "Publish changes online?");
        message = muse::trc("project/save", "We will need to generate a new MP3 for web playback.");
    } else if (visibility == cloud::Visibility::Public) {
        title = muse::trc("project/save", "Publish this score online?"),
        message = muse::trc("project/save", "All saved changes will be publicly visible on MuseScore.com. "
                                            "If you want to make frequent changes, we recommend saving this "
                                            "score privately until you’re ready to share it to the world.");
    } else {
        return true;
    }

    IInteractive::Result result = interactive()->warning(title, message, buttons, int(IInteractive::Button::Ok), options);

    bool ok = result.standardButton() == IInteractive::Button::Ok;
    if (ok && !result.showAgain()) {
        if (isPublishShare) {
            configuration()->setShouldWarnBeforePublish(false);
        } else {
            configuration()->setShouldWarnBeforeSavingPubliclyToCloud(false);
        }
    }

    return ok;
}

bool OpenSaveProjectScenario::warnBeforeSavingToExistingPubliclyVisibleCloudProject() const
{
    IInteractive::ButtonDatas buttons = {
        IInteractive::ButtonData(IInteractive::Button::Cancel, muse::trc("global", "Cancel")),
        IInteractive::ButtonData(IInteractive::Button::Ok, muse::trc("project/save", "Publish"), true)
    };

    IInteractive::Result result = interactive()->warning(
        muse::trc("project/save", "Publish changes online?"),
        muse::trc("project/save", "Your saved changes will be publicly visible. We will also "
                                  "need to generate a new MP3 for public playback."),
        buttons, int(IInteractive::Button::Ok));

    return result.standardButton() == IInteractive::Button::Ok;
}

Ret OpenSaveProjectScenario::warnCloudNotAvailableForUploading(bool isPublishShare) const
{
    if (isPublishShare) {
        interactive()->warning(muse::trc("project/save", "Unable to connect to MuseScore.com"),
                               muse::trc("project/save", "Please check your internet connection or try again later."));
        return make_ret(Ret::Code::Cancel);
    }

    IInteractive::ButtonDatas buttons = {
        IInteractive::ButtonData(IInteractive::Button::Cancel, muse::trc("global", "Cancel")),
        IInteractive::ButtonData(IInteractive::Button::Ok, muse::trc("project/save", "Save to computer"), true)
    };

    IInteractive::Result result = interactive()->warning(muse::trc("project/save", "Unable to connect to the cloud"),
                                                         muse::trc("project/save",
                                                                   "Please check your internet connection or try again later."),
                                                         buttons, int(IInteractive::Button::Ok));

    if (result.standardButton() == IInteractive::Button::Ok) {
        return Ret(RET_CODE_CHANGE_SAVE_LOCATION_TYPE);
    }

    return make_ret(Ret::Code::Cancel);
}

Ret OpenSaveProjectScenario::warnCloudNotAvailableForSharingAudio() const
{
    interactive()->warning(muse::trc("project/save", "Unable to connect to Audio.com"),
                           muse::trc("project/save", "Please check your internet connection or try again later."));
    return make_ret(Ret::Code::Cancel);
}

static std::string cloudStatusCodeErrorMessage(const Ret& ret, bool withHelp = false)
{
    std::string message;

    switch (ret.code()) {
    case int(cloud::Err::Status400_InvalidRequest):
        //: %1 will be replaced with the error code that MuseScore.com returned; this might contain english text
        //: that is deliberately not translated
        message = muse::qtrc("project/cloud", "MuseScore.com returned an error code: %1.")
                  .arg("400 Invalid request").toStdString();
        break;
    case int(cloud::Err::Status401_AuthorizationRequired):
        //: %1 will be replaced with the error code that MuseScore.com returned; this might contain english text
        //: that is deliberately not translated
        message = muse::qtrc("project/cloud", "MuseScore.com returned an error code: %1.")
                  .arg("401 Authorization required").toStdString();
        break;
    case int(cloud::Err::Status422_ValidationFailed):
        //: %1 will be replaced with the error code that MuseScore.com returned; this might contain english text
        //: that is deliberately not translated
        message = muse::qtrc("project/cloud", "MuseScore.com returned an error code: %1.")
                  .arg("422 Validation failed").toStdString();
        break;
    case int(cloud::Err::Status429_RateLimitExceeded):
        //: %1 will be replaced with the error code that MuseScore.com returned; this might contain english text
        //: that is deliberately not translated
        message = muse::qtrc("project/cloud", "MuseScore.com returned an error code: %1.")
                  .arg("429 Rate limit exceeded").toStdString();
        break;
    case int(cloud::Err::Status500_InternalServerError):
        //: %1 will be replaced with the error code that MuseScore.com returned; this might contain english text
        //: that is deliberately not translated
        message = muse::qtrc("project/cloud", "MuseScore.com returned an error code: %1.")
                  .arg("500 Internal server error").toStdString();
        break;
    case int(cloud::Err::UnknownStatusCode): {
        std::any status = ret.data("status");
        if (status.has_value()) {
            //: %1 will be replaced with the error code that MuseScore.com returned, which is a number.
            message = muse::qtrc("project/cloud", "MuseScore.com returned an unknown error code: %1.")
                      .arg(std::any_cast<int>(status)).toStdString();
        } else {
            message = muse::trc("project/cloud", "MuseScore.com returned an unknown error code.");
        }
    } break;
    }

    if (withHelp) {
        message += "\n\n" + muse::trc("project/cloud", "Please try again later, or get help for this problem on musescore.org.");
    }

    return message;
}

void OpenSaveProjectScenario::showCloudOpenError(const Ret& ret) const
{
    std::string title = muse::trc("project", "Your score could not be opened");
    std::string message;

    switch (ret.code()) {
    case int(Err::InvalidCloudScoreId):
        message = muse::trc("project", "This score is invalid.");
        break;
    case int(Err::FileOpenError):
        message = muse::trc("project/cloud", "The file could not be downloaded to your disk.");
        break;
    case int(cloud::Err::Status403_AccountNotActivated):
        message = muse::trc("project/cloud", "Your musescore.com account needs to be verified first. "
                                             "Please activate your account via the link in the activation email.");
        break;
    case int(cloud::Err::Status403_NotOwner):
        message = muse::trc("project/cloud", "This score does not belong to this account. To access this score, make sure you are logged in "
                                             "to the desktop app with the account to which this score belongs.");
        break;
    case int(cloud::Err::Status404_NotFound):
        message = muse::trc("project/cloud", "The score could not be found, or cannot be accessed by your account.");
        break;

    case int(cloud::Err::Status400_InvalidRequest):
    case int(cloud::Err::Status401_AuthorizationRequired):
    case int(cloud::Err::Status422_ValidationFailed):
    case int(cloud::Err::Status429_RateLimitExceeded):
    case int(cloud::Err::Status500_InternalServerError):
    case int(cloud::Err::UnknownStatusCode):
        message = cloudStatusCodeErrorMessage(ret);
        break;

    case int(cloud::Err::NetworkError):
        message = muse::trc("project/cloud", "Could not connect to <a href=\"https://musescore.com\">musescore.com</a>. "
                                             "Please check your internet connection or try again later.");
        break;
    default:
        message = muse::trc("project/cloud", "Please try again later.");
        break;
    }

    interactive()->warning(title, message);
}

Ret OpenSaveProjectScenario::showCloudSaveError(const Ret& ret, const CloudProjectInfo& info, bool isPublishShare,
                                                bool alreadyAttempted) const
{
    std::string title;
    if (alreadyAttempted) {
        title = isPublishShare
                ? muse::trc("project/save", "Your score could not be published")
                : muse::trc("project/save", "Your score could not be saved to the cloud");
    } else {
        title = isPublishShare
                ? muse::trc("project/save", "Your score cannot be published")
                : muse::trc("project/save", "Your score cannot be saved to the cloud");
    }

    std::string msg;

    static constexpr int helpBtnCode = int(IInteractive::Button::CustomButton) + 1;
    static constexpr int saveLocallyBtnCode = int(IInteractive::Button::CustomButton) + 2;
    static constexpr int saveAsBtnCode = int(IInteractive::Button::CustomButton) + 3;
    static constexpr int publishAsNewScoreBtnCode = int(IInteractive::Button::CustomButton) + 4;
    static constexpr int replaceBtnCode = int(IInteractive::Button::CustomButton) + 5;

    IInteractive::ButtonData okBtn = interactive()->buttonData(IInteractive::Button::Ok);
    IInteractive::ButtonData saveLocallyBtn { saveLocallyBtnCode, muse::trc("project/save", "Save to computer") };
    IInteractive::ButtonData helpBtn { helpBtnCode, muse::trc("project/save", "Get help") };

    IInteractive::ButtonDatas buttons = (alreadyAttempted || isPublishShare)
                                        ? (IInteractive::ButtonDatas { helpBtn, okBtn })
                                        : (IInteractive::ButtonDatas { helpBtn, saveLocallyBtn, okBtn });

    int defaultButtonCode = okBtn.btn;

    switch (ret.code()) {
    case int(cloud::Err::Status403_AccountNotActivated):
        msg = muse::trc("project/cloud", "Your musescore.com account needs to be verified first. "
                                         "Please activate your account via the link in the activation email.");
        buttons = { okBtn };
        break;
    case int(cloud::Err::Status409_Conflict):
        title = muse::trc("project/save", "There are conflicting changes in the online score");
        if (isPublishShare) {
            msg = muse::qtrc("project/save", "You can replace the <a href=\"%1\">online score</a>, or publish this as a new score "
                                             "to avoid losing changes in the current online version.")
                  .arg(info.sourceUrl.toString())
                  .toStdString();
            buttons = {
                interactive()->buttonData(IInteractive::Button::Cancel),
                IInteractive::ButtonData { publishAsNewScoreBtnCode, muse::trc("project/save", "Publish as new score") },
                IInteractive::ButtonData { replaceBtnCode, muse::trc("project/save", "Replace") }
            };
            defaultButtonCode = replaceBtnCode;
        } else {
            msg = muse::qtrc("project/save", "You can replace the <a href=\"%1\">online score</a>, or save this as a new file "
                                             "to avoid losing changes in the current online version.")
                  .arg(info.sourceUrl.toString())
                  .toStdString();
            buttons = {
                interactive()->buttonData(IInteractive::Button::Cancel),
                IInteractive::ButtonData { saveAsBtnCode, muse::trc("project/save", "Save as…") },
                IInteractive::ButtonData { replaceBtnCode, muse::trc("project/save", "Replace") }
            };
            defaultButtonCode = replaceBtnCode;
        }
        break;

    case int(cloud::Err::Status400_InvalidRequest):
    case int(cloud::Err::Status401_AuthorizationRequired):
    case int(cloud::Err::Status422_ValidationFailed):
    case int(cloud::Err::Status429_RateLimitExceeded):
    case int(cloud::Err::Status500_InternalServerError):
    case int(cloud::Err::UnknownStatusCode):
        msg = cloudStatusCodeErrorMessage(ret, /*withHelp=*/ true);
        break;

    case int(cloud::Err::NetworkError):
        msg = muse::trc("project/cloud", "Could not connect to <a href=\"https://musescore.com\">musescore.com</a>. "
                                         "Please check your internet connection or try again later.");
        break;
    default:
        msg = muse::trc("project/cloud", "Please try again later, or get help for this problem on musescore.org.");
        break;
    }

    IInteractive::Result result = interactive()->warning(title, msg, buttons, defaultButtonCode);
    switch (result.button()) {
    case helpBtnCode:
        interactive()->openUrl(configuration()->supportForumUrl());
        break;
    case saveLocallyBtnCode:
        return Ret(RET_CODE_CHANGE_SAVE_LOCATION_TYPE);
    case saveAsBtnCode:
        return Ret(RET_CODE_CONFLICT_RESPONSE_SAVE_AS);
    case publishAsNewScoreBtnCode:
        return Ret(RET_CODE_CONFLICT_RESPONSE_PUBLISH_AS_NEW_SCORE);
    case replaceBtnCode:
        return Ret(RET_CODE_CONFLICT_RESPONSE_REPLACE);
    }

    return make_ret(Ret::Code::Cancel);
}

Ret OpenSaveProjectScenario::showAudioCloudShareError(const Ret& ret) const
{
    std::string title= muse::trc("project/share", "Your audio could not be shared");
    std::string msg;

    IInteractive::ButtonData okBtn = interactive()->buttonData(IInteractive::Button::Ok);
    IInteractive::ButtonDatas buttons = IInteractive::ButtonDatas { okBtn };

    switch (ret.code()) {
    case int(cloud::Err::Status403_AccountNotActivated):
        msg = muse::trc("project/share", "Your audio.com account needs to be verified first. "
                                         "Please activate your account via the link in the activation email.");
        break;
    case int(cloud::Err::UnknownStatusCode): {
        std::any status = ret.data("status");
        if (status.has_value()) {
            //: %1 will be replaced with the error code that audio.com returned, which is a number.
            msg = muse::qtrc("project/share", "Audio.com returned an unknown error code: %1.")
                  .arg(std::any_cast<int>(status)).toStdString();
        } else {
            msg = muse::trc("project/share", "Audio.com returned an unknown error code.");
        }
        msg += "\n\n" + muse::trc("project/share", "Please try again later, or get help for this problem on audio.com.");
    } break;
    case int(cloud::Err::NetworkError):
        msg = muse::trc("project/share", "Could not connect to audio.com. "
                                         "Please check your internet connection or try again later.");
        break;
    default:
        msg = muse::trc("project/share", "Please try again later, or get help for this problem on audio.com.");
        break;
    }

    interactive()->warning(title, msg, buttons);

    return muse::make_ok();
}
