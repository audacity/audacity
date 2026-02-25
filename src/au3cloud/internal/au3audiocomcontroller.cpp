/*
* Audacity: A Digital Audio Editor
*/
#include "au3audiocomcontroller.h"

#include "global/translation.h"
#include "progress.h"
#include "ui/view/iconcodes.h"

#include "log.h"

using namespace muse;
using namespace au::au3cloud;

namespace {
constexpr auto SAVE_TO_CLOUD_URI("audacity://project/savetocloud");
}

void Au3AudioComController::init()
{
    dispatcher()->reg(this, "file-share-audio", this, &Au3AudioComController::shareAudio);
}

void Au3AudioComController::shareAudio()
{
    muse::UriQuery query(SAVE_TO_CLOUD_URI);
    query.addParam("formTitle", Val(trc("cloud", "Track title")));

    RetVal<Val> rv = interactive()->openSync(query);
    if (!rv.ret) {
        return;
    }

    std::string title = rv.val.toQString().toStdString();

    auto progress = audioComService()->shareAudio(title);
    progress->finished().onReceive(this, [this](const ProgressResult& result) {
        if (result.ret.success()) {
            const bool dismissable = false;
            toastService()->show(trc("global", "Success"),
                                 trc("cloud", "Audio shared to audio.com"),
                                 muse::ui::IconCode::Code::TICK,
                                 dismissable,
            {
                { trc("global", "Dismiss"), au::toast::ToastActionCode::None },
                { trc("cloud", "View on audio.com"), au::toast::ToastActionCode::Custom }
            }
                                 ).onResolve(this, [this](au::toast::ToastActionCode actionCode) {
                if (actionCode == au::toast::ToastActionCode::Custom) {
                    interactive()->openUrl(audioComService()->getSharedAudioPage());
                }
            });
        } else {
            toastService()->showError(trc("global", "Fail"), trc("cloud", "Unable to save audio file"));
        }
    });

    const bool dismissible = false;
    const bool showProgressInfo = true;
    toastService()->showWithProgress(
        trc("cloud", "Sharing audio to audio.com..."),
        {},
        progress,
        muse::ui::IconCode::Code::SHARE_AUDIO,
        dismissible,
        {},
        showProgressInfo
        );
}
