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

static const muse::UriQuery SAVE_TO_CLOUD_URI("audacity://project/savetocloud");

void Au3AudioComController::init()
{
    dispatcher()->reg(this, "file-share-audio", this, &Au3AudioComController::shareAudio);
}

void Au3AudioComController::shareAudio()
{
    auto project = globalContext()->currentProject();
    if (!project) {
        LOGW() << "No project opened";
        return;
    }

    RetVal<Val> rv = interactive()->openSync(SAVE_TO_CLOUD_URI);
    if (!rv.ret) {
        return;
    }

    std::string title = rv.val.toQString().toStdString();

    auto progress = audioComService()->shareAudio(project, title);
    progress->finished().onReceive(this, [this, project](const ProgressResult& result) {
        if (result.ret.success()) {
            const bool dismissable = false;
            toastService()->show(trc("au3cloud", "Success"),
                                 trc("au3cloud", "Audio shared to audio.com"),
                                 muse::ui::IconCode::Code::TICK,
                                 dismissable,
            {
                { trc("au3cloud", "Dismiss"), au::toast::ToastActionCode::None },
                { trc("au3cloud", "View on audio.com"), au::toast::ToastActionCode::Custom }
            }
                                 ).onResolve(this, [this, project](au::toast::ToastActionCode actionCode) {
                if (actionCode == au::toast::ToastActionCode::Custom) {
                    interactive()->openUrl(audioComService()->getSharedAudioPage());
                }
            });
        }
    });

    const bool dismissible = false;
    const bool showProgressInfo = true;
    toastService()->showWithProgress(
        trc("au3cloud", "Sharing audio to audio.com..."),
        {},
        progress,
        muse::ui::IconCode::Code::SHARE_AUDIO,
        dismissible,
        {},
        showProgressInfo
        );
}
