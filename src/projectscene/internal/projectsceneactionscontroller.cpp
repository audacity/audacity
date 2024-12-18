/*
* Audacity: A Digital Audio Editor
*/

#include "projectsceneactionscontroller.h"

using namespace muse;
using namespace au::projectscene;
using namespace muse::async;
using namespace muse::actions;

static const ActionCode VERTICAL_RULERS_CODE("toggle-vertical-rulers");
static const ActionCode MINUTES_SECONDS_RULER("minutes-seconds-ruler");
static const ActionCode BEATS_MEASURES_RULER("beats-measures-ruler");
static const ActionCode CLIP_PITCH_AND_SPEED_CODE("clip-pitch-speed");

void ProjectSceneActionsController::init()
{
    dispatcher()->reg(this, MINUTES_SECONDS_RULER, this, &ProjectSceneActionsController::toggleMinutesSecondsRuler);
    dispatcher()->reg(this, BEATS_MEASURES_RULER, this, &ProjectSceneActionsController::toggleBeatsMeasuresRuler);
    dispatcher()->reg(this, VERTICAL_RULERS_CODE, this, &ProjectSceneActionsController::toggleVerticalRulers);
    dispatcher()->reg(this, "update-display-while-playing", this, &ProjectSceneActionsController::updateDisplayWhilePlaying);
    dispatcher()->reg(this, "pinned-play-head", this, &ProjectSceneActionsController::pinnedPlayHead);
    dispatcher()->reg(this, CLIP_PITCH_AND_SPEED_CODE, this, &ProjectSceneActionsController::openClipPitchAndSpeedEdit);
    dispatcher()->reg(this, "insert-silence", this, &ProjectSceneActionsController::insertSilence);
}

void ProjectSceneActionsController::notifyActionCheckedChanged(const ActionCode& actionCode)
{
    m_actionCheckedChanged.send(actionCode);
}

void ProjectSceneActionsController::toggleMinutesSecondsRuler()
{
    if (configuration()->timelineRulerMode() == TimelineRulerMode::MINUTES_AND_SECONDS) {
        return;
    }

    configuration()->setTimelineRulerMode(TimelineRulerMode::MINUTES_AND_SECONDS);
    notifyActionCheckedChanged(MINUTES_SECONDS_RULER);
    notifyActionCheckedChanged(BEATS_MEASURES_RULER);
}

void ProjectSceneActionsController::toggleBeatsMeasuresRuler()
{
    if (configuration()->timelineRulerMode() == TimelineRulerMode::BEATS_AND_MEASURES) {
        return;
    }

    configuration()->setTimelineRulerMode(TimelineRulerMode::BEATS_AND_MEASURES);
    notifyActionCheckedChanged(MINUTES_SECONDS_RULER);
    notifyActionCheckedChanged(BEATS_MEASURES_RULER);
}

void ProjectSceneActionsController::toggleVerticalRulers()
{
    bool verticalRulersVisible = configuration()->isVerticalRulersVisible();
    configuration()->setVerticalRulersVisible(!verticalRulersVisible);
    notifyActionCheckedChanged(VERTICAL_RULERS_CODE);
}

void ProjectSceneActionsController::updateDisplayWhilePlaying()
{
    NOT_IMPLEMENTED;
}

void ProjectSceneActionsController::pinnedPlayHead()
{
    NOT_IMPLEMENTED;
}

void ProjectSceneActionsController::openClipPitchAndSpeedEdit(const ActionData& args)
{
    IF_ASSERT_FAILED(args.count() == 1) {
        return;
    }

    trackedit::ClipKey clipKey = args.arg<trackedit::ClipKey>(0);
    if (!clipKey.isValid()) {
        return;
    }

    muse::UriQuery query("audacity://projectscene/editpitchandspeed");
    query.addParam("trackId", muse::Val(std::to_string(clipKey.trackId)));
    query.addParam("clipId", muse::Val(std::to_string(clipKey.clipId)));
    query.addParam("focusItemName", muse::Val("pitch"));

    interactive()->open(query);
}

void ProjectSceneActionsController::insertSilence(const muse::actions::ActionData& args)
{
    muse::UriQuery query("audacity://projectscene/insertsilence");

    auto rv = interactive()->open(query);
}

bool ProjectSceneActionsController::actionChecked(const ActionCode& actionCode) const
{
    QMap<std::string, bool> isChecked {
        { VERTICAL_RULERS_CODE, configuration()->isVerticalRulersVisible() },
        { MINUTES_SECONDS_RULER, configuration()->timelineRulerMode() == TimelineRulerMode::MINUTES_AND_SECONDS },
        { BEATS_MEASURES_RULER, configuration()->timelineRulerMode() == TimelineRulerMode::BEATS_AND_MEASURES }
    };

    return isChecked[actionCode];
}

Channel<ActionCode> ProjectSceneActionsController::actionCheckedChanged() const
{
    return m_actionCheckedChanged;
}

bool ProjectSceneActionsController::canReceiveAction(const ActionCode&) const
{
    return globalContext()->currentProject() != nullptr;
}
