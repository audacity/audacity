/*
 * Audacity: A Digital Audio Editor
 */
#include "trackspectrogramsettingsdialogmodel.h"

namespace au::spectrogram {
TrackSpectrogramSettingsDialogModel::TrackSpectrogramSettingsDialogModel(QObject* parent)
    : QObject(parent), muse::Injectable(muse::iocCtxForQmlObject(this))
{
}

void TrackSpectrogramSettingsDialogModel::setTrackId(int value)
{
    if (m_trackId == value) {
        return;
    }
    m_trackId = value;
    emit trackIdChanged();
}

void TrackSpectrogramSettingsDialogModel::requestSpectrogramUpdate()
{
    // const ITrackeditProjectPtr project = globalContext()->currentTrackeditProject();
    // IF_ASSERT_FAILED(project) {
    //     return;
    // }
    // const auto track = project->track(m_trackId);
    // IF_ASSERT_FAILED(track) {
    //     return;
    // }
    // project->notifyAboutTrackChanged(*track);
}
}
