/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QJSValue>

#include "api/apiobject.h"
#include "context/iglobalcontext.h"
#include "modularity/ioc.h"
#include "playback/iplaybackcontroller.h"
#include "record/irecordcontroller.h"
#include "trackedit/dom/clip.h"
#include "trackedit/iselectioncontroller.h"
#include "trackedit/iprojecthistory.h"
#include "trackedit/itrackeditproject.h"

class AudacityProject;

namespace au::trackedit::api {
class ProjectApi : public muse::api::ApiObject
{
    Q_OBJECT
    Q_PROPERTY(QObject * selection READ selection)
    Q_PROPERTY(double defaultSampleRate READ defaultSampleRate)

    muse::ContextInject<context::IGlobalContext> globalContext = { this };
    muse::ContextInject<ISelectionController> selectionController = { this };
    muse::ContextInject<IProjectHistory> projectHistory = { this };
    muse::ContextInject<record::IRecordController> recordController = { this };
    muse::ContextInject<playback::IPlaybackController> playbackController = { this };

public:
    explicit ProjectApi(muse::api::IApiEngine* e);

    QObject* selection();
    double defaultSampleRate() const;

    // Tracks
    Q_INVOKABLE int trackCount() const;

    // Clips
    Q_INVOKABLE int clipCount(int trackIndex) const;
    Q_INVOKABLE QJSValue clipsOnTrack(int trackIndex) const;

    // Project
    Q_INVOKABLE double totalTime() const;
    Q_INVOKABLE QObject* beginEdit(const QString& name, QObject* session = nullptr);

private:
    AudacityProject* project() const;
    ITrackeditProjectPtr trackeditProject() const;
    Clips sortedClips(int trackIndex) const;
};
}
