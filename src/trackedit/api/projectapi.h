/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QJSValue>

#include "api/apiobject.h"
#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "trackedit/itrackeditproject.h"
#include "trackedit/dom/clip.h"
#include "trackedit/iselectioncontroller.h"

namespace au::trackedit::api {
class ProjectApi : public muse::api::ApiObject
{
    Q_OBJECT

    muse::ContextInject<context::IGlobalContext> globalContext = { this };
    muse::ContextInject<ISelectionController> selectionController = { this };

public:
    explicit ProjectApi(muse::api::IApiEngine* e);

    // Tracks
    Q_INVOKABLE int trackCount() const;

    // Clips
    Q_INVOKABLE int clipCount(int trackIndex) const;
    Q_INVOKABLE double clipStartTime(int trackIndex, int clipIndex) const;
    Q_INVOKABLE double clipEndTime(int trackIndex, int clipIndex) const;
    Q_INVOKABLE QJSValue clipsOnTrack(int trackIndex) const;

    // Project
    Q_INVOKABLE double totalTime() const;

    // Selection
    Q_INVOKABLE double selectionStart() const;
    Q_INVOKABLE double selectionEnd() const;
    Q_INVOKABLE bool hasSelection() const;

private:
    ITrackeditProjectPtr trackeditProject() const;
    Clips sortedClips(int trackIndex) const;
};
}
