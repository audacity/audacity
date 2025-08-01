/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>

#include "global/async/asyncable.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

namespace au::projectscene {
class TracksViewStateModel : public QObject, public muse::async::Asyncable
{
    Q_OBJECT

    // Context of user interaction
    Q_PROPERTY(int tracksVerticalOffset READ tracksVerticalOffset NOTIFY tracksVerticalOffsetChanged FINAL)
    Q_PROPERTY(bool tracksVerticalScrollLocked READ tracksVerticalScrollLocked NOTIFY tracksVerticalScrollLockedChanged FINAL)
    Q_PROPERTY(int tracksVerticalScrollPadding READ tracksVerticalScrollPadding FINAL CONSTANT)

    Q_PROPERTY(bool altPressed READ altPressed NOTIFY altPressedChanged FINAL)
    Q_PROPERTY(bool ctrlPressed READ ctrlPressed NOTIFY ctrlPressedChanged FINAL)
    Q_PROPERTY(bool escPressed READ escPressed NOTIFY escPressedChanged FINAL)

    Q_PROPERTY(bool snapEnabled READ snapEnabled NOTIFY snapEnabledChanged FINAL)

    muse::Inject<context::IGlobalContext> globalContext;

public:
    TracksViewStateModel(QObject* parent = nullptr);

    Q_INVOKABLE void init();

    bool tracksVerticalScrollLocked() const;
    int tracksVerticalScrollPadding() const;

    // context of user interaction
    int tracksVerticalOffset() const;
    bool altPressed() const;
    bool ctrlPressed() const;
    bool escPressed() const;

    Q_INVOKABLE void changeTracksVerticalOffset(int deltaY);
    Q_INVOKABLE void setMouseY(double y);

    Q_INVOKABLE void requestVerticalScrollLock();
    Q_INVOKABLE void requestVerticalScrollUnlock();

    Q_INVOKABLE bool snapEnabled() const;
    Q_INVOKABLE trackedit::TrackId trackAtPosition(double x, double y) const;

    Q_INVOKABLE int trackHeight(trackedit::TrackId trackId) const;
    Q_INVOKABLE int trackVerticalPosition(trackedit::TrackId trackId) const;

signals:
    void tracksVerticalOffsetChanged();
    void tracksVerticalScrollLockedChanged();
    void altPressedChanged();
    void ctrlPressedChanged();
    void escPressedChanged();

    void snapEnabledChanged();

private:
    static constexpr int m_tracksVerticalScrollPadding = 228;

    IProjectViewStatePtr viewState() const;

    muse::ValCh<int> m_tracksVerticalOffset;
    muse::ValCh<bool> m_tracksVerticalScrollLocked;

    muse::ValCh<bool> m_altPressed;
    muse::ValCh<bool> m_ctrlPressed;
    muse::ValCh<bool> m_escPressed;

    bool m_snapEnabled;
};
}
