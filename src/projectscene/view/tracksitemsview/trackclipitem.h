/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/modularity/ioc.h"
#include "projectscene/iprojectsceneconfiguration.h"
#include "trackedit/dom/clip.h"

#include "fadetypeconversion.h"
#include "viewtrackitem.h"

namespace au::projectscene {
class TrackClipItem : public ViewTrackItem
{
    Q_OBJECT

    muse::GlobalInject<IProjectSceneConfiguration> configuration;

    Q_PROPERTY(bool isGrouped READ isGrouped NOTIFY groupIdChanged FINAL)
    Q_PROPERTY(int pitch READ pitch NOTIFY pitchChanged FINAL)
    Q_PROPERTY(int speedPercentage READ speedPercentage NOTIFY speedPercentageChanged FINAL)
    Q_PROPERTY(bool isPitchModified READ isPitchModified NOTIFY pitchChanged FINAL)
    Q_PROPERTY(bool isSpeedModified READ isSpeedModified NOTIFY speedPercentageChanged FINAL)
    Q_PROPERTY(double fadeInDuration READ fadeInDuration NOTIFY fadesChanged FINAL)
    Q_PROPERTY(double fadeOutDuration READ fadeOutDuration NOTIFY fadesChanged FINAL)
    Q_PROPERTY(FadeTypes::Type fadeInType READ fadeInType NOTIFY fadesChanged FINAL)
    Q_PROPERTY(FadeTypes::Type fadeOutType READ fadeOutType NOTIFY fadesChanged FINAL)
    //! The time selection within the clip item, in pixels from its left edge
    Q_PROPERTY(double selectionStart READ selectionStart NOTIFY selectionPixelsChanged FINAL)
    Q_PROPERTY(double selectionWidth READ selectionWidth NOTIFY selectionPixelsChanged FINAL)
    //! Whether the clip is within the view and its buffer
    Q_PROPERTY(bool inView READ inView NOTIFY inViewChanged FINAL)
    //! Whether the clip's view is kept: it is while in view, and for a moment after leaving it, so that resizing
    //! the view or zooming doesn't destroy and recreate clip views on every step
    Q_PROPERTY(bool loaded READ loaded NOTIFY loadedChanged FINAL)
    //! To the neighbor clips' edges, in pixels; -1 without a neighbor
    Q_PROPERTY(double distanceToLeftNeighbor READ distanceToLeftNeighbor NOTIFY neighborDistancesChanged FINAL)
    Q_PROPERTY(double distanceToRightNeighbor READ distanceToRightNeighbor NOTIFY neighborDistancesChanged FINAL)
    //! The mouse is over the clip, or the clip is in use, so it needs a full ClipItem
    Q_PROPERTY(bool delegateRequested READ delegateRequested NOTIFY delegateRequestedChanged FINAL)
    //! The light clip view can't show the clip as it looks, so it needs a full ClipItem
    Q_PROPERTY(bool needsFullView READ needsFullView NOTIFY needsFullViewChanged FINAL)
    // Q_PROPERTY(QObject * envelopeModel READ envelopeModel NOTIFY envelopeModelChanged)

public:
    explicit TrackClipItem(QObject* parent);

    void setClip(const trackedit::Clip& clip);

    bool isGrouped() const;

    int pitch() const;
    int speedPercentage() const;
    bool isPitchModified() const;
    bool isSpeedModified() const;
    double speed() const;

    double fadeInDuration() const;
    double fadeOutDuration() const;
    FadeTypes::Type fadeInType() const;
    FadeTypes::Type fadeOutType() const;
    double clipStartTime() const { return m_clipStartTime; }
    double clipEndTime() const { return m_clipEndTime; }

    double selectionStart() const { return m_selectionStart; }
    double selectionWidth() const { return m_selectionWidth; }
    void setSelectionPixels(double start, double width);

    bool inView() const { return m_inView; }
    void setInView(bool inView);
    bool loaded() const { return m_loaded; }
    void unload();
    double distanceToLeftNeighbor() const { return m_distanceToLeftNeighbor; }
    double distanceToRightNeighbor() const { return m_distanceToRightNeighbor; }
    void setNeighborDistances(double left, double right);

    bool delegateRequested() const { return m_delegateRequested; }
    void setDelegateRequested(bool requested);
    bool needsFullView() const { return m_needsFullView; }
    void setNeedsFullView(bool needsFullView);

signals:
    void groupIdChanged();
    void pitchChanged();
    void speedPercentageChanged();
    void contentChanged();
    void fadesChanged();
    void selectionPixelsChanged();
    void inViewChanged();
    void loadedChanged();
    void neighborDistancesChanged();
    void delegateRequestedChanged();
    void needsFullViewChanged();

private:
    int m_groupId = -1;
    int m_pitch = 0;
    double m_speed = 1.0;
    double m_fadeInDuration = 0.0;
    double m_fadeOutDuration = 0.0;
    FadeTypes::Type m_fadeInType = toFadeTypesType(trackedit::defaultFadeType);
    FadeTypes::Type m_fadeOutType = toFadeTypesType(trackedit::defaultFadeType);
    double m_clipStartTime = 0.0;
    double m_clipEndTime = 0.0;
    double m_selectionStart = 0.0;
    double m_selectionWidth = 0.0;
    bool m_inView = false;
    bool m_loaded = false;
    double m_distanceToLeftNeighbor = -1.0;
    double m_distanceToRightNeighbor = -1.0;
    bool m_delegateRequested = false;
    bool m_needsFullView = false;
};
}
