/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "spectrogram/ispectrogramservice.h"

#include "framework/global/modularity/ioc.h"
#include "framework/global/async/asyncable.h"

#include <QObject>
#include <QQmlParserStatus>
#include <QVariantList>

namespace au::spectrogram {
class SpectrogramChannelRulerModel : public QObject, public QQmlParserStatus, public muse::async::Asyncable, public muse::Injectable
{
    Q_OBJECT

    Q_PROPERTY(int trackId READ trackId WRITE setTrackId NOTIFY trackIdChanged FINAL)
    Q_PROPERTY(int labelHeight READ labelHeight WRITE setLabelHeight NOTIFY labelHeightChanged FINAL)
    Q_PROPERTY(double channelHeight READ channelHeight WRITE setChannelHeight NOTIFY channelHeightChanged FINAL)
    Q_PROPERTY(QVariantList majorTicks READ majorTicks NOTIFY ticksChanged FINAL)
    Q_PROPERTY(QVariantList minorTicks READ minorTicks NOTIFY ticksChanged FINAL)
    Q_PROPERTY(double pointerFrequency READ pointerFrequency WRITE setPointerFrequency NOTIFY pointerFrequencyChanged FINAL)
    Q_PROPERTY(double pointerYPos READ pointerYPos NOTIFY pointerFrequencyChanged FINAL)

    muse::Inject<ISpectrogramService> spectrogramService{ this };

public:
    SpectrogramChannelRulerModel(QObject* parent = nullptr);

    int trackId() const;
    void setTrackId(int trackId);

    int labelHeight() const { return m_labelHeight; }
    void setLabelHeight(int height);

    double channelHeight() const;
    void setChannelHeight(double height);

    Q_INVOKABLE void zoomIn(double mouseY);
    Q_INVOKABLE void zoomOut(double mouseY);

    Q_INVOKABLE void scrollBy(double delta);

    QVariantList majorTicks() const;
    QVariantList minorTicks() const;

    double pointerFrequency() const { return m_pointerFrequency; }
    void setPointerFrequency(double frequency);

    double pointerYPos() const { return frequencyToPosition(m_pointerFrequency); }

signals:
    void trackIdChanged();
    void channelHeightChanged();
    void ticksChanged();
    void labelHeightChanged();
    void pointerFrequencyChanged();

private:
    void classBegin() override {}
    void componentComplete() override;

    void updateTicks();
    void zoomBy(double factor, double centerPosition);
    double frequencyToPosition(double freq) const;
    double positionToFrequency(double pos) const;

    int m_trackId = -1;
    int m_labelHeight = 0;
    double m_channelHeight = 0.0;
    double m_pointerFrequency = 0.0;
    SpectrogramRulerTicks m_ticks;
};
} // namespace au::spectrogram
