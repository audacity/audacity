/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "spectrogram/ispectrogramservice.h"
#include "spectrogram/view/ispectrogramviewservice.h"

#include "shared/axis/axistypes.h"

#include "framework/global/modularity/ioc.h"
#include "framework/global/async/asyncable.h"
#include "framework/ui/iuiconfiguration.h"

#include <QObject>
#include <QQmlParserStatus>
#include <QVariantList>

namespace au::spectrogram {
class SpectrogramChannelRulerModel : public QObject, public QQmlParserStatus, public muse::async::Asyncable, public muse::Contextable
{
    Q_OBJECT

    Q_PROPERTY(int trackId READ trackId WRITE setTrackId NOTIFY trackIdChanged FINAL)
    Q_PROPERTY(int labelHeight READ labelHeight WRITE setLabelHeight NOTIFY labelHeightChanged FINAL)
    Q_PROPERTY(double channelHeight READ channelHeight WRITE setChannelHeight NOTIFY channelHeightChanged FINAL)
    Q_PROPERTY(QVariantList majorTicks READ majorTicks NOTIFY ticksChanged FINAL)
    Q_PROPERTY(QVariantList minorTicks READ minorTicks NOTIFY ticksChanged FINAL)
    Q_PROPERTY(double rulerGuideYPos READ rulerGuideYPos WRITE setRulerGuideYPos NOTIFY rulerGuideYPosChanged FINAL)

    Q_PROPERTY(bool isMinZoom READ isMinZoom NOTIFY zoomStateChanged FINAL)
    Q_PROPERTY(bool isHighContrast READ isHighContrast NOTIFY isHighContrastChanged FINAL)

    muse::GlobalInject<muse::ui::IUiConfiguration> uiConfig;

    muse::ContextInject<ISpectrogramService> spectrogramService{ this };
    muse::ContextInject<ISpectrogramViewService> spectrogramViewService{ this };

public:
    SpectrogramChannelRulerModel(QObject* parent = nullptr);

    int trackId() const;
    void setTrackId(int trackId);

    int labelHeight() const { return m_labelHeight; }
    void setLabelHeight(int height);

    double channelHeight() const;
    void setChannelHeight(double height);

    bool isHighContrast() const { return uiConfig()->isHighContrast(); }

    Q_INVOKABLE void zoomIn(double mouseY);
    Q_INVOKABLE void zoomOut(double mouseY);
    Q_INVOKABLE void setPopupPosition(double yPos);
    Q_INVOKABLE void zoomInFromPopup();
    Q_INVOKABLE void zoomOutFromPopup();
    Q_INVOKABLE void resetZoom();

    Q_INVOKABLE void scrollBy(double delta);

    bool isMinZoom() const;

    QVariantList majorTicks() const;
    QVariantList minorTicks() const;

    double rulerGuideYPos() const;
    void setRulerGuideYPos(double yPos);

signals:
    void trackIdChanged();
    void channelHeightChanged();
    void ticksChanged();
    void labelHeightChanged();
    void rulerGuideYPosChanged();
    void zoomStateChanged();
    void isHighContrastChanged();

private:
    void classBegin() override {}
    void componentComplete() override;

    void updateTicks();
    void zoomBy(double factor, double centerPosition);
    double frequencyToPosition(double freq) const;
    double positionToFrequency(double pos) const;
    QVariantList tickListToVariants(const std::vector<au::shared::AxisTick>& ticks) const;

    int m_trackId = -1;
    int m_labelHeight = 0;
    double m_channelHeight = 0.0;
    SpectrogramRulerTicks m_ticks;
    double m_popupPosition = 0.0;
};
} // namespace au::spectrogram
