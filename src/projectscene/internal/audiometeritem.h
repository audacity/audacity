/*
* Audacity: A Digital Audio Editor
*/

#include <QtQuick/QQuickItem>
#include <QtQuick/QSGSimpleRectNode>
#include <QColor>
#include <cstdint>
#include <qsgnode.h>

#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "ui/iuiconfiguration.h"

#include "playback/view/common/playbackmetermodel.h"

namespace {
constexpr float MIN_DB = -145.0f;
}

namespace au::projectscene {
class AudioMeterItem : public QQuickItem, public muse::async::Asyncable
{
    Q_OBJECT

    muse::Inject<muse::ui::IUiConfiguration> uiConfiguration;

    Q_PROPERTY(float currentVolumePressure READ currentVolumePressure WRITE setCurrentVolumePressure)
    Q_PROPERTY(float currentRMS READ currentRMS WRITE setCurrentRMS)
    Q_PROPERTY(bool showClippedInfo READ showClippedInfo WRITE setShowClippedInfo)
    Q_PROPERTY(int recentPeakInterval READ recentPeakInterval WRITE setRecentPeakInterval)
    Q_PROPERTY(int overloadHeight READ overloadHeight WRITE setOverloadHeight)
    Q_PROPERTY(au::playback::PlaybackMeterModel * meterModel READ meterModel WRITE setMeterModel)

public:
    explicit AudioMeterItem(QQuickItem* parent = nullptr);

    float currentVolumePressure() const;
    void setCurrentVolumePressure(float value);
    float currentRMS() const;
    void setCurrentRMS(float value);
    bool showClippedInfo() const;
    void setShowClippedInfo(bool show);
    int recentPeakInterval() const;
    void setRecentPeakInterval(int intervalMsecs);
    int overloadHeight() const;
    void setOverloadHeight(int height);
    au::playback::PlaybackMeterModel* meterModel() const;
    void setMeterModel(au::playback::PlaybackMeterModel* model);

    Q_INVOKABLE void reset();
    Q_INVOKABLE void resetClipped();
    Q_INVOKABLE void load();

protected:
    QSGNode* updatePaintNode(QSGNode* oldNode, UpdatePaintNodeData* updatePaintNodeData) override;

private:
    float sampleToY(float sampleValue) const;
    void updatePeaks();

    QSGNode* createNodes();
    void drawBackground();
    void drawVolumeBar();
    void drawClippedIndicator();
    void drawRecentPeakIndicator();
    void drawMaxPeakIndicator();

    void drawVolumeBarDefault();
    void drawVolumeBarRMS();
    void drawVolumeBarGradient();

    bool m_showClippedInfo = true;
    int m_recentPeakIntervalMsecs = 600;
    int m_overloadHeight = 4;

    float m_currentVolumePressure = MIN_DB;
    float m_lastVolumePressure = MIN_DB;
    float m_currentRMS = MIN_DB;
    float m_lastRMS = MIN_DB;
    float m_maxPeak = MIN_DB;
    float m_lastMaxPeak = MIN_DB;
    float m_recentPeak = MIN_DB;
    float m_lastRecentPeak = MIN_DB;
    bool m_clipped = false;
    bool m_lastClipped = false;

    struct VolumeSample {
        float value;
        int64_t timestamp;
    };
    std::vector<VolumeSample> m_recentVolumePressure;

    au::playback::PlaybackMeterModel* m_meterModel = nullptr;

    QSGSimpleRectNode* m_backgroundNode = nullptr;
    QSGSimpleRectNode* m_volumeBarNode = nullptr;
    QSGSimpleRectNode* m_clippedNode = nullptr;
    QSGSimpleRectNode* m_recentPeakNode = nullptr;
    QSGSimpleRectNode* m_maxPeakNode = nullptr;
    QSGSimpleRectNode* m_rmsNode = nullptr;
    QSGGeometryNode* m_gradientNode = nullptr;

    QColor m_accentColor;
    QColor m_strokeColor;
    QColor m_btnColor;
    QColor m_clippedColor;

    bool m_fullRedrawNeeded = true;
};
}
