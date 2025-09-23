/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "dynamicstimelinetypes.h"
#include "painters/abstractsequencepainter.h"

#include <QQmlParserStatus>
#include <QQuickItem>
#include <QSGGeometryNode>
#include <QTimer>

#include <atomic>
#include <array>
#include <mutex>
#include <unordered_map>
#include <limits>

namespace au::effects {
class DynamicsTimeline : public QQuickItem
{
    Q_OBJECT

    Q_PROPERTY(double dbMin READ dbMin WRITE setDbMin NOTIFY dbMinChanged)
    Q_PROPERTY(double duration READ duration WRITE setDuration NOTIFY durationChanged)
    Q_PROPERTY(double dataPointRate READ dataPointRate WRITE setDataPointRate NOTIFY dataPointRateChanged)
    Q_PROPERTY(
        double audioThreadBufferDuration READ audioThreadBufferDuration WRITE setAudioThreadBufferDuration NOTIFY audioThreadBufferDurationChanged)

    Q_PROPERTY(bool showInputDb READ showInputDb WRITE setShowInputDb NOTIFY showInputDbChanged)
    Q_PROPERTY(bool showOutputDb READ showOutputDb WRITE setShowOutputDb NOTIFY showOutputDbChanged)
    Q_PROPERTY(bool showCompressionDb READ showCompressionDb WRITE setShowCompressionDb NOTIFY showCompressionDbChanged)

    Q_PROPERTY(double stopwatchTime READ stopwatchTime WRITE setStopwatchTime NOTIFY stopwatchTimeChanged)

public:
    explicit DynamicsTimeline(QQuickItem* parent = nullptr);

    double dbMin() const;
    void setDbMin(double dbMin);

    double duration() const;
    void setDuration(double duration);

    double dataPointRate() const;
    void setDataPointRate(double rate);

    double audioThreadBufferDuration() const;
    void setAudioThreadBufferDuration(double duration);

    bool showInputDb() const;
    void setShowInputDb(bool show);

    bool showOutputDb() const;
    void setShowOutputDb(bool show);

    bool showCompressionDb() const;
    void setShowCompressionDb(bool show);

    double stopwatchTime() const;
    void setStopwatchTime(double stopwatchTime);

    Q_INVOKABLE void onNewSamples(const QVariantList&);
    Q_INVOKABLE void clear();

signals:
    void dbMinChanged();
    void durationChanged();
    void dataPointRateChanged();
    void audioThreadBufferDurationChanged();
    void showInputDbChanged();
    void showOutputDbChanged();
    void showCompressionDbChanged();
    void stopwatchTimeChanged();

private:
    struct SequenceData {
        bool visible = true;
        std::unique_ptr<AbstractSequencePainter> drawer;
        QSGGeometryNode* geometryNode = nullptr;
        std::vector<double> pendingYValues;
    };

    enum class DrawerType {
        Line, Area
    };

    SequenceData createSequenceData(const QColor&, DrawerType, bool visible) const;
    void resetSequences();
    void updateDrawerViewportX();

    void componentComplete() override;
    double timeToX(double time) const;
    double xToTime(double x) const;
    double dbToY(double db) const;

    enum DynamicsDataId {
        eInputDb,
        eOutputDb,
        eOutputDbLine,
        eCompressionDb,
        _eSequenceCount
    };

    QSGNode* updatePaintNode(QSGNode*, UpdatePaintNodeData*) override;

    double m_dbMin = 0;
    double m_duration = 0;
    double m_dataPointRate = 0;
    double m_audioThreadBufferDuration = 0;
    int m_prevSampleIndex = -1;
    double m_stopwatchTime = 0;
    bool m_reset = false;

    // Difference time between stopwatch time and packet-arrival time.
    std::atomic<double> m_timeDiff = std::numeric_limits<float>::lowest();

    double m_drawerViewportX = 0;
    std::array<SequenceData, _eSequenceCount> m_sequences;
    std::vector<double> m_pendingXValues;
    std::mutex m_sampleMutex; // TODO consider using a ring buffer
};
} // namespace au::effects
