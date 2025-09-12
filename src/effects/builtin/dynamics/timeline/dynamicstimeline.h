#pragma once

#include "painters/abstractsequencepainter.h"
#include "profiler.h"

#include <QQmlParserStatus>
#include <QQuickItem>
#include <QSGGeometryNode>
#include <QTimer>

#include <array>
#include <mutex>
#include <unordered_map>

namespace au::effects {
class DynamicsTimeline : public QQuickItem
{
    Q_OBJECT

    Q_PROPERTY(double dbMin READ dbMin WRITE setDbMin NOTIFY dbMinChanged)
    Q_PROPERTY(
        double duration READ duration WRITE setDuration NOTIFY durationChanged)
    Q_PROPERTY(double samplePeriod READ samplePeriod WRITE setSamplePeriod NOTIFY
               samplePeriodChanged)

    Q_PROPERTY(bool showInputDb READ showInputDb WRITE setShowInputDb NOTIFY
               showInputDbChanged)
    Q_PROPERTY(bool showOutputDb READ showOutputDb WRITE setShowOutputDb NOTIFY
               showOutputDbChanged)
    Q_PROPERTY(bool showCompressionDb READ showCompressionDb WRITE
               setShowCompressionDb NOTIFY showCompressionDbChanged)

    Q_PROPERTY(double t READ t WRITE setT NOTIFY tChanged)

public:
    explicit DynamicsTimeline(QQuickItem* parent = nullptr);

    double dbMin() const;
    void setDbMin(double dbMin);

    double duration() const;
    void setDuration(double duration);

    double samplePeriod() const;
    void setSamplePeriod(double period);

    bool showInputDb() const;
    void setShowInputDb(bool show);

    bool showOutputDb() const;
    void setShowOutputDb(bool show);

    bool showCompressionDb() const;
    void setShowCompressionDb(bool show);

    double t() const;
    void setT(double t);

    Q_INVOKABLE void onNewSample(double inputDb, double outputDb, double compressionDb);
    Q_INVOKABLE void clear();

signals:
    void dbMinChanged();
    void durationChanged();
    void samplePeriodChanged();
    void showInputDbChanged();
    void showOutputDbChanged();
    void showCompressionDbChanged();
    void tChanged();

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

    SequenceData createSequenceData(const QColor&, DrawerType) const;
    void resetSequences();

    void componentComplete() override;
    double timeToX(double time) const;
    double xToTime(double x) const;
    double dbToY(double db) const;
    double drawerViewportX() const;

    enum SequenceId {
        eInputDb,
        eOutputDb,
        eOutputDbLine,
        eCompressionDb,
        _eSequenceCount
    };

    QSGNode* updatePaintNode(QSGNode*, UpdatePaintNodeData*) override;

    double m_dbMin = -60;
    double m_duration = 5;
    double m_samplePeriod = 0;
    int m_prevSampleIndex = -1;
    double m_t = 0;
    bool m_reset = false;

    QRectF m_drawerViewport;
    std::array<SequenceData, _eSequenceCount> m_sequences;
    std::vector<double> m_pendingXValues;
    std::mutex m_sampleMutex; // TODO consider using a ring buffer

    Profiler m_profiler;
};
} // namespace au::effects
