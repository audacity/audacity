#ifndef AU_PROJECTSCENE_WAVEVIEW_H
#define AU_PROJECTSCENE_WAVEVIEW_H

#include <QQuickItem>

#include "wavesource.h"

namespace au::projectscene {
class WaveView : public QQuickItem
{
    Q_OBJECT
    Q_PROPERTY(WaveSource source READ source WRITE setSource NOTIFY sourceChanged FINAL)

public:
    explicit WaveView(QQuickItem* parent = nullptr);

    WaveSource source() const;
    void setSource(const WaveSource& newSource);

signals:
    void sourceChanged();

protected:
    QSGNode* updatePaintNode(QSGNode* oldNode, UpdatePaintNodeData* updatePaintNodeData) override;

private:
    WaveSource m_source;
};
}

#endif // AU_PROJECTSCENE_WAVEVIEW_H
