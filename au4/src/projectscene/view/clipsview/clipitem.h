#ifndef AU_PROJECTSCENE_CLIPITEM_H
#define AU_PROJECTSCENE_CLIPITEM_H

#include <QObject>
#include "wavesource.h"

namespace au::projectscene {
class ClipItem : public QObject
{
    Q_OBJECT
    Q_PROPERTY(WaveSource waveSource READ waveSource NOTIFY waveSourceChanged FINAL)

public:
    ClipItem(QObject* parent = nullptr);

    WaveSource waveSource() const;
    void setWaveSource(WaveSource ws);

signals:
    void waveSourceChanged();

private:
    WaveSource m_waveSource;
};
}

#endif // AU_PROJECTSCENE_CLIPITEM_H
