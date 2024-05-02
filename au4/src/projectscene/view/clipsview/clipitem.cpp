#include "clipitem.h"

using namespace au::projectscene;

ClipItem::ClipItem(QObject* parent)
    : QObject(parent)
{}

WaveSource ClipItem::waveSource() const
{
    return m_waveSource;
}

void ClipItem::setWaveSource(WaveSource ws)
{
    m_waveSource = ws;
    emit waveSourceChanged();
}
