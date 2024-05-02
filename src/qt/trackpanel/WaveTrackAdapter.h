#pragma once

#include "Observer.h"
#include "TrackAdapter.h"

struct TrackListEvent;
class WaveTrack;

class WaveTrackAdapter : public TrackAdapterBase
{
   Q_OBJECT
   QML_ELEMENT
   QML_UNCREATABLE("")

   Q_PROPERTY(QString name READ getName WRITE setName NOTIFY nameChanged)
   Q_PROPERTY(qreal gain READ getGain WRITE setGain NOTIFY gainChanged)
   Q_PROPERTY(qreal pan READ getPan WRITE setPan NOTIFY panChanged)
   Q_PROPERTY(bool solo READ getSolo WRITE setSolo NOTIFY soloChanged)
   Q_PROPERTY(bool mute READ getMute WRITE setMute NOTIFY muteChanged)
   Q_PROPERTY(int channels READ getChannels CONSTANT)

   std::shared_ptr<WaveTrack> mWaveTrack;
public:

   WaveTrackAdapter(std::shared_ptr<WaveTrack> waveTrack, QObject* parent = nullptr);
   ~WaveTrackAdapter() override;

   Track* GetTrack() override;
   QString GetType() override;

   WaveTrack* GetAsWaveTrack();


   //
   QString getName() const;
   void setName(const QString& name);

   qreal getGain() const;
   void setGain(qreal gain);

   qreal getPan() const;
   void setPan(qreal pan);

   bool getSolo() const;
   void setSolo(bool solo);

   bool getMute() const;
   void setMute(bool mute);

   int getChannels() const;

signals:

   void nameChanged(QString);
   void gainChanged(qreal);
   void panChanged(qreal);
   void soloChanged(bool);
   void muteChanged(bool);

private:

   QString mName;
   qreal mGain;
   qreal mPan;
   bool mSolo;
   bool mMute;

   void OnTrackListEvent(TrackListEvent event);

   Observer::Subscription mTrackListEventSubscription;

};
