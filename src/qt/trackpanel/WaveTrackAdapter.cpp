#include "WaveTrackAdapter.h"

#include "CodeConversions.h"
#include "WaveTrack.h"

WaveTrackAdapter::WaveTrackAdapter(std::shared_ptr<WaveTrack> waveTrack, QObject* parent)
   : TrackAdapterBase(parent), mWaveTrack(std::move(waveTrack))
{
   if(auto owner = mWaveTrack->GetOwner())
      mTrackListEventSubscription = owner->Subscribe(*this, &WaveTrackAdapter::OnTrackListEvent);

   mName = audacity::ToQString(mWaveTrack->GetName());
   mGain = mWaveTrack->GetGain();
   mPan = mWaveTrack->GetPan();
   mSolo = mWaveTrack->GetSolo();
   mMute = mWaveTrack->GetMute();
}

WaveTrackAdapter::~WaveTrackAdapter() = default;

Track* WaveTrackAdapter::GetTrack()
{
   return mWaveTrack.get();
}

QString WaveTrackAdapter::GetType()
{
   return "waveTrack";
}

WaveTrack* WaveTrackAdapter::GetAsWaveTrack()
{
   return mWaveTrack.get();
}

QString WaveTrackAdapter::getName() const
{
   return mName;
}

void WaveTrackAdapter::setName(const QString& name)
{
   const auto tmp = audacity::ToWXString(name);
   if(tmp != mWaveTrack->GetName())
   {
      mName = name;
      mWaveTrack->SetName(audacity::ToWXString(name));
      emit nameChanged(name);
   }
}

qreal WaveTrackAdapter::getGain() const
{
   return mGain;
}

void WaveTrackAdapter::setGain(qreal gain)
{
   if(mWaveTrack->GetGain() != gain)
   {
      mGain = gain;
      mWaveTrack->SetGain(gain);
      emit gainChanged(gain);
   }
}

qreal WaveTrackAdapter::getPan() const
{
   return mPan;
}

void WaveTrackAdapter::setPan(qreal pan)
{
   if(mWaveTrack->GetPan() != pan)
   {
      mPan = pan;
      mWaveTrack->SetPan(pan);
      emit panChanged(pan);
   }
}

bool WaveTrackAdapter::getSolo() const
{
   return mSolo;
}

void WaveTrackAdapter::setSolo(bool solo)
{
   if(mWaveTrack->GetSolo() != solo)
   {
      mSolo = solo;
      mWaveTrack->SetSolo(solo);
      emit soloChanged(solo);
   }
}

bool WaveTrackAdapter::getMute() const
{
   return mMute;
}

void WaveTrackAdapter::setMute(bool mute)
{
   if(mWaveTrack->GetMute() != mute)
   {
      mMute = mute;
      mWaveTrack->SetMute(mute);
      emit muteChanged(mute);
   }
}

void WaveTrackAdapter::OnTrackListEvent(TrackListEvent event)
{
   if(event.mType == TrackListEvent::TRACK_DATA_CHANGE && event.mpTrack.lock() == mWaveTrack)
   {
      {
         const auto name = audacity::ToQString(mWaveTrack->GetName());
         if(name != mName)
         {
            mName = name;
            emit nameChanged(name);
         }
      }
      {
         const auto gain = mWaveTrack->GetGain();
         if(gain != mGain)
         {
            mGain = gain;
            emit gainChanged(gain);
         }
      }
      {
         const auto pan = mWaveTrack->GetPan();
         if(pan != mPan)
         {
            mPan = pan;
            emit panChanged(pan);
         }
      }
      {
         const auto solo = mWaveTrack->GetSolo();
         if(solo != mSolo)
         {
            mSolo = solo;
            emit soloChanged(solo);
         }
      }
      {
         const auto mute = mWaveTrack->GetMute();
         if(mute != mMute)
         {
            mMute = mute;
            emit muteChanged(mute);
         }
      }
   }
}

template <>
TrackAdapterBase* TrackAdapterBase::Create<WaveTrack>(WaveTrack& track, QObject* parent)
{
   return new WaveTrackAdapter(track.SharedPointer<WaveTrack>(), parent);
}
