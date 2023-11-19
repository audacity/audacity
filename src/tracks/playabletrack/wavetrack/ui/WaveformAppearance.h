/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file WaveformApperance.h
 
 Paul Licameli
 
 **********************************************************************/
#ifndef __AUDACITY_WAVEFORM_APPEARANCE__
#define __AUDACITY_WAVEFORM_APPEARANCE__

#include "WaveTrack.h"

#include "Track.h"

//! Persistent appearance settings that apply to all channels of a track
class WaveformAppearance : public TrackAttachment
{
public:
   static WaveformAppearance &Get(WaveChannel &channel);
   static const WaveformAppearance &Get(const WaveChannel &channel);

   explicit WaveformAppearance(WaveTrack &track);
   ~WaveformAppearance() override;

   void Reparent(const std::shared_ptr<Track> &parent) override;

private:
   std::weak_ptr<WaveTrack> mwTrack;
};

#endif
