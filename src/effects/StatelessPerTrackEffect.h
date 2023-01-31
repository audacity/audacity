/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file StatelessPerTrackEffect.h
 
 Paul Licameli
 
 **********************************************************************/
#ifndef __AUDACITY_STATELESS_PER_TRACK_EFFECT__
#define __AUDACITY_STATELESS_PER_TRACK_EFFECT__

#include "PerTrackEffect.h"

class StatelessPerTrackEffect
   : public PerTrackEffect
{
public:
   ~StatelessPerTrackEffect() override;
};

#endif
