/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file PendingTracks.h
 
 Paul Licameli
 
 **********************************************************************/
#ifndef __AUDACITY_PENDING_TRACKS__
#define __AUDACITY_PENDING_TRACKS__

#include "ClientData.h"

class AudacityProject;

class TRACK_API PendingTracks final
   : public ClientData::Base
{
public:
   static PendingTracks &Get(AudacityProject &project);
   static const PendingTracks &Get(const AudacityProject &project);
   ~PendingTracks();
};

#endif
