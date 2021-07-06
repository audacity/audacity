/**********************************************************************

 Sneedacity: A Digital Audio Editor

 PlayableTrackControls.cpp

 Paul Licameli split from TrackInfo.cpp

 **********************************************************************/

#ifndef __SNEEDACITY_PLAYABLE_TRACK_CONTROLS__
#define __SNEEDACITY_PLAYABLE_TRACK_CONTROLS__

#include "../../ui/CommonTrackControls.h"

class wxRect;
class Track;

class SNEEDACITY_DLL_API PlayableTrackControls /* not final */
   : public CommonTrackControls
{
public:
   // To help subclasses define GetTCPLines
   static const TCPLines& StaticTCPLines();

   static void GetMuteSoloRect(
      const wxRect & rect, wxRect & dest, bool solo, bool bHasSoloButton,
      const Track *pTrack);

   using CommonTrackControls::CommonTrackControls;
};

#endif
