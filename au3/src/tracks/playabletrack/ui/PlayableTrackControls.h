/**********************************************************************

 Audacity: A Digital Audio Editor

 PlayableTrackControls.cpp

 Paul Licameli split from TrackInfo.cpp

 **********************************************************************/

#ifndef __AUDACITY_PLAYABLE_TRACK_CONTROLS__
#define __AUDACITY_PLAYABLE_TRACK_CONTROLS__

#include "../../ui/CommonTrackControls.h"

class wxRect;
class Track;

class AUDACITY_DLL_API PlayableTrackControls /* not final */ : public CommonTrackControls
{
public:
    // To help subclasses define GetTCPLines
    static const TCPLines& StaticNoteTCPLines();
    static const TCPLines& StaticWaveTCPLines();

    static void GetMuteSoloRect(
        const wxRect& rect, wxRect& dest, bool solo, const Track* pTrack);

    static void GetEffectsButtonRect(
        const wxRect& rect, wxRect& dest, const Track* pTrack);

    using CommonTrackControls::CommonTrackControls;
};

#endif
