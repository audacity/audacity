/**********************************************************************

 Audacity: A Digital Audio Editor

 @file UndoTracks.h

 Paul Licameli

 **********************************************************************/
#ifndef __AUDACITY_UNDO_TRACKS__
#define __AUDACITY_UNDO_TRACKS__

class TrackList;
struct UndoStackElem;

namespace UndoTracks {
TRACK_API TrackList* Find(const UndoStackElem& state);
}

#endif
