/**********************************************************************

Audacity: A Digital Audio Editor

TrackPanelCellIterator.h

Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_TRACK_PANEL_CELL_ITERATOR__
#define __AUDACITY_TRACK_PANEL_CELL_ITERATOR__

#include "Track.h"
#include <wx/gdicmn.h>
#include <iterator>

class Track;
class TrackPanelCell;

class TrackPanel;

// A class that allows iteration over the rectangles of visible cells.
class TrackPanelCellIterator
   : public std::iterator<
      std::forward_iterator_tag,
      const std::pair<TrackPanelCell*, wxRect>
   >
{
public:
   enum class CellType {
      Track, Label, VRuler, Resizer, Background
   };

   TrackPanelCellIterator(TrackPanel *trackPanel, bool begin);

   // implement the STL iterator idiom

   TrackPanelCellIterator &operator++ ();
   TrackPanelCellIterator operator++ (int);

   friend inline bool operator==
      (const TrackPanelCellIterator &lhs, const TrackPanelCellIterator &rhs)
   {
      return lhs.mpCell == rhs.mpCell;
   }

   using value_type = std::pair<TrackPanelCell*, wxRect>;
   value_type operator * () const;

private:
   void UpdateRect();

   TrackPanel *mPanel;
   VisibleTrackIterator mIter;
   Track *mpTrack;
   TrackPanelCell *mpCell;
   CellType mType{ CellType::Track };
   bool mDidBackground{ false };
   wxRect mRect;
};

inline TrackPanelCellIterator::CellType &operator++
( TrackPanelCellIterator::CellType &type )
{
   type = TrackPanelCellIterator::CellType( 1 + int( type ) );
   return type;
}

inline bool operator !=
(const TrackPanelCellIterator &lhs, const TrackPanelCellIterator &rhs)
{
   return !(lhs == rhs);
}
#endif
