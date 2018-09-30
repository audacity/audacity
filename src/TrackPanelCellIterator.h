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
#include "MemoryX.h"

class Track;
class TrackPanelCell;

class TrackPanel;

// A class that allows iteration over the rectangles of visible cells.
class TrackPanelCellIterator
   : public std::iterator<
      std::forward_iterator_tag,
      const std::pair< std::shared_ptr< TrackPanelCell >, wxRect>
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
      return lhs.mpCell == rhs.mpCell &&
         lhs.mDidBackground == rhs.mDidBackground;
   }

   value_type operator * () const;

private:
   void UpdateRect();

   TrackPanel *mPanel;
   TrackIter<Track> mIter;
   std::shared_ptr<Track> mpTrack;
   std::shared_ptr<TrackPanelCell> mpCell;
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
