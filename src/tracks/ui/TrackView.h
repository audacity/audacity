/**********************************************************************

Audacity: A Digital Audio Editor

TrackView.h

Paul Licameli split from class Track

**********************************************************************/

#ifndef __AUDACITY_TRACK_VIEW__
#define __AUDACITY_TRACK_VIEW__

#include <memory>
#include "CommonTrackPanelCell.h" // to inherit

class Track;
class TrackList;
class TrackVRulerControls;
class TrackPanelResizerCell;

class AUDACITY_DLL_API TrackView /* not final */ : public CommonTrackCell
   , public std::enable_shared_from_this<TrackView>
{
   TrackView( const TrackView& ) = delete;
   TrackView &operator=( const TrackView& ) = delete;

public:
   enum : unsigned { DefaultHeight = 150 };

   explicit
   TrackView( const std::shared_ptr<Track> &pTrack );
   virtual ~TrackView() = 0;

   // some static conveniences, useful for summation over track iterator
   // ranges
   static int GetTrackHeight( const Track *pTrack );
   static int GetChannelGroupHeight( const Track *pTrack );
   // Total height of the given track and all previous ones (constant time!)
   static int GetCumulativeHeight( const Track *pTrack );
   static int GetTotalHeight( const TrackList &list );

   // Copy view state, for undo/redo purposes
   void CopyTo( Track &track ) const override;

   static TrackView &Get( Track & );
   static const TrackView &Get( const Track & );

   bool GetMinimized() const { return mMinimized; }
   void SetMinimized( bool minimized );

   int GetY() const { return mY; }
   int GetActualHeight() const { return mHeight; }
   virtual int GetMinimizedHeight() const = 0;
   int GetHeight() const;

   void SetY(int y) { DoSetY( y ); }
   void SetHeight(int height);

   // Return another, associated TrackPanelCell object that implements the
   // mouse actions for the vertical ruler
   std::shared_ptr<TrackVRulerControls> GetVRulerControls();
   std::shared_ptr<const TrackVRulerControls> GetVRulerControls() const;

   // by default returns nullptr, meaning that track has no drag controls area
   std::shared_ptr<CommonTrackCell> GetAffordanceControls();


   void WriteXMLAttributes( XMLWriter & ) const override;
   bool HandleXMLAttribute( const wxChar *attr, const wxChar *value ) override;

   // New virtual function.  The default just returns a one-element array
   // containing this.  Overrides might refine the Y axis.
   using Refinement = std::vector< std::pair<
      wxCoord, std::shared_ptr< TrackView >
   > >;
   virtual Refinement GetSubViews( const wxRect &rect );

   // default is false
   virtual bool IsSpectral() const;

   virtual void DoSetMinimized( bool isMinimized );

protected:

   // No need yet to make this virtual
   void DoSetY(int y);

   void DoSetHeight(int h);

   // Private factory to make appropriate object; class TrackView handles
   // memory management thereafter
   virtual std::shared_ptr<TrackVRulerControls> DoGetVRulerControls() = 0;
   // May return nullptr (which is default) if track does not need affordance area
   virtual std::shared_ptr<CommonTrackCell> DoGetAffordanceControls();

   std::shared_ptr<TrackVRulerControls> mpVRulerControls;
   std::shared_ptr<CommonTrackCell> mpAffordanceCellControl;

private:
   bool           mMinimized{ false };
   int            mY{ 0 };
   int            mHeight{ DefaultHeight };
};

#include "../../AttachedVirtualFunction.h"

struct DoGetViewTag;

using DoGetView =
AttachedVirtualFunction<
   DoGetViewTag,
   std::shared_ptr< TrackView >,
   Track
>;

struct GetDefaultTrackHeightTag;

using GetDefaultTrackHeight =
AttachedVirtualFunction<
   GetDefaultTrackHeightTag,
   int,
   Track
>;

#endif
