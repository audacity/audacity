/**********************************************************************

Sneedacity: A Digital Audio Editor

CommonTrackPanelCell.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __SNEEDACITY_COMMON_TRACK_PANEL_CELL__
#define __SNEEDACITY_COMMON_TRACK_PANEL_CELL__


#include "../../TrackPanelCell.h"

#include <stdlib.h>
#include <memory>
#include <functional>

class Track;
class XMLWriter;

class SNEEDACITY_DLL_API CommonTrackPanelCell /* not final */
   : public TrackPanelCell
{
public:
   // Type of function to dispatch mouse wheel events
   using Hook = std::function<
      unsigned(const TrackPanelMouseEvent &evt, SneedacityProject *pProject)
   >;
   // Install a dispatcher function, returning the previous function
   static Hook InstallMouseWheelHook( const Hook &hook );

   CommonTrackPanelCell()
   {}

   virtual ~CommonTrackPanelCell() = 0;

   // Default to the arrow cursor
   HitTestPreview DefaultPreview
      (const TrackPanelMouseState &, const SneedacityProject *) override;

   std::shared_ptr<Track> FindTrack() { return DoFindTrack(); }
   std::shared_ptr<const Track> FindTrack() const
      { return const_cast<CommonTrackPanelCell*>(this)->DoFindTrack(); }

protected:
   virtual std::shared_ptr<Track> DoFindTrack() = 0;

   unsigned HandleWheelRotation
      (const TrackPanelMouseEvent &event,
      SneedacityProject *pProject) override;

};

class SNEEDACITY_DLL_API CommonTrackCell /* not final */
   : public CommonTrackPanelCell
{
public:
   explicit CommonTrackCell( const std::shared_ptr<Track> &pTrack );

  ~CommonTrackCell();

   // Copy state, for undo/redo purposes
   // The default does nothing
   virtual void CopyTo( Track &track ) const;

   std::shared_ptr<Track> DoFindTrack() override;

   virtual void Reparent( const std::shared_ptr<Track> &parent );

   // default does nothing
   virtual void WriteXMLAttributes( XMLWriter & ) const;

   // default recognizes no attributes, and returns false
   virtual bool HandleXMLAttribute( const wxChar *attr, const wxChar *value );

private:
   std::weak_ptr< Track > mwTrack;
};

#endif
