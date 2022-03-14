/**********************************************************************

Audacity: A Digital Audio Editor

CommonTrackPanelCell.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_COMMON_TRACK_PANEL_CELL__
#define __AUDACITY_COMMON_TRACK_PANEL_CELL__

#include "TrackPanelCell.h"
#include "TrackAttachment.h" // to inherit

#include <stdlib.h>
#include <memory>
#include <functional>
#include "ComponentInterfaceSymbol.h"
#include "GlobalVariable.h"

#include "XMLTagHandler.h"

class CommandContext;
class Track;
class XMLWriter;

class AUDACITY_DLL_API CommonTrackPanelCell /* not final */
   : public TrackPanelCell
{
public:
   // Function to dispatch mouse wheel events
   struct AUDACITY_DLL_API MouseWheelHook : GlobalHook<MouseWheelHook,
      unsigned(const TrackPanelMouseEvent &evt, AudacityProject *pProject)
   >{};

   CommonTrackPanelCell()
   {}

   virtual ~CommonTrackPanelCell() = 0;

   // Default to the arrow cursor
   HitTestPreview DefaultPreview
      (const TrackPanelMouseState &, const AudacityProject *) override;

   std::shared_ptr<Track> FindTrack() { return DoFindTrack(); }
   std::shared_ptr<const Track> FindTrack() const
      { return const_cast<CommonTrackPanelCell*>(this)->DoFindTrack(); }

   struct MenuItem {
      using Action = std::function< void(const CommandContext &) >;

      MenuItem() = default;
      MenuItem( const Identifier &internal, const TranslatableString &msgid,
         Action action = {}, bool enabled = true )
         : symbol{ internal, msgid }, action{ move(action) }, enabled{ enabled }
      {}

      ComponentInterfaceSymbol symbol;
      Action action;
      bool enabled;
   };

   //! Return a list of items for DoContextMenu() (empties for separators)
   /*! If the vector is empty (as in the default), there is no context menu.

    Commands are invoked with temporary selection fields of CommandContext
    set to a point selected region at the mouse pick, and the cell's
    track.

    A function may be given, but if it is null, then the command can be found by
    name in the CommandManager.

    An item in the list with no command name marks a menu separator.

    The menu item is enabled only if it contains a true flag, but if looked up
    in the command manager, it must also satisfy the command manager's
    conditions.
    */
   virtual std::vector<MenuItem> GetMenuItems(
      const wxRect &rect, const wxPoint *pPosition, AudacityProject *pProject );

protected:
   virtual std::shared_ptr<Track> DoFindTrack() = 0;

   unsigned DoContextMenu(
      const wxRect &rect,
      wxWindow *pParent, const wxPoint *pPosition, AudacityProject *pProject)
   override;

   unsigned HandleWheelRotation
      (const TrackPanelMouseEvent &event,
      AudacityProject *pProject) override;

};

class AUDACITY_DLL_API CommonTrackCell /* not final */
   : public CommonTrackPanelCell, public TrackAttachment
{
public:
   explicit CommonTrackCell( const std::shared_ptr<Track> &pTrack );

  ~CommonTrackCell();

   std::shared_ptr<Track> DoFindTrack() override;

   void Reparent( const std::shared_ptr<Track> &parent ) override;

private:
   std::weak_ptr< Track > mwTrack;
};

#endif
