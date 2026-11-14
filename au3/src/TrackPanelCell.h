/**********************************************************************

Audacity: A Digital Audio Editor

TrackPanelCell.h

Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_TRACK_PANEL_CELL__
#define __AUDACITY_TRACK_PANEL_CELL__

#include <memory>
#include "TrackPanelDrawable.h" // to inherit

class AudacityProject;
struct HitTestPreview;
struct TrackPanelDrawingContext;
struct TrackPanelMouseEvent;
struct TrackPanelMouseState;
class ViewInfo;
class wxKeyEvent;
class wxPoint;
class wxRect;
class wxWindow;

class UIHandle;
using UIHandlePtr = std::shared_ptr<UIHandle>;

#include <vector>

/// \brief The TrackPanel is built up of nodes, subtrees of the CellularPanel's area
/// Common base class for TrackPanelCell (leaf) and TrackPanelGroup (nonleaf)
class AUDACITY_DLL_API /* not final */ TrackPanelNode : public TrackPanelDrawable
{
public:
    TrackPanelNode();
    virtual ~TrackPanelNode() = 0;
};

// A node of the TrackPanel that contains other nodes.
class AUDACITY_DLL_API TrackPanelGroup /* not final */ : public TrackPanelNode
{
public:
    TrackPanelGroup();
    virtual ~TrackPanelGroup();

    enum class Axis {
        X, Y
    };

    // A refinement of a given rectangle partitions it along one of its axes
    // and associates TrackPanelNodes with the partition.
    // The sequence of coordinates should be increasing, giving left or top
    // coordinates of sub-rectangles.
    // Null pointers are permitted to define empty spaces with no cell object.
    // If the first coordinate is right of or below the rectangle boundary,
    // then that also defines an empty space at the edge.
    // Sub-rectangles may be defined partly or wholly out of the bounds of the
    // given rectangle.  Such portions are ignored.
    using Child = std::pair< wxCoord, std::shared_ptr<TrackPanelNode> >;
    using Refinement = std::vector< Child >;
    using Subdivision = std::pair< Axis, Refinement >;

    // Report a subdivision of one of the axes of the given rectangle
    virtual Subdivision Children(const wxRect& rect) = 0;
};

/// Abstract base class defining TrackPanel's access to specialist classes that
/// implement drawing and user interactions
class AUDACITY_DLL_API TrackPanelCell /* not final */ : public TrackPanelNode
{
public:
    TrackPanelCell() = default;
    TrackPanelCell(const TrackPanelCell&) = delete;
    TrackPanelCell& operator=(const TrackPanelCell&) = delete;

    virtual ~TrackPanelCell () = 0;

    // May supply default cursor, status message, and tooltip, when there is no
    // handle to hit at the mouse position, or the handle does not supply them.
    virtual HitTestPreview DefaultPreview(const TrackPanelMouseState& state, const AudacityProject* pProject);

    // Return pointers to objects that can be queried for a status
    // bar message and cursor appropriate to the point, and that dispatch
    // mouse button events.
    // The button-down state passed to the function is as it will be at click
    // time -- not necessarily as it is now.
    virtual std::vector<UIHandlePtr> HitTest(const TrackPanelMouseState& state, const AudacityProject* pProject) = 0;

    // Return value is a bitwise OR of RefreshCode values
    // Include Cancelled in the flags to indicate that the event is not handled.
    // Default does only that.
    virtual unsigned HandleWheelRotation(const TrackPanelMouseEvent& event, AudacityProject* pProject);

    // A cell may delegate context menu handling to another one
    virtual std::shared_ptr<TrackPanelCell> ContextMenuDelegate()
    { return {}; }

    // The pPosition parameter indicates mouse position but may be NULL
    // Return value is a bitwise OR of RefreshCode values
    // Default implementation does nothing
    virtual unsigned DoContextMenu(const wxRect& rect, wxWindow* pParent, const wxPoint* pPosition, AudacityProject* pProject);

    // Return value is a bitwise OR of RefreshCode values
    // Default skips the event and does nothing
    virtual unsigned CaptureKey(wxKeyEvent& event, ViewInfo& viewInfo, wxWindow* pParent, AudacityProject* project);

    // Return value is a bitwise OR of RefreshCode values
    // Default skips the event and does nothing
    virtual unsigned KeyDown(wxKeyEvent& event, ViewInfo& viewInfo, wxWindow* pParent, AudacityProject* project);

    // Return value is a bitwise OR of RefreshCode values
    // Default skips the event and does nothing
    virtual unsigned KeyUp(wxKeyEvent& event, ViewInfo& viewInfo, wxWindow* pParent, AudacityProject* project);

    // Return value is a bitwise OR of RefreshCode values
    // Default skips the event and does nothing
    virtual unsigned Char(wxKeyEvent& event, ViewInfo& viewInfo, wxWindow* pParent, AudacityProject* project);

    // Return value is a bitwise OR of RefreshCode values
    // Notification to the focused cell that the CellularPanel is losing focus
    // Default does nothing, returns RefreshCode::RefreshNone
    virtual unsigned LoseFocus(AudacityProject* project);
};

#endif
