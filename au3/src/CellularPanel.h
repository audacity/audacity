/**********************************************************************

 Audacity: A Digital Audio Editor

 CellularPanel.h

 Paul Licameli

 **********************************************************************/

#ifndef __AUDACITY_CELLULAR_PANEL__
#define __AUDACITY_CELLULAR_PANEL__

#include "widgets/OverlayPanel.h" // to inherit

class ViewInfo;
class AudacityProject;

class TrackPanelCell;
struct TrackPanelDrawingContext;
class TrackPanelGroup;
class TrackPanelNode;
struct TrackPanelMouseEvent;
struct TrackPanelMouseState;
class TranslatableString;

class UIHandle;
using UIHandlePtr = std::shared_ptr<UIHandle>;

// This class manages a panel divided into a number of sub-rectangles called
// cells, that each implement hit tests returning click-drag-release handler
// objects, and other services.
// It has no dependency on the Track class.
class AUDACITY_DLL_API CellularPanel : public OverlayPanel
{
public:
    CellularPanel(wxWindow* parent, wxWindowID id, const wxPoint& pos, const wxSize& size, ViewInfo* viewInfo,
                  // default as for wxPanel:
                  long style = wxTAB_TRAVERSAL | wxNO_BORDER);
    ~CellularPanel() override;

    // Overridables:

    virtual AudacityProject* GetProject() const = 0;

    // Get the root object defining a recursive subdivision of the panel's
    // area into cells
    virtual std::shared_ptr<TrackPanelNode> Root() = 0;

    virtual std::shared_ptr<TrackPanelCell> GetFocusedCell() = 0;
    virtual void SetFocusedCell() = 0;

    virtual void ProcessUIHandleResult(TrackPanelCell* pClickedCell, TrackPanelCell* pLatestCell, unsigned refreshResult) = 0;

    virtual void UpdateStatusMessage(const TranslatableString&)  = 0;

public:
    // Structure and functions for generalized visitation of the subdivision
    struct Visitor {
        virtual ~Visitor();
        virtual void VisitCell(const wxRect& rect, TrackPanelCell& cell);
        virtual void BeginGroup(const wxRect& rect, TrackPanelGroup& group);
        virtual void EndGroup(const wxRect& rect, TrackPanelGroup& group);
    };

    // Most general visit
    void Visit(Visitor& visitor);

    // Easier visit when you care only about cells
    using SimpleCellVisitor
        =std::function< void (const wxRect& rect, TrackPanelCell& cell) >;
    void VisitCells(const SimpleCellVisitor& visitor);

    // Easier visits when you want to visit each node once only
    using SimpleNodeVisitor
        =std::function< void (const wxRect& rect, TrackPanelNode& node) >;
    void VisitPreorder(const SimpleNodeVisitor& visitor);
    void VisitPostorder(const SimpleNodeVisitor& visitor);

    // Find cell by coordinate
    struct FoundCell {
        std::shared_ptr< TrackPanelCell > pCell;
        wxRect rect;
    };

    FoundCell FindCell(int mouseX, int mouseY);

    // Search the tree of subdivisions of the panel area for the given cell.
    // If more than one sub-area is associated with the same cell object, it
    // is not specified which rectangle is returned.
    wxRect FindRect(const TrackPanelCell& cell);

    // Search the tree of subdivisions of the panel area for a node (group or
    // cell) satisfying the predicate. If more than one sub-area is associated
    // with some node satisfying the predicate, it is not specified which
    // rectangle is returned.
    wxRect FindRect(const std::function< bool(TrackPanelNode&) >& pred);

    UIHandlePtr Target();

    std::shared_ptr<TrackPanelCell> LastCell() const;

    bool IsMouseCaptured();

    wxCoord MostRecentXCoord() const;

    void HandleCursorForPresentMouseState(bool doHit = true);

    // Visit the Draw functions of all cells that intersect the panel area,
    // and of handles associated with such cells,
    // and of all groups of cells,
    // repeatedly with a pass count from 0 to nPasses - 1
    void Draw(TrackPanelDrawingContext& context, unsigned nPasses);

protected:
    bool HasEscape();
    bool CancelDragging(bool escaping);
    void DoContextMenu(std::shared_ptr<TrackPanelCell> pCell);
    void ClearTargets();

private:
    void Visit(
        const wxRect& rect, const std::shared_ptr<TrackPanelNode>& node, Visitor& visitor);

    bool HasRotation();
    bool ChangeTarget(bool forward, bool cycle);

    void OnMouseEvent(wxMouseEvent& event);
    void OnCaptureLost(wxMouseCaptureLostEvent& event);
    void OnCaptureKey(wxCommandEvent& event);
    void OnKeyDown(wxKeyEvent& event);
    void OnChar(wxKeyEvent& event);
    void OnKeyUp(wxKeyEvent& event);

    void OnSetFocus(wxFocusEvent& event);
    void OnKillFocus(wxFocusEvent& event);
    void DoKillFocus();

    void OnContextMenu(wxContextMenuEvent& event);

    void HandleInterruptedDrag();
    void Uncapture(bool escaping, wxMouseState* pState = nullptr);
    bool HandleEscapeKey(bool down);
    void UpdateMouseState(const wxMouseState& state);
    void HandleModifierKey();

    void HandleClick(const TrackPanelMouseEvent& tpmEvent);
    void HandleWheelRotation(TrackPanelMouseEvent& tpmEvent);

    void HandleMotion(wxMouseState& state, bool doHit = true);
    void HandleMotion(const TrackPanelMouseState& tpmState, bool doHit = true);
    void Leave();

protected:
    ViewInfo* mViewInfo;

    // To do: make a drawing method and make this private
    wxMouseState mLastMouseState;

private:
    struct State;
    std::unique_ptr<State> mState;

    struct Filter;

    DECLARE_EVENT_TABLE()
};

#endif
