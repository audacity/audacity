/**********************************************************************

  Audacity: A Digital Audio Editor

  ToolDock.cpp

  Dominic Mazzoni
  Shane T. Mueller
  Leland Lucius

*******************************************************************//**

\file ToolDock.cpp

  Implements ToolDock

*//*******************************************************************//**

\class ToolDock
\brief A dynamic panel where a ToolBar can be docked.

*//**********************************************************************/

#include "ToolDock.h"

#include <wx/tokenzr.h>

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#ifndef WX_PRECOMP
#include <wx/dcclient.h>
#include <wx/defs.h>
#include <wx/gdicmn.h>
#include <wx/panel.h>
#include <wx/settings.h>
#include <wx/window.h>
#endif  /*  */

#include <algorithm>

#include "AColor.h"
#include "AllThemeResources.h"
#include "ImageManipulation.h"
#include "Prefs.h"
#include "../widgets/Grabber.h"

const ToolBarConfiguration::Position
ToolBarConfiguration::UnspecifiedPosition { false };

auto ToolBarConfiguration::FindPlace(const ToolBar* bar) const
-> Iterator
{
    auto This = const_cast<ToolBarConfiguration*>(this);
    return std::find_if(This->begin(), This->end(),
                        [=](const Place& place){
        return place.pTree->pBar == bar;
    });
}

auto ToolBarConfiguration::FindPeers(const ToolBar* bar)
-> std::pair<Forest*, Forest::iterator>
{
    auto findTree = [=](Forest& forest){
        return std::find_if(forest.begin(), forest.end(),
                            [=](const Tree& tree){ return tree.pBar == bar; });
    };

    auto iter1 = findTree(mForest);
    if (iter1 != mForest.end()) {
        return { &mForest, iter1 }
    }

    Forest::iterator result;
    auto iter = std::find_if(begin(), end(),
                             [&](const Place& place){
        auto& children = place.pTree->children;
        return (result = findTree(children)) != children.end();
    }
                             );
    if (iter != end()) {
        return { &iter->pTree->children, result }
    }

    return { nullptr, Forest::iterator{} };
}

auto ToolBarConfiguration::Find(const ToolBar* bar) const -> Position
{
    auto iter = FindPlace(bar);
    if (iter == end()) {
        return UnspecifiedPosition;
    } else {
        return iter->position;
    }
}

ToolBar* ToolBarConfiguration::FindToolBar(Identifier toolBarID) const
{
    if (toolBarID.empty()) {
        return nullptr;
    }

    auto This = const_cast<ToolBarConfiguration*>(this);
    auto it = std::find_if(
        This->begin(), This->end(),
        [=](const Place& place)
    { return place.pTree->pBar->GetSection() == toolBarID; });

    return it != end() ? it->pTree->pBar : nullptr;
}

void ToolBarConfiguration::Insert(ToolBar* bar, Position position)
{
    if (position == UnspecifiedPosition) {
        // Add at the "end" of the layout
        // bottommost and rightmost
        Forest* pForest = &mForest;
        while (!pForest->empty()) {
            pForest = &pForest->back().children;
        }
        pForest->push_back(Tree {});
        pForest->back().pBar = bar;
    } else {
        // Insert at what depth?
        auto pForest = &mForest;
        if (position.rightOf) {
            const auto parent = FindPlace(position.rightOf);
            if (parent != end()) {
                // Insert among children of some node
                pForest = &parent->pTree->children;
            }
        } else {
            // Insert a new root in the top forest
        }

        // Insert at what breadth?
        const auto begin = pForest->begin();
        auto iter = begin;
        const auto end = pForest->end();
        bool adopt = false;

        if (position.below) {
            iter = std::find_if(begin, end,
                                [=](const Tree& tree){ return tree.pBar == position.below; }
                                );
            if (iter != end) {
                ++iter;
                if (iter != end) {
                    adopt = true;
                }
            } else {
                // Not found, default to topmost
                iter = begin;
            }
        } else {
            // No previous sibling specified, so insert as first
            adopt = (iter != end);
        }

        // Insert as a leaf, or as an internal node?
        if (adopt && position.adopt) {
            // Existing children of parent become grandchildren

            // Make NEW node
            Tree tree;
            tree.pBar = bar;

            // Do adoption
            const auto barHeight = bar->GetSize().GetY() + toolbarGap;
            auto totalHeight = 0;
            while (iter != pForest->end()
                   && barHeight
                   >= (totalHeight += (iter->pBar->GetSize().GetY() + toolbarGap))) {
                tree.children.push_back(Tree {});
                auto& child = tree.children.back();
                child.pBar = iter->pBar;
                child.children.swap(iter->children);
                iter = pForest->erase(iter);
            }

            // Put the node in the tree
            iter = pForest->insert(iter, Tree {});
            (*iter).swap(tree);
        } else {
            // Insert as a leaf
            pForest->insert(iter, Tree {})->pBar = bar;
        }
    }
}

void ToolBarConfiguration::InsertAtPath
    (ToolBar* bar, const std::vector<int>& path)
{
    auto pForest = &mForest;
    Tree* pTree {};

    // Guarantee the existence of nodes
    for (auto ii : path) {
        Forest::size_type uu = std::max(0, ii);
        // This may make more than one default-constructed tree, which we
        // will fill in with some other call to InsertAtPath, or else cleanup
        // with RemoveNulls
        pForest->resize(std::max(uu + 1, pForest->size()));
        pTree = &(*pForest)[uu];
        pForest = &pTree->children;
    }

    if (pTree) {
        pTree->pBar = bar;
    }
}

void ToolBarConfiguration::Remove(Forest& forest, Forest::iterator iter)
{
    // Reparent all of the children of the deleted node
    Tree tree;
    tree.swap(*iter);
    iter = forest.erase(iter);
    auto& children = tree.children;
    auto cIter = children.rbegin(), cEnd = children.rend();
    while (cIter != cEnd) {
        iter = forest.insert(iter, Tree {});
        (*iter).swap(*cIter);
        ++cIter;
    }
}

void ToolBarConfiguration::Remove(const ToolBar* bar)
{
    auto results = FindPeers(bar);
    auto pForest = results.first;
    if (pForest) {
        // Reparent all of the children of the deleted node
        auto iter = results.second;
        wxASSERT(iter->pBar == bar);
        Remove(*pForest, iter);
    }
}

void ToolBarConfiguration::Show(ToolBar* bar)
{
    // Do not assume the bar is absent, though in practice that is always so
    if (!Contains(bar)) {
        auto position = UnspecifiedPosition;
        const auto preferredNeighbors = bar->PreferredNeighbors();
        if (!preferredNeighbors.first.empty()
            || !preferredNeighbors.second.empty()) {
            auto leftNeighbor = FindToolBar(preferredNeighbors.first);
            auto topNeighbor = FindToolBar(preferredNeighbors.second);

            // Perform a sanity check to verify that neighbors are
            // really inside this configuration
            if (leftNeighbor != nullptr || topNeighbor != nullptr) {
                position = Position { leftNeighbor, topNeighbor }
            }
        }
        Insert(bar, position);
    }
}

void ToolBarConfiguration::Hide(ToolBar* bar)
{
    // Future:  might hide a bar without eliminating it from the configuration
    Remove(bar);
}

bool ToolBarConfiguration::IsRightmost(const ToolBar* bar) const
{
    auto iter = FindPlace(bar);
    auto endit = end();
    if (iter == endit) {
        // not present
        return true;
    }
    if (++iter == endit) {
        // Last of all
        return true;
    }
    if (bar->GetRect().y != iter->pTree->pBar->GetRect().y) {
        // Next step in preorder traversal is not rightward to a child drawn at
        // the same height
        return true;
    }
    return false;
}

bool ToolBarConfiguration::Read
    (ToolBarConfiguration* pConfiguration,
    Legacy* pLegacy,
    ToolBar* bar, bool& visible, bool defaultVisible)
{
    bool result = true;

    // Future: might remember visibility in the configuration, not forgetting
    // positions of hidden bars.
    gPrefs->Read(wxT("Show"), &visible, defaultVisible);

    if (pConfiguration && visible) {
        int ord;
        gPrefs->Read(wxT("Order"), &ord, -1);
        // Index was written 1-based
        --ord;
        if (ord >= 0) {
            // Legacy preferences
            while (pLegacy->bars.size() <= size_t(ord)) {
                pLegacy->bars.push_back(nullptr);
            }
            pLegacy->bars[ord] = bar;
        } else {
            wxString strPath;
            gPrefs->Read(wxT("Path"), &strPath);
            if (!strPath.empty()) {
                wxStringTokenizer toker { strPath, wxT(",") };
                std::vector<int> path;
                while (toker.HasMoreTokens()) {
                    auto token = toker.GetNextToken();
                    auto ii = wxAtoi(token);
                    path.push_back(ii);
                }
                pConfiguration->InsertAtPath(bar, path);
            }
        }
    }

    return result;
}

void ToolBarConfiguration::RemoveNulls(Forest& forest)
{
    for (size_t ii = 0; ii < forest.size(); ++ii) {
        if (forest[ii].pBar == nullptr) {
            Remove(forest, forest.begin() + ii--);
        }
    }

    // Now do the same recursively
    for (auto& tree : forest) {
        RemoveNulls(tree.children);
    }
}

void ToolBarConfiguration::PostRead(Legacy& legacy)
{
    // Be sure no nodes contain NULL,
    // against the case of obsolete preferences, perhaps
    RemoveNulls(mForest);

    // Interpret what was saved in old .cfg files under "order"
    // which specified toolbar configuration simply as a sequence, not a tree
    ToolBar* prev {};
    for (auto pBar : legacy.bars) {
        if (!pBar) {
            continue;
        }

        Position position{ prev };
        Insert(pBar, position);

        prev = pBar;
    }
}

void ToolBarConfiguration::Write
    (const ToolBarConfiguration* pConfiguration, const ToolBar* bar)
{
    // Assume a path has been set in gPrefs suitable for bar
    if (pConfiguration) {
        // Write comma-separated list of numbers specifying position in the tree
        wxString strPath;
        const auto cIter = pConfiguration->FindPlace(bar);
        const auto path = cIter.GetPath();
        if (!path.empty()) {
            auto iter = path.begin(), end = path.end();
            strPath += wxString::Format(wxT("%d"), *iter++);
            while (iter != end) {
                strPath += wxString::Format(wxT(",%d"), *iter++);
            }
        }
        gPrefs->Write(wxT("Path"), strPath);

        // Remove any legacy configuration info.
        // Note:  this causes Audacity 2.1.2 and earlier to create toolbars
        // always in default position when reading a .cfg saved by Audacity
        // 2.1.3 or later
        gPrefs->DeleteEntry(wxT("Order"));
    }
    gPrefs->Write(wxT("Show"), bar->IsVisible());
}

IMPLEMENT_CLASS(ToolDock, wxPanelWrapper);

////////////////////////////////////////////////////////////
/// Methods for ToolDock
////////////////////////////////////////////////////////////

//
// Custom event
//
//DEFINE_EVENT_TYPE( EVT_TOOLBAR_FLOAT );

BEGIN_EVENT_TABLE(ToolDock, wxPanelWrapper)
EVT_GRABBER(wxID_ANY, ToolDock::OnGrabber)
EVT_ERASE_BACKGROUND(ToolDock::OnErase)
EVT_PAINT(ToolDock::OnPaint)
EVT_SIZE(ToolDock::OnSize)
EVT_MOUSE_EVENTS(ToolDock::OnMouseEvents)
END_EVENT_TABLE()

//
// Constructor
//
ToolDock::ToolDock(wxEvtHandler* manager, wxWindow* parent, int dockid)
    : wxPanelWrapper(parent, dockid, wxDefaultPosition, parent->GetSize())
{
    SetLabel(XO("ToolDock"));
    SetName(XO("ToolDock"));

    // Init
    mManager = manager;
    SetBackgroundColour(theTheme.Colour(clrMedium));
    SetLayoutDirection(wxLayout_LeftToRight);
    // Use for testing gaps
    // SetOwnBackgroundColour( wxColour( 255, 0, 0 ) );
}

//
// Destructor
//
ToolDock::~ToolDock()
{
}

//
// Remove the toolbar from our control
//
void ToolDock::Undock(ToolBar* bar)
{
    if (mConfiguration.Contains(bar)) {
        mConfiguration.Remove(bar);
    }
    mBars[ bar->GetSection() ] = nullptr;
}

//
// Handle ToolDock events
//
void ToolDock::Dock(ToolBar* bar, bool deflate, ToolBarConfiguration::Position position)
{
#ifndef __WXMAC__
    // Apply the deflate fix only on Mac, else you introduce the opposite bug on others
    deflate = false;
#endif

    // Adopt the toolbar into our family
    bar->Reparent(this);
    mBars[ bar->GetSection() ] = bar;

    // Reset size
    bar->SetSize(
        // Undo the expansion that was applied when un-docking
        bar->GetSize().x - (deflate ? 2 * ToolBarFloatMargin : 0),
        // Don't need to adjust y the same way.
        bar->GetDockedSize().y
        );

    // Park the NEW bar in the correct berth
    if (!mConfiguration.Contains(bar) && bar->IsVisible()) {
        mConfiguration.Insert(bar, position);
    }

    // Inform toolbar of change
    bar->SetDocked(this, false);
}

// Initial docking of bars
void ToolDock::LoadConfig()
{
    // Add all ordered toolbars
    for (const auto& place : GetConfiguration()) {
        auto bar = place.pTree->pBar;
        this->Dock(bar, false);
        // Show it -- hidden bars are not (yet) ever saved as part of a
        // configuration
        Expose(bar->GetSection(), true);
    }
    Updated();
}

// A policy object for the skeleton routine below
class ToolDock::LayoutVisitor
{
public:
    virtual void ModifySize
        (ToolBar*,
        const wxRect&,
        ToolBarConfiguration::Position,
        ToolBarConfiguration::Position,
        wxSize&)
    {
    }

    virtual void Visit(ToolBar* ct, wxPoint point) = 0;

    virtual bool ShouldVisitSpaces() = 0;

    virtual void FinalRect
        (const wxRect&, ToolBarConfiguration::Position)
    {
    }
};

// Skeleton routine common to insertion of a toolbar, and figuring out
// width-constrained layout of toolbars
void ToolDock::VisitLayout(LayoutVisitor& visitor,
                           ToolBarConfiguration* pWrappedConfiguration)
{
    if (pWrappedConfiguration) {
        pWrappedConfiguration->Clear();
    }

    // Get size of our parent since we haven't been sized yet
    int width, height;
    GetParent()->GetClientSize(&width, &height);
    width -= toolbarGap;
    height -= toolbarGap;

    // Rectangle of space to allocate
    wxRect main{ toolbarGap, toolbarGap,
                 // Allow limited width, but arbitrary height, for the root rectangle
                 width, std::numeric_limits<int>::max() };

    // For recording the nested subdivisions of the rectangle
    struct Item {
        Identifier section;
        Item* parent{};
        ToolBar* lastSib {};
        ToolBar* lastWrappedChild {};
        wxRect rect;
    };
    std::vector<Item> items(mBars.size());
    Item* layout = items.data();
    Item* next = layout;

    ToolBar* lastRoot {};
    ToolBar* lastWrappedRoot {};

    // Process all docked and visible toolbars
    for ( const auto& place : this->GetConfiguration()) {
        // Cache toolbar pointer
        const auto ct = place.pTree->pBar;

        // set up the chain of ancestors.
        const auto parent = place.position.rightOf;
        const auto section = ct->GetSection();
        auto& newItem = *next++;
        if (parent) {
            newItem.parent = std::find_if(layout, next - 1, [&](Item& item){
                return parent->GetSection() == item.section;
            });
        }
        // Mark the slots that really were visited, for final pass through
        // the spaces.
        newItem.section = section;

        ToolBar* prevSib;
        if (!parent) {
            prevSib = lastRoot;
            lastRoot = ct;
        } else {
            auto& sib = newItem.parent->lastSib;
            prevSib = sib;
            sib = ct;
        }
        auto prevPosition = ToolBarConfiguration::Position{ parent, prevSib };

        // Determine the size of the toolbar to fit, with advice from
        // the visitor object
        wxSize sz = ct->GetSize();
        {
            wxRect temp;
            temp.SetPosition(ct->GetParent()->ClientToScreen(ct->GetPosition()));
            temp.SetSize(sz);
            visitor.ModifySize(ct, temp, prevPosition, place.position, sz);
        }

        // Inflate the size to leave margins
        int tw = sz.GetWidth() + toolbarGap;
        int th = sz.GetHeight() + toolbarGap;

        // Choose the rectangle to subdivide
        // Find a box that we fit in by going up the tree as needed --
        // thus when parent space is exhausted, fall back on ancestors --
        // so if the tree has too much depth for the width of the
        // window, the toolbars may "wrap."
        // Can always fall back to the main rectangle even if the bar is too
        // wide.
        auto pItem = newItem.parent;
        auto pRect = pItem ? &pItem->rect : &main;
        while (pRect != &main)
        {
            // Get out if it will fit
            bool bTooWide = tw > pRect->GetWidth();
            // We'd like to be able to add a tall toolbar in at the start of a row,
            // even if there isn't enough height for it.
            // If so, we'd have to at least change how we calculate 'bTooHigh'.
            bool bTooHigh = th > pRect->GetHeight();
            //bTooHigh &= stack[stkcnt].GetWidth() < (width - toolbarGap);
            //bTooHigh = false;

            if (!bTooWide && !bTooHigh) {
                break;
            }

            auto parentItem = pItem->parent;
            if (!parentItem) {
                pItem = nullptr;
                pRect = &main;
            } else {
                pItem = parentItem;
                pRect = &pItem->rect;
            }
        }

        // Record where the toolbar wrapped
        ToolBar*& sib = pItem ? pItem->lastWrappedChild : lastWrappedRoot;
        ToolBarConfiguration::Position newPosition {
            pItem ? this->mBars[ pItem->section ] : nullptr,
            sib
        };
        sib = ct;
        if (pWrappedConfiguration) {
            pWrappedConfiguration->Insert(ct, newPosition);
        }

        // Place the toolbar at the upper left part of the rectangle.
        const auto cpos = pRect->GetPosition();
        visitor.Visit(ct, cpos);

        // Allocate an upper portion of the rectangle to this bar.
        pRect->y += th;
        pRect->height -= th;

        // A right portion of that upper portion remains available for
        // descendant bars and is remembered in the layout array.
        int x = cpos.x + tw;
        newItem.rect = wxRect{ x, cpos.y, width - x, th };
    }

    if (visitor.ShouldVisitSpaces()) {
        // Visit the fringe where NEW leaves of the tree could go

        // Sort top to bottom for definiteness, though perhaps not really needed
        // Do not use the parent pointers after this sort!
        std::sort(layout, next,
                  [](const Item& lhs, const Item& rhs){
            return lhs.rect.y < rhs.rect.y;
        }
                  );
        for (auto iter = layout; iter != next; ++iter) {
            const auto& item = *iter;
            const auto& rect = item.rect;

            auto globalRect = rect;
            globalRect.SetPosition(this->ClientToScreen(rect.GetPosition()));

            // Let the visitor determine size
            wxSize sz {};
            ToolBarConfiguration::Position
                position { this->mBars[ item.section ], item.lastWrappedChild },
            prevPosition {};
            visitor.ModifySize(nullptr, globalRect, prevPosition, position, sz);
            int tw = sz.GetWidth() + toolbarGap;
            int th = sz.GetHeight() + toolbarGap;

            // Test fit
            bool bTooWide = tw > rect.GetWidth();
            bool bTooHigh = th > rect.GetHeight();
            if (!bTooWide && !bTooHigh) {
                // Call visitor again to confirm the placement
                const auto cpos = rect.GetPosition();
                visitor.Visit(nullptr, cpos);
            }
        }
    }

    // Report the final bounding box of all the bars, and a position where
    // you can insert a NEW bar at bottom left.
    ToolBarConfiguration::Position finalPosition { nullptr, lastRoot };
    visitor.FinalRect(
        wxRect { toolbarGap, toolbarGap, main.width, main.y }, finalPosition
        );
}

//
// Layout the toolbars
//
void ToolDock::LayoutToolBars()
{
    struct SizeSetter final : public LayoutVisitor
    {
        SizeSetter (ToolDock* d)
            : dock{d} {}

        void Visit
            (ToolBar* bar, wxPoint point)
        override
        {
            // Place the toolbar
            if (bar) {
                bar->SetPosition(point);
            }
        }

        bool ShouldVisitSpaces() override
        {
            return false;
        }

        virtual void FinalRect
            (const wxRect& rect, ToolBarConfiguration::Position)
        override
        {
            // Set the final size of the dock window
            dock->SetMinSize(rect.GetSize());
        }

        ToolDock* dock;
    } sizeSetter {
        this
    };
    VisitLayout(sizeSetter, &mWrappedConfiguration);

    // Set tab order and layout internal controls.
    {
        ToolBar* lt{};
        for ( const auto& place : GetConfiguration()) {
            auto ct = place.pTree->pBar;
            if (lt) {
                ct->MoveAfterInTabOrder(lt);
            }
            lt = ct;
            // Bug 1371.
            // After a dock size change, the toolbars may need relaying inside.
            lt->Layout();
        }
    }

    // Clean things up
    Refresh(false);
}

// Determine the position where a NEW bar would be placed
//
// 'rect' will be the rectangle for the dock marker (black triangle)
ToolBarConfiguration::Position
ToolDock::PositionBar(ToolBar* t, const wxPoint& pos, wxRect& rect)
{
    // Set width and size, but we must still find x and y.
    rect = t->GetRect();

    using Position = ToolBarConfiguration::Position;
    Position result { ToolBarConfiguration::UnspecifiedPosition };
    struct Inserter : public LayoutVisitor
    {
        struct Stop {};

        Inserter(Position& p, wxRect& r, const wxPoint& pt, ToolBar* t)
            : result(p), rect(r), point(pt), tb(t)
        {}

        void ModifySize
            (ToolBar* ct,
            const wxRect& rectIn,
            ToolBarConfiguration::Position prevPosition,
            ToolBarConfiguration::Position position,
            wxSize& sz)
        override
        {
            // Maybe insert the NEW bar if it hasn't already been done
            // and is in the right place.

            // Does the location fall within this bar?
            if (rectIn.Contains(point)) {
                sz = tb->GetDockedSize();
                // Choose a position always, if there is a bar to displace.
                // Else, only if the fit is possible.
                if (ct || (sz.x <= rectIn.width && sz.y <= rectIn.height)) {
                    // May choose current or previous.
                    if (ct
                        && (sz.y < rectIn.height
                            || point.y < (rectIn.GetTop() + rectIn.GetBottom()) / 2)) {
                        // "Wedge" the bar into a crack alone, not adopting others,
                        // if either a short bar displaces a tall one, or else
                        // the displacing bar is at least at tall, but the pointer is
                        // in the upper half of the box.
                        usedPrev = true, result = prevPosition, result.adopt = false;
                    } else {
                        result = position;
                    }
                }
                // Now wait until the other callback below to discover x and y
            }
        }

        void Visit
            (ToolBar*, wxPoint pointIn)
        override
        {
            if (result != ToolBarConfiguration::UnspecifiedPosition) {
                // If we've placed it, we're done.
                rect.x = pointIn.x;
                rect.y = pointIn.y;
                if (usedPrev) {
                    rect.y -= tb->GetDockedSize().GetHeight() / 2;
                }

                throw Stop {};
            }
        }

        bool ShouldVisitSpaces() override
        {
            return true;
        }

        void FinalRect
            (const wxRect& finalRect, ToolBarConfiguration::Position finalPosition)
        override
        {
            if (result == ToolBarConfiguration::UnspecifiedPosition) {
                // Default of all other placements.
                result = finalPosition;
                wxPoint point1 { finalRect.GetLeft(), finalRect.GetBottom() };
                rect.SetPosition(point1);
            }
        }

        Position& result;
        wxRect& rect;
        const wxPoint point;
        ToolBar* const tb;
        bool usedPrev { false };
    } inserter {
        result, rect, pos, t
    };

    try {
        this->VisitLayout(inserter);
    } catch (const Inserter::Stop&) {}

    // rect is decided
    return result;
}

void ToolDock::WrapConfiguration(ToolBarConfiguration& backup)
{
    backup.Clear();
    backup.Swap(mConfiguration);
    mConfiguration.Swap(mWrappedConfiguration);
}

void ToolDock::RestoreConfiguration(ToolBarConfiguration& backup)
{
    mWrappedConfiguration.Clear();
    mWrappedConfiguration.Swap(mConfiguration);
    mConfiguration.Swap(backup);
}

//
// Set the visible/hidden state of a toolbar
//
void ToolDock::Expose(Identifier type, bool show)
{
    ToolBar* t = mBars[ type ];

    // Maintain the docked array
    const auto shown = mConfiguration.Shows(t);
    if (show && !shown) {
        mConfiguration.Show(t);
    } else if (!show && shown) {
        mConfiguration.Hide(t);
    }

    // Make it (dis)appear
    t->Expose(show);
}

//
// Queues an EVT_TOOLBAR_UPDATED command event to notify any
// interested parties of an updated toolbar or dock layout
//
void ToolDock::Updated()
{
    // Queue an update event
    wxCommandEvent e(EVT_TOOLBAR_UPDATED, GetId());
    GetParent()->GetEventHandler()->AddPendingEvent(e);
}

//
// Handle grabber clicking
//
void ToolDock::OnGrabber(GrabberEvent& event)
{
    // auto pos = event.GetPosition();
    if (!event.IsEscaping()) {
        // Pass it on to the manager since it isn't in the handling hierarchy
        mManager->ProcessEvent(event);
    }
}

//
// Handle sizing
//
void ToolDock::OnSize(wxSizeEvent& WXUNUSED(event))
{
//   event.Skip();
}

//
// Prevent flicker
//
void ToolDock::OnErase(wxEraseEvent& WXUNUSED(event))
{
    // Ignore it to prevent flashing
}

//
// Repaint toolbar gap lines
//
void ToolDock::OnPaint(wxPaintEvent& WXUNUSED(event))
{
    // Don't use a wxBufferedPaintDC() here.  It produces a bogus
    // background on Windows and GTK.
    wxPaintDC dc(this);

    // Start with a clean background
    //
    // Under GTK, we don't set the toolbar background to the background
    // colour in the system theme.  Instead we use our own colour.

    dc.SetBackground(wxBrush(theTheme.Colour(clrMedium)));
    dc.Clear();

    // Set the gap color
    AColor::Dark(&dc, false);

    // Draw the initial horizontal and vertical gaps
    wxSize sz = GetClientSize();

    // Draw the gap between each bar
    for (const auto& place : GetConfiguration()) {
        auto toolbar = place.pTree->pBar;
        if (!toolbar) {
            continue;
        }

        wxRect r = toolbar->GetRect();

        // Draw a horizontal line under the bar extending to the right edge of
        // the dock
        AColor::Line(dc,
                     r.GetLeft(),
                     r.GetBottom() + 1,
                     sz.GetWidth(),
                     r.GetBottom() + 1);
    }
}

void ToolDock::OnMouseEvents(wxMouseEvent& event)
{
    // Do this hack so scrubber can detect mouse drags anywhere
    event.ResumePropagation(wxEVENT_PROPAGATE_MAX);
    event.Skip();
}
