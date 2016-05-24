/**********************************************************************

  Audacity: A Digital Audio Editor

  KeyView.cpp

*******************************************************************//*!

\class KeyView
\brief Provides multiple views of keyboard shortcuts

*//*********************************************************************/

#include "../Audacity.h"

#include <wx/defs.h>
#include <wx/settings.h>
#include <wx/vlbox.h>

#include "../AColor.h"
#include "../ShuttleGui.h"
#include "../commands/CommandManager.h"
#include "../commands/Keyboard.h"
#include "KeyView.h"

#include <wx/arrimpl.cpp>

// Various drawing constants
#define KV_BITMAP_SIZE 16
#define KV_LEFT_MARGIN 2
#define KV_COLUMN_SPACER 5
#define KV_VSCROLL_WIDTH 16   /* figure this out automatically? */

// Define the KeyNode arrays
WX_DEFINE_OBJARRAY(KeyNodeArray);
WX_DEFINE_OBJARRAY(KeyNodeArrayPtr);

// Define the event table
BEGIN_EVENT_TABLE(KeyView, wxVListBox)
   EVT_LEFT_DOWN(KeyView::OnLeftDown)
   EVT_KEY_DOWN(KeyView::OnKeyDown)
   EVT_LISTBOX(wxID_ANY, KeyView::OnSelected)
   EVT_SET_FOCUS(KeyView::OnSetFocus)
   EVT_KILL_FOCUS(KeyView::OnKillFocus)
   EVT_SIZE(KeyView::OnSize)
   EVT_SCROLLWIN(KeyView::OnScroll)
END_EVENT_TABLE();

// ============================================================================
// KeyView class
// ============================================================================
KeyView::KeyView(wxWindow *parent,
                 wxWindowID id,
                 const wxPoint & pos,
                 const wxSize & size)
: wxVListBox(parent, id, pos, size, wxBORDER_THEME | wxHSCROLL | wxVSCROLL),
  mScrollX(0),
  mWidth(0)
{
#if wxUSE_ACCESSIBILITY
   // Create and set accessibility object
   SetAccessible(mAx = safenew KeyViewAx(this));
#endif

   // The default view
   mViewType = ViewByTree;

   // Calculate measurements used for columns and scrolling
   RecalcExtents();
}

KeyView::~KeyView()
{
}

//
// Returns the index of the selected node
//
int
KeyView::GetSelected() const
{
   return LineToIndex(GetSelection());
}

//
// Returns the name of the control
//
wxString
KeyView::GetName() const
{
   // Just forward request
   return wxVListBox::GetName();
}

//
// Returns the label for the given index
//
wxString
KeyView::GetLabel(int index) const
{
   // Make sure index is valid
   if (index < 0 || index >= (int) mNodes.GetCount())
   {
      wxASSERT(false);
      return wxEmptyString;
   }

   return mNodes[index].label;
}

//
// Returns the prefix (if available) prepended to the label for the given index
//
wxString
KeyView::GetFullLabel(int index) const
{
   // Make sure index is valid
   if (index < 0 || index >= (int) mNodes.GetCount())
   {
      wxASSERT(false);
      return wxEmptyString;
   }

   // Cache the node and label
   KeyNode & node = mNodes[index];
   wxString label = node.label;

   // Prepend the prefix if available
   if (!node.prefix.IsEmpty())
   {
      label = node.prefix + wxT(" - ") + label;
   }

   return label;
}

//
// Returns the index for the given name
//
int
KeyView::GetIndexByName(const wxString & name) const
{
   int cnt = (int) mNodes.GetCount();

   // Search the nodes for the key
   for (int i = 0; i < cnt; i++)
   {
      if (name.CmpNoCase(mNodes[i].name) == 0)
      {
         return mNodes[i].index;
      }
   }

   return wxNOT_FOUND;
}

//
// Returns the command manager name for the given index
//
wxString
KeyView::GetName(int index) const
{
   // Make sure index is valid
   if (index < 0 || index >= (int) mNodes.GetCount())
   {
      wxASSERT(false);
      return wxEmptyString;
   }

   return mNodes[index].name;
}

//
// Returns the command manager index for the given key combination
//
wxString
KeyView::GetNameByKey(const wxString & key) const
{
   int cnt = (int) mNodes.GetCount();

   // Search the nodes for the key
   for (int i = 0; i < cnt; i++)
   {
      if (key.CmpNoCase(mNodes[i].key) == 0)
      {
         return mNodes[i].name;
      }
   }

   return wxEmptyString;
}

//
// Returns the index for the given key
//
int
KeyView::GetIndexByKey(const wxString & key) const
{
   int cnt = (int) mNodes.GetCount();

   // Search the nodes for the key
   for (int i = 0; i < cnt; i++)
   {
      if (key.CmpNoCase(mNodes[i].key) == 0)
      {
         return mNodes[i].index;
      }
   }

   return wxNOT_FOUND;
}

//
// Returns the key for the given index
//
wxString
KeyView::GetKey(int index) const
{
   // Make sure index is valid
   if (index < 0 || index >= (int) mNodes.GetCount())
   {
      wxASSERT(false);
      return wxEmptyString;
   }

   return mNodes[index].key;
}

//
// Use to determine if a key can be assigned to the given index
//
bool
KeyView::CanSetKey(int index) const
{
   // Make sure index is valid
   if (index < 0 || index >= (int) mNodes.GetCount())
   {
      wxASSERT(false);
      return false;
   }

   // Parents can't be assigned keys
   return !mNodes[index].isparent;
}

//
// Sets the key for the given index
//
bool
KeyView::SetKey(int index, const wxString & key)
{
   // Make sure index is valid
   if (index < 0 || index >= (int) mNodes.GetCount())
   {
      wxASSERT(false);
      return false;
   }

   // Cache the node
   KeyNode & node = mNodes[index];

   // Do not allow setting keys on branches
   if (node.isparent)
   {
      return false;
   }

   // Set the NEW key
   node.key = key;

   // Check to see if the key column needs to be expanded
   int x, y;
   GetTextExtent(node.key, &x, &y);
   if (x > mKeyWidth || y > mLineHeight)
   {
      // New key is wider than column so recalc extents (will refresh view)
      RecalcExtents();
      return true;
   }

   // Refresh the view lines
   RefreshAll();

   return true;
}

//
// Sets the key for the given name
//
bool
KeyView::SetKeyByName(const wxString & name, const wxString & key)
{
   int index = GetIndexByName(name);

   // Bail is the name wasn't found
   if (index == wxNOT_FOUND)
   {
      return false;
   }

   // Go set the key
   return SetKey(index, key);
}

//
// Sets the view type
//
void
KeyView::SetView(ViewByType type)
{
   int index = LineToIndex(GetSelection());

   // Handle an existing selection
   if (index != wxNOT_FOUND)
   {
      // Cache the currently selected node
      KeyNode & node = mNodes[index];

      // Expand branches if switching to Tree view and a line
      // is currently selected
      if (type == ViewByTree)
      {
         // Cache the node's depth
         int depth = node.depth;

         // Search for its parents, setting each one as open
         for (int i = node.index - 1; i >= 0 && depth > 1; i--)
         {
            if (mNodes[i].depth < depth)
            {
               mNodes[i].isopen = true;
               depth = mNodes[i].depth;
            }
         }
      }
   }

   // Unselect any currently selected line...do even if none selected
   SelectNode(-1);

   // Save NEW type
   mViewType = type;

   // Refresh the view lines
   RefreshLines();

   // Reselect old node (if possible)
   if (index != wxNOT_FOUND)
   {
      SelectNode(index);
   }

   return;
}

//
// Sets the filter
//
void
KeyView::SetFilter(const wxString & filter)
{
   int index = LineToIndex(GetSelection());

   // Unselect any currently selected line...do even if none selected
   SelectNode(-1);

   // Save the filter
   mFilter = filter.Lower();

   // Refresh the view lines
   RefreshLines();

   // Reselect old node (if possible)
   if (index != wxNOT_FOUND)
   {
      SelectNode(index);
   }
}

//
// Expand all branches
//
void
KeyView::ExpandAll()
{
   int cnt = (int) mNodes.GetCount();

   // Set all parent nodes to open
   for (int i = 0; i < cnt; i++)
   {
      KeyNode & node = mNodes[i];

      if (node.isparent)
      {
         node.isopen = true;
      }
   }

   RefreshLines();
}

//
// Collapse all branches
//
void
KeyView::CollapseAll()
{
   int cnt = (int) mNodes.GetCount();

   // Set all parent nodes to closed
   for (int i = 0; i < cnt; i++)
   {
      KeyNode & node = mNodes[i];

      if (node.isparent)
      {
         node.isopen = false;
      }
   }

   RefreshLines();
}

//
// Recalculate the measurements used for columns and scrolling
//
void
KeyView::RecalcExtents()
{
   // Reset
   mLineHeight = 0;
   mCommandWidth = 0;
   mKeyWidth = 0;

   // Examine all nodes
   int cnt = (int) mNodes.GetCount();
   for (int i = 0; i < cnt; i++)
   {
      KeyNode & node = mNodes[i];
      int x, y;

      if (node.iscat)
      {
         // Measure the category
         GetTextExtent(node.category, &x, &y);
      }
      else if (node.ispfx)
      {
         // Measure the prefix
         GetTextExtent(node.prefix, &x, &y);
      }
      else
      {
         // Measure the key
         GetTextExtent(node.key, &x, &y);
         mLineHeight = wxMax(mLineHeight, y);
         mKeyWidth = wxMax(mKeyWidth, x);

         // Prepend prefix for view types other than tree
         wxString label = node.label;
         if (mViewType != ViewByTree && !node.prefix.IsEmpty())
         {
            label = node.prefix + wxT(" - ") + label;
         }

         // Measure the label
         GetTextExtent(label, &x, &y);
      }

      // Finish calc for command column
      mLineHeight = wxMax(mLineHeight, y);
      mCommandWidth = wxMax(mCommandWidth, x);
   }

   // Update horizontal scrollbar
   UpdateHScroll();
}

//
// Update the horizontal scrollbar or remove it if not needed
//
void
KeyView::UpdateHScroll()
{
   // Get the internal dimensions of the view
   wxRect r = GetClientRect();

   // Calculate the full line width
   mWidth = KV_LEFT_MARGIN +
            mCommandWidth +
            KV_COLUMN_SPACER +
            mKeyWidth +
            KV_VSCROLL_WIDTH;

   // Retrieve the current horizontal scroll amount
   mScrollX = GetScrollPos(wxHORIZONTAL);

   if (mWidth <= r.GetWidth())
   {
      // Remove the scrollbar if it will fit within client width
      SetScrollbar(wxHORIZONTAL, 0, 0, 0);
   }
   else
   {
      // Set scrollbar metrics
      SetScrollbar(wxHORIZONTAL, mScrollX, r.GetWidth(), mWidth);
   }

   // Refresh the entire view
   RefreshAll();
}

//
// Process a NEW set of bindings
//
void
KeyView::RefreshBindings(const wxArrayString & names,
                         const wxArrayString & categories,
                         const wxArrayString & prefixes,
                         const wxArrayString & labels,
                         const wxArrayString & keys)
{
   bool firsttime = mNodes.GetCount() == 0;

   // Start clean
   mNodes.Clear();

   // Same as in RecalcExtents() but do it inline
   mLineHeight = 0;
   mKeyWidth = 0;
   mCommandWidth = 0;

   wxString lastcat;
   wxString lastpfx;
   int nodecnt = 0;
   int depth = 1;
   bool incat = false;
   bool inpfx = false;

   // Examine all names...all arrays passed have the same indexes
   int cnt = (int) names.GetCount();
   for (int i = 0; i < cnt; i++)
   {
      wxString name = names[i];
      int x, y;

      // Remove any menu code from the category and prefix
      wxString cat = wxMenuItem::GetLabelText(categories[i]);
      wxString pfx = wxMenuItem::GetLabelText(prefixes[i]);

      // Append "Menu" this node is for a menu title
      if (cat != wxT("Command"))
      {
         cat.Append(wxT(" "));
         cat += _("Menu");
      }

      // Process a NEW category
      if (cat != lastcat)
      {
         // A NEW category always finishes any current subtree
         if (inpfx)
         {
            // Back to category level
            depth--;
            inpfx = false;
         }

         // Only time this is not true is during the first iteration
         if (incat)
         {
            // Back to root level
            depth--;
            incat = false;
         }

         // Remember for next iteration
         lastcat = cat;

         // Add a NEW category node
         if (cat != wxEmptyString)
         {
            KeyNode node;

            // Fill in the node info
            node.name = wxEmptyString;    // don't associate branches with a command
            node.category = cat;
            node.prefix = pfx;
            node.label = cat;
            node.index = nodecnt++;
            node.iscat = true;
            node.isparent = true;
            node.depth = depth++;

            // Add it to the tree
            mNodes.Add(node);
            incat = true;

            // Measure category
            GetTextExtent(cat, &x, &y);
            mLineHeight = wxMax(mLineHeight, y);
            mCommandWidth = wxMax(mCommandWidth, x);
         }
      }

      // Process a NEW prefix
      if (pfx != lastpfx)
      {
         // Done with prefix branch
         if (inpfx)
         {
            depth--;
            inpfx = false;
         }

         // Remember for next iteration
         lastpfx = pfx;

         // Add a NEW prefix node
         if (pfx != wxEmptyString)
         {
            KeyNode node;

            // Fill in the node info
            node.name = wxEmptyString;    // don't associate branches with a command
            node.category = cat;
            node.prefix = pfx;
            node.label = pfx;
            node.index = nodecnt++;
            node.ispfx = true;
            node.isparent = true;
            node.depth = depth++;

            // Add it to the tree
            mNodes.Add(node);
            inpfx = true;
         }
      }

      // Add the key entry
      KeyNode node;
      node.category = cat;
      node.prefix = pfx;

      // Labels for undo and redo change according to the last command
      // which can be undone/redone, so give them a special check in order
      // not to confuse users
      if (name == wxT("Undo"))
      {
         node.label = _("Undo");
      }
      else if (name == wxT("Redo"))
      {
         node.label = _("Redo");
      }
      else
      {
         // Strip any menu codes from label
         node.label = wxMenuItem::GetLabelText(labels[i].BeforeFirst(wxT('\t')));
      }

      // Fill in remaining info
      node.name = name;
      node.key = KeyStringDisplay(keys[i]);
      node.index = nodecnt++;
      node.depth = depth;

      // Add it to the tree
      mNodes.Add(node);

      // Measure key
      GetTextExtent(node.key, &x, &y);
      mLineHeight = wxMax(mLineHeight, y);
      mKeyWidth = wxMax(mKeyWidth, x);

      // Prepend prefix for all view types to determine maximum
      // column widths
      wxString label = node.label;
      if (!node.prefix.IsEmpty())
      {
         label = node.prefix + wxT(" - ") + label;
      }

      // Measure label
      GetTextExtent(label, &x, &y);
      mLineHeight = wxMax(mLineHeight, y);
      mCommandWidth = wxMax(mCommandWidth, x);
   }

#if 0
   // For debugging
   for (int j = 0; j < mNodes.GetCount(); j++)
   {
      KeyNode & node = mNodes[j];
      wxLogDebug(wxT("NODE line %4d index %4d depth %1d open %1d parent %1d cat %1d pfx %1d name %s STR %s | %s | %s"),
         node.line,
         node.index,
         node.depth,
         node.isopen,
         node.isparent,
         node.iscat,
         node.ispfx,
         node.name.c_str(),
         node.category.c_str(),
         node.prefix.c_str(),
         node.label.c_str());
   }
#endif

   // Update horizontal scrollbar
   UpdateHScroll();

   // Refresh the view lines
   RefreshLines();

   // Set the selected node if this was the first time through
   if (firsttime)
   {
      SelectNode(LineToIndex(0));
   }
}

//
// Refresh the list of lines within the current view
//
void
KeyView::RefreshLines()
{
   int cnt = (int) mNodes.GetCount();
   int linecnt = 0;
   mLines.Empty();

   // Process a filter if one is set
   if (!mFilter.IsEmpty())
   {
      // Examine all nodes
      for (int i = 0; i < cnt; i++)
      {
         KeyNode & node = mNodes[i];

         // Reset line number
         node.line = wxNOT_FOUND;

         // Search columns based on view type
         wxString searchit;
         switch (mViewType)
         {
            // The x"01" separator is used to prevent finding a
            // match comprising the end of the label and beginning
            // of the key.  It was chosen since it's not very likely
            // to appear in the filter itself.
            case ViewByTree:
               searchit = node.label.Lower() +
                          wxT("\01x") +
                          node.key.Lower();
            break;

            case ViewByName:
               searchit = node.label.Lower();
            break;

            case ViewByKey:
               searchit = node.key.Lower();
            break;
         }
         if (searchit.Find(mFilter) == wxNOT_FOUND)
         {
            // Not found so continue to next node
            continue;
         }

         // For the Key View, if the filter is a single character,
         // then it has to be the last character in the searchit string,
         // and be preceded by nothing or +.
         if ((mViewType == ViewByKey) && 
               (mFilter.Len() == 1) && 
               (!mFilter.IsSameAs(searchit.Last()) ||
                  ((searchit.Len() > 1) && 
                     ((wxString)(searchit.GetChar(searchit.Len() - 2)) != wxT("+")))))
         {
            // Not suitable so continue to next node
            continue;
         }

         // For tree view, we must make sure all parent nodes are included
         // whether they match the filter or not.
         if (mViewType == ViewByTree)
         {
            KeyNodeArrayPtr queue;
            int depth = node.depth;

            // This node is a category or prefix node, so always mark them
            // as open.
            //
            // What this is really doing is resolving a situation where the
            // the filter matches a parent node and nothing underneath.  In
            // this case, the node would never be marked as open.
            if (node.isparent)
            {
               node.isopen = true;
            }

            // Examine siblings until a parent is found.
            for (int j = node.index - 1; j >= 0 && depth > 0; j--)
            {
               // Found a parent
               if (mNodes[j].depth < depth)
               {
                  // Examine all previously added nodes to see if this nodes
                  // ancestors need to be added prior to adding this node.
                  bool found = false;
                  for (int k = (int) mLines.GetCount() - 1; k >= 0; k--)
                  {
                     // The node indexes match, so we've found the parent of the
                     // child node.
                     if (mLines[k]->index == mNodes[j].index)
                     {
                        found = true;
                        break;
                     }
                  }

                  // The parent wasn't found so remember it for later
                  // addition.  Can't add directory to mLines here since
                  // they will wind up in reverse order.
                  if (!found)
                  {
                     queue.Add(&mNodes[j]);
                  }

                  // Traverse up the tree
                  depth = mNodes[j].depth;
               }
            }

            // Add any queues nodes to list.  This will all be
            // parent nodes, so mark them as open.
            for (int j = (int) queue.GetCount() - 1; j >= 0; j--)
            {
               queue[j]->isopen = true;
               queue[j]->line = linecnt++;
               mLines.Add(queue[j]);
            }
         }

         // Finally add the child node
         node.line = linecnt++;
         mLines.Add(&node);
      }
   }
   else
   {
      // Examine all nodes - non-filtered
      for (int i = 0; i < cnt; i++)
      {
         KeyNode & node = mNodes[i];

         // Reset line number
         node.line = wxNOT_FOUND;

         // Node is either a category or prefix
         if (node.isparent)
         {
            // Only need to do this for tree views
            if (mViewType != ViewByTree)
            {
               continue;
            }

            // Add the node
            node.line = linecnt++;
            mLines.Add(&node);

            // If this node is not open, then skip all of it's decendants
            if (!node.isopen)
            {
               bool iscat = node.iscat;
               bool ispfx = node.ispfx;

               // Skip nodes until we find a node that has a different
               // category or prefix
               while (i < cnt)
               {
                  KeyNode & skip = mNodes[i];

                  if ((iscat && skip.category != node.category) ||
                      (ispfx && skip.prefix != node.prefix))
                  {
                     break;
                  }

                  // Bump to next node
                  i++;
               }

               // Index is pointing to the node that was different or
               // past the end, so back off to last node of this branch.
               i--;
            }
            continue;
         }

         // Add child node to list
         node.line = linecnt++;
         mLines.Add(&node);
      }
   }

   // Sort list based on type
   switch (mViewType)
   {
      case ViewByTree:
         mLines.Sort(CmpKeyNodeByTree);
      break;

      case ViewByName:
         mLines.Sort(CmpKeyNodeByName);
      break;

      case ViewByKey:
         mLines.Sort(CmpKeyNodeByKey);
      break;
   }

   // Now, reassign the line numbers
   for (int i = 0; i < (int) mLines.GetCount(); i++)
   {
      mLines[i]->line = i;
   }

#if 0
   // For debugging
   for (int j = 0; j < mLines.GetCount(); j++)
   {
      KeyNode & node = *mLines[j];
      wxLogDebug(wxT("LINE line %4d index %4d depth %1d open %1d parent %1d cat %1d pfx %1d name %s STR %s | %s | %s"),
         node.line,
         node.index,
         node.depth,
         node.isopen,
         node.isparent,
         node.iscat,
         node.ispfx,
         node.name.c_str(),
         node.category.c_str(),
         node.prefix.c_str(),
         node.label.c_str());
   }
#endif

   // Tell listbox the NEW count and refresh the entire view
   SetItemCount(mLines.GetCount());
   RefreshAll();

#if wxUSE_ACCESSIBILITY
   // Let accessibility know that the list has changed
   mAx->ListUpdated();
#endif
}

//
// Select a node
//
// Parameter can be wxNOT_FOUND to clear selection
//
void
KeyView::SelectNode(int index)
{
   int line = IndexToLine(index);

   // Tell the listbox to select the line
   SetSelection(line);

#if wxUSE_ACCESSIBILITY
   // And accessibility
   mAx->SetCurrentLine(line);
#endif

   // Always send an event to let parent know of selection change
   //
   // Must do this ourselves becuase we want to send notifications
   // even if there isn't an item selected and SendSelectedEvent()
   // doesn't allow sending an event for indexes not in the listbox.
   wxCommandEvent event(wxEVT_COMMAND_LISTBOX_SELECTED, GetId());
   event.SetEventObject(this);
   event.SetInt(line);
   (void)GetEventHandler()->ProcessEvent(event);
}

//
// Converts a line index to a node index
//
int
KeyView::LineToIndex(int line) const
{
   if (line < 0 || line >= (int) mLines.GetCount())
   {
      return wxNOT_FOUND;
   }

   return mLines[line]->index;
}

//
// Converts a node index to a line index
//
int
KeyView::IndexToLine(int index) const
{
   if (index < 0 || index >= (int) mNodes.GetCount())
   {
      return wxNOT_FOUND;
   }

   return mNodes[index].line;
}

//
// Draw the background for a given line
//
// This is called by the listbox when it needs to redraw the view.
//
void
KeyView::OnDrawBackground(wxDC & dc, const wxRect & rect, size_t line) const
{
   const KeyNode *node = mLines[line];
   wxRect r = rect;
   wxCoord indent = 0;

   // When in tree view mode, each younger branch gets indented by the
   // width of the open/close bitmaps
   if (mViewType == ViewByTree)
   {
      indent += node->depth * KV_BITMAP_SIZE;
   }

   // Offset left side by the indentation (if any) and scroll amounts
   r.x = indent - mScrollX;

   // If the line width is less than the client width, then we want to
   // extend the background to the right edge of the client view.  Otherwise,
   // go all the way to the end of the line width...this will draw past the
   // right edge, but that's what we want.
   r.width = wxMax(mWidth, r.width);

   // Selected lines get a solid background
   if (IsSelected(line))
   {
      if (FindFocus() == this)
      {
         // Focused lines get highlighted background
         dc.SetBrush(wxBrush(wxSystemSettings::GetColour(wxSYS_COLOUR_HIGHLIGHT)));
         AColor::DrawFocus(dc, r);
         dc.DrawRectangle(r);
      }
      else
      {
         // Non focused lines get a light background
         dc.SetBrush(wxBrush(wxSystemSettings::GetColour(wxSYS_COLOUR_BTNFACE)));
         dc.SetPen(*wxTRANSPARENT_PEN);
         dc.DrawRectangle(r);
      }
   }
   else
   {
      // Non-selected lines get a thin bottom border
      dc.SetPen(wxColour(240, 240, 240));
      dc.DrawLine(r.GetLeft(), r.GetBottom(), r.GetRight(), r.GetBottom());
   }
}

//
// Draw a line
//
// This is called by the listbox when it needs to redraw the view.
//
void
KeyView::OnDrawItem(wxDC & dc, const wxRect & rect, size_t line) const
{
   const KeyNode *node = mLines[line];
   wxString label = node->label;

   // Make sure the DC has a valid font
   dc.SetFont(GetFont());

   // Set the text color based on selection and focus
   if (IsSelected(line) && FindFocus() == this)
   {
      dc.SetTextForeground(wxSystemSettings::GetColour(wxSYS_COLOUR_LISTBOXHIGHLIGHTTEXT));
   }
   else
   {
      dc.SetTextForeground(wxSystemSettings::GetColour(wxSYS_COLOUR_LISTBOXTEXT));
   }

   // Tree views get bitmaps
   if (mViewType == ViewByTree)
   {
      // Adjust left edge to account for scrolling
      wxCoord x = rect.x - mScrollX;

      if (node->iscat || node->ispfx)
      {
         wxCoord bx = x;
         wxCoord by = rect.y;

         if (node->ispfx)
         {
            bx += KV_BITMAP_SIZE;
         }

         dc.SetBrush(*wxTRANSPARENT_BRUSH);
         dc.SetPen(*wxBLACK_PEN);
         dc.DrawRectangle(bx + 3, by + 4, 9, 9);
         if (node->isopen)
         {
            AColor::Line(dc, bx + 5, by + 8, bx + 9, by + 8);
         }
         else
         {
            AColor::Line(dc, bx + 7, by + 6, bx + 7, by + 10);
            AColor::Line(dc, bx + 5, by + 8, bx + 9, by + 8);
         }
      }

      // Indent text
      x += KV_LEFT_MARGIN;

      // Draw the command and key columns
      dc.DrawText(label, x + node->depth * KV_BITMAP_SIZE, rect.y);
      dc.DrawText(node->key, x + mCommandWidth + KV_COLUMN_SPACER, rect.y);
   }
   else
   {
      // Adjust left edge by margin and account for scrolling
      wxCoord x = rect.x + KV_LEFT_MARGIN - mScrollX;

      // Prepend prefix if available
      if (!node->prefix.IsEmpty())
      {
         label = node->prefix + wxT(" - ") + label;
      }

      // Swap the columns based on view type
      if(mViewType == ViewByName)
      {
         // Draw command column and then key column
         dc.DrawText(label, x, rect.y);
         dc.DrawText(node->key, x + mCommandWidth + KV_COLUMN_SPACER, rect.y);
      }
      else if(mViewType == ViewByKey)
      {
         // Draw key columnd and then command column
         dc.DrawText(node->key, x, rect.y);
         dc.DrawText(label, x + mKeyWidth + KV_COLUMN_SPACER, rect.y);
      }
   }

   return;
}

//
// Provide the height of the given line
//
// This is called by the listbox when it needs to redraw the view.
//
wxCoord
KeyView::OnMeasureItem(size_t WXUNUSED(line)) const
{
   // All lines are of equal height
   //
   // (add a magic 1 for decenders...looks better...not required)
   return mLineHeight + 1;
}

//
// Handle the wxEVT_LISTBOX event
//
void
KeyView::OnSelected(wxCommandEvent & event)
{
   // Allow further processing
   event.Skip();

#if wxUSE_ACCESSIBILITY
   // Tell accessibility of the change
   mAx->SetCurrentLine(event.GetInt());
#endif
}

//
// Handle the wxEVT_SET_FOCUS event
//
void
KeyView::OnSetFocus(wxFocusEvent & event)
{
   // Allow further processing
   event.Skip();

   // Refresh the selected line to pull in any changes while
   // focus was away...like when setting a NEW key value.  This
   // will also refresh the visual (highlighted) state.
   if (GetSelection() != wxNOT_FOUND)
   {
	   RefreshRow(GetSelection());
   }

#if wxUSE_ACCESSIBILITY
   // Tell accessibility of the change
   mAx->SetCurrentLine(GetSelection());
#endif
}

//
// Handle the wxEVT_KILL_FOCUS event
//
void
KeyView::OnKillFocus(wxFocusEvent & event)
{
   // Allow further processing
   event.Skip();

   // Refresh the selected line to adjust visual highlighting.
   if (GetSelection() != wxNOT_FOUND)
   {
	   RefreshRow(GetSelection());
   }
}

//
// Handle the wxEVT_SIZE event
//
void
KeyView::OnSize(wxSizeEvent & WXUNUSED(event))
{
   // Update horizontal scrollbar
   UpdateHScroll();
}

//
// Handle the wxEVT_SCROLL event
//
void
KeyView::OnScroll(wxScrollWinEvent & event)
{
   // We only care bout the horizontal scrollbar.
   if (event.GetOrientation() != wxHORIZONTAL)
   {
      // Allow further processing
      event.Skip();
      return;
   }

   // Get NEW scroll position and scroll the view
   mScrollX = event.GetPosition();
   SetScrollPos(wxHORIZONTAL, mScrollX);

   // Refresh the entire view
   RefreshAll();
}

//
// Handle the wxEVT_KEY_DOWN event
//
void
KeyView::OnKeyDown(wxKeyEvent & event)
{
   int line = GetSelection();

   int keycode = event.GetKeyCode();
   switch (keycode)
   {
      // The LEFT key moves selection to parent or collapses selected
      // node if it is expanded.
      case WXK_LEFT:
      {
         // Nothing selected...nothing to do
         if (line == wxNOT_FOUND)
         {
            // Allow further processing
            event.Skip();
            break;
         }

         KeyNode *node = mLines[line];

         // Collapse the node if it is open
         if (node->isopen)
         {
            // No longer open
            node->isopen = false;

            // Don't want the view to scroll vertically, so remember the current
            // top line.
            size_t topline = GetVisibleBegin();

            // Refresh the view now that the number of lines have changed
            RefreshLines();

            // Reset the original top line
            ScrollToRow(topline);

            // And make sure current line is still selected
            SelectNode(LineToIndex(line));
         }
         else
         {
            // Move selection to the parent of this node
            for (int i = line - 1; i >= 0; i--)
            {
               // Found the parent
               if (mLines[i]->depth < node->depth)
               {
                  // So select it
                  SelectNode(LineToIndex(i));
                  break;
               }
            }
         }

         // Further processing of the event is not wanted
         // (we didn't call event.Skip()
      }
      break;

      // The RIGHT key moves the selection to the first child or expands
      // the node if it is a parent.
      case WXK_RIGHT:
      {
         // Nothing selected...nothing to do
         if (line == wxNOT_FOUND)
         {
            // Allow further processing
            event.Skip();
            break;
         }

         KeyNode *node = mLines[line];

         // Only want parent nodes
         if (node->isparent)
         {
            // It is open so move select to first child
            if (node->isopen)
            {
               // But only if there is one
               if (line < (int) mLines.GetCount() - 1)
               {
                  SelectNode(LineToIndex(line + 1));
               }
            }
            else
            {
               // Node is now open
               node->isopen = true;

               // Don't want the view to scroll vertically, so remember the current
               // top line.
               size_t topline = GetVisibleBegin();

               // Refresh the view now that the number of lines have changed
               RefreshLines();

               // Reset the original top line
               ScrollToRow(topline);

               // And make sure current line is still selected
               SelectNode(LineToIndex(line));
            }
         }

         // Further processing of the event is not wanted
         // (we didn't call event.Skip()
      }
      break;

      // Move selection to next node whose 1st character matches
      // the keycode
      default:
      {
         int cnt = (int) mLines.GetCount();
         bool found = false;

         // Search the entire list if none is currently selected
         if (line == wxNOT_FOUND)
         {
            line = cnt;
         }
         else
         {
            // Search from the node following the current one
            for (int i = line + 1; i < cnt; i++)
            {
               wxString label;

               // Get the string to search based on view type
               if (mViewType == ViewByTree)
               {
                  label = GetLabel(LineToIndex(i));
               }
               else if (mViewType == ViewByName)
               {
                  label = GetFullLabel(LineToIndex(i));
               }
               else if (mViewType == ViewByKey)
               {
                  label = GetKey(LineToIndex(i));
               }

               // Move selection if they match
               if (label.Left(1).IsSameAs(keycode, false))
               {
                  SelectNode(LineToIndex(i));

                  found = true;

                  break;
               }
            }
         }

         // A match wasn't found
         if (!found)
         {
            // So scan from the start of the list to the current node
            for (int i = 0; i < line; i++)
            {
               wxString label;

               // Get the string to search based on view type
               if (mViewType == ViewByTree)
               {
                  label = GetLabel(LineToIndex(i));
               }
               else if (mViewType == ViewByName)
               {
                  label = GetFullLabel(LineToIndex(i));
               }
               else if (mViewType == ViewByKey)
               {
                  label = GetKey(LineToIndex(i));
               }

               // Move selection if they match
               if (label.Left(1).IsSameAs(keycode, false))
               {
                  SelectNode(LineToIndex(i));

                  found = true;

                  break;
               }
            }
         }

         // A node wasn't found so allow further processing
         if (!found) {
            event.Skip();
         }

         // Otherwise, further processing of the event is not wanted
         // (we didn't call event.Skip()
      }
   }
}

//
// Handle the wxEVT_LEFT_DOWN event
//
void
KeyView::OnLeftDown(wxMouseEvent & event)
{
   // Only check if for tree view
   if (mViewType != ViewByTree)
   {
      // Allow further processing (important for focus handling)
      event.Skip();

      return;
   }

   // Get the mouse position when the button was pressed
   wxPoint pos = event.GetPosition();

   // And see if it was on a line within the view
   int line = VirtualHitTest(pos.y);

   // It was on a line
   if (line != wxNOT_FOUND)
   {
      KeyNode *node = mLines[line];

      // Toggle the open state if this is a parent node
      if (node->isparent)
      {
         // Toggle state
         node->isopen = !node->isopen;

         // Don't want the view to scroll vertically, so remember the current
         // top line.
         size_t topline = GetVisibleBegin();

         // Refresh the view now that the number of lines have changed
         RefreshLines();

         // Reset the original top line
         ScrollToRow(topline);

         // And make sure current line is still selected
         SelectNode(LineToIndex(line));
      }
   }

   // Allow further processing (important for focus handling)
   event.Skip();
}

//
// Sort compare function for tree view
//
// We want to leave the "menu" nodes alone as they are in the
// order as they appear in the menus.  But, we want to sort the
// "command" nodes.
//
// To accomplish this, we prepend each label with it's line number
// (in hex) for "menu" nodes.  This ensures they will remain in
// their original order.
//
// We prefix all "command" nodes with "ffffffff" (highest hex value)
// to allow the sort to reorder them as needed.
//
int
KeyView::CmpKeyNodeByTree(KeyNode ***n1, KeyNode ***n2)
{
   KeyNode *t1 = (**n1);
   KeyNode *t2 = (**n2);
   wxString k1 = t1->label;
   wxString k2 = t2->label;

   // This is a "command" node if its category is "Command"
   // and it is a child of the "Command" category.  This latter
   // test ensures that the "Command" parent will be handled
   // as a "menu" node and remain at the bottom of the list.
   if (t1->category == _("Command") && !t1->isparent)
   {
      // A "command" node, so prepend the highest hex value
      k1.Printf(wxT("ffffffff%s"), t1->label.c_str());
   }
   else
   {
      // A "menu" node, so prepend the line number
      k1.Printf(wxT("%08x%s"), (unsigned int) t1->line, t1->label.c_str());
   }

   // See above for explanation
   if (t2->category == _("Command") && !t2->isparent)
   {
      // A "command" node, so prepend the highest hex value
      k2.Printf(wxT("ffffffff%s"), t2->label.c_str());
   }
   else
   {
      // A "menu" node, so prepend the line number
      k2.Printf(wxT("%08x%s"), (unsigned int) t2->line, t2->label.c_str());
   }

   // See wxWidgets documentation for explanation of comparison results.
   // These will produce an ascending order.
   if (k1 < k2)
   {
      return -1;
   }

   if (k1 > k2)
   {
      return 1;
   }

   return 0;
}

//
// Sort compare function for command view
//
// Nothing special here, just a standard ascending sort.
//
int
KeyView::CmpKeyNodeByName(KeyNode ***n1, KeyNode ***n2)
{
   KeyNode *t1 = (**n1);
   KeyNode *t2 = (**n2);
   wxString k1 = t1->label;
   wxString k2 = t2->label;

   // Prepend prefix if available
   if (!t1->prefix.IsEmpty())
   {
      k1 = t1->prefix + wxT(" - ") + k1;
   }

   // Prepend prefix if available
   if (!t2->prefix.IsEmpty())
   {
      k2 = t2->prefix + wxT(" - ") + k2;
   }

   // See wxWidgets documentation for explanation of comparison results.
   // These will produce an ascending order.
   if (k1 < k2)
   {
      return -1;
   }

   if (k1 > k2)
   {
      return 1;
   }

   return 0;
}

//
// Sort compare function for key view
//
// We want all nodes with key assignments to appear in ascending order
// at the top of the list and all nodes without assignment to appear in
// ascending order at the bottom of the list.
//
// We accomplish this by by prefixing all non-assigned entries with 0xff.
// This will force them to the end, but still allow them to be sorted in
// ascending order.
//
// The assigned entries simply get sorted as normal.
//
int
KeyView::CmpKeyNodeByKey(KeyNode ***n1, KeyNode ***n2)
{
   KeyNode *t1 = (**n1);
   KeyNode *t2 = (**n2);
   wxString k1 = t1->key;
   wxString k2 = t2->key;

   // Left node is unassigned, so prefix it
   if(k1.IsEmpty())
   {
      k1 = wxT("\xff");
   }

   // Right node is unassigned, so prefix it
   if(k2.IsEmpty())
   {
      k2 = wxT("\xff");
   }

   // Add prefix if available
   if (!t1->prefix.IsEmpty())
   {
      k1 += t1->prefix + wxT(" - ");
   }

   // Add prefix if available
   if (!t2->prefix.IsEmpty())
   {
      k2 += t2->prefix + wxT(" - ");
   }

   // Add labels
   k1 += t1->label;
   k2 += t2->label;

   // See wxWidgets documentation for explanation of comparison results.
   // These will produce an ascending order.
   if (k1 < k2)
   {
      return -1;
   }

   if (k1 > k2)
   {
      return 1;
   }

   return 0;
}

#if wxUSE_ACCESSIBILITY

//
// Return parenthood state of line
//
bool
KeyView::HasChildren(int line)
{
   // Make sure line is valid
   if (line < 0 || line >= (int) mLines.GetCount())
   {
      wxASSERT(false);
      return false;
   }

   return mLines[line]->isparent;
}

//
// Returns espanded/collapsed state of line
//
bool
KeyView::IsExpanded(int line)
{
   // Make sure line is valid
   if (line < 0 || line >= (int) mLines.GetCount())
   {
      wxASSERT(false);
      return false;
   }

   return mLines[line]->isopen;
}

//
// Returns the height of the line
//
wxCoord
KeyView::GetLineHeight(int line)
{
   // Make sure line is valid
   if (line < 0 || line >= (int) mLines.GetCount())
   {
      wxASSERT(false);
      return 0;
   }

   return OnGetRowHeight(line);
}

//
// Returns the value to be presented to accessibility
//
// Currently, the command and key are both provided.
//
wxString
KeyView::GetValue(int line)
{
   // Make sure line is valid
   if (line < 0 || line >= (int) mLines.GetCount())
   {
      wxASSERT(false);
      return wxEmptyString;
   }
   int index = LineToIndex(line);

   // Get the label and key values
   wxString value;
   if (mViewType == ViewByTree)
   {
      value = GetLabel(index);
   }
   else
   {
      value = GetFullLabel(index);
   }
   wxString key = GetKey(index);

   // Add the key if it isn't empty
   if (!key.IsEmpty())
   {
      if (mViewType == ViewByKey)
      {
         value = key + wxT(" ") + value;
      }
      else
      {
         value = value + wxT(" ") + key;
      }
   }

   return value;
}

//
// Returns the current view type
//
ViewByType
KeyView::GetViewType()
{
   return mViewType;
}

// ============================================================================
// Accessibility provider for the KeyView class
// ============================================================================
KeyViewAx::KeyViewAx(KeyView *view)
: wxWindowAccessible(view)
{
   mView = view;
   mLastId = -1;
}

//
// Send an event notification to accessibility that the view
// has changed.
//
void
KeyViewAx::ListUpdated()
{
   NotifyEvent(wxACC_EVENT_OBJECT_REORDER,
               mView,
               wxOBJID_CLIENT,
               0);
}

//
// Inform accessibility a NEW line has been selected and/or a previously
// selected line is being unselected
//
void
KeyViewAx::SetCurrentLine(int line)
{
   // Only send selection remove notification if a line was
   // previously selected
   if (mLastId != -1)
   {
      NotifyEvent(wxACC_EVENT_OBJECT_SELECTIONREMOVE,
               mView,
               wxOBJID_CLIENT,
               mLastId);
   }

   // Nothing is selected now
   mLastId = -1;

   // Just clearing selection
   if (line != wxNOT_FOUND)
   {
      // Convert line number to childId
      LineToId(line, mLastId);

      // Send notifications that the line has focus
      NotifyEvent(wxACC_EVENT_OBJECT_FOCUS,
                  mView,
                  wxOBJID_CLIENT,
                  mLastId);

      // And is selected
      NotifyEvent(wxACC_EVENT_OBJECT_SELECTION,
                  mView,
                  wxOBJID_CLIENT,
                  mLastId);
   }
}

//
// Convert the childId to a line number and return FALSE if it
// represents a child or TRUE if it a line
//
bool
KeyViewAx::IdToLine(int childId, int & line)
{
   if (childId == wxACC_SELF)
   {
      return false;
   }

   // Convert to line
   line = childId - 1;

   // Make sure id is valid
   if (line < 0 || line >= (int) mView->GetItemCount())
   {
      // Indicate the control itself in this case
      return false;
   }

   return true;
}

//
// Convert the line number to a childId.
//
bool
KeyViewAx::LineToId(int line, int & childId)
{
   // Make sure line is valid
   if (line < 0 || line >= (int) mView->GetItemCount())
   {
      // Indicate the control itself in this case
      childId = wxACC_SELF;
      return false;
   }

   // Convert to line
   childId = line + 1;

   return true;
}

// Can return either a child object, or an integer
// representing the child element, starting from 1.
wxAccStatus
KeyViewAx::HitTest(const wxPoint & pt, int *childId, wxAccessible **childObject)
{
   // Just to be safe
   *childObject = NULL;

   wxPoint pos = mView->ScreenToClient(pt);

   // See if it's on a line within the view
   int line = mView->HitTest(pos);

   // It was on a line
   if (line != wxNOT_FOUND)
   {
      LineToId(line, *childId);
      return wxACC_OK;
   }

   // Let the base class handle it
   return wxACC_NOT_IMPLEMENTED;
}

// Retrieves the address of an IDispatch interface for the specified child.
// All objects must support this property.
wxAccStatus
KeyViewAx::GetChild(int childId, wxAccessible** child)
{
   if (childId == wxACC_SELF)
   {
      *child = this;
   }
   else
   {
      *child = NULL;
   }

   return wxACC_OK;
}

// Gets the number of children.
wxAccStatus
KeyViewAx::GetChildCount(int *childCount)
{
   *childCount = (int) mView->GetItemCount();

   return wxACC_OK;
}

// Gets the default action for this object (0) or > 0 (the action for a child).
// Return wxACC_OK even if there is no action. actionName is the action, or the empty
// string if there is no action.
// The retrieved string describes the action that is performed on an object,
// not what the object does as a result. For example, a toolbar button that prints
// a document has a default action of "Press" rather than "Prints the current document."
wxAccStatus
KeyViewAx::GetDefaultAction(int WXUNUSED(childId), wxString *actionName)
{
   actionName->Clear();

   return wxACC_OK;
}

// Returns the description for this object or a child.
wxAccStatus
KeyViewAx::GetDescription(int WXUNUSED(childId), wxString *description)
{
   description->Clear();

   return wxACC_OK;
}

// Returns help text for this object or a child, similar to tooltip text.
wxAccStatus
KeyViewAx::GetHelpText(int WXUNUSED(childId), wxString *helpText)
{
   helpText->Clear();

   return wxACC_OK;
}

// Returns the keyboard shortcut for this object or child.
// Return e.g. ALT+K
wxAccStatus
KeyViewAx::GetKeyboardShortcut(int WXUNUSED(childId), wxString *shortcut)
{
   shortcut->Clear();

   return wxACC_OK;
}

// Returns the rectangle for this object (id = 0) or a child element (id > 0).
// rect is in screen coordinates.
wxAccStatus
KeyViewAx::GetLocation(wxRect & rect, int elementId)
{
   int line;

   if (IdToLine(elementId, line))
   {
      if (!mView->IsVisible(line))
      {
         return wxACC_FAIL;
      }

      wxRect rectLine;

      rectLine.width = mView->GetClientSize().GetWidth();

      // iterate over all visible lines
      for (int i = (int) mView->GetVisibleBegin(); i <= line; i++)
      {
         wxCoord hLine = mView->GetLineHeight(i);

         rectLine.height = hLine;

         rect = rectLine;
         wxPoint margins = mView->GetMargins();
         rect.Deflate(margins.x, margins.y);
         rectLine.y += hLine;
      }

      rect.SetPosition(mView->ClientToScreen(rect.GetPosition()));
   }
   else
   {
      rect = mView->GetRect();
      rect.SetPosition(mView->GetParent()->ClientToScreen(rect.GetPosition()));
   }

   return wxACC_OK;
}

wxAccStatus
KeyViewAx::Navigate(wxNavDir WXUNUSED(navDir),
                    int WXUNUSED(fromId),
                    int *WXUNUSED(toId),
                    wxAccessible **WXUNUSED(toObject))
{
   return wxACC_NOT_IMPLEMENTED;
}

// Gets the name of the specified object.
wxAccStatus
KeyViewAx::GetName(int childId, wxString *name)
{
   int line;

   if (!IdToLine(childId, line))
   {
      *name = mView->GetName();
   }
   else
   {
      if (IdToLine(childId, line))
      {
         *name = mView->GetValue(line);
      }
   }

   return wxACC_OK;
}

wxAccStatus
KeyViewAx::GetParent(wxAccessible ** WXUNUSED(parent))
{
   return wxACC_NOT_IMPLEMENTED;
}

// Returns a role constant.
wxAccStatus
KeyViewAx::GetRole(int childId, wxAccRole *role)
{
   if (childId == wxACC_SELF)
   {
#if defined(__WXMSW__)
      *role = mView->GetViewType() == ViewByTree ? wxROLE_SYSTEM_OUTLINE : wxROLE_SYSTEM_LIST;
#endif

#if defined(__WXMAC__)
      *role = wxROLE_SYSTEM_GROUPING;
#endif
   }
   else
   {
#if defined(__WXMAC__)
      *role = wxROLE_SYSTEM_TEXT;
#else
      *role = mView->GetViewType() == ViewByTree ? wxROLE_SYSTEM_OUTLINEITEM : wxROLE_SYSTEM_LISTITEM;
#endif
   }

   return wxACC_OK;
}

// Gets a variant representing the selected children
// of this object.
// Acceptable values:
// - a null variant (IsNull() returns TRUE)
// - a list variant (GetType() == wxT("list"))
// - an integer representing the selected child element,
//   or 0 if this object is selected (GetType() == wxT("long"))
// - a "void*" pointer to a wxAccessible child object
wxAccStatus
KeyViewAx::GetSelections(wxVariant *selections)
{
   int id;

   LineToId(mView->GetSelection(), id);

   *selections = (long) id;

   return wxACC_OK;
}

// Returns a state constant.
wxAccStatus
KeyViewAx::GetState(int childId, long *state)
{
   int flag = wxACC_STATE_SYSTEM_FOCUSABLE;
   int line;

   if (!IdToLine(childId, line))
   {
      *state = wxACC_STATE_SYSTEM_FOCUSABLE; // |
               //mView->FindFocus() == mView ? wxACC_STATE_SYSTEM_FOCUSED : 0;
      return wxACC_OK;
   }

#if defined(__WXMSW__)
   int selected = mView->GetSelection();

   flag |= wxACC_STATE_SYSTEM_SELECTABLE;

   if (line == selected)
   {
      flag |= wxACC_STATE_SYSTEM_FOCUSED |
              wxACC_STATE_SYSTEM_SELECTED;
   }

   if (mView->HasChildren(line))
   {
      flag |= mView->IsExpanded(line) ?
         wxACC_STATE_SYSTEM_EXPANDED :
         wxACC_STATE_SYSTEM_COLLAPSED;
   }
#endif

#if defined(__WXMAC__1)
   if (mGrid->IsInSelection(row, col))
   {
      flag |= wxACC_STATE_SYSTEM_SELECTED;
   }

   if (mGrid->GetGridCursorRow() == row && mGrid->GetGridCursorCol() == col)
   {
       flag |= wxACC_STATE_SYSTEM_FOCUSED;
   }

   if (mGrid->IsReadOnly(row, col))
   {
      flag |= wxACC_STATE_SYSTEM_UNAVAILABLE;
   }
#endif

   *state = flag;

   return wxACC_OK;
}

// Returns a localized string representing the value for the object
// or child.
wxAccStatus
KeyViewAx::GetValue(int childId, wxString *strValue)
{
   int line;

   strValue->Clear();

   if (!IdToLine(childId, line))
   {
      return wxACC_NOT_IMPLEMENTED;
   }

#if defined(__WXMSW__)
   if (mView->GetViewType() == ViewByTree)
   {
      KeyNode *node = mView->mLines[line];
      strValue->Printf(wxT("%d"), node->depth - 1);
   }

   // Don't set a value for the other view types
   return wxACC_NOT_IMPLEMENTED;
#endif

#if defined(__WXMAC__)
   return GetName(childId, strValue);
#endif
}

#if defined(__WXMAC__)
// Selects the object or child.
wxAccStatus
KeyViewAx::Select(int childId, wxAccSelectionFlags selectFlags)
{
#if 0
   int row;
   int col;

   if (GetRowCol(childId, row, col))
   {

      if (selectFlags & wxACC_SEL_TAKESELECTION)
      {
         mGrid->SetGridCursor(row, col);
      }

      mGrid->SelectBlock(row, col, row, col, selectFlags & wxACC_SEL_ADDSELECTION);
   }
#endif
   return wxACC_OK;
}
#endif

// Gets the window with the keyboard focus.
// If childId is 0 and child is NULL, no object in
// this subhierarchy has the focus.
// If this object has the focus, child should be 'this'.
wxAccStatus
KeyViewAx::GetFocus(int * WXUNUSED(childId), wxAccessible **child)
{
   *child = this;

   return wxACC_OK;
}

#endif // wxUSE_ACCESSIBILITY
