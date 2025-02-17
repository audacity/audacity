/**********************************************************************

  Audacity: A Digital Audio Editor

  KeyView.h

**********************************************************************/

#ifndef __AUDACITY_WIDGETS_KEYVIEW__
#define __AUDACITY_WIDGETS_KEYVIEW__

#include "audacity/Types.h"

#include <vector>
#include <wx/setup.h> // for wxUSE_* macros
#include <wx/defs.h>
#include <wx/vlbox.h> // to inherit wxVListBox

#include "Keyboard.h"

// Class holding all information about a node.  Rather than a real tree
// we store these in an array and simulate a tree.
class KeyNode
{
public:
    KeyNode()
    {
        index = -1;
        line = -1;
        depth = -1;
        iscat = false;
        ispfx = false;
        isparent = false;
        isopen = false;
    }

    KeyNode(const KeyNode&) = default;
    KeyNode& operator =(const KeyNode&) = default;
    //KeyNode( KeyNode && ) = default;
    //KeyNode &operator = ( KeyNode && ) = default;

public:
    CommandID name;
    wxString category;
    wxString prefix;
    wxString label;
    NormalizedKeyString key;
    int index;
    int line;
    int depth;
    bool iscat;
    bool ispfx;
    bool isparent;
    bool isopen;
};

// Declare the KeyNode arrays

// Types of view currently supported
enum ViewByType : int
{
    ViewByTree,
    ViewByName,
    ViewByKey
};

#if wxUSE_ACCESSIBILITY
class KeyViewAx;
#endif

// The KeyView class
class KeyView final : public wxVListBox
{
public:
    KeyView(wxWindow* parent, wxWindowID id = wxID_ANY, const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize);
    virtual ~KeyView();
    wxString GetName() const override; // Gets the control name from the base class

    void RefreshBindings(const CommandIDs& names, const TranslatableStrings& categories, const TranslatableStrings& prefixes,
                         const TranslatableStrings& labels, const std::vector<NormalizedKeyString>& keys, bool bSort);

    int GetSelected() const;

    wxString GetLabel(int index) const;
    wxString GetFullLabel(int index) const;

    int GetIndexByName(const CommandID& name) const;
    CommandID GetName(int index) const;
    CommandID GetNameByKey(const NormalizedKeyString& key) const;

    int GetIndexByKey(const NormalizedKeyString& key) const;
    NormalizedKeyString GetKey(int index) const;
    bool CanSetKey(int index) const;
    bool SetKey(int index, const NormalizedKeyString& key);
    bool SetKeyByName(const CommandID& name, const NormalizedKeyString& key);

    void SetView(ViewByType type);

    void SetFilter(const wxString& filter);

    void ExpandAll();
    void CollapseAll();

    void SelectNode(int index);

private:
    void RecalcExtents();
    void UpdateHScroll();
    void RefreshLines(bool bSort = true);

    int LineToIndex(int line) const;
    int IndexToLine(int index) const;

    void OnDrawBackground(wxDC& dc, const wxRect& rect, size_t line) const override;
    void OnDrawItem(wxDC& dc, const wxRect& rect, size_t line) const override;
    wxCoord OnMeasureItem(size_t line) const override;

    void OnSelected(wxCommandEvent& event);
    void OnSetFocus(wxFocusEvent& event);
    void OnKillFocus(wxFocusEvent& event);
    void OnSize(wxSizeEvent& event);
    void OnScroll(wxScrollWinEvent& event);
    void OnKeyDown(wxKeyEvent& event);
    void OnLeftDown(wxMouseEvent& event);

    static bool CmpKeyNodeByTree(KeyNode* n1, KeyNode* n2);
    static bool CmpKeyNodeByName(KeyNode* n1, KeyNode* n2);
    static bool CmpKeyNodeByKey(KeyNode* n1, KeyNode* n2);

#if wxUSE_ACCESSIBILITY
    friend class KeyViewAx;

    bool HasChildren(int line);
    bool IsExpanded(int line);
    wxCoord GetLineHeight(int line);
    wxString GetValue(int line);
    ViewByType GetViewType();
#endif

private:
    std::vector<KeyNode> mNodes;
    std::vector<KeyNode*> mLines;

    ViewByType mViewType;
    wxString mFilter;

    wxCoord mScrollX;
    wxCoord mWidth;

    size_t mLineCount;
    wxCoord mLineHeight;
    wxCoord mKeyX;
    int mCommandWidth;
    wxCoord mKeyWidth;

#if wxUSE_ACCESSIBILITY
    KeyViewAx* mAx;
#endif

    DECLARE_EVENT_TABLE()
};

#endif
