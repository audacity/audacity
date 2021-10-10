/*!********************************************************************
*
 Audacity: A Digital Audio Editor

 TextEditHelper.h

 Vitaly Sverchinsky

 **********************************************************************/

#pragma once

#include <wx/string.h>
#include <wx/font.h>

#include "UIHandle.h"
#include <wx/colour.h>

class wxDC;
class wxMouseEvent;

class AudacityProject;
class TextEditDelegate;
class Track;

//Used as a delegate for TextEditHelper
class TextEditDelegate
{
public:

    virtual ~TextEditDelegate();

    virtual void OnTextEditFinished(AudacityProject* project, const wxString& text) = 0;
    virtual void OnTextEditCancelled(AudacityProject* project) = 0;
    virtual void OnTextModified(AudacityProject* project, const wxString& text) = 0;
    virtual void OnTextContextMenu(AudacityProject* project, const wxPoint& position) = 0;

};


//Used as a helper object for drawing, editing
//and text navigation in TrackPanel
class TextEditHelper
{
    wxString mText;
    wxFont mFont;
    wxRect mBBox;

    wxColor mTextSelectionColor;
    wxColor mTextColor;

    bool mRightDragging{ false };
    //Index of the symbol from which text drawing should start,
    //in cases where the whole text cannot fit into provided "view"
    //rectangle. Used both in hit testing and drawing.
    int mOffset{ 0 };
    int mInitialCursorPos{ 0 };
    int mCurrentCursorPos{ 0 };

    // TextEditHelper will send messages about changes to the object
    // that implements TextEditDelegate, if present
    std::weak_ptr<TextEditDelegate> mDelegate;

public:
    static bool IsGoodEditKeyCode(int keyCode);

    TextEditHelper(const std::weak_ptr<TextEditDelegate>& delegate, const wxString& text, const wxFont& font);
   
   ~TextEditHelper()
   {
   }

    void SetTextColor(const wxColor& textColor);
    void SetTextSelectionColor(const wxColor& textSelectionColor);

    void Cancel(AudacityProject* project);
    void Finish(AudacityProject* project);

    std::pair<int, int> GetSelection() const;
    void SetSelection(int from, int to);
    bool IsSelectionEmpty();

    bool OnKeyDown(int keyCode, int mods, AudacityProject* project);
    bool OnChar(int charCode, AudacityProject* project);

    bool OnClick(const wxMouseEvent& event, AudacityProject* project);
    bool OnDrag(const wxMouseEvent& event, AudacityProject* project);
    bool OnRelease(const wxMouseEvent& event, AudacityProject* project);

    void Draw(wxDC& dc, const wxRect& rect);

    bool CutSelectedText(AudacityProject& project);
    bool CopySelectedText(AudacityProject& project);
    bool PasteSelectedText(AudacityProject& project);

    bool GetCharPositionX(int index, int* outX);

    const wxRect& GetBBox() const;

protected:

    bool HandleDragRelease(const wxMouseEvent& event, AudacityProject* project);

    void RemoveSelectedText(AudacityProject* project);
    int FindCursorIndex(const wxPoint& point);
    
};
