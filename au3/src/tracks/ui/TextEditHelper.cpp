/*!********************************************************************
*
 Audacity: A Digital Audio Editor

 TextEditHelper.h

 Vitaly Sverchinsky

 The major part of the logic is extracted from LabelTrackView.cpp and
 LabelTextHandle.cpp

 **********************************************************************/

#include "TextEditHelper.h"

#include <wx/app.h>
#include <wx/dc.h>
#include <wx/dcmemory.h>
#include <wx/clipbrd.h>

#include "../../RefreshCode.h"

TextEditDelegate::~TextEditDelegate() = default;

bool TextEditHelper::IsGoodEditKeyCode(int keyCode)
{
    // Accept everything outside of WXK_START through WXK_COMMAND, plus the keys
    // within that range that are usually printable, plus the ones we use for
    // keyboard navigation.
    return keyCode < WXK_START
           || (keyCode >= WXK_END && keyCode < WXK_UP)
           || (keyCode == WXK_RIGHT)
           || (keyCode >= WXK_NUMPAD0 && keyCode <= WXK_DIVIDE)
           || (keyCode >= WXK_NUMPAD_SPACE && keyCode <= WXK_NUMPAD_ENTER)
           || (keyCode >= WXK_NUMPAD_HOME && keyCode <= WXK_NUMPAD_END)
           || (keyCode >= WXK_NUMPAD_DELETE && keyCode <= WXK_NUMPAD_DIVIDE) ||
#if defined(__WXMAC__)
           (keyCode > WXK_RAW_CONTROL) ||
#endif
           (keyCode > WXK_WINDOWS_MENU);
}

TextEditHelper::TextEditHelper(const std::weak_ptr<TextEditDelegate>& delegate, const wxString& text, const wxFont& font)
    : mText(text),
    mFont(font),
    mInitialCursorPos(0),
    mDelegate(delegate)
{
    mCurrentCursorPos = text.Length();
}

void TextEditHelper::SetTextColor(const wxColor& textColor)
{
    mTextColor = textColor;
}

void TextEditHelper::SetTextSelectionColor(const wxColor& textSelectionColor)
{
    mTextSelectionColor = textSelectionColor;
}

void TextEditHelper::Cancel(AudacityProject* project)
{
    if (auto lock = mDelegate.lock()) {
        lock->OnTextEditCancelled(project);
    }
}

void TextEditHelper::Finish(AudacityProject* project)
{
    if (auto lock = mDelegate.lock()) {
        lock->OnTextEditFinished(project, mText);
    }
}

std::pair<int, int> TextEditHelper::GetSelection() const
{
    return std::make_pair(mInitialCursorPos, mCurrentCursorPos);
}

void TextEditHelper::SetSelection(int from, int to)
{
    mInitialCursorPos = from;
    mCurrentCursorPos = to;
}

void TextEditHelper::SelectAll()
{
    mInitialCursorPos = 0;
    mCurrentCursorPos = mText.Length();
}

bool TextEditHelper::IsSelectionEmpty()
{
    return mCurrentCursorPos == mInitialCursorPos;
}

bool TextEditHelper::CaptureKey(int, int mods)
{
    return mods == wxMOD_NONE || mods == wxMOD_SHIFT;
}

bool TextEditHelper::OnKeyDown(int keyCode, int mods, AudacityProject* project)
{
    auto delegate = mDelegate.lock();
    if (!delegate) {
        return false;
    }

    if (!CaptureKey(keyCode, mods)) {
        return false;
    }

    wxUniChar wchar;
    bool more = true;

    switch (keyCode) {
    case WXK_BACK:
    {
        //IF the label is not blank THEN get rid of a letter or letters according to cursor position
        if (!mText.empty()) {
            // IF there are some highlighted letters, THEN DELETE them
            if (mInitialCursorPos != mCurrentCursorPos) {
                RemoveSelectedText(project);
            } else {
                // DELETE one codepoint leftwards
                while ((mCurrentCursorPos > 0) && more) {
                    wchar = mText.at(mCurrentCursorPos - 1);
                    mText.erase(mCurrentCursorPos - 1, 1);
                    mCurrentCursorPos--;
                    if (((int)wchar > 0xDFFF) || ((int)wchar < 0xDC00)) {
                        delegate->OnTextModified(project, mText);
                        more = false;
                    }
                }
            }
            mInitialCursorPos = mCurrentCursorPos;
            mOffset = std::clamp(mOffset, 0, std::max(0, static_cast<int>(mText.Length()) - 1));
            return true;
        }
    }
    break;

    case WXK_DELETE:
    case WXK_NUMPAD_DELETE:
    {
        int len = mText.length();
        //If the label is not blank get rid of a letter according to cursor position
        if (len > 0) {
            // if there are some highlighted letters, DELETE them
            if (mInitialCursorPos != mCurrentCursorPos) {
                RemoveSelectedText(project);
            } else {
                // DELETE one codepoint rightwards
                while ((mCurrentCursorPos < len) && more) {
                    wchar = mText.at(mCurrentCursorPos);
                    mText.erase(mCurrentCursorPos, 1);
                    if (((int)wchar > 0xDBFF) || ((int)wchar < 0xD800)) {
                        delegate->OnTextModified(project, mText);
                        more = false;
                    }
                }
            }
            mInitialCursorPos = mCurrentCursorPos;
            mOffset = std::clamp(mOffset, 0, std::max(0, static_cast<int>(mText.Length()) - 1));
            return true;
        }
    }
    break;

    case WXK_HOME:
    case WXK_NUMPAD_HOME:
        // Move cursor to beginning of label
        mCurrentCursorPos = 0;
        if (mods == wxMOD_SHIFT) {
        } else {
            mInitialCursorPos = mCurrentCursorPos;
        }
        return true;
    case WXK_END:
    case WXK_NUMPAD_END:
        // Move cursor to end of label
        mCurrentCursorPos = (int)mText.length();
        if (mods == wxMOD_SHIFT) {
        } else {
            mInitialCursorPos = mCurrentCursorPos;
        }
        return true;

    case WXK_LEFT:
    case WXK_NUMPAD_LEFT:
        // Moving cursor left
        if (mods != wxMOD_SHIFT && mCurrentCursorPos != mInitialCursorPos) {
            //put cursor to the left edge of selection
            mInitialCursorPos = mCurrentCursorPos
                                    =std::min(mInitialCursorPos, mCurrentCursorPos);
        } else {
            while ((mCurrentCursorPos > 0) && more) {
                wchar = mText.at(mCurrentCursorPos - 1);
                more = !(((int)wchar > 0xDFFF) || ((int)wchar < 0xDC00));

                --mCurrentCursorPos;
            }
            if (mods != wxMOD_SHIFT) {
                mInitialCursorPos = mCurrentCursorPos;
            }
        }
        return true;

    case WXK_RIGHT:
    case WXK_NUMPAD_RIGHT:
        // Moving cursor right
        if (mods != wxMOD_SHIFT && mCurrentCursorPos != mInitialCursorPos) {
            //put cursor to the right edge of selection
            mInitialCursorPos = mCurrentCursorPos
                                    =std::max(mInitialCursorPos, mCurrentCursorPos);
        } else {
            while ((mCurrentCursorPos < (int)mText.length()) && more) {
                wchar = mText.at(mCurrentCursorPos);
                more = !(((int)wchar > 0xDBFF) || ((int)wchar < 0xD800));

                ++mCurrentCursorPos;
            }
            if (mods != wxMOD_SHIFT) {
                mInitialCursorPos = mCurrentCursorPos;
            }
        }

        return true;

    case WXK_ESCAPE:
        delegate->OnTextEditCancelled(project);
        return true;
    case WXK_RETURN:
    case WXK_NUMPAD_ENTER:
    case WXK_TAB:
        delegate->OnTextEditFinished(project, mText);
        return true;
    }
    return false;
}

bool TextEditHelper::OnChar(int charCode, AudacityProject* project)
{
    auto delegate = mDelegate.lock();
    if (!delegate) {
        return false;
    }

    if (charCode == 0 || wxIscntrl(charCode)) {
        return false;
    }

    // Test if cursor is in the end of string or not
    if (mInitialCursorPos != mCurrentCursorPos) {
        RemoveSelectedText(project);
    }

    if (mCurrentCursorPos < (int)mText.length()) {
        // Get substring on the righthand side of cursor
        wxString rightPart = mText.Mid(mCurrentCursorPos);
        // Set title to substring on the lefthand side of cursor
        mText = mText.Left(mCurrentCursorPos);
        //append charcode
        mText += charCode;
        //append the right part substring
        mText += rightPart;
    } else {
        //append charCode
        mText += charCode;
    }

    delegate->OnTextModified(project, mText);

    //moving cursor position forward
    mInitialCursorPos = ++mCurrentCursorPos;

    return true;
}

bool TextEditHelper::OnClick(const wxMouseEvent& event, AudacityProject*)
{
    if (event.ButtonDown()) {
        bool result = false;
        if (mBBox.Contains(event.GetPosition())) {
            if (event.LeftDown()) {
                mRightDragging = false;
                auto position = FindCursorIndex(event.GetPosition());
                auto initial = mInitialCursorPos;
                if (event.ShiftDown()) {
#ifdef __WXMAC__
                    // Set the drag anchor at the end of the previous selection
                    // that is farther from the NEW drag end
                    const auto current = mCurrentCursorPos;
                    if (abs(position - current) > abs(position - initial)) {
                        initial = current;
                    }
#else
                    // initial position remains as before
#endif
                } else {
                    initial = position;
                }

                mInitialCursorPos = initial;
                mCurrentCursorPos = position;
            } else {
                if (mInitialCursorPos == mCurrentCursorPos) {
                    auto position = FindCursorIndex(event.GetPosition());
                    mInitialCursorPos = mCurrentCursorPos = position;
                }
                // Actually this might be right or middle down
                mRightDragging = true;
            }
            result = true;
        }
#if defined(__WXGTK__) && (HAVE_GTK)
        if (evt.MiddleDown()) {
            // Paste text, making a NEW label if none is selected.
            wxTheClipboard->UsePrimarySelection(true);
            view.PasteSelectedText(project, newSel.t0(), newSel.t1());
            wxTheClipboard->UsePrimarySelection(false);
            result = true;
        }
#endif
        return result;
    }
    return false;
}

bool TextEditHelper::OnDrag(const wxMouseEvent& event, AudacityProject* project)
{
    return HandleDragRelease(event, project);
}

bool TextEditHelper::OnRelease(const wxMouseEvent& event, AudacityProject* project)
{
    return HandleDragRelease(event, project);
}

bool TextEditHelper::Draw(wxDC& dc, const wxRect& rect)
{
    mBBox = rect;

    if (rect.IsEmpty()) {
        return false;
    }

    const auto cursorHeight = dc.GetFontMetrics().height;

    dc.SetFont(mFont);

    wxDCClipper clipper(dc, rect);

    auto curPosX = 0;
    auto maxOffset = static_cast<int>(mText.Length());
    mOffset = 0;
    if (maxOffset > 0) {
        const auto rtl = wxTheApp->GetLayoutDirection() == wxLayout_RightToLeft;
        {
            auto leftBound = rect.GetLeft();
            auto rightBound = rect.GetRight() + 1;
            GetCharPositionX(mCurrentCursorPos, &curPosX);

            if ((!rtl && curPosX >= rightBound) || (rtl && curPosX < leftBound)) {
                while (mOffset < maxOffset)
                {
                    GetCharPositionX(mCurrentCursorPos, &curPosX);
                    if (curPosX < rightBound && curPosX >= leftBound) {
                        break;
                    }
                    ++mOffset;
                }
            }
            if ((!rtl && curPosX < leftBound) || (rtl && curPosX >= rightBound)) {
                while (mOffset > 0)
                {
                    GetCharPositionX(mCurrentCursorPos, &curPosX);
                    if (curPosX >= leftBound && curPosX < rightBound) {
                        break;
                    }
                    --mOffset;
                }
            }
        }
        // Text doesn't fit into rectangle
        if (mOffset >= maxOffset) {
            return false;
        }

        if (mCurrentCursorPos != mInitialCursorPos) {
            auto left = 0;
            auto right = 0;
            GetCharPositionX(std::min(mCurrentCursorPos, mInitialCursorPos), &left);
            GetCharPositionX(std::max(mCurrentCursorPos, mInitialCursorPos), &right);
            dc.SetPen(*wxTRANSPARENT_PEN);
            dc.SetBrush(mTextSelectionColor);
            dc.DrawRectangle(wxRect(left, rect.GetTop() + (rect.GetHeight() - cursorHeight) / 2, right - left, cursorHeight));
        }

        dc.SetTextBackground(wxTransparentColour);
        dc.SetTextForeground(mTextColor);
        dc.SetFont(wxFont(wxFontInfo()));
        dc.DrawLabel(mText.Mid(mOffset), rect, (rtl ? wxALIGN_RIGHT : wxALIGN_LEFT) | wxALIGN_CENTER_VERTICAL);
    } else {
        mCurrentCursorPos = mInitialCursorPos = 0;
        GetCharPositionX(mCurrentCursorPos, &curPosX);
    }

    if (mCurrentCursorPos == mInitialCursorPos) {
        dc.SetPen(mTextColor);
        auto top = rect.GetTop() + (rect.GetHeight() - cursorHeight) / 2;
        dc.DrawLine(curPosX, top, curPosX, top + cursorHeight);
    }
    return true;
}

bool TextEditHelper::HandleDragRelease(const wxMouseEvent& event, AudacityProject* project)
{
    if (event.Dragging()) {
        if (!mRightDragging) {
            mCurrentCursorPos = FindCursorIndex(event.GetPosition());
            return true;
        }
    } else if (event.RightUp() && mBBox.Contains(event.GetPosition())) {
        auto delegate = mDelegate.lock();
        if (delegate) {
            // popup menu for editing
            // TODO: handle context menus via CellularPanel?
            delegate->OnTextContextMenu(project, event.GetPosition());
            return true;
        }
    }
    return false;
}

void TextEditHelper::RemoveSelectedText(AudacityProject* project)
{
    auto delegate = mDelegate.lock();
    if (!delegate) {
        return;
    }

    wxString left, right;

    int init = mInitialCursorPos;
    int cur = mCurrentCursorPos;
    if (init > cur) {
        std::swap(init, cur);
    }

    if (init > 0) {
        left = mText.Left(init);
    }

    if (cur < (int)mText.length()) {
        right = mText.Mid(cur);
    }

    mText = left + right;

    delegate->OnTextModified(project, mText);

    mInitialCursorPos = mCurrentCursorPos = left.length();
}

int TextEditHelper::FindCursorIndex(const wxPoint& point)
{
    int result = -1;
    wxMemoryDC dc;
    if (mFont.Ok()) {
        dc.SetFont(mFont);
    }

    // A bool indicator to see if set the cursor position or not
    bool finished = false;
    int charIndex = 1;
    int partWidth;
    int oneWidth;
    //double bound;
    wxString subString;

    auto offsetX = 0;
    if (mOffset > 0) {
        offsetX = dc.GetTextExtent(mText.Left(mOffset)).GetWidth();
    }

    const auto layout = wxTheApp->GetLayoutDirection();

    const int length = mText.length();
    while (!finished && (charIndex < length + 1))
    {
        int unichar = (int)mText.at(charIndex - 1);
        if ((0xDC00 <= unichar) && (unichar <= 0xDFFF)) {
            charIndex++;
            continue;
        }
        subString = mText.Left(charIndex);
        // Get the width of substring
        dc.GetTextExtent(subString, &partWidth, NULL);

        // Get the width of the last character
        dc.GetTextExtent(subString.Right(1), &oneWidth, NULL);

        if (layout == wxLayout_RightToLeft) {
            auto bound = mBBox.GetRight() - partWidth + offsetX + oneWidth / 2;
            if (point.x >= bound) {
                result = charIndex - 1;
                finished = true;
            }
        } else {
            auto bound = mBBox.GetLeft() + partWidth - offsetX - oneWidth / 2;
            if (point.x <= bound) {
                result = charIndex - 1;
                finished = true;
            }
        }
        if (!finished) {
            ++charIndex;
        } else {
            break;
        }
    }
    if (!finished) {
        // Cursor should be in the last position
        result = length;
    }

    return result;
}

bool TextEditHelper::GetCharPositionX(int index, int* outX)
{
    if (!mFont.Ok()) {
        return false;
    }

    wxMemoryDC dc;
    dc.SetFont(mFont);

    int offsetX{ 0 };
    if (mOffset > 0) {
        offsetX = dc.GetTextExtent(mText.Left(mOffset)).GetWidth();
    }

    if (wxTheApp->GetLayoutDirection() == wxLayout_RightToLeft) {
        if (index <= 0) {
            *outX = mBBox.GetRight() + offsetX;
        } else {
            *outX = mBBox.GetRight() - dc.GetTextExtent(mText.Left(index)).GetWidth() + offsetX;
        }
    } else {
        if (index <= 0) {
            *outX = mBBox.GetLeft() - offsetX;
        } else {
            *outX = mBBox.GetLeft() + dc.GetTextExtent(mText.Left(index)).GetWidth() - offsetX;
        }
    }

    return true;
}

const wxRect& TextEditHelper::GetBBox() const
{
    return mBBox;
}

/// Cut the selected text in the text box
///  @return true if text is selected in text box, false otherwise
bool TextEditHelper::CutSelectedText(AudacityProject& project)
{
    auto delegate = mDelegate.lock();
    if (!delegate) {
        return false;
    }

    if (mCurrentCursorPos == mInitialCursorPos) {
        return false;
    }

    int init = mInitialCursorPos;
    int cur = mCurrentCursorPos;
    if (init > cur) {
        std::swap(init, cur);
    }

    wxString left, right;
    // data for cutting
    wxString data = mText.Mid(init, cur - init);

    // get left-remaining text
    if (init > 0) {
        left = mText.Left(init);
    }

    // get right-remaining text
    if (cur < (int)mText.length()) {
        right = mText.Mid(cur);
    }

    // set title to the combination of the two remainders
    mText = left + right;

    delegate->OnTextModified(&project, mText);
    // copy data onto clipboard
    if (wxTheClipboard->Open()) {
        // Clipboard owns the data you give it
        wxTheClipboard->SetData(safenew wxTextDataObject(data));
        wxTheClipboard->Close();
    }

    // set cursor positions
    mInitialCursorPos = mCurrentCursorPos = left.length();

    return true;
}

/// Copy the selected text in the text box
///  @return true if text is selected in text box, false otherwise
bool TextEditHelper::CopySelectedText(AudacityProject& project)
{
    if (mCurrentCursorPos == mInitialCursorPos) {
        return false;
    }

    int init = mInitialCursorPos;
    int cur = mCurrentCursorPos;
    if (init > cur) {
        std::swap(init, cur);
    }

    if (init == cur) {
        return false;
    }

    // data for copying
    wxString data = mText.Mid(init, cur - init);

    // copy the data on clipboard
    if (wxTheClipboard->Open()) {
        // Clipboard owns the data you give it
        wxTheClipboard->SetData(safenew wxTextDataObject(data));
        wxTheClipboard->Close();
    }

    return true;
}

// PRL:  should this set other fields of the label selection?
/// Paste the text on the clipboard to text box
///  @return true if mouse is clicked in text box, false otherwise
bool TextEditHelper::PasteSelectedText(AudacityProject& project)
{
    auto delegate = mDelegate.lock();
    if (!delegate) {
        return false;
    }

    wxString text, left, right;

    // if text data is available
    if (wxTheClipboard->IsSupported(wxDF_UNICODETEXT)) {
        if (wxTheClipboard->Open()) {
            wxTextDataObject data;
            wxTheClipboard->GetData(data);
            wxTheClipboard->Close();
            text = data.GetText();
        }

        // Convert control characters to blanks
        for (int i = 0; i < (int)text.length(); i++) {
            if (wxIscntrl(text[i])) {
                text[i] = wxT(' ');
            }
        }
    }

    int cur = mCurrentCursorPos, init = mInitialCursorPos;
    if (init > cur) {
        std::swap(init, cur);
    }

    left = mText.Left(init);
    if (cur < (int)mText.length()) {
        right = mText.Mid(cur);
    }

    mText = left + text + right;

    delegate->OnTextModified(&project, mText);

    mInitialCursorPos = mCurrentCursorPos = left.length() + text.length();

    return true;
}
