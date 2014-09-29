/////////////////////////////////////////////////////////////////////////////
//
// Backport from wxWidgets-3.0-rc1
//
/////////////////////////////////////////////////////////////////////////////
// Name:        src/common/valnum.cpp
// Purpose:     Numeric validator classes implementation
// Author:      Vadim Zeitlin based on the submission of Fulvio Senore
// Created:     2010-11-06
// Copyright:   (c) 2010 wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

// ============================================================================
// Declarations
// ============================================================================

// ----------------------------------------------------------------------------
// headers
// ----------------------------------------------------------------------------

// For compilers that support precompilation, includes "wx.h".
#include "wx/wxprec.h"

#ifdef __BORLANDC__
    #pragma hdrstop
#endif

#if wxUSE_VALIDATORS && wxUSE_TEXTCTRL

#ifndef WX_PRECOMP
    #include "wx/textctrl.h"
    #include "wx/combobox.h"
#endif

#include "valnum.h"
#include "numformatter.h"

// ============================================================================
// wxNumValidatorBase implementation
// ============================================================================

BEGIN_EVENT_TABLE(wxNumValidatorBase, wxValidator)
    EVT_CHAR(wxNumValidatorBase::OnChar)
    EVT_KILL_FOCUS(wxNumValidatorBase::OnKillFocus)
END_EVENT_TABLE()

int wxNumValidatorBase::GetFormatFlags() const
{
    int flags = wxNumberFormatter::Style_None;
    if ( m_style & wxNUM_VAL_THOUSANDS_SEPARATOR )
        flags |= wxNumberFormatter::Style_WithThousandsSep;
    if ( m_style & wxNUM_VAL_NO_TRAILING_ZEROES )
        flags |= wxNumberFormatter::Style_NoTrailingZeroes;
    if ( m_style & wxNUM_VAL_ONE_TRAILING_ZERO )
        flags |= wxNumberFormatter::Style_OneTrailingZero;
    if ( m_style & wxNUM_VAL_TWO_TRAILING_ZEROES )
        flags |= wxNumberFormatter::Style_TwoTrailingZeroes;
    if ( m_style & wxNUM_VAL_THREE_TRAILING_ZEROES )
        flags |= wxNumberFormatter::Style_ThreeTrailingZeroes;

    return flags;
}

wxTextEntry *wxNumValidatorBase::GetTextEntry() const
{
#if wxUSE_TEXTCTRL
    if ( wxTextCtrl *text = wxDynamicCast(m_validatorWindow, wxTextCtrl) )
        return text;
#endif // wxUSE_TEXTCTRL

    wxFAIL_MSG(wxT("Can only be used with wxTextCtrl or wxComboBox"));

    return NULL;
}

void
wxNumValidatorBase::GetCurrentValueAndInsertionPoint(wxString& val,
                                                             int& pos) const
{
    wxTextEntry * const control = GetTextEntry();
    if ( !control )
        return;

    val = control->GetValue();
    pos = control->GetInsertionPoint();

    long selFrom, selTo;
    control->GetSelection(&selFrom, &selTo);

    const long selLen = selTo - selFrom;
    if ( selLen )
    {
        // Remove selected text because pressing a key would make it disappear.
        val.erase(selFrom, selLen);

        // And adjust the insertion point to have correct position in the new
        // string.
        if ( pos > selFrom )
        {
            if ( pos >= selTo )
                pos -= selLen;
            else
                pos = selFrom;
        }
    }
}

bool wxNumValidatorBase::IsMinusOk(const wxString& val, int pos) const
{
    // Minus is only ever accepted in the beginning of the string.
    if ( pos != 0 )
        return false;

    // And then only if there is no existing minus sign there.
    if ( !val.empty() && val[0] == '-' )
        return false;

    return true;
}

void wxNumValidatorBase::OnChar(wxKeyEvent& event)
{
    // By default we just validate this key so don't prevent the normal
    // handling from taking place.
    event.Skip();

    if ( !m_validatorWindow )
        return;

#if wxUSE_UNICODE
    const int ch = event.GetUnicodeKey();
    const int c = event.GetKeyCode();
    if ( c > WXK_START )
    {
        // It's a character without any Unicode equivalent at all, e.g. cursor
        // arrow or function key, we never filter those.
        return;
    }
#else // !wxUSE_UNICODE
    const int ch = event.GetKeyCode();
    if ( ch > WXK_DELETE )
    {
        // Not a character neither.
        return;
    }
#endif // wxUSE_UNICODE/!wxUSE_UNICODE

    if ( c < WXK_SPACE || c == WXK_DELETE )
    {
        // Allow ASCII control characters and Delete.
        return;
    }

    // Check if this character is allowed in the current state.
    wxString val;
    int pos;
    GetCurrentValueAndInsertionPoint(val, pos);

    if ( !IsCharOk(val, pos, ch) )
    {
        if ( !wxValidator::IsSilent() )
            wxBell();

        // Do not skip the event in this case, stop handling it here.
        event.Skip(false);
    }
}

void wxNumValidatorBase::OnKillFocus(wxFocusEvent& event)
{
    wxTextEntry * const control = GetTextEntry();
    if ( !control )
        return;

    // When we change the control value below, its "modified" status is reset
    // so we need to explicitly keep it marked as modified if it was so in the
    // first place.
    //
    // Notice that only wxTextCtrl (and not wxTextEntry) has
    // IsModified()/MarkDirty() methods hence the need for dynamic cast.
    wxTextCtrl * const text = wxDynamicCast(m_validatorWindow, wxTextCtrl);
    const bool wasModified = text ? text->IsModified() : false;

    control->ChangeValue(NormalizeString(control->GetValue()));

    if ( wasModified )
        text->MarkDirty();

    event.Skip();
}

// ============================================================================
// wxIntegerValidatorBase implementation
// ============================================================================

wxString wxIntegerValidatorBase::ToString(LongestValueType value) const
{
    return wxNumberFormatter::ToString(value, GetFormatFlags());
}

bool
wxIntegerValidatorBase::FromString(const wxString& s, LongestValueType *value)
{
    return wxNumberFormatter::FromString(s, value);
}

bool
wxIntegerValidatorBase::IsCharOk(const wxString& val, int pos, wxChar ch) const
{
    // We may accept minus sign if we can represent negative numbers at all.
    if ( ch == '-' )
    {
        // Notice that entering '-' can make our value invalid, for example if
        // we're limited to -5..15 range and the current value is 12, then the
        // new value would be (invalid) -12. We consider it better to let the
        // user do this because perhaps he is going to press Delete key next to
        // make it -2 and forcing him to delete 1 first would be unnatural.
        //
        // TODO: It would be nice to indicate that the current control contents
        //       is invalid (if it's indeed going to be the case) once
        //       wxValidator supports doing this non-intrusively.
        return m_min < 0 && IsMinusOk(val, pos);
    }

    // We only accept digits here (remember that '-' is taken care of by the
    // base class already).
    if ( ch < '0' || ch > '9' )
        return false;

    // And the value after insertion needs to be in the defined range.
    LongestValueType value;
    if ( !FromString(GetValueAfterInsertingChar(val, pos, ch), &value) )
        return false;

    wxString smin = ToString(m_min);
    wxString smax = ToString(m_max);
    if ( pos < (int) smin.Length() )
       return true;
    if ( pos < (int) smax.Length() - 1 )
       return true;

    return IsInRange(value);
}

// ============================================================================
// wxFloatingPointValidatorBase implementation
// ============================================================================

wxString wxFloatingPointValidatorBase::ToString(LongestValueType value) const
{
    return wxNumberFormatter::ToString(value, m_precision, GetFormatFlags());
}

bool
wxFloatingPointValidatorBase::FromString(const wxString& s,
                                         LongestValueType *value)
{
    return wxNumberFormatter::FromString(s, value);
}

bool
wxFloatingPointValidatorBase::IsCharOk(const wxString& val,
                                       int pos,
                                       wxChar ch) const
{
    // We may accept minus sign if we can represent negative numbers at all.
    if ( ch == '-' )
        return m_min < 0 && IsMinusOk(val, pos);

    const wxChar separator = wxNumberFormatter::GetDecimalSeparator();
    if ( ch == separator )
    {
        if ( val.find(separator) != wxString::npos )
        {
            // There is already a decimal separator, can't insert another one.
            return false;
        }

        // Prepending a separator before the minus sign isn't allowed.
        if ( pos == 0 && !val.empty() && val[0] == '-' )
            return false;

        // Otherwise always accept it, adding a decimal separator doesn't
        // change the number value and, in particular, can't make it invalid.
        // OTOH the checks below might not pass because strings like "." or
        // "-." are not valid numbers so parsing them would fail, hence we need
        // to treat it specially here.
        return true;
    }

    // Must be a digit then.
    if ( ch < '0' || ch > '9' )
        return false;

    // Check whether the value we'd obtain if we accepted this key is correct.
    const wxString newval(GetValueAfterInsertingChar(val, pos, ch));

    LongestValueType value;
    if ( !FromString(newval, &value) )
        return false;

    // Also check that it doesn't have too many decimal digits.
    const size_t posSep = newval.find(separator);
    if ( posSep != wxString::npos && newval.length() - posSep - 1 > m_precision )
        return false;

    // Finally check whether it is in the range.
    return IsInRange(value);
}

#endif // wxUSE_VALIDATORS && wxUSE_TEXTCTRL
