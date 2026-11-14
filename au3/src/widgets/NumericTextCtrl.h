/**********************************************************************

  Audacity: A Digital Audio Editor

  NumericTextCtrl.h

  Dominic Mazzoni

  See NumericConverter.cpp for documentation on how to use the
  format string to specify how a NumericTextCtrl's fields are
  laid out.

**********************************************************************/
#ifndef __AUDACITY_TIME_TEXT_CTRL__
#define __AUDACITY_TIME_TEXT_CTRL__

#include "NumericConverter.h"

#include "MemoryX.h"
#include "ComponentInterface.h"
#include <vector>
#include <wx/setup.h> // for wxUSE_* macros
#include <wx/defs.h>
#include <wx/control.h> // to inherit

// One event type for each type of control.  Event is raised when a control
// changes its format.  Owners of controls of the same type can listen and
// update their formats to agree.
DECLARE_EXPORTED_EVENT_TYPE(AUDACITY_DLL_API, EVT_TIMETEXTCTRL_UPDATED, -1);
DECLARE_EXPORTED_EVENT_TYPE(AUDACITY_DLL_API, EVT_FREQUENCYTEXTCTRL_UPDATED, -1);
DECLARE_EXPORTED_EVENT_TYPE(AUDACITY_DLL_API, EVT_BANDWIDTHTEXTCTRL_UPDATED,
                            -1);

class AUDACITY_DLL_API NumericTextCtrl final : public wxControl, public NumericConverter
{
    friend class NumericTextCtrlAx;

public:
    DECLARE_DYNAMIC_CLASS(NumericTextCtrl)

    struct Options {
        bool autoPos { true };
        bool readOnly { false };
        bool menuEnabled { true };
        bool hasInvalidValue { false };
        double invalidValue { -1.0 };
        NumericFormatID formatSymbol {};
        TranslatableString customFormat {};
        bool hasValue { false };
        double value{ -1.0 };

        Options() {}

        Options& AutoPos(bool enable) { autoPos = enable; return *this; }
        Options& ReadOnly(bool enable) { readOnly = enable; return *this; }
        Options& MenuEnabled(bool enable) { menuEnabled = enable; return *this; }
        Options& InvalidValue(bool has, double v = -1.0)
        { hasInvalidValue = has, invalidValue = v; return *this; }
        // use a custom format not in the tables:
        Options& FormatSymbol(const NumericFormatID& f)
        { formatSymbol = f; return *this; }
        Options& CustomFormat(const TranslatableString& f)
        { customFormat = f; return *this; }
        Options& Value(bool has, double v)
        { hasValue = has, value = v; return *this; }
    };

    NumericTextCtrl(
        const FormatterContext& context, wxWindow* parent, wxWindowID winid, NumericConverterType type,
        const NumericFormatID& formatName = {}, double value = 0.0, const Options& options = {}, const wxPoint& pos = wxDefaultPosition,
        const wxSize& size = wxDefaultSize);

    virtual ~NumericTextCtrl();

    // Hide the inherited function that takes wxString
    void SetName(const TranslatableString& name);

    wxSize ComputeSizing(bool update = true, wxCoord digitW = 0, wxCoord digitH = 0);

    bool Layout() override;
    void Fit() override;

    void SetValue(double newValue);

    // returns true if the format type really changed:
    bool SetTypeAndFormatName(
        const NumericConverterType& type, const NumericFormatID& formatName);
    // returns true iff the format name really changed:
    bool SetFormatName(const NumericFormatID& formatName);
    bool SetCustomFormat(const TranslatableString& customFormat);

    void SetFieldFocus(int /* digit */);

    wxSize GetDimensions() { return wxSize(mWidth + mButtonWidth, mHeight); }
    wxSize GetDigitSize() { return wxSize(mDigitBoxW, mDigitBoxH); }
    void SetDigitSize(int width, int height);
    void SetReadOnly(bool readOnly = true);
    void EnableMenu(bool enable = true);

    // The text control permits typing DELETE to make the value invalid only if this
    // function has previously been called.
    // Maybe you want something other than the default of -1 to indicate the invalid value
    // this control returns to the program, so you can specify.
    void SetInvalidValue(double invalidValue);

    int GetFocusedDigit() { return mFocusedDigit; }

private:
    void OnFormatUpdated(bool resetFocus) override;
    void HandleFormatterChanged(bool resetFocus);

    void OnCaptureKey(wxCommandEvent& event);
    void OnKeyDown(wxKeyEvent& event);
    void OnKeyUp(wxKeyEvent& event);
    void OnMouse(wxMouseEvent& event);
    void OnErase(wxEraseEvent& event);
    void OnPaint(wxPaintEvent& event);
    void OnFocus(wxFocusEvent& event);
    void OnContext(wxContextMenuEvent& event);

    // Formats mValue into mValueString, using the method of the base class.
    // Triggers a refresh of the wx window only when the value actually
    // changed since last time a refresh was triggered.
    void ValueToControls() override;
    void ControlsToValue() override;

    // If autoPos was enabled, focus the first non-zero digit
    void UpdateAutoFocus();

    void Updated(bool keyup = false);

private:
    struct FieldPosition final
    {
        int fieldX;  // x-position of the field on-screen
        int fieldW;  // width of the field on-screen
        int labelX;  // x-position of the label on-screen
    };

    std::vector<wxRect> mBoxes;
    std::vector<FieldPosition> mFieldPositions;

    wxRect GetBox(size_t ii) const;

    bool mMenuEnabled;
    bool mReadOnly;

    std::unique_ptr<wxBitmap> mBackgroundBitmap;

    std::unique_ptr<wxFont> mDigitFont, mLabelFont;
    int mDigitBoxW;
    int mDigitBoxH;
    int mDigitW;
    int mDigitH;
    int mBorderLeft;
    int mBorderTop;
    int mBorderRight;
    int mBorderBottom;
    int mWidth;
    int mHeight;
    int mButtonWidth;

    int mFocusedDigit { 0 };

    // If true, the focus will be set to the first non-zero digit
    bool mAutoPos;

    // Keeps track of extra fractional scrollwheel steps
    double mScrollRemainder;

    NumericConverterType mType;

    bool mAllowInvalidValue;

    DECLARE_EVENT_TABLE()
};

#endif // __AUDACITY_TIME_TEXT_CTRL__
