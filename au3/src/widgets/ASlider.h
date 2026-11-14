/**********************************************************************

  Audacity: A Digital Audio Editor

  ASlider.h

  Dominic Mazzoni

  This class is a custom slider.

**********************************************************************/

#ifndef __AUDACITY_SLIDER__
#define __AUDACITY_SLIDER__

#include <wx/setup.h> // for wxUSE_* macros
#include <wx/defs.h>
#include <wx/timer.h> // member variable
#include "wxPanelWrapper.h" // to inherit

class wxBitmap;
class wxSize;
class wxPoint;
class wxTextCtrl;

class Ruler;
class TipWindow;

//
// Predefined slider types (mStyle)
//
#define FRAC_SLIDER 1    // 0.0...1.0
#define DB_SLIDER 2      // -36...36 dB
#define PAN_SLIDER 3     // -1.0...1.0
#define SPEED_SLIDER 4  // 0.01 ..3.0

#define VEL_SLIDER 5    // -50..50

#define PERCENT_SLIDER 6 //0% .. 100%

#define DB_MIN -36.0f
#define DB_MAX 36.0f
#define FRAC_MIN 0.0f
#define FRAC_MAX 1.0f
#define SPEED_MIN 0.001f
#define SPEED_MAX 3.000f
#define VEL_MIN -50.0f
#define VEL_MAX 50.0f

// Customizable slider only: If stepValue is STEP_CONTINUOUS,
// every value on the slider between minValue and maxValue
// will be possible
//
#define STEP_CONTINUOUS 0.0f

//
// Lightweight slider - i.e. a slider that doesn't appear in
// its own window, but rather draws itself inside an existing
// window (used inside Track Labels).  The ASlider class,
// which uses this class, is below.
//

class AUDACITY_DLL_API LWSlider
{
    friend class ASlider;
    friend class ASliderAx;

public:

    // MM: Construct customizable slider
    LWSlider(wxWindow* parent, const TranslatableString& name, const wxPoint& pos, const wxSize& size, float minValue, float maxValue,
             float stepValue, bool canUseShift, int style, bool showlabels=true, bool drawticks=true, bool drawtrack=true,
             bool alwayshidetip=false, bool heavyweight=false, bool popup=true, int orientation = wxHORIZONTAL); // wxHORIZONTAL or wxVERTICAL. wxVERTICAL is currently only for DB_SLIDER.

    // Construct predefined slider
    LWSlider(wxWindow* parent, const TranslatableString& name, const wxPoint& pos, const wxSize& size, int style, bool showlabels=true,
             bool drawticks=true, bool drawtrack=true, bool alwayshidetip=false, bool heavyweight=false, bool popup=true,
             int orientation = wxHORIZONTAL); // wxHORIZONTAL or wxVERTICAL. wxVERTICAL is currently only for DB_SLIDER.

    void Init(wxWindow* parent, const TranslatableString& name, const wxPoint& pos, const wxSize& size, float minValue, float maxValue,
              float stepValue, bool canUseShift, int style, bool showlabels, bool drawticks, bool drawtrack, bool alwayshidetip,
              bool heavyweight, bool popup, float speed, int orientation = wxHORIZONTAL); // wxHORIZONTAL or wxVERTICAL. wxVERTICAL is currently only for DB_SLIDER.

    virtual ~LWSlider();

    wxWindowID GetId();
    void SetId(wxWindowID id);

    void SetName(const TranslatableString& name);

    void SetDefaultValue(float value);
    void SetDefaultShortcut(bool value);

    void GetScroll(float& line, float& page);
    void SetScroll(float line, float page);

    void ShowTip(bool show);
    void SetToolTipTemplate(const TranslatableString& tip);

    float Get(bool convert = true);
    void Set(float value);

    void Increase(float steps);
    void Decrease(float steps);

    // If set to less than 1.0, moving the mouse one pixel will move
    // the slider by less than 1 unit
    void SetSpeed(float speed);

    void Move(const wxPoint& newpos);

    void AdjustSize(const wxSize& sz);

    void OnPaint(wxDC& dc, bool highlighted);
    void OnSize(wxSizeEvent& event);
    void OnMouseEvent(wxMouseEvent& event);
    void OnKeyDown(wxKeyEvent& event);
    void Refresh();
    void Redraw();

    bool ShowDialog();
    bool ShowDialog(wxPoint pos);

    void SetEnabled(bool enabled);
    bool GetEnabled() const;

    float GetMinValue() const;
    float GetMaxValue() const;

    void SetParent(wxWindow* parent);
    void SendUpdate(float newValue);

    wxString GetStringValue() const;

    void OnKillFocus();

private:

    TranslatableString GetTip(float value) const;
    TranslatableStrings GetWidestTips() const;
    void FormatPopWin();
    void SetPopWinPosition();
    void CreatePopWin();
    void DrawToBitmap(wxDC& dc);

    bool DoShowDialog(wxPoint pos);

    int ValueToPosition(float val);
    float DragPositionToValue(int fromPos, bool shiftDown);
    float ClickPositionToValue(int fromPos, bool shiftDown);

    wxWindow* mParent;

    int mStyle;
    int mOrientation; // wxHORIZONTAL or wxVERTICAL. wxVERTICAL is currently only for DB_SLIDER.

    bool mShowLabels;
    bool mDrawTicks;
    bool mDrawTrack;

    bool mAlwaysHideTip;

    bool mHW; // is it really heavyweight (in a window)
    bool mPopup; // should display dialog on double click

    int mLeft;
    int mTop;

    int mWidth;                 //In pixels
    int mHeight;                //In pixels

    // for (mOrientation == wxHORIZONTAL)
    int mCenterY;

    int mLeftX;
    int mRightX;
    int mWidthX;

    // for (mOrientation == wxVERTICAL) //v Vertical PAN_SLIDER currently not handled, forced to horizontal.
    int mCenterX;

    int mTopY;
    int mBottomY; // low values at bottom
    int mHeightY;

    int mThumbWidth;            //In pixels
    int mThumbHeight;           //In pixels

    float mClickValue;
    int mClickPos; // position in x if (mOrientation == wxHORIZONTAL), else in y

    float mMinValue;
    float mMaxValue;
    float mStepValue;
    float mSpeed;

    float mScrollLine;
    float mScrollPage;

    float mCurrentValue;

    bool mDefaultShortcut;
    float mDefaultValue;

    bool mCanUseShift;

    wxWindowID mID;

    wxWeakRef<TipWindow> mTipPanel;
    TranslatableString mTipTemplate;

    bool mIsDragging;

    std::unique_ptr<wxBitmap> mBitmap, mThumbBitmap, mThumbBitmapHilited;

    TranslatableString mName;

    bool mEnabled;
};

class AUDACITY_DLL_API ASlider /* not final */ : public wxPanel
{
    friend class ASliderAx;

public:
    struct Options {
        Options() {}

        int style{ FRAC_SLIDER };
        wxOrientation orientation{ wxHORIZONTAL };
        bool showLabels{ true };
        bool drawTicks{ true };
        bool drawTrack{ true };
        bool alwaysHideTip{ false };
        bool popup{ true };
        bool canUseShift{ true };
        float stepValue{ STEP_CONTINUOUS };

        float line{ 1.0 };
        float page{ 5.0 };

        Options& Style(int s) { style = s; return *this; }
        Options& Orientation(wxOrientation o)
        { orientation = o; return *this; }
        Options& ShowLabels(bool l) { showLabels = l; return *this; }
        Options& DrawTicks(bool t) { drawTicks = t; return *this; }
        Options& DrawTrack(bool t) { drawTrack = t; return *this; }
        Options& AlwayHideTip(bool t) { alwaysHideTip = t; return *this; }
        Options& Popup(bool p) { popup = p; return *this; }
        Options& CanUseShift(bool c) { canUseShift = c; return *this; }
        Options& StepValue(float v) { stepValue = v; return *this; }

        Options& Line(float l) { line = l; return *this; }
        Options& Page(float p) { page = p; return *this; }
    };

    ASlider(wxWindow* parent, wxWindowID id, const TranslatableString& name, const wxPoint& pos, const wxSize& size,
            const Options& options = Options {});
    virtual ~ASlider();

    bool AcceptsFocus() const override { return s_AcceptsFocus; }
    bool AcceptsFocusFromKeyboard() const override { return true; }

    void SetFocusFromKbd() override;

    bool SetBackgroundColour(const wxColour& colour) override;

    void GetScroll(float& line, float& page);
    void SetScroll(float line, float page);

    void SetToolTipTemplate(const TranslatableString& tip);

    float Get(bool convert = true);
    void Set(float value);

    void Increase(float steps);
    void Decrease(float steps);
    bool ShowDialog(wxPoint pos = wxPoint(-1, -1));

    void SetSpeed(float speed);

    void OnErase(wxEraseEvent& event);
    void OnPaint(wxPaintEvent& event);
    void OnSize(wxSizeEvent& event);
    void OnMouseEvent(wxMouseEvent& event);
    void OnCaptureLost(wxMouseCaptureLostEvent& event);
    void OnKeyDown(wxKeyEvent& event);
    void OnSlider(wxCommandEvent& event);
    void OnSetFocus(wxFocusEvent& event);
    void OnKillFocus(wxFocusEvent& event);
    void OnTimer(wxTimerEvent& event);

    // Overrides of the wxWindow functions with the same semantics
    bool Enable(bool enable = true) override;
    bool IsEnabled() const;

private:
    static bool s_AcceptsFocus;
    struct Resetter {
        void operator ()(bool* p) const
        {
            if (p) {
                *p = false;
            }
        }
    };
    using TempAllowFocus = std::unique_ptr<bool, Resetter>;

public:
    static TempAllowFocus TemporarilyAllowFocus();

private:
    std::unique_ptr<LWSlider> mLWSlider;
    bool mSliderIsFocused;
    wxTimer mTimer;

protected:
    int mStyle;

public:
    DECLARE_EVENT_TABLE()
};

#define SLIDER_DIALOG_TEXTCTRL 100

// This is a modal dialog that contains an ASlider
// and a text-entry box which can be used to set the
// value of a slider.
class SliderDialog final : public wxDialogWrapper
{
public:
    SliderDialog(wxWindow* parent, wxWindowID id, const TranslatableString& title, wxPoint position, wxSize size, int style, float value,
                 float line, float page, LWSlider* pSlider=nullptr);
    ~SliderDialog();

    float Get();

private:
    bool TransferDataToWindow() override;
    bool TransferDataFromWindow() override;

    void OnSlider(wxCommandEvent& event);
    void OnTextChange(wxCommandEvent& event);

    ASlider* mSlider;
    wxTextCtrl* mTextCtrl;
    int mStyle;
    LWSlider* mpOrigin;
    float mValue;

public:
    DECLARE_EVENT_TABLE()
};

#endif
