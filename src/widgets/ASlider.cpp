/**********************************************************************

  Audacity: A Digital Audio Editor

  ASlider.cpp

  Dominic Mazzoni

*******************************************************************//**

\class ASlider
\brief ASlider is a custom slider, allowing for a slicker look and
feel.

It allows you to use images for the slider background and
the thumb.

*//****************************************************************//**

\class LWSlider
\brief Lightweight version of ASlider.  In other words it does not
have a window permanently associated with it.

*//****************************************************************//**

\class SliderDialog
\brief Pop up dialog used with an LWSlider.

*//****************************************************************//**

\class TipPanel
\brief A wxPopupWindow used to give the numerical value of an LWSlider
or ASlider.

*//*******************************************************************/


#include "../Audacity.h"

#include <math.h>

#include <wx/defs.h>
#include <wx/dcbuffer.h>
#include <wx/dcclient.h>
#include <wx/dcmemory.h>
#include <wx/graphics.h>
#include <wx/image.h>
#include <wx/msgdlg.h>
#include <wx/panel.h>
#include <wx/tooltip.h>
#include <wx/debug.h>
#include <wx/textctrl.h>
#include <wx/valtext.h>
#include <wx/dialog.h>
#include <wx/sizer.h>
#include <wx/button.h>
#include <wx/statline.h>
#include <wx/sizer.h>
#include <wx/settings.h>
#include <wx/popupwin.h>
#include <wx/window.h>

#include "../Experimental.h"
#include "ASlider.h"
#include "Ruler.h"

#include "../AColor.h"
#include "../ImageManipulation.h"
#include "../Project.h"
#include "../ShuttleGui.h"

#include "../Theme.h"
#include "../AllThemeResources.h"

#if defined __WXMSW__
const int sliderFontSize = 10;
#else
const int sliderFontSize = 12;
#endif

//
// TipPanel
//

class TipPanel final : public wxFrame
{
 public:
   TipPanel(wxWindow *parent, const wxString & label);
   virtual ~TipPanel() {}

   wxSize GetSize() const;
   void SetPos(const wxPoint & pos);
   void SetLabel(const wxString & label);

private:
   void OnPaint(wxPaintEvent & event);
#if defined(__WXGTK__)
   void OnCreate(wxWindowCreateEvent & event);
#endif

private:
   wxString mMaxLabel;
   wxString mLabel;
   int mWidth;
   int mHeight;

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(TipPanel, wxFrame)
   EVT_PAINT(TipPanel::OnPaint)
#if defined(__WXGTK__)
   EVT_WINDOW_CREATE(TipPanel::OnCreate)
#endif
END_EVENT_TABLE()

TipPanel::TipPanel(wxWindow *parent, const wxString & maxLabel)
:  wxFrame(parent, wxID_ANY, wxString{}, wxDefaultPosition, wxDefaultSize,
           wxFRAME_SHAPED | wxFRAME_FLOAT_ON_PARENT)
{
   SetBackgroundStyle(wxBG_STYLE_PAINT);

   mMaxLabel = maxLabel;
   wxFont labelFont(sliderFontSize, wxSWISS, wxNORMAL, wxNORMAL);
   GetTextExtent(mMaxLabel, &mWidth, &mHeight, NULL, NULL, &labelFont);

   mWidth += 8;
   mHeight += 8;

#if defined(__WXMSW__) || defined(__WXMAC__)
   wxGraphicsPath path = wxGraphicsRenderer::GetDefaultRenderer()->CreatePath();
   path.AddRoundedRectangle(0, 0, mWidth, mHeight, 5);
   SetShape(path);
#endif
}

wxSize TipPanel::GetSize() const
{
   return wxSize(mWidth, mHeight);
}

void TipPanel::SetPos(const wxPoint & pos)
{
   SetSize(pos.x, pos.y, mWidth, mHeight);
}

void TipPanel::SetLabel(const wxString & label)
{
   mLabel = label;
}

void TipPanel::OnPaint(wxPaintEvent & WXUNUSED(event))
{
   wxAutoBufferedPaintDC dc(this);

   dc.SetPen(*wxBLACK_PEN);
   dc.SetBrush(AColor::tooltipBrush);
   dc.DrawRoundedRectangle(0, 0, mWidth, mHeight, 5);

   dc.SetFont(wxFont(sliderFontSize, wxSWISS, wxNORMAL, wxNORMAL));
   dc.SetTextForeground(AColor::tooltipPen.GetColour());

   int textWidth, textHeight;
   dc.GetTextExtent(mLabel, &textWidth, &textHeight);
   dc.DrawText(mLabel, (mWidth - textWidth) / 2, (mHeight - textHeight) / 2);
}

#if defined(__WXGTK__)
void TipPanel::OnCreate(wxWindowCreateEvent & WXUNUSED(event))
{
   wxGraphicsPath path = wxGraphicsRenderer::GetDefaultRenderer()->CreatePath();
   path.AddRoundedRectangle(0, 0, mWidth, mHeight, 5);
   SetShape(path);
}
#endif

//
// SliderDialog
//

BEGIN_EVENT_TABLE(SliderDialog, wxDialogWrapper)
   EVT_SLIDER(wxID_ANY,SliderDialog::OnSlider)
END_EVENT_TABLE();

SliderDialog::SliderDialog(wxWindow * parent, wxWindowID id,
                           const wxString & title,
                           wxPoint position,
                           wxSize size,
                           int style,
                           float value,
                           float line,
                           float page):
   wxDialogWrapper(parent,id,title,position),
   mStyle(style)
{
   SetName(GetTitle());
   ShuttleGui S(this, eIsCreating);

   S.StartVerticalLay();
   {
      mTextCtrl = S.AddTextBox(wxEmptyString,
                               wxEmptyString,
                               15);
      mTextCtrl->SetValidator(wxTextValidator(wxFILTER_NUMERIC));

      mSlider = safenew ASlider(this,
                            wxID_ANY,
                            title,
                            wxDefaultPosition,
                            size,
                            style,
                            false);
      mSlider->SetScroll(line, page);
      S.AddWindow(mSlider, wxEXPAND);
   }
   S.EndVerticalLay();

   S.AddStandardButtons(eOkButton | eCancelButton);

   Fit();

   mSlider->Set(value);
}

SliderDialog::~SliderDialog()
{
}

bool SliderDialog::TransferDataToWindow()
{
   mTextCtrl->SetValue(wxString::Format(wxT("%g"), mSlider->Get(false)));
   mTextCtrl->SetSelection(-1, -1);

   return true;
}

bool SliderDialog::TransferDataFromWindow()
{
   double value;

   mTextCtrl->GetValue().ToDouble(&value);
   if (mStyle == DB_SLIDER)
      value = DB_TO_LINEAR(value);
   mSlider->Set(value);

   return true;
}

void SliderDialog::OnSlider(wxCommandEvent & event)
{
   TransferDataToWindow();

   event.Skip(false);
}

float SliderDialog::Get()
{
   return mSlider->Get(false);
}

//
// LWSlider
//

// Define the thumb outline
static const wxPoint2DDouble outer[] =
{
   wxPoint2DDouble(  2,  0 ),
   wxPoint2DDouble(  8,  0 ),
   wxPoint2DDouble( 10,  2 ),
   wxPoint2DDouble( 10,  8 ),
   wxPoint2DDouble(  5, 13 ),
   wxPoint2DDouble(  0,  8 ),
   wxPoint2DDouble(  0,  2 ),
   wxPoint2DDouble(  2,  0 )
};

// Define the left and top interior components when enabled
static const wxPoint2DDouble enabledLeftBegin[] =
{
   wxPoint2DDouble(  2,  1 ),
   wxPoint2DDouble(  1,  2 ),
   wxPoint2DDouble(  1,  8 ),
   wxPoint2DDouble(  4,  4 ),
   wxPoint2DDouble(  4,  7 )
};
static const wxPoint2DDouble enabledLeftEnd[] =
{
   wxPoint2DDouble(  8,  1 ),
   wxPoint2DDouble(  1,  8 ),
   wxPoint2DDouble(  5, 12 ),
   wxPoint2DDouble(  6,  4 ),
   wxPoint2DDouble(  6,  7 )
};

// Define right and bottom interior components when enabled
static const wxPoint2DDouble enabledRightBegin[] =
{
   wxPoint2DDouble(  9,  2 ),
   wxPoint2DDouble(  9,  8 ),
   wxPoint2DDouble(  4,  5 ),
   wxPoint2DDouble(  4,  8 ),
};
static const wxPoint2DDouble enabledRightEnd[] =
{
   wxPoint2DDouble(  9,  8 ),
   wxPoint2DDouble(  6, 11 ),
   wxPoint2DDouble(  6,  5 ),
   wxPoint2DDouble(  6,  8 )
};

// Define the interior stripes when disabled
static const wxPoint2DDouble disabledStripesBegin[] =
{
   wxPoint2DDouble(  3,  2 ),
   wxPoint2DDouble(  5,  2 ),
   wxPoint2DDouble(  7,  2 ),
   wxPoint2DDouble(  2,  3 ),
   wxPoint2DDouble(  2,  5 ),
   wxPoint2DDouble(  2,  7 ),
};
static const wxPoint2DDouble disabledStripesEnd[] =
{
   wxPoint2DDouble(  8,  7 ),
   wxPoint2DDouble(  8,  5 ),
   wxPoint2DDouble(  8,  3 ),
   wxPoint2DDouble(  7,  8 ),
   wxPoint2DDouble(  6,  9 ),
   wxPoint2DDouble(  5, 10 ),
};

// Define the right and bottom interior components when disabled
static const wxPoint2DDouble disabledRightBegin[] =
{
   wxPoint2DDouble(  9,  2 ),
   wxPoint2DDouble(  9,  8 ),
};
static const wxPoint2DDouble disabledRightEnd[] =
{
   wxPoint2DDouble(  9,  8 ),
   wxPoint2DDouble(  6, 11 ),
};

// Construct customizable slider
LWSlider::LWSlider(wxWindow * parent,
                     const wxString &name,
                     const wxPoint &pos,
                     const wxSize &size,
                     float minValue,
                     float maxValue,
                     float stepValue,
                     bool canUseShift,
                     int style,
                     bool heavyweight /* = false */,
                     bool popup /* = true */,
                     int orientation /* = wxHORIZONTAL */) // wxHORIZONTAL or wxVERTICAL. wxVERTICAL is currently only for DB_SLIDER.
{
   Init(parent, name, pos, size, minValue, maxValue,
        stepValue, canUseShift, style, heavyweight, popup, 1.0, orientation);
}

#ifdef EXPERIMENTAL_MIDI_OUT
void LWSlider::SetStyle(int style)
{
   mStyle = style;
   mSpeed = 1.0;
   switch(style)
   {
   case PAN_SLIDER:
      mMinValue = -1.0f;
      mMaxValue = +1.0f;
      mStepValue = 0.1f;
      mOrientation = wxHORIZONTAL; //v Vertical PAN_SLIDER currently not handled, forced to horizontal.
      mName = _("Pan");
      break;
   case DB_SLIDER:
      mMinValue = DB_MIN;
      if (mOrientation == wxHORIZONTAL)
         mMaxValue = DB_MAX;
      else
         mMaxValue = DB_MAX; // for MixerBoard //v Previously was 6dB for MixerBoard, but identical for now.
      mStepValue = 1.0f;
      mSpeed = 0.5;
      mName = _("Gain");
      break;
   case FRAC_SLIDER:
      mMinValue = FRAC_MIN;
      mMaxValue = FRAC_MAX;
      mStepValue = STEP_CONTINUOUS;
      break;
   case SPEED_SLIDER:
      mMinValue = SPEED_MIN;
      mMaxValue = SPEED_MAX;
      mStepValue = STEP_CONTINUOUS;
      break;
#ifdef EXPERIMENTAL_MIDI_OUT
   case VEL_SLIDER:
      mMinValue = VEL_MIN;
      mMaxValue = VEL_MAX;
      mStepValue = 1.0f;
      mSpeed = 0.5;
      mName = _("Velocity");
      break;
#endif
   default:
      mMinValue = FRAC_MIN;
      mMaxValue = FRAC_MAX;
      mStepValue = 0.0f;
      wxASSERT(false); // undefined style
   }
}
#endif

// Construct predefined slider
LWSlider::LWSlider(wxWindow *parent,
                   const wxString &name,
                   const wxPoint &pos,
                   const wxSize &size,
                   int style,
                   bool heavyweight /* = false */,
                   bool popup /* = true */,
                   int orientation /* = wxHORIZONTAL */) // wxHORIZONTAL or wxVERTICAL. wxVERTICAL is currently only for DB_SLIDER.
{
   wxString leftLabel, rightLabel;
#ifdef EXPERIMENTAL_MIDI_OUT
   mOrientation = orientation;
   mName = name;

   SetStyle(style);

   Init(parent, mName, pos, size, mMinValue, mMaxValue, mStepValue,
        true, style, heavyweight, popup, mSpeed, mOrientation);
#else
   float minValue, maxValue, stepValue;
   float speed = 1.0;

   switch(style)
   {
   case PAN_SLIDER:
      minValue = -1.0f;
      maxValue = +1.0f;
      stepValue = 0.1f;
      orientation = wxHORIZONTAL; //v Vertical PAN_SLIDER currently not handled, forced to horizontal.
      break;
   case DB_SLIDER:
      minValue = -36.0f;
      if (orientation == wxHORIZONTAL)
         maxValue = 36.0f;
      else
         maxValue = 36.0f; // for MixerBoard //v Previously was 6dB for MixerBoard, but identical for now.
      stepValue = 1.0f;
      speed = 0.5;
      break;
   case FRAC_SLIDER:
      minValue = 0.0f;
      maxValue = 1.0f;
      stepValue = STEP_CONTINUOUS;
      break;
   case SPEED_SLIDER:
      minValue = 0.01f;
      maxValue = 3.0f;
      stepValue = STEP_CONTINUOUS;
      break;

   default:
      minValue = 0.0f;
      maxValue = 1.0f;
      stepValue = 0.0f;
      wxASSERT(false); // undefined style
   }

   Init(parent, name, pos, size, minValue, maxValue, stepValue,
        true, style, heavyweight, popup, speed, orientation);

#endif
}

void LWSlider::Init(wxWindow * parent,
                    const wxString &name,
                    const wxPoint &pos,
                    const wxSize &size,
                    float minValue,
                    float maxValue,
                    float stepValue,
                    bool canUseShift,
                    int style,
                    bool heavyweight,
                    bool popup,
                    float speed,
                    int orientation /* = wxHORIZONTAL */) // wxHORIZONTAL or wxVERTICAL. wxVERTICAL is currently only for DB_SLIDER.
{
   mEnabled = true;
   mName = name;
   mStyle = style;
   mOrientation = orientation;
   mIsDragging = false;
   mParent = parent;
   mHW = heavyweight;
   mPopup = popup;
   mSpeed = speed;
   mID = wxID_ANY;
   mMinValue = minValue;
   mMaxValue = maxValue;
   mStepValue = stepValue;
   mCanUseShift = canUseShift;
   mCurrentValue = 0.0f;
   mDefaultValue = 0.0f;
   mDefaultShortcut = false;
   mBitmap = NULL;
   mThumbBitmap = NULL;
   mScrollLine = 1.0f;
   mScrollPage = 5.0f;
   mTipPanel = NULL;

   AdjustSize(size);

   Move(pos);
}

LWSlider::~LWSlider()
{
}

wxWindowID LWSlider::GetId()
{
   return mID;
}

void LWSlider::SetId(wxWindowID id)
{
   mID = id;
}

void LWSlider::SetDefaultValue(float value)
{
   SetDefaultShortcut(true);
   mDefaultValue = value;
}

void LWSlider::SetDefaultShortcut(bool value)
{
   mDefaultShortcut = value;
}

void LWSlider::GetScroll(float & line, float & page)
{
   line = mScrollLine;
   page = mScrollPage;
}

void LWSlider::SetScroll(float line, float page)
{
   mScrollLine = line;
   mScrollPage = page;
}

void LWSlider::Move(const wxPoint &newpos)
{
   mLeft = newpos.x;
   mTop = newpos.y;
}

void LWSlider::AdjustSize(const wxSize & sz)
{
   mWidth = sz.GetWidth();
   mHeight = sz.GetHeight();

   if( mBitmap ){
      mBitmap.reset();
   }
   mThumbWidth = 14;
   mThumbHeight = 14;

   if (mOrientation == wxHORIZONTAL)
   {
      mCenterY = mHeight - 9;
   }
   else
   {
      mCenterX = mWidth - 9;
   }

   if (mOrientation == wxHORIZONTAL)
   {
      mLeftX = mThumbWidth/2;
      mRightX = mWidth - mThumbWidth/2 - 1;
      mWidthX = mRightX - mLeftX;
   }
   else
   {
      mTopY = mThumbWidth/2;
      mBottomY = mHeight - mThumbWidth/2 - 1;
      mHeightY = mBottomY - mTopY;
   }

   Refresh();
}

void LWSlider::OnPaint(wxDC &dc)
{
   if (!mBitmap || !mThumbBitmap)
   {
      Draw(dc);
   }

   //thumbPos should be in pixels
   int thumbPos = ValueToPosition(mCurrentValue);
   int thumbOrtho; // position in axis orthogonal to mOrientation
   if (mOrientation == wxHORIZONTAL)
      thumbOrtho = mCenterY - (mThumbHeight/2);
   else
      thumbOrtho = mCenterX - (mThumbWidth/2);

#if !defined(__WXMAC__)
   if( mHW )
   {
      dc.Clear();
   }
#endif

   dc.DrawBitmap(*mBitmap, mLeft, mTop, true);
   if (mOrientation == wxHORIZONTAL)
   {
      dc.DrawBitmap(*mThumbBitmap, mLeft+thumbPos, mTop+thumbOrtho, true);
   }
   else
   {
      dc.DrawBitmap(*mThumbBitmap, mLeft+thumbOrtho, mTop+thumbPos, true);
   }

   if (mTipPanel)
   {
      mTipPanel->Update();
   }
}

void LWSlider::OnSize( wxSizeEvent & event )
{
   AdjustSize(event.GetSize());

   Refresh();
}

void LWSlider::Draw(wxDC & paintDC)
{
   // The color we'll use to create the mask
   wxColour transparentColour(255, 254, 255);

   // Set up the memory DC
   wxMemoryDC dc;

   // Create the bitmap
   mThumbBitmap = std::make_unique<wxBitmap>();
   mThumbBitmap->Create(mThumbWidth, mThumbHeight, paintDC);
   dc.SelectObject(*mThumbBitmap);

#if !defined(__WXMAC__)
   // Clear the background for our mask
   dc.SetBackground(transparentColour);
   dc.Clear();
#endif

   {
      // Create the graphics contexxt
      std::unique_ptr<wxGraphicsContext> gc{ wxGraphicsContext::Create(dc) };

      // For vertical, we use the same downward pointing thumb, but rotate and flip it
      if (mOrientation == wxVERTICAL)
      {
         gc->Translate(0, 3);
         gc->Scale(1, -1);
         gc->Rotate((-90 * M_PI) / 180);
      }
      else
      {
         gc->Translate(1.5, 0);
      }

      // Draw the thumb outline
      gc->SetBrush(wxBrush(mEnabled ? wxColour(192, 192, 216) : wxColour(238, 238, 238)));
      gc->SetPen(wxPen(mEnabled ? wxColour(0, 0, 0) : wxColour(119, 119, 119)));
      gc->DrawLines(WXSIZEOF(outer), outer);

      // The interior is based on whether the slider is enabled or not
      if (mEnabled)
      {
         // Draw the left and top interior components
         gc->SetPen(wxPen(wxColour(255, 255, 255)));
         gc->StrokeLines(WXSIZEOF(enabledLeftBegin), enabledLeftBegin, enabledLeftEnd);

         // Draw the right and bottom interior components
         gc->SetPen(wxPen(wxColour(141, 141, 178)));
         gc->StrokeLines(WXSIZEOF(enabledRightBegin), enabledRightBegin, enabledRightEnd);
      }
      else
      {
         // Draw the interior stripes
         gc->SetPen(wxPen(wxColour(200, 200, 200)));
         gc->StrokeLines(WXSIZEOF(disabledStripesBegin), disabledStripesBegin, disabledStripesEnd);

         // Draw the right and bottom interior components
         gc->SetPen(wxPen(wxColour(153, 153, 153)));
         gc->StrokeLines(WXSIZEOF(disabledRightBegin), disabledRightBegin, disabledRightEnd);
      }
   }
   dc.SelectObject(wxNullBitmap);

#if !defined(__WXMAC__)
   // Now create and set the mask
   // SetMask takes ownership
   mThumbBitmap->SetMask(safenew wxMask(*mThumbBitmap, transparentColour));
#endif

   //
   // Now the background bitmap
   //

   mBitmap = std::make_unique<wxBitmap>();
   mBitmap->Create(mWidth, mHeight, paintDC);
   dc.SelectObject(*mBitmap);

   // DO-THEME Mask colour!!  JC-Aug-2007
   // Needed with experimental theming!
   //  ... because windows blends against this colour.
#ifdef EXPERIMENTAL_THEMING
   transparentColour = theTheme.Colour(clrTrackInfo);
#else
   transparentColour = wxColour(255, 254, 255);
#endif

#if !defined(__WXMAC__)
   dc.SetBackground(transparentColour);
   dc.Clear();
#endif

   // Draw the line along which the thumb moves.
   AColor::Dark(&dc, false);

   if (mOrientation == wxHORIZONTAL)
   {
      AColor::Line(dc, mLeftX, mCenterY+1, mRightX+2, mCenterY+1);
   }
   else //v if (mStyle != DB_SLIDER) // Let the ruler do it for vertical DB_SLIDER.
   {
      AColor::Line(dc, mCenterX+1, mTopY, mCenterX+1, mBottomY+2);
   }

   // Draw +/- or L/R first.  We need to draw these before the tick marks.
   if (mStyle == PAN_SLIDER)
   {
      //v Vertical PAN_SLIDER currently not handled, forced to horizontal.

      // sliderFontSize is for the tooltip.
      // we need something smaller here...
      wxFont labelFont(sliderFontSize-3, wxSWISS, wxNORMAL, wxNORMAL);
      dc.SetFont(labelFont);

      // Colors
#ifdef EXPERIMENTAL_THEMING
      dc.SetTextForeground( theTheme.Colour( clrTrackPanelText ));

      // TransparentColour should be same as clrTrackInfo.
      dc.SetTextBackground( theTheme.Colour( clrTrackInfo ) );
      dc.SetBackground( theTheme.Colour( clrTrackInfo ) );
      // HAVE to use solid and not transparent here,
      // otherwise windows will do it's clever font optimisation trick,
      // but against a default colour of white, which is not OK on a dark
      // background.
      dc.SetBackgroundMode( wxSOLID );
#else
      dc.SetTextForeground(mEnabled ? wxColour(0, 0, 0) : wxColour(128, 128, 128));
      dc.SetTextBackground(wxColour(255,255,255));
#endif

      /* i18n-hint: One-letter abbreviation for Left, in the Pan slider */
      dc.DrawText(_("L"), mLeftX, 0);

      /* i18n-hint: One-letter abbreviation for Right, in the Pan slider */
      dc.DrawText(_("R"), mRightX-6,0);
   }
   else
   {
      // draw the '-' and the '+'
#ifdef EXPERIMENTAL_THEMING
      wxPen pen( theTheme.Colour( clrTrackPanelText ));
      dc.SetPen( pen );
#else
      dc.SetPen(mEnabled ? *wxBLACK : wxColour(128, 128, 128));
#endif

      if (mOrientation == wxHORIZONTAL)
      {
         AColor::Line(dc, mLeftX, mCenterY-10, mLeftX+4, mCenterY-10);
         AColor::Line(dc, mRightX-5, mCenterY-10, mRightX-1, mCenterY-10);
         AColor::Line(dc, mRightX-3, mCenterY-12, mRightX-3, mCenterY-8);
      }
      else
      {
         // Vertical DB_SLIDER is for gain slider in MixerBoard.
         // We use a Ruler instead of marks & ticks.
         // Draw '+' and '-' only for other vertical sliders.
         if (mStyle != DB_SLIDER)
         {
            AColor::Line(dc, mCenterX-12, mBottomY-3,  mCenterX-8, mBottomY-3);
            AColor::Line(dc, mCenterX-12, mTopY+3,     mCenterX-8, mTopY+3);
            AColor::Line(dc, mCenterX-10, mTopY,       mCenterX-10, mTopY+5);
         }
      }
   }

   //v 20090820: Ruler doesn't align with slider correctly -- yet.
   //if ((mOrientation == wxVERTICAL) && (mStyle == DB_SLIDER))
   //{
   //   if (!mpRuler)
   //   {
   //      mpRuler = std::make_unique<Ruler>();
   //      mpRuler->mbTicksOnly = false;
   //      mpRuler->mbTicksAtExtremes = true;

   //      #ifdef __WXMSW__
   //         const int kFontSize = 8;
   //      #else
   //         const int kFontSize = 10;
   //      #endif
   //      wxFont rulerFont(kFontSize, wxSWISS, wxNORMAL, wxNORMAL);
   //      mpRuler->SetFonts(rulerFont, rulerFont, rulerFont);

   //      mpRuler->SetFlip(false);
   //      mpRuler->SetLabelEdges(true);

   //      mpRuler->SetOrientation(wxVERTICAL);
   //      mpRuler->SetRange(mMaxValue, mMinValue);
   //      mpRuler->SetFormat(Ruler::LinearDBFormat);
   //   }
   //   mpRuler->SetBounds(mLeft, mTop,  mWidth, mHeightY); //v Why the magic number reqd on height to get it to line up?    + 9);
   //   mpRuler->Draw(*dc);
   //}
   //else
   {
      // tick marks
      int divs = 10;
      double upp;
      if (mOrientation == wxHORIZONTAL)
      {
         upp = divs / (double)(mWidthX-1);
      }
      else
      {
         if (mStyle == DB_SLIDER)
            divs = mMaxValue - mMinValue + 1;
         upp = divs / (double)(mHeightY-1);
      }
      double d = 0.0;
      int int_d = -1;
      const int kMax = (mOrientation == wxHORIZONTAL) ? mWidthX : mHeightY;
      for(int p = 0; p <= kMax; p++)
      {
         if (((int)d) > int_d)
         {
            int_d = (int)d;
            int tickLength = ((int_d == 0) || (int_d == divs)) ? 5: 3; // longer ticks at extremes
            AColor::Light(&dc, false);
            if (mOrientation == wxHORIZONTAL)
            {
               AColor::Line(dc, mLeftX+p, mCenterY-tickLength, mLeftX+p, mCenterY-1); // ticks above
            }
            else
            {
               AColor::Line(dc, mCenterX-tickLength, mTopY+p, mCenterX-1, mTopY+p); // ticks at left
            }

            AColor::Dark(&dc, false);

            if (mOrientation == wxHORIZONTAL)
            {
               AColor::Line(dc, mLeftX+p+1, mCenterY-tickLength+1, mLeftX+p+1, mCenterY-1); // ticks above
            }
            else
            {
               AColor::Line(dc, mCenterX-tickLength+1, mTopY+p+1, mCenterX-1, mTopY+p+1); // ticks at left
            }
         }
         d += upp;
      }
   }

   dc.SelectObject(wxNullBitmap);

#if !defined(__WXMAC__)
   // SetMask takes ownership
   mBitmap->SetMask(safenew wxMask(*mBitmap, transparentColour));
#endif
}

void LWSlider::SetToolTipTemplate(const wxString & tip)
{
   mTipTemplate = tip;
}

void LWSlider::ShowTip(bool show)
{
   if (show)
   {
      if (mTipPanel)
      {
         if (mTipPanel->IsShownOnScreen())
         {
            return;
         }

         mTipPanel.reset();
      }

      CreatePopWin();
      FormatPopWin();
      SetPopWinPosition();
      mTipPanel->ShowWithoutActivating();
   }
   else
   {
      if (mTipPanel)
      {
         mTipPanel->Hide();
         mTipPanel.reset();
      }
   }
}

void LWSlider::CreatePopWin()
{
   mTipPanel = std::make_unique<TipPanel>(mParent, GetMaxTip());
}

void LWSlider::SetPopWinPosition()
{
   if (mTipPanel)
   {
      wxSize sz = mTipPanel->GetSize();
      wxPoint pt;

      if (mOrientation == wxHORIZONTAL)
      {
         pt.x = mLeft + ((mWidth - sz.x) / 2);
         pt.y = mTop + mHeight + 1;
      }
      else
      {
         pt.x = mLeft + mWidth + 1;
         pt.y = mTop + ((mHeight - sz.y) / 2);
      }

      mTipPanel->SetPos(mParent->ClientToScreen(pt));
   }
}

void LWSlider::FormatPopWin()
{
   if (!mTipPanel)
   {
      return;
   }

   mTipPanel->SetLabel(GetTip(mCurrentValue));
   mTipPanel->Refresh();
}

wxString LWSlider::GetTip(float value) const
{
   wxString label;

   if (mTipTemplate.IsEmpty())
   {
      wxString val;

      switch(mStyle)
      {
      case FRAC_SLIDER:
         val.Printf(wxT("%.2f"), value);
         break;
   
      case DB_SLIDER:
         val.Printf(wxT("%+.1f dB"), value);
         if (val.Right(1) == wxT("0"))
         {
            val.Left(val.Length() - 2);
         }
         break;

      case PAN_SLIDER:
         if (value == 0.0)
         {
            val = _("Center");
         }
         else
         {
            val.Printf(wxT("%.0f%% %s"),
               value * (value < 0.0 ? -100.0f : 100.0f),
               value < 0.0 ? _("Left") : _("Right"));
         }
         break;

      case SPEED_SLIDER:
         val.Printf(wxT("%.2fx"), value);
         break;

#ifdef EXPERIMENTAL_MIDI_OUT
      case VEL_SLIDER:
          val.Printf(wxT("%s%d"),
                     (value > 0.0f ? _("+") : wxT("")),
                     (int) value);
          break;
#endif
      }

      label.Printf(wxT("%s: %s"), mName.c_str(), val.c_str());
   }
   else
   {
      label.Printf(mTipTemplate, value);
   }

   return label;
}

wxString LWSlider::GetMaxTip() const
{
   wxString label;

   if (mTipTemplate.IsEmpty())
   {
      wxString val;

      switch(mStyle)
      {
      case FRAC_SLIDER:
         val.Printf(wxT("%d.99"), (int) (mMinValue - mMaxValue));
         break;

      case DB_SLIDER:
         val = wxT("-99.999 dB");
         break;

      case PAN_SLIDER:
         val = wxT("-100% Right");
         break;

      case SPEED_SLIDER:
         val = wxT("9.99x");
         break;

#ifdef EXPERIMENTAL_MIDI_OUT
      case VEL_SLIDER:
          val = wxT("+127");
          break;
#endif
      }

      label.Printf(wxT("%s: %s"), mName.c_str(), val.c_str());
   }
   else
   {
      label.Printf(mTipTemplate, floor(mMaxValue - mMinValue) + 0.999);
   }

   return label;
}

bool LWSlider::ShowDialog()
{
   return DoShowDialog( mParent->ClientToScreen(wxPoint( mLeft, mTop ) ) );
}

bool LWSlider::ShowDialog(wxPoint pos)
{
   return DoShowDialog( pos );
}

bool LWSlider::DoShowDialog(wxPoint pos)
{
   float value;
   bool changed = false;

   SliderDialog dlg( NULL,
                     wxID_ANY,
                     mName,
                     pos,
                     wxSize( mWidth, mHeight ),
                     mStyle,
                     Get(),
                     mScrollLine,
                     mScrollPage);
   if (pos == wxPoint(-1, -1)) {
      dlg.Center();
   }

   if( dlg.ShowModal() == wxID_OK )
   {
      value = dlg.Get();
      if( value != mCurrentValue )
      {
         mCurrentValue = value;
         changed = true;
      }
   }

   return changed;
}

void LWSlider::OnMouseEvent(wxMouseEvent & event)
{
   if (event.Entering())
   {
      // Display the tooltip in the status bar
      wxString tip = GetTip(mCurrentValue);
      GetActiveProject()->TP_DisplayStatusMessage(tip);
      Refresh();
   }
   else if (event.Leaving())
   {
      if (!mIsDragging)
      {
         ShowTip(false);
      }
      GetActiveProject()->TP_DisplayStatusMessage(wxT(""));
      Refresh();
   }

   // Events other than mouse-overs are ignored when we are disabled
   if (!mEnabled)
      return;

   // Windows sends a right button mouse event when you press the context menu
   // key, so ignore it.
   if ((event.RightDown() && !event.RightIsDown()) ||
       (event.RightUp() && event.GetPosition() == wxPoint(-1, -1)))
   {
      event.Skip(false);
      return;
   }

   float prevValue = mCurrentValue;

   // Figure out the thumb position
   wxRect r;
   if (mOrientation == wxHORIZONTAL)
   {
      r.x = mLeft + ValueToPosition(mCurrentValue);
      r.y = mTop + (mCenterY - (mThumbHeight / 2));
   }
   else
   {
      r.x = mLeft + (mCenterX - (mThumbWidth / 2));
      r.y = mTop + ValueToPosition(mCurrentValue);
   }
   r.width = mThumbWidth;
   r.height = mThumbHeight;

   wxRect tolerantThumbRect = r;
   tolerantThumbRect.Inflate(3, 3);

   // Should probably use a right click instead/also
   if( event.ButtonDClick() && mPopup )
   {
      //On a double-click, we should pop up a dialog.
      DoShowDialog(mParent->ClientToScreen(wxPoint(event.m_x,event.m_y)));
   }
   else if( event.ButtonDown() )
   {
      if( mDefaultShortcut && event.ControlDown() )
      {
         mCurrentValue = mDefaultValue;
      }

      if( event.RightDown() ) {
         mParent->SetFocus();
      }

      // Thumb clicked?
      //
      // Do not change position until first drag.  This helps
      // with unintended value changes.
      if( tolerantThumbRect.Contains( event.GetPosition() ) )
      {
         // Remember mouse position and current value
         mClickPos = (mOrientation == wxHORIZONTAL) ? event.m_x : event.m_y;
         mClickValue = mCurrentValue;

         mIsDragging = true;
      }
      // Clicked to set location?
      else
      {
         mCurrentValue =
            ClickPositionToValue(
               (mOrientation == wxHORIZONTAL) ? event.m_x : event.m_y,
               event.ShiftDown());
      }

      if (!mParent->HasCapture()) {
         mParent->CaptureMouse();
      }

      ShowTip(true);
   }
   else if( event.ButtonUp() )
   {
      mIsDragging = false;
      if (mParent->HasCapture())
         mParent->ReleaseMouse();

      ShowTip(false);
   }
   else if (event.Dragging() && mIsDragging)
   {
      if (mOrientation == wxHORIZONTAL)
      {
         if (event.m_y < (r.y - 2 * r.height) ||
             event.m_y > (r.y + 3 * r.height)) {
            // If the mouse y coordinate is relatively far from the slider,
            // snap back to the original position
            mCurrentValue = mClickValue;
         }
         else {
            // Otherwise, move the slider to the right position based
            // on the mouse position
            mCurrentValue = DragPositionToValue(event.m_x, event.ShiftDown());
         }
      }
      else // (mOrientation == wxVERTICAL)
      {
         if (event.m_x < (r.x - 2 * r.width) ||
             event.m_x > (r.x + 3 * r.width)) {
            // If the mouse x coordinate is relatively far from the slider,
            // snap back to the original position
            mCurrentValue = mClickValue;
         }
         else {
            // Otherwise, move the slider to the right position based
            // on the mouse position
            mCurrentValue = DragPositionToValue(event.m_y, event.ShiftDown());
         }
      }
   }
   else if( event.m_wheelRotation != 0 )
   {
      //Calculate the number of steps in a given direction this event
      //represents (allows for two or more clicks on a single event.)
      double steps =  event.m_wheelRotation /
         (event.m_wheelDelta > 0 ? (double)event.m_wheelDelta : 120.0);

      if( steps < 0.0 )
      {
         Decrease( (float)-steps );
      }
      else
      {
         Increase( (float)steps );
      }
      SendUpdate( mCurrentValue );
   }

   if( prevValue != mCurrentValue )
      SendUpdate( mCurrentValue );
}

void LWSlider::OnKeyEvent(wxKeyEvent & event)
{
   if (mEnabled)
   {
      switch( event.GetKeyCode() )
      {
         case WXK_TAB:
            mParent->Navigate(event.ShiftDown()
                              ? wxNavigationKeyEvent::IsBackward
                              : wxNavigationKeyEvent::IsForward);
            break;

         case WXK_RIGHT:
         case WXK_UP:
            Increase( mScrollLine );
            SendUpdate( mCurrentValue );
            break;

         case WXK_LEFT:
         case WXK_DOWN:
            Decrease( mScrollLine );
            SendUpdate( mCurrentValue );
            break;

         case WXK_PAGEUP:
            Increase( mScrollPage );
            SendUpdate( mCurrentValue );
            break;

         case WXK_PAGEDOWN:
            Decrease( mScrollPage );
            SendUpdate( mCurrentValue );
            break;

         case WXK_HOME:
            SendUpdate( mMinValue );
            break;

         case WXK_END:
            SendUpdate( mMaxValue );
            break;

         case WXK_RETURN:
         case WXK_NUMPAD_ENTER:
            {
               wxTopLevelWindow *tlw = wxDynamicCast(wxGetTopLevelParent(mParent), wxTopLevelWindow);
               wxWindow *def = tlw->GetDefaultItem();
               if (def && def->IsEnabled()) {
                  wxCommandEvent cevent(wxEVT_COMMAND_BUTTON_CLICKED,
                        def->GetId());
                  mParent->GetEventHandler()->ProcessEvent(cevent);
               }
            }

         default:
            // Allow it to propagate
            event.Skip();
            break;
      }
   }
   else
   {
      event.Skip();
   }
}

void LWSlider::SendUpdate( float newValue )
{
   mCurrentValue = newValue;

   FormatPopWin();

   Refresh();

   wxCommandEvent e( wxEVT_COMMAND_SLIDER_UPDATED, mID );
   int intValue = (int)( ( mCurrentValue - mMinValue ) * 1000.0f /
                         ( mMaxValue - mMinValue ) );
   e.SetInt( intValue );
   mParent->GetEventHandler()->ProcessEvent(e);
}

int LWSlider::ValueToPosition(float val)
{
   float fRange = mMaxValue - mMinValue;
   if (mOrientation == wxHORIZONTAL)
      return (int)rint((val - mMinValue) * mWidthX / fRange);
   else
      // low values at bottom
      return (int)rint((mMaxValue - val) * mHeightY / fRange);
}

void LWSlider::SetSpeed(float speed)
{
   mSpeed = speed;
}

// Given the mouse slider coordinate in fromPos, compute the NEW value
// of the slider when clicking to set a NEW position.
float LWSlider::ClickPositionToValue(int fromPos, bool shiftDown)
{
   int nSpan;
   int pos;
   if (mOrientation == wxHORIZONTAL)
   {
      pos = (fromPos - mLeft - (mThumbWidth / 2));
      nSpan = mWidthX;
   }
   else
   {
      // wxVERTICAL => Low values at bottom.
      pos = mBottomY - fromPos;
      nSpan = mHeightY;
   }

   // MM: Special cases: If position is at the very left or the
   // very right (or top/bottom for wxVERTICAL), set minimum/maximum value without other checks
   if (pos <= 0)
      return mMinValue;
   if (pos >= nSpan)
      return mMaxValue;

   float val = (pos / (float)nSpan)
      * (mMaxValue - mMinValue) + mMinValue;

   if (val < mMinValue)
      val = mMinValue;
   if (val > mMaxValue)
      val = mMaxValue;

   if (!(mCanUseShift && shiftDown) && mStepValue != STEP_CONTINUOUS)
   {
      // MM: If shift is not down, or we don't allow usage
      // of shift key at all, trim value to steps of
      // provided size.
      val = (int)(val / mStepValue + 0.5 * (val>0?1.0f:-1.0f)) * mStepValue;
   }

   return val;
}

// Given the mouse slider coordinate in fromPos, compute the NEW value
// of the slider during a drag.
float LWSlider::DragPositionToValue(int fromPos, bool shiftDown)
{
   int delta = (fromPos - mClickPos);

   float speed = mSpeed;
   // Precision enhancement for Shift drags
   if (mCanUseShift && shiftDown)
      speed *= 0.4f;

   // wxVERTICAL => Low values at bottom, so throw in the minus sign here with -mHeightY.
   float denominator = (mOrientation == wxHORIZONTAL) ? mWidthX : -mHeightY;
   float val = mClickValue +
      speed * (delta / denominator) * (mMaxValue - mMinValue);

   if (val < mMinValue)
      val = mMinValue;
   if (val > mMaxValue)
      val = mMaxValue;

   if (!(mCanUseShift && shiftDown) && mStepValue != STEP_CONTINUOUS)
   {
      // MM: If shift is not down, or we don't allow usage
      // of shift key at all, and the slider has not continuous values,
      // trim value to steps of provided size.
      val = (int)(val / mStepValue + 0.5 * (val>0?1.0f:-1.0f)) * mStepValue;
   }

   return val;
}

float LWSlider::Get( bool convert )
{
   if (mStyle == DB_SLIDER)
      return (convert ? DB_TO_LINEAR(mCurrentValue) : mCurrentValue);
   else
      return mCurrentValue;
}

void LWSlider::Set(float value)
{
   if (mIsDragging)
      return;
   if (mStyle == DB_SLIDER)
      mCurrentValue = LINEAR_TO_DB(value);
   else
      mCurrentValue = value;

   if (mCurrentValue < mMinValue)
      mCurrentValue = mMinValue;
   if (mCurrentValue > mMaxValue)
      mCurrentValue = mMaxValue;

   Refresh();
}

void LWSlider::Increase(float steps)
{
   float stepValue = mStepValue;

   if ( stepValue == 0.0 )
   {
      stepValue = ( mMaxValue - mMinValue ) / 10.0;
   }

   mCurrentValue += ( steps * stepValue );

   if ( mCurrentValue < mMinValue )
   {
      mCurrentValue = mMinValue;
   }
   else if ( mCurrentValue > mMaxValue )
   {
      mCurrentValue = mMaxValue;
   }

   Refresh();
}

void LWSlider::Decrease(float steps)
{
   float stepValue = mStepValue;

   if ( stepValue == 0.0 )
   {
      stepValue = ( mMaxValue - mMinValue ) / 10.0;
   }

   mCurrentValue -= ( steps * stepValue );

   if ( mCurrentValue < mMinValue )
   {
      mCurrentValue = mMinValue;
   }
   else if ( mCurrentValue > mMaxValue )
   {
      mCurrentValue = mMaxValue;
   }

   Refresh();
}

void LWSlider::Refresh()
{
   if (mHW)
      mParent->Refresh(false);
}

bool LWSlider::GetEnabled()
{
   return mEnabled;
}

void LWSlider::SetEnabled(bool enabled)
{
   mEnabled = enabled;

   mThumbBitmap.reset();

   Refresh();
}

//
// ASlider
//

BEGIN_EVENT_TABLE(ASlider, wxWindow)
   EVT_CHAR(ASlider::OnKeyEvent)
   EVT_MOUSE_EVENTS(ASlider::OnMouseEvent)
   EVT_MOUSE_CAPTURE_LOST(ASlider::OnCaptureLost)
   EVT_PAINT(ASlider::OnPaint)
   EVT_SIZE(ASlider::OnSize)
   EVT_ERASE_BACKGROUND(ASlider::OnErase)
   EVT_SLIDER(wxID_ANY, ASlider::OnSlider)
   EVT_SET_FOCUS(ASlider::OnSetFocus)
   EVT_KILL_FOCUS(ASlider::OnKillFocus)
   EVT_TIMER(wxID_ANY, ASlider::OnTimer)
END_EVENT_TABLE()

ASlider::ASlider( wxWindow * parent,
                  wxWindowID id,
                  const wxString &name,
                  const wxPoint & pos,
                  const wxSize & size,
                  int style,
                  bool popup,
                  bool canUseShift,
                  float stepValue,
                  int orientation /*= wxHORIZONTAL*/)
: wxPanel( parent, id, pos, size, wxWANTS_CHARS )
{
   mLWSlider = std::make_unique<LWSlider>( this,
                             name,
                             wxPoint(0,0),
                             size,
                             style,
                             canUseShift,
                             popup,
                             orientation);
   mLWSlider->mStepValue = stepValue;
   mLWSlider->SetId( id );
   SetName( name );

   mSliderIsFocused = false;

   mStyle = style;

   mTimer.SetOwner(this);

#if wxUSE_ACCESSIBILITY
   SetAccessible( safenew ASliderAx( this ) );
#endif
}


ASlider::~ASlider()
{
   if(HasCapture())
      ReleaseMouse();
}

void ASlider::OnSlider(wxCommandEvent &event)
{

   if ( event.GetId() == mLWSlider->GetId() )
   {
#if wxUSE_ACCESSIBILITY
      GetAccessible()->NotifyEvent( wxACC_EVENT_OBJECT_VALUECHANGE,
                                    this,
                                    wxOBJID_CLIENT,
                                    wxACC_SELF );
#endif
   }

   event.Skip();
}

void ASlider::OnSize(wxSizeEvent &event)
{
   mLWSlider->OnSize( event );
}

void ASlider::OnErase(wxEraseEvent & WXUNUSED(event))
{
   // Ignore it to prevent flashing
}

void ASlider::OnPaint(wxPaintEvent & WXUNUSED(event))
{
   wxPaintDC dc(this);

   mLWSlider->OnPaint(dc);

   if ( mSliderIsFocused )
   {
      wxRect r( 0, 0, mLWSlider->mWidth, mLWSlider->mHeight );

      r.Deflate( 1, 1 );

      AColor::DrawFocus( dc, r );
   }
}

void ASlider::OnMouseEvent(wxMouseEvent &event)
{
   if (event.Entering())
   {
      mTimer.StartOnce(1000);
   }
   else if (event.Leaving())
   {
      mTimer.Stop();
   }

   mLWSlider->OnMouseEvent(event);
}

void ASlider::OnCaptureLost(wxMouseCaptureLostEvent & WXUNUSED(event))
{
   wxMouseEvent e(wxEVT_LEFT_UP);
   mLWSlider->OnMouseEvent(e);
}

void ASlider::OnKeyEvent(wxKeyEvent &event)
{
   mLWSlider->OnKeyEvent(event);
}

void ASlider::OnSetFocus(wxFocusEvent & WXUNUSED(event))
{
   mSliderIsFocused = true;
   Refresh();
}

void ASlider::OnKillFocus(wxFocusEvent & WXUNUSED(event))
{
   mSliderIsFocused = false;
   Refresh();
}

void ASlider::OnTimer(wxTimerEvent & WXUNUSED(event))
{
   mLWSlider->ShowTip(true);
}

void ASlider::GetScroll(float & line, float & page)
{
   mLWSlider->GetScroll(line, page);
}

void ASlider::SetScroll(float line, float page)
{
   mLWSlider->SetScroll(line, page);
}

void ASlider::SetToolTipTemplate(const wxString & tip)
{
   mLWSlider->SetToolTipTemplate(tip);
}

float ASlider::Get( bool convert )
{
   return mLWSlider->Get( convert );
}

void ASlider::Set(float value)
{
   mLWSlider->Set(value);
}

#ifdef EXPERIMENTAL_MIDI_OUT
void ASlider::SetStyle(int style)
{
   mStyle = style;
   mLWSlider->SetStyle(style);
}
#endif

void ASlider::Increase(float steps)
{
   mLWSlider->Increase(steps);
}

void ASlider::Decrease(float steps)
{
   mLWSlider->Decrease(steps);
}

bool ASlider::ShowDialog(wxPoint pos)
{
   return mLWSlider->ShowDialog(pos);
}

void ASlider::SetSpeed(float speed)
{
   mLWSlider->SetSpeed(speed);
}

bool ASlider::Enable(bool enable)
{
   if (mLWSlider->GetEnabled() == enable)
      return false;

   mLWSlider->SetEnabled(enable);

   wxWindow::Enable(enable);

   return true;
}

bool ASlider::IsEnabled() const
{
   return mLWSlider->GetEnabled();
}

bool ASlider::s_AcceptsFocus{ false };

auto ASlider::TemporarilyAllowFocus() -> TempAllowFocus {
   s_AcceptsFocus = true;
   return TempAllowFocus{ &s_AcceptsFocus };
}

// This compensates for a but in wxWidgets 3.0.2 for mac:
// Couldn't set focus from keyboard when AcceptsFocus returns false;
// this bypasses that limitation
void ASlider::SetFocusFromKbd()
{
   auto temp = TemporarilyAllowFocus();
   SetFocus();
}

#if wxUSE_ACCESSIBILITY

ASliderAx::ASliderAx( wxWindow * window ) :
   wxWindowAccessible( window )
{
}

ASliderAx::~ASliderAx()
{
}

// Retrieves the address of an IDispatch interface for the specified child.
// All objects must support this property.
wxAccStatus ASliderAx::GetChild( int childId, wxAccessible** child )
{
   if ( childId == wxACC_SELF )
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
wxAccStatus ASliderAx::GetChildCount(int* childCount)
{
   *childCount = 3;

   return wxACC_OK;
}

// Gets the default action for this object (0) or > 0 (the action for a child).
// Return wxACC_OK even if there is no action. actionName is the action, or the empty
// string if there is no action.
// The retrieved string describes the action that is performed on an object,
// not what the object does as a result. For example, a toolbar button that prints
// a document has a default action of "Press" rather than "Prints the current document."
wxAccStatus ASliderAx::GetDefaultAction( int WXUNUSED(childId), wxString *actionName )
{
   actionName->Clear();

   return wxACC_OK;
}

// Returns the description for this object or a child.
wxAccStatus ASliderAx::GetDescription( int WXUNUSED(childId), wxString *description )
{
   description->Clear();

   return wxACC_OK;
}

// Gets the window with the keyboard focus.
// If childId is 0 and child is NULL, no object in
// this subhierarchy has the focus.
// If this object has the focus, child should be 'this'.
wxAccStatus ASliderAx::GetFocus(int* childId, wxAccessible** child)
{
   *childId = 0;
   *child = this;

   return wxACC_OK;
}

// Returns help text for this object or a child, similar to tooltip text.
wxAccStatus ASliderAx::GetHelpText( int WXUNUSED(childId), wxString *helpText )
{
   helpText->Clear();

   return wxACC_OK;
}

// Returns the keyboard shortcut for this object or child.
// Return e.g. ALT+K
wxAccStatus ASliderAx::GetKeyboardShortcut( int WXUNUSED(childId), wxString *shortcut )
{
   shortcut->Clear();

   return wxACC_OK;
}

// Returns the rectangle for this object (id = 0) or a child element (id > 0).
// rect is in screen coordinates.
wxAccStatus ASliderAx::GetLocation( wxRect& rect, int WXUNUSED(elementId) )
{
   ASlider *as = wxDynamicCast( GetWindow(), ASlider );

   rect = as->GetRect();
   rect.SetPosition( as->GetParent()->ClientToScreen( rect.GetPosition() ) );

   return wxACC_OK;
}

// Gets the name of the specified object.
wxAccStatus ASliderAx::GetName(int WXUNUSED(childId), wxString* name)
{
   ASlider *as = wxDynamicCast( GetWindow(), ASlider );

   *name = as->GetName();

   return wxACC_OK;
}

// Returns a role constant.
wxAccStatus ASliderAx::GetRole(int childId, wxAccRole* role)
{
   switch( childId )
   {
      case 0:
         *role = wxROLE_SYSTEM_SLIDER;
      break;

      case 1:
      case 3:
         *role = wxROLE_SYSTEM_PUSHBUTTON;
      break;

      case 2:
         *role = wxROLE_SYSTEM_INDICATOR;
      break;
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
wxAccStatus ASliderAx::GetSelections( wxVariant * WXUNUSED(selections) )
{
   return wxACC_NOT_IMPLEMENTED;
}

// Returns a state constant.
wxAccStatus ASliderAx::GetState(int childId, long* state)
{
   ASlider *as = wxDynamicCast( GetWindow(), ASlider );

   switch( childId )
   {
      case 0:
         *state = wxACC_STATE_SYSTEM_FOCUSABLE;
      break;

      case 1:
         if ( as->mLWSlider->mCurrentValue == as->mLWSlider->mMinValue )
         {
            *state = wxACC_STATE_SYSTEM_INVISIBLE;
         }
      break;

      case 3:
         if ( as->mLWSlider->mCurrentValue == as->mLWSlider->mMaxValue )
         {
            *state = wxACC_STATE_SYSTEM_INVISIBLE;
         }
      break;
   }

   // Do not use mSliderIsFocused is not set until after this method
   // is called.
   *state |= ( as == wxWindow::FindFocus() ? wxACC_STATE_SYSTEM_FOCUSED : 0 );

   return wxACC_OK;
}

// Returns a localized string representing the value for the object
// or child.
wxAccStatus ASliderAx::GetValue(int childId, wxString* strValue)
{
   ASlider *as = wxDynamicCast( GetWindow(), ASlider );

   if ( childId == 0 )
   {
      switch( as->mLWSlider->mStyle )
      {
         case FRAC_SLIDER:
            strValue->Printf( wxT("%.0f"), as->mLWSlider->mCurrentValue * 100 );
            break;

         case DB_SLIDER:
            strValue->Printf( wxT("%.0f"), as->mLWSlider->mCurrentValue );
            break;

         case PAN_SLIDER:
            strValue->Printf( wxT("%.0f"), as->mLWSlider->mCurrentValue * 100 );
            break;

         case SPEED_SLIDER:
            strValue->Printf( wxT("%.0f"), as->mLWSlider->mCurrentValue * 100 );
            break;
#ifdef EXPERIMENTAL_MIDI_OUT
         case VEL_SLIDER:
            strValue->Printf( wxT("%.0f"), as->mLWSlider->mCurrentValue);
            break;
#endif

      }

      return wxACC_OK;
   }

   return wxACC_NOT_SUPPORTED;
}

#endif
