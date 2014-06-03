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
\brief A wxPanel or a wxPopupWindow used to give the numerical value
of an LWSlider or ASlider.

*//*******************************************************************/

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#ifndef WX_PRECOMP
// Include your minimal set of headers here, or wx.h
#include <wx/window.h>
#endif


#include "../Audacity.h"

#include <math.h>

#include <wx/defs.h>
#include <wx/dcclient.h>
#include <wx/dcmemory.h>
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

#ifndef __WXMAC__
#define USE_POPUPWIN 1
#endif

#if USE_POPUPWIN
#include <wx/popupwin.h>
#endif

#if defined(__WXMAC__)
#include <wx/sysopt.h>
#endif

#include "../Experimental.h"
#include "ASlider.h"

#include "../AColor.h"
#include "../ImageManipulation.h"
#include "../Project.h"
#include "../ShuttleGui.h"

#include "../Theme.h"
#include "../AllThemeResources.h"

//#include "../../images/SliderThumb.xpm"

#include "../../images/SliderThumbAlpha.xpm"
#include "../../images/SliderThumbDisabled.xpm"
#include "../../images/SliderThumb_Vertical.xpm"
#include "../../images/SliderThumb_VerticalAlpha.xpm"
#include "../../images/SliderThumb_VerticalDisabled.xpm"

#if defined __WXMSW__
const int sliderFontSize = 10;
#else
const int sliderFontSize = 12;
#endif

wxWindow* LWSlider::sharedTipPanel;
//
// TipPanel
//

#if USE_POPUPWIN
class TipPanel : public wxPopupWindow
#else
// On the Mac we use a wxFrame since wxPopWindow isn't supported yet
class TipPanel : public wxFrame
#endif
{
 public:
   TipPanel(wxWindow * parent, wxWindowID id,
            wxString label,
            const wxPoint &pos);
   virtual ~TipPanel(){}
   void SetPos(const wxPoint &pos, wxString maxLabel);

   void OnPaint(wxPaintEvent & event);

   void SetTargetParent( wxWindowBase *newParent ){mParent = (wxWindow*)newParent;}

   wxString label;

   wxWindow *mParent;
#ifdef USE_POPUPWIN
   //wxwin won't let you create a wxPopupWindow without a parent frame.
   //so we use a permanent offscreen dummy since projects can be deleted and their childs go with them.
   static wxFrame*  sharedDummyParent;
#endif

   DECLARE_EVENT_TABLE()
};

#if USE_POPUPWIN
#define kDummyOffsetX -32000
#define kDummyOffsetY -32000
wxFrame*  TipPanel::sharedDummyParent;

BEGIN_EVENT_TABLE(TipPanel, wxPopupWindow)
   EVT_PAINT(TipPanel::OnPaint)
END_EVENT_TABLE()

TipPanel::TipPanel(wxWindow *parent, wxWindowID  WXUNUSED(id),
                   wxString label, const wxPoint &pos):
wxPopupWindow(TipPanel::sharedDummyParent)
{
   this->label = label;
   mParent = parent;
   SetPos(pos, label);
}

void TipPanel::SetPos(const wxPoint& pos, wxString maxLabel)
{
   int x = pos.x;
   int y = pos.y;

//   if (mParent)
//      mParent->ClientToScreen(&x,&y);

   wxClientDC dc(this);
   wxFont labelFont(sliderFontSize, wxSWISS, wxNORMAL, wxNORMAL);
   dc.SetFont(labelFont);
   int width, height;
   dc.GetTextExtent(maxLabel, &width, &height);
   height += 4;
   SetSize(x - width/2, y, width, height);
}

#else

BEGIN_EVENT_TABLE(TipPanel, wxFrame)
   EVT_PAINT(TipPanel::OnPaint)
END_EVENT_TABLE()

TipPanel::TipPanel(wxWindow *parent, wxWindowID id,
                   wxString label,
                   const wxPoint &pos):
   wxFrame(NULL, id, wxEmptyString, wxDefaultPosition, wxDefaultSize,
           wxNO_BORDER | wxFRAME_NO_TASKBAR)
{
   mParent = parent;
   this->label = label;
   SetPos(pos, label);
}

void TipPanel::SetPos(const wxPoint& pos, wxString maxLabel)
{
   wxClientDC dc(this);
   wxFont labelFont(sliderFontSize, wxSWISS, wxNORMAL, wxNORMAL);
   dc.SetFont(labelFont);
   int width, height;
   dc.GetTextExtent(maxLabel, &width, &height);
   width += 4;
   height += 4;
   int left = pos.x - width/2;
   if (left < 0)
      left = 0;
   SetSize(left, pos.y, width, height);
   Raise();
}

#endif

void TipPanel::OnPaint(wxPaintEvent& WXUNUSED(event))
{
   wxPaintDC dc(this);
   int width, height, textWidth, textHeight;

   wxFont labelFont(sliderFontSize, wxSWISS, wxNORMAL, wxNORMAL);
   dc.SetFont(labelFont);
   GetClientSize(&width, &height);
#if defined(__WXMAC__)
   dc.SetPen(AColor::tooltipBrush.GetColour());
#else
   dc.SetPen(*wxBLACK_PEN);
#endif
   dc.SetBrush(AColor::tooltipBrush);
   dc.DrawRectangle(0, 0, width, height);
   dc.GetTextExtent(label, &textWidth, &textHeight);
   dc.DrawText(label, (width-textWidth)/2, (height-textHeight)/2);
}

//
// SliderDialog
//

BEGIN_EVENT_TABLE(SliderDialog, wxDialog)
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
   wxDialog(parent,id,title,position),
   mStyle(style)
{
   ShuttleGui S(this, eIsCreating);

   S.StartVerticalLay();
   {
      mTextCtrl = S.AddTextBox(wxEmptyString,
                               wxEmptyString,
                               15);
      mTextCtrl->SetValidator(wxTextValidator(wxFILTER_NUMERIC));

      mSlider = new ASlider(this,
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
      value = pow(10.0, value / 20.0);
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

// Construct customizable slider
LWSlider::LWSlider(wxWindow * parent,
                     wxString name,
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
                   wxString name,
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
                    wxString name,
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
   mWidth = size.x;
   mHeight = size.y;
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
   mThumbBitmapAllocated = false;
   mScrollLine = 1.0f;
   mScrollPage = 5.0f;

   mpRuler = NULL; // Do this and Move() before Draw().
   Move(pos);
   Draw();

   CreatePopWin();
}

LWSlider::~LWSlider()
{
   delete mBitmap;
   if (mThumbBitmapAllocated) {
      delete mThumbBitmap;
   }
   delete mpRuler;
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

wxWindow* LWSlider::GetToolTipParent() const
{
   wxWindow *top = mParent;
   while(top && top->GetParent()) {
      top = top->GetParent();
   }

   return top;
}

void LWSlider::CreatePopWin()
{
   maxTipLabel = mName + wxT(": 000000");

   if (mStyle == PAN_SLIDER || mStyle == DB_SLIDER || mStyle == SPEED_SLIDER
#ifdef EXPERIMENTAL_MIDI_OUT
       || mStyle == VEL_SLIDER
#endif
       )
      maxTipLabel += wxT("000");


   if(!LWSlider::sharedTipPanel) {
#ifdef USE_POPUPWIN
      TipPanel::sharedDummyParent = new wxFrame(NULL, -1, wxT("offscreentip"), wxPoint(kDummyOffsetX, kDummyOffsetY));
#endif
      LWSlider::sharedTipPanel = new TipPanel(mParent, -1, maxTipLabel, wxDefaultPosition);
      LWSlider::sharedTipPanel->Hide();
   }
}

void LWSlider::SetPopWinPosition()
{
   wxPoint pt;
   if (mOrientation == wxHORIZONTAL)
      pt = wxPoint(mWidth/2 + mLeft, mHeight + mTop + 1);
   else
      pt = wxPoint(mWidth + mLeft + 1, mHeight/2 + mTop);
   pt = mParent->ClientToScreen(pt);

   if (LWSlider::sharedTipPanel)
      ((TipPanel *)LWSlider::sharedTipPanel)->SetPos(pt, maxTipLabel);
}

void LWSlider::Move(const wxPoint &newpos)
{
   mLeft = newpos.x;
   mTop = newpos.y;
}

void LWSlider::RecreateTipWin()
{
   LWSlider::sharedTipPanel->Destroy();
   LWSlider::sharedTipPanel = NULL;
   CreatePopWin();
}

void LWSlider::OnPaint(wxDC &dc, bool  WXUNUSED(selected))
{
   //thumbPos should be in pixels
   int thumbPos = ValueToPosition(mCurrentValue);
   int thumbOrtho; // position in axis orthogonal to mOrientation
   if (mOrientation == wxHORIZONTAL)
      thumbOrtho = mCenterY - (mThumbHeight/2);
   else
      thumbOrtho = mCenterX - (mThumbWidth/2);

#if defined(__WXMSW__)
   if( mHW )
   {
      dc.Clear();
   }
#endif
#if defined(__WXGTK__)
   if (mHW)
   {
      dc.SetBackground(wxSystemSettings::GetColour(wxSYS_COLOUR_BACKGROUND));
      dc.Clear();
   }
#endif

   dc.DrawBitmap(*mBitmap, mLeft, mTop, true);
   if (mOrientation == wxHORIZONTAL)
      dc.DrawBitmap(*mThumbBitmap, mLeft+thumbPos, mTop+thumbOrtho, true);
   else
      dc.DrawBitmap(*mThumbBitmap, mLeft+thumbOrtho, mTop+thumbPos, true);

   if (LWSlider::sharedTipPanel)
      LWSlider::sharedTipPanel->Refresh();
}

void LWSlider::OnSize( wxSizeEvent & event )
{
   mWidth = event.GetSize().GetX();
   mHeight = event.GetSize().GetY();

   Draw();

   Refresh();
}

void LWSlider::Draw()
{
   //
   // Get the thumb slider bitmap
   //
   // AD: Setting the mThumbBitmap pointer requires caution, because
   // ownership of the object pointed to varies. If we've allocated
   // mThumbBitmap we must delete it first, and we must set
   // mThumbBitmapAllocated according to whether we have.
   //

   if (mEnabled && mOrientation == wxHORIZONTAL)
   {
      if (mThumbBitmapAllocated)
         delete mThumbBitmap;
      mThumbBitmap = &theTheme.Bitmap( bmpSliderThumb );
      mThumbBitmapAllocated = false;
   }
   //v \todo Convert this to an image in AllThemeResources, as bmpSliderThumb. Make an alpha also, as for horizontal slider thumb?
   else if (mOrientation == wxHORIZONTAL)
   {
      wxImage thumbImage(wxBitmap(SliderThumbDisabled).ConvertToImage());

      if (mThumbBitmapAllocated)
         delete mThumbBitmap;
      mThumbBitmap = new wxBitmap(thumbImage);
      mThumbBitmapAllocated = true;

      mThumbBitmap->SetMask(new wxMask(wxBitmap(SliderThumbAlpha), *wxBLACK));
   }
   else if (mEnabled)
   {
      wxImage thumbImage(wxBitmap(SliderThumb_Vertical).ConvertToImage());
      if (mThumbBitmapAllocated)
         delete mThumbBitmap;
      mThumbBitmap = new wxBitmap(thumbImage);
      mThumbBitmapAllocated = true;

      mThumbBitmap->SetMask(
            new wxMask(wxBitmap(SliderThumb_VerticalAlpha), *wxBLACK));
   }
   else
   {
      wxImage thumbImage(wxBitmap(
               SliderThumb_VerticalDisabled).ConvertToImage());

      if (mThumbBitmapAllocated)
         delete mThumbBitmap;
      mThumbBitmap = new wxBitmap(thumbImage);
      mThumbBitmapAllocated = true;

      mThumbBitmap->SetMask(
            new wxMask(wxBitmap(SliderThumb_VerticalAlpha), *wxBLACK));
   }

   //
   // Now the background bitmap
   //

   if( mBitmap )
   {
      delete mBitmap;
      mBitmap = NULL;
   }

   if (mOrientation == wxHORIZONTAL)
      mCenterY = mHeight - 9;
   else
      mCenterX = mWidth - 9;

   mThumbWidth = mThumbBitmap->GetWidth();
   mThumbHeight = mThumbBitmap->GetHeight();

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

   wxMemoryDC *dc = new wxMemoryDC();
   mBitmap = new wxBitmap(mWidth, mHeight);

   // Set background to an unused color.  This allows for easy
   // mask creation.  And garbage can be displayed if it isn't
   // cleared.
   dc->SelectObject(*mBitmap);

   wxColour TransparentColour = wxColour( 255, 254, 255 );
   // DO-THEME Mask colour!!  JC-Aug-2007
   // Needed with experimental theming!
   //  ... because windows blends against this colour.
#ifdef EXPERIMENTAL_THEMING
   TransparentColour = theTheme.Colour( clrTrackInfo );
#endif

   dc->SetBackground( wxBrush( TransparentColour  ) );
   dc->Clear();

   // Draw the line along which the thumb moves.
   AColor::Dark(dc, false);

   if (mOrientation == wxHORIZONTAL)
      AColor::Line(*dc, mLeftX, mCenterY+1, mRightX+2, mCenterY+1);
   else //v if (mStyle != DB_SLIDER) // Let the ruler do it for vertical DB_SLIDER.
      AColor::Line(*dc, mCenterX+1, mTopY, mCenterX+1, mBottomY+2);


   // Draw +/- or L/R first.  We need to draw these before the tick marks.
   if (mStyle == PAN_SLIDER)
   {
      //v Vertical PAN_SLIDER currently not handled, forced to horizontal.

      // sliderFontSize is for the tooltip.
      // we need something smaller here...
      wxFont labelFont(sliderFontSize-3, wxSWISS, wxNORMAL, wxNORMAL);
      dc->SetFont(labelFont);

      // Colors
#ifdef EXPERIMENTAL_THEMING
      dc->SetTextForeground( theTheme.Colour( clrTrackPanelText ));

      // TransparentColour should be same as clrTrackInfo.
      dc->SetTextBackground( theTheme.Colour( clrTrackInfo ) );
      dc->SetBackground( theTheme.Colour( clrTrackInfo ) );
      // HAVE to use solid and not transparent here,
      // otherwise windows will do it's clever font optimisation trick,
      // but against a default colour of white, which is not OK on a dark
      // background.
      dc->SetBackgroundMode( wxSOLID );
#else
      if (mEnabled)
         dc->SetTextForeground( wxColour( 0,0,0) );
      else
         dc->SetTextForeground( wxColour(128, 128, 128));

      dc->SetTextBackground( wxColour( 255,255,255));
#endif

      /* i18n-hint: One-letter abbreviation for Left, in the Pan slider */
      dc->DrawText(_("L"), mLeftX, 0);

      /* i18n-hint: One-letter abbreviation for Right, in the Pan slider */
      dc->DrawText(_("R"), mRightX-6,0);
   } else
   {
      // draw the '-' and the '+'
#ifdef EXPERIMENTAL_THEMING
      wxPen pen( theTheme.Colour( clrTrackPanelText ));
      dc->SetPen( pen );
#else
      if (mEnabled)
         dc->SetPen(*wxBLACK_PEN);
      else
         dc->SetPen(wxColour(128, 128, 128));
#endif
      if (mOrientation == wxHORIZONTAL)
      {
         AColor::Line(*dc, mLeftX, mCenterY-10, mLeftX+4, mCenterY-10);
         AColor::Line(*dc, mRightX-5, mCenterY-10, mRightX-1, mCenterY-10);
         AColor::Line(*dc, mRightX-3, mCenterY-12, mRightX-3, mCenterY-8);
      }
      else
      {
         // Vertical DB_SLIDER is for gain slider in MixerBoard.
         // We use a Ruler instead of marks & ticks.
         // Draw '+' and '-' only for other vertical sliders.
         if (mStyle != DB_SLIDER)
         {
            AColor::Line(*dc, mCenterX-12, mBottomY-3,  mCenterX-8, mBottomY-3);
            AColor::Line(*dc, mCenterX-12, mTopY+3,     mCenterX-8, mTopY+3);
            AColor::Line(*dc, mCenterX-10, mTopY,       mCenterX-10, mTopY+5);
         }
      }
   }

   //v 20090820: Ruler doesn't align with slider correctly -- yet.
   //if ((mOrientation == wxVERTICAL) && (mStyle == DB_SLIDER))
   //{
   //   if (!mpRuler)
   //   {
   //      mpRuler = new Ruler();
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
         upp = divs / (double)(mWidthX-1);
      else
      {
         if (mStyle == DB_SLIDER)
            divs = mMaxValue - mMinValue + 1;
         upp = divs / (double)(mHeightY-1);
      }
      double d = 0.0;
      int int_d = -1;
      const int kMax = (mOrientation == wxHORIZONTAL) ? mWidthX : mHeightY;
      for(int p = 0; p <= kMax; p++) {
         if (((int)d) > int_d) {
            int_d = (int)d;
            int tickLength = ((int_d == 0) || (int_d == divs)) ? 5: 3; // longer ticks at extremes
            AColor::Light(dc, false);
            if (mOrientation == wxHORIZONTAL)
               AColor::Line(*dc, mLeftX+p, mCenterY-tickLength, mLeftX+p, mCenterY-1); // ticks above
            else
               AColor::Line(*dc, mCenterX-tickLength, mTopY+p, mCenterX-1, mTopY+p); // ticks at left

            AColor::Dark(dc, false);

            if (mOrientation == wxHORIZONTAL)
               AColor::Line(*dc, mLeftX+p+1, mCenterY-tickLength+1, mLeftX+p+1, mCenterY-1); // ticks above
            else
               AColor::Line(*dc, mCenterX-tickLength+1, mTopY+p+1, mCenterX-1, mTopY+p+1); // ticks at left
         }
         d += upp;
      }
   }


   // Must preceed creating the mask as that will attempt to
   // select the bitmap into another DC.
   delete dc;

   mBitmap->SetMask( new wxMask( *mBitmap, TransparentColour ) );
}


void LWSlider::FormatPopWin()
{
   wxString label;
   wxString valstr;

   switch(mStyle) {
   case FRAC_SLIDER:
      label.Printf(wxT("%s: %.2f"), mName.c_str(), mCurrentValue);
      break;

   case DB_SLIDER:
      valstr.Printf(wxT("%.1f"), mCurrentValue);
      if (valstr.Right(1) == wxT("0"))
         valstr = valstr.Left(valstr.Length() - 2);
      if (mCurrentValue > 0)
         valstr = wxT("+") + valstr;

      label.Printf(wxT("%s: %s dB"), mName.c_str(), valstr.c_str());
      break;
   case PAN_SLIDER:
      if (mCurrentValue == 0.0)
         label.Printf(wxT("%s: %s"), mName.c_str(),
                      _("Center"));
      else {
         if (mCurrentValue < 0.0)
            label.Printf(wxT("%s: %.0f%% %s"), mName.c_str(),
                         -mCurrentValue * 100.0f, _("Left"));
         else /* if (val > 0.0) */
            label.Printf(wxT("%s: %.0f%% %s"), mName.c_str(),
                         mCurrentValue * 100.0f, _("Right"));
      }

      break;
   case SPEED_SLIDER:
      label.Printf(wxT("%s: %.2fx"), mName.c_str(), mCurrentValue);
      break;
#ifdef EXPERIMENTAL_MIDI_OUT
   case VEL_SLIDER:
       label.Printf(wxT("%s: %s%d"), mName.c_str(),
                    (mCurrentValue > 0.0f ? _("+") : wxT("")),
                    (int) mCurrentValue);
#endif
   }

   ((TipPanel *)LWSlider::sharedTipPanel)->label = label;
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

void LWSlider::DeleteSharedTipPanel()
{
   if(LWSlider::sharedTipPanel) {
      ((TipPanel*)LWSlider::sharedTipPanel)->Destroy();
#ifdef USE_POPUPWIN
      TipPanel::sharedDummyParent->Destroy();
#endif
   }
   LWSlider::sharedTipPanel = NULL;
}

void LWSlider::OnMouseEvent(wxMouseEvent & event)
{
   if (event.Entering()) {
      #if wxUSE_TOOLTIPS // Not available in wxX11
      // Display the tooltip in the status bar
      if (mParent->GetToolTip())
      {
         wxString tip = mParent->GetToolTip()->GetTip();
         GetActiveProject()->TP_DisplayStatusMessage(tip);
         Refresh();
      }
      #endif
   }
   else if (event.Leaving())
   {
      GetActiveProject()->TP_DisplayStatusMessage(wxT(""));
      Refresh();
   }

   // Events other than mouse-overs are ignored when we are disabled
   if (!mEnabled)
      return;

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
      if( mDefaultShortcut && event.CmdDown() )
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

      mParent->CaptureMouse();
      // wxSetCursor(wxCURSOR_BLANK);
      ((TipPanel*)LWSlider::sharedTipPanel)->SetTargetParent(mParent);
      FormatPopWin();
      SetPopWinPosition();
      LWSlider::sharedTipPanel->Show();
      //hide mouseover tooltip
      wxToolTip::Enable(false);
   }
   else if( event.ButtonUp() )
   {
      mIsDragging = false;
      if (mParent->HasCapture())
         mParent->ReleaseMouse();
      LWSlider::sharedTipPanel->Hide();
      //restore normal tooltip behavor for mouseovers
      wxToolTip::Enable(true);
      // wxSetCursor(wxNullCursor);
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
#if !wxCHECK_VERSION(2,7,0)
         case WXK_PRIOR:
#endif
            Increase( mScrollPage );
            SendUpdate( mCurrentValue );
            break;

         case WXK_PAGEDOWN:
#if !wxCHECK_VERSION(2,7,0)
         case WXK_NEXT:
#endif
            Decrease( mScrollPage );
            SendUpdate( mCurrentValue );
            break;

         case WXK_HOME:
            SendUpdate( mMinValue );
            break;

         case WXK_END:
            SendUpdate( mMaxValue );
            break;

         case WXK_TAB:
            {
               wxNavigationKeyEvent nevent;
               nevent.SetWindowChange( event.ControlDown() );
               nevent.SetDirection( !event.ShiftDown() );
               nevent.SetEventObject( mParent );
               nevent.SetCurrentFocus( mParent );
               mParent->GetParent()->ProcessEvent( nevent );
            }
            break;

         case WXK_RETURN:
         case WXK_NUMPAD_ENTER:
            {
               wxTopLevelWindow *tlw = wxDynamicCast(wxGetTopLevelParent(mParent), wxTopLevelWindow);
               wxWindow *def = tlw->GetDefaultItem();
               if (def && def->IsEnabled()) {
                  wxCommandEvent cevent(wxEVT_COMMAND_BUTTON_CLICKED,
                        def->GetId());
                  mParent->ProcessEvent(cevent);
               }
            }

         default:
            // Allow it to propagate
            event.Skip();
            break;
      }
   }

   event.Skip();
}

void LWSlider::SendUpdate( float newValue )
{
   mCurrentValue = newValue;
   FormatPopWin();
   LWSlider::sharedTipPanel->Refresh();
   Refresh();

   wxCommandEvent e( wxEVT_COMMAND_SLIDER_UPDATED, mID );
   int intValue = (int)( ( mCurrentValue - mMinValue ) * 1000.0f /
                         ( mMaxValue - mMinValue ) );
   e.SetInt( intValue );
   mParent->ProcessEvent( e );
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

// Given the mouse slider coordinate in fromPos, compute the new value
// of the slider when clicking to set a new position.
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

// Given the mouse slider coordinate in fromPos, compute the new value
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
      return ( convert ? pow(10.0f, mCurrentValue / 20.0f) : mCurrentValue );
   else
      return mCurrentValue;
}

void LWSlider::Set(float value)
{
   if (mIsDragging)
      return;
   if (mStyle == DB_SLIDER)
      mCurrentValue = 20.0f*log10(value);
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
   Draw();
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
END_EVENT_TABLE()

ASlider::ASlider( wxWindow * parent,
                  wxWindowID id,
                  wxString name,
                  const wxPoint & pos,
                  const wxSize & size,
                  int style,
                  bool popup,
                  bool canUseShift,
                  float stepValue,
                  int orientation /*= wxHORIZONTAL*/)
: wxPanel( parent, id, pos, size, wxWANTS_CHARS )
{
   mLWSlider = new LWSlider( this,
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

#if wxUSE_ACCESSIBILITY
   SetAccessible( new ASliderAx( this ) );
#endif
}


ASlider::~ASlider()
{
   delete mLWSlider;
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

#ifdef EXPERIMENTAL_THEMING
   wxColour Col(GetParent()->GetBackgroundColour());
   this->SetBackgroundColour( Col );
#endif

   mLWSlider->OnPaint(dc, false);

   if ( mSliderIsFocused )
   {
      wxRect r( 0, 0, mLWSlider->mWidth, mLWSlider->mHeight );

      r.Deflate( 1, 1 );

      AColor::DrawFocus( dc, r );
   }
}

void ASlider::OnMouseEvent(wxMouseEvent &event)
{
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

void ASlider::RecreateTipWin()
{
   mLWSlider->RecreateTipWin();
}

void ASlider::GetScroll(float & line, float & page)
{
   mLWSlider->GetScroll(line, page);
}

void ASlider::SetScroll(float line, float page)
{
   mLWSlider->SetScroll(line, page);
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
   return true;
}

bool ASlider::IsEnabled() const
{
   return mLWSlider->GetEnabled();
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
