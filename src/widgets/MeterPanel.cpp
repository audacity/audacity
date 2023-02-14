/**********************************************************************

  Audacity: A Digital Audio Editor

  MeterPanel.cpp

  Dominic Mazzoni
  Vaughan Johnson

  2004.06.25 refresh rate limited to 30mS, by ChackoN

*******************************************************************//**

\class MeterPanel
\brief VU Meter, for displaying recording/playback level

  This is a bunch of common code that can display many different
  forms of VU meters and other displays.

  But note that a lot of later code here assumes these are
  MeterToolBar meters, e.g., MeterPanel::StartMonitoring,
  so these are not as generic/common as originally intended.

*//****************************************************************//**

\class MeterBar
\brief A struct used by MeterPanel to hold the position of one bar.

*//****************************************************************//**

\class MeterUpdateMsg
\brief Message used to update the MeterPanel

*//****************************************************************//**

\class MeterUpdateQueue
\brief Queue of MeterUpdateMsg used to feed the MeterPanel.

*//******************************************************************/

#include "MeterPanel.h"

#include <algorithm>
#include <wx/setup.h> // for wxUSE_* macros
#include <wx/wxcrtvararg.h>
#include <wx/defs.h>
#include <wx/dcbuffer.h>
#include <wx/frame.h>
#include <wx/menu.h>
#include <wx/settings.h>
#include <wx/textdlg.h>
#include <wx/numdlg.h>
#include <wx/radiobut.h>
#include <wx/tooltip.h>

#include <math.h>

#include "AudioIO.h"
#include "AColor.h"
#include "../widgets/BasicMenu.h"
#include "ImageManipulation.h"
#include "Decibels.h"
#include "Project.h"
#include "ProjectAudioIO.h"
#include "ProjectStatus.h"
#include "../ProjectWindows.h"
#include "Prefs.h"
#include "ShuttleGui.h"
#include "Theme.h"
#include "wxWidgetsWindowPlacement.h"

#include "AllThemeResources.h"
#include "valnum.h"

#if wxUSE_ACCESSIBILITY
#include "WindowAccessible.h"

class MeterAx final : public WindowAccessible
{
public:
   MeterAx(wxWindow * window);

   virtual ~ MeterAx();

   // Performs the default action. childId is 0 (the action for this object)
   // or > 0 (the action for a child).
   // Return wxACC_NOT_SUPPORTED if there is no default action for this
   // window (e.g. an edit control).
   wxAccStatus DoDefaultAction(int childId) override;

   // Retrieves the address of an IDispatch interface for the specified child.
   // All objects must support this property.
   wxAccStatus GetChild(int childId, wxAccessible** child) override;

   // Gets the number of children.
   wxAccStatus GetChildCount(int* childCount) override;

   // Gets the default action for this object (0) or > 0 (the action for a child).
   // Return wxACC_OK even if there is no action. actionName is the action, or the empty
   // string if there is no action.
   // The retrieved string describes the action that is performed on an object,
   // not what the object does as a result. For example, a toolbar button that prints
   // a document has a default action of "Press" rather than "Prints the current document."
   wxAccStatus GetDefaultAction(int childId, wxString *actionName) override;

   // Returns the description for this object or a child.
   wxAccStatus GetDescription(int childId, wxString *description) override;

   // Gets the window with the keyboard focus.
   // If childId is 0 and child is NULL, no object in
   // this subhierarchy has the focus.
   // If this object has the focus, child should be 'this'.
   wxAccStatus GetFocus(int *childId, wxAccessible **child) override;

   // Returns help text for this object or a child, similar to tooltip text.
   wxAccStatus GetHelpText(int childId, wxString *helpText) override;

   // Returns the keyboard shortcut for this object or child.
   // Return e.g. ALT+K
   wxAccStatus GetKeyboardShortcut(int childId, wxString *shortcut) override;

   // Returns the rectangle for this object (id = 0) or a child element (id > 0).
   // rect is in screen coordinates.
   wxAccStatus GetLocation(wxRect& rect, int elementId) override;
   
   // Returns a role constant.
   wxAccStatus GetRole(int childId, wxAccRole *role) override;

   // Gets a variant representing the selected children
   // of this object.
   // Acceptable values:
   // - a null variant (IsNull() returns TRUE)
   // - a list variant (GetType() == wxT("list"))
   // - an integer representing the selected child element,
   //   or 0 if this object is selected (GetType() == wxT("long"))
   // - a "void*" pointer to a wxAccessible child object
   wxAccStatus GetSelections(wxVariant *selections) override;

   // Returns a state constant.
   wxAccStatus GetState(int childId, long* state) override;

   // Returns a localized string representing the value for the object
   // or child.
   wxAccStatus GetValue(int childId, wxString* strValue) override;

};

#endif // wxUSE_ACCESSIBILITY

static const long MIN_REFRESH_RATE = 1;
static const long MAX_REFRESH_RATE = 100;

/* Updates to the meter are passed across via meter updates, each contained in
 * a MeterUpdateMsg object */
wxString MeterUpdateMsg::toString()
{
wxString output;  // somewhere to build up a string in
output = wxString::Format(wxT("Meter update msg: %i channels, %i samples\n"), \
      kMaxMeterBars, numFrames);
for (int i = 0; i<kMaxMeterBars; i++)
   {  // for each channel of the meters
   output += wxString::Format(wxT("%f peak, %f rms "), peak[i], rms[i]);
   if (clipping[i])
      output += wxString::Format(wxT("clipped "));
   else
      output += wxString::Format(wxT("no clip "));
   output += wxString::Format(wxT("%i head, %i tail\n"), headPeakCount[i], tailPeakCount[i]);
   }
return output;
}

wxString MeterUpdateMsg::toStringIfClipped()
{
   for (int i = 0; i<kMaxMeterBars; i++)
   {
      if (clipping[i] || (headPeakCount[i] > 0) || (tailPeakCount[i] > 0))
         return toString();
   }
   return wxT("");
}

//
// The MeterPanel passes itself messages via this queue so that it can
// communicate between the audio thread and the GUI thread.
// This class uses lock-free synchronization with atomics.
//

MeterUpdateQueue::MeterUpdateQueue(size_t maxLen):
   mBufferSize(maxLen)
{
   Clear();
}

// destructor
MeterUpdateQueue::~MeterUpdateQueue()
{
}

void MeterUpdateQueue::Clear()
{
   mStart.store(0);
   mEnd.store(0);
}

// Add a message to the end of the queue.  Return false if the
// queue was full.
bool MeterUpdateQueue::Put(MeterUpdateMsg &msg)
{
   auto start = mStart.load(std::memory_order_acquire);
   auto end = mEnd.load(std::memory_order_relaxed);
   // mStart can be greater than mEnd because it is all mod mBufferSize
   assert( (end + mBufferSize - start) >= 0 );
   int len = (end + mBufferSize - start) % mBufferSize;

   // Never completely fill the queue, because then the
   // state is ambiguous (mStart==mEnd)
   if (len + 1 >= (int)(mBufferSize))
      return false;

   //wxLogDebug(wxT("Put: %s"), msg.toString());

   mBuffer[end] = msg;
   mEnd.store((end + 1) % mBufferSize, std::memory_order_release);

   return true;
}

// Get the next message from the start of the queue.
// Return false if the queue was empty.
bool MeterUpdateQueue::Get(MeterUpdateMsg &msg)
{
   auto start = mStart.load(std::memory_order_relaxed);
   auto end = mEnd.load(std::memory_order_acquire);
   int len = (end + mBufferSize - start) % mBufferSize;

   if (len == 0)
      return false;

   msg = mBuffer[start];
   mStart.store((start + 1) % mBufferSize, std::memory_order_release);

   return true;
}

//
// MeterPanel class
//

#include "../../images/SpeakerMenu.xpm"
#include "../../images/MicMenu.xpm"

// How many pixels between items?
const static int gap = 2;

const static wxChar *PrefStyles[] =
{
   wxT("AutomaticStereo"),
   wxT("HorizontalStereo"),
   wxT("VerticalStereo")
};

enum {
   OnMeterUpdateID = 6000,
   OnMonitorID,
   OnPreferencesID,
   OnTipTimeoutID
};

BEGIN_EVENT_TABLE(MeterPanel, MeterPanelBase)
   EVT_TIMER(OnMeterUpdateID, MeterPanel::OnMeterUpdate)
   EVT_TIMER(OnTipTimeoutID, MeterPanel::OnTipTimeout)
   EVT_SLIDER(wxID_ANY, MeterPanel::SetMixer)
   EVT_MOUSE_EVENTS(MeterPanel::OnMouse)
   EVT_CONTEXT_MENU(MeterPanel::OnContext)
   EVT_KEY_DOWN(MeterPanel::OnKeyDown)
   EVT_CHAR_HOOK(MeterPanel::OnCharHook)
   EVT_SET_FOCUS(MeterPanel::OnSetFocus)
   EVT_KILL_FOCUS(MeterPanel::OnKillFocus)
   EVT_ERASE_BACKGROUND(MeterPanel::OnErase)
   EVT_PAINT(MeterPanel::OnPaint)
   EVT_SIZE(MeterPanel::OnSize)
   EVT_MENU(OnMonitorID, MeterPanel::OnMonitor)
   EVT_MENU(OnPreferencesID, MeterPanel::OnPreferences)
END_EVENT_TABLE()

IMPLEMENT_CLASS(MeterPanel, wxPanelWrapper)

MeterPanel::MeterPanel(AudacityProject *project,
             wxWindow* parent, wxWindowID id,
             bool isInput,
             const wxPoint& pos /*= wxDefaultPosition*/,
             const wxSize& size /*= wxDefaultSize*/,
             Style style /*= HorizontalStereo*/,
             float fDecayRate /*= 60.0f*/)
: MeterPanelBase(parent, id, pos, size, wxTAB_TRAVERSAL | wxNO_BORDER | wxWANTS_CHARS),
   mProject(project),
   mQueue{ 1024 },
   mWidth(size.x),
   mHeight(size.y),
   mIsInput(isInput),
   mDesiredStyle(style),
   mGradient(true),
   mDB(true),
   mDBRange(DecibelScaleCutoff.Read()),
   mDecay(true),
   mDecayRate(fDecayRate),
   mClip(true),
   mNumPeakSamplesToClip(3),
   mPeakHoldDuration(3),
   mT(0),
   mRate(0),
   mMonitoring(false),
   mActive(false),
   mNumBars(0),
   mLayoutValid(false),
   mBitmap{}
{
   // i18n-hint: Noun (the meter is used for playback or record level monitoring)
   SetName( XO("Meter") );
   // Suppress warnings about the header file
   wxUnusedVar(SpeakerMenu_xpm);
   wxUnusedVar(MicMenu_xpm);
   wxUnusedVar(PrefStyles);

   mStyle = mDesiredStyle;

   mIsFocused = false;

#if wxUSE_ACCESSIBILITY
   SetAccessible(safenew MeterAx(this));
#endif

   // Do this BEFORE UpdatePrefs()!
   mRuler.SetFonts(GetFont(), GetFont(), GetFont());
   mRuler.SetFlip(mStyle != MixerTrackCluster);
   mRuler.SetLabelEdges(true);
   //mRuler.SetTickColour( wxColour( 0,0,255 ) );

   if (mStyle != MixerTrackCluster)
   {
      mSlider = std::make_unique<LWSlider>(this, XO(""),
         pos,
         size,
         PERCENT_SLIDER,
         false,   /* showlabels */
         false,   /* drawticks */
         false,   /* drawtrack */
         false     /* alwayshidetip */
      );
      mSlider->SetScroll(0.1f, 2.0f);
   }

   UpdateSliderControl();
   UpdatePrefs();

   wxColour backgroundColour = theTheme.Colour( clrMedium);
   mBkgndBrush = wxBrush(backgroundColour, wxBRUSHSTYLE_SOLID);
   SetBackgroundColour( backgroundColour );
   
   mPeakPeakPen = wxPen(theTheme.Colour( clrMeterPeak),        1, wxPENSTYLE_SOLID);
   mDisabledPen = wxPen(theTheme.Colour( clrMeterDisabledPen), 1, wxPENSTYLE_SOLID);

   mAudioIOStatusSubscription = AudioIO::Get()
      ->Subscribe(*this, &MeterPanel::OnAudioIOStatus);

   mAudioCaptureSubscription = AudioIO::Get()
      ->Subscribe(*this, &MeterPanel::OnAudioCapture);

   if (mIsInput) {
      mPen       = wxPen(   theTheme.Colour( clrMeterInputPen         ), 1, wxPENSTYLE_SOLID);
      mBrush     = wxBrush( theTheme.Colour( clrMeterInputBrush       ), wxBRUSHSTYLE_SOLID);
      mRMSBrush  = wxBrush( theTheme.Colour( clrMeterInputRMSBrush    ), wxBRUSHSTYLE_SOLID);
      mClipBrush = wxBrush( theTheme.Colour( clrMeterInputClipBrush   ), wxBRUSHSTYLE_SOLID);
//      mLightPen  = wxPen(   theTheme.Colour( clrMeterInputLightPen    ), 1, wxSOLID);
//      mDarkPen   = wxPen(   theTheme.Colour( clrMeterInputDarkPen     ), 1, wxSOLID);
   }
   else {
      mPen       = wxPen(   theTheme.Colour( clrMeterOutputPen        ), 1, wxPENSTYLE_SOLID);
      mBrush     = wxBrush( theTheme.Colour( clrMeterOutputBrush      ), wxBRUSHSTYLE_SOLID);
      mRMSBrush  = wxBrush( theTheme.Colour( clrMeterOutputRMSBrush   ), wxBRUSHSTYLE_SOLID);
      mClipBrush = wxBrush( theTheme.Colour( clrMeterOutputClipBrush  ), wxBRUSHSTYLE_SOLID);
//      mLightPen  = wxPen(   theTheme.Colour( clrMeterOutputLightPen   ), 1, wxSOLID);
//      mDarkPen   = wxPen(   theTheme.Colour( clrMeterOutputDarkPen    ), 1, wxSOLID);
   }

//   mDisabledBkgndBrush = wxBrush(theTheme.Colour( clrMeterDisabledBrush), wxSOLID);
   // No longer show a difference in the background colour when not monitoring.
   // We have the tip instead.
   mDisabledBkgndBrush = mBkgndBrush;

   mTipTimer.SetOwner(this, OnTipTimeoutID);
   mTimer.SetOwner(this, OnMeterUpdateID);
   // TODO: Yikes.  Hard coded sample rate.
   // JKC: I've looked at this, and it's benignish.  It just means that the meter
   // balistics are right for 44KHz and a bit more frisky than they should be
   // for higher sample rates.
   Reset(44100.0, true);
}

void MeterPanel::Clear()
{
   mQueue.Clear();
}

void MeterPanel::UpdatePrefs()
{
   mDBRange = DecibelScaleCutoff.Read();

   mMeterRefreshRate =
      std::max(MIN_REFRESH_RATE, std::min(MAX_REFRESH_RATE,
         gPrefs->Read(Key(wxT("RefreshRate")), 30)));
   mGradient = gPrefs->Read(Key(wxT("Bars")), wxT("Gradient")) == wxT("Gradient");
   mDB = gPrefs->Read(Key(wxT("Type")), wxT("dB")) == wxT("dB");
   mMeterDisabled = gPrefs->Read(Key(wxT("Disabled")), (long)0);

   if (mDesiredStyle != MixerTrackCluster)
   {
      wxString style = gPrefs->Read(Key(wxT("Style")));
      if (style == wxT("AutomaticStereo"))
      {
         mDesiredStyle = AutomaticStereo;
      }
      else if (style == wxT("HorizontalStereo"))
      {
         mDesiredStyle = HorizontalStereo;
      }
      else if (style == wxT("VerticalStereo"))
      {
         mDesiredStyle = VerticalStereo;
      }
      else
      {
         mDesiredStyle = AutomaticStereo;
      }
   }

   // Set the desired orientation (resets ruler orientation)
   SetActiveStyle(mDesiredStyle);

   // Reset to ensure NEW size is retrieved when language changes
   mLeftSize = wxSize(0, 0);
   mRightSize = wxSize(0, 0);

   Reset(mRate, false);

   mLayoutValid = false;

   Refresh(false);
}

static int MeterPrefsID()
{
   static int value = wxNewId();
   return value;
}

void MeterPanel::UpdateSelectedPrefs(int id)
{
   if (id == MeterPrefsID())
   {
#if USE_PORTMIXER
      if (mIsInput && mSlider)
      {
         // Show or hide the input slider based on whether it works
         auto gAudioIO = AudioIO::Get();
         mSlider->SetEnabled(mEnabled && gAudioIO->InputMixerWorks());
      }
#endif
      UpdatePrefs();
   }
}

void MeterPanel::UpdateSliderControl()
{
#if USE_PORTMIXER
   float inputVolume;
   float playbackVolume;
   int inputSource;

   // Show or hide the input slider based on whether it works
   auto gAudioIO = AudioIO::Get();
   if (mIsInput && mSlider)
      mSlider->SetEnabled(mEnabled && gAudioIO->InputMixerWorks());

   gAudioIO->GetMixer(&inputSource, &inputVolume, &playbackVolume);

   const auto volume = mIsInput ? inputVolume : playbackVolume;

   if (mSlider && (mSlider->Get() != volume))
      mSlider->Set(volume);

#endif // USE_PORTMIXER
}

void MeterPanel::OnErase(wxEraseEvent & WXUNUSED(event))
{
   // Ignore it to prevent flashing
}

void MeterPanel::OnPaint(wxPaintEvent & WXUNUSED(event))
{
#if defined(__WXMAC__)
   auto paintDC = std::make_unique<wxPaintDC>(this);
#else
   std::unique_ptr<wxDC> paintDC{ wxAutoBufferedPaintDCFactory(this) };
#endif
   wxDC & destDC = *paintDC;
   wxColour clrText = theTheme.Colour( clrTrackPanelText );
   wxColour clrBoxFill = theTheme.Colour( clrMedium );

   if (mLayoutValid == false || (mStyle == MixerTrackCluster ))
   {
      // Create a NEW one using current size and select into the DC
      mBitmap = std::make_unique<wxBitmap>();
      mBitmap->Create(mWidth, mHeight, destDC);
      wxMemoryDC dc;
      dc.SelectObject(*mBitmap);

      // Go calculate all of the layout metrics
      HandleLayout(dc);

      // Start with a clean background
      // LLL:  Should research USE_AQUA_THEME usefulness...
//#ifndef USE_AQUA_THEME
#ifdef EXPERIMENTAL_THEMING
      //if( !mMeterDisabled )
      //{
      //   mBkgndBrush.SetColour( GetParent()->GetBackgroundColour() );
      //}
#endif
      mBkgndBrush.SetColour( GetBackgroundColour() );
      dc.SetPen(*wxTRANSPARENT_PEN);
      dc.SetBrush(mBkgndBrush);
      dc.DrawRectangle(0, 0, mWidth, mHeight);
//#endif

      // MixerTrackCluster style has no icon or L/R labels
      if (mStyle != MixerTrackCluster)
      {
         dc.SetFont(GetFont());
         dc.SetTextForeground( clrText );
         dc.SetTextBackground( clrBoxFill );
         dc.DrawText(mLeftText, mLeftTextPos.x, mLeftTextPos.y);
         dc.DrawText(mRightText, mRightTextPos.x, mRightTextPos.y);
      }

      // Setup the colors for the 3 sections of the meter bars
      wxColor green(117, 215, 112);
      wxColor yellow(255, 255, 0);
      wxColor red(255, 0, 0);

      // Bug #2473 - (Sort of) Hack to make text on meters more
      // visible with darker backgrounds. It would be better to have
      // different colors entirely and as part of the theme.
      if (GetBackgroundColour().GetLuminance() < 0.25)
      {
         green = wxColor(117-100, 215-100, 112-100);
         yellow = wxColor(255-100, 255-100, 0);
         red = wxColor(255-100, 0, 0);
      }
      else if (GetBackgroundColour().GetLuminance() < 0.50)
      {
         green = wxColor(117-50, 215-50, 112-50);
         yellow = wxColor(255-50, 255-50, 0);
         red = wxColor(255-50, 0, 0);
      }

      // Draw the meter bars at maximum levels
      for (unsigned int i = 0; i < mNumBars; i++)
      {
         // Give it a recessed look
         AColor::Bevel(dc, false, mBar[i].b);

         // Draw the clip indicator bevel
         if (mClip)
         {
            AColor::Bevel(dc, false, mBar[i].rClip);
         }

         // Cache bar rect
         wxRect r = mBar[i].r;

         if (mGradient)
         {
            // Calculate the size of the two gradiant segments of the meter
            double gradw;
            double gradh;
            if (mDB)
            {
               gradw = (double) r.GetWidth() / mDBRange * 6.0;
               gradh = (double) r.GetHeight() / mDBRange * 6.0;
            }
            else
            {
               gradw = (double) r.GetWidth() / 100 * 25;
               gradh = (double) r.GetHeight() / 100 * 25;
            }

            if (mBar[i].vert)
            {
               // Draw the "critical" segment (starts at top of meter and works down)
               r.SetHeight(gradh);
               dc.GradientFillLinear(r, red, yellow, wxSOUTH);

               // Draw the "warning" segment
               r.SetTop(r.GetBottom());
               dc.GradientFillLinear(r, yellow, green, wxSOUTH);

               // Draw the "safe" segment
               r.SetTop(r.GetBottom());
               r.SetBottom(mBar[i].r.GetBottom());
               dc.SetPen(*wxTRANSPARENT_PEN);
               dc.SetBrush(green);
               dc.DrawRectangle(r);
            }
            else
            {
               // Draw the "safe" segment
               r.SetWidth(r.GetWidth() - (int) (gradw + gradw + 0.5));
               dc.SetPen(*wxTRANSPARENT_PEN);
               dc.SetBrush(green);
               dc.DrawRectangle(r);

               // Draw the "warning"  segment
               r.SetLeft(r.GetRight() + 1);
               r.SetWidth(floor(gradw));
               dc.GradientFillLinear(r, green, yellow);

               // Draw the "critical" segment
               r.SetLeft(r.GetRight() + 1);
               r.SetRight(mBar[i].r.GetRight());
               dc.GradientFillLinear(r, yellow, red);
            }
#ifdef EXPERIMENTAL_METER_LED_STYLE
            if (!mBar[i].vert)
            {
               wxRect r = mBar[i].r;
               wxPen BackgroundPen;
               BackgroundPen.SetColour( wxSystemSettings::GetColour(wxSYS_COLOUR_3DFACE) );
               dc.SetPen( BackgroundPen );
               int i;
               for(i=0;i<r.width;i++)
               {
                  // 2 pixel spacing between the LEDs
                  if( (i%7)<2 ){
                     AColor::Line( dc, i+r.x, r.y, i+r.x, r.y+r.height );
                  } else {
                     // The LEDs have triangular ends.  
                     // This code shapes the ends.
                     int j = abs( (i%7)-4);
                     AColor::Line( dc, i+r.x, r.y, i+r.x, r.y+j +1);
                     AColor::Line( dc, i+r.x, r.y+r.height-j, i+r.x, r.y+r.height );
                  }
               }
            }
#endif
         }
      }
      mRuler.SetTickColour( clrText );
      dc.SetTextForeground( clrText );
      // Draw the ruler
#ifndef EXPERIMENTAL_DA
      mRuler.Draw(dc);
#endif

      // Bitmap created...unselect
      dc.SelectObject(wxNullBitmap);
   }

   // Copy predrawn bitmap to the dest DC
   destDC.DrawBitmap(*mBitmap, 0, 0);

   // Go draw the meter bars, Left & Right channels using current levels
   for (unsigned int i = 0; i < mNumBars; i++)
   {
      DrawMeterBar(destDC, &mBar[i]);
   }

   destDC.SetTextForeground( clrText );

#ifndef EXPERIMENTAL_DA
   // We can have numbers over the bars, in which case we have to draw them each time.
   if (mStyle == HorizontalStereoCompact || mStyle == VerticalStereoCompact)
   {
      mRuler.SetTickColour( clrText );
      // If the text colour is too similar to the meter colour, then we need a background
      // for the text.  We require a total of at least one full-scale RGB difference.
      int d = theTheme.ColourDistance( clrText, theTheme.Colour( clrMeterOutputRMSBrush ) );
      if( d < 256 )
      {
         destDC.SetBackgroundMode( wxSOLID );
         destDC.SetTextBackground( clrBoxFill );
      }
      mRuler.Draw(destDC);
   }
#endif

   if (mStyle != MixerTrackCluster)
   {
      bool highlighted =
      wxRect{ mSliderPos, mSliderSize }.Contains(
         ScreenToClient(
            ::wxGetMousePosition()));

      mSlider->Move(mSliderPos);
      mSlider->AdjustSize(mSliderSize);
      mSlider->OnPaint(destDC, highlighted);
   }

   if (mIsFocused)
   {
      auto r = GetClientRect();
      AColor::DrawFocus(destDC, r);
   }
}

void MeterPanel::OnSize(wxSizeEvent & WXUNUSED(event))
{
   GetClientSize(&mWidth, &mHeight);

   mLayoutValid = false;
   Refresh();
}

void MeterPanel::OnMouse(wxMouseEvent &evt)
{
   if ((evt.GetEventType() == wxEVT_MOTION || evt.Entering() || evt.Leaving())) {
      mLayoutValid = false;
      Refresh();
   }

   if (mStyle == MixerTrackCluster) // MixerTrackCluster style has no menu.
      return;

   if (evt.Entering()) {
      mTipTimer.StartOnce(500);
   }
   else if(evt.Leaving())
      mTipTimer.Stop();

   if (evt.RightDown())
      ShowMenu(evt.GetPosition());
   else
   {
      
      if (mSlider)
         mSlider->OnMouseEvent(evt);
   }
}

void MeterPanel::OnCharHook(wxKeyEvent& evt)
{
   switch(evt.GetKeyCode())
   {
   // These are handled in the OnCharHook handler because, on Windows at least, the
   // key up event will be passed on to the menu if we show it here.  This causes
   // the default sound to be heard if assigned.
   case WXK_RETURN:
   case WXK_NUMPAD_ENTER:
   case WXK_WINDOWS_MENU:
   case WXK_MENU:
      if (mStyle != MixerTrackCluster)
         ShowMenu(GetClientRect().GetBottomLeft());
      else
         evt.Skip();
      break;
   default:
      evt.Skip();
      break;
   }
}

void MeterPanel::OnContext(wxContextMenuEvent &evt)
{
   if (mStyle != MixerTrackCluster) // MixerTrackCluster style has no menu.
   {
      ShowMenu(GetClientRect().GetBottomLeft());
   }
   else
   {
      evt.Skip();
   }
}

void MeterPanel::OnKeyDown(wxKeyEvent &evt)
{
   switch (evt.GetKeyCode())
   {
   case WXK_TAB:
      if (evt.ShiftDown())
         Navigate(wxNavigationKeyEvent::IsBackward);
      else
         Navigate(wxNavigationKeyEvent::IsForward);
      break;
   default:
      mSlider->OnKeyDown(evt);
      break;
   }
}

void MeterPanel::OnSetFocus(wxFocusEvent & WXUNUSED(evt))
{
   mIsFocused = true;
   Refresh(false);
}

void MeterPanel::OnKillFocus(wxFocusEvent & WXUNUSED(evt))
{
   if(mSlider)
      mSlider->OnKillFocus();
   mTipTimer.Stop();

   mIsFocused = false;
   Refresh(false);
}

void MeterPanel::SetStyle(Style newStyle)
{
   if (mStyle != newStyle && mDesiredStyle == AutomaticStereo)
   {
      SetActiveStyle(newStyle);

      mLayoutValid = false;

      Refresh(false);
   }
}

void MeterPanel::SetMixer(wxCommandEvent & WXUNUSED(event))
{
#if USE_PORTMIXER
   if (mSlider)
   {
      float inputVolume;
      float outputVolume;
      int inputSource;

      Refresh();

      auto gAudioIO = AudioIO::Get();
      gAudioIO->GetMixer(&inputSource, &inputVolume, &outputVolume);

      if (mIsInput)
         inputVolume = mSlider->Get();
      else
         outputVolume = mSlider->Get();

      gAudioIO->SetMixer(inputSource, inputVolume, outputVolume);

#if wxUSE_ACCESSIBILITY
      GetAccessible()->NotifyEvent( wxACC_EVENT_OBJECT_VALUECHANGE,
                                    this,
                                    wxOBJID_CLIENT,
                                    wxACC_SELF );
#endif

   }
#endif // USE_PORTMIXER
}

bool MeterPanel::ShowDialog()
{
   if (!mSlider)
      return false;

   auto changed = mSlider->ShowDialog();
   if (changed)
   {
      wxCommandEvent e;
      SetMixer(e);
   }

   return changed;
}

void MeterPanel::Increase(float steps)
{
   if (mSlider)
   {
      wxCommandEvent e;

      mSlider->Increase(steps);
      SetMixer(e);
   }
}

void MeterPanel::Decrease(float steps)
{
   if (mSlider)
   {
      wxCommandEvent e;

      mSlider->Decrease(steps);
      SetMixer(e);
   }
}

void MeterPanel::Reset(double sampleRate, bool resetClipping)
{
   mT = 0;
   mRate = sampleRate;
   for (int j = 0; j < kMaxMeterBars; j++)
   {
      ResetBar(&mBar[j], resetClipping);
   }

   // wxTimers seem to be a little unreliable - sometimes they stop for
   // no good reason, so this "primes" it every now and then...
   mTimer.Stop();

   // While it's stopped, empty the queue
   mQueue.Clear();

   mLayoutValid = false;

   mTimer.Start(1000 / mMeterRefreshRate);

   Refresh(false);
}

static float floatMax(float a, float b)
{
   return a>b? a: b;
}

/* Unused as yet.
static int intmin(int a, int b)
{
   return a<b? a: b;
}
*/

static int intmax(int a, int b)
{
   return a>b? a: b;
}

static float ClipZeroToOne(float z)
{
   if (z > 1.0)
      return 1.0;
   else if (z < 0.0)
      return 0.0;
   else
      return z;
}

static float ToDB(float v, float range)
{
   double db;
   if (v > 0)
      db = LINEAR_TO_DB(fabs(v));
   else
      db = -999;
   return ClipZeroToOne((db + range) / range);
}

void MeterPanel::UpdateDisplay(
   unsigned numChannels, int numFrames, const float *sampleData)
{
   auto sptr = sampleData;
   auto num = std::min(numChannels, mNumBars);
   MeterUpdateMsg msg;

   memset(&msg, 0, sizeof(msg));
   msg.numFrames = numFrames;

   for(int i=0; i<numFrames; i++) {
      for(unsigned int j=0; j<num; j++) {
         msg.peak[j] = floatMax(msg.peak[j], fabs(sptr[j]));
         msg.rms[j] += sptr[j]*sptr[j];

         // In addition to looking for mNumPeakSamplesToClip peaked
         // samples in a row, also send the number of peaked samples
         // at the head and tail, in case there's a run of peaked samples
         // that crosses block boundaries
         if (fabs(sptr[j])>=MAX_AUDIO) {
            if (msg.headPeakCount[j]==i)
               msg.headPeakCount[j]++;
            msg.tailPeakCount[j]++;
            if (msg.tailPeakCount[j] > mNumPeakSamplesToClip)
               msg.clipping[j] = true;
         }
         else
            msg.tailPeakCount[j] = 0;
      }
      sptr += numChannels;
   }
   for(unsigned int j=0; j<mNumBars; j++)
      msg.rms[j] = sqrt(msg.rms[j]/numFrames);

   mQueue.Put(msg);
}

// Vaughan, 2010-11-29: This not currently used. See comments in MixerTrackCluster::UpdateMeter().
//void MeterPanel::UpdateDisplay(int numChannels, int numFrames,
//                           // Need to make these double-indexed arrays if we handle more than 2 channels.
//                           float* maxLeft, float* rmsLeft,
//                           float* maxRight, float* rmsRight,
//                           const size_t kSampleCount)
//{
//   int i, j;
//   int num = intmin(numChannels, mNumBars);
//   MeterUpdateMsg msg;
//
//   msg.numFrames = kSampleCount;
//   for(j=0; j<mNumBars; j++) {
//      msg.peak[j] = 0.0;
//      msg.rms[j] = 0.0;
//      msg.clipping[j] = false;
//      msg.headPeakCount[j] = 0;
//      msg.tailPeakCount[j] = 0;
//   }
//
//   for(i=0; i<numFrames; i++) {
//      for(j=0; j<num; j++) {
//         msg.peak[j] = floatMax(msg.peak[j], ((j == 0) ? maxLeft[i] : maxRight[i]));
//         msg.rms[j] = floatMax(msg.rms[j], ((j == 0) ? rmsLeft[i] : rmsRight[i]));
//
//         // In addition to looking for mNumPeakSamplesToClip peaked
//         // samples in a row, also send the number of peaked samples
//         // at the head and tail, in case there's a run
//         // of peaked samples that crosses block boundaries.
//         if (fabs((j == 0) ? maxLeft[i] : maxRight[i]) >= MAX_AUDIO)
//         {
//            if (msg.headPeakCount[j]==i)
//               msg.headPeakCount[j]++;
//            msg.tailPeakCount[j]++;
//            if (msg.tailPeakCount[j] > mNumPeakSamplesToClip)
//               msg.clipping[j] = true;
//         }
//         else
//            msg.tailPeakCount[j] = 0;
//      }
//   }
//
//   mQueue.Put(msg);
//}

void MeterPanel::OnMeterUpdate(wxTimerEvent & WXUNUSED(event))
{
   MeterUpdateMsg msg;
   int numChanges = 0;
#ifdef EXPERIMENTAL_AUTOMATED_INPUT_LEVEL_ADJUSTMENT
   double maxPeak = 0.0;
   bool discarded = false;
#endif

   // We shouldn't receive any events if the meter is disabled, but clear it to be safe
   if (mMeterDisabled) {
      mQueue.Clear();
      return;
   }


   // There may have been several update messages since the last
   // time we got to this function.  Catch up to real-time by
   // popping them off until there are none left.  It is necessary
   // to process all of them, otherwise we won't handle peaks and
   // peak-hold bars correctly.
   while(mQueue.Get(msg)) {
      numChanges++;
      double deltaT = msg.numFrames / mRate;

      mT += deltaT;
      for(unsigned int j=0; j<mNumBars; j++) {
         mBar[j].isclipping = false;

         //
         if (mDB) {
            msg.peak[j] = ToDB(msg.peak[j], mDBRange);
            msg.rms[j] = ToDB(msg.rms[j], mDBRange);
         }

         if (mDecay) {
            if (mDB) {
               float decayAmount = mDecayRate * deltaT / mDBRange;
               mBar[j].peak = floatMax(msg.peak[j],
                                       mBar[j].peak - decayAmount);
            }
            else {
               double decayAmount = mDecayRate * deltaT;
               double decayFactor = DB_TO_LINEAR(-decayAmount);
               mBar[j].peak = floatMax(msg.peak[j],
                                       mBar[j].peak * decayFactor);
            }
         }
         else
            mBar[j].peak = msg.peak[j];

         // This smooths out the RMS signal
         float smooth = pow(0.9, (double)msg.numFrames/1024.0);
         mBar[j].rms = mBar[j].rms * smooth + msg.rms[j] * (1.0 - smooth);

         if (mT - mBar[j].peakHoldTime > mPeakHoldDuration ||
             mBar[j].peak > mBar[j].peakHold) {
            mBar[j].peakHold = mBar[j].peak;
            mBar[j].peakHoldTime = mT;
         }

         if (mBar[j].peak > mBar[j].peakPeakHold )
            mBar[j].peakPeakHold = mBar[j].peak;

         if (msg.clipping[j] ||
             mBar[j].tailPeakCount+msg.headPeakCount[j] >=
             mNumPeakSamplesToClip){
            mBar[j].clipping = true;
            mBar[j].isclipping = true;
         }

         mBar[j].tailPeakCount = msg.tailPeakCount[j];
#ifdef EXPERIMENTAL_AUTOMATED_INPUT_LEVEL_ADJUSTMENT
         if (mT > gAudioIO->AILAGetLastDecisionTime()) {
            discarded = false;
            maxPeak = msg.peak[j] > maxPeak ? msg.peak[j] : maxPeak;
            wxPrintf("%f@%f ", msg.peak[j], mT);
         }
         else {
            discarded = true;
            wxPrintf("%f@%f discarded\n", msg.peak[j], mT);
         }
#endif
      }
   } // while

   if (numChanges > 0) {
      #ifdef EXPERIMENTAL_AUTOMATED_INPUT_LEVEL_ADJUSTMENT
         if (gAudioIO->AILAIsActive() && mIsInput && !discarded) {
            gAudioIO->AILAProcess(maxPeak);
            putchar('\n');
         }
      #endif
      RepaintBarsNow();
   }
}

void MeterPanel::OnTipTimeout(wxTimerEvent& evt)
{
   if(mSlider)
      mSlider->ShowTip(true);
}


float MeterPanel::GetMaxPeak() const
{
   float maxPeak = 0.;

   for(unsigned int j=0; j<mNumBars; j++)
      maxPeak = mBar[j].peak > maxPeak ? mBar[j].peak : maxPeak;

   return(maxPeak);
}

float MeterPanel::GetPeakHold() const
{
   auto peakHold = .0f;
   for (unsigned int i = 0; i < mNumBars; i++)
      peakHold = std::max(peakHold, mBar[i].peakPeakHold);
   return peakHold;
}

wxFont MeterPanel::GetFont() const
{
   int fontSize = 10;
#if defined __WXMSW__
   fontSize = 8;
#endif

   return wxFont(fontSize, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL);
}

void MeterPanel::ResetBar(MeterBar *b, bool resetClipping)
{
   b->peak = 0.0;
   b->rms = 0.0;
   b->peakHold = 0.0;
   b->peakHoldTime = 0.0;
   if (resetClipping)
   {
      b->clipping = false;
      b->peakPeakHold = 0.0;
   }
   b->isclipping = false;
   b->tailPeakCount = 0;
}

bool MeterPanel::IsActive() const
{
   return mActive;
}

bool MeterPanel::IsMonitoring() const
{
   return mMonitoring;
}

bool MeterPanel::IsClipping() const
{
   for (int c = 0; c < mNumBars; c++)
      if (mBar[c].clipping)
         return true;
   return false;
}

void MeterPanel::SetActiveStyle(Style newStyle)
{
   mStyle = newStyle;

   // Set dummy ruler bounds so width/height can be retrieved
   // NOTE: Make sure the Right and Bottom values are large enough to
   //       ensure full width/height of digits get calculated.
   mRuler.SetBounds(0, 0, 500, 500);

   if (mDB)
   {
      mRuler.SetFormat(Ruler::LinearDBFormat);
      if (mStyle == HorizontalStereo || mStyle == HorizontalStereoCompact)
      {
         mRuler.SetOrientation(wxHORIZONTAL);
         mRuler.SetRange(-mDBRange, 0);
      }
      else
      {
         mRuler.SetOrientation(wxVERTICAL);
         mRuler.SetRange(0, -mDBRange);
      }
   }
   else
   {
      mRuler.SetFormat(Ruler::RealFormat);
      if (mStyle == HorizontalStereo || mStyle == HorizontalStereoCompact)
      {
         mRuler.SetOrientation(wxHORIZONTAL);
         mRuler.SetRange(0, 1);
      }
      else
      {
         mRuler.SetOrientation(wxVERTICAL);
         mRuler.SetRange(1, 0);
      }
   }

   mRuler.GetMaxSize(&mRulerWidth, &mRulerHeight);
}

void MeterPanel::SetBarAndClip(int iBar, bool vert)
{
   // Save the orientation
   mBar[iBar].vert = vert;

   // Create the bar rectangle and educe to fit inside the bevel
   mBar[iBar].r = mBar[iBar].b;
   mBar[iBar].r.x += 1;
   mBar[iBar].r.width -= 1;
   mBar[iBar].r.y += 1;
   mBar[iBar].r.height -= 1;

   if (vert)
   {
      if (mClip)
      {
         // Create the clip rectangle
         mBar[iBar].rClip = mBar[iBar].b;
         mBar[iBar].rClip.height = 3;

         // Make room for the clipping indicator
         mBar[iBar].b.y += 3 + gap;
         mBar[iBar].b.height -= 3 + gap;
         mBar[iBar].r.y += 3 + gap;
         mBar[iBar].r.height -= 3 + gap;
      }
   }
   else
   {
      if (mClip)
      {
         // Make room for the clipping indicator
         mBar[iBar].b.width -= 4;
         mBar[iBar].r.width -= 4;

         // Create the indicator rectangle
         mBar[iBar].rClip = mBar[iBar].b;
         mBar[iBar].rClip.x = mBar[iBar].b.GetRight() + 1 + gap; // +1 for bevel
         mBar[iBar].rClip.width = 3;
      }
   }
}

void MeterPanel::HandleLayout(wxDC &dc)
{
   // Refresh to reflect any language changes
   /* i18n-hint: One-letter abbreviation for Left, in VU Meter */
   mLeftText = _("L");
   /* i18n-hint: One-letter abbreviation for Right, in VU Meter */
   mRightText = _("R");

   dc.SetFont(GetFont());
   int width = mWidth;
   int height = mHeight;
   int left = 0;
   int top = 0;
   int barw;
   int barh;
   int lside;
   int rside;

   // MixerTrackCluster has no L/R labels or icon
   if (mStyle != MixerTrackCluster)
   {
      if (mDesiredStyle == AutomaticStereo)
      {
         SetActiveStyle(width > height ? HorizontalStereo : VerticalStereo);
      }
   
      if (mStyle == HorizontalStereoCompact || mStyle == HorizontalStereo)
      {
         SetActiveStyle(height < 50 ? HorizontalStereoCompact : HorizontalStereo);
      }
      else if (mStyle == VerticalStereoCompact || mStyle == VerticalStereo)
      {
         SetActiveStyle(width < 100 ? VerticalStereoCompact : VerticalStereo);
      }
   
      if (mLeftSize.GetWidth() == 0)  // Not yet initialized to dc.
      {
         dc.GetTextExtent(mLeftText, &mLeftSize.x, &mLeftSize.y);
         dc.GetTextExtent(mRightText, &mRightSize.x, &mRightSize.y);
      }
   }

   int ltxtWidth = mLeftSize.GetWidth();
   int ltxtHeight = mLeftSize.GetHeight();
   int rtxtWidth = mRightSize.GetWidth();
   int rtxtHeight = mRightSize.GetHeight();

   switch (mStyle)
   {
   default:
      wxPrintf(wxT("Style not handled yet!\n"));
      break;
   case MixerTrackCluster:
      // width is now the entire width of the meter canvas
      width -= mRulerWidth + left;

      // height is now the entire height of the meter canvas
      height -= top + gap;

      // barw is half of the canvas while allowing for a gap between meters
      barw = (width - gap) / 2;

      // barh is now the height of the canvas
      barh = height;

      // We always have 2 bars
      mNumBars = 2;

      // Save dimensions of the left bevel
      mBar[0].b = wxRect(left, top, barw, barh);

      // Save dimensions of the right bevel
      mBar[1].b = mBar[0].b;
      mBar[1].b.SetLeft(mBar[0].b.GetRight() + 1 + gap); // +1 for right edge

      // Set bar and clipping indicator dimensions
      SetBarAndClip(0, true);
      SetBarAndClip(1, true);

      mRuler.SetBounds(mBar[1].r.GetRight() + 1,   // +1 for the bevel
                       mBar[1].r.GetTop(),
                       mWidth,
                       mBar[1].r.GetBottom());
      mRuler.OfflimitsPixels(0, 0);
      break;
   case VerticalStereo:
      // Determine required width of each side;
      lside = ltxtWidth + gap;
      rside = intmax(mRulerWidth, rtxtWidth);

      // left is now the right edge of the icon or L label
      left = lside;

      // Ensure there's a margin between top edge of window and the meters
      top = gap;

      // Position the L/R labels
      mLeftTextPos = wxPoint(left - ltxtWidth - gap, height - gap - ltxtHeight);
      mRightTextPos = wxPoint(width - rside - gap, height - gap - rtxtHeight);

      // left is now left edge of left bar
      left += gap;

      // width is now the entire width of the meter canvas
      width -= gap + rside + gap + left;

      // height is now the entire height of the meter canvas
      height -= top + gap;

      mSliderPos = wxPoint{ 0, top - gap };
      mSliderSize = wxSize{ width, height + 2 * gap };

      // barw is half of the canvas while allowing for a gap between meters
      barw = (width - gap) / 2;

      // barh is now the height of the canvas
      barh = height;

      // We always have 2 bars
      mNumBars = 2;

      // Save dimensions of the left bevel
      mBar[0].b = wxRect(left, top, barw, barh);

      // Save dimensions of the right bevel
      mBar[1].b = mBar[0].b;
      mBar[1].b.SetLeft(mBar[0].b.GetRight() + 1 + gap); // +1 for right edge

      // Set bar and clipping indicator dimensions
      SetBarAndClip(0, true);
      SetBarAndClip(1, true);

      mRuler.SetBounds(mBar[1].r.GetRight() + 1,   // +1 for the bevel
                       mBar[1].r.GetTop(),
                       mWidth,
                       mBar[1].r.GetBottom());
      mRuler.OfflimitsPixels(mRightTextPos.y - gap, mBar[1].r.GetBottom());
      break;
   case VerticalStereoCompact:
      // Ensure there's a margin between top edge of window and the meters
      top = gap;

      // height is now the entire height of the meter canvas
      height -= top + gap + ltxtHeight + gap;

      mSliderPos = wxPoint{ 0, top - gap };
      mSliderSize = wxSize{ width, height + 2 * gap };

      // barw is half of the canvas while allowing for a gap between meters
      barw = (width / 2) - gap;

      // barh is now the height of the canvas
      barh = height;

      // We always have 2 bars
      mNumBars = 2;

      // Save dimensions of the left bevel
      mBar[0].b = wxRect(left, top, barw, barh);

      // Save dimensions of the right bevel
      mBar[1].b = mBar[0].b;
      mBar[1].b.SetLeft(mBar[0].b.GetRight() + 1 + gap); // +1 for right edge

      // Set bar and clipping indicator dimensions
      SetBarAndClip(0, true);
      SetBarAndClip(1, true);

      // L/R is centered horizontally under each bar
      mLeftTextPos = wxPoint(mBar[0].b.GetLeft() + ((mBar[0].b.GetWidth() - ltxtWidth) / 2), top + barh + gap);
      mRightTextPos = wxPoint(mBar[1].b.GetLeft() + ((mBar[1].b.GetWidth() - rtxtWidth) / 2), top + barh + gap);

      mRuler.SetBounds((mWidth - mRulerWidth) / 2,
                       mBar[1].r.GetTop(),
                       (mWidth - mRulerWidth) / 2,
                       mBar[1].r.GetBottom());
      mRuler.OfflimitsPixels(0, 0);
      break;
   case HorizontalStereo:
      // Button right next to dragger.
      left = 0;

      // Add a gap between bottom of icon and bottom of window
      height -= gap;
      
      left = gap;

      // Make sure there's room for icon and gap between the bottom of the meter and icon
      height -= rtxtHeight + gap;

      // L/R is centered vertically and to the left of a each bar
      mLeftTextPos = wxPoint(left, (height / 4) - ltxtHeight / 2);
      mRightTextPos = wxPoint(left, (height * 3 / 4) - rtxtHeight / 2);

      // Add width of widest of the L/R characters
      left += intmax(ltxtWidth, rtxtWidth); //, iconWidth);

      mSliderPos = wxPoint{ left - gap, 0 };

      // Add gap between L/R and meter bevel
      left += gap;

      // width is now the entire width of the meter canvas
      width -= left;

      mSliderSize = wxSize{ width + 2 * gap, height };

      // barw is now the width of the canvas minus gap between canvas and right window edge
      barw = width - gap;

      // barh is half of the canvas while allowing for a gap between meters
      barh = (height - gap) / 2;

      // We always have 2 bars
      mNumBars = 2;

      // Save dimensions of the top bevel
      mBar[0].b = wxRect(left, top, barw, barh);

      // Save dimensions of the bottom bevel
      mBar[1].b = mBar[0].b;
      mBar[1].b.SetTop(mBar[0].b.GetBottom() + 1 + gap); // +1 for bottom edge

      // Set bar and clipping indicator dimensions
      SetBarAndClip(0, false);
      SetBarAndClip(1, false);

      mRuler.SetBounds(mBar[1].r.GetLeft(),
                       mBar[1].r.GetBottom() + 1, // +1 to fit below bevel
                       mBar[1].r.GetRight(),
                       mHeight - mBar[1].r.GetBottom() + 1);
      break;
   case HorizontalStereoCompact:
      left = gap;

      // L/R is centered vertically and to the left of a each bar
      mLeftTextPos = wxPoint(left, (height / 4) - (ltxtHeight / 2));
      mRightTextPos = wxPoint(left, (height * 3 / 4) - (ltxtHeight / 2));

      // Add width of widest of the L/R characters
      left += intmax(ltxtWidth, rtxtWidth);

      mSliderPos = wxPoint{ left - gap, 0 };

      // Add gap between L/R and meter bevel
      left += gap;

      // width is now the entire width of the meter canvas
      width -= left;

      mSliderSize = wxSize{ width + 2 * gap, height };

      // barw is now the width of the canvas minus gap between canvas and window edge
      barw = width - gap;

      // barh is half of the canvas while allowing for a gap between meters
      barh = (height - gap) / 2;

      // We always have 2 bars
      mNumBars = 2;

      // Save dimensions of the top bevel
      mBar[0].b = wxRect(left, top, barw, barh);

      // Save dimensions of the bottom bevel
      // Since the bars butt up against the window's top and bottom edges, we need
      // to include an extra pixel in the bottom bar when the window height and
      // meter height do not exactly match.
      mBar[1].b = mBar[0].b;
      mBar[1].b.SetTop(mBar[0].b.GetBottom() + 1 + gap); // +1 for bottom bevel
      mBar[1].b.SetHeight(mHeight - mBar[1].b.GetTop() - 1); // +1 for bottom bevel

      // Add clipping indicators - do after setting bar/bevel dimensions above
      SetBarAndClip(0, false);
      SetBarAndClip(1, false);

      mRuler.SetBounds(mBar[1].r.GetLeft(),
                       mBar[1].b.GetTop() - (mRulerHeight / 2),
                       mBar[1].r.GetRight(),
                       mBar[1].b.GetTop() - (mRulerHeight / 2));
      mRuler.OfflimitsPixels(0, 0);
      break;
   }

   mLayoutValid = true;
}

void MeterPanel::RepaintBarsNow()
{
   if (mLayoutValid)
   {
      // Invalidate the bars so they get redrawn
      for (unsigned int i = 0; i < mNumBars; i++)
      {
         Refresh(false);
      }

      // Immediate redraw (using wxPaintDC)
      Update();

      return;
   }
}

void MeterPanel::DrawMeterBar(wxDC &dc, MeterBar *bar)
{
   // Cache some metrics
   wxCoord x = bar->r.GetLeft();
   wxCoord y = bar->r.GetTop();
   wxCoord w = bar->r.GetWidth();
   wxCoord h = bar->r.GetHeight();
   wxCoord ht;
   wxCoord wd;

   // Setup for erasing the background
   dc.SetPen(*wxTRANSPARENT_PEN);
   dc.SetBrush(mMeterDisabled ? mDisabledBkgndBrush : mBkgndBrush);

   if (mGradient)
   {
      // Map the predrawn bitmap into the source DC
      wxMemoryDC srcDC;
      srcDC.SelectObject(*mBitmap);

      if (bar->vert)
      {
         // Copy as much of the predrawn meter bar as is required for the
         // current peak.
         // (h - 1) corresponds to the mRuler.SetBounds() in HandleLayout()
         ht = (int)(bar->peak * (h - 1) + 0.5);

         // Blank out the rest
         if (h - ht)
         {
            // ht includes peak value...not really needed but doesn't hurt
            dc.DrawRectangle(x, y, w, h - ht);
         }

         // Copy as much of the predrawn meter bar as is required for the
         // current peak.
         // +/-1 to include the peak position
         if (ht)
         {
            dc.Blit(x, y + h - ht - 1, w, ht + 1, &srcDC, x, y + h - ht - 1);
         }

         // Draw the "recent" peak hold line using the predrawn meter bar so that
         // it will be the same color as the original level.
         // (h - 1) corresponds to the mRuler.SetBounds() in HandleLayout()
         ht = (int)(bar->peakHold * (h - 1) + 0.5);
         if (ht > 1)
         {
            dc.Blit(x, y + h - ht - 1, w, 2, &srcDC, x, y + h - ht - 1);
         }

         // Draw the "maximum" peak hold line
         // (h - 1) corresponds to the mRuler.SetBounds() in HandleLayout()
         dc.SetPen(mPeakPeakPen);
         ht = (int)(bar->peakPeakHold * (h - 1) + 0.5);
         if (ht > 0)
         {
            AColor::Line(dc, x, y + h - ht - 1, x + w - 1, y + h - ht - 1);
            if (ht > 1)
            {
               AColor::Line(dc, x, y + h - ht, x + w - 1, y + h - ht);
            }
         }
      }
      else
      {
         // Calculate the peak position
         // (w - 1) corresponds to the mRuler.SetBounds() in HandleLayout()
         wd = (int)(bar->peak * (w - 1) + 0.5);

         // Blank out the rest
         if (w - wd)
         {
            // wd includes peak value...not really needed but doesn't hurt
            dc.DrawRectangle(x + wd, y, w - wd, h);
         }

         // Copy as much of the predrawn meter bar as is required for the
         // current peak.  But, only blit() if there's something to copy
         // to prevent display corruption.
         // +1 to include peak position
         if (wd)
         {
            dc.Blit(x, y, wd + 1, h, &srcDC, x, y);
         }

         // Draw the "recent" peak hold line using the predrawn meter bar so that
         // it will be the same color as the original level.
         // -1 to give a 2 pixel width
         wd = (int)(bar->peakHold * (w - 1) + 0.5);
         if (wd > 1)
         {
            dc.Blit(x + wd - 1, y, 2, h, &srcDC, x + wd, y);
         }

         // Draw the "maximum" peak hold line using a themed color
         // (w - 1) corresponds to the mRuler.SetBounds() in HandleLayout()
         dc.SetPen(mPeakPeakPen);
         wd = (int)(bar->peakPeakHold * (w - 1) + 0.5);
         if (wd > 0)
         {
            AColor::Line(dc, x + wd, y, x + wd, y + h - 1);
            if (wd > 1)
            {
               AColor::Line(dc, x + wd - 1, y, x + wd - 1, y + h - 1);
            }
         }
      }

      // No longer need the source DC, so unselect the predrawn bitmap
      srcDC.SelectObject(wxNullBitmap);
   }
   else
   {
      if (bar->vert)
      {
         // Calculate the peak position
         // (h - 1) corresponds to the mRuler.SetBounds() in HandleLayout()
         ht = (int)(bar->peak * (h - 1) + 0.5);

         // Blank out the rest
         if (h - ht)
         {
            // ht includes peak value...not really needed but doesn't hurt
            dc.DrawRectangle(x, y, w, h - ht);
         }

         // Draw the peak level
         // +/-1 to include the peak position
         dc.SetPen(*wxTRANSPARENT_PEN);
         dc.SetBrush(mMeterDisabled ? mDisabledBkgndBrush : mBrush);
         if (ht)
         {
            dc.DrawRectangle(x, y + h - ht - 1, w, ht + 1);
         }

         // Draw the "recent" peak hold line
         // (h - 1) corresponds to the mRuler.SetBounds() in HandleLayout()
         dc.SetPen(mPen);
         ht = (int)(bar->peakHold * (h - 1) + 0.5);
         if (ht > 0)
         {
            AColor::Line(dc, x, y + h - ht - 1, x + w - 1, y + h - ht - 1);
            if (ht > 1)
            {
               AColor::Line(dc, x, y + h - ht, x + w - 1, y + h - ht);
            }
         }

         // Calculate the rms position
         // (h - 1) corresponds to the mRuler.SetBounds() in HandleLayout()
         // +1 to include the rms position
         ht = (int)(bar->rms * (h - 1) + 0.5);

         // Draw the RMS level
         dc.SetPen(*wxTRANSPARENT_PEN);
         dc.SetBrush(mMeterDisabled ? mDisabledBkgndBrush : mRMSBrush);
         if (ht)
         {
            dc.DrawRectangle(x, y + h - ht - 1, w, ht + 1);
         }

         // Draw the "maximum" peak hold line
         // (h - 1) corresponds to the mRuler.SetBounds() in HandleLayout()
         dc.SetPen(mPeakPeakPen);
         ht = (int)(bar->peakPeakHold * (h - 1) + 0.5);
         if (ht > 0)
         {
            AColor::Line(dc, x, y + h - ht - 1, x + w - 1, y + h - ht - 1);
            if (ht > 1)
            {
               AColor::Line(dc, x, y + h - ht, x + w - 1, y + h - ht);
            }
         }
      }
      else
      {
         // Calculate the peak position
         // (w - 1) corresponds to the mRuler.SetBounds() in HandleLayout()
         wd = (int)(bar->peak * (w - 1) + 0.5);

         // Blank out the rest
         if (w - wd)
         {
            // wd includes peak value...not really needed but doesn't hurt
            dc.DrawRectangle(x + wd, y, w - wd, h);
         }

         // Draw the peak level
         // +1 to include peak position
         dc.SetPen(*wxTRANSPARENT_PEN);
         dc.SetBrush(mMeterDisabled ? mDisabledBkgndBrush : mBrush);
         if (wd)
         {
            dc.DrawRectangle(x, y, wd + 1, h);
         }

         // Draw the "recent" peak hold line
         // (w - 1) corresponds to the mRuler.SetBounds() in HandleLayout()
         dc.SetPen(mPen);
         wd = (int)(bar->peakHold * (w - 1) + 0.5);
         if (wd > 0)
         {
            AColor::Line(dc, x + wd, y, x + wd, y + h - 1);
            if (wd > 1)
            {
               AColor::Line(dc, x + wd - 1, y, x + wd - 1, y + h - 1);
            }
         }

         // Calculate the rms position
         // (w - 1) corresponds to the mRuler.SetBounds() in HandleLayout()
         wd = (int)(bar->rms * (w - 1) + 0.5);

         // Draw the rms level 
         // +1 to include the rms position
         dc.SetPen(*wxTRANSPARENT_PEN);
         dc.SetBrush(mMeterDisabled ? mDisabledBkgndBrush : mRMSBrush);
         if (wd)
         {
            dc.DrawRectangle(x, y, wd + 1, h);
         }

         // Draw the "maximum" peak hold line using a themed color
         // (w - 1) corresponds to the mRuler.SetBounds() in HandleLayout()
         dc.SetPen(mPeakPeakPen);
         wd = (int)(bar->peakPeakHold * (w - 1) + 0.5);
         if (wd > 0)
         {
            AColor::Line(dc, x + wd, y, x + wd, y + h - 1);
            if (wd > 1)
            {
               AColor::Line(dc, x + wd - 1, y, x + wd - 1, y + h - 1);
            }
         }
      }
   }

   // If meter had a clipping indicator, draw or erase it
   // LLL:  At least I assume that's what "mClip" is supposed to be for as
   //       it is always "true".
   if (mClip)
   {
      if (bar->clipping)
      {
         dc.SetBrush(mClipBrush);
      }
      else
      {
         dc.SetBrush(mMeterDisabled ? mDisabledBkgndBrush : mBkgndBrush);
      }
      dc.SetPen(*wxTRANSPARENT_PEN);
      wxRect r(bar->rClip.GetX() + 1,
               bar->rClip.GetY() + 1,
               bar->rClip.GetWidth() - 1,
               bar->rClip.GetHeight() - 1);
      dc.DrawRectangle(r);
   }
}

bool MeterPanel::IsMeterDisabled() const
{
   return mMeterDisabled != 0;
}

void MeterPanel::StartMonitoring()
{
   bool start = !mMonitoring;

   auto gAudioIO = AudioIO::Get();
   if (gAudioIO->IsMonitoring()){
      gAudioIO->StopStream();
   } 

   if (start && !gAudioIO->IsBusy()){
      AudacityProject *p = mProject;
      if (p)
         gAudioIO->StartMonitoring(ProjectAudioIO::GetDefaultOptions(*p));

      mLayoutValid = false;

      Refresh(false);
   }
}

void MeterPanel::StopMonitoring(){
   mMonitoring = false;
   auto gAudioIO = AudioIO::Get();
   if (gAudioIO->IsMonitoring()){
      gAudioIO->StopStream();
   } 
}

void MeterPanel::OnAudioIOStatus(AudioIOEvent evt)
{
   if (!mIsInput != (evt.type == AudioIOEvent::PLAYBACK))
      return;

   AudacityProject *p = evt.pProject;
   mActive = evt.on && (p == mProject);
   if( mActive ){
      mTimer.Start(1000 / mMeterRefreshRate);
      if (evt.type == AudioIOEvent::MONITOR)
         mMonitoring = mActive;
   } else {
      mTimer.Stop();
      mMonitoring = false;
   }

   // Only refresh is we're the active meter
   if (IsShownOnScreen())
      Refresh(false);
}

void MeterPanel::OnAudioCapture(AudioIOEvent event)
{
   if (event.type == AudioIOEvent::CAPTURE && event.pProject != mProject)
   {
      mEnabled = !event.on;

      if (mSlider)
         mSlider->SetEnabled(mEnabled);
   }
}

// SaveState() and RestoreState() exist solely for purpose of recreating toolbars
// They should really be querying the project for current audio I/O state, but there
// isn't a clear way of doing that just yet.  (It should NOT query AudioIO.)
auto MeterPanel::SaveState() -> State
{
   return { true, mMonitoring, mActive };
}

void MeterPanel::RestoreState(const State &state)
{
   if (!state.mSaved)
      return;

   mMonitoring = state.mMonitoring;
   mActive = state.mActive;
   //wxLogDebug("Restore state for %p, is %i", this, mActive );

   if (mActive)
      mTimer.Start(1000 / mMeterRefreshRate);
}

//
// Pop-up menu
//

void MeterPanel::ShowMenu(const wxPoint & pos)
{
   wxMenu menu;
   // Note: these should be kept in the same order as the enum
   if (mIsInput) {
      wxMenuItem *mi;
      if (mMonitoring)
         mi = menu.Append(OnMonitorID, _("Stop Monitoring"));
      else
         mi = menu.Append(OnMonitorID, _("Start Monitoring"));
      mi->Enable(!mActive || mMonitoring);
   }

   menu.Append(OnPreferencesID, _("Options..."));

   BasicMenu::Handle{ &menu }.Popup(
      wxWidgetsWindowPlacement{ this },
      { pos.x, pos.y }
   );
}

void MeterPanel::SetName(const TranslatableString& tip)
{
   wxPanelWrapper::SetName(tip);
   if(mSlider)
      mSlider->SetName(tip);
}


void MeterPanel::OnMonitor(wxCommandEvent & WXUNUSED(event))
{
   StartMonitoring();
}

void MeterPanel::OnPreferences(wxCommandEvent & WXUNUSED(event))
{
   wxTextCtrl *rate;
   wxRadioButton *gradient;
   wxRadioButton *rms;
   wxRadioButton *db;
   wxRadioButton *linear;
   wxRadioButton *automatic;
   wxRadioButton *horizontal;
   wxRadioButton *vertical;
   int meterRefreshRate = mMeterRefreshRate;

   auto title = mIsInput ? XO("Recording Meter Options") : XO("Playback Meter Options");

   // Dialog is a child of the project, rather than of the toolbar.
   // This determines where it pops up.

   wxDialogWrapper dlg( FindProjectFrame( mProject ), wxID_ANY, title);
   dlg.SetName();
   ShuttleGui S(&dlg, eIsCreating);
   S.StartVerticalLay();
   {
      S.StartStatic(XO("Refresh Rate"), 0);
      {
         S.AddFixedText(XO(
"Higher refresh rates make the meter show more frequent\nchanges. A rate of 30 per second or less should prevent\nthe meter affecting audio quality on slower machines."));
         S.StartHorizontalLay();
         {
            rate = S.Name(XO("Meter refresh rate per second [1-100]"))
               .Validator<IntegerValidator<long>>(
                  &mMeterRefreshRate, NumValidatorStyle::DEFAULT,
                  MIN_REFRESH_RATE, MAX_REFRESH_RATE)
               .AddTextBox(XXO("Meter refresh rate per second [1-100]: "),
                                wxString::Format(wxT("%d"), meterRefreshRate),
                                10);
         }
         S.EndHorizontalLay();
      }
      S.EndStatic();

      S.StartHorizontalLay();
      {
        S.StartStatic(XO("Meter Style"), 0);
        {
           S.StartVerticalLay();
           {
              gradient = S.AddRadioButton(XXO("Gradient"), true, mGradient);
              rms = S.AddRadioButtonToGroup(XXO("RMS"), false, mGradient);
           }
           S.EndVerticalLay();
        }
        S.EndStatic();

        S.StartStatic(XO("Meter Type"), 0);
        {
           S.StartVerticalLay();
           {
              db = S.AddRadioButton(XXO("dB"), true, mDB);
              linear = S.AddRadioButtonToGroup(XXO("Linear"), false, mDB);
           }
           S.EndVerticalLay();
        }
        S.EndStatic();

        S.StartStatic(XO("Orientation"), 1);
        {
           S.StartVerticalLay();
           {
              automatic = S.AddRadioButton(
                  XXO("Automatic"), AutomaticStereo, mDesiredStyle);
              horizontal = S.AddRadioButtonToGroup(
                  XXO("Horizontal"), HorizontalStereo, mDesiredStyle);
              vertical = S.AddRadioButtonToGroup(
                  XXO("Vertical"), VerticalStereo, mDesiredStyle);
           }
           S.EndVerticalLay();
        }
        S.EndStatic();
      }
      S.EndHorizontalLay();
      S.AddStandardButtons();
   }
   S.EndVerticalLay();
   dlg.Layout();
   dlg.Fit();

   dlg.CenterOnParent();

   if (dlg.ShowModal() == wxID_OK)
   {
      wxArrayStringEx style{
         wxT("AutomaticStereo") ,
         wxT("HorizontalStereo") ,
         wxT("VerticalStereo") ,
      };

      int s = 0;
      s = automatic->GetValue() ? 0 : s;
      s = horizontal->GetValue() ? 1 : s;
      s = vertical->GetValue() ? 2 : s;

      gPrefs->Write(Key(wxT("Style")), style[s]);
      gPrefs->Write(Key(wxT("Bars")), gradient->GetValue() ? wxT("Gradient") : wxT("RMS"));
      gPrefs->Write(Key(wxT("Type")), db->GetValue() ? wxT("dB") : wxT("Linear"));
      gPrefs->Write(Key(wxT("RefreshRate")), rate->GetValue());

      gPrefs->Flush();

      // Currently, there are 2 playback meters and 2 record meters and any number of 
      // mixerboard meters, so we have to send out an preferences updated message to
      // ensure they all update themselves.
      PrefsListener::Broadcast(MeterPrefsID());
   }
}

wxString MeterPanel::Key(const wxString & key) const
{
   if (mStyle == MixerTrackCluster)
   {
      return wxT("/Meter/Mixerboard/") + key;
   }

   if (mIsInput)
   {
      return wxT("/Meter/Input/") + key;
   }

   return wxT("/Meter/Output/") + key;
}

// This compensates for a but in wxWidgets 3.0.2 for mac:
// Couldn't set focus from keyboard when AcceptsFocus returns false;
// this bypasses that limitation
void MeterPanel::SetFocusFromKbd()
{
   auto temp = TemporarilyAllowFocus();
   SetFocus();
}


#if wxUSE_ACCESSIBILITY

MeterAx::MeterAx(wxWindow *window):
   WindowAccessible(window)
{
}

MeterAx::~MeterAx()
{
}

// Performs the default action. childId is 0 (the action for this object)
// or > 0 (the action for a child).
// Return wxACC_NOT_SUPPORTED if there is no default action for this
// window (e.g. an edit control).
wxAccStatus MeterAx::DoDefaultAction(int WXUNUSED(childId))
{
   MeterPanel *m = wxDynamicCast(GetWindow(), MeterPanel);

   if (m && m->mIsInput)
      m->StartMonitoring();

   return wxACC_OK;
}

// Retrieves the address of an IDispatch interface for the specified child.
// All objects must support this property.
wxAccStatus MeterAx::GetChild(int childId, wxAccessible** child)
{
   if (childId == wxACC_SELF)
      *child = this;
   else
      *child = NULL;
   return wxACC_OK;
}

// Gets the number of children.
wxAccStatus MeterAx::GetChildCount(int* childCount)
{
   *childCount = 0;
   return wxACC_OK;
}

// Gets the default action for this object (0) or > 0 (the action for
// a child).  Return wxACC_OK even if there is no action. actionName
// is the action, or the empty string if there is no action.  The
// retrieved string describes the action that is performed on an
// object, not what the object does as a result. For example, a
// toolbar button that prints a document has a default action of
// "Press" rather than "Prints the current document."
wxAccStatus MeterAx::GetDefaultAction(int WXUNUSED(childId), wxString* actionName)
{
   *actionName = _("Press");
   return wxACC_OK;
}

// Returns the description for this object or a child.
wxAccStatus MeterAx::GetDescription(int WXUNUSED(childId), wxString *description)
{
   description->clear();
   return wxACC_NOT_SUPPORTED;
}

// Gets the window with the keyboard focus.
// If childId is 0 and child is NULL, no object in
// this subhierarchy has the focus.
// If this object has the focus, child should be 'this'.
wxAccStatus MeterAx::GetFocus(int* childId, wxAccessible** child)
{
   *childId = 0;
   *child = this;
   return wxACC_OK;
}

// Returns help text for this object or a child, similar to tooltip text.
wxAccStatus MeterAx::GetHelpText(int WXUNUSED(childId), wxString *helpText)
{
   helpText->clear();
   return wxACC_NOT_SUPPORTED;
}

// Returns the keyboard shortcut for this object or child.
// Return e.g. ALT+K
wxAccStatus MeterAx::GetKeyboardShortcut(int WXUNUSED(childId), wxString *shortcut)
{
   shortcut->clear();
   return wxACC_OK;
}

// Returns the rectangle for this object (id = 0) or a child element (id > 0).
// rect is in screen coordinates.
wxAccStatus MeterAx::GetLocation(wxRect & rect, int WXUNUSED(elementId))
{
   MeterPanel *m = wxDynamicCast(GetWindow(), MeterPanel);

   rect = m->GetClientRect();
   rect.SetPosition(m->ClientToScreen(rect.GetPosition()));

   return wxACC_OK;
}

// Returns a role constant.
wxAccStatus MeterAx::GetRole(int WXUNUSED(childId), wxAccRole* role)
{
      *role = wxROLE_SYSTEM_SLIDER;

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
wxAccStatus MeterAx::GetSelections(wxVariant * WXUNUSED(selections))
{
   return wxACC_NOT_IMPLEMENTED;
}

// Returns a state constant.
wxAccStatus MeterAx::GetState(int WXUNUSED(childId), long* state)
{
   MeterPanel *m = wxDynamicCast( GetWindow(), MeterPanel );

   *state = wxACC_STATE_SYSTEM_FOCUSABLE;
   *state |= ( m == wxWindow::FindFocus() ? wxACC_STATE_SYSTEM_FOCUSED : 0 );

   return wxACC_OK;
}

// Returns a localized string representing the value for the object
// or child.
wxAccStatus MeterAx::GetValue(int WXUNUSED(childId), wxString* strValue)
{
   MeterPanel *m = wxDynamicCast(GetWindow(), MeterPanel);

   *strValue = m->mSlider->GetStringValue();
   return wxACC_OK;
}

#endif
