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

*//******************************************************************/
#include "MeterPanel.h"

#include <algorithm>
#include <wx/setup.h> // for wxUSE_* macros
#include <wx/wxcrtvararg.h>
#include <wx/defs.h>
#include <wx/dcbuffer.h>
#include <wx/frame.h>
#include <wx/menu.h>
#include <wx/textdlg.h>
#include <wx/numdlg.h>
#include <wx/radiobut.h>
#include <wx/tooltip.h>

#include "AColor.h"
#include "AILA.h"
#include "AudioIO.h"
#include "../widgets/BasicMenu.h"
#include "ImageManipulation.h"
#include "Decibels.h"
#include "LinearUpdater.h"
#include "Project.h"
#include "ProjectAudioIO.h"
#include "ProjectStatus.h"
#include "../ProjectWindows.h"
#include "Prefs.h"
#include "ShuttleGui.h"
#include "Theme.h"
#include "wxWidgetsWindowPlacement.h"
#include "../widgets/LinearUpdater.h"
#include "../widgets/LinearDBFormat.h"
#include "../widgets/RealFormat.h"

#include "AllThemeResources.h"
#include "../widgets/valnum.h"

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

//
// MeterPanel class
//

#include "../../images/SpeakerMenu.xpm"
#include "../../images/MicMenu.xpm"

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
             Style style /*= HorizontalStereo*/)
: MeterPanelBase(parent, id, pos, size, wxTAB_TRAVERSAL | wxNO_BORDER | wxWANTS_CHARS),
   mProject(project),
   mWidth(size.x),
   mHeight(size.y),
   mIsInput(isInput),
   mDesiredStyle(style),
   mRate(0),
   mMonitoring(false),
   mActive(false),
   mPainter{ true, true, isInput, clrMedium },
   mLayoutValid(false),
   mRuler{ LinearUpdater::Instance(), LinearDBFormat::Instance() }
, PeakAndRmsMeter{ DecibelScaleCutoff.Read() }
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
   SetBackgroundColour( backgroundColour );

   mAudioIOStatusSubscription = AudioIO::Get()
      ->Subscribe(*this, &MeterPanel::OnAudioIOStatus);

   mAudioCaptureSubscription = AudioIO::Get()
      ->Subscribe(*this, &MeterPanel::OnAudioCapture);

   mTipTimer.SetOwner(this, OnTipTimeoutID);
   mTimer.SetOwner(this, OnMeterUpdateID);
   // TODO: Yikes.  Hard coded sample rate.
   // JKC: I've looked at this, and it's benignish.  It just means that the meter
   // balistics are right for 44KHz and a bit more frisky than they should be
   // for higher sample rates.
   Reset(44100.0, true);
   
   MeterPanelBase::Init(this);
}

void MeterPanel::UpdatePrefs()
{
   mDBRange = DecibelScaleCutoff.Read();

   mMeterRefreshRate =
      std::max(MIN_REFRESH_RATE, std::min(MAX_REFRESH_RATE,
         gPrefs->Read(Key(wxT("RefreshRate")), 30L)));
   mPainter.SetGradient(
      gPrefs->Read(Key(wxT("Bars")), wxT("Gradient")) == wxT("Gradient"));
   mDB = gPrefs->Read(Key(wxT("Type")), wxT("dB")) == wxT("dB");
   mMeterDisabled = gPrefs->Read(Key(wxT("Disabled")), 0L);

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
   // Show or hide the input slider based on whether it works
   auto gAudioIO = AudioIO::Get();
   if (mIsInput && mSlider)
      mSlider->SetEnabled(mEnabled && gAudioIO->InputMixerWorks());
   const auto [inputSource, inputVolume, playbackVolume] =
      gAudioIO->GetMixer();
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
      // Go calculate all of the layout metrics
      HandleLayout();
   
      // Create a new one using current size and select into the DC
      mPainter.AllocateBitmap(destDC, mWidth, mHeight);
      auto pBitmap = mPainter.GetBitmap();
      assert(pBitmap);

      wxMemoryDC dc;
      auto &bitmap = *pBitmap;
      dc.SelectObject(bitmap);

      // Paint text on the bitmap
      // MixerTrackCluster style has no icon or L/R labels
      if (mStyle != MixerTrackCluster)
      {
         dc.SetFont(GetFont());
         dc.SetTextForeground( clrText );
         dc.SetTextBackground( clrBoxFill );
         dc.DrawText(mLeftText, mLeftTextPos.x, mLeftTextPos.y);
         dc.DrawText(mRightText, mRightTextPos.x, mRightTextPos.y);
      }

      // Draw the meter bars at maximum levels
      dc.SelectObject(wxNullBitmap);
      for (unsigned int i = 0; i < mNumBars; ++i)
         mPainter.FillBitmap(mBar[i], mDB, mDBRange);
      dc.SelectObject(bitmap);

      // Paint a ruler on the bitmap, though for some styles this gets partly
      // overpainted in DrawMeterBar, so must be painted again
      mRuler.SetTickColour( clrText );
      dc.SetTextForeground( clrText );
      // Draw the ruler
      mRuler.Draw(dc);
   }

   // Copy predrawn bitmap to the dest DC
   destDC.DrawBitmap(*mPainter.GetBitmap(), 0, 0);

   // Go draw the meter bars, Left & Right channels using current levels
   for (unsigned int i = 0; i < mNumBars; i++)
      mPainter.DrawMeterBar(destDC, mMeterDisabled, mBar[i], mStats[i]);

   destDC.SetTextForeground( clrText );

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
   if (mSlider) {
      Refresh();
      auto gAudioIO = AudioIO::Get();
      auto settings = gAudioIO->GetMixer();
      auto &[inputSource, inputVolume, outputVolume] = settings;
      if (mIsInput)
         inputVolume = mSlider->Get();
      else
         outputVolume = mSlider->Get();
      gAudioIO->SetMixer(settings);

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
   mRate = sampleRate;

   // wxTimers seem to be a little unreliable - sometimes they stop for
   // no good reason, so this "primes" it every now and then...
   mTimer.Stop();

   // While it's stopped, empty the queue
   PeakAndRmsMeter::Reset(sampleRate, resetClipping);

   mLayoutValid = false;

   mTimer.Start(1000 / mMeterRefreshRate);

   Refresh(false);
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

void MeterPanel::Receive(
   [[maybe_unused]] double time,
   [[maybe_unused]] const MeterUpdateMsg &msg)
{
   ++mNumChanges;
#ifdef EXPERIMENTAL_AUTOMATED_INPUT_LEVEL_ADJUSTMENT
   for (size_t j = 0; j < mNumBars; ++j) {
      if (time > AILA::Get().GetLastDecisionTime()) {
         mDiscarded = false;
         mMaxPeak = std::max<double>(msg.peak[j], mMaxPeak);
         wxPrintf("%f@%f ", msg.peak[j], time);
      }
      else {
         mDiscarded = true;
         wxPrintf("%f@%f discarded\n", msg.peak[j], time);
      }
   }
#endif
}

void MeterPanel::OnMeterUpdate(wxTimerEvent &)
{
   mMaxPeak = 0.0;
   mNumChanges = 0;
   mDiscarded = false;

   Poll();
   if (mNumChanges > 0) {
#ifdef EXPERIMENTAL_AUTOMATED_INPUT_LEVEL_ADJUSTMENT
      if (AILA::Get().IsActive() && mIsInput && !mDiscarded) {
         AILA::Get().Process(mProject, IsClipping(), GetDBRange(), mMaxPeak);
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

float MeterPanel::GetPeakHold() const
{
   auto peakHold = .0f;
   for (unsigned int i = 0; i < mNumBars; i++)
      peakHold = std::max(peakHold, mStats[i].peakPeakHold);
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

bool MeterPanel::IsActive() const
{
   return mActive;
}

bool MeterPanel::IsMonitoring() const
{
   return mMonitoring;
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
      mRuler.SetFormat(&LinearDBFormat::Instance());
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
      mRuler.SetFormat(&RealFormat::LinearInstance());
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

void MeterPanel::HandleLayout()
{
   // Needed only for text size computations
   wxMemoryDC dc;

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

   const auto clip = mPainter.GetClip();
   auto &mBar0 = mBar[0];
   auto &mBar1 = mBar[1];
   switch (mStyle)
   {
   default:
      wxPrintf(wxT("Style not handled yet!\n"));
      break;
   case MixerTrackCluster: {
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
      const auto bevel0 = wxRect(left, top, barw, barh);

      // Save dimensions of the right bevel
      auto bevel1 = bevel0;
      bevel1.SetLeft(bevel0.GetRight() + 1 + gap); // +1 for right edge

      // Set bar and clipping indicator dimensions
      mBar0.SetRectangles(bevel0, true, clip);
      mBar1.SetRectangles(bevel1, true, clip);

      mRuler.SetBounds(mBar1.rect.GetRight() + 1,   // +1 for the bevel
                       mBar1.rect.GetTop(),
                       mWidth,
                       mBar1.rect.GetBottom());
      mRuler.OfflimitsPixels(0, 0);
      break;
   }
   case VerticalStereo: {
      // Determine required width of each side;
      lside = ltxtWidth + gap;
      rside = std::max(mRulerWidth, rtxtWidth);

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
      const auto bevel0 = wxRect(left, top, barw, barh);

      // Save dimensions of the right bevel
      auto bevel1 = bevel0;
      bevel1.SetLeft(bevel0.GetRight() + 1 + gap); // +1 for right edge

      // Set bar and clipping indicator dimensions
      mBar0.SetRectangles(bevel0, true, clip);
      mBar1.SetRectangles(bevel1, true, clip);

      mRuler.SetBounds(mBar1.rect.GetRight() + 1,   // +1 for the bevel
                       mBar1.rect.GetTop(),
                       mWidth,
                       mBar1.rect.GetBottom());
      mRuler.OfflimitsPixels(mRightTextPos.y - gap, mBar1.rect.GetBottom());
      break;
   }
   case VerticalStereoCompact: {
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
      const auto bevel0 = wxRect(left, top, barw, barh);

      // Save dimensions of the right bevel
      auto bevel1 = bevel0;
      bevel1.SetLeft(bevel0.GetRight() + 1 + gap); // +1 for right edge

      // Set bar and clipping indicator dimensions
      mBar0.SetRectangles(bevel0, true, clip);
      mBar1.SetRectangles(bevel1, true, clip);

      // L/R is centered horizontally under each bar
      mLeftTextPos = wxPoint(mBar0.bevel.GetLeft() + ((mBar0.bevel.GetWidth() - ltxtWidth) / 2), top + barh + gap);
      mRightTextPos = wxPoint(mBar1.bevel.GetLeft() + ((mBar1.bevel.GetWidth() - rtxtWidth) / 2), top + barh + gap);

      mRuler.SetBounds((mWidth - mRulerWidth) / 2,
                       mBar1.rect.GetTop(),
                       (mWidth - mRulerWidth) / 2,
                       mBar1.rect.GetBottom());
      mRuler.OfflimitsPixels(0, 0);
      break;
   }
   case HorizontalStereo: {
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
      left += std::max(ltxtWidth, rtxtWidth); //, iconWidth);

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
      const auto bevel0 = wxRect(left, top, barw, barh);

      // Save dimensions of the bottom bevel
      auto bevel1 = bevel0;
      bevel1.SetTop(bevel0.GetBottom() + 1 + gap); // +1 for bottom edge

      // Set bar and clipping indicator dimensions
      mBar0.SetRectangles(bevel0, false, clip);
      mBar1.SetRectangles(bevel1, false, clip);

      mRuler.SetBounds(mBar1.rect.GetLeft(),
                       mBar1.rect.GetBottom() + 1, // +1 to fit below bevel
                       mBar1.rect.GetRight(),
                       mHeight - mBar1.rect.GetBottom() + 1);
      break;
   }
   case HorizontalStereoCompact: {
      left = gap;

      // L/R is centered vertically and to the left of a each bar
      mLeftTextPos = wxPoint(left, (height / 4) - (ltxtHeight / 2));
      mRightTextPos = wxPoint(left, (height * 3 / 4) - (ltxtHeight / 2));

      // Add width of widest of the L/R characters
      left += std::max(ltxtWidth, rtxtWidth);

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
      const auto bevel0 = wxRect(left, top, barw, barh);

      // Save dimensions of the bottom bevel
      // Since the bars butt up against the window's top and bottom edges, we need
      // to include an extra pixel in the bottom bar when the window height and
      // meter height do not exactly match.
      auto bevel1 = bevel0;
      bevel1.SetTop(bevel0.GetBottom() + 1 + gap); // +1 for bottom bevel
      bevel1.SetHeight(mHeight - bevel1.GetTop() - 1); // +1 for bottom bevel

      // Add clipping indicators - do after setting bar/bevel dimensions above
      mBar0.SetRectangles(bevel0, false, clip);
      mBar1.SetRectangles(bevel1, false, clip);

      mRuler.SetBounds(mBar1.rect.GetLeft(),
                       mBar1.bevel.GetTop() - (mRulerHeight / 2),
                       mBar1.rect.GetRight(),
                       mBar1.bevel.GetTop() - (mRulerHeight / 2));
      mRuler.OfflimitsPixels(0, 0);
      break;
   }
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
      wxPanelWrapper::Update();

      return;
   }
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

void MeterPanel::OnAudioIOStatus(const AudioIOEvent &evt)
{
   const auto interested =
      mIsInput ? (evt.Capturing() || evt.Monitoring()) : evt.Playing();
   if (!interested)
      return;

   const auto p = evt.wProject.lock().get();
   mActive = evt.Starting() && (p == mProject);
   if (mActive) {
      mTimer.Start(1000 / mMeterRefreshRate);
      if (evt.Monitoring())
         mMonitoring = mActive;
   } else {
      mTimer.Stop();
      mMonitoring = false;
   }

   // Only refresh is we're the active meter
   if (IsShownOnScreen())
      Refresh(false);
}

void MeterPanel::OnAudioCapture(const AudioIOEvent &evt)
{
   if (evt.Capturing() && evt.wProject.lock().get() != mProject)
   {
      mEnabled = evt.Stopping();
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
         mi = menu.Append(OnMonitorID, _("Disable Silent Monitoring"));
      else
         mi = menu.Append(OnMonitorID, _("Enable Silent Monitoring"));
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
              const bool grad = mPainter.GetGradient();
              gradient =
                 S.AddRadioButton(XXO("Gradient"), true, grad);
              rms =
                 S.AddRadioButtonToGroup(XXO("RMS"), false, grad);
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
