/**********************************************************************

  Audacity: A Digital Audio Editor

  PeakAndRmsMeter.cpp

  Dominic Mazzoni
  Vaughan Johnson

  Paul Licameli split from MeterPanel.cpp

*********************************************************************/
#include "PeakAndRmsMeter.h"

#include <wx/string.h>

#include <math.h>

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

PeakAndRmsMeter::PeakAndRmsMeter(int dbRange, float decayRate)
   : mDecayRate{ decayRate }
   , mDBRange{ dbRange }
{}

PeakAndRmsMeter::~PeakAndRmsMeter() = default;

void PeakAndRmsMeter::Clear()
{
   mQueue.Clear();
}

#if 0
void MeterPanel::UpdatePrefs()
{
   mDBRange = DecibelScaleCutoff.Read();

   mMeterRefreshRate =
      std::max(MIN_REFRESH_RATE, std::min(MAX_REFRESH_RATE,
         gPrefs->Read(Key(wxT("RefreshRate")), 30L)));
   mGradient = gPrefs->Read(Key(wxT("Bars")), wxT("Gradient")) == wxT("Gradient");
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
      //if( !mMeterDisabled )
      //{
      //   mBkgndBrush.SetColour( GetParent()->GetBackgroundColour() );
      //}
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
      mRuler.Draw(dc);

      // Bitmap created...unselect
      dc.SelectObject(wxNullBitmap);
   }

   // Copy predrawn bitmap to the dest DC
   destDC.DrawBitmap(*mBitmap, 0, 0);

   // Go draw the meter bars, Left & Right channels using current levels
   for (unsigned int i = 0; i < mNumBars; i++)
   {
      DrawMeterBar(destDC, mBar[i], mStats[i]);
   }

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
#endif

void PeakAndRmsMeter::Reset(double sampleRate, bool resetClipping)
{
   mT = 0;
   mRate = sampleRate;
   for (int j = 0; j < kMaxMeterBars; j++)
      mStats[j].Reset(resetClipping);
   mQueue.Clear();
}

static float ToDB(float v, float range)
{
   double db;
   if (v > 0)
      db = LINEAR_TO_DB(fabs(v));
   else
      db = -999;
   return std::clamp((db + range) / range, 0.0, 1.0);
}

void PeakAndRmsMeter::Update(unsigned numChannels,
   unsigned long numFrames, const float *sampleData, bool interleaved)
{
   auto sptr = sampleData;
   const auto majorStep = (interleaved ? numChannels : 1);
   const auto minorStep = (interleaved ? 1 : numFrames);
   auto num = std::min(numChannels, mNumBars);
   MeterUpdateMsg msg;

   memset(&msg, 0, sizeof(msg));
   msg.numFrames = numFrames;

   for (size_t i = 0; i < numFrames; ++i, sptr += majorStep) {
      for (size_t j = 0; j < num; ++j) {
         const auto sample = sptr[j * minorStep];
         msg.peak[j] = std::max(msg.peak[j], fabs(sample));
         msg.rms[j] += sample * sample;

         // In addition to looking for mNumPeakSamplesToClip peaked
         // samples in a row, also send the number of peaked samples
         // at the head and tail, in case there's a run of peaked samples
         // that crosses block boundaries
         if (fabs(sample) >= MAX_AUDIO) {
            if (msg.headPeakCount[j] == i)
               ++msg.headPeakCount[j];
            ++msg.tailPeakCount[j];
            if (msg.tailPeakCount[j] > mNumPeakSamplesToClip)
               msg.clipping[j] = true;
         }
         else
            msg.tailPeakCount[j] = 0;
      }
   }
   for (unsigned int j = 0; j < mNumBars; ++j) {
      auto &rms = msg.rms[j];
      rms = sqrt(rms / numFrames);
   }

   mQueue.Put(msg);
}

void PeakAndRmsMeter::Poll()
{
   MeterUpdateMsg msg;
   unsigned numChanges = 0;

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
   while (mQueue.Get(msg)) {
      ++numChanges;
      double deltaT = msg.numFrames / mRate;

      mT += deltaT;
      for (size_t j = 0; j < mNumBars; ++j) {
         auto &stats = mStats[j];
         if (mDB) {
            msg.peak[j] = ToDB(msg.peak[j], mDBRange);
            msg.rms[j] = ToDB(msg.rms[j], mDBRange);
         }

         if (mDecay) {
            if (mDB) {
               float decayAmount = mDecayRate * deltaT / mDBRange;
               stats.peak = std::max(msg.peak[j], stats.peak - decayAmount);
            }
            else {
               double decayAmount = mDecayRate * deltaT;
               double decayFactor = DB_TO_LINEAR(-decayAmount);
               stats.peak =
                  std::max<float>(msg.peak[j], stats.peak * decayFactor);
            }
         }
         else
            stats.peak = msg.peak[j];

         // This smooths out the RMS signal
         float smooth = pow(0.9, (double)msg.numFrames / 1024.0);
         stats.rms = stats.rms * smooth + msg.rms[j] * (1.0 - smooth);

         if (mT - stats.peakHoldTime > mPeakHoldDuration ||
             stats.peak > stats.peakHold) {
            stats.peakHold = stats.peak;
            stats.peakHoldTime = mT;
         }

         if (stats.peak > stats.peakPeakHold )
            stats.peakPeakHold = stats.peak;

         if (msg.clipping[j] ||
             stats.tailPeakCount + msg.headPeakCount[j] >=
             mNumPeakSamplesToClip){
            stats.clipping = true;
         }

         stats.tailPeakCount = msg.tailPeakCount[j];
         Receive(mT, msg);
      }
   } // while
}

void PeakAndRmsMeter::Receive(double, const MeterUpdateMsg &)
{
}

bool PeakAndRmsMeter::IsClipping() const
{
   for (int c = 0; c < mNumBars; c++)
      if (mStats[c].clipping)
         return true;
   return false;
}

int PeakAndRmsMeter::GetDBRange() const
{
   return mDB ? mDBRange : -1;
}

#if 0
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
      wxPanelWrapper::Update();

      return;
   }
}
#endif

bool PeakAndRmsMeter::IsDisabled() const
{
   return mMeterDisabled != 0;
}
