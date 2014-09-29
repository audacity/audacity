/**********************************************************************

  Audacity: A Digital Audio Editor

  MixerBoard.cpp

  Vaughan Johnson, January 2007
  Dominic Mazzoni

**********************************************************************/

#include "Audacity.h"

#include "Experimental.h"

#include <math.h>

#include <wx/dcmemory.h>
#include <wx/arrimpl.cpp>
#include <wx/settings.h> // for wxSystemSettings::GetColour and wxSystemSettings::GetMetric

#include "AColor.h"
#include "MixerBoard.h"
#ifdef EXPERIMENTAL_MIDI_OUT
   #include "NoteTrack.h"
#endif
#include "Project.h"
#include "Track.h"


#include "../images/MusicalInstruments.h"
#ifdef __WXMSW__
   #include "../images/AudacityLogo.xpm"
#else
   #include "../images/AudacityLogo48x48.xpm"
#endif


// class MixerTrackSlider

BEGIN_EVENT_TABLE(MixerTrackSlider, ASlider)
   EVT_MOUSE_EVENTS(MixerTrackSlider::OnMouseEvent)

   EVT_SET_FOCUS(MixerTrackSlider::OnFocus)
   EVT_KILL_FOCUS(MixerTrackSlider::OnFocus)
   EVT_COMMAND(wxID_ANY, EVT_CAPTURE_KEY, MixerTrackSlider::OnCaptureKey)

END_EVENT_TABLE()

MixerTrackSlider::MixerTrackSlider(wxWindow * parent,
                                    wxWindowID id,
                                    wxString name,
                                    const wxPoint & pos,
                                    const wxSize & size,
                                    int style /*= FRAC_SLIDER*/,
                                    bool popup /*= true*/,
                                    bool canUseShift /*= true*/,
                                    float stepValue /*= STEP_CONTINUOUS*/,
                                    int orientation /*= wxHORIZONTAL*/)
: ASlider(parent, id, name, pos, size,
            style, popup, canUseShift, stepValue, orientation)
{
}

void MixerTrackSlider::OnMouseEvent(wxMouseEvent &event)
{
   ASlider::OnMouseEvent(event);

   if (event.ButtonUp())
   {
      MixerTrackCluster* pMixerTrackCluster = (MixerTrackCluster*)(this->GetParent());
      switch (mStyle)
      {
      case DB_SLIDER: pMixerTrackCluster->HandleSliderGain(true); break;
      case PAN_SLIDER: pMixerTrackCluster->HandleSliderPan(true); break;
      default: break; // no-op
      }
   }
}

void MixerTrackSlider::OnFocus(wxFocusEvent &event)
{
   wxCommandEvent e(EVT_CAPTURE_KEYBOARD);

   if (event.GetEventType() == wxEVT_KILL_FOCUS) {
      e.SetEventType(EVT_RELEASE_KEYBOARD);
   }
   e.SetEventObject(this);
   GetParent()->GetEventHandler()->ProcessEvent(e);

   Refresh(false);

   event.Skip();
}

void MixerTrackSlider::OnCaptureKey(wxCommandEvent &event)
{
   wxKeyEvent *kevent = (wxKeyEvent *)event.GetEventObject();
   int keyCode = kevent->GetKeyCode();

   // Pass LEFT/RIGHT/UP/DOWN/PAGEUP/PAGEDOWN through for input/output sliders
   if (keyCode == WXK_LEFT || keyCode == WXK_RIGHT ||
       keyCode == WXK_UP || keyCode == WXK_DOWN ||
       keyCode == WXK_PAGEUP || keyCode == WXK_PAGEDOWN) {
      return;
   }

   event.Skip();

   return;
}



// class MixerTrackCluster

#define kInset             4
#define kDoubleInset       (2 * kInset)
#define kTripleInset       (3 * kInset)
#define kQuadrupleInset    (4 * kInset)

#define TRACK_NAME_HEIGHT                    18
#define MUSICAL_INSTRUMENT_HEIGHT_AND_WIDTH  48
#define MUTE_SOLO_HEIGHT                     16
#define PAN_HEIGHT                           24

#define kLeftSideStackWidth         MUSICAL_INSTRUMENT_HEIGHT_AND_WIDTH - kDoubleInset //vvv Change when numbers shown on slider scale.
#define kRightSideStackWidth        MUSICAL_INSTRUMENT_HEIGHT_AND_WIDTH + kDoubleInset
#define kMixerTrackClusterWidth     kLeftSideStackWidth + kRightSideStackWidth + kQuadrupleInset // kDoubleInset margin on both sides

enum {
   ID_BITMAPBUTTON_MUSICAL_INSTRUMENT = 13000,
   ID_SLIDER_PAN,
   ID_SLIDER_GAIN,
   ID_TOGGLEBUTTON_MUTE,
   ID_TOGGLEBUTTON_SOLO,
};

BEGIN_EVENT_TABLE(MixerTrackCluster, wxPanel)
   EVT_CHAR(MixerTrackCluster::OnKeyEvent)
   EVT_MOUSE_EVENTS(MixerTrackCluster::OnMouseEvent)
   EVT_PAINT(MixerTrackCluster::OnPaint)

   EVT_BUTTON(ID_BITMAPBUTTON_MUSICAL_INSTRUMENT, MixerTrackCluster::OnButton_MusicalInstrument)
   EVT_SLIDER(ID_SLIDER_PAN, MixerTrackCluster::OnSlider_Pan)
   EVT_SLIDER(ID_SLIDER_GAIN, MixerTrackCluster::OnSlider_Gain)
   //v EVT_COMMAND_SCROLL(ID_SLIDER_GAIN, MixerTrackCluster::OnSliderScroll_Gain)
   EVT_COMMAND(ID_TOGGLEBUTTON_MUTE, wxEVT_COMMAND_BUTTON_CLICKED, MixerTrackCluster::OnButton_Mute)
   EVT_COMMAND(ID_TOGGLEBUTTON_SOLO, wxEVT_COMMAND_BUTTON_CLICKED, MixerTrackCluster::OnButton_Solo)
END_EVENT_TABLE()

MixerTrackCluster::MixerTrackCluster(wxWindow* parent,
                                       MixerBoard* grandParent, AudacityProject* project,
                                       WaveTrack* pLeftTrack, WaveTrack* pRightTrack /*= NULL*/,
                                       const wxPoint& pos /*= wxDefaultPosition*/,
                                       const wxSize& size /*= wxDefaultSize*/)
: wxPanel(parent, -1, pos, size)
{
   mMixerBoard = grandParent;
   mProject = project;
#ifdef EXPERIMENTAL_MIDI_OUT
   if (pLeftTrack->GetKind() == Track::Note) {
      mLeftTrack = NULL;
      mNoteTrack = (NoteTrack*) pLeftTrack;
      mTrack = pLeftTrack;
   } else {
      wxASSERT(pLeftTrack->GetKind() == Track::Wave);
      mTrack = mLeftTrack = pLeftTrack;
      mNoteTrack = NULL;
   }
#else
   wxASSERT(pLeftTrack->GetKind() == Track::Wave);
   mLeftTrack = pLeftTrack;
#endif
   mRightTrack = pRightTrack;

   SetName(mLeftTrack->GetName());

   this->SetBackgroundColour(wxSystemSettings::GetColour(wxSYS_COLOUR_3DFACE));

   // Not sure why, but sizers weren't getting offset vertically,
   // probably because not using wxDefaultPosition,
   // so positions are calculated explicitly below, and sizers code was removed.
   // (Still available in Audacity_UmixIt branch off 1.2.6.)

   // track name
   wxPoint ctrlPos(kDoubleInset, kDoubleInset);
   wxSize ctrlSize(size.GetWidth() - kQuadrupleInset, TRACK_NAME_HEIGHT);
   mStaticText_TrackName =
      #ifdef EXPERIMENTAL_MIDI_OUT
         new wxStaticText(this, -1, mTrack->GetName(), ctrlPos, ctrlSize,
                           wxALIGN_CENTRE | wxST_NO_AUTORESIZE | wxSUNKEN_BORDER);
      #else
         new wxStaticText(this, -1, mLeftTrack->GetName(), ctrlPos, ctrlSize,
                           wxALIGN_CENTRE | 0x0001 | wxBORDER_SUNKEN);
      #endif
   //v Useful when different tracks are different colors, but not now.
   //    mStaticText_TrackName->SetBackgroundColour(this->GetTrackColor());


   // gain slider at left
   ctrlPos.x = kDoubleInset;
   ctrlPos.y += TRACK_NAME_HEIGHT + kDoubleInset;
   const int nGainSliderHeight =
      size.GetHeight() - ctrlPos.y - kQuadrupleInset;
   ctrlSize.Set(kLeftSideStackWidth - kQuadrupleInset, nGainSliderHeight);
#ifdef EXPERIMENTAL_MIDI_OUT
   if (mNoteTrack) {
      mSlider_Gain =
         new MixerTrackSlider(
               this, ID_SLIDER_GAIN,
               /* i18n-hint: title of the MIDI Velocity slider */
               _("Velocity"),
               ctrlPos, ctrlSize, VEL_SLIDER, true,
               true, 0.0, wxVERTICAL);
   } else
#endif
   mSlider_Gain =
      new MixerTrackSlider(
            this, ID_SLIDER_GAIN,
            /* i18n-hint: title of the Gain slider, used to adjust the volume */
            _("Gain"),
            ctrlPos, ctrlSize, DB_SLIDER, true,
            true, 0.0, wxVERTICAL);
   mSlider_Gain->SetName(_("Gain"));

   this->UpdateGain();


   // other controls and meter at right

   // musical instrument image
   ctrlPos.x += kLeftSideStackWidth + kInset; // + kInset to center it in right side stack
   ctrlSize.Set(MUSICAL_INSTRUMENT_HEIGHT_AND_WIDTH, MUSICAL_INSTRUMENT_HEIGHT_AND_WIDTH);
#ifdef EXPERIMENTAL_MIDI_OUT
   wxBitmap* bitmap = mMixerBoard->GetMusicalInstrumentBitmap(mTrack->GetName());
#else
   wxBitmap* bitmap = mMixerBoard->GetMusicalInstrumentBitmap(mLeftTrack);
#endif
   wxASSERT(bitmap);
   mBitmapButton_MusicalInstrument =
      new wxBitmapButton(this, ID_BITMAPBUTTON_MUSICAL_INSTRUMENT, *bitmap,
                           ctrlPos, ctrlSize,
                           wxBU_AUTODRAW, wxDefaultValidator,
                           _("Musical Instrument"));
   mBitmapButton_MusicalInstrument->SetName(_("Musical Instrument"));


   // pan slider
   ctrlPos.x -= kInset; // Remove inset for instrument, so Pan is at leftmost of left side stack.
   ctrlPos.y += MUSICAL_INSTRUMENT_HEIGHT_AND_WIDTH + kDoubleInset;
   ctrlSize.Set(kRightSideStackWidth, PAN_HEIGHT);

   // The width of the pan slider must be odd (don't ask).
   if (!(ctrlSize.x & 1))
      ctrlSize.x--;

   mSlider_Pan =
      new MixerTrackSlider(
            this, ID_SLIDER_PAN,
            /* i18n-hint: Title of the Pan slider, used to move the sound left or right */
            _("Pan"),
            ctrlPos, ctrlSize, PAN_SLIDER, true);
   mSlider_Pan->SetName(_("Pan"));

   this->UpdatePan();

   // mute/solo buttons stacked below Pan slider
   ctrlPos.y += PAN_HEIGHT + kDoubleInset;
   ctrlSize.Set(mMixerBoard->mMuteSoloWidth, MUTE_SOLO_HEIGHT);
   mToggleButton_Mute =
      new AButton(this, ID_TOGGLEBUTTON_MUTE,
                  ctrlPos, ctrlSize,
                  *(mMixerBoard->mImageMuteUp), *(mMixerBoard->mImageMuteOver),
                  *(mMixerBoard->mImageMuteDown), *(mMixerBoard->mImageMuteDisabled),
                  true); // toggle button
   mToggleButton_Mute->SetName(_("Mute"));
   mToggleButton_Mute->SetAlternateImages(
      *(mMixerBoard->mImageMuteUp), *(mMixerBoard->mImageMuteOver),
      *(mMixerBoard->mImageMuteDown), *(mMixerBoard->mImageMuteDisabled));
   this->UpdateMute();

   ctrlPos.y += MUTE_SOLO_HEIGHT;
   mToggleButton_Solo =
      new AButton(this, ID_TOGGLEBUTTON_SOLO,
                  ctrlPos, ctrlSize,
                  *(mMixerBoard->mImageSoloUp), *(mMixerBoard->mImageSoloOver),
                  *(mMixerBoard->mImageSoloDown), *(mMixerBoard->mImageSoloDisabled),
                  true); // toggle button
   mToggleButton_Solo->SetName(_("Solo"));
   this->UpdateSolo();
   bool bSoloNone = mProject->IsSoloNone();
   mToggleButton_Solo->Show(!bSoloNone);


   // meter
   ctrlPos.y += (bSoloNone ? 0 : MUTE_SOLO_HEIGHT) + kDoubleInset;
   const int nMeterHeight =
      nGainSliderHeight -
      (MUSICAL_INSTRUMENT_HEIGHT_AND_WIDTH + kDoubleInset) -
      (PAN_HEIGHT + kDoubleInset) -
      (MUTE_SOLO_HEIGHT + (bSoloNone ? 0 : MUTE_SOLO_HEIGHT) + kDoubleInset);
   ctrlSize.Set(kRightSideStackWidth, nMeterHeight);
#ifdef EXPERIMENTAL_MIDI_OUT
   if (mLeftTrack) {
#endif
   mMeter =
      new Meter(this, -1, // wxWindow* parent, wxWindowID id,
                false, // bool isInput
                ctrlPos, ctrlSize, // const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize,
                Meter::MixerTrackCluster); // Style style = HorizontalStereo,
   mMeter->SetName(_("Signal Level Meter"));
#ifdef EXPERIMENTAL_MIDI_OUT
   } else {
      mMeter = NULL;
   }
#endif

   #if wxUSE_TOOLTIPS
    #ifdef EXPERIMENTAL_MIDI_OUT
      mStaticText_TrackName->SetToolTip(mTrack->GetName());
    #else
      mStaticText_TrackName->SetToolTip(mLeftTrack->GetName());
    #endif
      mToggleButton_Mute->SetToolTip(_("Mute"));
      mToggleButton_Solo->SetToolTip(_("Solo"));
    #ifdef EXPERIMENTAL_MIDI_OUT
     if (mLeftTrack)
    #endif
      mMeter->SetToolTip(_("Signal Level Meter"));
   #endif // wxUSE_TOOLTIPS

   #ifdef __WXMAC__
      wxSizeEvent dummyEvent;
      this->OnSize(dummyEvent);
      UpdateGain();
   #endif
}

void MixerTrackCluster::HandleResize() // For wxSizeEvents, update gain slider and meter.
{
   wxSize scrolledWindowClientSize = this->GetParent()->GetClientSize();
   const int newClusterHeight =
      scrolledWindowClientSize.GetHeight() - kDoubleInset - // nClusterHeight from MixerBoard::UpdateTrackClusters
      wxSystemSettings::GetMetric(wxSYS_HSCROLL_Y) + // wxScrolledWindow::GetClientSize doesn't account for its scrollbar size.
      kDoubleInset;

   this->SetSize(-1, newClusterHeight);

   // Change only the heights of mSlider_Gain and mMeter.
   // But update shown status of mToggleButton_Solo, which affects top of mMeter.
   const int nGainSliderHeight =
      newClusterHeight -
      (kInset + // margin above mStaticText_TrackName
         TRACK_NAME_HEIGHT + kDoubleInset) - // mStaticText_TrackName + margin
      kQuadrupleInset; // margin below gain slider
   mSlider_Gain->SetSize(-1, nGainSliderHeight);

   bool bSoloNone = mProject->IsSoloNone();

   mToggleButton_Solo->Show(!bSoloNone);

   const int nRequiredHeightAboveMeter =
      MUSICAL_INSTRUMENT_HEIGHT_AND_WIDTH + kDoubleInset +
      PAN_HEIGHT + kDoubleInset +
      MUTE_SOLO_HEIGHT + (bSoloNone ? 0 : MUTE_SOLO_HEIGHT) + kDoubleInset;
   const int nMeterY =
      kDoubleInset + // margin at top
      TRACK_NAME_HEIGHT + kDoubleInset +
      nRequiredHeightAboveMeter;
   const int nMeterHeight = nGainSliderHeight - nRequiredHeightAboveMeter;
   #ifdef EXPERIMENTAL_MIDI_OUT
   if (mMeter)
   #endif
   mMeter->SetSize(-1, nMeterY, -1, nMeterHeight);
}

void MixerTrackCluster::HandleSliderGain(const bool bWantPushState /*= false*/)
{
   float fValue = mSlider_Gain->Get();
   if (mLeftTrack)
      mLeftTrack->SetGain(fValue);
#ifdef EXPERIMENTAL_MIDI_OUT
   else
      mNoteTrack->SetGain(fValue);
#endif
   if (mRightTrack)
      mRightTrack->SetGain(fValue);

   // Update the TrackPanel correspondingly.
#ifdef EXPERIMENTAL_MIDI_OUT
   mProject->RefreshTPTrack(mTrack);
#else
   mProject->RefreshTPTrack(mLeftTrack);
#endif
   if (bWantPushState)
      mProject->TP_PushState(_("Moved gain slider"), _("Gain"), PUSH_CONSOLIDATE );
}

void MixerTrackCluster::HandleSliderPan(const bool bWantPushState /*= false*/)
{
   float fValue = mSlider_Pan->Get();
   if (mLeftTrack) // test in case track is a NoteTrack
      mLeftTrack->SetPan(fValue);
   if (mRightTrack)
      mRightTrack->SetPan(fValue);

   // Update the TrackPanel correspondingly.
#ifdef EXPERIMENTAL_MIDI_OUT
   mProject->RefreshTPTrack(mTrack);
#else
   mProject->RefreshTPTrack(mLeftTrack);
#endif

   if (bWantPushState)
      mProject->TP_PushState(_("Moved pan slider"), _("Pan"), PUSH_CONSOLIDATE );
}

void MixerTrackCluster::ResetMeter(const bool bResetClipping)
{
#ifdef EXPERIMENTAL_MIDI_OUT
   if (mMeter)
#endif
       mMeter->Reset(mLeftTrack->GetRate(), bResetClipping);
}


// These are used by TrackPanel for synchronizing control states, etc.

// Update the controls that can be affected by state change.
void MixerTrackCluster::UpdateForStateChange()
{
   this->UpdateName();
   this->UpdatePan();
   this->UpdateGain();
}

void MixerTrackCluster::UpdateName()
{
#ifdef EXPERIMENTAL_MIDI_OUT
   const wxString newName = mTrack->GetName();
#else
   const wxString newName = mLeftTrack->GetName();
#endif
   SetName(newName);
   mStaticText_TrackName->SetLabel(newName);
   #if wxUSE_TOOLTIPS
      mStaticText_TrackName->SetToolTip(newName);
   #endif
   mBitmapButton_MusicalInstrument->SetBitmapLabel(
#ifdef EXPERIMENTAL_MIDI_OUT
      *(mMixerBoard->GetMusicalInstrumentBitmap(newName)));
#else
      *(mMixerBoard->GetMusicalInstrumentBitmap(mLeftTrack)));
#endif
}

void MixerTrackCluster::UpdateMute()
{
#ifdef EXPERIMENTAL_MIDI_OUT
   mToggleButton_Mute->SetAlternate(mTrack->GetSolo());
   if (mTrack->GetMute())
#else
   mToggleButton_Mute->SetAlternate(mLeftTrack->GetSolo());
   if (mLeftTrack->GetMute())
#endif
      mToggleButton_Mute->PushDown();
   else
      mToggleButton_Mute->PopUp();
}

void MixerTrackCluster::UpdateSolo()
{
#ifdef EXPERIMENTAL_MIDI_OUT
   bool bIsSolo = mTrack->GetSolo();
#else
   bool bIsSolo = mLeftTrack->GetSolo();
#endif
   if (bIsSolo)
      mToggleButton_Solo->PushDown();
   else
      mToggleButton_Solo->PopUp();
   mToggleButton_Mute->SetAlternate(bIsSolo);
}

void MixerTrackCluster::UpdatePan()
{
#ifdef EXPERIMENTAL_MIDI_OUT
   if (mNoteTrack) {
      mSlider_Pan->Hide();
      return;
   }
#endif
   mSlider_Pan->Set(mLeftTrack->GetPan());
}

void MixerTrackCluster::UpdateGain()
{
#ifdef EXPERIMENTAL_MIDI_OUT
   if (mNoteTrack) {
      mSlider_Gain->SetStyle(VEL_SLIDER);
      mSlider_Gain->Set(mNoteTrack->GetGain());
      return;
   }
   mSlider_Gain->SetStyle(DB_SLIDER);
#endif
   mSlider_Gain->Set(mLeftTrack->GetGain());
}

void MixerTrackCluster::UpdateMeter(const double t0, const double t1)
{
#ifdef EXPERIMENTAL_MIDI_OUT
   // NoteTracks do not (currently) register on meters. It would probably be
   // a good idea to display 16 channel "active" lights rather than a meter
   if (!mLeftTrack)
      return;
#else
   wxASSERT(mLeftTrack && (mLeftTrack->GetKind() == Track::Wave));
#endif

   //vvv Vaughan, 2010-11-27:
   // NOTE TO ROGER DANNENBERG:
   // I undid the mTrack hack in this conditional, as the rest of the method still assumed it's a wavetrack
   // so dereferencing mLeftTrack would have gotten a NULL pointer fault.
   // I really think MixerTrackCluster should be factored for NoteTracks.
   // REPLY: I think bSuccess prevents dereferencing mLeftTrack, but I will
   // check. We should talk about whether it's better to factor
   // MixerTrackCluster or more fully hide track types from MixerTrackCluster.
   // For now, out change plan produced the following:
   // Vaughan, 2011=10-15: There's no bSuccess here, so I don't know what you mean.
   //    But this change is consistent with the others for EXPERIMENTAL_MIDI_OUT, so I accept it.
   if ((t0 < 0.0) || (t1 < 0.0) || (t0 >= t1) || // bad time value or nothing to show
#ifdef EXPERIMENTAL_MIDI_OUT
         ((mMixerBoard->HasSolo() || mTrack->GetMute()) && !mTrack->GetSolo()))
#else
         ((mMixerBoard->HasSolo() || mLeftTrack->GetMute()) && !mLeftTrack->GetSolo()))
#endif
   {
      //v Vaughan, 2011-02-25: Moved the update back to TrackPanel::OnTimer() as it helps with
      //    playback issues reported by Bill and noted on Bug 258, so no assert.
      // Vaughan, 2011-02-04: Now that we're updating all meters from audacityAudioCallback,
      //    this causes an assert if you click Mute while playing, because ResetMeter() resets
      //    the timer, and wxTimerbase says that can only be done from main thread --
      //    but it seems to work fine.
      this->ResetMeter(false);
      return;
   }

   // Vaughan, 2010-11-27:
   // This commented out code is flawed. Mistaken understanding of "frame" vs "window".
   // Caused me to override Meter::UpdateDisplay().
   // But I think it's got a good idea, of calling WaveTracks' GetMinMax and GetRMS
   // instead of passing in all the data and asking the meter to derive peak and rms.
   // May be worth revisiting as I think it should perform better, because it uses the min/max/rms
   // stored in blockfiles, rather than calculating them, but for now, changing it to use the
   // original Meter::UpdateDisplay(). New code is below the previous (now commented out).
   //
   //const int kFramesPerBuffer = 4;
   //float min; // dummy, since it's not shown in meters
   //float* maxLeft = new float[kFramesPerBuffer];
   //float* rmsLeft = new float[kFramesPerBuffer];
   //float* maxRight = new float[kFramesPerBuffer];
   //float* rmsRight = new float[kFramesPerBuffer];
   //
   //#ifdef EXPERIMENTAL_MIDI_OUT
   //   bool bSuccess = (mLeftTrack != NULL);
   //#else
   //   bool bSuccess = true;
   //#endif

   //const double dFrameInterval = (t1 - t0) / (double)kFramesPerBuffer;
   //double dFrameT0 = t0;
   //double dFrameT1 = t0 + dFrameInterval;
   //int i = 0;
   //while (bSuccess && (i < kFramesPerBuffer))
   //{
   //   bSuccess &=
   //      mLeftTrack->GetMinMax(&min, &(maxLeft[i]), dFrameT0, dFrameT1) &&
   //      mLeftTrack->GetRMS(&(rmsLeft[i]), dFrameT0, dFrameT1);
   //   if (bSuccess && mRightTrack)
   //      bSuccess &=
   //         mRightTrack->GetMinMax(&min, &(maxRight[i]), dFrameT0, dFrameT1) &&
   //         mRightTrack->GetRMS(&(rmsRight[i]), dFrameT0, dFrameT1);
   //   else
   //   {
   //      // Mono: Start with raw values same as left.
   //      // To be modified by bWantPostFadeValues and channel pan/gain.
   //      maxRight[i] = maxLeft[i];
   //      rmsRight[i] = rmsLeft[i];
   //   }
   //   dFrameT0 += dFrameInterval;
   //   dFrameT1 += dFrameInterval;
   //   i++;
   //}
   //
   //const bool bWantPostFadeValues = true; //v Turn this into a checkbox on MixerBoard? For now, always true.
   //if (bSuccess && bWantPostFadeValues)
   //if (bSuccess)
   //{
   //   for (i = 0; i < kFramesPerBuffer; i++)
   //   {
   //      float gain = mLeftTrack->GetChannelGain(0);
   //      maxLeft[i] *= gain;
   //      rmsLeft[i] *= gain;
   //      if (mRightTrack)
   //         gain = mRightTrack->GetChannelGain(1);
   //      maxRight[i] *= gain;
   //      rmsRight[i] *= gain;
   //   }
   //   mMeter->UpdateDisplay(
   //      2, // If mono, show left track values in both meters, as in MeterToolBar, rather than kNumChannels.
   //      kFramesPerBuffer,
   //      maxLeft, rmsLeft,
   //      maxRight, rmsRight,
   //      mLeftTrack->TimeToLongSamples(t1 - t0));
   //}
   //
   //delete[] maxLeft;
   //delete[] rmsLeft;
   //delete[] maxRight;
   //delete[] rmsRight;

   sampleCount startSample = (sampleCount)((mLeftTrack->GetRate() * t0) + 0.5);
   sampleCount nFrames = (sampleCount)((mLeftTrack->GetRate() * (t1 - t0)) + 0.5);
   float* meterFloatsArray = NULL;
   float* tempFloatsArray = new float[nFrames];
   bool bSuccess = mLeftTrack->Get((samplePtr)tempFloatsArray, floatSample, startSample, nFrames);
   if (bSuccess)
   {
      // We always pass a stereo sample array to the meter, as it shows 2 channels.
      // Mono shows same in both meters.
      // Since we're not mixing, need to duplicate same signal for "right" channel in mono case.
      meterFloatsArray = new float[2 * nFrames];

      // Interleave for stereo. Left/mono first.
      for (int index = 0; index < nFrames; index++)
         meterFloatsArray[2 * index] = tempFloatsArray[index];

      if (mRightTrack)
         bSuccess = mRightTrack->Get((samplePtr)tempFloatsArray, floatSample, startSample, nFrames);

      if (bSuccess)
         // Interleave right channel, or duplicate same signal for "right" channel in mono case.
         for (int index = 0; index < nFrames; index++)
            meterFloatsArray[(2 * index) + 1] = tempFloatsArray[index];
   }

   //const bool bWantPostFadeValues = true; //v Turn this into a checkbox on MixerBoard? For now, always true.
   //if (bSuccess && bWantPostFadeValues)
   if (bSuccess)
   {
      //vvv Need to apply envelope, too? See Mixer::MixSameRate.
      float gain = mLeftTrack->GetChannelGain(0);
      if (gain < 1.0)
         for (int index = 0; index < nFrames; index++)
            meterFloatsArray[2 * index] *= gain;
      if (mRightTrack)
         gain = mRightTrack->GetChannelGain(1);
      else
         gain = mLeftTrack->GetChannelGain(1);
      if (gain < 1.0)
         for (int index = 0; index < nFrames; index++)
            meterFloatsArray[(2 * index) + 1] *= gain;
      // Clip to [-1.0, 1.0] range.
      for (int index = 0; index < 2 * nFrames; index++)
         if (meterFloatsArray[index] < -1.0)
            meterFloatsArray[index] = -1.0;
         else if (meterFloatsArray[index] > 1.0)
            meterFloatsArray[index] = 1.0;

      mMeter->UpdateDisplay(2, nFrames, meterFloatsArray);
   }
   else
      this->ResetMeter(false);

   delete[] meterFloatsArray;
   delete[] tempFloatsArray;
}

// private

wxColour MixerTrackCluster::GetTrackColor()
{
   //#if (AUDACITY_BRANDING == BRAND_UMIXIT)
   //   return AColor::GetTrackColor((void*)mLeftTrack);
   //#else
      return wxColour(102, 255, 102); // same as Meter playback color
   //#endif
}


// event handlers

void MixerTrackCluster::HandleSelect(const bool bShiftDown)
{
   if (bShiftDown)
   {
      // ShiftDown => Just toggle selection on this track.
#ifdef EXPERIMENTAL_MIDI_OUT
      bool bSelect = !mTrack->GetSelected();
      mTrack->SetSelected(bSelect);
#else
      bool bSelect = !mLeftTrack->GetSelected();
      mLeftTrack->SetSelected(bSelect);
#endif
      if (mRightTrack)
         mRightTrack->SetSelected(bSelect);

      // Refresh only this MixerTrackCluster and WaveTrack in TrackPanel.
      this->Refresh(true);
#ifdef EXPERIMENTAL_MIDI_OUT
      mProject->RefreshTPTrack(mTrack);
#else
      mProject->RefreshTPTrack(mLeftTrack);
#endif
   }
   else
   {
      // exclusive select
      mProject->SelectNone();
#ifdef EXPERIMENTAL_MIDI_OUT
      mTrack->SetSelected(true);
#else
      mLeftTrack->SetSelected(true);
#endif
      if (mRightTrack)
         mRightTrack->SetSelected(true);

      if (mProject->GetSel0() >= mProject->GetSel1())
      {
         // No range previously selected, so use the range of this track.
#ifdef EXPERIMENTAL_MIDI_OUT
         mProject->mViewInfo.sel0 = mTrack->GetOffset();
         mProject->mViewInfo.sel1 = mTrack->GetEndTime();
#else
         mProject->mViewInfo.sel0 = mLeftTrack->GetOffset();
         mProject->mViewInfo.sel1 = mLeftTrack->GetEndTime();
#endif
      }

      // Exclusive select, so refresh all MixerTrackClusters.
      //    This could just be a call to wxWindow::Refresh, but this is
      //    more efficient and when ProjectLogo is shown as background,
      //    it's necessary to prevent blinking.
      mMixerBoard->RefreshTrackClusters(false);
   }
}

void MixerTrackCluster::OnKeyEvent(wxKeyEvent & event)
{
   mProject->HandleKeyDown(event);
}

void MixerTrackCluster::OnMouseEvent(wxMouseEvent& event)
{
   if (event.ButtonUp())
      this->HandleSelect(event.ShiftDown());
   else
      event.Skip();
}

void MixerTrackCluster::OnPaint(wxPaintEvent & WXUNUSED(event))
{
   wxPaintDC dc(this);

   #ifdef __WXMAC__
      // Fill with correct color, not scroller background. Done automatically on Windows.
      AColor::Medium(&dc, false);
      dc.DrawRectangle(this->GetClientRect());
   #endif

   wxSize clusterSize = this->GetSize();
   wxRect bev(0, 0, clusterSize.GetWidth() - 1, clusterSize.GetHeight() - 1);

#ifdef EXPERIMENTAL_MIDI_OUT
   if (mTrack->GetSelected())
#else
   if (mLeftTrack->GetSelected())
#endif
   {
      for (unsigned int i = 0; i < 4; i++) // 4 gives a big bevel, but there were complaints about visibility otherwise.
      {
         bev.Inflate(-1, -1);
         AColor::Bevel(dc, false, bev);
      }
   }
   else
      AColor::Bevel(dc, true, bev);
}


void MixerTrackCluster::OnButton_MusicalInstrument(wxCommandEvent& WXUNUSED(event))
{
   bool bShiftDown = ::wxGetMouseState().ShiftDown();
   this->HandleSelect(bShiftDown);
}

void MixerTrackCluster::OnSlider_Gain(wxCommandEvent& WXUNUSED(event))
{
   this->HandleSliderGain();
}

//v void MixerTrackCluster::OnSliderScroll_Gain(wxScrollEvent& WXUNUSED(event))
//{
   //int sliderValue = (int)(mSlider_Gain->Get()); //v mSlider_Gain->GetValue();
   //#ifdef __WXMSW__
   //   // Negate because wxSlider on Windows has min at top, max at bottom.
   //   // mSlider_Gain->GetValue() is in [-6,36]. wxSlider has min at top, so this is [-36dB,6dB].
   //   sliderValue = -sliderValue;
   //#endif
   //wxString str = _("Gain: ");
   //if (sliderValue > 0)
   //   str += "+";
   //str += wxString::Format("%d dB", sliderValue);
   //mSlider_Gain->SetToolTip(str);
//}

void MixerTrackCluster::OnSlider_Pan(wxCommandEvent& WXUNUSED(event))
{
   this->HandleSliderPan();
}

void MixerTrackCluster::OnButton_Mute(wxCommandEvent& WXUNUSED(event))
{
#ifdef EXPERIMENTAL_MIDI_OUT
   mProject->HandleTrackMute(mTrack, mToggleButton_Mute->WasShiftDown());
   mToggleButton_Mute->SetAlternate(mTrack->GetSolo());
#else
   mProject->HandleTrackMute(mLeftTrack, mToggleButton_Mute->WasShiftDown());
   mToggleButton_Mute->SetAlternate(mLeftTrack->GetSolo());
#endif

   // Update the TrackPanel correspondingly.
   if (mProject->IsSoloSimple())
   {
      // Have to refresh all tracks.
      mMixerBoard->UpdateSolo();
      mProject->RedrawProject();
   }
   else
      // Update only the changed track.
#ifdef EXPERIMENTAL_MIDI_OUT
      mProject->RefreshTPTrack(mTrack);
#else
      mProject->RefreshTPTrack(mLeftTrack);
#endif
}

void MixerTrackCluster::OnButton_Solo(wxCommandEvent& WXUNUSED(event))
{
#ifdef EXPERIMENTAL_MIDI_OUT
   mProject->HandleTrackSolo(mTrack, mToggleButton_Solo->WasShiftDown());
   bool bIsSolo = mTrack->GetSolo();
#else
   mProject->HandleTrackSolo(mLeftTrack, mToggleButton_Solo->WasShiftDown());
   bool bIsSolo = mLeftTrack->GetSolo();
#endif
   mToggleButton_Mute->SetAlternate(bIsSolo);

   // Update the TrackPanel correspondingly.
   if (mProject->IsSoloSimple())
   {
      // Have to refresh all tracks.
      mMixerBoard->UpdateMute();
      mMixerBoard->UpdateSolo();
      mProject->RedrawProject();
   }
   else
      // Update only the changed track.
#ifdef EXPERIMENTAL_MIDI_OUT
      mProject->RefreshTPTrack(mTrack);
#else
      mProject->RefreshTPTrack(mLeftTrack);
#endif
}


// class MusicalInstrument

MusicalInstrument::MusicalInstrument(wxBitmap* pBitmap, const wxString strXPMfilename)
{
   mBitmap = pBitmap;

   int nUnderscoreIndex;
   wxString strFilename = strXPMfilename;
   strFilename.MakeLower(); // Make sure, so we don't have to do case insensitive comparison.
   wxString strKeyword;
   while ((nUnderscoreIndex = strFilename.Find(wxT('_'))) != -1)
   {
      strKeyword = strFilename.Left(nUnderscoreIndex);
      mKeywords.Add(strKeyword);
      strFilename = strFilename.Mid(nUnderscoreIndex + 1);
   }
   if (!strFilename.IsEmpty()) // Skip trailing underscores.
      mKeywords.Add(strFilename); // Add the last one.
}

MusicalInstrument::~MusicalInstrument()
{
   delete mBitmap;
   mKeywords.Clear();
}

WX_DEFINE_OBJARRAY(MusicalInstrumentArray);


// class MixerBoardScrolledWindow

// wxScrolledWindow ignores mouse clicks in client area,
// but they don't get passed to Mixerboard.
// We need to catch them to deselect all track clusters.

BEGIN_EVENT_TABLE(MixerBoardScrolledWindow, wxScrolledWindow)
   EVT_MOUSE_EVENTS(MixerBoardScrolledWindow::OnMouseEvent)
END_EVENT_TABLE()

MixerBoardScrolledWindow::MixerBoardScrolledWindow(AudacityProject* project,
                                                   MixerBoard* parent, wxWindowID id /*= -1*/,
                                                   const wxPoint& pos /*= wxDefaultPosition*/,
                                                   const wxSize& size /*= wxDefaultSize*/,
                                                   long style /*= wxHSCROLL | wxVSCROLL*/)
: wxScrolledWindow(parent, id, pos, size, style)
{
   mMixerBoard = parent;
   mProject = project;
}

MixerBoardScrolledWindow::~MixerBoardScrolledWindow()
{
}

void MixerBoardScrolledWindow::OnMouseEvent(wxMouseEvent& event)
{
   if (event.ButtonUp())
   {
      //v Even when I implement MixerBoard::OnMouseEvent and call event.Skip()
      // here, MixerBoard::OnMouseEvent never gets called.
      // So, added mProject to MixerBoardScrolledWindow and just directly do what's needed here.
      mProject->SelectNone();
   }
   else
      event.Skip();
}


// class MixerBoard

#define MIXER_BOARD_MIN_HEIGHT      460

// Min width is one cluster wide, plus margins.
#define MIXER_BOARD_MIN_WIDTH       kTripleInset + kMixerTrackClusterWidth + kTripleInset


BEGIN_EVENT_TABLE(MixerBoard, wxWindow)
   EVT_SIZE(MixerBoard::OnSize)
END_EVENT_TABLE()

MixerBoard::MixerBoard(AudacityProject* pProject,
                        wxFrame* parent,
                        const wxPoint& pos /*= wxDefaultPosition*/,
                        const wxSize& size /*= wxDefaultSize*/)
: wxWindow(parent, -1, pos, size)
{
   // public data members

   // mute & solo button images
   // Create once and store on MixerBoard for use in all MixerTrackClusters.
   mImageMuteUp = NULL;
   mImageMuteOver = NULL;
   mImageMuteDown = NULL;
   mImageMuteDownWhileSolo = NULL;
   mImageMuteDisabled = NULL;
   mImageSoloUp = NULL;
   mImageSoloOver = NULL;
   mImageSoloDown = NULL;
   mImageSoloDisabled = NULL;

   mMuteSoloWidth = kRightSideStackWidth - kInset; // correct for max width, but really set in MixerBoard::CreateMuteSoloImages

   // private data members
   this->LoadMusicalInstruments(); // Set up mMusicalInstruments.
   mProject = pProject;

   mScrolledWindow =
      new MixerBoardScrolledWindow(
         pProject, // AudacityProject* project,
         this, -1, // wxWindow* parent, wxWindowID id = -1,
         this->GetClientAreaOrigin(), // const wxPoint& pos = wxDefaultPosition,
         size, // const wxSize& size = wxDefaultSize,
         wxHSCROLL); // long style = wxHSCROLL | wxVSCROLL, const wxString& name = "scrolledWindow")

   // Set background color to same as TrackPanel background.
   #ifdef EXPERIMENTAL_THEMING
      mScrolledWindow->SetBackgroundColour(this->GetParent()->GetBackgroundColour());
   #else
      mScrolledWindow->SetBackgroundColour(wxSystemSettings::GetColour(wxSYS_COLOUR_3DSHADOW));
   #endif

   mScrolledWindow->SetScrollRate(10, 0); // no vertical scroll
   mScrolledWindow->SetVirtualSize(size);

   /* This doesn't work to make the mScrolledWindow automatically resize, so do it explicitly in OnSize.
         wxBoxSizer* pBoxSizer = new wxBoxSizer(wxVERTICAL);
         pBoxSizer->Add(mScrolledWindow, 0, wxExpand, 0);
         this->SetAutoLayout(true);
         this->SetSizer(pBoxSizer);
         pBoxSizer->Fit(this);
         pBoxSizer->SetSizeHints(this);
      */

   mPrevT1 = 0.0;
   mTracks = mProject->GetTracks();
}

MixerBoard::~MixerBoard()
{
   // public data members
   delete mImageMuteUp;
   delete mImageMuteOver;
   delete mImageMuteDown;
   delete mImageMuteDownWhileSolo;
   delete mImageMuteDisabled;
   delete mImageSoloUp;
   delete mImageSoloOver;
   delete mImageSoloDown;
   delete mImageSoloDisabled;

   // private data members
   mMusicalInstruments.Clear();
}

// Reassign mixer input strips (MixerTrackClusters) to Track Clusters
// both have the same order.
// If EXPERIMENTAL_MIDI_OUT, then Note Tracks appear in the
// mixer, and we must be able to convert and reuse a MixerTrackCluster
// from audio to midi or midi to audio. This task is handled by
// UpdateForStateChange().
//
void MixerBoard::UpdateTrackClusters()
{
   if (!mImageMuteUp)
      this->CreateMuteSoloImages();

   const int nClusterHeight = mScrolledWindow->GetClientSize().GetHeight() - kDoubleInset;
   const size_t nClusterCount = mMixerTrackClusters.GetCount();
   unsigned int nClusterIndex = 0;
   TrackListIterator iterTracks(mTracks);
   MixerTrackCluster* pMixerTrackCluster = NULL;
   Track* pLeftTrack;
   Track* pRightTrack;

   pLeftTrack = iterTracks.First();
   while (pLeftTrack) {
      pRightTrack = pLeftTrack->GetLinked() ? iterTracks.Next() : NULL;

      if (pLeftTrack->GetKind() == Track::Wave
#ifdef EXPERIMENTAL_MIDI_OUT
          || pLeftTrack->GetKind() == Track::Note
#endif
          )
      {
         if (nClusterIndex < nClusterCount)
         {
            // Already showing it.
            // Track clusters are maintained in the same order as the WaveTracks.
            // Track pointers can change for the "same" track for different states
            // on the undo stack, so update the pointers and display name.
#ifdef EXPERIMENTAL_MIDI_OUT
            if (pLeftTrack->GetKind() == Track::Note) {
               mMixerTrackClusters[nClusterIndex]->mNoteTrack = (NoteTrack*)pLeftTrack;
               mMixerTrackClusters[nClusterIndex]->mLeftTrack = NULL;
            } else {
               mMixerTrackClusters[nClusterIndex]->mNoteTrack = NULL;
               mMixerTrackClusters[nClusterIndex]->mLeftTrack = (WaveTrack*)pLeftTrack;
            }
#else
            mMixerTrackClusters[nClusterIndex]->mLeftTrack = (WaveTrack*)pLeftTrack;
#endif
            mMixerTrackClusters[nClusterIndex]->mRightTrack = (WaveTrack*)pRightTrack;
            mMixerTrackClusters[nClusterIndex]->UpdateForStateChange();
         }
         else
         {
            // Not already showing it. Add a new MixerTrackCluster.
            wxPoint clusterPos(
               (kInset +                                       // extra inset to left for first one, so it's double
                  (nClusterIndex *
                     (kInset + kMixerTrackClusterWidth)) +     // left margin and width for each to its left
                  kInset),                                     // plus left margin for new cluster
               kInset);
            wxSize clusterSize(kMixerTrackClusterWidth, nClusterHeight);
            pMixerTrackCluster =
               new MixerTrackCluster(mScrolledWindow, this, mProject,
                                       (WaveTrack*)pLeftTrack, (WaveTrack*)pRightTrack,
                                       clusterPos, clusterSize);
            if (pMixerTrackCluster)
               mMixerTrackClusters.Add(pMixerTrackCluster);
         }
         nClusterIndex++;
      }
      pLeftTrack = iterTracks.Next();
   }

   if (pMixerTrackCluster)
   {
      // Added at least one MixerTrackCluster.
      this->UpdateWidth();
      this->ResizeTrackClusters();
   }
   else if (nClusterIndex < nClusterCount)
   {
      // We've got too many clusters.
      // This can happen only on things like Undo New Audio Track or Undo Import
      // that don't call RemoveTrackCluster explicitly.
      // We've already updated the track pointers for the clusters to the left, so just remove all the rest.
      // Keep nClusterIndex constant and successively delete from left to right.
      for (unsigned int nCounter = nClusterIndex; nCounter < nClusterCount; nCounter++)
#ifdef EXPERIMENTAL_MIDI_OUT
         this->RemoveTrackCluster(mMixerTrackClusters[nClusterIndex]->mTrack);
#else
         this->RemoveTrackCluster(mMixerTrackClusters[nClusterIndex]->mLeftTrack);
#endif
   }
}

int MixerBoard::GetTrackClustersWidth()
{
   return
      kInset +                                     // extra margin at left for first one
      (mMixerTrackClusters.GetCount() *            // number of tracks times
         (kInset + kMixerTrackClusterWidth)) +     // left margin and width for each
      kDoubleInset;                                // plus final right margin
}

#ifdef EXPERIMENTAL_MIDI_OUT
void MixerBoard::MoveTrackCluster(const Track* pTrack,
                                  bool bUp) // Up in TrackPanel is left in MixerBoard.
#else
void MixerBoard::MoveTrackCluster(const WaveTrack* pTrack,
                                  bool bUp) // Up in TrackPanel is left in MixerBoard.
#endif
{
   MixerTrackCluster* pMixerTrackCluster;
   int nIndex = FindMixerTrackCluster(pTrack, &pMixerTrackCluster);
   if (pMixerTrackCluster == NULL)
      return; // Couldn't find it.

   wxPoint pos;
   if (bUp)
   {  // Move it up (left).
      if (nIndex <= 0)
         return; // It's already first (0), or not found (-1).

      pos = pMixerTrackCluster->GetPosition();
      mMixerTrackClusters[nIndex] = mMixerTrackClusters[nIndex - 1];
      mMixerTrackClusters[nIndex]->Move(pos);

      mMixerTrackClusters[nIndex - 1] = pMixerTrackCluster;
      pMixerTrackCluster->Move(pos.x - (kInset + kMixerTrackClusterWidth), pos.y);
   }
   else
   {  // Move it down (right).
      if (((unsigned int)nIndex + 1) >= mMixerTrackClusters.GetCount())
         return; // It's already last.

      pos = pMixerTrackCluster->GetPosition();
      mMixerTrackClusters[nIndex] = mMixerTrackClusters[nIndex + 1];
      mMixerTrackClusters[nIndex]->Move(pos);

      mMixerTrackClusters[nIndex + 1] = pMixerTrackCluster;
      pMixerTrackCluster->Move(pos.x + (kInset + kMixerTrackClusterWidth), pos.y);
   }
}

#ifdef EXPERIMENTAL_MIDI_OUT
void MixerBoard::RemoveTrackCluster(const Track* pTrack)
#else
void MixerBoard::RemoveTrackCluster(const WaveTrack* pTrack)
#endif
{
   // Find and destroy.
   MixerTrackCluster* pMixerTrackCluster;
   int nIndex = this->FindMixerTrackCluster(pTrack, &pMixerTrackCluster);
   if (pMixerTrackCluster == NULL)
      return; // Couldn't find it.

   mMixerTrackClusters.RemoveAt(nIndex);
   pMixerTrackCluster->Destroy(); // delete is unsafe on wxWindow.

   // Close the gap, if any.
   wxPoint pos;
   int targetX;
   for (unsigned int i = nIndex; i < mMixerTrackClusters.GetCount(); i++)
   {
      pos = mMixerTrackClusters[i]->GetPosition();
      targetX =
         kInset +                                     // extra inset to left for first one, so it's double
         (i * (kInset + kMixerTrackClusterWidth)) +   // left margin and width for each
         kInset;                                      // plus left margin for this cluster
      if (pos.x != targetX)
         mMixerTrackClusters[i]->Move(targetX, pos.y);
   }

   this->UpdateWidth();

#ifdef EXPERIMENTAL_MIDI_OUT
   // Sanity check: if there is still a MixerTrackCluster with pTrack, then
   // we deleted the first but should have deleted the last:
   FindMixerTrackCluster(pTrack, &pMixerTrackCluster);
   assert(pMixerTrackCluster == NULL);
#endif
}


#ifdef EXPERIMENTAL_MIDI_OUT
wxBitmap* MixerBoard::GetMusicalInstrumentBitmap(const wxString name)
#else
wxBitmap* MixerBoard::GetMusicalInstrumentBitmap(const WaveTrack* pLeftTrack)
#endif
{
   if (mMusicalInstruments.IsEmpty())
      return NULL;

   // random choice:    return mMusicalInstruments[(int)pLeftTrack % mMusicalInstruments.GetCount()].mBitmap;

#ifdef EXPERIMENTAL_MIDI_OUT
   const wxString strTrackName(name.MakeLower());
#else
   const wxString strTrackName(pLeftTrack->GetName().MakeLower());
#endif
   size_t nBestItemIndex = 0;
   unsigned int nBestScore = 0;
   unsigned int nInstrIndex = 0;
   unsigned int nKeywordIndex;
   unsigned int nNumKeywords;
   unsigned int nPointsPerMatch;
   unsigned int nScore;
   for (nInstrIndex = 0; nInstrIndex < mMusicalInstruments.GetCount(); nInstrIndex++)
   {
      nScore = 0;

      nNumKeywords = mMusicalInstruments[nInstrIndex].mKeywords.GetCount();
      if (nNumKeywords > 0)
      {
         nPointsPerMatch = 10 / nNumKeywords;
         for (nKeywordIndex = 0; nKeywordIndex < nNumKeywords; nKeywordIndex++)
            if (strTrackName.Contains(mMusicalInstruments[nInstrIndex].mKeywords[nKeywordIndex]))
            {
               nScore +=
                  nPointsPerMatch +
                  // Longer keywords get more points.
                  (2 * mMusicalInstruments[nInstrIndex].mKeywords[nKeywordIndex].Length());
            }
      }

      // Choose later one if just matching nBestScore, for better variety,
      // and so default works as last element.
      if (nScore >= nBestScore)
      {
         nBestScore = nScore;
         nBestItemIndex = nInstrIndex;
      }
   }
   return mMusicalInstruments[nBestItemIndex].mBitmap;
}

bool MixerBoard::HasSolo()
{
   TrackListIterator iterTracks(mTracks);
   Track* pTrack;
   for (pTrack = iterTracks.First(); pTrack; pTrack = iterTracks.Next())
      if (pTrack->GetSolo())
         return true;
   return false;
}

#ifdef EXPERIMENTAL_MIDI_OUT
void MixerBoard::RefreshTrackCluster(const Track* pTrack, bool bEraseBackground /*= true*/)
#else
void MixerBoard::RefreshTrackCluster(const WaveTrack* pTrack, bool bEraseBackground )
#endif
{
   MixerTrackCluster* pMixerTrackCluster;
   this->FindMixerTrackCluster(pTrack, &pMixerTrackCluster);
   if (pMixerTrackCluster)
      pMixerTrackCluster->Refresh(bEraseBackground);
}

void MixerBoard::RefreshTrackClusters(bool bEraseBackground /*= true*/)
{
   for (unsigned int i = 0; i < mMixerTrackClusters.GetCount(); i++)
      mMixerTrackClusters[i]->Refresh(bEraseBackground);
}

void MixerBoard::ResizeTrackClusters()
{
   for (unsigned int nClusterIndex = 0; nClusterIndex < mMixerTrackClusters.GetCount(); nClusterIndex++)
      mMixerTrackClusters[nClusterIndex]->HandleResize();
}

void MixerBoard::ResetMeters(const bool bResetClipping)
{
   mPrevT1 = BAD_STREAM_TIME;

   if (!this->IsShown())
      return;

   for (unsigned int i = 0; i < mMixerTrackClusters.GetCount(); i++)
      mMixerTrackClusters[i]->ResetMeter(bResetClipping);
}

#ifdef EXPERIMENTAL_MIDI_OUT
void MixerBoard::UpdateName(const Track* pTrack)
#else
void MixerBoard::UpdateName(const WaveTrack* pTrack)
#endif
{
   MixerTrackCluster* pMixerTrackCluster;
   this->FindMixerTrackCluster(pTrack, &pMixerTrackCluster);
   if (pMixerTrackCluster)
      pMixerTrackCluster->UpdateName();
}

#ifdef EXPERIMENTAL_MIDI_OUT
void MixerBoard::UpdateMute(const Track* pTrack /*= NULL*/) // NULL means update for all tracks.
#else
void MixerBoard::UpdateMute(const WaveTrack* pTrack /*= NULL*/) // NULL means update for all tracks.
#endif
{
   if (pTrack == NULL)
   {
      for (unsigned int i = 0; i < mMixerTrackClusters.GetCount(); i++)
         mMixerTrackClusters[i]->UpdateMute();
   }
   else
   {
      MixerTrackCluster* pMixerTrackCluster;
      FindMixerTrackCluster(pTrack, &pMixerTrackCluster);
      if (pMixerTrackCluster)
         pMixerTrackCluster->UpdateMute();
   }
}

#ifdef EXPERIMENTAL_MIDI_OUT
void MixerBoard::UpdateSolo(const Track* pTrack /*= NULL*/) // NULL means update for all tracks.
#else
void MixerBoard::UpdateSolo(const WaveTrack* pTrack /*= NULL*/) // NULL means update for all tracks.
#endif
{
   if (pTrack == NULL)
   {
      for (unsigned int i = 0; i < mMixerTrackClusters.GetCount(); i++)
         mMixerTrackClusters[i]->UpdateSolo();
   }
   else
   {
      MixerTrackCluster* pMixerTrackCluster;
      FindMixerTrackCluster(pTrack, &pMixerTrackCluster);
      if (pMixerTrackCluster)
         pMixerTrackCluster->UpdateSolo();
   }
}

#ifdef EXPERIMENTAL_MIDI_OUT
void MixerBoard::UpdatePan(const Track* pTrack)
#else
void MixerBoard::UpdatePan(const WaveTrack* pTrack)
#endif
{
   MixerTrackCluster* pMixerTrackCluster;
   FindMixerTrackCluster(pTrack, &pMixerTrackCluster);
   if (pMixerTrackCluster)
      pMixerTrackCluster->UpdatePan();
}

#ifdef EXPERIMENTAL_MIDI_OUT
void MixerBoard::UpdateGain(const Track* pTrack)
#else
void MixerBoard::UpdateGain(const WaveTrack* pTrack)
#endif
{
   MixerTrackCluster* pMixerTrackCluster;
   FindMixerTrackCluster(pTrack, &pMixerTrackCluster);
   if (pMixerTrackCluster)
      pMixerTrackCluster->UpdateGain();
}

void MixerBoard::UpdateMeters(const double t1, const bool bLoopedPlay)
{
   if (!this->IsShown() || (t1 == BAD_STREAM_TIME))
      return;

   if (mPrevT1 == BAD_STREAM_TIME)
   {
      mPrevT1 = t1;
      return;
   }

   // In loopedPlay mode, at the end of the loop, mPrevT1 is set to
   // selection end, so the next t1 will be less, but we do want to
   // keep updating the meters.
   if (t1 <= mPrevT1)
   {
      if (bLoopedPlay)
         mPrevT1 = t1;
      return;
   }

   for (unsigned int i = 0; i < mMixerTrackClusters.GetCount(); i++)
      mMixerTrackClusters[i]->UpdateMeter(mPrevT1, t1);

   mPrevT1 = t1;
}


void MixerBoard::UpdateWidth()
{
   int newWidth = this->GetTrackClustersWidth();

   // Min width is one cluster wide, plus margins.
   if (newWidth < MIXER_BOARD_MIN_WIDTH)
      newWidth = MIXER_BOARD_MIN_WIDTH;

   mScrolledWindow->SetVirtualSize(newWidth, -1);
   this->GetParent()->SetSize(newWidth + kDoubleInset, -1);
}

//
// private methods
//

void MixerBoard::CreateMuteSoloImages()
{
   // Much of this is taken from TrackLabel::DrawMuteSolo.
   wxMemoryDC dc;
   wxString str = _("Mute");
   int textWidth, textHeight;

   int fontSize = 10;
   #ifdef __WXMSW__
      fontSize = 8;
   #endif
   wxFont font(fontSize, wxSWISS, wxNORMAL, wxNORMAL);
   this->GetTextExtent(str, &textWidth, &textHeight, NULL, NULL, &font);
   mMuteSoloWidth = textWidth + kQuadrupleInset;
   if (mMuteSoloWidth < kRightSideStackWidth - kInset)
      mMuteSoloWidth = kRightSideStackWidth - kInset;

   wxBitmap bitmap(mMuteSoloWidth, MUTE_SOLO_HEIGHT);
   dc.SelectObject(bitmap);
   wxRect bev(0, 0, mMuteSoloWidth - 1, MUTE_SOLO_HEIGHT - 1);

   // mute button images
   AColor::Mute(&dc, false, false, false);
   dc.DrawRectangle(bev);

   wxCoord x = bev.x + (bev.width - textWidth) / 2;
   wxCoord y = bev.y + (bev.height - textHeight) / 2;
   dc.SetFont(font);
   dc.DrawText(str, x, y);

   AColor::Bevel(dc, true, bev);

   mImageMuteUp = new wxImage(bitmap.ConvertToImage());
   mImageMuteOver = new wxImage(bitmap.ConvertToImage()); // Same as up, for now.

   AColor::Mute(&dc, true, true, false);
   dc.DrawRectangle(bev);
   dc.DrawText(str, x, y);
   AColor::Bevel(dc, false, bev);
   mImageMuteDown = new wxImage(bitmap.ConvertToImage());

   AColor::Mute(&dc, true, true, true);
   dc.DrawRectangle(bev);
   dc.DrawText(str, x, y);
   AColor::Bevel(dc, false, bev);
   mImageMuteDownWhileSolo = new wxImage(bitmap.ConvertToImage());

   mImageMuteDisabled = new wxImage(mMuteSoloWidth, MUTE_SOLO_HEIGHT); // Leave empty because unused.


   // solo button images
   AColor::Solo(&dc, false, false);
   dc.DrawRectangle(bev);

   str = _("Solo");
   dc.GetTextExtent(str, &textWidth, &textHeight);
   x = bev.x + (bev.width - textWidth) / 2;
   y = bev.y + (bev.height - textHeight) / 2;
   dc.DrawText(str, x, y);

   AColor::Bevel(dc, true, bev);

   mImageSoloUp = new wxImage(bitmap.ConvertToImage());
   mImageSoloOver = new wxImage(bitmap.ConvertToImage()); // Same as up, for now.

   AColor::Solo(&dc, true, true);
   dc.DrawRectangle(bev);
   dc.DrawText(str, x, y);
   AColor::Bevel(dc, false, bev);
   mImageSoloDown = new wxImage(bitmap.ConvertToImage());

   mImageSoloDisabled = new wxImage(mMuteSoloWidth, MUTE_SOLO_HEIGHT); // Leave empty because unused.
}

#ifdef EXPERIMENTAL_MIDI_OUT
int MixerBoard::FindMixerTrackCluster(const Track* pTrack,
                                      MixerTrackCluster** hMixerTrackCluster) const
#else
int MixerBoard::FindMixerTrackCluster(const WaveTrack* pLeftTrack,
                                        MixerTrackCluster** hMixerTrackCluster) const
#endif
{
   *hMixerTrackCluster = NULL;
   for (unsigned int i = 0; i < mMixerTrackClusters.GetCount(); i++)
   {
#ifdef EXPERIMENTAL_MIDI_OUT
      if (mMixerTrackClusters[i]->mTrack == pTrack)
#else
      if (mMixerTrackClusters[i]->mLeftTrack == pLeftTrack)
#endif
      {
         *hMixerTrackCluster = mMixerTrackClusters[i];
         return i;
      }
   }
   return -1;
}

void MixerBoard::LoadMusicalInstruments()
{
   wxRect bev(1, 1, MUSICAL_INSTRUMENT_HEIGHT_AND_WIDTH - 2, MUSICAL_INSTRUMENT_HEIGHT_AND_WIDTH - 2);
   wxBitmap* bitmap;
   wxMemoryDC dc;
   MusicalInstrument* pMusicalInstrument;


   bitmap = new wxBitmap((const char**)acoustic_guitar_gtr_xpm);
   dc.SelectObject(*bitmap);
   AColor::Bevel(dc, false, bev);
   pMusicalInstrument = new MusicalInstrument(bitmap, wxT("acoustic_guitar_gtr"));
   mMusicalInstruments.Add(pMusicalInstrument);

   bitmap = new wxBitmap((const char**)acoustic_piano_pno_xpm);
   dc.SelectObject(*bitmap);
   AColor::Bevel(dc, false, bev);
   pMusicalInstrument = new MusicalInstrument(bitmap, wxT("acoustic_piano_pno"));
   mMusicalInstruments.Add(pMusicalInstrument);

   bitmap = new wxBitmap((const char**)back_vocal_bg_vox_xpm);
   dc.SelectObject(*bitmap);
   AColor::Bevel(dc, false, bev);
   pMusicalInstrument = new MusicalInstrument(bitmap, wxT("back_vocal_bg_vox"));
   mMusicalInstruments.Add(pMusicalInstrument);

   bitmap = new wxBitmap((const char**)clap_xpm);
   dc.SelectObject(*bitmap);
   AColor::Bevel(dc, false, bev);
   pMusicalInstrument = new MusicalInstrument(bitmap, wxT("clap"));
   mMusicalInstruments.Add(pMusicalInstrument);


   bitmap = new wxBitmap((const char**)drums_dr_xpm);
   dc.SelectObject(*bitmap);
   AColor::Bevel(dc, false, bev);
   pMusicalInstrument = new MusicalInstrument(bitmap, wxT("drums_dr"));
   mMusicalInstruments.Add(pMusicalInstrument);

   bitmap = new wxBitmap((const char**)electric_bass_guitar_bs_gtr_xpm);
   dc.SelectObject(*bitmap);
   AColor::Bevel(dc, false, bev);
   pMusicalInstrument = new MusicalInstrument(bitmap, wxT("electric_bass_guitar_bs_gtr"));
   mMusicalInstruments.Add(pMusicalInstrument);

   bitmap = new wxBitmap((const char**)electric_guitar_gtr_xpm);
   dc.SelectObject(*bitmap);
   AColor::Bevel(dc, false, bev);
   pMusicalInstrument = new MusicalInstrument(bitmap, wxT("electric_guitar_gtr"));
   mMusicalInstruments.Add(pMusicalInstrument);

   bitmap = new wxBitmap((const char**)electric_piano_pno_key_xpm);
   dc.SelectObject(*bitmap);
   AColor::Bevel(dc, false, bev);
   pMusicalInstrument = new MusicalInstrument(bitmap, wxT("electric_piano_pno_key"));
   mMusicalInstruments.Add(pMusicalInstrument);


   bitmap = new wxBitmap((const char**)kick_xpm);
   dc.SelectObject(*bitmap);
   AColor::Bevel(dc, false, bev);
   pMusicalInstrument = new MusicalInstrument(bitmap, wxT("kick"));
   mMusicalInstruments.Add(pMusicalInstrument);

   bitmap = new wxBitmap((const char**)loop_xpm);
   dc.SelectObject(*bitmap);
   AColor::Bevel(dc, false, bev);
   pMusicalInstrument = new MusicalInstrument(bitmap, wxT("loop"));
   mMusicalInstruments.Add(pMusicalInstrument);

   bitmap = new wxBitmap((const char**)organ_org_xpm);
   dc.SelectObject(*bitmap);
   AColor::Bevel(dc, false, bev);
   pMusicalInstrument = new MusicalInstrument(bitmap, wxT("organ_org"));
   mMusicalInstruments.Add(pMusicalInstrument);

   bitmap = new wxBitmap((const char**)perc_xpm);
   dc.SelectObject(*bitmap);
   AColor::Bevel(dc, false, bev);
   pMusicalInstrument = new MusicalInstrument(bitmap, wxT("perc"));
   mMusicalInstruments.Add(pMusicalInstrument);


   bitmap = new wxBitmap((const char**)sax_xpm);
   dc.SelectObject(*bitmap);
   AColor::Bevel(dc, false, bev);
   pMusicalInstrument = new MusicalInstrument(bitmap, wxT("sax"));
   mMusicalInstruments.Add(pMusicalInstrument);

   bitmap = new wxBitmap((const char**)snare_xpm);
   dc.SelectObject(*bitmap);
   AColor::Bevel(dc, false, bev);
   pMusicalInstrument = new MusicalInstrument(bitmap, wxT("snare"));
   mMusicalInstruments.Add(pMusicalInstrument);

   bitmap = new wxBitmap((const char**)string_violin_cello_xpm);
   dc.SelectObject(*bitmap);
   AColor::Bevel(dc, false, bev);
   pMusicalInstrument = new MusicalInstrument(bitmap, wxT("string_violin_cello"));
   mMusicalInstruments.Add(pMusicalInstrument);

   bitmap = new wxBitmap((const char**)synth_xpm);
   dc.SelectObject(*bitmap);
   AColor::Bevel(dc, false, bev);
   pMusicalInstrument = new MusicalInstrument(bitmap, wxT("synth"));
   mMusicalInstruments.Add(pMusicalInstrument);


   bitmap = new wxBitmap((const char**)tambo_xpm);
   dc.SelectObject(*bitmap);
   AColor::Bevel(dc, false, bev);
   pMusicalInstrument = new MusicalInstrument(bitmap, wxT("tambo"));
   mMusicalInstruments.Add(pMusicalInstrument);

   bitmap = new wxBitmap((const char**)trumpet_horn_xpm);
   dc.SelectObject(*bitmap);
   AColor::Bevel(dc, false, bev);
   pMusicalInstrument = new MusicalInstrument(bitmap, wxT("trumpet_horn"));
   mMusicalInstruments.Add(pMusicalInstrument);

   bitmap = new wxBitmap((const char**)turntable_xpm);
   dc.SelectObject(*bitmap);
   AColor::Bevel(dc, false, bev);
   pMusicalInstrument = new MusicalInstrument(bitmap, wxT("turntable"));
   mMusicalInstruments.Add(pMusicalInstrument);

   bitmap = new wxBitmap((const char**)vibraphone_vibes_xpm);
   dc.SelectObject(*bitmap);
   AColor::Bevel(dc, false, bev);
   pMusicalInstrument = new MusicalInstrument(bitmap, wxT("vibraphone_vibes"));
   mMusicalInstruments.Add(pMusicalInstrument);


   bitmap = new wxBitmap((const char**)vocal_vox_xpm);
   dc.SelectObject(*bitmap);
   AColor::Bevel(dc, false, bev);
   pMusicalInstrument = new MusicalInstrument(bitmap, wxT("vocal_vox"));
   mMusicalInstruments.Add(pMusicalInstrument);


   // This one must be last, so it wins when best score is 0.
   bitmap = new wxBitmap((const char**)_default_instrument_xpm);
   dc.SelectObject(*bitmap);
   AColor::Bevel(dc, false, bev);
   pMusicalInstrument = new MusicalInstrument(bitmap, wxEmptyString);
   mMusicalInstruments.Add(pMusicalInstrument);
}

// event handlers

void MixerBoard::OnSize(wxSizeEvent &evt)
{
   // this->FitInside() doesn't work, and it doesn't happen automatically. Is wxScrolledWindow wrong?
   mScrolledWindow->SetSize(evt.GetSize());

   this->ResizeTrackClusters();
   this->RefreshTrackClusters(true);
}


// class MixerBoardFrame

BEGIN_EVENT_TABLE(MixerBoardFrame, wxFrame)
   EVT_CLOSE(MixerBoardFrame::OnCloseWindow)
   EVT_MAXIMIZE(MixerBoardFrame::OnMaximize)
   EVT_SIZE(MixerBoardFrame::OnSize)
END_EVENT_TABLE()

// Default to fitting one track.
const wxSize kDefaultSize =
   wxSize(MIXER_BOARD_MIN_WIDTH, MIXER_BOARD_MIN_HEIGHT);

MixerBoardFrame::MixerBoardFrame(AudacityProject* parent)
: wxFrame(parent, -1,
            wxString::Format(_("Audacity Mixer Board%s"),
                              ((parent->GetName() == wxEmptyString) ?
                                 wxT("") :
                                 wxString::Format(wxT(" - %s"),
                                                parent->GetName().c_str()).c_str())),
            wxDefaultPosition, kDefaultSize,
            //vvv Bug in wxFRAME_FLOAT_ON_PARENT:
            // If both the project frame and MixerBoardFrame are minimized and you restore MixerBoardFrame,
            // you can't restore project frame until you close MixerBoardFrame, but then project frame and
            // MixerBoardFrame are restored but MixerBoardFrame is unresponsive because it thinks it's not shown.
            //    wxDEFAULT_FRAME_STYLE | wxFRAME_FLOAT_ON_PARENT)
            wxDEFAULT_FRAME_STYLE)
{
   mMixerBoard = new MixerBoard(parent, this, wxDefaultPosition, kDefaultSize);

   this->SetSizeHints(MIXER_BOARD_MIN_WIDTH, MIXER_BOARD_MIN_HEIGHT);

   mMixerBoard->UpdateTrackClusters();

   // loads either the XPM or the windows resource, depending on the platform
   #if !defined(__WXMAC__) && !defined(__WXX11__)
      #ifdef __WXMSW__
         wxIcon ic(wxICON(AudacityLogo));
      #else
         wxIcon ic(wxICON(AudacityLogo48x48));
      #endif
      SetIcon(ic);
   #endif
}

MixerBoardFrame::~MixerBoardFrame()
{
}

// event handlers
void MixerBoardFrame::OnCloseWindow(wxCloseEvent &WXUNUSED(event))
{
   this->Hide();
}

void MixerBoardFrame::OnMaximize(wxMaximizeEvent &event)
{
   // Update the size hints to show all tracks before skipping to let default handling happen.
   mMixerBoard->UpdateWidth();
   event.Skip();
}

void MixerBoardFrame::OnSize(wxSizeEvent & WXUNUSED(event))
{
   mMixerBoard->SetSize(this->GetClientSize());
}


