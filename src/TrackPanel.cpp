/**********************************************************************

  Audacity: A Digital Audio Editor

  TrackPanel.cpp

  Dominic Mazzoni
  and lots of other contributors

  Implements TrackPanel and TrackInfo.

********************************************************************//*!

\todo
  Refactoring of the TrackPanel, possibly as described
  in \ref TrackPanelRefactor

*//*****************************************************************//*!

\file TrackPanel.cpp
\brief
  Implements TrackPanel and TrackInfo.

  TrackPanel.cpp is currently some of the worst code in Audacity.
  It's not really unreadable, there's just way too much stuff in this
  one file.  Rather than apply a quick fix, the long-term plan
  is to create a GUITrack class that knows how to draw itself
  and handle events.  Then this class just helps coordinate
  between tracks.

  Plans under discussion are described in \ref TrackPanelRefactor

*//********************************************************************/

// Documentation: Rather than have a lengthy \todo section, having
// a \todo a \file and a \page in EXACTLY that order gets Doxygen to
// put the following lengthy description of refactoring on a NEW page
// and link to it from the docs.

/*****************************************************************//**

\class TrackPanel
\brief
  The TrackPanel class coordinates updates and operations on the
  main part of the screen which contains multiple tracks.

  It uses many other classes, but in particular it uses the
  TrackInfo class to draw the controls area on the left of a track,
  and the TrackArtist class to draw the actual waveforms.

  Note that in some of the older code here, e.g., GetLabelWidth(),
  "Label" means the TrackInfo plus the vertical ruler.
  Confusing relative to LabelTrack labels.

  The TrackPanel manages multiple tracks and their TrackInfos.

  Note that with stereo tracks there will be one TrackInfo
  being used by two wavetracks.

*//*****************************************************************//**

\class TrackInfo
\brief
  The TrackInfo is shown to the side of a track
  It has the menus, pan and gain controls displayed in it.
  So "Info" is somewhat a misnomer. Should possibly be "TrackControls".

  TrackPanel and not TrackInfo takes care of the functionality for
  each of the buttons in that panel.

  In its current implementation TrackInfo is not derived from a
  wxWindow.  Following the original coding style, it has
  been coded as a 'flyweight' class, which is passed
  state as needed, except for the array of gains and pans.

  If we'd instead coded it as a wxWindow, we would have an instance
  of this class for each instance displayed.

*//**************************************************************//**

\class TrackPanelListener
\brief A now badly named class which is used to give access to a
subset of the TrackPanel methods from all over the place.

*//**************************************************************//**

\class TrackList
\brief A list of TrackListNode items.

*//**************************************************************//**

\class TrackListIterator
\brief An iterator for a TrackList.

*//**************************************************************//**

\class TrackListNode
\brief Used by TrackList, points to a Track.

*//**************************************************************//**

\class TrackPanel::AudacityTimer
\brief Timer class dedicated to infomring the TrackPanel that it
is time to refresh some aspect of the screen.

*//*****************************************************************//**

\page TrackPanelRefactor Track Panel Refactor
\brief Planned refactoring of TrackPanel.cpp

 - Move menus from current TrackPanel into TrackInfo.
 - Convert TrackInfo from 'flyweight' to heavyweight.
 - Split GuiStereoTrack and GuiWaveTrack out from TrackPanel.

  JKC: Incremental refactoring started April/2003

  Possibly aiming for Gui classes something like this - it's under
  discussion:

<pre>
   +----------------------------------------------------+
   |      AdornedRulerPanel                             |
   +----------------------------------------------------+
   +----------------------------------------------------+
   |+------------+ +-----------------------------------+|
   ||            | | (L)  GuiWaveTrack                 ||
   || TrackInfo  | +-----------------------------------+|
   ||            | +-----------------------------------+|
   ||            | | (R)  GuiWaveTrack                 ||
   |+------------+ +-----------------------------------+|
   +-------- GuiStereoTrack ----------------------------+
   +----------------------------------------------------+
   |+------------+ +-----------------------------------+|
   ||            | | (L)  GuiWaveTrack                 ||
   || TrackInfo  | +-----------------------------------+|
   ||            | +-----------------------------------+|
   ||            | | (R)  GuiWaveTrack                 ||
   |+------------+ +-----------------------------------+|
   +-------- GuiStereoTrack ----------------------------+
</pre>

  With the whole lot sitting in a TrackPanel which forwards
  events to the sub objects.

  The GuiStereoTrack class will do the special logic for
  Stereo channel grouping.

  The precise names of the classes are subject to revision.
  Have deliberately not created NEW files for the NEW classes
  such as AdornedRulerPanel and TrackInfo - yet.

*//*****************************************************************/


#include "Audacity.h"
#include "Experimental.h"
#include "TrackPanel.h"
#include "TrackPanelCell.h"
#include "TrackPanelCellIterator.h"

//#define DEBUG_DRAW_TIMING 1
// #define SPECTRAL_EDITING_ESC_KEY

#include <wx/fontenum.h>
#include <wx/numdlg.h>
#include <wx/spinctrl.h>

#include "FreqWindow.h" // for SpectrumAnalyst

#include "AColor.h"
#include "AllThemeResources.h"
#include "AudioIO.h"
#include "Envelope.h"
#include "float_cast.h"
#include "LabelTrack.h"
#include "MixerBoard.h"

#include "NoteTrack.h"
#include "NumberScale.h"
#include "Prefs.h"
#include "Snap.h"
#include "ShuttleGui.h"
#include "TimeTrack.h"
#include "TrackArtist.h"
#include "TrackPanelAx.h"
#include "UndoManager.h"
#include "WaveTrack.h"

#include "commands/Keyboard.h"

#include "ondemand/ODManager.h"

#include "prefs/PrefsDialog.h"
#include "prefs/SpectrumPrefs.h"
#include "prefs/WaveformPrefs.h"

#include "toolbars/ControlToolBar.h"
#include "toolbars/ToolsToolBar.h"

// To do:  eliminate this!
#include "tracks/ui/Scrubbing.h"

#define ZOOMLIMIT 0.001f

//This loads the appropriate set of cursors, depending on platform.
#include "../images/Cursors.h"

DEFINE_EVENT_TYPE(EVT_TRACK_PANEL_TIMER)

/*

This is a diagram of TrackPanel's division of one (non-stereo) track rectangle.
Total height equals Track::GetHeight()'s value.  Total width is the wxWindow's width.
Each charater that is not . represents one pixel.

Inset space of this track, and top inset of the next track, are used to draw the focus highlight.

Top inset of the right channel of a stereo track, and bottom shadow line of the
left channel, are used for the channel separator.

TrackInfo::GetTrackInfoWidth() == GetVRulerOffset()
counts columns from the left edge up to and including controls, and is a constant.

GetVRulerWidth() is variable -- all tracks have the same ruler width at any time,
but that width may be adjusted when tracks change their vertical scales.

GetLabelWidth() counts columns up to and including the VRuler.
GetLeftOffset() is yet one more -- it counts the "one pixel" column.

FindTrack() for label returns a rectangle up to and including the One Pixel column,
but OMITS left and top insets

FindTrack() for !label returns a rectangle with x == GetLeftOffset(), and INCLUDES
right and top insets

+--------------- ... ------ ... --------------------- ...       ... -------------+
| Top Inset                                                                      |
|                                                                                |
|  +------------ ... ------ ... --------------------- ...       ... ----------+  |
| L|+-Border---- ... ------ ... --------------------- ...       ... -Border-+ |R |
| e||+---------- ... -++--- ... -+++----------------- ...       ... -------+| |i |
| f|B|                ||         |||                                       |BS|g |
| t|o| Controls       || V       |O|  The good stuff                       |oh|h |
|  |r|                || R       |n|                                       |ra|t |
| I|d|                || u       |e|                                       |dd|  |
| n|e|                || l       | |                                       |eo|I |
| s|r|                || e       |P|                                       |rw|n |
| e|||                || r       |i|                                       ||||s |
| t|||                ||         |x|                                       ||||e |
|  |||                ||         |e|                                       ||||t |
|  |||                ||         |l|                                       ||||  |
|  |||                ||         |||                                       ||||  |

.  ...                ..         ...                                       ....  .
.  ...                ..         ...                                       ....  .
.  ...                ..         ...                                       ....  .

|  |||                ||         |||                                       ||||  |
|  ||+----------     -++--  ... -+++----------------- ...       ... -------+|||  |
|  |+-Border---- ... -----  ... --------------------- ...       ... -Border-+||  |
|  |  Shadow---- ... -----  ... --------------------- ...       ... --Shadow-+|  |
*/

// Is the distance between A and B less than D?
template < class A, class B, class DIST > bool within(A a, B b, DIST d)
{
   return (a > b - d) && (a < b + d);
}

template < class CLIPPEE, class CLIPVAL >
    void clip_top(CLIPPEE & clippee, CLIPVAL val)
{
   if (clippee > val)
      clippee = val;
}

template < class CLIPPEE, class CLIPVAL >
    void clip_bottom(CLIPPEE & clippee, CLIPVAL val)
{
   if (clippee < val)
      clippee = val;
}

enum {
   TrackPanelFirstID = 2000,
   OnSetNameID,
   OnSetFontID,

   OnMoveUpID,
   OnMoveDownID,
   OnMoveTopID,
   OnMoveBottomID,

   OnUpOctaveID,
   OnDownOctaveID,

   OnChannelLeftID,
   OnChannelRightID,
   OnChannelMonoID,

   OnRate8ID,              // <---
   OnRate11ID,             //    |
   OnRate16ID,             //    |
   OnRate22ID,             //    |
   OnRate44ID,             //    |
   OnRate48ID,             //    | Leave these in order
   OnRate88ID,             //    |
   OnRate96ID,             //    |
   OnRate176ID,            //    | see OnTrackMenu()
   OnRate192ID,            //    |
   OnRate352ID,            //    |
   OnRate384ID,            //    |
   OnRateOtherID,          //    |
                           //    |
   On16BitID,              //    |
   On24BitID,              //    |
   OnFloatID,              // <---

   OnWaveformID,
   OnWaveformDBID,
   OnSpectrumID,
   OnSpectrogramSettingsID,

   OnSplitStereoID,
   OnSplitStereoMonoID,
   OnMergeStereoID,
   OnSwapChannelsID,

   OnSetTimeTrackRangeID,

   OnTimeTrackLinID,
   OnTimeTrackLogID,
   OnTimeTrackLogIntID,

   // Reserve an ample block of ids for waveform scale types
   OnFirstWaveformScaleID,
   OnLastWaveformScaleID = OnFirstWaveformScaleID + 9,

   // Reserve an ample block of ids for spectrum scale types
   OnFirstSpectrumScaleID,
   OnLastSpectrumScaleID = OnFirstSpectrumScaleID + 19,

   OnZoomInVerticalID,
   OnZoomOutVerticalID,
   OnZoomFitVerticalID,
};

BEGIN_EVENT_TABLE(TrackPanel, OverlayPanel)
    EVT_MOUSE_EVENTS(TrackPanel::OnMouseEvent)
    EVT_MOUSE_CAPTURE_LOST(TrackPanel::OnCaptureLost)
    EVT_COMMAND(wxID_ANY, EVT_CAPTURE_KEY, TrackPanel::OnCaptureKey)
    EVT_KEY_DOWN(TrackPanel::OnKeyDown)
    EVT_KEY_UP(TrackPanel::OnKeyUp)
    EVT_CHAR(TrackPanel::OnChar)
    EVT_PAINT(TrackPanel::OnPaint)
    EVT_SET_FOCUS(TrackPanel::OnSetFocus)
    EVT_KILL_FOCUS(TrackPanel::OnKillFocus)
    EVT_CONTEXT_MENU(TrackPanel::OnContextMenu)
    EVT_MENU(OnSetNameID, TrackPanel::OnSetName)
    EVT_MENU(OnSetFontID, TrackPanel::OnSetFont)
    EVT_MENU(OnSetTimeTrackRangeID, TrackPanel::OnSetTimeTrackRange)

    EVT_MENU_RANGE(OnMoveUpID, OnMoveDownID, TrackPanel::OnMoveTrack)
    EVT_MENU_RANGE(OnMoveTopID, OnMoveBottomID, TrackPanel::OnMoveTrack)
    EVT_MENU_RANGE(OnUpOctaveID, OnDownOctaveID, TrackPanel::OnChangeOctave)
    EVT_MENU_RANGE(OnChannelLeftID, OnChannelMonoID,
               TrackPanel::OnChannelChange)
    EVT_MENU_RANGE(OnWaveformID, OnSpectrumID, TrackPanel::OnSetDisplay)
    EVT_MENU(OnSpectrogramSettingsID, TrackPanel::OnSpectrogramSettings)
    EVT_MENU_RANGE(OnRate8ID, OnRate384ID, TrackPanel::OnRateChange)
    EVT_MENU_RANGE(On16BitID, OnFloatID, TrackPanel::OnFormatChange)
    EVT_MENU(OnRateOtherID, TrackPanel::OnRateOther)
    EVT_MENU(OnSwapChannelsID, TrackPanel::OnSwapChannels)
    EVT_MENU(OnSplitStereoID, TrackPanel::OnSplitStereo)
    EVT_MENU(OnSplitStereoMonoID, TrackPanel::OnSplitStereoMono)
    EVT_MENU(OnMergeStereoID, TrackPanel::OnMergeStereo)

    EVT_MENU(OnTimeTrackLinID, TrackPanel::OnTimeTrackLin)
    EVT_MENU(OnTimeTrackLogID, TrackPanel::OnTimeTrackLog)
    EVT_MENU(OnTimeTrackLogIntID, TrackPanel::OnTimeTrackLogInt)

    EVT_MENU_RANGE(OnFirstWaveformScaleID, OnLastWaveformScaleID, TrackPanel::OnWaveformScaleType)
    EVT_MENU_RANGE(OnFirstSpectrumScaleID, OnLastSpectrumScaleID, TrackPanel::OnSpectrumScaleType)

    EVT_MENU(OnZoomInVerticalID, TrackPanel::OnZoomInVertical)
    EVT_MENU(OnZoomOutVerticalID, TrackPanel::OnZoomOutVertical)
    EVT_MENU(OnZoomFitVerticalID, TrackPanel::OnZoomFitVertical)

    EVT_TIMER(wxID_ANY, TrackPanel::OnTimer)
END_EVENT_TABLE()

/// Makes a cursor from an XPM, uses CursorId as a fallback.
/// TODO:  Move this function to some other source file for reuse elsewhere.
std::unique_ptr<wxCursor> MakeCursor( int WXUNUSED(CursorId), const char * pXpm[36],  int HotX, int HotY )
{
#ifdef CURSORS_SIZE32
   const int HotAdjust =0;
#else
   const int HotAdjust =8;
#endif

   wxImage Image = wxImage(wxBitmap(pXpm).ConvertToImage());
   Image.SetMaskColour(255,0,0);
   Image.SetMask();// Enable mask.

   Image.SetOption( wxIMAGE_OPTION_CUR_HOTSPOT_X, HotX-HotAdjust );
   Image.SetOption( wxIMAGE_OPTION_CUR_HOTSPOT_Y, HotY-HotAdjust );
   return std::make_unique<wxCursor>( Image );
}



// Don't warn us about using 'this' in the base member initializer list.
#ifndef __WXGTK__ //Get rid if this pragma for gtk
#pragma warning( disable: 4355 )
#endif
TrackPanel::TrackPanel(wxWindow * parent, wxWindowID id,
                       const wxPoint & pos,
                       const wxSize & size,
                       const std::shared_ptr<TrackList> &tracks,
                       ViewInfo * viewInfo,
                       TrackPanelListener * listener,
                       AdornedRulerPanel * ruler)
   : OverlayPanel(parent, id, pos, size, wxWANTS_CHARS | wxNO_BORDER),
     mTrackInfo(this),
     mListener(listener),
     mTracks(tracks),
     mViewInfo(viewInfo),
     mRuler(ruler),
     mTrackArtist(nullptr),
     mRefreshBacking(false),
     mConverter(NumericConverter::TIME),
     mAutoScrolling(false),
     mVertScrollRemainder(0),
     vrulerSize(36,0)
#ifndef __WXGTK__   //Get rid if this pragma for gtk
#pragma warning( default: 4355 )
#endif
{
   SetLabel(_("Track Panel"));
   SetName(_("Track Panel"));
   SetBackgroundStyle(wxBG_STYLE_PAINT);

   {
      auto pAx = std::make_unique <TrackPanelAx>( this );
#if wxUSE_ACCESSIBILITY
      // wxWidgets owns the accessible object
      SetAccessible(mAx = pAx.release());
#else
      // wxWidgets doesn not own the object, but we need to retain it
      mAx = std::move(pAx);
#endif
   }

   mMouseCapture = IsUncaptured;
   mSlideUpDownOnly = false;
   mLabelTrackStartXPos=-1;
   mCircularTrackNavigation = false;


   mRedrawAfterStop = false;

   mPencilCursor  = MakeCursor( wxCURSOR_PENCIL,    DrawCursorXpm,    12, 22);
   mSelectCursor  = MakeCursor( wxCURSOR_IBEAM,     IBeamCursorXpm,   17, 16);
   mEnvelopeCursor= MakeCursor( wxCURSOR_ARROW,     EnvCursorXpm,     16, 16);
   mDisabledCursor= MakeCursor( wxCURSOR_NO_ENTRY,  DisabledCursorXpm,16, 16);
   mSlideCursor   = MakeCursor( wxCURSOR_SIZEWE,    TimeCursorXpm,    16, 16);
   mZoomInCursor  = MakeCursor( wxCURSOR_MAGNIFIER, ZoomInCursorXpm,  19, 15);
   mZoomOutCursor = MakeCursor( wxCURSOR_MAGNIFIER, ZoomOutCursorXpm, 19, 15);
   mLabelCursorLeft  = MakeCursor( wxCURSOR_ARROW,  LabelCursorLeftXpm, 19, 15);
   mLabelCursorRight = MakeCursor( wxCURSOR_ARROW,  LabelCursorRightXpm, 16, 16);
   
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   mBottomFrequencyCursor =
      MakeCursor( wxCURSOR_ARROW,  BottomFrequencyCursorXpm, 16, 16);
   mTopFrequencyCursor =
      MakeCursor( wxCURSOR_ARROW,  TopFrequencyCursorXpm, 16, 16);
   mBandWidthCursor = MakeCursor( wxCURSOR_ARROW,  BandWidthCursorXpm, 16, 16);
#endif

#if USE_MIDI
   mStretchMode = stretchCenter;
   mStretching = false;
   mStretched = false;
   mStretchStart = 0;
   mStretchCursor = MakeCursor( wxCURSOR_BULLSEYE,  StretchCursorXpm, 16, 16);
   mStretchLeftCursor = MakeCursor( wxCURSOR_BULLSEYE,
                                    StretchLeftCursorXpm, 16, 16);
   mStretchRightCursor = MakeCursor( wxCURSOR_BULLSEYE,
                                     StretchRightCursorXpm, 16, 16);
#endif

   mArrowCursor = std::make_unique<wxCursor>(wxCURSOR_ARROW);
   mSmoothCursor = std::make_unique<wxCursor>(wxCURSOR_SPRAYCAN);
   mResizeCursor = std::make_unique<wxCursor>(wxCURSOR_SIZENS);
   mRearrangeCursor = std::make_unique<wxCursor>(wxCURSOR_HAND);
   mAdjustLeftSelectionCursor = std::make_unique<wxCursor>(wxCURSOR_POINT_LEFT);
   mAdjustRightSelectionCursor = std::make_unique<wxCursor>(wxCURSOR_POINT_RIGHT);

   // Menu pointers are set to NULL here.  Delay building of menus until after
   // the command managter is finished, so that we can look up shortcut
   // key strings that need to appear in some of the popup menus.
   mWaveTrackMenu = NULL;
   mChannelItemsInsertionPoint = 0;
   
   mNoteTrackMenu = NULL;
   mLabelTrackMenu = NULL;
   mTimeTrackMenu = NULL;

   BuildMenus();

   mTrackArtist = std::make_unique<TrackArtist>();

   mTrackArtist->SetInset(1, kTopMargin, kRightMargin, kBottomMargin);

   mCapturedTrack = NULL;
   mPopupMenuTarget = NULL;

   mTimeCount = 0;
   mTimer.parent = this;
   mTimer.Start(kTimerInterval, FALSE);

   //Initialize a member variable pointing to the current
   //drawing track.
   mDrawingTrack =NULL;

   //More initializations, some of these for no other reason than
   //to prevent runtime memory check warnings
   mZoomStart = -1;
   mZoomEnd = -1;
   mPrevWidth = -1;
   mPrevHeight = -1;

   // This is used to snap the cursor to the nearest track that
   // lines up with it.
   mSnapManager = NULL;
   mSnapLeft = -1;
   mSnapRight = -1;

   // Register for tracklist updates
   mTracks->Connect(EVT_TRACKLIST_RESIZED,
                    wxCommandEventHandler(TrackPanel::OnTrackListResized),
                    NULL,
                    this);
   mTracks->Connect(EVT_TRACKLIST_UPDATED,
                    wxCommandEventHandler(TrackPanel::OnTrackListUpdated),
                    NULL,
                    this);

#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   mFreqSelMode = FREQ_SEL_INVALID;
   mFrequencySnapper = std::make_unique<SpectrumAnalyst>();

   mLastF0 = mLastF1 = SelectedRegion::UndefinedFrequency;
#endif

   mSelStartValid = false;
   mSelStart = 0;
}


TrackPanel::~TrackPanel()
{
   mTimer.Stop();

   // Unregister for tracklist updates
   mTracks->Disconnect(EVT_TRACKLIST_UPDATED,
                       wxCommandEventHandler(TrackPanel::OnTrackListUpdated),
                       NULL,
                       this);
   mTracks->Disconnect(EVT_TRACKLIST_RESIZED,
                       wxCommandEventHandler(TrackPanel::OnTrackListResized),
                       NULL,
                       this);

   // This can happen if a label is being edited and the user presses
   // ALT+F4 or Command+Q
   if (HasCapture())
      ReleaseMouse();

   DeleteMenus();
}

void TrackPanel::BuildMenusIfNeeded(void)
{
   if (!mRateMenu)
      BuildMenus();
}

void TrackPanel::BuildMenus(void)
{
   // Get rid of existing menus
   DeleteMenus();

   // Use AppendCheckItem so we can have ticks beside the items.
   // We would use AppendRadioItem but it only currently works on windows and GTK.
   auto rateMenu = std::make_unique<wxMenu>();
   rateMenu->AppendRadioItem(OnRate8ID, wxT("8000 Hz"));
   rateMenu->AppendRadioItem(OnRate11ID, wxT("11025 Hz"));
   rateMenu->AppendRadioItem(OnRate16ID, wxT("16000 Hz"));
   rateMenu->AppendRadioItem(OnRate22ID, wxT("22050 Hz"));
   rateMenu->AppendRadioItem(OnRate44ID, wxT("44100 Hz"));
   rateMenu->AppendRadioItem(OnRate48ID, wxT("48000 Hz"));
   rateMenu->AppendRadioItem(OnRate88ID, wxT("88200 Hz"));
   rateMenu->AppendRadioItem(OnRate96ID, wxT("96000 Hz"));
   rateMenu->AppendRadioItem(OnRate176ID, wxT("176400 Hz"));
   rateMenu->AppendRadioItem(OnRate192ID, wxT("192000 Hz"));
   rateMenu->AppendRadioItem(OnRate352ID, wxT("352800 Hz"));
   rateMenu->AppendRadioItem(OnRate384ID, wxT("384000 Hz"));
   rateMenu->AppendRadioItem(OnRateOtherID, _("&Other..."));

   auto formatMenu = std::make_unique<wxMenu>();
   formatMenu->AppendRadioItem(On16BitID, GetSampleFormatStr(int16Sample));
   formatMenu->AppendRadioItem(On24BitID, GetSampleFormatStr(int24Sample));
   formatMenu->AppendRadioItem(OnFloatID, GetSampleFormatStr(floatSample));

   /* build the pop-down menu used on wave (sampled audio) tracks */
   mWaveTrackMenu = std::make_unique<wxMenu>();
   BuildCommonDropMenuItems(mWaveTrackMenu.get());   // does name, up/down etc
   mWaveTrackMenu->AppendRadioItem(OnWaveformID, _("Wa&veform"));
   mWaveTrackMenu->AppendRadioItem(OnWaveformDBID, _("&Waveform (dB)"));
   mWaveTrackMenu->AppendRadioItem(OnSpectrumID, _("&Spectrogram"));
   mWaveTrackMenu->Append(OnSpectrogramSettingsID, _("S&pectrogram Settings..."));
   mWaveTrackMenu->AppendSeparator();

   // include both mono and stereo items as a work around for bug 1250

   mWaveTrackMenu->AppendRadioItem(OnChannelMonoID, _("&Mono"));
   mWaveTrackMenu->AppendRadioItem(OnChannelLeftID, _("&Left Channel"));
   mWaveTrackMenu->AppendRadioItem(OnChannelRightID, _("&Right Channel"));
   mWaveTrackMenu->Append(OnMergeStereoID, _("Ma&ke Stereo Track"));
   mWaveTrackMenu->Append(OnSwapChannelsID, _("Swap Stereo &Channels"));
   mWaveTrackMenu->Append(OnSplitStereoID, _("Spl&it Stereo Track"));
   mWaveTrackMenu->Append(OnSplitStereoMonoID, _("Split Stereo to Mo&no"));
   mWaveTrackMenu->AppendSeparator();

   mWaveTrackMenu->Append(0, _("&Format"), (mFormatMenu = formatMenu.release()));

   mWaveTrackMenu->AppendSeparator();

   mWaveTrackMenu->Append(0, _("Rat&e"), (mRateMenu = rateMenu.release()));

   /* build the pop-down menu used on note (MIDI) tracks */
   mNoteTrackMenu = std::make_unique<wxMenu>();
   BuildCommonDropMenuItems(mNoteTrackMenu.get());   // does name, up/down etc
   mNoteTrackMenu->Append(OnUpOctaveID, _("Up &Octave"));
   mNoteTrackMenu->Append(OnDownOctaveID, _("Down Octa&ve"));

   /* build the pop-down menu used on label tracks */
   mLabelTrackMenu = std::make_unique<wxMenu>();
   BuildCommonDropMenuItems(mLabelTrackMenu.get());   // does name, up/down etc
   mLabelTrackMenu->Append(OnSetFontID, _("&Font..."));

   /* build the pop-down menu used on time warping tracks */
   mTimeTrackMenu = std::make_unique<wxMenu>();
   BuildCommonDropMenuItems(mTimeTrackMenu.get());   // does name, up/down etc
   mTimeTrackMenu->AppendRadioItem(OnTimeTrackLinID, wxT("&Linear scale"));
   mTimeTrackMenu->AppendRadioItem(OnTimeTrackLogID, _("L&ogarithmic scale"));

   mTimeTrackMenu->AppendSeparator();
   mTimeTrackMenu->Append(OnSetTimeTrackRangeID, _("&Range..."));
   mTimeTrackMenu->AppendCheckItem(OnTimeTrackLogIntID, _("Logarithmic &Interpolation"));

/*
   mRulerWaveformMenu = std::make_unique<wxMenu>();
   BuildVRulerMenuItems
      (mRulerWaveformMenu.get(), OnFirstWaveformScaleID,
       WaveformSettings::GetScaleNames());

   mRulerSpectrumMenu = std::make_unique<wxMenu>();
   BuildVRulerMenuItems
      (mRulerSpectrumMenu.get(), OnFirstSpectrumScaleID,
       SpectrogramSettings::GetScaleNames());
*/
}

void TrackPanel::BuildCommonDropMenuItems(wxMenu * menu)
{
   menu->Append(OnSetNameID, _("&Name..."));
   menu->AppendSeparator();
   // It is not correct to use KeyStringDisplay here -- wxWidgets will apply
   // its equivalent to the key names passed to menu functions.
   menu->Append(OnMoveUpID, _("Move Track &Up") + wxT("\t") + 
                           (GetProject()->GetCommandManager()->GetKeyFromName(wxT("TrackMoveUp"))));
   menu->Append(OnMoveDownID, _("Move Track &Down") + wxT("\t") + 
                           (GetProject()->GetCommandManager()->GetKeyFromName(wxT("TrackMoveDown"))));
   menu->Append(OnMoveTopID, _("Move Track to &Top") + wxT("\t") + 
                           (GetProject()->GetCommandManager()->GetKeyFromName(wxT("TrackMoveTop"))));
   menu->Append(OnMoveBottomID, _("Move Track to &Bottom") + wxT("\t") + 
                           (GetProject()->GetCommandManager()->GetKeyFromName(wxT("TrackMoveBottom"))));
   menu->AppendSeparator();

}

/*
// left over from PRL's vertical ruler context menu experiment in 2.1.2
// static
void TrackPanel::BuildVRulerMenuItems
(wxMenu * menu, int firstId, const wxArrayString &names)
{
   int id = firstId;
   for (int ii = 0, nn = names.size(); ii < nn; ++ii)
      menu->AppendRadioItem(id++, names[ii]);
   menu->AppendSeparator();
   menu->Append(OnZoomInVerticalID, _("Zoom In\tLeft-Click/Left-Drag"));
   menu->Append(OnZoomOutVerticalID, _("Zoom Out\tShift-Left-Click"));
   menu->Append(OnZoomFitVerticalID, _("Zoom to Fit\tShift-Right-Click"));
}
*/

void TrackPanel::DeleteMenus(void)
{
   // Note that the submenus (mRateMenu, ...)
   // are deleted by their parent

   mRateMenu = mFormatMenu = nullptr;

   mWaveTrackMenu.reset();
   mNoteTrackMenu.reset();
   mLabelTrackMenu.reset();
   mTimeTrackMenu.reset();
   mRulerWaveformMenu.reset();
   mRulerSpectrumMenu.reset();
}

#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
void TrackPanel::UpdateVirtualStereoOrder()
{
   TrackListOfKindIterator iter(TrackKind::Wave, mTracks);
   Track *t;
   int temp;

   for (t = iter.First(); t; t = iter.Next()) {
      const auto wt = static_cast<WaveTrack*>(t);
      if(t->GetChannel() == Track::MonoChannel){

         if(WaveTrack::mMonoAsVirtualStereo && wt->GetPan() != 0){
            temp = wt->GetHeight();
            wt->SetHeight(temp*wt->GetVirtualTrackPercentage());
            wt->SetHeight(temp - wt->GetHeight(),true);
         }else if(!WaveTrack::mMonoAsVirtualStereo && wt->GetPan() != 0){
            wt->SetHeight(wt->GetHeight() + wt->GetHeight(true));
         }
      }
   }
   t = iter.First();
   if(t){
      t->ReorderList(false);
   }
}
#endif

void TrackPanel::UpdatePrefs()
{
   gPrefs->Read(wxT("/GUI/AutoScroll"), &mViewInfo->bUpdateTrackIndicator,
      true);
   gPrefs->Read(wxT("/GUI/AdjustSelectionEdges"), &mAdjustSelectionEdges,
      true);
   gPrefs->Read(wxT("/GUI/CircularTrackNavigation"), &mCircularTrackNavigation,
      false);
   gPrefs->Read(wxT("/GUI/Solo"), &mSoloPref, wxT("Simple"));

#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
   bool temp = WaveTrack::mMonoAsVirtualStereo;
   gPrefs->Read(wxT("/GUI/MonoAsVirtualStereo"), &WaveTrack::mMonoAsVirtualStereo,
      false);

   if(WaveTrack::mMonoAsVirtualStereo != temp)
      UpdateVirtualStereoOrder();
#endif

   mViewInfo->UpdatePrefs();

   if (mTrackArtist) {
      mTrackArtist->UpdatePrefs();
   }

   // All vertical rulers must be recalculated since the minimum and maximum
   // frequences may have been changed.
   UpdateVRulers();

   mTrackInfo.UpdatePrefs();

   Refresh();
}

/// Remembers the track we clicked on and why we captured it.
/// We also use this method to clear the record
/// of the captured track, passing NULL as the track.
void TrackPanel::SetCapturedTrack( Track * t, enum MouseCaptureEnum MouseCapture )
{
#if defined(__WXDEBUG__)
   if (t == NULL) {
      wxASSERT(MouseCapture == IsUncaptured);
   }
   else {
      wxASSERT(MouseCapture != IsUncaptured);
   }
#endif
   mCapturedTrack = t;
   mMouseCapture = MouseCapture;
}

void TrackPanel::SelectNone()
{
   TrackListIterator iter(GetTracks());
   Track *t = iter.First();
   while (t) {
      SelectTrack(t, false, false);
      t = iter.Next();
   }
}

/// Select all tracks marked by the label track lt
void TrackPanel::SelectTracksByLabel( LabelTrack *lt )
{
   TrackListIterator iter(GetTracks());
   Track *t = iter.First();

   //do nothing if at least one other track is selected
   while (t) {
      if( t->GetSelected() && t != lt )
         return;
      t = iter.Next();
   }

   //otherwise, select all tracks
   t = iter.First();
   while( t )
   {
      SelectTrack(t, true);
      t = iter.Next();
   }
}

// Set selection length to the length of a track -- but if sync-lock is turned
// on, use the largest possible selection in the sync-lock group.
// If it's a stereo track, do the same for the stereo channels.
void TrackPanel::SelectTrackLength(Track *t)
{
   AudacityProject *p = GetActiveProject();
   SyncLockedTracksIterator it(GetTracks());
   Track *t1 = it.StartWith(t);
   double minOffset = t->GetOffset();
   double maxEnd = t->GetEndTime();

   // If we have a sync-lock group and sync-lock linking is on,
   // check the sync-lock group tracks.
   if (p->IsSyncLocked() && t1 != NULL)
   {
      for ( ; t1; t1 = it.Next())
      {
         if (t1->GetOffset() < minOffset)
            minOffset = t1->GetOffset();
         if (t1->GetEndTime() > maxEnd)
            maxEnd = t1->GetEndTime();
      }
   }
   else
   {
      // Otherwise, check for a stereo pair
      t1 = t->GetLink();
      if (t1)
      {
         if (t1->GetOffset() < minOffset)
            minOffset = t1->GetOffset();
         if (t1->GetEndTime() > maxEnd)
            maxEnd = t1->GetEndTime();
      }
   }

   // PRL: double click or click on track control.
   // should this select all frequencies too?  I think not.
   mViewInfo->selectedRegion.setTimes(minOffset, maxEnd);
}

void TrackPanel::GetTracksUsableArea(int *width, int *height) const
{
   GetSize(width, height);
   if (width) {
      *width -= GetLeftOffset();
      *width -= kRightMargin;
      *width = std::max(0, *width);
   }
}

/// Gets the pointer to the AudacityProject that
/// goes with this track panel.
AudacityProject * TrackPanel::GetProject() const
{
   //JKC casting away constness here.
   //Do it in two stages in case 'this' is not a wxWindow.
   //when the compiler will flag the error.
   wxWindow const * const pConstWind = this;
   wxWindow * pWind=(wxWindow*)pConstWind;
#ifdef EXPERIMENTAL_NOTEBOOK
   pWind = pWind->GetParent(); //Page
   wxASSERT( pWind );
   pWind = pWind->GetParent(); //Notebook
   wxASSERT( pWind );
#endif
   pWind = pWind->GetParent(); //MainPanel
   wxASSERT( pWind );
   pWind = pWind->GetParent(); //Project
   wxASSERT( pWind );
   return (AudacityProject*)pWind;
}

/// AS: This gets called on our wx timer events.
void TrackPanel::OnTimer(wxTimerEvent& )
{
#ifdef __WXMAC__
   // Unfortunate part of fix for bug 1431
   // Without this, the toolbars hide only every other time that you press
   // the yellow title bar button.  For some reason, not every press sends
   // us a deactivate event for the application.
   {
      auto project = GetProject();
      if (project->IsIconized())
         project->MacShowUndockedToolbars(false);
   }
#endif

   mTimeCount++;
   // AS: If the user is dragging the mouse and there is a track that
   //  has captured the mouse, then scroll the screen, as necessary.
   if ((mMouseCapture==IsSelecting) && mCapturedTrack) {
      ScrollDuringDrag();
   }

   AudacityProject *const p = GetProject();

   // Check whether we were playing or recording, but the stream has stopped.
   if (p->GetAudioIOToken()>0 && !IsAudioActive())
   {
      //the stream may have been started up after this one finished (by some other project)
      //in that case reset the buttons don't stop the stream
      p->GetControlToolBar()->StopPlaying(!gAudioIO->IsStreamActive());
   }

   // Next, check to see if we were playing or recording
   // audio, but now Audio I/O is completely finished.
   if (p->GetAudioIOToken()>0 &&
         !gAudioIO->IsAudioTokenActive(p->GetAudioIOToken()))
   {
      p->FixScrollbars();
      p->SetAudioIOToken(0);
      p->RedrawProject();

      mRedrawAfterStop = false;

      //ANSWER-ME: Was DisplaySelection added to solve a repaint problem?
      DisplaySelection();
   }
   if (mLastDrawnSelectedRegion != mViewInfo->selectedRegion) {
      UpdateSelectionDisplay();
   }

   // Notify listeners for timer ticks
   {
      wxCommandEvent e(EVT_TRACK_PANEL_TIMER);
      p->GetEventHandler()->ProcessEvent(e);
   }

   DrawOverlays(false);
   mRuler->DrawOverlays(false);

   if(IsAudioActive() && gAudioIO->GetNumCaptureChannels()) {

      // Periodically update the display while recording

      if (!mRedrawAfterStop) {
         mRedrawAfterStop = true;
         MakeParentRedrawScrollbars();
         mListener->TP_ScrollUpDown( 99999999 );
         Refresh( false );
      }
      else {
         if ((mTimeCount % 5) == 0) {
            // Must tell OnPaint() to recreate the backing bitmap
            // since we've not done a full refresh.
            mRefreshBacking = true;
            Refresh( false );
         }
      }
   }
   if(mTimeCount > 1000)
      mTimeCount = 0;
}

///  We check on each timer tick to see if we need to scroll.
///  Scrolling is handled by mListener, which is an interface
///  to the window TrackPanel is embedded in.
void TrackPanel::ScrollDuringDrag()
{
   // DM: If we're "autoscrolling" (which means that we're scrolling
   //  because the user dragged from inside to outside the window,
   //  not because the user clicked in the scroll bar), then
   //  the selection code needs to be handled slightly differently.
   //  We set this flag ("mAutoScrolling") to tell the selecting
   //  code that we didn't get here as a result of a mouse event,
   //  and therefore it should ignore the mouseEvent parameter,
   //  and instead use the last known mouse position.  Setting
   //  this flag also causes the Mac to redraw immediately rather
   //  than waiting for the next update event; this makes scrolling
   //  smoother on MacOS 9.

   if (mMouseMostRecentX >= mCapturedRect.x + mCapturedRect.width) {
      mAutoScrolling = true;
      mListener->TP_ScrollRight();
   }
   else if (mMouseMostRecentX < mCapturedRect.x) {
      mAutoScrolling = true;
      mListener->TP_ScrollLeft();
   }
   else {
      // Bug1387:  enable autoscroll during drag, if the pointer is at either extreme x
      // coordinate of the screen, even if that is still within the track area.

      int xx = mMouseMostRecentX, yy = 0;
      this->ClientToScreen(&xx, &yy);
      if (xx == 0) {
         mAutoScrolling = true;
         mListener->TP_ScrollLeft();
      }
      else {
         int width, height;
         ::wxDisplaySize(&width, &height);
         if (xx == width - 1) {
            mAutoScrolling = true;
            mListener->TP_ScrollRight();
         }
      }
   }

   if (mAutoScrolling) {
      // AS: To keep the selection working properly as we scroll,
      //  we fake a mouse event (remember, this method is called
      //  from a timer tick).

      // AS: For some reason, GCC won't let us pass this directly.
      wxMouseEvent e(wxEVT_MOTION);
      HandleSelect(e);
      mAutoScrolling = false;
   }
}

double TrackPanel::GetScreenEndTime() const
{
   int width;
   GetTracksUsableArea(&width, NULL);
   return mViewInfo->PositionToTime(width, 0, true);
}

/// AS: OnPaint( ) is called during the normal course of
///  completing a repaint operation.
void TrackPanel::OnPaint(wxPaintEvent & /* event */)
{
   mLastDrawnSelectedRegion = mViewInfo->selectedRegion;

#if DEBUG_DRAW_TIMING
   wxStopWatch sw;
#endif

   {
      wxPaintDC dc(this);

      // Retrieve the damage rectangle
      wxRect box = GetUpdateRegion().GetBox();

      // Recreate the backing bitmap if we have a full refresh
      // (See TrackPanel::Refresh())
      if (mRefreshBacking || (box == GetRect()))
      {
         // Reset (should a mutex be used???)
         mRefreshBacking = false;

         // Redraw the backing bitmap
         DrawTracks(&GetBackingDCForRepaint());

         // Copy it to the display
         DisplayBitmap(dc);
      }
      else
      {
         // Copy full, possibly clipped, damage rectangle
         RepairBitmap(dc, box.x, box.y, box.width, box.height);
      }

      // Done with the clipped DC

      // Drawing now goes directly to the client area.
      // DrawOverlays() may need to draw outside the clipped region.
      // (Used to make a NEW, separate wxClientDC, but that risks flashing
      // problems on Mac.)
      dc.DestroyClippingRegion();
      DrawOverlays(true, &dc);
   }

#if DEBUG_DRAW_TIMING
   sw.Pause();
   wxLogDebug(wxT("Total: %ld milliseconds"), sw.Time());
   wxPrintf(wxT("Total: %ld milliseconds\n"), sw.Time());
#endif
}

/// Makes our Parent (well, whoever is listening to us) push their state.
/// this causes application state to be preserved on a stack for undo ops.
void TrackPanel::MakeParentPushState(const wxString &desc, const wxString &shortDesc,
                                     UndoPush flags)
{
   mListener->TP_PushState(desc, shortDesc, flags);
}

void TrackPanel::MakeParentPushState(const wxString &desc, const wxString &shortDesc)
{
   MakeParentPushState(desc, shortDesc, UndoPush::AUTOSAVE);
}

void TrackPanel::MakeParentModifyState(bool bWantsAutoSave)
{
   mListener->TP_ModifyState(bWantsAutoSave);
}

void TrackPanel::MakeParentRedrawScrollbars()
{
   mListener->TP_RedrawScrollbars();
}

void TrackPanel::HandleInterruptedDrag()
{

   // Certain drags need to complete their effects before handling keystroke shortcut
   // commands:  those that have undoable editing effects.  For others, keystrokes are
   // harmless and we do nothing.
   switch (mMouseCapture)
   {
      case IsUncaptured:
      case IsVZooming:
      case IsSelecting:
      case IsSelectingLabelText:
      case IsResizing:
      case IsResizingBetweenLinkedTracks:
      case IsResizingBelowLinkedTracks:
      case IsMuting:
      case IsSoloing:
      case IsMinimizing:
      case IsPopping:
      case IsZooming:
         return;

      default:
         ;
   }

   /*
    So this includes the cases:

    IsClosing,
    IsAdjustingLabel,
    IsAdjustingSample,
    IsRearranging,
    IsSliding,
    IsEnveloping,
    IsGainSliding,
    IsPanSliding,
    WasOverCutLine,
    IsStretching
    */
   // The bogus id isn't used anywhere, but may help with debugging.
   // as this is sending a bogus mouse up.  The mouse button is still actually down
   // and may go up again.
   const int idBogusUp = 2;
   wxMouseEvent evt { wxEVT_LEFT_UP };
   evt.SetId( idBogusUp );
   evt.SetPosition(this->ScreenToClient(::wxGetMousePosition()));
   this->ProcessEvent(evt);
}

bool TrackPanel::HandleEscapeKey(bool down)
{
   if (!down)
      return false;

   switch (mMouseCapture)
   {
   case IsSelecting:
   {
      TrackListIterator iter(GetTracks());
      std::vector<bool>::const_iterator
         it = mInitialTrackSelection.begin(),
         end = mInitialTrackSelection.end();
      for (Track *t = iter.First(); t; t = iter.Next()) {
         wxASSERT(it != end);
         t->SetSelected(*it++);
      }
      mLastPickedTrack = mInitialLastPickedTrack;
      mViewInfo->selectedRegion = mInitialSelection;

      // Refresh mixer board for change of set of selected tracks
      if (MixerBoard* pMixerBoard = this->GetMixerBoard())
         pMixerBoard->Refresh();
   }
      break;
   case IsZooming:
   case IsVZooming:
   //case IsAdjustingSample:
      break;
   case IsResizing:
      mCapturedTrack->SetHeight(mInitialActualHeight);
      mCapturedTrack->SetMinimized(mInitialMinimized);
      break;
   case IsResizingBetweenLinkedTracks:
   {
      Track *const next = mTracks->GetNext(mCapturedTrack);
      mCapturedTrack->SetHeight(mInitialUpperActualHeight);
      mCapturedTrack->SetMinimized(mInitialMinimized);
      next->SetHeight(mInitialActualHeight);
      next->SetMinimized(mInitialMinimized);
   }
      break;
   case IsResizingBelowLinkedTracks:
   {
      Track *const prev = mTracks->GetPrev(mCapturedTrack);
      mCapturedTrack->SetHeight(mInitialActualHeight);
      mCapturedTrack->SetMinimized(mInitialMinimized);
      prev->SetHeight(mInitialUpperActualHeight);
      prev->SetMinimized(mInitialMinimized);
   }
      break;
   default:
   {
      // Not escaping from a mouse drag
      return false;
   }
   }

   // Common part in all cases that escape from a drag
   SetCapturedTrack(NULL, IsUncaptured);
   if (HasCapture())
      ReleaseMouse();
   wxMouseEvent dummy;
   HandleCursor(dummy);
   Refresh(false);

   return true;
}

void TrackPanel::HandleAltKey(bool down)
{
   mLastMouseEvent.m_altDown = down;
   HandleCursorForLastMouseEvent();
}

void TrackPanel::HandleShiftKey(bool down)
{
   mLastMouseEvent.m_shiftDown = down;
   HandleCursorForLastMouseEvent();
}

void TrackPanel::HandleControlKey(bool down)
{
   mLastMouseEvent.m_controlDown = down;
   HandleCursorForLastMouseEvent();
}

void TrackPanel::HandlePageUpKey()
{
   mListener->TP_ScrollWindow(2 * mViewInfo->h - GetScreenEndTime());
}

void TrackPanel::HandlePageDownKey()
{
   mListener->TP_ScrollWindow(GetScreenEndTime());
}

void TrackPanel::HandleCursorForLastMouseEvent()
{
   HandleCursor(mLastMouseEvent);
}

MixerBoard* TrackPanel::GetMixerBoard()
{
   AudacityProject *p = GetProject();
   wxASSERT(p);
   return p->GetMixerBoard();
}

/// Used to determine whether it is safe or not to perform certain
/// edits at the moment.
/// @return true if audio is being recorded or is playing.
bool TrackPanel::IsUnsafe()
{
   return IsAudioActive();
}

bool TrackPanel::IsAudioActive()
{
   AudacityProject *p = GetProject();
   return p->IsAudioActive();
}


/// Uses a previously noted 'activity' to determine what
/// cursor to use.
/// @var mMouseCapture holds the current activity.
bool TrackPanel::SetCursorByActivity( )
{
   bool unsafe = IsUnsafe();

   switch( mMouseCapture )
   {
   case IsSelecting:
      SetCursor(*mSelectCursor);
      return true;
   case IsSliding:
      SetCursor( unsafe ? *mDisabledCursor : *mSlideCursor);
      return true;
   case IsEnveloping:
      SetCursor( unsafe ? *mDisabledCursor : *mEnvelopeCursor);
      return true;
   case IsRearranging:
      SetCursor( unsafe ? *mDisabledCursor : *mRearrangeCursor);
      return true;
   case IsAdjustingLabel:
   case IsSelectingLabelText:
      return true;
#ifdef USE_MIDI
   case IsStretching:
      SetCursor( unsafe ? *mDisabledCursor : *mStretchCursor);
      return true;
#endif
   default:
      break;
   }
   return false;
}

bool TrackPanel::SetCursorForCutline(WaveTrack * track, wxRect &rect, const wxMouseEvent &event)
{
   if (IsOverCutline(track, rect, event)) {
      bool unsafe = IsUnsafe();
      SetCursor(unsafe ? *mDisabledCursor : *mArrowCursor);
      return true;
      // No tip string?
   }
   return false;
}

/// When in the "label" (TrackInfo or vertical ruler), we can either vertical zoom or re-order tracks.
/// Dont't change cursor/tip to zoom if display is not waveform (either linear of dB) or Spectrum
void TrackPanel::SetCursorAndTipWhenInLabel( Track * t,
         const wxMouseEvent &event, wxString &tip )
{
   if (event.m_x >= GetVRulerOffset() && (t->GetKind() == Track::Wave) )
   {
      tip = _("Click to vertically zoom in. Shift-click to zoom out. Drag to specify a zoom region.");
      SetCursor(event.ShiftDown()? *mZoomOutCursor : *mZoomInCursor);
   }
#ifdef USE_MIDI
   else if (event.m_x >= GetVRulerOffset() && t->GetKind() == Track::Note) {
      tip = _("Click to verticaly zoom in, Shift-click to zoom out, Drag to create a particular zoom region.");
      SetCursor(event.ShiftDown() ? *mZoomOutCursor : *mZoomInCursor);
   }
#endif
   else {
      // Set a status message if over TrackInfo.
      tip = _("Drag the track vertically to change the order of the tracks.");
      SetCursor(*mArrowCursor);
   }
}

/// When in the resize area we can adjust size or relative size.
void TrackPanel::SetCursorAndTipWhenInVResizeArea( bool bLinked, wxString &tip )
{
   // Check to see whether it is the first channel of a stereo track
   if (bLinked) {
      // If we are in the label we got here 'by mistake' and we're
      // not actually in the resize area at all.  (The resize area
      // is shorter when it is between stereo tracks).

      tip = _("Click and drag to adjust relative size of stereo tracks.");
      SetCursor(*mResizeCursor);
   } else {
      tip = _("Click and drag to resize the track.");
      SetCursor(*mResizeCursor);
   }
}

/// When in a label track, find out if we've hit anything that
/// would cause a cursor change.
void TrackPanel::SetCursorAndTipWhenInLabelTrack( LabelTrack * pLT,
       const wxMouseEvent & event, wxString &tip )
{
   int edge=pLT->OverGlyph(event.m_x, event.m_y);
   if(edge !=0)
   {
      SetCursor(*mArrowCursor);
   }

   //KLUDGE: We refresh the whole Label track when the icon hovered over
   //changes colouration.  As well as being inefficient we are also
   //doing stuff that should be delegated to the label track itself.
   edge += pLT->mbHitCenter ? 4:0;
   if( edge != pLT->mOldEdge )
   {
      pLT->mOldEdge = edge;
      RefreshTrack( pLT );
   }
   // IF edge!=0 THEN we've set the cursor and we're done.
   // signal this by setting the tip.
   if( edge != 0 )
   {
      tip =
         (pLT->mbHitCenter ) ?
         _("Drag one or more label boundaries.") :
         _("Drag label boundary.");
   }
}

namespace {

// This returns true if we're a spectral editing track.
inline bool isSpectralSelectionTrack(const Track *pTrack) {
   if (pTrack &&
       pTrack->GetKind() == Track::Wave) {
      const WaveTrack *const wt = static_cast<const WaveTrack*>(pTrack);
      const SpectrogramSettings &settings = wt->GetSpectrogramSettings();
      const int display = wt->GetDisplay();
      return (display == WaveTrack::Spectrum) && settings.SpectralSelectionEnabled();
   }
   else {
      return false;
   }
}

} // namespace

// If we're in OnDemand mode, we may change the tip.
void TrackPanel::MaySetOnDemandTip( Track * t, wxString &tip )
{
   wxASSERT( t );
   //For OD regions, we need to override and display the percent complete for this task.
   //first, make sure it's a wavetrack.
   if(t->GetKind() != Track::Wave)
      return;
   //see if the wavetrack exists in the ODManager (if the ODManager exists)
   if(!ODManager::IsInstanceCreated())
      return;
   //ask the wavetrack for the corresponding tip - it may not change tip, but that's fine.
   ODManager::Instance()->FillTipForWaveTrack(static_cast<WaveTrack*>(t), tip);
   return;
}

#ifdef EXPERIMENTAL_SPECTRAL_EDITING
void TrackPanel::HandleCenterFrequencyCursor
(bool shiftDown, wxString &tip, const wxCursor ** ppCursor)
{
#ifndef SPECTRAL_EDITING_ESC_KEY
   tip =
      shiftDown ?
      _("Click and drag to move center selection frequency.") :
      _("Click and drag to move center selection frequency to a spectral peak.");

#else
   shiftDown;

   tip =
      _("Click and drag to move center selection frequency.");

#endif

   *ppCursor = mEnvelopeCursor.get();
}

void TrackPanel::HandleCenterFrequencyClick
(bool shiftDown, const WaveTrack *wt, double value)
{
   if (shiftDown) {
      // Disable time selection
      mSelStartValid = false;
      mFreqSelTrack = wt;
      mFreqSelPin = value;
      mFreqSelMode = FREQ_SEL_DRAG_CENTER;
   }
   else {
#ifndef SPECTRAL_EDITING_ESC_KEY
      // Start center snapping
      // Turn center snapping on (the only way to do this)
      mFreqSelMode = FREQ_SEL_SNAPPING_CENTER;
      // Disable time selection
      mSelStartValid = false;
      StartSnappingFreqSelection(wt);
#endif
   }
}
#endif

// The select tool can have different cursors and prompts depending on what
// we hover over, most notably when hovering over the selction boundaries.
// Determine and set the cursor and tip accordingly.
void TrackPanel::SetCursorAndTipWhenSelectTool( Track * t,
        const wxMouseEvent & event, wxRect &rect, bool bMultiToolMode,
        wxString &tip, const wxCursor ** ppCursor )
{
   // Do not set the default cursor here and re-set later, that causes
   // flashing.
   *ppCursor = mSelectCursor.get();

   //In Multi-tool mode, give multitool prompt if no-special-hit.
   if( bMultiToolMode ) {
      // Look up the current key binding for Preferences.
      // (Don't assume it's the default!)
      wxString keyStr
         (GetProject()->GetCommandManager()->GetKeyFromName(wxT("Preferences")));
      if (keyStr.IsEmpty())
         // No keyboard preference defined for opening Preferences dialog
         /* i18n-hint: These are the names of a menu and a command in that menu */
         keyStr = _("Edit, Preferences...");
      else
         keyStr = KeyStringDisplay(keyStr);
      /* i18n-hint: %s is usually replaced by "Ctrl+P" for Windows/Linux, "Command+," for Mac */
      tip = wxString::Format(
         _("Multi-Tool Mode: %s for Mouse and Keyboard Preferences."),
         keyStr.c_str());
      // Later in this function we may point to some other string instead.
   }

   // Not over a track?  Get out of here.
   if(!t)
      return;

   //Make sure we are within the selected track
   // Adjusting the selection edges can be turned off in
   // the preferences...
   if ( !t->GetSelected() || !mAdjustSelectionEdges)
   {
      MaySetOnDemandTip( t, tip );
      return;
   }

   {
      wxInt64 leftSel = mViewInfo->TimeToPosition(mViewInfo->selectedRegion.t0(), rect.x);
      wxInt64 rightSel = mViewInfo->TimeToPosition(mViewInfo->selectedRegion.t1(), rect.x);
      // Something is wrong if right edge comes before left edge
      wxASSERT(!(rightSel < leftSel));
   }

   const bool bShiftDown = event.ShiftDown();

#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   if ( (mFreqSelMode == FREQ_SEL_SNAPPING_CENTER) &&
      isSpectralSelectionTrack(t)) {
      // Not shift-down, but center frequency snapping toggle is on
      tip = _("Click and drag to set frequency bandwidth.");
      *ppCursor = mEnvelopeCursor.get();
      return;
   }
#endif

   // If not shift-down and not snapping center, then
   // choose boundaries only in snapping tolerance,
   // and may choose center.
   SelectionBoundary boundary =
        ChooseBoundary(event, t, rect, !bShiftDown, !bShiftDown);

#ifdef USE_MIDI
   // The MIDI HitTest will only succeed if we are on a midi track, so 
   // typically we will fall through.
   switch( boundary) {
      case SBNone:
      case SBLeft:
      case SBRight:
         if ( HitTestStretch(t, rect, event)) {
            tip = _("Click and drag to stretch within selected region.");
            *ppCursor = mStretchCursor.get();
            return;
         }
         break;
      default:
         break;
   }
#endif

   switch (boundary) {
      case SBNone:
         if( bShiftDown ){
            // wxASSERT( false );
            // Same message is used for moving left right top or bottom edge.
            tip = _("Click to move selection boundary to cursor.");
            // No cursor change.
            return;
         }
         break;
      case SBLeft:
         tip = _("Click and drag to move left selection boundary.");
         *ppCursor = mAdjustLeftSelectionCursor.get();
         return;
      case SBRight:
         tip = _("Click and drag to move right selection boundary.");
         *ppCursor = mAdjustRightSelectionCursor.get();
         return;
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
      case SBBottom:
         tip = _("Click and drag to move bottom selection frequency.");
         *ppCursor = mBottomFrequencyCursor.get();
         return;
      case SBTop:
         tip = _("Click and drag to move top selection frequency.");
         *ppCursor = mTopFrequencyCursor.get();
         return;
      case SBCenter:
         HandleCenterFrequencyCursor(bShiftDown, tip, ppCursor);
         return;
      case SBWidth:
         tip = _("Click and drag to adjust frequency bandwidth.");
         *ppCursor = mBandWidthCursor.get();
         return;
#endif
      default:
         wxASSERT(false);
   } // switch
   // Falls through the switch if there was no boundary found.

   MaySetOnDemandTip( t, tip );
}

/// In this method we know what tool we are using,
/// so set the cursor accordingly.
void TrackPanel::SetCursorAndTipByTool( int tool,
         const wxMouseEvent & event, wxString& )
{
   bool unsafe = IsUnsafe();

   // Change the cursor based on the active tool.
   switch (tool) {
   case selectTool:
      wxFAIL;// should have already been handled
      break;
   case envelopeTool:
      SetCursor(unsafe ? *mDisabledCursor : *mEnvelopeCursor);
      break;
   case slideTool:
      SetCursor(unsafe ? *mDisabledCursor : *mSlideCursor);
      break;
   case zoomTool:
      SetCursor(event.ShiftDown()? *mZoomOutCursor : *mZoomInCursor);
      break;
   case drawTool:
      if (unsafe)
         SetCursor(*mDisabledCursor);
      else
         SetCursor(event.AltDown()? *mSmoothCursor : *mPencilCursor);
      break;
   }
   // doesn't actually change the tip itself, but it could (should?) do at some
   // future date.
}

///  TrackPanel::HandleCursor( ) sets the cursor drawn at the mouse location.
///  As this procedure checks which region the mouse is over, it is
///  appropriate to establish the message in the status bar.
void TrackPanel::HandleCursor(const wxMouseEvent & event)
{
   mLastMouseEvent = event;

   // (1), If possible, set the cursor based on the current activity
   //      ( leave the StatusBar alone ).
   if( SetCursorByActivity() )
      return;

   // (2) If we are not over a track at all, set the cursor to Arrow and
   //     clear the StatusBar,
   wxRect labelRect, trackRect;
   Track *const label = FindTrack(event.m_x, event.m_y, true, true, &labelRect);
   Track *const nonlabel = FindTrack(event.m_x, event.m_y, false, false, &trackRect);
   Track *const track = label ? label : nonlabel;

   if (!track) {
      SetCursor(*mArrowCursor);
      mListener->TP_DisplayStatusMessage(wxT(""));
      return;
   }

   // (3) The easy cases are done.
   // Now we've got to hit-test against a number of different possibilities.
   // We could be over the label or a vertical ruler etc...

   // Strategy here is to set the tip when we have determined and
   // set the correct cursor.  We stop doing tests for what we've
   // hit once the tip is not NULL.

   wxString tip;

   // Are we within the vertical resize area?
   if (nonlabel
      ? within(event.m_y, trackRect.y + trackRect.height, TRACK_RESIZE_REGION)
      : within(event.m_y, labelRect.y + labelRect.height, TRACK_RESIZE_REGION))
   {
      SetCursorAndTipWhenInVResizeArea(nonlabel && track->GetLinked(), tip);
      // tip may still be NULL at this point, in which case we go on looking.
   }

   if ((tip == wxString()) && label) {
      SetCursorAndTipWhenInLabel( label, event, tip );
   }

   // Otherwise, we must be over a track of some kind
   // Is it a label track?
   if ((tip == wxString()) && (track->GetKind() == Track::Label))
   {
      // We are over a label track
      SetCursorAndTipWhenInLabelTrack( static_cast<LabelTrack*>(track), event, tip );
      // ..and if we haven't yet determined the cursor,
      // we go on to do all the standard track hit tests.
   }

   if ((tip == wxString()) &&
      nonlabel &&
      nonlabel->GetKind() == Track::Wave &&
      SetCursorForCutline(static_cast<WaveTrack*>(nonlabel), trackRect, event))
      return;

   if( tip == wxString() )
   {
      ToolsToolBar * ttb = mListener->TP_GetToolsToolBar();
      if( ttb == NULL )
         return;
      // JKC: DetermineToolToUse is called whenever the mouse
      // moves.  I had some worries about calling it when in
      // multimode as it then has to hit-test all 'objects' in
      // the track panel, but performance seems fine in
      // practice (on a P500).
      int tool = DetermineToolToUse( ttb, event );

      tip = GetProject()->GetScrubber().StatusMessageForWave();
      if( tip.IsEmpty() )
         tip = ttb->GetMessageForTool(tool);

      if( tool != selectTool )
      {
         // We don't include the select tool in
         // SetCursorAndTipByTool() because it's more complex than
         // the other tool cases.
         SetCursorAndTipByTool( tool, event, tip);
      }
      else
      {
         bool bMultiToolMode = ttb->IsDown(multiTool);
         const wxCursor *pSelection = 0;
         SetCursorAndTipWhenSelectTool
            ( track, event, trackRect, bMultiToolMode, tip, &pSelection );
         if (pSelection)
            // Set cursor once only here, to avoid flashing during drags
            SetCursor(*pSelection);
      }
   }

   if (tip != wxString())
      mListener->TP_DisplayStatusMessage(tip);
}


/// This method handles various ways of starting and extending
/// selections.  These are the selections you make by clicking and
/// dragging over a waveform.
void TrackPanel::HandleSelect(wxMouseEvent & event)
{
   wxRect rect;
   Track *t = FindTrack(event.m_x, event.m_y, false, false, &rect);

   // AS: Ok, did the user just click the mouse, release the mouse,
   //  or drag?
   if (event.LeftDown() ||
      (event.LeftDClick() && event.CmdDown())) {
      // AS: Now, did they click in a track somewhere?  If so, we want
      //  to extend the current selection or start a NEW selection,
      //  depending on the shift key.  If not, cancel all selections.
      if (t)
         SelectionHandleClick(event, t, rect);
   } else if (event.LeftUp() || event.RightUp()) {
      mSnapManager.reset();
      // Do not draw yellow lines
      if (mSnapLeft != -1 || mSnapRight != -1) {
         mSnapLeft = mSnapRight = -1;
         Refresh(false);
      }

      SetCapturedTrack( NULL );
      //Send the NEW selection state to the undo/redo stack:
      MakeParentModifyState(false);

#ifdef EXPERIMENTAL_SPECTRAL_EDITING
      // This stops center snapping with mouse movement
      mFreqSelMode = FREQ_SEL_INVALID;
#endif

   } else if (event.LeftDClick() && !event.ShiftDown()) {
      if (!mCapturedTrack) {
         wxRect rect;
         mCapturedTrack =
            FindTrack(event.m_x, event.m_y, false, false, &rect);
         if (!mCapturedTrack)
            return;
      }

      // Deselect all other tracks and select this one.
      SelectNone();

      SelectTrack(mCapturedTrack, true);

      // Default behavior: select whole track
      SelectTrackLength(mCapturedTrack);

      // Special case: if we're over a clip in a WaveTrack,
      // select just that clip
      if (mCapturedTrack->GetKind() == Track::Wave) {
         WaveTrack *w = (WaveTrack *)mCapturedTrack;
         WaveClip *selectedClip = w->GetClipAtX(event.m_x);
         if (selectedClip) {
            mViewInfo->selectedRegion.setTimes(
               selectedClip->GetOffset(), selectedClip->GetEndTime());
         }
         //Also, capture this track for dragging until we up-click.
         mCapturedClipArray.push_back(TrackClip(w, selectedClip));

         mMouseCapture = IsSliding;

         Refresh(false);
         StartSlide(event);
         goto done;
      }

      Refresh(false);
      SetCapturedTrack( NULL );
      MakeParentModifyState(false);
   }
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
#ifdef SPECTRAL_EDITING_ESC_KEY
   else if (!event.IsButton() &&
            mFreqSelMode == FREQ_SEL_SNAPPING_CENTER &&
            !mViewInfo->selectedRegion.isPoint())
      MoveSnappingFreqSelection(event.m_y, rect.y, rect.height, t);
#endif
#endif
 done:
   SelectionHandleDrag(event, t);
}

void TrackPanel::SelectTrack(Track *pTrack, bool selected, bool updateLastPicked)
{
   bool wasCorrect = (selected == pTrack->GetSelected());

   if (selected) {
      // This handles the case of linked tracks, selecting all channels
      mTracks->Select(pTrack, true);
      if (updateLastPicked)
         mLastPickedTrack = pTrack;
   }
   else {
      mTracks->Select(pTrack, false);
      if (updateLastPicked && pTrack == mLastPickedTrack)
         mLastPickedTrack = nullptr;
   }

   // Update mixer board, but only as needed so it does not flicker.
   if (!wasCorrect) {
      MixerBoard* pMixerBoard = this->GetMixerBoard();
      if (pMixerBoard && (pTrack->GetKind() == Track::Wave))
         pMixerBoard->RefreshTrackCluster(static_cast<WaveTrack*>(pTrack));
   }
}

void TrackPanel::SelectRangeOfTracks(Track *sTrack, Track *eTrack)
{
   if (eTrack) {
      // Swap the track pointers if needed
      if (eTrack->GetIndex() < sTrack->GetIndex())
         std::swap(sTrack, eTrack);

      TrackListIterator iter(GetTracks());
      sTrack = iter.StartWith(sTrack);
      do {
         SelectTrack(sTrack, true, false);
         if (sTrack == eTrack) {
            break;
         }

         sTrack = iter.Next();
      } while (sTrack);
   }
}

void TrackPanel::ChangeSelectionOnShiftClick(Track * pTrack){

   // Optional: Track already selected?  Nothing to do.
   // If we enable this, Shift-Click behaves like click in this case.
   //if( pTrack->GetSelected() )
   //   return;

   // Find first and last selected track.
   Track* pFirst = nullptr;
   Track* pLast = nullptr;
   // We will either extend from the first or from the last.
   Track* pExtendFrom= nullptr;

   TrackListIterator iter(GetTracks());
   for (Track *t = iter.First(); t; t = iter.Next()) {
      const bool isSelected = t->GetSelected();
      // If our track is after the first, extend from the first.
      if( t == pTrack ){
         pExtendFrom = pFirst;
      }
      // Record first and last selected.
      if( isSelected ){
         if( !pFirst )
            pFirst = t;
         pLast = t;
      }
   }
   // Our track was the first or earlier.  Extend from the last.
   if( !pExtendFrom )
      pExtendFrom = pLast;

   SelectNone();
   if( pExtendFrom )
      SelectRangeOfTracks(pTrack, pExtendFrom);
   else
      SelectTrack( pTrack, true );
}

/// This method gets called when we're handling selection
/// and the mouse was just clicked.
void TrackPanel::SelectionHandleClick(wxMouseEvent & event,
                                      Track * pTrack, wxRect rect)
{
   mCapturedTrack = pTrack;
   rect.y += kTopMargin;
   rect.height -= kTopMargin + kBottomMargin;
   mCapturedRect = rect;

   mMouseCapture=IsSelecting;
   mInitialSelection = mViewInfo->selectedRegion;
   mInitialLastPickedTrack = mLastPickedTrack;

   // Save initial state of track selections
   mInitialTrackSelection.clear();
   {
      TrackListIterator iter(GetTracks());
      for (Track *t = iter.First(); t; t = iter.Next()) {
         const bool isSelected = t->GetSelected();
         mInitialTrackSelection.push_back(isSelected);
      }
   }

   // We create a NEW snap manager in case any snap-points have changed
   mSnapManager = std::make_unique<SnapManager>(GetTracks(), mViewInfo);

   mSnapLeft = -1;
   mSnapRight = -1;

#ifdef USE_MIDI
   mStretching = false;
   bool stretch = HitTestStretch(pTrack, rect, event);
#endif

   bool bShiftDown = event.ShiftDown();
   bool bCtrlDown = event.ControlDown();
   if (bShiftDown || (bCtrlDown

#ifdef USE_MIDI
       && !stretch
#endif
   )) {

      if( bShiftDown )
         ChangeSelectionOnShiftClick( pTrack );
      if( bCtrlDown ){
         //bool bIsSelected = pTrack->GetSelected();
         bool bIsSelected = false;
         // could set bIsSelected true here, but toggling is more technically correct.
         // if we want to match behaviour in Track Control Panel.
         SelectTrack( pTrack, !bIsSelected, false );
      }

      double value;
      // Shift-click, choose closest boundary
      SelectionBoundary boundary =
         ChooseBoundary(event, pTrack, rect, false, false, &value);
      switch (boundary) {
         case SBLeft:
         case SBRight:
         {
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
            // If drag starts, change time selection only
            // (also exit frequency snapping)
            mFreqSelMode = FREQ_SEL_INVALID;
#endif
            mSelStartValid = true;
            mSelStart = value;
            ExtendSelection(event.m_x, rect.x, pTrack);
            break;
         }
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
         case SBBottom:
         case SBTop:
         {
            // Reach this case only for wave tracks
            mFreqSelTrack = static_cast<const WaveTrack *>(pTrack);
            mFreqSelPin = value;
            mFreqSelMode =
               (boundary == SBBottom)
               ? FREQ_SEL_BOTTOM_FREE : FREQ_SEL_TOP_FREE;

            // Drag frequency only, not time:
            mSelStartValid = false;
            ExtendFreqSelection(event.m_y, rect.y, rect.height);
            break;
         }
         case SBCenter:
            HandleCenterFrequencyClick(true,
               // Reach this case only for wave tracks
               static_cast<const WaveTrack *>(pTrack), value);
            break;
#endif
         default:
            wxASSERT(false);
      };

      UpdateSelectionDisplay();
      // For persistence of the selection change:
      MakeParentModifyState(false);
      return;
   }

   else if(event.CmdDown()
#ifdef USE_MIDI
      && !stretch
#endif
   ) {

      // Used to jump the play head, but it is redundant with timeline quick play
      // StartOrJumpPlayback(event);

      // Not starting a drag
      SetCapturedTrack(NULL, IsUncaptured);
      return;
   }

   //Make sure you are within the selected track
   bool startNewSelection = true;
   if (pTrack && pTrack->GetSelected()) {
      // Adjusting selection edges can be turned off in the
      // preferences now
      if (mAdjustSelectionEdges) {
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
         if (mFreqSelMode == FREQ_SEL_SNAPPING_CENTER &&
            isSpectralSelectionTrack(pTrack)) {
            // Ignore whether we are inside the time selection.
            // Exit center-snapping, start dragging the width.
            mFreqSelMode = FREQ_SEL_PINNED_CENTER;
            mFreqSelTrack = static_cast<WaveTrack*>(pTrack);
            mFreqSelPin = mViewInfo->selectedRegion.fc();
            // Do not adjust time boundaries
            mSelStartValid = false;
            ExtendFreqSelection(event.m_y, rect.y, rect.height);
            UpdateSelectionDisplay();
            // Frequency selection persists too, so do this:
            MakeParentModifyState(false);

            return;
         }
      else
#endif
         {
            // Not shift-down, choose boundary only within snapping
            double value;
            SelectionBoundary boundary =
               ChooseBoundary(event, pTrack, rect, true, true, &value);
            switch (boundary) {
            case SBNone:
               // startNewSelection remains true
               break;
            case SBLeft:
            case SBRight:
               startNewSelection = false;
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
               // Disable frequency selection
               mFreqSelMode = FREQ_SEL_INVALID;
#endif
               mSelStartValid = true;
               mSelStart = value;
               break;
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
            case SBBottom:
            case SBTop:
            case SBWidth:
               startNewSelection = false;
               // Disable time selection
               mSelStartValid = false;
               // Reach this case only for wave tracks
               mFreqSelTrack = static_cast<const WaveTrack*>(pTrack);
               mFreqSelPin = value;
               mFreqSelMode =
                  (boundary == SBWidth) ? FREQ_SEL_PINNED_CENTER :
                  (boundary == SBBottom) ? FREQ_SEL_BOTTOM_FREE :
                  FREQ_SEL_TOP_FREE;
               break;
            case SBCenter:
               HandleCenterFrequencyClick(false,
                  // Reach this case only for wave track
                  static_cast<const WaveTrack *>(pTrack), value);
               startNewSelection = false;
               break;
#endif
            default:
               wxASSERT(false);
            }
         }
      } // mAdjustSelectionEdges
   }

#ifdef USE_MIDI
   if (stretch) {
      // stretch is true only when pTrack is note
      const auto nt = static_cast<NoteTrack *>(pTrack);
      // find nearest beat to sel0, sel1
      double minPeriod = 0.05; // minimum beat period
      double qBeat0, qBeat1;
      double centerBeat = 0.0f;
      mStretchSel0 = nt->NearestBeatTime(mViewInfo->selectedRegion.t0(), &qBeat0);
      mStretchSel1 = nt->NearestBeatTime(mViewInfo->selectedRegion.t1(), &qBeat1);

      // If there is not (almost) a beat to stretch that is slower
      // than 20 beats per second, don't stretch
      if (within(qBeat0, qBeat1, 0.9) ||
          (mStretchSel1 - mStretchSel0) / (qBeat1 - qBeat0) < minPeriod) return;

      if (startNewSelection) { // mouse is not at an edge, but after
         // quantization, we could be indicating the selection edge
         mSelStartValid = true;
         mSelStart = std::max(0.0, mViewInfo->PositionToTime(event.m_x, rect.x));
         mStretchStart = nt->NearestBeatTime(mSelStart, &centerBeat);
         if (within(qBeat0, centerBeat, 0.1)) {
            mListener->TP_DisplayStatusMessage(
                    _("Click and drag to stretch selected region."));
            SetCursor(*mStretchLeftCursor);
            // mStretchMode = stretchLeft;
            mSelStart = mViewInfo->selectedRegion.t1();
            // condition that implies stretchLeft
            startNewSelection = false;
         } else if (within(qBeat1, centerBeat, 0.1)) {
            mListener->TP_DisplayStatusMessage(
                    _("Click and drag to stretch selected region."));
            SetCursor(*mStretchRightCursor);
            // mStretchMode = stretchRight;
            mSelStart = mViewInfo->selectedRegion.t0();
            // condition that implies stretchRight
            startNewSelection = false;
         }
      }

      if (startNewSelection) {
         mStretchMode = stretchCenter;
         mStretchLeftBeats = qBeat1 - centerBeat;
         mStretchRightBeats = centerBeat - qBeat0;
      } else if (mSelStartValid && mViewInfo->selectedRegion.t1() == mSelStart) {
         // note that at this point, mSelStart is at the opposite
         // end of the selection from the cursor. If the cursor is
         // over sel0, then mSelStart is at sel1.
         mStretchMode = stretchLeft;
      } else {
         mStretchMode = stretchRight;
      }

      if (mStretchMode == stretchLeft) {
         mStretchLeftBeats = 0;
         mStretchRightBeats = qBeat1 - qBeat0;
      } else if (mStretchMode == stretchRight) {
         mStretchLeftBeats = qBeat1 - qBeat0;
         mStretchRightBeats = 0;
      }
      mViewInfo->selectedRegion.setTimes(mStretchSel0, mStretchSel1);
      mStretching = true;
      mStretched = false;

      /* i18n-hint: (noun) The track that is used for MIDI notes which can be
      dragged to change their duration.*/
      MakeParentPushState(_("Stretch Note Track"),
      /* i18n-hint: In the history list, indicates a MIDI note has
      been dragged to change its duration (stretch it). Using either past
      or present tense is fine here.  If unsure, go for whichever is
      shorter.*/
      _("Stretch"));

      // Full refresh since the label area may need to indicate
      // newly selected tracks. (I'm really not sure if the label area
      // needs to be refreshed or how to just refresh non-label areas.-RBD)
      Refresh(false);

      // Make sure the ruler follows suit.
      mRuler->DrawSelection();

      // As well as the SelectionBar.
      DisplaySelection();

      return;
   }
#endif
   if (startNewSelection) {
      // If we didn't move a selection boundary, start a NEW selection
      SelectNone();
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
      StartFreqSelection (event.m_y, rect.y, rect.height, pTrack);
#endif
      StartSelection(event.m_x, rect.x);
      SelectTrack(pTrack, true);
      SetFocusedTrack(pTrack);
      //On-Demand: check to see if there is an OD thing associated with this track.
      if (pTrack->GetKind() == Track::Wave) {
         if(ODManager::IsInstanceCreated())
            ODManager::Instance()->DemandTrackUpdate((WaveTrack*)pTrack,mSelStart);
      }
      DisplaySelection();
   }
}


/// Reset our selection markers.
void TrackPanel::StartSelection(int mouseXCoordinate, int trackLeftEdge)
{
   mSelStartValid = true;
   mSelStart = std::max(0.0, mViewInfo->PositionToTime(mouseXCoordinate, trackLeftEdge));

   double s = mSelStart;

   if (mSnapManager) {
      mSnapLeft = -1;
      mSnapRight = -1;
      bool snappedPoint, snappedTime;
      if (mSnapManager->Snap(mCapturedTrack, mSelStart, false,
                             &s, &snappedPoint, &snappedTime)) {
         if (snappedPoint)
            mSnapLeft = mViewInfo->TimeToPosition(s, trackLeftEdge);
      }
   }

   mViewInfo->selectedRegion.setTimes(s, s);

   SonifyBeginModifyState();
   MakeParentModifyState(false);
   SonifyEndModifyState();
}

/// Extend the existing selection
void TrackPanel::ExtendSelection(int mouseXCoordinate, int trackLeftEdge,
                                 Track *pTrack)
{
   if (!mSelStartValid)
      // Must be dragging frequency bounds only.
      return;

   double selend = std::max(0.0, mViewInfo->PositionToTime(mouseXCoordinate, trackLeftEdge));
   clip_bottom(selend, 0.0);

   double origSel0, origSel1;
   double sel0, sel1;

   if (pTrack == NULL && mCapturedTrack != NULL)
      pTrack = mCapturedTrack;

   if (mSelStart < selend) {
      sel0 = mSelStart;
      sel1 = selend;
   }
   else {
      sel1 = mSelStart;
      sel0 = selend;
   }

   origSel0 = sel0;
   origSel1 = sel1;

   if (mSnapManager) {
      mSnapLeft = -1;
      mSnapRight = -1;
      bool snappedPoint, snappedTime;
      if (mSnapManager->Snap(mCapturedTrack, sel0, false,
                             &sel0, &snappedPoint, &snappedTime)) {
         if (snappedPoint)
            mSnapLeft = mViewInfo->TimeToPosition(sel0, trackLeftEdge);
      }
      if (mSnapManager->Snap(mCapturedTrack, sel1, true,
                             &sel1, &snappedPoint, &snappedTime)) {
         if (snappedPoint)
            mSnapRight = mViewInfo->TimeToPosition(sel1, trackLeftEdge);
      }

      // Check if selection endpoints are too close together to snap (unless
      // using snap-to-time -- then we always accept the snap results)
      if (mSnapLeft >= 0 && mSnapRight >= 0 && mSnapRight - mSnapLeft < 3 &&
            !snappedTime) {
         sel0 = origSel0;
         sel1 = origSel1;
         mSnapLeft = -1;
         mSnapRight = -1;
      }
   }

   mViewInfo->selectedRegion.setTimes(sel0, sel1);

   //On-Demand: check to see if there is an OD thing associated with this track.  If so we want to update the focal point for the task.
   if (pTrack && (pTrack->GetKind() == Track::Wave) && ODManager::IsInstanceCreated())
      ODManager::Instance()->DemandTrackUpdate((WaveTrack*)pTrack,sel0); //sel0 is sometimes less than mSelStart
}

void TrackPanel::UpdateSelectionDisplay()
{
   // Full refresh since the label area may need to indicate
   // newly selected tracks.
   Refresh(false);

   // Make sure the ruler follows suit.
   mRuler->DrawSelection();

   // As well as the SelectionBar.
   DisplaySelection();
}

void TrackPanel::UpdateAccessibility()
{
   if (mAx)
      mAx->Updated();
}

#ifdef EXPERIMENTAL_SPECTRAL_EDITING
namespace {
   
inline double findMaxRatio(double center, double rate)
{
   const double minFrequency = 1.0;
   const double maxFrequency = (rate / 2.0);
   const double frequency =
      std::min(maxFrequency,
      std::max(minFrequency, center));
   return
      std::min(frequency / minFrequency, maxFrequency / frequency);
}

}

void TrackPanel::SnapCenterOnce(const WaveTrack *pTrack, bool up)
{
   const SpectrogramSettings &settings = pTrack->GetSpectrogramSettings();
   const auto windowSize = settings.GetFFTLength();
   const double rate = pTrack->GetRate();
   const double nyq = rate / 2.0;
   const double binFrequency = rate / windowSize;

   double f1 = mViewInfo->selectedRegion.f1();
   double centerFrequency = mViewInfo->selectedRegion.fc();
   if (centerFrequency <= 0) {
      centerFrequency = up ? binFrequency : nyq;
      f1 = centerFrequency * sqrt(2.0);
   }

   const double ratio = f1 / centerFrequency;
   const int originalBin = floor(0.5 + centerFrequency / binFrequency);
   const int limitingBin = up ? floor(0.5 + nyq / binFrequency) : 1;

   // This is crude and wasteful, doing the FFT each time the command is called.
   // It would be better to cache the data, but then invalidation of the cache would
   // need doing in all places that change the time selection.
   StartSnappingFreqSelection(pTrack);
   double snappedFrequency = centerFrequency;
   int bin = originalBin;
   if (up) {
      while (snappedFrequency <= centerFrequency &&
             bin < limitingBin)
         snappedFrequency = mFrequencySnapper->FindPeak(++bin * binFrequency, NULL);
   }
   else {
      while (snappedFrequency >= centerFrequency &&
         bin > limitingBin)
         snappedFrequency = mFrequencySnapper->FindPeak(--bin * binFrequency, NULL);
   }

   mViewInfo->selectedRegion.setFrequencies
      (snappedFrequency / ratio, snappedFrequency * ratio);
}

void TrackPanel::StartSnappingFreqSelection (const WaveTrack *pTrack)
{
   static const size_t minLength = 8;

   const double rate = pTrack->GetRate();

   // Grab samples, just for this track, at these times
   std::vector<float> frequencySnappingData;
   const auto start =
      pTrack->TimeToLongSamples(mViewInfo->selectedRegion.t0());
   const auto end =
      pTrack->TimeToLongSamples(mViewInfo->selectedRegion.t1());
   const auto length =
      std::min(frequencySnappingData.max_size(),
         limitSampleBufferSize( 10485760, // as in FreqWindow.cpp
                               end - start ));
   const auto effectiveLength = std::max(minLength, length);
   frequencySnappingData.resize(effectiveLength, 0.0f);
   pTrack->Get(
      reinterpret_cast<samplePtr>(&frequencySnappingData[0]),
      floatSample, start, length);

   // Use same settings as are now used for spectrogram display,
   // except, shrink the window as needed so we get some answers

   const SpectrogramSettings &settings = pTrack->GetSpectrogramSettings();
   auto windowSize = settings.GetFFTLength();

   while(windowSize > effectiveLength)
      windowSize >>= 1;
   const int windowType = settings.windowType;

   mFrequencySnapper->Calculate(
      SpectrumAnalyst::Spectrum, windowType, windowSize, rate,
      &frequencySnappingData[0], length);

   // We can now throw away the sample data but we keep the spectrum.
}

void TrackPanel::MoveSnappingFreqSelection (int mouseYCoordinate,
                                            int trackTopEdge,
                                            int trackHeight, Track *pTrack)
{
   if (pTrack &&
       pTrack->GetSelected() &&
       isSpectralSelectionTrack(pTrack)) {
      // Spectral selection track is always wave
      const auto wt = static_cast<const WaveTrack *>(pTrack);
      // PRL:
      // What happens if center snapping selection began in one spectrogram track,
      // then continues inside another?  We do not then recalculate
      // the spectrum (as was done in StartSnappingFreqSelection)
      // but snap according to the peaks in the old track.
      // I am not worrying about that odd case.
      const double rate = wt->GetRate();
      const double frequency =
         PositionToFrequency(wt, false, mouseYCoordinate,
         trackTopEdge, trackHeight);
      const double snappedFrequency =
         mFrequencySnapper->FindPeak(frequency, NULL);
      const double maxRatio = findMaxRatio(snappedFrequency, rate);
      double ratio = 2.0; // An arbitrary octave on each side, at most
      {
         const double f0 = mViewInfo->selectedRegion.f0();
         const double f1 = mViewInfo->selectedRegion.f1();
         if (f1 >= f0 && f0 >= 0)
            // Preserve already chosen ratio instead
            ratio = sqrt(f1 / f0);
      }
      ratio = std::min(ratio, maxRatio);

      mFreqSelPin = snappedFrequency;
      mViewInfo->selectedRegion.setFrequencies(
         snappedFrequency / ratio, snappedFrequency * ratio);

      mFreqSelTrack = wt;
      // SelectNone();
      // SelectTrack(pTrack, true);
      SetFocusedTrack(pTrack);
   }
}

void TrackPanel::StartFreqSelection (int mouseYCoordinate, int trackTopEdge,
                                     int trackHeight, Track *pTrack)
{
   mFreqSelTrack = NULL;
   mFreqSelMode = FREQ_SEL_INVALID;
   mFreqSelPin = SelectedRegion::UndefinedFrequency;

   if (isSpectralSelectionTrack(pTrack)) {
      // Spectral selection track is always wave
      mFreqSelTrack = static_cast<const WaveTrack *>(pTrack);
      mFreqSelMode = FREQ_SEL_FREE;
      mFreqSelPin =
         PositionToFrequency(mFreqSelTrack, false, mouseYCoordinate,
                             trackTopEdge, trackHeight);
      mViewInfo->selectedRegion.setFrequencies(mFreqSelPin, mFreqSelPin);
   }
}

void TrackPanel::ExtendFreqSelection(int mouseYCoordinate, int trackTopEdge,
                                     int trackHeight)
{
   // When dragWidth is true, and not dragging the center,
   // adjust both top and bottom about geometric mean.

   if (mFreqSelMode == FREQ_SEL_INVALID ||
       mFreqSelMode == FREQ_SEL_SNAPPING_CENTER)
      return;

   // Extension happens only when dragging in the same track in which we
   // started, and that is of a spectrogram display type.

   const WaveTrack* wt = mFreqSelTrack;
   const double rate =  wt->GetRate();
   const double frequency =
      PositionToFrequency(wt, true, mouseYCoordinate,
         trackTopEdge, trackHeight);

   // Dragging center?
   if (mFreqSelMode == FREQ_SEL_DRAG_CENTER) {
      if (frequency == rate || frequency < 1.0)
         // snapped to top or bottom
         mViewInfo->selectedRegion.setFrequencies(
            SelectedRegion::UndefinedFrequency,
            SelectedRegion::UndefinedFrequency);
      else {
         // mFreqSelPin holds the ratio of top to center
         const double maxRatio = findMaxRatio(frequency, rate);
         const double ratio = std::min(maxRatio, mFreqSelPin);
         mViewInfo->selectedRegion.setFrequencies(
            frequency / ratio, frequency * ratio);
      }
   }
   else if (mFreqSelMode == FREQ_SEL_PINNED_CENTER) {
      if (mFreqSelPin >= 0) {
         // Change both upper and lower edges leaving centre where it is.
         if (frequency == rate || frequency < 1.0)
            // snapped to top or bottom
            mViewInfo->selectedRegion.setFrequencies(
               SelectedRegion::UndefinedFrequency,
               SelectedRegion::UndefinedFrequency);
         else {
            // Given center and mouse position, find ratio of the larger to the
            // smaller, limit that to the frequency scale bounds, and adjust
            // top and bottom accordingly.
            const double maxRatio = findMaxRatio(mFreqSelPin, rate);
            double ratio = frequency / mFreqSelPin;
            if (ratio < 1.0)
               ratio = 1.0 / ratio;
            ratio = std::min(maxRatio, ratio);
            mViewInfo->selectedRegion.setFrequencies(
               mFreqSelPin / ratio, mFreqSelPin * ratio);
         }
      }
   }
   else {
      // Dragging of upper or lower.
      const bool bottomDefined =
         !(mFreqSelMode == FREQ_SEL_TOP_FREE && mFreqSelPin < 0);
      const bool topDefined =
         !(mFreqSelMode == FREQ_SEL_BOTTOM_FREE && mFreqSelPin < 0);
      if (!bottomDefined || (topDefined && mFreqSelPin < frequency)) {
         // Adjust top
         if (frequency == rate)
            // snapped high; upper frequency is undefined
            mViewInfo->selectedRegion.setF1(SelectedRegion::UndefinedFrequency);
         else
            mViewInfo->selectedRegion.setF1(std::max(1.0, frequency));

         mViewInfo->selectedRegion.setF0(mFreqSelPin);
      }
      else {
         // Adjust bottom
         if (frequency < 1.0)
            // snapped low; lower frequency is undefined
            mViewInfo->selectedRegion.setF0(SelectedRegion::UndefinedFrequency);
         else
            mViewInfo->selectedRegion.setF0(std::min(rate / 2.0, frequency));

         mViewInfo->selectedRegion.setF1(mFreqSelPin);
      }
   }
}

void TrackPanel::ToggleSpectralSelection()
{
   SelectedRegion &region = mViewInfo->selectedRegion;
   const double f0 = region.f0();
   const double f1 = region.f1();
   const bool haveSpectralSelection =
      !(f0 == SelectedRegion::UndefinedFrequency &&
        f1 == SelectedRegion::UndefinedFrequency);
   if (haveSpectralSelection)
   {
      mLastF0 = f0;
      mLastF1 = f1;
      region.setFrequencies
         (SelectedRegion::UndefinedFrequency, SelectedRegion::UndefinedFrequency);
   }
   else
      region.setFrequencies(mLastF0, mLastF1);
}

void TrackPanel::ResetFreqSelectionPin(double hintFrequency, bool logF)
{
   switch (mFreqSelMode) {
   case FREQ_SEL_INVALID:
   case FREQ_SEL_SNAPPING_CENTER:
      mFreqSelPin = -1.0;
      break;

   case FREQ_SEL_PINNED_CENTER:
      mFreqSelPin = mViewInfo->selectedRegion.fc();
      break;

   case FREQ_SEL_DRAG_CENTER:
      {
         // Re-pin the width
         const double f0 = mViewInfo->selectedRegion.f0();
         const double f1 = mViewInfo->selectedRegion.f1();
         if (f0 >= 0 && f1 >= 0)
            mFreqSelPin = sqrt(f1 / f0);
         else
            mFreqSelPin = -1.0;
      }
      break;

   case FREQ_SEL_FREE:
      // Pin which?  Farther from the hint which is the presumed
      // mouse position.
      {
         const double f0 = mViewInfo->selectedRegion.f0();
         const double f1 = mViewInfo->selectedRegion.f1();
         if (logF) {
            if (f1 < 0)
               mFreqSelPin = f0;
            else {
               const double logf1 = log(std::max(1.0, f1));
               const double logf0 = log(std::max(1.0, f0));
               const double logHint = log(std::max(1.0, hintFrequency));
               if (std::abs (logHint - logf1) < std::abs (logHint - logf0))
                  mFreqSelPin = f0;
               else
                  mFreqSelPin = f1;
            }
         }
         else {
            if (f1 < 0 ||
                std::abs (hintFrequency - f1) < std::abs (hintFrequency - f0))
               mFreqSelPin = f0;
            else
               mFreqSelPin = f1;
            }
      }
      break;

   case FREQ_SEL_TOP_FREE:
      mFreqSelPin = mViewInfo->selectedRegion.f0();
      break;

   case FREQ_SEL_BOTTOM_FREE:
      mFreqSelPin = mViewInfo->selectedRegion.f1();
      break;

   default:
      wxASSERT(false);
   }
}
#endif

#ifdef USE_MIDI
void TrackPanel::Stretch(int mouseXCoordinate, int trackLeftEdge,
                         Track *pTrack)
{
   if (mStretched) { // Undo stretch and redo it with NEW mouse coordinates
      // Drag handling was not originally implemented with Undo in mind --
      // there are saved pointers to tracks that are not supposed to change.
      // Undo will change tracks, so convert pTrack, mCapturedTrack to index
      // values, then look them up after the Undo
      TrackListIterator iter(GetTracks());
      int pTrackIndex = pTrack->GetIndex();
      int capturedTrackIndex =
         (mCapturedTrack ? mCapturedTrack->GetIndex() : 0);

      GetProject()->OnUndo();

      // Undo brings us back to the pre-click state, but we want to
      // quantize selected region to integer beat boundaries. These
      // were saved in mStretchSel[12] variables:
      mViewInfo->selectedRegion.setTimes(mStretchSel0, mStretchSel1);

      mStretched = false;
      int index = 0;
      for (Track *t = iter.First(GetTracks()); t; t = iter.Next()) {
         if (index == pTrackIndex) pTrack = t;
         if (mCapturedTrack && index == capturedTrackIndex) mCapturedTrack = t;
         index++;
      }
   }

   if (pTrack == NULL && mCapturedTrack != NULL)
      pTrack = mCapturedTrack;

   if (!pTrack || pTrack->GetKind() != Track::Note) {
      return;
   }

   NoteTrack *pNt = (NoteTrack *) pTrack;
   double moveto = std::max(0.0, mViewInfo->PositionToTime(mouseXCoordinate, trackLeftEdge));

   // check to make sure tempo is not higher than 20 beats per second
   // (In principle, tempo can be higher, but not infinity.)
   double minPeriod = 0.05; // minimum beat period
   double qBeat0, qBeat1;
   pNt->NearestBeatTime(mViewInfo->selectedRegion.t0(), &qBeat0); // get beat
   pNt->NearestBeatTime(mViewInfo->selectedRegion.t1(), &qBeat1);

   // We could be moving 3 things: left edge, right edge, a point between
   switch (mStretchMode) {
   case stretchLeft: {
      // make sure target duration is not too short
      double dur = mViewInfo->selectedRegion.t1() - moveto;
      if (dur < mStretchRightBeats * minPeriod) {
         dur = mStretchRightBeats * minPeriod;
         moveto = mViewInfo->selectedRegion.t1() - dur;
      }
      if (pNt->StretchRegion(mStretchSel0, mStretchSel1, dur)) {
         pNt->SetOffset(pNt->GetOffset() + moveto - mStretchSel0);
         mViewInfo->selectedRegion.setT0(moveto);
      }
      break;
   }
   case stretchRight: {
      // make sure target duration is not too short
      double dur = moveto - mViewInfo->selectedRegion.t0();
      if (dur < mStretchLeftBeats * minPeriod) {
         dur = mStretchLeftBeats * minPeriod;
         moveto = mStretchSel0 + dur;
      }
      if (pNt->StretchRegion(mStretchSel0, mStretchSel1, dur)) {
         mViewInfo->selectedRegion.setT1(moveto);
      }
      break;
   }
   case stretchCenter: {
      // make sure both left and right target durations are not too short
      double left_dur = moveto - mViewInfo->selectedRegion.t0();
      double right_dur = mViewInfo->selectedRegion.t1() - moveto;
      double centerBeat;
      pNt->NearestBeatTime(mSelStart, &centerBeat);
      if (left_dur < mStretchLeftBeats * minPeriod) {
         left_dur = mStretchLeftBeats * minPeriod;
         moveto = mStretchSel0 + left_dur;
      }
      if (right_dur < mStretchRightBeats * minPeriod) {
         right_dur = mStretchRightBeats * minPeriod;
         moveto = mStretchSel1 - right_dur;
      }
      pNt->StretchRegion(mStretchStart, mStretchSel1, right_dur);
      pNt->StretchRegion(mStretchSel0, mStretchStart, left_dur);
      break;
   }
   default:
      wxASSERT(false);
      break;
   }
   MakeParentPushState(_("Stretch Note Track"), _("Stretch"),
      UndoPush::CONSOLIDATE | UndoPush::AUTOSAVE);
   mStretched = true;
   Refresh(false);
}
#endif

/// AS: If we're dragging to extend a selection (or actually,
///  if the screen is scrolling while you're selecting), we
///  handle it here.
void TrackPanel::SelectionHandleDrag(wxMouseEvent & event, Track *clickedTrack)
{
   // AS: If we're not in the process of selecting (set in
   //  the SelectionHandleClick above), fuhggeddaboudit.
   if (mMouseCapture!=IsSelecting)
      return;

   // Also fuhggeddaboudit if we're not dragging and not autoscrolling.
   if (!event.Dragging() && !mAutoScrolling)
      return;

   if (event.CmdDown()) {
      // Ctrl-drag has no meaning, fuhggeddaboudit
      // JKC YES it has meaning.
      //return;
   }

   wxRect rect      = mCapturedRect;
   Track *pTrack = mCapturedTrack;

   if (!pTrack) {
      pTrack = FindTrack(event.m_x, event.m_y, false, false, &rect);
      rect.y += kTopMargin;
      rect.height -= kTopMargin + kBottomMargin;
   }

   // Also fuhggeddaboudit if not in a track.
   if (!pTrack)
      return;

   int x = mAutoScrolling ? mMouseMostRecentX : event.m_x;
   int y = mAutoScrolling ? mMouseMostRecentY : event.m_y;

   // JKC: Logic to prevent a selection smaller than 5 pixels to
   // prevent accidental dragging when selecting.
   // (if user really wants a tiny selection, they should zoom in).
   // Can someone make this value of '5' configurable in
   // preferences?
   const int minimumSizedSelection = 5; //measured in pixels

   // Might be dragging frequency bounds only, test
   if (mSelStartValid) {
      wxInt64 SelStart = mViewInfo->TimeToPosition(mSelStart, rect.x); //cvt time to pixels.
      // Abandon this drag if selecting < 5 pixels.
      if (wxLongLong(SelStart-x).Abs() < minimumSizedSelection
#ifdef USE_MIDI        // limiting selection size is good, and not starting
          && !mStretching // stretch unless mouse moves 5 pixels is good, but
#endif                 // once stretching starts, it's ok to move even 1 pixel
          )
          return;
   }

   // Handle which tracks are selected
   Track *sTrack = pTrack;
   Track *eTrack = FindTrack(x, y, false, false, NULL);
   SelectRangeOfTracks(sTrack, eTrack);

#ifdef USE_MIDI
   if (mStretching) {
      // the following is also in ExtendSelection, called below
      // probably a good idea to "hoist" the code to before this "if" stmt
      if (clickedTrack == NULL && mCapturedTrack != NULL)
         clickedTrack = mCapturedTrack;
      Stretch(x, rect.x, clickedTrack);
      return;
   }
#endif

#ifdef EXPERIMENTAL_SPECTRAL_EDITING
#ifndef SPECTRAL_EDITING_ESC_KEY
   if (mFreqSelMode == FREQ_SEL_SNAPPING_CENTER &&
      !mViewInfo->selectedRegion.isPoint())
      MoveSnappingFreqSelection(y, rect.y, rect.height, pTrack);
   else
#endif
   if (mFreqSelTrack == pTrack)
      ExtendFreqSelection(y, rect.y, rect.height);
#endif

   ExtendSelection(x, rect.x, clickedTrack);
   // If scrubbing does not use the helper poller thread, then
   // don't do this at every mouse event, because it slows down seek-scrub.
   // Instead, let OnTimer do it, which is often enough.
   // And even if scrubbing does use the thread, then skipping this does not
   // bring that advantage, but it is probably still a good idea anyway.
   // UpdateSelectionDisplay();
}

#ifdef EXPERIMENTAL_SPECTRAL_EDITING
// Seems 4 is too small to work at the top.  Why?
enum { FREQ_SNAP_DISTANCE = 10 };

/// Converts a position (mouse Y coordinate) to
/// frequency, in Hz.
double TrackPanel::PositionToFrequency(const WaveTrack *wt,
                                       bool maySnap,
                                       wxInt64 mouseYCoordinate,
                                       wxInt64 trackTopEdge,
                                       int trackHeight) const
{
   const double rate = wt->GetRate();

   // Handle snapping
   if (maySnap &&
       mouseYCoordinate - trackTopEdge < FREQ_SNAP_DISTANCE)
      return rate;
   if (maySnap &&
       trackTopEdge + trackHeight - mouseYCoordinate < FREQ_SNAP_DISTANCE)
      return -1;

   const SpectrogramSettings &settings = wt->GetSpectrogramSettings();
   float minFreq, maxFreq;
   wt->GetSpectrumBounds(&minFreq, &maxFreq);
   const NumberScale numberScale(settings.GetScale(minFreq, maxFreq, rate, false));
   const double p = double(mouseYCoordinate - trackTopEdge) / trackHeight;
   return numberScale.PositionToValue(1.0 - p);
}

/// Converts a frequency to screen y position.
wxInt64 TrackPanel::FrequencyToPosition(const WaveTrack *wt,
                                        double frequency,
                                        wxInt64 trackTopEdge,
                                        int trackHeight) const
{
   const double rate = wt->GetRate();

   const SpectrogramSettings &settings = wt->GetSpectrogramSettings();
   float minFreq, maxFreq;
   wt->GetSpectrumBounds(&minFreq, &maxFreq);
   const NumberScale numberScale(settings.GetScale(minFreq, maxFreq, rate, false));
   const float p = numberScale.ValueToPosition(frequency);
   return trackTopEdge + wxInt64((1.0 - p) * trackHeight);
}
#endif

template<typename T>
inline void SetIfNotNull( T * pValue, const T Value )
{
   if( pValue == NULL )
      return;
   *pValue = Value;
}


TrackPanel::SelectionBoundary TrackPanel::ChooseTimeBoundary
(double selend, bool onlyWithinSnapDistance,
 wxInt64 *pPixelDist, double *pPinValue) const
{
   const double t0 = mViewInfo->selectedRegion.t0();
   const double t1 = mViewInfo->selectedRegion.t1();
   const wxInt64 posS = mViewInfo->TimeToPosition(selend);
   const wxInt64 pos0 = mViewInfo->TimeToPosition(t0);
   wxInt64 pixelDist = std::abs(posS - pos0);
   bool chooseLeft = true;

   if (mViewInfo->selectedRegion.isPoint())
      // Special case when selection is a point, and thus left
      // and right distances are the same
      chooseLeft = (selend < t0);
   else {
      const wxInt64 pos1 = mViewInfo->TimeToPosition(t1);
      const wxInt64 rightDist = std::abs(posS - pos1);
      if (rightDist < pixelDist)
         chooseLeft = false, pixelDist = rightDist;
   }

   SetIfNotNull(pPixelDist, pixelDist);

   if (onlyWithinSnapDistance &&
       pixelDist >= SELECTION_RESIZE_REGION) {
      SetIfNotNull( pPinValue, -1.0);
      return SBNone;
   }
   else if (chooseLeft) {
      SetIfNotNull( pPinValue, t1);
      return SBLeft;
   }
   else {
      SetIfNotNull( pPinValue, t0);
      return SBRight;
   }
}


TrackPanel::SelectionBoundary TrackPanel::ChooseBoundary
(const wxMouseEvent & event, const Track *pTrack, const wxRect &rect,
bool mayDragWidth, bool onlyWithinSnapDistance,
 double *pPinValue) const
{
   // Choose one of four boundaries to adjust, or the center frequency.
   // May choose frequencies only if in a spectrogram view and
   // within the time boundaries.
   // May choose no boundary if onlyWithinSnapDistance is true.
   // Otherwise choose the eligible boundary nearest the mouse click.
   const double selend = mViewInfo->PositionToTime(event.m_x, rect.x);
   wxInt64 pixelDist = 0;
   SelectionBoundary boundary =
      ChooseTimeBoundary(selend, onlyWithinSnapDistance,
      &pixelDist, pPinValue);

#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   const double t0 = mViewInfo->selectedRegion.t0();
   const double t1 = mViewInfo->selectedRegion.t1();
   const double f0 = mViewInfo->selectedRegion.f0();
   const double f1 = mViewInfo->selectedRegion.f1();
   const double fc = mViewInfo->selectedRegion.fc();
   double ratio = 0;

   bool chooseTime = true;
   bool chooseBottom = true;
   bool chooseCenter = false;
   // Consider adjustment of frequencies only if mouse is
   // within the time boundaries
   if (!mViewInfo->selectedRegion.isPoint() &&
       t0 <= selend && selend < t1 &&
       isSpectralSelectionTrack(pTrack)) {
      // Spectral selection track is always wave
      const auto wt = static_cast<const WaveTrack*>(pTrack);
      const wxInt64 bottomSel = (f0 >= 0)
         ? FrequencyToPosition(wt, f0, rect.y, rect.height)
         : rect.y + rect.height;
      const wxInt64 topSel = (f1 >= 0)
         ? FrequencyToPosition(wt, f1, rect.y, rect.height)
         : rect.y;
      wxInt64 signedBottomDist = (int)(event.m_y - bottomSel);
      wxInt64 verticalDist = std::abs(signedBottomDist);
      if (bottomSel == topSel)
         // Top and bottom are too close to resolve on screen
         chooseBottom = (signedBottomDist >= 0);
      else {
         const wxInt64 topDist = abs((int)(event.m_y - topSel));
         if (topDist < verticalDist)
            chooseBottom = false, verticalDist = topDist;
      }
      if (fc > 0
#ifdef SPECTRAL_EDITING_ESC_KEY
         && mayDragWidth
#endif
         ) {
         const wxInt64 centerSel =
            FrequencyToPosition(wt, fc, rect.y, rect.height);
         const wxInt64 centerDist = abs((int)(event.m_y - centerSel));
         if (centerDist < verticalDist)
            chooseCenter = true, verticalDist = centerDist,
            ratio = f1 / fc;
      }
      if (verticalDist >= 0 &&
          verticalDist < pixelDist) {
         pixelDist = verticalDist;
         chooseTime = false;
      }
   }

   if (!chooseTime) {
      // PRL:  Seems I need a larger tolerance to make snapping work
      // at top of track, not sure why
      if (onlyWithinSnapDistance &&
          pixelDist >= FREQ_SNAP_DISTANCE) {
         SetIfNotNull( pPinValue, -1.0);
         return SBNone;
      }
      else if (chooseCenter) {
         SetIfNotNull( pPinValue, ratio);
         return SBCenter;
      }
      else if (mayDragWidth && fc > 0) {
         SetIfNotNull(pPinValue, fc);
         return SBWidth;
      }
      else if (chooseBottom) {
         SetIfNotNull( pPinValue, f1 );
         return SBBottom;
      }
      else {
         SetIfNotNull(pPinValue, f0);
         return SBTop;
      }
   }
   else
#endif
   {
      return boundary;
   }
}

/// HandleEnvelope gets called when the user is changing the
/// amplitude envelope on a track.
void TrackPanel::HandleEnvelope(wxMouseEvent & event)
{
   if (event.LeftDown()) {
      wxRect rect;
      Track *pTrack = FindTrack(event.m_x, event.m_y, false, false, &rect);

      if (!pTrack)
         return;

      SetCapturedTrack(pTrack, IsEnveloping);

      if (mCapturedTrack->GetKind() == Track::Wave)
      {
         mCapturedEnvelope =
            ((WaveTrack*)mCapturedTrack)->GetEnvelopeAtX(event.GetX());
      } else {
         mCapturedEnvelope = NULL;
      }

      mCapturedRect = rect;
      mCapturedRect.y += kTopMargin;
      mCapturedRect.height -= kTopMargin + kBottomMargin;
   }
   // AS: if there's actually a selected track, then forward all of the
   //  mouse events to its envelope.
   if (mCapturedTrack)
      ForwardEventToEnvelope(event);

   // We test for IsEnveloping, because we could have had our action stopped already,
   // and already recorded and the second mouse up is bogus.
   // e.g could be stopped by some key press.  Bug 1496.
   if ((mMouseCapture == IsEnveloping ) && event.LeftUp()) {
      SetCapturedTrack( NULL );
      MakeParentPushState(
         /* i18n-hint: (verb) Audacity has just adjusted the envelope .*/
         _("Adjusted envelope."),
         /* i18n-hint: The envelope is a curve that controls the audio loudness.*/
         _("Envelope")
         );
   }
}

/// We've established we're a time track.
/// send events for its envelope.
void TrackPanel::ForwardEventToTimeTrackEnvelope(wxMouseEvent & event)
{
   // Assume captured track was time
   const auto ptimetrack = static_cast<TimeTrack *>(mCapturedTrack);
   Envelope *pspeedenvelope = ptimetrack->GetEnvelope();

   wxRect envRect = mCapturedRect;
   double lower = ptimetrack->GetRangeLower(), upper = ptimetrack->GetRangeUpper();
   const double dBRange = mViewInfo->dBr;
   if (ptimetrack->GetDisplayLog()) {
      // MB: silly way to undo the work of GetWaveYPos while still getting a logarithmic scale
      lower = LINEAR_TO_DB(std::max(1.0e-7, lower)) / dBRange + 1.0;
      upper = LINEAR_TO_DB(std::max(1.0e-7, upper)) / dBRange + 1.0;
   }
   bool needUpdate =
      pspeedenvelope->MouseEvent(
         event, envRect,
         *mViewInfo,
         ptimetrack->GetDisplayLog(), dBRange, lower, upper);
   if (needUpdate) {
      RefreshTrack(mCapturedTrack);
   }
}

/// We've established we're a wave track.
/// send events for its envelope.
void TrackPanel::ForwardEventToWaveTrackEnvelope(wxMouseEvent & event)
{
   // Assume captured track was wave
   const auto pwavetrack = static_cast<WaveTrack*>(mCapturedTrack);
   Envelope *penvelope = mCapturedEnvelope;

   // Possibly no-envelope, for example when in spectrum view mode.
   // if so, then bail out.
   if (!penvelope)
      return;

   // AS: WaveTracks can be displayed in several different formats.
   //  This asks which one is in use. (ie, Wave, Spectrum, etc)
   int display = pwavetrack->GetDisplay();

   if (display == WaveTrack::Waveform) {
      const bool dB = !pwavetrack->GetWaveformSettings().isLinear();
      const double dBRange = pwavetrack->GetWaveformSettings().dBRange;
      bool needUpdate;

      // AS: Then forward our mouse event to the envelope.
      // It'll recalculate and then tell us whether or not to redraw.
      wxRect envRect = mCapturedRect;
      float zoomMin, zoomMax;
      pwavetrack->GetDisplayBounds(&zoomMin, &zoomMax);
      needUpdate = penvelope->MouseEvent(
         event, envRect,
         *mViewInfo,
         dB, dBRange, zoomMin, zoomMax);

      // If this track is linked to another track, make the identical
      // change to the linked envelope:
      // Assume linked track is wave or null
      const auto link = static_cast<WaveTrack *>(mCapturedTrack->GetLink());
      if (link) {
         Envelope *e2 = link->GetEnvelopeAtX(event.GetX());
         // There isn't necessarily an envelope there; no guarantee a
         // linked track has the same WaveClip structure...
         bool updateNeeded = false;
         if (e2) {
            wxRect envRect = mCapturedRect;
            float zoomMin, zoomMax;
            pwavetrack->GetDisplayBounds(&zoomMin, &zoomMax);
            updateNeeded = e2->MouseEvent(event, envRect,
                                          *mViewInfo, dB, dBRange,
                                          zoomMin, zoomMax);
            needUpdate |= updateNeeded;
         }
         if(!e2 || !updateNeeded)   // no envelope found at this x point, or found but not updated
         {
            if( (e2 = link->GetActiveEnvelope()) != 0 )  // search for any active DragPoint
            {
               wxRect envRect = mCapturedRect;
               float zoomMin, zoomMax;
               pwavetrack->GetDisplayBounds(&zoomMin, &zoomMax);
               needUpdate |= e2->MouseEvent(event, envRect,
                                            *mViewInfo, dB, dBRange,
                                            zoomMin, zoomMax);
            }
         }
      }

      if (needUpdate) {
         RefreshTrack(mCapturedTrack);
      }
   }
}


/// The Envelope class actually handles things at the mouse
/// event level, so we have to forward the events over.  Envelope
/// will then tell us whether or not we need to redraw.

// AS: I'm not sure why we can't let the Envelope take care of
//  redrawing itself.  ?

void TrackPanel::ForwardEventToEnvelope(wxMouseEvent & event)
{
   if (mCapturedTrack && mCapturedTrack->GetKind() == Track::Time)
   {
      ForwardEventToTimeTrackEnvelope( event );
   }
   else if (mCapturedTrack && mCapturedTrack->GetKind() == Track::Wave)
   {
      ForwardEventToWaveTrackEnvelope( event );
   }
}

void TrackPanel::HandleSlide(wxMouseEvent & event)
{
   if (event.LeftDown())
      StartSlide(event);

   if (mMouseCapture != IsSliding)
      return;

   if (event.Dragging() && mCapturedTrack)
      DoSlide(event);

   if (event.LeftUp()) {
      for (size_t i = 0; i < mCapturedClipArray.size(); i++)
      {
         TrackClip &trackClip = mCapturedClipArray[i];
         WaveClip* pWaveClip = trackClip.clip;
         // Note that per TrackPanel::AddClipsToCaptured(Track *t, double t0, double t1),
         // in the non-WaveTrack case, the code adds a NULL clip to mCapturedClipArray,
         // so we have to check for that any time we're going to deref it.
         // Previous code did not check it here, and that caused bug 367 crash.
         if (pWaveClip &&
             trackClip.track != trackClip.origTrack)
         {
            // Now that user has dropped the clip into a different track,
            // make sure the sample rate matches the destination track (mCapturedTrack).
            // Assume the clip was dropped in a wave track
            pWaveClip->Resample
               (static_cast<WaveTrack*>(trackClip.track)->GetRate());
            pWaveClip->MarkChanged();
         }
      }

      SetCapturedTrack( NULL );

      mSnapManager.reset();

      // Do not draw yellow lines
      if (mSnapLeft != -1 || mSnapRight != -1) {
         mSnapLeft = mSnapRight = -1;
         Refresh(false);
      }

      if (!mDidSlideVertically && mHSlideAmount==0)
         return;

      MakeParentRedrawScrollbars();

      wxString msg;
      bool consolidate;
      if (mDidSlideVertically) {
         msg.Printf(_("Moved clips to another track"));
         consolidate = false;
      }
      else {
         wxString direction = mHSlideAmount>0 ?
            /* i18n-hint: a direction as in left or right.*/
            _("right") :
            /* i18n-hint: a direction as in left or right.*/
            _("left");
         /* i18n-hint: %s is a direction like left or right */
         msg.Printf(_("Time shifted tracks/clips %s %.02f seconds"),
                    direction.c_str(), fabs(mHSlideAmount));
         consolidate = true;
      }
      MakeParentPushState(msg, _("Time-Shift"),
         consolidate ? (UndoPush::CONSOLIDATE) : (UndoPush::AUTOSAVE));
   }
}

namespace {
   // Don't count right channels.
   WaveTrack *NthAudioTrack(TrackList &list, int nn)
   {
      if (nn >= 0) {
         TrackListOfKindIterator iter(Track::Wave, &list);
         Track *pTrack = iter.First();
         while (pTrack && nn--)
            pTrack = iter.Next(true);
         return static_cast<WaveTrack*>(pTrack);
      }

      return NULL;
   }

   // Don't count right channels.
   int TrackPosition(TrackList &list, Track *pFindTrack)
   {
      Track *const partner = pFindTrack->GetLink();
      TrackListOfKindIterator iter(Track::Wave, &list);
      int nn = 0;
      for (Track *pTrack = iter.First(); pTrack; pTrack = iter.Next(true), ++nn) {
         if (pTrack == pFindTrack ||
             pTrack == partner)
            return nn;
      }
      return -1;
   }

   WaveClip *FindClipAtTime(WaveTrack *pTrack, double time)
   {
      if (pTrack) {
         // WaveClip::GetClipAtX doesn't work unless the clip is on the screen and can return bad info otherwise
         // instead calculate the time manually
         double rate = pTrack->GetRate();
         auto s0 = (sampleCount)(time * rate + 0.5);

         if (s0 >= 0)
            return pTrack->GetClipAtSample(s0);
      }

      return 0;
   }
}

/// Prepare for sliding.
void TrackPanel::StartSlide(wxMouseEvent & event)
{
   wxRect rect;

   mHSlideAmount = 0.0;
   mDidSlideVertically = false;

   mTrackExclusions.clear();

   Track *vt = FindTrack(event.m_x, event.m_y, false, false, &rect);
   if (!vt)
      return;

   ToolsToolBar * ttb = mListener->TP_GetToolsToolBar();
   bool multiToolModeActive = (ttb && ttb->IsDown(multiTool));

   double clickTime =
      mViewInfo->PositionToTime(event.m_x, GetLeftOffset());
   mCapturedClipIsSelection =
      (vt->GetSelected() &&
      clickTime > mViewInfo->selectedRegion.t0() &&
      clickTime < mViewInfo->selectedRegion.t1());

   if ((vt->GetKind() == Track::Wave
#ifdef USE_MIDI
        || vt->GetKind() == Track::Note
#endif
       ) && !event.ShiftDown())
   {
#ifdef USE_MIDI
      if (vt->GetKind() == Track::Wave) {
#endif
         WaveTrack* wt = (WaveTrack*)vt;
         mCapturedClip = wt->GetClipAtX(event.m_x);
         if (mCapturedClip == NULL)
            return;
#ifdef USE_MIDI
      }
      else {
         mCapturedClip = NULL;
      }
#endif
      // The captured clip is the focus, but we need to create a list
      // of all clips that have to move, also...

      mCapturedClipArray.clear();

      // First, if click was in selection, capture selected clips; otherwise
      // just the clicked-on clip
      if (mCapturedClipIsSelection) {
         TrackListIterator iter(GetTracks());
         for (Track *t = iter.First(); t; t = iter.Next()) {
            if (t->GetSelected()) {
               AddClipsToCaptured(t, true);
               if (t->GetKind() != Track::Wave)
                  mTrackExclusions.push_back(t);
            }
         }
      }
      else {
         mCapturedClipArray.push_back(TrackClip(vt, mCapturedClip));

         // Check for stereo partner
         Track *partner = mTracks->GetLink(vt);
         WaveTrack *wt;
         if (mCapturedClip &&
             // Assume linked track is wave or null
             nullptr != (wt = static_cast<WaveTrack*>(partner))) {
            WaveClip *const clip =
               FindClipAtTime(wt,
                  mViewInfo->PositionToTime(event.m_x, GetLeftOffset()));
            if (clip)
               mCapturedClipArray.push_back(TrackClip(partner, clip));
         }
      }

      // Now, if sync-lock is enabled, capture any clip that's linked to a
      // captured clip.
      if (GetProject()->IsSyncLocked()) {
         // AWD: mCapturedClipArray expands as the loop runs, so newly-added
         // clips are considered (the effect is like recursion and terminates
         // because AddClipsToCaptured doesn't add duplicate clips); to remove
         // this behavior just store the array size beforehand.
         for (unsigned int i = 0; i < mCapturedClipArray.size(); ++i) {
            // Capture based on tracks that have clips -- that means we
            // don't capture based on links to label tracks for now (until
            // we can treat individual labels as clips)
            if (mCapturedClipArray[i].clip) {
               // Iterate over sync-lock group tracks.
               SyncLockedTracksIterator git(GetTracks());
               for (Track *t = git.StartWith(mCapturedClipArray[i].track);
                     t; t = git.Next() )
               {
                  AddClipsToCaptured(t,
                        mCapturedClipArray[i].clip->GetStartTime(),
                        mCapturedClipArray[i].clip->GetEndTime() );
                  if (t->GetKind() != Track::Wave)
                     mTrackExclusions.push_back(t);
               }
            }
#ifdef USE_MIDI
            // Capture additional clips from NoteTracks
            Track *nt = mCapturedClipArray[i].track;
            if (nt->GetKind() == Track::Note) {
               // Iterate over sync-lock group tracks.
               SyncLockedTracksIterator git(GetTracks());
               for (Track *t = git.StartWith(nt); t; t = git.Next())
               {
                  AddClipsToCaptured(t, nt->GetStartTime(), nt->GetEndTime());
                  if (t->GetKind() != Track::Wave)
                     mTrackExclusions.push_back(t);
               }
            }
#endif
         }
      }

   } else {
      mCapturedClip = NULL;
      mCapturedClipArray.clear();
   }

   mSlideUpDownOnly = event.CmdDown() && !multiToolModeActive;

   mCapturedTrack = vt;
   mCapturedRect = rect;

   mMouseClickX = event.m_x;
   mMouseClickY = event.m_y;

   mSelStartValid = true;
   mSelStart = mViewInfo->PositionToTime(event.m_x, rect.x);

   mSnapManager = std::make_unique<SnapManager>(GetTracks(),
                                  mViewInfo,
                                  &mCapturedClipArray,
                                  &mTrackExclusions,
                                  true); // don't snap to time
   mSnapLeft = -1;
   mSnapRight = -1;
   mSnapPreferRightEdge = false;
   if (mCapturedClip) {
      if (fabs(mSelStart - mCapturedClip->GetEndTime()) <
          fabs(mSelStart - mCapturedClip->GetStartTime()))
         mSnapPreferRightEdge = true;
   }

   mMouseCapture = IsSliding;
}

// Helper for the above, adds a track's clips to mCapturedClipArray (eliminates
// duplication of this logic)
void TrackPanel::AddClipsToCaptured(Track *t, bool withinSelection)
{
   if (withinSelection)
      AddClipsToCaptured(t, mViewInfo->selectedRegion.t0(),
                         mViewInfo->selectedRegion.t1());
   else
      AddClipsToCaptured(t, t->GetStartTime(), t->GetEndTime());
}

// Adds a track's clips to mCapturedClipArray within a specified time
void TrackPanel::AddClipsToCaptured(Track *t, double t0, double t1)
{
   if (t->GetKind() == Track::Wave)
   {
      for(const auto &clip: static_cast<WaveTrack*>(t)->GetClips())
      {
         if ( ! clip->AfterClip(t0) && ! clip->BeforeClip(t1) )
         {
            // Avoid getting clips that were already captured
            bool newClip = true;
            for (unsigned int i = 0; i < mCapturedClipArray.size(); ++i) {
               if (mCapturedClipArray[i].clip == clip.get()) {
                  newClip = false;
                  break;
               }
            }

            if (newClip)
               mCapturedClipArray.push_back(TrackClip(t, clip.get()));
         }
      }
   }
   else
   {
      // This handles label tracks rather heavy-handedly -- it would be nice to
      // treat individual labels like clips

      // Avoid adding a track twice
      bool newClip = true;
      for (unsigned int i = 0; i < mCapturedClipArray.size(); ++i) {
         if (mCapturedClipArray[i].track == t) {
            newClip = false;
            break;
         }
      }

      if (newClip) {
#ifdef USE_MIDI
         // do not add NoteTrack if the data is outside of time bounds
         if (t->GetKind() == Track::Note) {
            if (t->GetEndTime() < t0 || t->GetStartTime() > t1)
               return;
         }
#endif
         mCapturedClipArray.push_back(TrackClip(t, NULL));
      }
   }
}

/// Slide tracks horizontally, or slide clips horizontally or vertically
/// (e.g. moving clips between tracks).

// GM: DoSlide now implementing snap-to
// samples functionality based on sample rate.
void TrackPanel::DoSlide(wxMouseEvent & event)
{
   unsigned int i;

   // find which track the mouse is currently in (mouseTrack) -
   // this may not be the same as the one we started in...
   Track *mouseTrack = FindTrack(event.m_x, event.m_y, false, false, NULL);
   if (mouseTrack == NULL) {
      // Allow sliding if the pointer is not over any track, but only if x is
      // within the bounds of the tracks area.
      int width;
      GetTracksUsableArea(&width, NULL);
      if (event.m_x >= GetLeftOffset() && event.m_x < GetLeftOffset() + width)
         mouseTrack = mCapturedTrack;
      else
         return;
   }

   // Start by undoing the current slide amount; everything
   // happens relative to the original horizontal position of
   // each clip...
#ifdef USE_MIDI
   if (mCapturedClipArray.size())
#else
   if (mCapturedClip)
#endif
   {
      for(i=0; i<mCapturedClipArray.size(); i++) {
         if (mCapturedClipArray[i].clip)
            mCapturedClipArray[i].clip->Offset(-mHSlideAmount);
         else
            mCapturedClipArray[i].track->Offset(-mHSlideAmount);
      }
   }
   else {
      mCapturedTrack->Offset(-mHSlideAmount);
      Track* link = mTracks->GetLink(mCapturedTrack);
      if (link)
         link->Offset(-mHSlideAmount);
   }

   if (mCapturedClipIsSelection) {
      // Slide the selection, too
      mViewInfo->selectedRegion.move(-mHSlideAmount);
   }
   mHSlideAmount = 0.0;

   // Implement sliding within the track(s)
   double desiredSlideAmount;
   if (mSlideUpDownOnly) {
      desiredSlideAmount = 0.0;
   }
   else {
      desiredSlideAmount =
         mViewInfo->PositionToTime(event.m_x) -
         mViewInfo->PositionToTime(mMouseClickX);
      bool trySnap = false;
      double clipLeft = 0, clipRight = 0;
#ifdef USE_MIDI
      if (mouseTrack->GetKind() == Track::Wave) {
         WaveTrack *mtw = (WaveTrack *)mouseTrack;
         desiredSlideAmount = rint(mtw->GetRate() * desiredSlideAmount) /
            mtw->GetRate();  // set it to a sample point
      }
      // Adjust desiredSlideAmount using SnapManager
      if (mSnapManager && mCapturedClipArray.size()) {
         trySnap = true;
         if (mCapturedClip) {
            clipLeft = mCapturedClip->GetStartTime() + desiredSlideAmount;
            clipRight = mCapturedClip->GetEndTime() + desiredSlideAmount;
         }
         else {
            clipLeft = mCapturedTrack->GetStartTime() + desiredSlideAmount;
            clipRight = mCapturedTrack->GetEndTime() + desiredSlideAmount;
         }
      }
#else
      {
         trySnap = true;
         desiredSlideAmount = rint(mouseTrack->GetRate() * desiredSlideAmount) /
            mouseTrack->GetRate();  // set it to a sample point
         if (mSnapManager && mCapturedClip) {
            clipLeft = mCapturedClip->GetStartTime() + desiredSlideAmount;
            clipRight = mCapturedClip->GetEndTime() + desiredSlideAmount;
         }
      }
#endif
      if (trySnap) {
         double newClipLeft = clipLeft;
         double newClipRight = clipRight;

         bool dummy1, dummy2;
         mSnapManager->Snap(mCapturedTrack, clipLeft, false, &newClipLeft,
            &dummy1, &dummy2);
         mSnapManager->Snap(mCapturedTrack, clipRight, false, &newClipRight,
            &dummy1, &dummy2);

         // Only one of them is allowed to snap
         if (newClipLeft != clipLeft && newClipRight != clipRight) {
            if (mSnapPreferRightEdge)
               newClipLeft = clipLeft;
            else
               newClipRight = clipRight;
         }

         // Take whichever one snapped (if any) and compute the NEW desiredSlideAmount
         mSnapLeft = -1;
         mSnapRight = -1;
         if (newClipLeft != clipLeft) {
            double difference = (newClipLeft - clipLeft);
            desiredSlideAmount += difference;
            mSnapLeft = mViewInfo->TimeToPosition(newClipLeft, GetLeftOffset());
         }
         else if (newClipRight != clipRight) {
            double difference = (newClipRight - clipRight);
            desiredSlideAmount += difference;
            mSnapRight = mViewInfo->TimeToPosition(newClipRight, GetLeftOffset());
         }
      }
   }

   // Scroll during vertical drag.
   // EnsureVisible(mouseTrack); //vvv Gale says this has problems on Linux, per bug 393 thread. Revert for 2.0.2.
   bool slidVertically = false;

   // If the mouse is over a track that isn't the captured track,
   // decide which tracks the captured clips should go to.
   if (mCapturedClip && mouseTrack != mCapturedTrack /*&&
       !mCapturedClipIsSelection*/)
   {
      const int diff =
         TrackPosition(*mTracks, mouseTrack) -
         TrackPosition(*mTracks, mCapturedTrack);
      for (unsigned ii = 0, nn = mCapturedClipArray.size(); ii < nn; ++ii) {
         TrackClip &trackClip = mCapturedClipArray[ii];
         if (trackClip.clip) {
            // Move all clips up or down by an equal count of audio tracks.
            Track *const pSrcTrack = trackClip.track;
            auto pDstTrack = NthAudioTrack(*mTracks,
               diff + TrackPosition(*mTracks, pSrcTrack));
            // Can only move mono to mono, or left to left, or right to right
            // And that must be so for each captured clip
            bool stereo = (pSrcTrack->GetLink() != 0);
            if (pDstTrack && stereo && !pSrcTrack->GetLinked())
               // Assume linked track is wave or null
               pDstTrack = static_cast<WaveTrack*>(pDstTrack->GetLink());
            bool ok = pDstTrack &&
               (stereo == (pDstTrack->GetLink() != 0)) &&
               (!stereo || (pSrcTrack->GetLinked() == pDstTrack->GetLinked()));
            if (ok)
               trackClip.dstTrack = pDstTrack;
            else
               return;
         }
      }

      // Having passed that test, remove clips temporarily from their
      // tracks, so moving clips don't interfere with each other
      // when we call CanInsertClip()
      for (unsigned ii = 0, nn = mCapturedClipArray.size(); ii < nn;  ++ii) {
         TrackClip &trackClip = mCapturedClipArray[ii];
         WaveClip *const pSrcClip = trackClip.clip;
         if (pSrcClip)
            trackClip.holder =
            // Assume track is wave because it has a clip
            static_cast<WaveTrack*>(trackClip.track)->
               RemoveAndReturnClip(pSrcClip);
      }

      // Now check that the move is possible
      bool ok = true;
      for (unsigned ii = 0, nn = mCapturedClipArray.size(); ok && ii < nn; ++ii) {
         TrackClip &trackClip = mCapturedClipArray[ii];
         WaveClip *const pSrcClip = trackClip.clip;
         if (pSrcClip)
            ok = trackClip.dstTrack->CanInsertClip(pSrcClip);
      }
      
      if (!ok) {
         // Failure -- put clips back where they were
         for (unsigned ii = 0, nn = mCapturedClipArray.size(); ii < nn;  ++ii) {
            TrackClip &trackClip = mCapturedClipArray[ii];
            WaveClip *const pSrcClip = trackClip.clip;
            if (pSrcClip)
               // Assume track is wave because it has a clip
               static_cast<WaveTrack*>(trackClip.track)->
                  AddClip(std::move(trackClip.holder));
         }
         return;
      }
      else {
         // Do the vertical moves of clips
         for (unsigned ii = 0, nn = mCapturedClipArray.size(); ii < nn; ++ii) {
            TrackClip &trackClip = mCapturedClipArray[ii];
            WaveClip *const pSrcClip = trackClip.clip;
            if (pSrcClip) {
               const auto dstTrack = trackClip.dstTrack;
               dstTrack->AddClip(std::move(trackClip.holder));
               trackClip.track = dstTrack;
            }
         }

         mCapturedTrack = mouseTrack;
         mDidSlideVertically = true;

         // Make the offset permanent; start from a "clean slate"
         mMouseClickX = event.m_x;
      }

      // Not done yet, check for horizontal movement.
      slidVertically = true;
   }

   if (desiredSlideAmount == 0.0) {
      Refresh(false);
      return;
   }

   mHSlideAmount = desiredSlideAmount;

#ifdef USE_MIDI
   if (mCapturedClipArray.size())
#else
   if (mCapturedClip)
#endif
   {
      double allowed;
      double initialAllowed;
      double safeBigDistance = 1000 + 2.0 * (mTracks->GetEndTime() -
                                             mTracks->GetStartTime());

      do { // loop to compute allowed, does not actually move anything yet
         initialAllowed = mHSlideAmount;

         unsigned int i, j;
         for(i=0; i<mCapturedClipArray.size(); i++) {
            WaveTrack *track = (WaveTrack *)mCapturedClipArray[i].track;
            WaveClip *clip = mCapturedClipArray[i].clip;

            if (clip) { // only audio clips are used to compute allowed
               // Move all other selected clips totally out of the way
               // temporarily because they're all moving together and
               // we want to find out if OTHER clips are in the way,
               // not one of the moving ones
               for(j=0; j<mCapturedClipArray.size(); j++) {
                  WaveClip *clip2 = mCapturedClipArray[j].clip;
                  if (clip2 && clip2 != clip)
                     clip2->Offset(-safeBigDistance);
               }

               if (track->CanOffsetClip(clip, mHSlideAmount, &allowed)) {
                  if (mHSlideAmount != allowed) {
                     mHSlideAmount = allowed;
                     mSnapLeft = mSnapRight = -1; // see bug 1067
                  }
               }
               else {
                  mHSlideAmount = 0.0;
                  mSnapLeft = mSnapRight = -1; // see bug 1067
               }

               for(j=0; j<mCapturedClipArray.size(); j++) {
                  WaveClip *clip2 = mCapturedClipArray[j].clip;
                  if (clip2 && clip2 != clip)
                     clip2->Offset(safeBigDistance);
               }
            }
         }
      } while (mHSlideAmount != initialAllowed);

      if (mHSlideAmount != 0.0) { // finally, here is where clips are moved
         unsigned int i;
         for(i=0; i<mCapturedClipArray.size(); i++) {
            Track *track = mCapturedClipArray[i].track;
            WaveClip *clip = mCapturedClipArray[i].clip;
            if (clip)
               clip->Offset(mHSlideAmount);
            else
               track->Offset(mHSlideAmount);
         }
      }
   }
   else {
      // For Shift key down, or
      // For non wavetracks, specifically label tracks ...
      mCapturedTrack->Offset(mHSlideAmount);
      Track* link = mTracks->GetLink(mCapturedTrack);
      if (link)
         link->Offset(mHSlideAmount);
   }

   if (mCapturedClipIsSelection) {
      // Slide the selection, too
      mViewInfo->selectedRegion.move(mHSlideAmount);
   }

   if (slidVertically) {
      // NEW origin
      mHSlideAmount = 0;
   }

   Refresh(false);
}


///  This method takes care of our different zoom
///  possibilities.  It is possible for a user to just
///  "zoom in" or "zoom out," but it is also possible
///  for a user to drag and select an area that he
///  or she wants to be zoomed in on.  We use mZoomStart
///  and mZoomEnd to track the beggining and end of such
///  a zoom area.  Note that the ViewInfo member
///  mViewInfo actually keeps track of our zoom constant,
///  so we achieve zooming by altering the zoom constant
///  and forcing a refresh.
void TrackPanel::HandleZoom(wxMouseEvent & event)
{
   if (event.ButtonDown() || event.LeftDClick()) {
      HandleZoomClick(event);
   }
   else if (mMouseCapture == IsZooming) {
      if (event.Dragging()) {
         HandleZoomDrag(event);
      }
      else if (event.ButtonUp()) {
         HandleZoomButtonUp(event);
      }
   }
}

/// Zoom button down, record the position.
void TrackPanel::HandleZoomClick(wxMouseEvent & event)
{
   if (mCapturedTrack)
      return;

   mCapturedTrack = FindTrack(event.m_x, event.m_y, false, false,
                              &mCapturedRect);
   if (!mCapturedTrack)
      return;

   SetCapturedTrack(mCapturedTrack, IsZooming);

   mZoomStart = event.m_x;
   mZoomEnd = event.m_x;
}

/// Zoom drag
void TrackPanel::HandleZoomDrag(wxMouseEvent & event)
{
   const int left = GetLeftOffset();
   const int right = GetSize().x - kRightMargin - 1;

   mZoomEnd = event.m_x;

   if (event.m_x < left) {
      mZoomEnd = left;
   }
   else if (event.m_x > right) {
      mZoomEnd = right;
   }

   if (IsDragZooming()) {
      Refresh(false);
   }
}

/// Zoom button up
void TrackPanel::HandleZoomButtonUp(wxMouseEvent & event)
{
   if (mZoomEnd < mZoomStart)
      std::swap(mZoomStart, mZoomEnd);

   if (IsDragZooming())
      DragZoom(event, GetLeftOffset());
   else
      DoZoomInOut(event, GetLeftOffset());

   mZoomEnd = mZoomStart = 0;

   SetCapturedTrack(NULL);

   MakeParentRedrawScrollbars();
   Refresh(false);
}

/// Determines if drag zooming is active
bool TrackPanel::IsDragZooming(int zoomStart, int zoomEnd)
{
   return (abs(zoomEnd - zoomStart) > DragThreshold);
}

/// Determines if the a modal tool is active
bool TrackPanel::IsMouseCaptured()
{
   return (mMouseCapture != IsUncaptured || mCapturedTrack != NULL);
}


///  This actually sets the Zoom value when you're done doing
///  a drag zoom.
void TrackPanel::DragZoom(wxMouseEvent & event, int trackLeftEdge)
{
   double left = mViewInfo->PositionToTime(mZoomStart, trackLeftEdge);
   double right = mViewInfo->PositionToTime(mZoomEnd, trackLeftEdge);
   double multiplier = (GetScreenEndTime() - mViewInfo->h) / (right - left);
   if (event.ShiftDown())
      multiplier = 1.0 / multiplier;

   mViewInfo->ZoomBy(multiplier);

   mViewInfo->h = left;
}

/// This handles normal Zoom In/Out, if you just clicked;
/// IOW, if you were NOT dragging to zoom an area.
/// \todo MAGIC NUMBER: We've got several in this method.
void TrackPanel::DoZoomInOut(wxMouseEvent & event, int trackLeftEdge)
{
   double center_h = mViewInfo->PositionToTime(event.m_x, trackLeftEdge);

   const double multiplier =
      (event.RightUp() || event.RightDClick() || event.ShiftDown())
      ? 0.5 : 2.0;
   mViewInfo->ZoomBy(multiplier);

   if (event.MiddleUp() || event.MiddleDClick())
      mViewInfo->SetZoom(ZoomInfo::GetDefaultZoom()); // AS: Reset zoom.

   double new_center_h = mViewInfo->PositionToTime(event.m_x, trackLeftEdge);

   mViewInfo->h += (center_h - new_center_h);
}

/// Vertical zooming (triggered by clicking in the
/// vertical ruler)
void TrackPanel::HandleVZoom(wxMouseEvent & event)
{
   if (event.ButtonDown() || event.ButtonDClick()) {
      HandleVZoomClick( event );
   }
   else if (event.Dragging()) {
      HandleVZoomDrag( event );
   }
   else if (event.ButtonUp()) {
      HandleVZoomButtonUp( event );
   }
   //TODO-MB: add timetrack zooming here!
}

/// VZoom click
void TrackPanel::HandleVZoomClick( wxMouseEvent & event )
{
   if (mCapturedTrack)
      return;
   mCapturedTrack = FindTrack(event.m_x, event.m_y, true, false,
                              &mCapturedRect);
   if (!mCapturedTrack)
      return;

   if (mCapturedTrack->GetKind() == Track::Wave
#ifdef USE_MIDI
            || mCapturedTrack->GetKind() == Track::Note
#endif
         )
   {
      mMouseCapture = IsVZooming;
      mZoomStart = event.m_y;
      mZoomEnd = event.m_y;
      // change note track to zoom like audio track
      //#ifdef USE_MIDI
      //      if (mCapturedTrack->GetKind() == Track::Note) {
      //          ((NoteTrack *) mCapturedTrack)->StartVScroll();
      //      }
      //#endif
   }
}

/// VZoom drag
void TrackPanel::HandleVZoomDrag( wxMouseEvent & event )
{
   mZoomEnd = event.m_y;
   if (IsDragZooming()){
      // changed Note track to work like audio track
      //#ifdef USE_MIDI
      //      if (mCapturedTrack && mCapturedTrack->GetKind() == Track::Note) {
      //         ((NoteTrack *) mCapturedTrack)->VScroll(mZoomStart, mZoomEnd);
      //      }
      //#endif
      Refresh(false);
   }
}

/// VZoom Button up.
/// There are three cases:
///   - Drag-zooming; we already have a min and max
///   - Zoom out; ensure we don't go too small.
///   - Zoom in; ensure we don't go too large.
void TrackPanel::HandleVZoomButtonUp( wxMouseEvent & event )
{
   if (!mCapturedTrack)
      return;

   mMouseCapture = IsUncaptured;

#ifdef USE_MIDI
   // handle vertical scrolling in Note Track. This is so different from
   // zooming in audio tracks that it is handled as a special case from
   // which we then return
   if (mCapturedTrack->GetKind() == Track::Note) {
      NoteTrack *nt = (NoteTrack *) mCapturedTrack;
      if (IsDragZooming()) {
         nt->ZoomTo(mZoomStart, mZoomEnd);
      } else if (event.ShiftDown() || event.RightUp()) {
         nt->ZoomOut(mZoomEnd);
      } else {
         nt->ZoomIn(mZoomEnd);
      }
      mZoomEnd = mZoomStart = 0;
      Refresh(false);
      mCapturedTrack = NULL;
      MakeParentModifyState(true);
      return;
   }
#endif


   // don't do anything if track is not wave
   if (mCapturedTrack->GetKind() != Track::Wave)
      return;

   /*
   if (event.RightUp() &&
       !(event.ShiftDown() || event.CmdDown())) {
      OnVRulerMenu(mCapturedTrack, &event);
      return;
   }
   */

   HandleWaveTrackVZoom(static_cast<WaveTrack*>(mCapturedTrack),
      event.ShiftDown(), event.RightUp());
   mCapturedTrack = NULL;
}

void TrackPanel::HandleWaveTrackVZoom(WaveTrack *track, bool shiftDown, bool rightUp)
{
   HandleWaveTrackVZoom(GetTracks(), mCapturedRect, mZoomStart, mZoomEnd,
      track, shiftDown, rightUp, false);
   mZoomEnd = mZoomStart = 0;
   UpdateVRuler(track);
   Refresh(false);
   MakeParentModifyState(true);
}

//static
void TrackPanel::HandleWaveTrackVZoom
(TrackList *tracks, const wxRect &rect,
 int zoomStart, int zoomEnd,
 WaveTrack *track, bool shiftDown, bool rightUp,
 bool fixedMousePoint)
{
   // Assume linked track is wave or null
   const auto partner = static_cast<WaveTrack *>(tracks->GetLink(track));
   int height = track->GetHeight() - (kTopMargin + kBottomMargin);
   int ypos = rect.y + kBorderThickness;

   // Ensure start and end are in order (swap if not).
   if (zoomEnd < zoomStart)
      std::swap(zoomStart, zoomEnd);

   float min, max, minBand = 0;
   const double rate = track->GetRate();
   const float halfrate = rate / 2;
   const SpectrogramSettings &settings = track->GetSpectrogramSettings();
   NumberScale scale;
   const bool spectral = (track->GetDisplay() == WaveTrack::Spectrum);
   const bool spectrumLinear = spectral &&
      (track->GetSpectrogramSettings().scaleType == SpectrogramSettings::stLinear);

   if (spectral) {
      track->GetSpectrumBounds(&min, &max);
      scale = (settings.GetScale(min, max, rate, false));
      const auto fftLength = settings.GetFFTLength();
      const float binSize = rate / fftLength;

      // JKC:  Following discussions of Bug 1208 I'm allowing zooming in 
      // down to one bin.
//      const int minBins =
//         std::min(10, fftLength / 2); //minimum 10 freq bins, unless there are less
      const int minBins = 1;
      minBand = minBins * binSize;
   }
   else
      track->GetDisplayBounds(&min, &max);

   if (IsDragZooming(zoomStart, zoomEnd)) {
      // Drag Zoom
      const float tmin = min, tmax = max;

      if (spectral) {
         double xmin = 1 - (zoomEnd - ypos) / (float)height;
         double xmax = 1 - (zoomStart - ypos) / (float)height;
         const float middle = (xmin + xmax) / 2;
         const float middleValue = scale.PositionToValue(middle);

         min = std::max(spectrumLinear ? 0.0f : 1.0f,
            std::min(middleValue - minBand / 2,
               scale.PositionToValue(xmin)
         ));
         max = std::min(halfrate,
            std::max(middleValue + minBand / 2,
               scale.PositionToValue(xmax)
         ));
      }
      else {
         const float p1 = (zoomStart - ypos) / (float)height;
         const float p2 = (zoomEnd - ypos) / (float)height;
         max = (tmax * (1.0-p1) + tmin * p1);
         min = (tmax * (1.0-p2) + tmin * p2);

         // Waveform view - allow zooming down to a range of ZOOMLIMIT
         if (max - min < ZOOMLIMIT) {     // if user attempts to go smaller...
            const float c = (min+max)/2;  // ...set centre of view to centre of dragged area and top/bottom to ZOOMLIMIT/2 above/below
            min = c - ZOOMLIMIT/2.0;
            max = c + ZOOMLIMIT/2.0;
         }
      }
   }
   else if (shiftDown || rightUp) {
      // Zoom OUT
      if (spectral) {
         if (shiftDown && rightUp) {
            // Zoom out full
            min = spectrumLinear ? 0.0f : 1.0f;
            max = halfrate;
         }
         else {
            // Zoom out

            const float p1 = (zoomStart - ypos) / (float)height;
            // (Used to zoom out centered at midline, ignoring the click, if linear view.
            //  I think it is better to be consistent.  PRL)
            // Center zoom-out at the midline
            const float middle = // spectrumLinear ? 0.5f :
               1.0f - p1;

            if (fixedMousePoint) {
               min = std::max(spectrumLinear ? 0.0f : 1.0f, scale.PositionToValue(-middle));
               max = std::min(halfrate, scale.PositionToValue(1.0f + p1));
            }
            else {
               min = std::max(spectrumLinear ? 0.0f : 1.0f, scale.PositionToValue(middle - 1.0f));
               max = std::min(halfrate, scale.PositionToValue(middle + 1.0f));
            }
         }
      }
      else {
         // Zoom out to -1.0...1.0 first, then, and only
         // then, if they click again, allow one more
         // zoom out.
         if (shiftDown && rightUp) {
            // Zoom out full
            min = -1.0;
            max = 1.0;
         }
         else {
            // Zoom out
            const WaveformSettings &settings = track->GetWaveformSettings();
            const bool linear = settings.isLinear();
            const float top = linear
               ? 2.0
               : (LINEAR_TO_DB(2.0) + settings.dBRange) / settings.dBRange;
            if (min <= -1.0 && max >= 1.0) {
               // Go to the maximal zoom-out
               min = -top;
               max = top;
            }
            else {
               // limit to +/- 1 range unless already outside that range...
               float minRange = (min < -1) ? -top : -1.0;
               float maxRange = (max > 1) ? top : 1.0;
               // and enforce vertical zoom limits.
               const float p1 = (zoomStart - ypos) / (float)height;
               if (fixedMousePoint) {
                  const float oldRange = max - min;
                  const float c = (max * (1.0 - p1) + min * p1);
                  min = std::min(maxRange - ZOOMLIMIT,
                     std::max(minRange, c - 2 * (1.0f - p1) * oldRange));
                  max = std::max(minRange + ZOOMLIMIT,
                     std::min(maxRange, c + 2 * p1 * oldRange));
               }
               else {
                  const float c = p1 * min + (1 - p1) * max;
                  const float l = (max - min);
                  min = std::min(maxRange - ZOOMLIMIT,
                     std::max(minRange, c - l));
                  max = std::max(minRange + ZOOMLIMIT,
                     std::min(maxRange, c + l));
               }
            }
         }
      }
   }
   else {
      // Zoom IN
      if (spectral) {
         // Center the zoom-in at the click
         const float p1 = (zoomStart - ypos) / (float)height;
         const float middle = 1.0f - p1;
         const float middleValue = scale.PositionToValue(middle);

         if (fixedMousePoint) {
            min = std::max(spectrumLinear ? 0.0f : 1.0f,
               std::min(middleValue - minBand * middle,
               scale.PositionToValue(0.5f * middle)
            ));
            max = std::min(halfrate,
               std::max(middleValue + minBand * p1,
               scale.PositionToValue(middle + 0.5f * p1)
            ));
         }
         else {
            min = std::max(spectrumLinear ? 0.0f : 1.0f,
               std::min(middleValue - minBand / 2,
               scale.PositionToValue(middle - 0.25f)
            ));
            max = std::min(halfrate,
               std::max(middleValue + minBand / 2,
               scale.PositionToValue(middle + 0.25f)
            ));
         }
      }
      else {
         // Zoom in centered on cursor
         if (min < -1.0 || max > 1.0) {
            min = -1.0;
            max = 1.0;
         }
         else {
            // Enforce maximum vertical zoom
            const float oldRange = max - min;
            const float l = std::max(ZOOMLIMIT, 0.5f * oldRange);
            const float ratio = l / (max - min);

            const float p1 = (zoomStart - ypos) / (float)height;
            const float c = (max * (1.0 - p1) + min * p1);
            if (fixedMousePoint)
               min = c - ratio * (1.0f - p1) * oldRange,
               max = c + ratio * p1 * oldRange;
            else
               min = c - 0.5 * l,
               max = c + 0.5 * l;
         }
      }
   }

   if (spectral) {
      track->SetSpectrumBounds(min, max);
      if (partner)
         partner->SetSpectrumBounds(min, max);
   }
   else {
      track->SetDisplayBounds(min, max);
      if (partner)
         partner->SetDisplayBounds(min, max);
   }
}

namespace {
// Is the sample horizontally nearest to the cursor sufficiently separated from
// its neighbors that the pencil tool should be allowed to drag it?
bool SampleResolutionTest(const ViewInfo &viewInfo, const WaveTrack *wt, double time, double rate, int width)
{
   // Require more than 3 pixels per sample
   // Round to an exact sample time
   const double adjustedTime = wt->LongSamplesToTime(wt->TimeToLongSamples(time));
   const wxInt64 xx = std::max(wxInt64(0), viewInfo.TimeToPosition(adjustedTime));
   ZoomInfo::Intervals intervals;
   viewInfo.FindIntervals(rate, intervals, width);
   ZoomInfo::Intervals::const_iterator it = intervals.begin(), end = intervals.end(), prev;
   wxASSERT(it != end && it->position == 0);
   do
      prev = it++;
   while (it != end && it->position <= xx);
   const double threshold = 3 * rate; // three times as many pixels per second, as samples
   return prev->averageZoom > threshold;
}
}

/// Determines if we can edit samples in a wave track.
/// Also pops up warning messages in certain cases where we can't.
///  @return true if we can edit the samples, false otherwise.
bool TrackPanel::IsSampleEditingPossible( wxMouseEvent &event, const WaveTrack * wt )
{
   //Exit if it's not waveform display
   //Get out of here if we shouldn't be drawing right now:
   //If we aren't displaying the waveform, Display a message dialog
   const int display = wt->GetDisplay();

   if (WaveTrack::Waveform != display)
   {
      wxMessageBox(_("To use Draw, choose 'Waveform' in the Track Drop-down Menu."), _("Draw Tool"));
      return false;
   }

   bool showPoints;
   {
      wxRect rect;
      FindTrack(event.m_x, event.m_y, false, false, &rect);
      const double rate = wt->GetRate();
      const double time = mViewInfo->PositionToTime(event.m_x, rect.x);
      int width;
      GetTracksUsableArea(&width, NULL);
      showPoints = SampleResolutionTest(*mViewInfo, wt, time, rate, width);
   }

   //If we aren't zoomed in far enough, show a message dialog.
   if(!showPoints)
   {
      wxMessageBox(_("To use Draw, zoom in further until you can see the individual samples."), _("Draw Tool"));
      return false;
   }
   return true;
}

float TrackPanel::FindSampleEditingLevel(wxMouseEvent &event, double dBRange, double t0)
{
   // Calculate where the mouse is located vertically (between +/- 1)
   float zoomMin, zoomMax;
   mDrawingTrack->GetDisplayBounds(&zoomMin, &zoomMax);

   const int y = event.m_y - mDrawingTrackTop;
   const int height = mDrawingTrack->GetHeight() - (kTopMargin + kBottomMargin);
   const bool dB = !mDrawingTrack->GetWaveformSettings().isLinear();
   float newLevel =
      ::ValueOfPixel(y, height, false, dB, dBRange, zoomMin, zoomMax);

   //Take the envelope into account
   Envelope *const env = mDrawingTrack->GetEnvelopeAtX(event.m_x);
   if (env)
   {
      double envValue = env->GetValue(t0);
      if (envValue > 0)
         newLevel /= envValue;
      else
         newLevel = 0;

      //Make sure the NEW level is between +/-1
      newLevel = std::max(-1.0f, std::min(1.0f, newLevel));
   }

   return newLevel;
}

/// We're in a track view and zoomed enough to see the samples.
/// Someone has just clicked the mouse.  What do we do?
void TrackPanel::HandleSampleEditingClick( wxMouseEvent & event )
{
   //declare a rectangle to determine clicking position
   wxRect rect;

   //Get the track the mouse is over, and save it away for future events
   mDrawingTrack = NULL;
   Track *const t = FindTrack(event.m_x, event.m_y, false, false, &rect);

   if (!t || (t->GetKind() != Track::Wave))
      return;
   const auto wt = static_cast<WaveTrack *>(t);
   if( !IsSampleEditingPossible( event, wt ) )
      return;

   /// \todo Should mCapturedTrack take the place of mDrawingTrack??
   mDrawingTrack = wt;
   mDrawingTrackTop = rect.y + kTopMargin;

   //If we are still around, we are drawing in earnest.  Set some member data structures up:
   //First, calculate the starting sample.  To get this, we need the time
   double t0 = mViewInfo->PositionToTime(event.m_x, GetLeftOffset());

   //convert t0 to samples
   mDrawingStartSample = mDrawingTrack->TimeToLongSamples(t0);
   // quantize
   t0 = mDrawingTrack->LongSamplesToTime(mDrawingStartSample);

   //Determine how drawing should occur.  If alt is down,
   //do a smoothing, instead of redrawing.
   if( event.m_altDown )
   {
      //*************************************************
      //***  ALT-DOWN-CLICK (SAMPLE SMOOTHING)        ***
      //*************************************************
      //
      //  Smoothing works like this:  There is a smoothing kernel radius constant that
      //  determines how wide the averaging window is.  Plus, there is a smoothing brush radius,
      //  which determines how many pixels wide around the selected pixel this smoothing is applied.
      //
      //  Samples will be replaced by a mixture of the original points and the smoothed points,
      //  with a triangular mixing probability whose value at the center point is
      //  SMOOTHING_PROPORTION_MAX and at the far bounds is SMOOTHING_PROPORTION_MIN

      //Get the region of samples around the selected point
      int sampleRegionSize = 1 + 2 * (SMOOTHING_KERNEL_RADIUS + SMOOTHING_BRUSH_RADIUS);
      float *sampleRegion = new float[sampleRegionSize];
      float * newSampleRegion = new float[1 + 2 * SMOOTHING_BRUSH_RADIUS];

      //Get a sample  from the track to do some tricks on.
      mDrawingTrack->Get((samplePtr)sampleRegion, floatSample,
                                       mDrawingStartSample - SMOOTHING_KERNEL_RADIUS - SMOOTHING_BRUSH_RADIUS,
                                       sampleRegionSize);
      int i, j;

      //Go through each point of the smoothing brush and apply a smoothing operation.
      for(j = -SMOOTHING_BRUSH_RADIUS; j <= SMOOTHING_BRUSH_RADIUS; j++){
         float sumOfSamples = 0;
         for (i= -SMOOTHING_KERNEL_RADIUS; i <= SMOOTHING_KERNEL_RADIUS; i++){
            //Go through each point of the smoothing kernel and find the average

            //The average is a weighted average, scaled by a weighting kernel that is simply triangular
            // A triangular kernel across N items, with a radius of R ( 2 R + 1 points), if the farthest:
            // points have a probability of a, the entire triangle has total probability of (R + 1)^2.
            //      For sample number i and middle brush sample M,  (R + 1 - abs(M-i))/ ((R+1)^2) gives a
            //   legal distribution whose total probability is 1.
            //
            //
            //                weighting factor                       value
            sumOfSamples += (SMOOTHING_KERNEL_RADIUS + 1 - abs(i)) * sampleRegion[i + j + SMOOTHING_KERNEL_RADIUS + SMOOTHING_BRUSH_RADIUS];

         }
         newSampleRegion[j + SMOOTHING_BRUSH_RADIUS] = sumOfSamples/((SMOOTHING_KERNEL_RADIUS + 1) *(SMOOTHING_KERNEL_RADIUS + 1) );
      }


      // Now that the NEW sample levels are determined, go through each and mix it appropriately
      // with the original point, according to a 2-part linear function whose center has probability
      // SMOOTHING_PROPORTION_MAX and extends out SMOOTHING_BRUSH_RADIUS, at which the probability is
      // SMOOTHING_PROPORTION_MIN.  _MIN and _MAX specify how much of the smoothed curve make it through.

      float prob;

      for(j=-SMOOTHING_BRUSH_RADIUS; j <= SMOOTHING_BRUSH_RADIUS; j++){

         prob = SMOOTHING_PROPORTION_MAX - (float)abs(j)/SMOOTHING_BRUSH_RADIUS * (SMOOTHING_PROPORTION_MAX - SMOOTHING_PROPORTION_MIN);

         newSampleRegion[j+SMOOTHING_BRUSH_RADIUS] =
            newSampleRegion[j + SMOOTHING_BRUSH_RADIUS] * prob +
            sampleRegion[SMOOTHING_BRUSH_RADIUS + SMOOTHING_KERNEL_RADIUS + j] * (1 - prob);
      }
      //Set the sample to the point of the mouse event
      mDrawingTrack->Set((samplePtr)newSampleRegion, floatSample, mDrawingStartSample - SMOOTHING_BRUSH_RADIUS, 1 + 2 * SMOOTHING_BRUSH_RADIUS);

      //Clean this up right away to avoid a memory leak
      delete[] sampleRegion;
      delete[] newSampleRegion;

      mDrawingLastDragSampleValue = 0;
   }
   else
   {
      //*************************************************
      //***   PLAIN DOWN-CLICK (NORMAL DRAWING)       ***
      //*************************************************

      SetCapturedTrack(t, IsAdjustingSample);

      //Otherwise (e.g., the alt button is not down) do normal redrawing, based on the mouse position.
      const float newLevel = FindSampleEditingLevel
         (event, mDrawingTrack->GetWaveformSettings().dBRange, t0);

      //Set the sample to the point of the mouse event
      mDrawingTrack->Set((samplePtr)&newLevel, floatSample, mDrawingStartSample, 1);

      mDrawingLastDragSampleValue = newLevel;
   }

   //Set the member data structures for drawing
   mDrawingLastDragSample=mDrawingStartSample;

   //Redraw the region of the selected track
   RefreshTrack(mDrawingTrack);
}

void TrackPanel::HandleSampleEditingDrag( wxMouseEvent & event )
{
   //*************************************************
   //***    DRAG-DRAWING                           ***
   //*************************************************

   //The following will happen on a drag or a down-click.
   // The point should get re-drawn at the location of the mouse.
   //Exit if the mDrawingTrack is null.
   if( mDrawingTrack == NULL)
      return;

   //Exit dragging if the alt key is down--Don't allow left-right dragging for smoothing operation
   if (mMouseCapture != IsAdjustingSample)
      return;

   sampleCount s0;     //declare this for use below.  It designates the sample number which to draw.

   // Figure out what time the click was at
   //Find the point that we want to redraw at. If the control button is down,
   //adjust only the originally clicked-on sample

   if( event.m_controlDown) {
      //*************************************************
      //***   CTRL-DOWN (Hold Initial Sample Constant ***
      //*************************************************

      s0 = mDrawingStartSample;
   }
   else {
      //*************************************************
      //***    Normal CLICK-drag  (Normal drawing)    ***
      //*************************************************

      //Otherwise, adjust the sample you are dragging over right now.
      //convert this to samples
      const double t = mViewInfo->PositionToTime(event.m_x, GetLeftOffset());
      s0 = mDrawingTrack->TimeToLongSamples(t);
   }

   const double t0 = mDrawingTrack->LongSamplesToTime(s0);

   //Otherwise, do normal redrawing, based on the mouse position.
   // Calculate where the mouse is located vertically (between +/- 1)

   const float newLevel = FindSampleEditingLevel
      (event, mDrawingTrack->GetWaveformSettings().dBRange, t0);

   //Now, redraw all samples between current and last redrawn sample, inclusive
   //Go from the smaller to larger sample.
   const auto start = std::min( s0, mDrawingLastDragSample);
   const auto end   = std::max( s0, mDrawingLastDragSample);
   // Few enough samples to be drawn individually on screen will not
   // overflow size_t:
   const auto size = ( end - start + 1 ).as_size_t();
   if (size == 1) {
      mDrawingTrack->Set((samplePtr)&newLevel, floatSample, start, size);
   }
   else {
      std::vector<float> values(size);
      for (auto i = start; i <= end; ++i) {
         //This interpolates each sample linearly:
         // i - start will not overflow size_t either:
         values[( i - start ).as_size_t()] =
            mDrawingLastDragSampleValue + (newLevel - mDrawingLastDragSampleValue)  *
            (i - mDrawingLastDragSample).as_float() /
            (s0 - mDrawingLastDragSample).as_float();
      }
      mDrawingTrack->Set((samplePtr)&values[0], floatSample, start, size);
   }

   //Update the member data structures.
   mDrawingLastDragSample=s0;
   mDrawingLastDragSampleValue = newLevel;

   //Redraw the region of the selected track
   RefreshTrack(mDrawingTrack);
}

void TrackPanel::HandleSampleEditingButtonUp( wxMouseEvent & WXUNUSED(event))
{
   //*************************************************
   //***    UP-CLICK  (Finish drawing)             ***
   //*************************************************
   SetCapturedTrack( NULL );
   //On up-click, send the state to the undo stack
   mDrawingTrack=NULL;       //Set this to NULL so it will catch improper drag events.
   MakeParentPushState(_("Moved Samples"),
                       _("Sample Edit"),
                       UndoPush::CONSOLIDATE | UndoPush::AUTOSAVE);
}


/// This handles adjusting individual samples by hand using the draw tool(s)
///
/// There are several member data structure for handling drawing:
///  - mDrawingTrack:               keeps track of which track you clicked down on, so drawing doesn't
///                                 jump to a NEW track
///  - mDrawingTrackTop:            The top position of the drawing track--makes drawing easier.
///  - mDrawingStartSample:         The sample you clicked down on, so that you can hold it steady
///  - mDrawingLastDragSample:      When drag-drawing, this keeps track of the last sample you dragged over,
///                                 so it can smoothly redraw samples that got skipped over
///  - mDrawingLastDragSampleValue: The value of the last
void TrackPanel::HandleSampleEditing(wxMouseEvent & event)
{
   if (event.LeftDown() ) {
      HandleSampleEditingClick( event);
   } else if (mDrawingTrack && event.Dragging()) {
      HandleSampleEditingDrag( event );
   }  else if(mDrawingTrack && event.ButtonUp()) {
      HandleSampleEditingButtonUp( event );
   }
}


// This is for when a given track gets the x.
void TrackPanel::HandleClosing(wxMouseEvent & event)
{
   Track *t = mCapturedTrack;
   wxRect rect = mCapturedRect;

   wxRect closeRect;
   mTrackInfo.GetCloseBoxRect(rect, closeRect);

   wxClientDC dc(this);

   if (event.Dragging())
      mTrackInfo.DrawCloseBox(&dc, rect, closeRect.Contains(event.m_x, event.m_y));
   else if (event.LeftUp()) {
      mTrackInfo.DrawCloseBox(&dc, rect, false);
      if (closeRect.Contains(event.m_x, event.m_y)) {
         if (!IsUnsafe())
            GetProject()->RemoveTrack(t);
      }
      SetCapturedTrack( NULL );
   }

   this->UpdateViewIfNoTracks();
   this->Refresh(false);
}

void TrackPanel::UpdateViewIfNoTracks()
{
   if (mTracks->IsEmpty())
   {
      // BG: There are no more tracks on screen
      //BG: Set zoom to normal
      mViewInfo->SetZoom(ZoomInfo::GetDefaultZoom());

      //STM: Set selection to 0,0
      //PRL: and default the rest of the selection information
      mViewInfo->selectedRegion = SelectedRegion();

      // PRL:  Following causes the time ruler to align 0 with left edge.
      // Bug 972
      mViewInfo->h = 0;

      mListener->TP_RedrawScrollbars();
      mListener->TP_DisplayStatusMessage(wxT("")); //STM: Clear message if all tracks are removed
   }
}

void TrackPanel::HandlePopping(wxMouseEvent & event)
{
   Track *t = mCapturedTrack;
   wxRect rect = mCapturedRect;

   if( t==NULL ){
      SetCapturedTrack( NULL );
      return;
   }

   wxRect titleRect;
   mTrackInfo.GetTitleBarRect(rect, titleRect);

   wxClientDC dc(this);

   if (event.Dragging()) {
      mTrackInfo.DrawTitleBar(&dc, rect, t, titleRect.Contains(event.m_x, event.m_y));
   }
   else if (event.LeftUp()) {
      if (titleRect.Contains(event.m_x, event.m_y))
      {
         OnTrackMenu(t);
      }

      SetCapturedTrack( NULL );

      mTrackInfo.DrawTitleBar(&dc, rect, t, false);
   }
}

/// Handle when the mute or solo button is pressed for some track.
void TrackPanel::HandleMutingSoloing(wxMouseEvent & event, bool solo)
{
   Track *t = mCapturedTrack;
   wxRect rect = mCapturedRect;

   if( t==NULL ){
      wxASSERT(false);// Soloing or muting but no captured track!
      SetCapturedTrack( NULL );
      return;
   }

   wxRect buttonRect;
   mTrackInfo.GetMuteSoloRect(rect, buttonRect, solo, HasSoloButton());

   wxClientDC dc(this);

   if (event.Dragging()){
         mTrackInfo.DrawMuteSolo(&dc, rect, t, buttonRect.Contains(event.m_x, event.m_y),
                   solo, HasSoloButton());
   }
   else if (event.LeftUp() )
   {
      if (buttonRect.Contains(event.m_x, event.m_y))
      {
         // For either, MakeParentPushState to make the track state dirty.
         if(solo)
            GetProject()->DoTrackSolo(t, event.ShiftDown());
         else
            GetProject()->DoTrackMute(t, event.ShiftDown());
      }
      SetCapturedTrack( NULL );
      // mTrackInfo.DrawMuteSolo(&dc, rect, t, false, solo);
      Refresh(false);
   }
}

void TrackPanel::HandleMinimizing(wxMouseEvent & event)
{
   Track *t = mCapturedTrack;
   wxRect rect = mCapturedRect;

   if (t == NULL) {
      SetCapturedTrack(NULL);
      return;
   }

   wxRect buttonRect;
   mTrackInfo.GetMinimizeRect(rect, buttonRect);

   wxClientDC dc(this);

   if (event.Dragging()) {
      mTrackInfo.DrawMinimize(&dc, rect, t, buttonRect.Contains(event.m_x, event.m_y));
   }
   else if (event.LeftUp()) {
      if (buttonRect.Contains(event.m_x, event.m_y)) {
         t->SetMinimized(!t->GetMinimized());
         if (mTracks->GetLink(t))
            mTracks->GetLink(t)->SetMinimized(t->GetMinimized());
         MakeParentRedrawScrollbars();
         MakeParentModifyState(true);
      }

      SetCapturedTrack(NULL);

      mTrackInfo.DrawMinimize(&dc, rect, t, false);
      Refresh(false);
      GetActiveProject()->RedrawProject();
   }
}

void TrackPanel::HandleSliders(wxMouseEvent &event, bool pan)
{
   LWSlider *slider;
#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
   bool panZero = false;
#endif

   // On the Mac, we'll lose track capture if the slider dialog
   // is displayed, but it doesn't hurt to do this for all plats.
   WaveTrack *capturedTrack = (WaveTrack *) mCapturedTrack;

   if (pan)
      slider = mTrackInfo.PanSlider(capturedTrack, true);
   else
      slider = mTrackInfo.GainSlider(capturedTrack, true);

   slider->OnMouseEvent(event);

   //If we have a double-click, do this...
   if (event.LeftDClick())
      mMouseCapture = IsUncaptured;

   float newValue = slider->Get();
   MixerBoard* pMixerBoard = this->GetMixerBoard(); // Update mixer board, too.
#ifdef EXPERIMENTAL_MIDI_OUT
  if (capturedTrack->GetKind() == Track::Wave)
#endif
  {
     const auto wt = static_cast<WaveTrack*>(capturedTrack);

   // Assume linked track is wave or null
   const auto link = static_cast<WaveTrack *>(mTracks->GetLink(wt));

   if (pan) {
#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
      panZero = wt->SetPan(newValue);
#else
      wt->SetPan(newValue);
#endif
      if (link)
         link->SetPan(newValue);

#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
      if(panZero) MakeParentRedrawScrollbars();
#endif

      if (pMixerBoard)
         pMixerBoard->UpdatePan(wt);
   }
   else {
      wt->SetGain(newValue);
      if (link)
         link->SetGain(newValue);

      if (pMixerBoard)
         pMixerBoard->UpdateGain(wt);
   }
  }
#ifdef EXPERIMENTAL_MIDI_OUT
  else {
     // mCapturedTrack is not wave...
      if (!pan) {
         // .. so assume it is note
         static_cast<NoteTrack*>(mCapturedTrack)->SetGain(newValue);
#ifdef EXPERIMENTAL_MIXER_BOARD
            if (pMixerBoard)
               // probably should modify UpdateGain to take a track that is
               // either a WaveTrack or a NoteTrack.
               pMixerBoard->UpdateGain((WaveTrack*)capturedTrack);
#endif
      }
   }
#endif

   RefreshTrack(capturedTrack);

   if (event.ButtonUp()) {
#ifdef EXPERIMENTAL_MIDI_OUT
    if (capturedTrack->GetKind() == Track::Wave) {
#endif
      MakeParentPushState(pan ? _("Moved pan slider") : _("Moved gain slider"),
                          pan ? _("Pan") : _("Gain"),
                          UndoPush::CONSOLIDATE);
#ifdef EXPERIMENTAL_MIDI_OUT
    } else {
      MakeParentPushState(_("Moved velocity slider"), _("Velocity"), UndoPush::CONSOLIDATE);
    }
#endif
      SetCapturedTrack( NULL );
   }
}

// The tracks positions within the list have changed, so update the vertical
// ruler size for the track that triggered the event.
void TrackPanel::OnTrackListResized(wxCommandEvent & e)
{
   Track *t = (Track *) e.GetClientData();
   UpdateVRuler(t);
   e.Skip();
}

// Tracks have been added or removed from the list.  Handle adds as if
// a resize has taken place.
void TrackPanel::OnTrackListUpdated(wxCommandEvent & e)
{
   // Tracks may have been deleted, so check to see if the focused track was on of them.
   if (!mTracks->Contains(GetFocusedTrack())) {
      SetFocusedTrack(NULL);
   }

   if (mCapturedTrack &&
       !mTracks->Contains(mCapturedTrack)) {
      SetCapturedTrack(nullptr);
      if (HasCapture())
         ReleaseMouse();
   }

   if (mFreqSelTrack &&
       !mTracks->Contains(mFreqSelTrack)) {
      mFreqSelTrack = nullptr;
      if (HasCapture())
         ReleaseMouse();
   }

   if (mPopupMenuTarget &&
       !mTracks->Contains(mPopupMenuTarget)) {
      mPopupMenuTarget = nullptr;
      if (HasCapture())
         ReleaseMouse();
   }

   if (mLastPickedTrack && !mTracks->Contains(mLastPickedTrack))
      mLastPickedTrack = nullptr;

   if (e.GetClientData()) {
      OnTrackListResized(e);
      return;
   }

   e.Skip();
}

void TrackPanel::OnContextMenu(wxContextMenuEvent & WXUNUSED(event))
{
   OnTrackMenu();
}

/// This handles when the user clicks on the "Label" area
/// of a track, ie the part with all the buttons and the drop
/// down menu, etc.
// That is, TrackInfo and vertical ruler rect.
void TrackPanel::HandleLabelClick(wxMouseEvent & event)
{
   // AS: If not a click, ignore the mouse event.
   if (!event.ButtonDown() && !event.ButtonDClick()) {
      return;
   }

   // MIDI tracks use the right mouse button, but other tracks get confused
   // if they see anything other than a left click.
   bool isleft = event.Button(wxMOUSE_BTN_LEFT);

   bool unsafe = IsUnsafe();

   wxRect rect;

   Track *t = FindTrack(event.m_x, event.m_y, true, true, &rect);

   // LL: Check close box
   if (isleft && CloseFunc(t, rect, event.m_x, event.m_y))
      return;

   // LL: Check title bar for popup
   if (isleft && PopupFunc(t, rect, event.m_x, event.m_y))
      return;

   // VJ: Check sync-lock icon and the blank area to the left of the minimize button.
   // Have to do it here, because if track is shrunk such that these areas occlude controls,
   // e.g., mute/solo, don't want the "Funcs" below to set up handling.
   // Only result of doing so is to select the track. Don't care whether isleft.
   bool bTrackSelClick = this->TrackSelFunc(t, rect, event.m_x, event.m_y);
   if (!bTrackSelClick)
   {
      // MM: Check minimize buttons on WaveTracks. Must be before
      //     solo/mute buttons, sliders etc.
      if (isleft && MinimizeFunc(t, rect, event.m_x, event.m_y))
         return;

      if (isleft && t->GetKind() == Track::Wave)
      {
         // DM: Check Mute and Solo buttons on WaveTracks:
         if (MuteSoloFunc(t, rect, event.m_x, event.m_y, false) ||
            MuteSoloFunc(t, rect, event.m_x, event.m_y, true))
            return;

         if (GainFunc(t, rect, event, event.m_x, event.m_y))
            return;

         if (PanFunc(t, rect, event, event.m_x, event.m_y))
            return;
      }
#ifdef USE_MIDI
      // DM: If it's a NoteTrack, it has special controls
      else if (t->GetKind() == Track::Note)
      {
         wxRect midiRect;
#ifdef EXPERIMENTAL_MIDI_OUT
         // this is an awful hack: make a NEW rectangle at an offset because
         // MuteSoloFunc thinks buttons are located below some text, e.g.
         // "Mono, 44100Hz 32-bit float", but this is not true for a Note track
         wxRect muteSoloRect(rect);
         muteSoloRect.y -= 34; // subtract the height of wave track text
         if (MuteSoloFunc(t, muteSoloRect, event.m_x, event.m_y, false) ||
            MuteSoloFunc(t, muteSoloRect, event.m_x, event.m_y, true))
            return;

         // this is a similar hack: GainFunc expects a Wave track slider, so it's
         // looking in the wrong place. We pass it a bogus rectangle created when
         // the slider was placed to "fake" GainFunc into finding the slider in
         // its actual location.
         if (GainFunc(t, ((NoteTrack *) t)->GetGainPlacementRect(),
            event, event.m_x, event.m_y))
            return;
#endif
         mTrackInfo.GetTrackControlsRect(rect, midiRect);
         if (midiRect.Contains(event.m_x, event.m_y) &&
            ((NoteTrack *)t)->LabelClick(midiRect, event.m_x, event.m_y,
            event.Button(wxMOUSE_BTN_RIGHT))) {
            Refresh(false);
            return;
         }
      }
#endif // USE_MIDI

   }

   if (!isleft) {
      return;
   }

   // DM: If they weren't clicking on a particular part of a track label,
   //  deselect other tracks and select this one.

   // JH: also, capture the current track for rearranging, so the user
   //  can drag the track up or down to swap it with others
   if (!unsafe) {
      mRearrangeCount = 0;
      SetCapturedTrack(t, IsRearranging);
      TrackPanel::CalculateRearrangingThresholds(event);
   }

   HandleListSelection(t, event.ShiftDown(), event.ControlDown(), !unsafe);
}

void TrackPanel::HandleListSelection(Track *t, bool shift, bool ctrl,
                                     bool modifyState)
{
   // AS: If the shift button is being held down, invert
   //  the selection on this track.
   if (ctrl) {
      SelectTrack(t, !t->GetSelected(), false);
      Refresh(false);
   }
   else {
      if (shift && mLastPickedTrack)
         ChangeSelectionOnShiftClick( t );
      else{
         SelectNone();
         SelectTrack(t, true);
         SelectTrackLength(t);
      }

      SetFocusedTrack(t);
      this->Refresh(false);
      MixerBoard* pMixerBoard = this->GetMixerBoard();
      if (pMixerBoard)
         pMixerBoard->RefreshTrackClusters();
   }

   if (modifyState)
      MakeParentModifyState(true);
}

/// The user is dragging one of the tracks: change the track order
/// accordingly
void TrackPanel::HandleRearrange(wxMouseEvent & event)
{
   // are we finishing the drag?
   if (event.LeftUp()) {
      if (mRearrangeCount != 0) {
         wxString dir;
         dir = mRearrangeCount < 0 ? _("up") : _("down");
         MakeParentPushState(wxString::Format(_("Moved '%s' %s"),
            mCapturedTrack->GetName().c_str(),
            dir.c_str()),
            _("Move Track"));
      }

      SetCapturedTrack(NULL);
      SetCursor(*mArrowCursor);
      return;
   }

   // probably harmless during play?  However, we do disallow the click, so check this too.
   bool unsafe = IsUnsafe();
   if (unsafe)
      return;

   MixerBoard* pMixerBoard = this->GetMixerBoard(); // Update mixer board, too.
   if (event.m_y < mMoveUpThreshold || event.m_y < 0) {
      mTracks->MoveUp(mCapturedTrack);
      --mRearrangeCount;
#ifdef EXPERIMENTAL_MIDI_OUT
      if (pMixerBoard && (mCapturedTrack->GetKind() == Track::Wave ||
                          mCapturedTrack->GetKind() == Track::Note))
         pMixerBoard->MoveTrackCluster(mCapturedTrack, true /* up */);
#else
      if (pMixerBoard && (mCapturedTrack->GetKind() == Track::Wave))
         pMixerBoard->MoveTrackCluster((WaveTrack*)mCapturedTrack, true /* up */);
#endif
   }
   else if (event.m_y > mMoveDownThreshold || event.m_y > GetRect().GetHeight()) {
      mTracks->MoveDown(mCapturedTrack);
      ++mRearrangeCount;
      /* i18n-hint: a direction as in up or down.*/
#ifdef EXPERIMENTAL_MIDI_OUT
      if (pMixerBoard && (mCapturedTrack->GetKind() == Track::Wave ||
                          mCapturedTrack->GetKind() == Track::Note))
         pMixerBoard->MoveTrackCluster(mCapturedTrack, false /* down */);
#else
      if (pMixerBoard && (mCapturedTrack->GetKind() == Track::Wave))
         pMixerBoard->MoveTrackCluster((WaveTrack*)mCapturedTrack, false /* down */);
#endif
   }
   else
   {
      return;
   }

   // JH: if we moved up or down, recalculate the thresholds and make sure the
   // track is fully on-screen.
   TrackPanel::CalculateRearrangingThresholds(event);
   EnsureVisible(mCapturedTrack);
}

/// Figure out how far the user must drag the mouse up or down
/// before the track will swap with the one above or below
void TrackPanel::CalculateRearrangingThresholds(wxMouseEvent & event)
{
   wxASSERT(mCapturedTrack);

   // JH: this will probably need to be tweaked a bit, I'm just
   //   not sure what formula will have the best feel for the
   //   user.
   if (mTracks->CanMoveUp(mCapturedTrack))
      mMoveUpThreshold =
          event.m_y - mTracks->GetGroupHeight( mTracks->GetPrev(mCapturedTrack,true) );
   else
      mMoveUpThreshold = INT_MIN;

   if (mTracks->CanMoveDown(mCapturedTrack))
      mMoveDownThreshold =
          event.m_y + mTracks->GetGroupHeight( mTracks->GetNext(mCapturedTrack,true) );
   else
      mMoveDownThreshold = INT_MAX;
}

bool TrackPanel::GainFunc(Track * t, wxRect rect, wxMouseEvent &event,
                          int x, int y)
{
   wxRect sliderRect;
   mTrackInfo.GetGainRect(rect, sliderRect);
   if (!sliderRect.Contains(x, y))
      return false;

   SetCapturedTrack( t, IsGainSliding);
   mCapturedRect = rect;
   HandleSliders(event, false);

   return true;
}

bool TrackPanel::PanFunc(Track * t, wxRect rect, wxMouseEvent &event,
                         int x, int y)
{
   wxRect sliderRect;
   mTrackInfo.GetPanRect(rect, sliderRect);
   if (!sliderRect.Contains(x, y))
      return false;

   SetCapturedTrack( t, IsPanSliding);
   mCapturedRect = rect;
   HandleSliders(event, true);

   return true;
}

/// Mute or solo the given track (t).  If solo is true, we're
/// soloing, otherwise we're muting.  Basically, check and see
/// whether x and y fall within the  area of the appropriate button.
bool TrackPanel::MuteSoloFunc(Track * t, wxRect rect, int x, int y,
                              bool solo)
{
   wxRect buttonRect;
   mTrackInfo.GetMuteSoloRect(rect, buttonRect, solo, HasSoloButton());
   if (!buttonRect.Contains(x, y))
      return false;

   wxClientDC dc(this);
   SetCapturedTrack( t, solo ? IsSoloing : IsMuting);
   mCapturedRect = rect;

   mTrackInfo.DrawMuteSolo(&dc, rect, t, true, solo, HasSoloButton());
   return true;
}

bool TrackPanel::TrackSelFunc(Track * WXUNUSED(t), wxRect rect, int x, int y)
{
   // First check the blank space to left of minimize button.
   wxRect selRect;
   mTrackInfo.GetMinimizeRect(rect, selRect); // for y and height
   selRect.x = rect.x;
   selRect.width = 16; // (kTrackInfoBtnSize)
   selRect.height++;
   if (selRect.Contains(x, y))
      return true;

   // Try the sync-lock rect.
   mTrackInfo.GetSyncLockIconRect(rect, selRect);
   selRect.height++;
   return selRect.Contains(x, y);
}

bool TrackPanel::MinimizeFunc(Track * t, wxRect rect, int x, int y)
{
   wxRect buttonRect;
   mTrackInfo.GetMinimizeRect(rect, buttonRect);
   if (!buttonRect.Contains(x, y))
      return false;

   SetCapturedTrack(t, IsMinimizing);
   mCapturedRect = rect;

   wxClientDC dc(this);
   mTrackInfo.DrawMinimize(&dc, rect, t, true);

   return true;
}

bool TrackPanel::CloseFunc(Track * t, wxRect rect, int x, int y)
{
   wxRect closeRect;
   mTrackInfo.GetCloseBoxRect(rect, closeRect);

   if (!closeRect.Contains(x, y))
      return false;

   wxClientDC dc(this);
   SetCapturedTrack( t, IsClosing );
   mCapturedRect = rect;

   mTrackInfo.DrawCloseBox(&dc, rect, true);
   return true;
}

bool TrackPanel::PopupFunc(Track * t, wxRect rect, int x, int y)
{
   wxRect titleRect;
   mTrackInfo.GetTitleBarRect(rect, titleRect);
   if (!titleRect.Contains(x, y))
      return false;

   wxClientDC dc(this);
   SetCapturedTrack( t, IsPopping );
   mCapturedRect = rect;

   mTrackInfo.DrawTitleBar(&dc, rect, t, true);
   return true;
}

///  ButtonDown means they just clicked and haven't released yet.
///  We use this opportunity to save which track they clicked on,
///  and the initial height of the track, so as they drag we can
///  update the track size.
void TrackPanel::HandleResizeClick( wxMouseEvent & event )
{
   wxRect rTrack;
   wxRect rLabel;

   // DM: Figure out what track is about to be resized
   Track *track = FindTrack(event.m_x, event.m_y, false, false, &rTrack);

   if (!track) {
      // This will only return unlinked tracks or left channels of stereo tracks
      // or NULL:
      track = FindTrack(event.m_x, event.m_y, true, true, &rLabel);
      // If stereo, get the right channel.
      if (track && track->GetLinked())
         track = track->GetLink();
   }

   if (!track) {
      return;
   }

   mMouseClickY = event.m_y;

#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
   // To do: escape key
   if(MONO_WAVE_PAN(t)){
      //STM:  Determine whether we should rescale one or two tracks
      if (t->GetVirtualStereo()) {
         // mCapturedTrack is the lower track
         mInitialTrackHeight = t->GetHeight(true);
         mInitialUpperTrackHeight = t->GetHeight();
         SetCapturedTrack(t, IsResizingBelowLinkedTracks);
      }
      else {
         // mCapturedTrack is the upper track
         mInitialTrackHeight = t->GetHeight(true);
         mInitialUpperTrackHeight = t->GetHeight();
         SetCapturedTrack(t, IsResizingBetweenLinkedTracks);
      }
   }else{
      Track *prev = mTracks->GetPrev(t);
      Track *next = mTracks->GetNext(t);

      //STM:  Determine whether we should rescale one or two tracks
      if (prev && prev->GetLink() == t) {
         // mCapturedTrack is the lower track
         mInitialTrackHeight = t->GetHeight();
         mInitialMinimized = t->GetMinimized();
         mInitialUpperTrackHeight = prev->GetHeight();
         SetCapturedTrack(t, IsResizingBelowLinkedTracks);
      }
      else if (next && t->GetLink() == next) {
         // mCapturedTrack is the upper track
         mInitialTrackHeight = next->GetHeight();
         mInitialMinimized = next->GetMinimized();
         mInitialUpperTrackHeight = t->GetHeight();
         SetCapturedTrack(t, IsResizingBetweenLinkedTracks);
      }
      else {
         // DM: Save the initial mouse location and the initial height
         mInitialTrackHeight = t->GetHeight();
         mInitialMinimized = t->GetMinimized();
         SetCapturedTrack(t, IsResizing);
      }
   }
#else // EXPERIMENTAL_OUTPUT_DISPLAY
   Track *prev = mTracks->GetPrev(track);
   Track *next = mTracks->GetNext(track);

   //STM:  Determine whether we should rescale one or two tracks
   if (prev && prev->GetLink() == track) {
      // mCapturedTrack is the lower track
      mInitialTrackHeight = track->GetHeight();
      mInitialActualHeight = track->GetActualHeight();
      mInitialMinimized = track->GetMinimized();
      mInitialUpperTrackHeight = prev->GetHeight();
      mInitialUpperActualHeight = prev->GetActualHeight();
      SetCapturedTrack(track, IsResizingBelowLinkedTracks);
   }
   else if (next && track->GetLink() == next) {
      // mCapturedTrack is the upper track
      mInitialTrackHeight = next->GetHeight();
      mInitialActualHeight = next->GetActualHeight();
      mInitialMinimized = next->GetMinimized();
      mInitialUpperTrackHeight = track->GetHeight();
      mInitialUpperActualHeight = track->GetActualHeight();
      SetCapturedTrack(track, IsResizingBetweenLinkedTracks);
   }
   else {
      // DM: Save the initial mouse location and the initial height
      mInitialTrackHeight = track->GetHeight();
      mInitialActualHeight = track->GetActualHeight();
      mInitialMinimized = track->GetMinimized();
      SetCapturedTrack(track, IsResizing);
   }
#endif // EXPERIMENTAL_OUTPUT_DISPLAY
}

///  This happens when the button is released from a drag.
///  Since we actually took care of resizing the track when
///  we got drag events, all we have to do here is clean up.
///  We also modify the undo state (the action doesn't become
///  undo-able, but it gets merged with the previous undo-able
///  event).
void TrackPanel::HandleResizeButtonUp(wxMouseEvent & WXUNUSED(event))
{
   SetCapturedTrack( NULL );
   MakeParentRedrawScrollbars();
   MakeParentModifyState(false);
}

///  Resize dragging means that the mouse button IS down and has moved
///  from its initial location.  By the time we get here, we
///  have already received a ButtonDown() event and saved the
///  track being resized in mCapturedTrack.
void TrackPanel::HandleResizeDrag(wxMouseEvent & event)
{
   int delta = (event.m_y - mMouseClickY);

   // On first drag, jump out of minimized mode.  Initial height
   // will be height of minimized track.
   //
   // This used to be in HandleResizeClick(), but simply clicking
   // on a resize border would switch the minimized state.
   if (mCapturedTrack->GetMinimized()) {
      Track *link = mCapturedTrack->GetLink();

      mCapturedTrack->SetHeight(mCapturedTrack->GetHeight());
      mCapturedTrack->SetMinimized(false);

      if (link) {
         link->SetHeight(link->GetHeight());
         link->SetMinimized(false);
         // Initial values must be reset since they weren't based on the
         // minimized heights.
         mInitialUpperTrackHeight = link->GetHeight();
         mInitialTrackHeight = mCapturedTrack->GetHeight();
      }
#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
      else if(MONO_WAVE_PAN(mCapturedTrack)){
         mCapturedTrack->SetMinimized(false);
         mInitialUpperTrackHeight = mCapturedTrack->GetHeight();
         mInitialTrackHeight = mCapturedTrack->GetHeight(true);
      }
#endif
   }

   //STM: We may be dragging one or two (stereo) tracks.
   // If two, resize proportionally if we are dragging the lower track, and
   // adjust compensatively if we are dragging the upper track.
#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
   switch( mMouseCapture )
   {
   case IsResizingBelowLinkedTracks:
      {
         if(MONO_WAVE_PAN(mCapturedTrack)){
            double proportion = static_cast < double >(mInitialTrackHeight)
                / (mInitialTrackHeight + mInitialUpperTrackHeight);

            int newTrackHeight = static_cast < int >
                (mInitialTrackHeight + delta * proportion);

            int newUpperTrackHeight = static_cast < int >
                (mInitialUpperTrackHeight + delta * (1.0 - proportion));

            //make sure neither track is smaller than its minimum height
            if (newTrackHeight < mCapturedTrack->GetMinimizedHeight())
               newTrackHeight = mCapturedTrack->GetMinimizedHeight();
            if (newUpperTrackHeight < mCapturedTrack->GetMinimizedHeight())
               newUpperTrackHeight = mCapturedTrack->GetMinimizedHeight();

            mCapturedTrack->SetHeight(newTrackHeight,true);
            mCapturedTrack->SetHeight(newUpperTrackHeight);
         }
         else{
            Track *prev = mTracks->GetPrev(mCapturedTrack);

            double proportion = static_cast < double >(mInitialTrackHeight)
                / (mInitialTrackHeight + mInitialUpperTrackHeight);

            int newTrackHeight = static_cast < int >
                (mInitialTrackHeight + delta * proportion);

            int newUpperTrackHeight = static_cast < int >
                (mInitialUpperTrackHeight + delta * (1.0 - proportion));

            //make sure neither track is smaller than its minimum height
            if (newTrackHeight < mCapturedTrack->GetMinimizedHeight())
               newTrackHeight = mCapturedTrack->GetMinimizedHeight();
            if (newUpperTrackHeight < prev->GetMinimizedHeight())
               newUpperTrackHeight = prev->GetMinimizedHeight();

            mCapturedTrack->SetHeight(newTrackHeight);
            prev->SetHeight(newUpperTrackHeight);
         }
         break;
      }
   case IsResizingBetweenLinkedTracks:
      {
         if(MONO_WAVE_PAN(mCapturedTrack)){
            int newUpperTrackHeight = mInitialUpperTrackHeight + delta;
            int newTrackHeight = mInitialTrackHeight - delta;

            // make sure neither track is smaller than its minimum height
            if (newTrackHeight < mCapturedTrack->GetMinimizedHeight()) {
               newTrackHeight = mCapturedTrack->GetMinimizedHeight();
               newUpperTrackHeight =
                   mInitialUpperTrackHeight + mInitialTrackHeight - mCapturedTrack->GetMinimizedHeight();
            }
            if (newUpperTrackHeight < mCapturedTrack->GetMinimizedHeight()) {
               newUpperTrackHeight = mCapturedTrack->GetMinimizedHeight();
               newTrackHeight =
                   mInitialUpperTrackHeight + mInitialTrackHeight - mCapturedTrack->GetMinimizedHeight();
            }
            float temp = 1.0f;
            if(newUpperTrackHeight != 0.0f)
               temp = (float)newUpperTrackHeight/(float)(newUpperTrackHeight + newTrackHeight);

            mCapturedTrack->SetVirtualTrackPercentage(temp);
            mCapturedTrack->SetHeight(newUpperTrackHeight);
            mCapturedTrack->SetHeight(newTrackHeight,true);
         }
         else{
            Track *next = mTracks->GetNext(mCapturedTrack);
            int newUpperTrackHeight = mInitialUpperTrackHeight + delta;
            int newTrackHeight = mInitialTrackHeight - delta;

            // make sure neither track is smaller than its minimum height
            if (newTrackHeight < next->GetMinimizedHeight()) {
               newTrackHeight = next->GetMinimizedHeight();
               newUpperTrackHeight =
                   mInitialUpperTrackHeight + mInitialTrackHeight - next->GetMinimizedHeight();
            }
            if (newUpperTrackHeight < mCapturedTrack->GetMinimizedHeight()) {
               newUpperTrackHeight = mCapturedTrack->GetMinimizedHeight();
               newTrackHeight =
                   mInitialUpperTrackHeight + mInitialTrackHeight - mCapturedTrack->GetMinimizedHeight();
            }

            mCapturedTrack->SetHeight(newUpperTrackHeight);
            next->SetHeight(newTrackHeight);
            break;
         }
         break;
      }
   case IsResizing:
      {
         int newTrackHeight = mInitialTrackHeight + delta;
         if (newTrackHeight < mCapturedTrack->GetMinimizedHeight())
            newTrackHeight = mCapturedTrack->GetMinimizedHeight();
         mCapturedTrack->SetHeight(newTrackHeight);
         break;
      }
   default:
      // don't refresh in this case.
      return;
   }
#else // EXPERIMENTAL_OUTPUT_DISPLAY
   switch( mMouseCapture )
   {
   case IsResizingBelowLinkedTracks:
      {
         Track *prev = mTracks->GetPrev(mCapturedTrack);

         double proportion = static_cast < double >(mInitialTrackHeight)
             / (mInitialTrackHeight + mInitialUpperTrackHeight);

         int newTrackHeight = static_cast < int >
             (mInitialTrackHeight + delta * proportion);

         int newUpperTrackHeight = static_cast < int >
             (mInitialUpperTrackHeight + delta * (1.0 - proportion));

         //make sure neither track is smaller than its minimum height
         if (newTrackHeight < mCapturedTrack->GetMinimizedHeight())
            newTrackHeight = mCapturedTrack->GetMinimizedHeight();
         if (newUpperTrackHeight < prev->GetMinimizedHeight())
            newUpperTrackHeight = prev->GetMinimizedHeight();

         mCapturedTrack->SetHeight(newTrackHeight);
         prev->SetHeight(newUpperTrackHeight);
         break;
      }
   case IsResizingBetweenLinkedTracks:
      {
         Track *next = mTracks->GetNext(mCapturedTrack);
         int newUpperTrackHeight = mInitialUpperTrackHeight + delta;
         int newTrackHeight = mInitialTrackHeight - delta;

         // make sure neither track is smaller than its minimum height
         if (newTrackHeight < next->GetMinimizedHeight()) {
            newTrackHeight = next->GetMinimizedHeight();
            newUpperTrackHeight =
                mInitialUpperTrackHeight + mInitialTrackHeight - next->GetMinimizedHeight();
         }
         if (newUpperTrackHeight < mCapturedTrack->GetMinimizedHeight()) {
            newUpperTrackHeight = mCapturedTrack->GetMinimizedHeight();
            newTrackHeight =
                mInitialUpperTrackHeight + mInitialTrackHeight - mCapturedTrack->GetMinimizedHeight();
         }

         mCapturedTrack->SetHeight(newUpperTrackHeight);
         next->SetHeight(newTrackHeight);
         break;
      }
   case IsResizing:
      {
         int newTrackHeight = mInitialTrackHeight + delta;
         if (newTrackHeight < mCapturedTrack->GetMinimizedHeight())
            newTrackHeight = mCapturedTrack->GetMinimizedHeight();
         mCapturedTrack->SetHeight(newTrackHeight);
         break;
      }
   default:
      // don't refresh in this case.
      return;
   }
#endif // EXPERIMENTAL_OUTPUT_DISPLAY
   Refresh(false);
}

/// HandleResize gets called when:
///  - A mouse-down event occurs in the "resize region" of a track,
///    i.e. to change its vertical height.
///  - A mouse event occurs and mIsResizing==true (i.e. while
///    the resize is going on)
void TrackPanel::HandleResize(wxMouseEvent & event)
{
   if (event.LeftDown()) {
      HandleResizeClick( event );
   }
   else if (event.LeftUp())
   {
      HandleResizeButtonUp( event );
   }
   else if (event.Dragging()) {
      HandleResizeDrag( event );
   }
}

/// Handle mouse wheel rotation (for zoom in/out, vertical and horizontal scrolling)
void TrackPanel::HandleWheelRotation(wxMouseEvent & event)
{
   double steps {};
#if defined(__WXMAC__) && defined(EVT_MAGNIFY)
   // PRL:
   // Pinch and spread implemented in wxWidgets 3.1.0, or cherry-picked from
   // the future in custom build of 3.0.2
   if (event.Magnify()) {
      event.SetControlDown(true);
      steps = 2 * event.GetMagnification();
   }
   else
#endif
   {
      steps = event.m_wheelRotation /
         (event.m_wheelDelta > 0 ? (double)event.m_wheelDelta : 120.0);
   }

   if(event.GetWheelAxis() == wxMOUSE_WHEEL_HORIZONTAL) {
      // Two-fingered horizontal swipe on mac is treated like shift-mousewheel
      event.SetShiftDown(true);
      // This makes the wave move in the same direction as the fingers, and the scrollbar
      // thumb moves oppositely
      steps *= -1;
   }

   if(!event.HasAnyModifiers()) {
      // We will later un-skip if we do anything, but if we don't,
      // propagate the event up for the sake of the scrubber
      event.Skip();
      event.ResumePropagation(wxEVENT_PROPAGATE_MAX);
   }

   if (GetTracks()->IsEmpty())
      // Scrolling and Zoom in and out commands are disabled when there are no tracks.
      // This should be disabled too for consistency.  Otherwise
      // you do see changes in the time ruler.
      return;

   // Special case of pointer in the vertical ruler
   if (event.ShiftDown() || event.CmdDown()) {
      wxRect rect;
      Track *const pTrack = FindTrack(event.m_x, event.m_y, true, false, &rect);
      if (pTrack && event.m_x >= GetVRulerOffset()) {
         HandleWheelRotationInVRuler(event, steps, pTrack, rect);
         // Always stop propagation even if the ruler didn't change.  The ruler
         // is a narrow enough target.
         event.Skip(false);
         return;
      }
   }

   if (event.ShiftDown()
       // Don't pan during smooth scrolling.  That would conflict with keeping
       // the play indicator centered.
       && !GetProject()->GetScrubber().IsScrollScrubbing())
   {
      // MM: Scroll left/right when used with Shift key down
      mListener->TP_ScrollWindow(
         mViewInfo->OffsetTimeByPixels(
            mViewInfo->PositionToTime(0), 50.0 * -steps));
   }
   else if (event.CmdDown())
   {
#if 0
         // JKC: Alternative scroll wheel zooming code
         // using AudacityProject zooming, which is smarter,
         // it keeps selections on screen and centred if it can,
         // also this ensures mousewheel and zoom buttons give same result.
         double ZoomFactor = pow(2.0, steps);
         AudacityProject *p = GetProject();
         if( steps > 0 )
            p->ZoomInByFactor( ZoomFactor );
         else
            p->ZoomOutByFactor( ZoomFactor );
#endif
      // MM: Zoom in/out when used with Control key down
      // We're converting pixel positions to times,
      // counting pixels from the left edge of the track.
      int trackLeftEdge = GetLeftOffset();

      // Time corresponding to mouse position
      wxCoord xx;
      double center_h;
      if (GetProject()->GetScrubber().IsScrollScrubbing()) {
         // Expand or contract about the center, ignoring mouse position
         center_h = mViewInfo->h + (GetScreenEndTime() - mViewInfo->h) / 2.0;
         xx = mViewInfo->TimeToPosition(center_h, trackLeftEdge);
      }
      else {
         xx = event.m_x;
         center_h = mViewInfo->PositionToTime(xx, trackLeftEdge);
      }
      // Time corresponding to last (most far right) audio.
      double audioEndTime = mTracks->GetEndTime();

      // When zooming in in empty space, it's easy to 'lose' the waveform.
      // This prevents it.
      // IF zooming in
      if (steps > 0)
      {
         // IF mouse is to right of audio
         if (center_h > audioEndTime)
            // Zooming brings far right of audio to mouse.
            center_h = audioEndTime;
      }

      mViewInfo->ZoomBy(pow(2.0, steps));

      double new_center_h = mViewInfo->PositionToTime(xx, trackLeftEdge);
      mViewInfo->h += (center_h - new_center_h);

      MakeParentRedrawScrollbars();
      Refresh(false);
   }
   else
   {
#ifdef EXPERIMENTAL_SCRUBBING_SCROLL_WHEEL
      if (GetProject()->GetScrubber().IsScrubbing()) {
         GetProject()->GetScrubber().HandleScrollWheel(steps);
         event.Skip(false);
      }
      else
#endif
      {
         // MM: Scroll up/down when used without modifier keys
         double lines = steps * 4 + mVertScrollRemainder;
         mVertScrollRemainder = lines - floor(lines);
         lines = floor(lines);
         const bool didSomething = mListener->TP_ScrollUpDown((int)-lines);
         event.Skip(!didSomething);
      }
   }
}

void TrackPanel::HandleWheelRotationInVRuler
   (wxMouseEvent &event, double steps, Track *pTrack, const wxRect &rect)
{
   if (pTrack->GetKind() == Track::Wave) {
      WaveTrack *const wt = static_cast<WaveTrack*>(pTrack);
      // Assume linked track is wave or null
      const auto partner = static_cast<WaveTrack*>(wt->GetLink());
      const bool isDB =
         wt->GetDisplay() == WaveTrack::Waveform &&
         wt->GetWaveformSettings().scaleType == WaveformSettings::stLogarithmic;

      // Special cases for Waveform dB only.
      // Set the bottom of the dB scale but only if it's visible
      if (isDB && event.ShiftDown() && event.CmdDown()) {
         float min, max;
         wt->GetDisplayBounds(&min, &max);
         if (min < 0.0 && max > 0.0) {
            WaveformSettings &settings = wt->GetIndependentWaveformSettings();
            float olddBRange = settings.dBRange;
            if (steps < 0)
               // Zoom out
               settings.NextLowerDBRange();
            else
               settings.NextHigherDBRange();
            float newdBRange = settings.dBRange;

            if (partner) {
               WaveformSettings &settings = partner->GetIndependentWaveformSettings();
               if (steps < 0)
                  // Zoom out
                  settings.NextLowerDBRange();
               else
                  settings.NextHigherDBRange();
            }
            const float extreme = (LINEAR_TO_DB(2) + newdBRange) / newdBRange;
            max = std::min(extreme, max * olddBRange / newdBRange);
            min = std::max(-extreme, min * olddBRange / newdBRange);
            wt->SetLastdBRange();
            wt->SetDisplayBounds(min, max);
            if (partner) {
               partner->SetLastdBRange();
               partner->SetDisplayBounds(min, max);
            }
         }
      }
      else if (event.CmdDown() && !event.ShiftDown()) {
         HandleWaveTrackVZoom(
            GetTracks(), rect, event.m_y, event.m_y,
            wt, false, (steps < 0),
            true);
      }
      else if (!event.CmdDown() && event.ShiftDown()) {
         // Scroll some fixed number of pixels, independent of zoom level or track height:
         static const float movement = 10.0f;
         const int height = wt->GetHeight() - (kTopMargin + kBottomMargin);
         const bool spectral = (wt->GetDisplay() == WaveTrack::Spectrum);
         if (spectral) {
            const float delta = steps * movement / height;
            SpectrogramSettings &settings = wt->GetIndependentSpectrogramSettings();
            const bool isLinear = settings.scaleType == SpectrogramSettings::stLinear;
            float bottom, top;
            wt->GetSpectrumBounds(&bottom, &top);
            const double rate = wt->GetRate();
            const float bound = rate / 2;
            const NumberScale numberScale(settings.GetScale(bottom, top, rate, false));
            float newTop =
               std::min(bound, numberScale.PositionToValue(1.0f + delta));
            const float newBottom =
               std::max((isLinear ? 0.0f : 1.0f),
                        numberScale.PositionToValue(numberScale.ValueToPosition(newTop) - 1.0f));
            newTop =
               std::min(bound,
                        numberScale.PositionToValue(numberScale.ValueToPosition(newBottom) + 1.0f));

            wt->SetSpectrumBounds(newBottom, newTop);
            if (partner)
               partner->SetSpectrumBounds(newBottom, newTop);
         }
         else {
            float topLimit = 2.0;
            if (isDB) {
               const float dBRange = wt->GetWaveformSettings().dBRange;
               topLimit = (LINEAR_TO_DB(topLimit) + dBRange) / dBRange;
            }
            const float bottomLimit = -topLimit;
            float top, bottom;
            wt->GetDisplayBounds(&bottom, &top);
            const float range = top - bottom;
            const float delta = range * steps * movement / height;
            float newTop = std::min(topLimit, top + delta);
            const float newBottom = std::max(bottomLimit, newTop - range);
            newTop = std::min(topLimit, newBottom + range);
            wt->SetDisplayBounds(newBottom, newTop);
            if (partner)
               partner->SetDisplayBounds(newBottom, newTop);
         }
      }
      else
         return;

      UpdateVRuler(pTrack);
      Refresh(false);
      MakeParentModifyState(true);
   }
   else {
      // To do: time track?  Note track?
   }
   return;
}

/// Filter captured keys typed into LabelTracks.
void TrackPanel::OnCaptureKey(wxCommandEvent & event)
{
   HandleInterruptedDrag();

   // Only deal with LabelTracks
   Track *t = GetFocusedTrack();
   if (!t || t->GetKind() != Track::Label) {
      event.Skip();
      return;
   }
   wxKeyEvent *kevent = (wxKeyEvent *)event.GetEventObject();

   event.Skip(!((LabelTrack *)t)->CaptureKey(*kevent));
}

void TrackPanel::OnKeyDown(wxKeyEvent & event)
{
   switch (event.GetKeyCode())
   {
   case WXK_ESCAPE:
      if(HandleEscapeKey(true))
         // Don't skip the event, eat it so that
         // AudacityApp does not also stop any playback.
         return;
      else
         break;

   case WXK_ALT:
      HandleAltKey(true);
      break;

   case WXK_SHIFT:
      HandleShiftKey(true);
      break;

   case WXK_CONTROL:
      HandleControlKey(true);
      break;

   // Allow PageUp and PageDown keys to
   //scroll the Track Panel left and right
   case WXK_PAGEUP:
      HandlePageUpKey();
      return;

   case WXK_PAGEDOWN:
      HandlePageDownKey();
      return;
   }

   Track *t = GetFocusedTrack();

   // Only deal with LabelTracks
   if (!t || t->GetKind() != Track::Label) {
      event.Skip();
      return;
   }

   LabelTrack *lt = (LabelTrack *)t;
   double bkpSel0 = mViewInfo->selectedRegion.t0(),
      bkpSel1 = mViewInfo->selectedRegion.t1();

   // Pass keystroke to labeltrack's handler and add to history if any
   // updates were done
   if (lt->OnKeyDown(mViewInfo->selectedRegion, event))
      MakeParentPushState(_("Modified Label"),
                          _("Label Edit"),
                          UndoPush::CONSOLIDATE);

   // Make sure caret is in view
   int x;
   if (lt->CalcCursorX(&x)) {
      ScrollIntoView(x);
   }

   // If selection modified, refresh
   // Otherwise, refresh track display if the keystroke was handled
   if( bkpSel0 != mViewInfo->selectedRegion.t0() ||
       bkpSel1 != mViewInfo->selectedRegion.t1() )
      Refresh( false );
   else if (!event.GetSkipped())
      RefreshTrack(t);
}

void TrackPanel::OnChar(wxKeyEvent & event)
{
   switch (event.GetKeyCode())
   {
   case WXK_ESCAPE:
   case WXK_ALT:
   case WXK_SHIFT:
   case WXK_CONTROL:
   case WXK_PAGEUP:
   case WXK_PAGEDOWN:
      return;
   }

   // Only deal with LabelTracks
   Track *t = GetFocusedTrack();
   if (!t || t->GetKind() != Track::Label) {
      event.Skip();
      return;
   }

   double bkpSel0 = mViewInfo->selectedRegion.t0(),
      bkpSel1 = mViewInfo->selectedRegion.t1();
   // Pass keystroke to labeltrack's handler and add to history if any
   // updates were done
   if (((LabelTrack *)t)->OnChar(mViewInfo->selectedRegion, event))
      MakeParentPushState(_("Modified Label"),
                          _("Label Edit"),
                          UndoPush::CONSOLIDATE);

   // If selection modified, refresh
   // Otherwise, refresh track display if the keystroke was handled
   if( bkpSel0 != mViewInfo->selectedRegion.t0() ||
       bkpSel1 != mViewInfo->selectedRegion.t1() )
      Refresh( false );
   else if (!event.GetSkipped())
      RefreshTrack(t);
}

void TrackPanel::OnKeyUp(wxKeyEvent & event)
{
   bool didSomething = false;
   switch (event.GetKeyCode())
   {
   case WXK_ESCAPE:
      didSomething = HandleEscapeKey(false);
      break;
   case WXK_ALT:
      HandleAltKey(false);
      break;

   case WXK_SHIFT:
      HandleShiftKey(false);
      break;

   case WXK_CONTROL:
      HandleControlKey(false);
      break;
   }

   if(!didSomething)
      event.Skip();
}

/// Should handle the case when the mouse capture is lost.
void TrackPanel::OnCaptureLost(wxMouseCaptureLostEvent & WXUNUSED(event))
{
   wxMouseEvent e(wxEVT_LEFT_UP);

   e.m_x = mMouseMostRecentX;
   e.m_y = mMouseMostRecentY;

   OnMouseEvent(e);
}

/// This handles just generic mouse events.  Then, based
/// on our current state, we forward the mouse events to
/// various interested parties.
void TrackPanel::OnMouseEvent(wxMouseEvent & event)
{
#if defined(__WXMAC__) && defined(EVT_MAGNIFY)
   // PRL:
   // Pinch and spread implemented in wxWidgets 3.1.0, or cherry-picked from
   // the future in custom build of 3.0.2
   if (event.Magnify()) {
      HandleWheelRotation(event);
   }
#endif

   if (event.m_wheelRotation != 0)
      HandleWheelRotation(event);

   if (event.LeftDown() || event.LeftIsDown() || event.Moving()) {
      // Skip, even if we do something, so that the left click or drag
      // may have an additional effect in the scrubber.
      event.Skip();
      event.ResumePropagation(wxEVENT_PROPAGATE_MAX);
   }

   if (!mAutoScrolling) {
      mMouseMostRecentX = event.m_x;
      mMouseMostRecentY = event.m_y;
   }

   if (event.LeftDown()) {
      mCapturedTrack = NULL;

      // The activate event is used to make the
      // parent window 'come alive' if it didn't have focus.
      wxActivateEvent e;
      GetParent()->GetEventHandler()->ProcessEvent(e);

      // wxTimers seem to be a little unreliable, so this
      // "primes" it to make sure it keeps going for a while...

      // When this timer fires, we call TrackPanel::OnTimer and
      // possibly update the screen for offscreen scrolling.
      mTimer.Stop();
      mTimer.Start(kTimerInterval, FALSE);
   }

   if (event.ButtonDown()) {
      SetFocus();
   }
   if (event.ButtonUp()) {
      if (HasCapture())
         ReleaseMouse();
   }

   if (event.Leaving())
   {

      // PRL:  was this test really needed?  It interfered with my refactoring
      // that tried to eliminate those enum values.
      // I think it was never true, that mouse capture was pan or gain sliding,
      // but no mouse button was down.
      // if (mMouseCapture != IsPanSliding && mMouseCapture != IsGainSliding)

      auto buttons =
         // Bug 1325: button state in Leaving events is unreliable on Mac.
         // Poll the global state instead.
         // event.ButtonIsDown(wxMOUSE_BTN_ANY);
         ::wxGetMouseState().ButtonIsDown(wxMOUSE_BTN_ANY);

      if(!buttons) {
         SetCapturedTrack(NULL);

#if defined(__WXMAC__)

         // We must install the cursor ourselves since the window under
         // the mouse is no longer this one and wx2.8.12 makes that check.
         // Should re-evaluate with wx3.
         wxSTANDARD_CURSOR->MacInstall();
#endif
      }
   }

   switch( mMouseCapture )
   {
   case IsVZooming:
      HandleVZoom(event);
      break;
   case IsClosing:
      HandleClosing(event);
      break;
   case IsPopping:
      HandlePopping(event);
      break;
   case IsMuting:
      HandleMutingSoloing(event, false);
      break;
   case IsSoloing:
      HandleMutingSoloing(event, true);
      break;
   case IsResizing:
   case IsResizingBetweenLinkedTracks:
   case IsResizingBelowLinkedTracks:
      HandleResize(event);
      HandleCursor(event);
      break;
   case IsRearranging:
      HandleRearrange(event);
      break;
   case IsGainSliding:
      HandleSliders(event, false);
      break;
   case IsPanSliding:
      HandleSliders(event, true);
      break;
   case IsMinimizing:
      HandleMinimizing(event);
      break;
   case IsZooming:
      HandleZoom(event);
      break;
   case IsAdjustingLabel:
      // Reach this case only when the captured track was label
      HandleGlyphDragRelease(static_cast<LabelTrack *>(mCapturedTrack), event);
      break;
   case IsSelectingLabelText:
      // Reach this case only when the captured track was label
      HandleTextDragRelease(static_cast<LabelTrack *>(mCapturedTrack), event);
      break;
   default: //includes case of IsUncaptured
      // This is where most button-downs are detected
      HandleTrackSpecificMouseEvent(event);
      break;
   }

   if (event.ButtonDown() && IsMouseCaptured()) {
      if (!HasCapture())
         CaptureMouse();
   }

   //EnsureVisible should be called after the up-click.
   if (event.ButtonUp()) {
      wxRect rect;

      Track *t = FindTrack(event.m_x, event.m_y, false, false, &rect);
      if (t)
         EnsureVisible(t);
   }
}

namespace {
   int FindMergeLine(WaveTrack *track, double time)
   {
      const double tolerance = 0.5 / track->GetRate();
      int ii = 0;
      for (const auto loc: track->GetCachedLocations()) {
         if (loc.typ == WaveTrackLocation::locationMergePoint &&
            fabs(time - loc.pos) < tolerance)
            return ii;
         ++ii;
      }
      return -1;
   }
}

bool TrackPanel::HandleTrackLocationMouseEvent(WaveTrack * track, wxRect &rect, wxMouseEvent &event)
{
   // FIXME: Disable this and return true when CutLines aren't showing?
   // (Don't use gPrefs-> for the fix as registry access is slow).

   if (mMouseCapture == WasOverCutLine)
   {
      if (event.ButtonUp()) {
         mMouseCapture = IsUncaptured;
         return false;
      }
      else
         // Needed to avoid select events after button down
         return true;
   }
   else if (!IsUnsafe() && IsOverCutline(track, rect, event))
   {
      if (!mCapturedTrackLocationRect.Contains(event.m_x, event.m_y))
      {
         SetCapturedTrack( NULL );
         return false;
      }

      bool handled = false;

      if (event.LeftDown())
      {
         if (mCapturedTrackLocation.typ == WaveTrackLocation::locationCutLine)
         {
            // When user presses left button on cut line, expand the line again
            double cutlineStart = 0, cutlineEnd = 0;

            if (track->ExpandCutLine(mCapturedTrackLocation.pos, &cutlineStart, &cutlineEnd))
            {
               // Assume linked track is wave or null
               const auto linked =
                  static_cast<WaveTrack*>(mTracks->GetLink(track));
               if (linked) {
                  // Expand the cutline in the opposite channel if it is present.

                  // PRL:  Do NOT report that the event is not handled if the other
                  // channel doesn't also have a cutline to expand at the same time.
                  // Just ignore the return.  Bug1310.

                  /* bool success = */
                     linked->ExpandCutLine(mCapturedTrackLocation.pos);
                  /*
                  if (!success)
                     return false;
                  */
               }

               mViewInfo->selectedRegion.setTimes(cutlineStart, cutlineEnd);
               DisplaySelection();
               MakeParentPushState(_("Expanded Cut Line"), _("Expand"));
               handled = true;
            }
         }
         else if (mCapturedTrackLocation.typ == WaveTrackLocation::locationMergePoint) {
            const double pos = mCapturedTrackLocation.pos;
            if (!track->MergeClips(mCapturedTrackLocation.clipidx1, mCapturedTrackLocation.clipidx2))
               return false;

            // Assume linked track is wave or null
            const auto linked =
               static_cast<WaveTrack*>(mTracks->GetLink(track));
            if (linked) {
               // Don't assume correspondence of merge points across channels!
               int idx = FindMergeLine(linked, pos);
               if (idx >= 0) {
                  WaveTrack::Location location = linked->GetCachedLocations()[idx];
                  if (!linked->MergeClips(location.clipidx1, location.clipidx2))
                     return false;
               }
            }

            MakeParentPushState(_("Merged Clips"),_("Merge"), UndoPush::CONSOLIDATE);
            handled = true;
         }
      }

      if (!handled && event.RightDown())
      {
         track->RemoveCutLine(mCapturedTrackLocation.pos);
         // Assume linked track is wave or null
         const auto linked =
            static_cast<WaveTrack*>(mTracks->GetLink(track));
         if (linked)
            linked->RemoveCutLine(mCapturedTrackLocation.pos);
         MakeParentPushState(_("Removed Cut Line"), _("Remove") );
         handled = true;
      }

      if (handled)
      {
         SetCapturedTrack( NULL );
         // Effect happened at button-down, but treat like a dragging mode until
         // button-up.
         mMouseCapture = WasOverCutLine;
         RefreshTrack(track);
         return true;
      }


      return true;
   }

   if( event.LeftDown() ){
      bool bShift = event.ShiftDown();
      bool bCtrlDown = event.ControlDown();
      bool unsafe = IsUnsafe();
      bCtrlDown = false;
      if( /*bShift ||*/ bCtrlDown ){

         HandleListSelection(track, bShift, bCtrlDown, !unsafe);
         return true;
      }
   }
   return false;
}

bool TrackPanel::IsOverCutline(WaveTrack * track, wxRect &rect, const wxMouseEvent &event)
{
   for (auto loc: track->GetCachedLocations())
   {
      const double x = mViewInfo->TimeToPosition(loc.pos);
      if (x >= 0 && x < rect.width)
      {
         wxRect locRect;
         locRect.x = (int)(rect.x + x) - 5;
         locRect.width = 11;
         locRect.y = rect.y;
         locRect.height = rect.height;
         if (locRect.Contains(event.m_x, event.m_y))
         {
            mCapturedTrackLocation = loc;
            mCapturedTrackLocationRect = locRect;
            return true;
         }
      }
   }

   return false;
}


/// Event has happened on a track and it has been determined to be a label track.
bool TrackPanel::HandleLabelTrackClick(LabelTrack * lTrack, wxRect &rect, wxMouseEvent & event)
{
   if (!event.ButtonDown())
      return false;

   if(event.LeftDown())
   {
      /// \todo This method is one of a large number of methods in
      /// TrackPanel which suitably modified belong in other classes.
      TrackListIterator iter(GetTracks());
      Track *n = iter.First();

      while (n) {
         if (n->GetKind() == Track::Label && lTrack != n) {
            ((LabelTrack *)n)->ResetFlags();
            ((LabelTrack *)n)->Unselect();
         }
         n = iter.Next();
      }
   }

   mCapturedRect = rect;
   mCapturedRect.width -= kRightMargin;

   lTrack->HandleClick(event, mCapturedRect, *mViewInfo, &mViewInfo->selectedRegion);

   if (lTrack->IsAdjustingLabel())
   {
      SetCapturedTrack(lTrack, IsAdjustingLabel);

      //If we are adjusting a label on a labeltrack, do not do anything
      //that follows. Instead, redraw the track.
      RefreshTrack(lTrack);
      return true;
   }

   if( event.LeftDown() ){
      bool bShift = event.ShiftDown();
      bool bCtrlDown = event.ControlDown();
      bool unsafe = IsUnsafe();

      if( /*bShift ||*/ bCtrlDown ){

         HandleListSelection(lTrack, bShift, bCtrlDown, !unsafe);
         return true;
      }
   }


   // IF the user clicked a label, THEN select all other tracks by Label
   if (lTrack->IsSelected()) {
      SelectTracksByLabel(lTrack);
      // Do this after, for the effect on mLastPickedTrack:
      SelectTrack(lTrack, true);
      DisplaySelection();

      // Not starting a drag
      SetCapturedTrack(NULL, IsUncaptured);

      if(mCapturedTrack == NULL)
         SetCapturedTrack(lTrack, IsSelectingLabelText);

      RefreshTrack(lTrack);
      return true;
   }

   // handle shift+ctrl down
   /*if (event.ShiftDown()) { // && event.ControlDown()) {
      lTrack->SetHighlightedByKey(true);
      Refresh(false);
      return;
   }*/




   // return false, there is more to do...
   return false;
}

/// Event has happened on a track and it has been determined to be a label track.
void TrackPanel::HandleGlyphDragRelease(LabelTrack * lTrack, wxMouseEvent & event)
{
   if (!lTrack)
      return;

   /// \todo This method is one of a large number of methods in
   /// TrackPanel which suitably modified belong in other classes.
   if (event.Dragging()) {
      ;
   }
   else if (event.LeftUp())
      SetCapturedTrack(NULL);

   if (lTrack->HandleGlyphDragRelease(event, mCapturedRect,
      *mViewInfo, &mViewInfo->selectedRegion)) {
      MakeParentPushState(_("Modified Label"),
         _("Label Edit"),
         UndoPush::CONSOLIDATE);
   }

   // Update cursor on the screen if it is a point.
   DrawOverlays(false);
   mRuler->DrawOverlays(false);

   //If we are adjusting a label on a labeltrack, do not do anything
   //that follows. Instead, redraw the track.
   RefreshTrack(lTrack);
   return;
}

/// Event has happened on a track and it has been determined to be a label track.
void TrackPanel::HandleTextDragRelease(LabelTrack * lTrack, wxMouseEvent & event)
{
   if (!lTrack)
      return;

   lTrack->HandleTextDragRelease(event);

   /// \todo This method is one of a large number of methods in
   /// TrackPanel which suitably modified belong in other classes.
   if (event.Dragging()) {
      ;
   }
   else if (event.ButtonUp())
      SetCapturedTrack(NULL);

   // handle dragging
   if (event.Dragging()) {
      // locate the initial mouse position
      if (event.LeftIsDown()) {
         if (mLabelTrackStartXPos == -1) {
            mLabelTrackStartXPos = event.m_x;
            mLabelTrackStartYPos = event.m_y;

            if ((lTrack->getSelectedIndex() != -1) &&
               lTrack->OverTextBox(
               lTrack->GetLabel(lTrack->getSelectedIndex()),
               mLabelTrackStartXPos,
               mLabelTrackStartYPos))
            {
               mLabelTrackStartYPos = -1;
            }
         }
         // if initial mouse position in the text box
         // then only drag text
         if (mLabelTrackStartYPos == -1) {
            RefreshTrack(lTrack);
            return;
         }
      }
   }

   // handle mouse left button up
   if (event.LeftUp()) {
      mLabelTrackStartXPos = -1;
   }
}

// AS: I don't really understand why this code is sectioned off
//  from the other OnMouseEvent code.
void TrackPanel::HandleTrackSpecificMouseEvent(wxMouseEvent & event)
{
   Track * pTrack;
   Track * pControlTrack;
   wxRect rTrack;
   wxRect rLabel;

   bool unsafe = IsUnsafe();

   pControlTrack = FindTrack(event.m_x, event.m_y, true, true, &rLabel);
   pTrack = FindTrack(event.m_x, event.m_y, false, false, &rTrack);

   //call HandleResize if I'm over the border area
   if (event.LeftDown() &&
          (within(event.m_y, rTrack.y + rTrack.height, TRACK_RESIZE_REGION)
        || within(event.m_y, rLabel.y + rLabel.height,
                  TRACK_RESIZE_REGION))) {
      HandleResize(event);
      HandleCursor(event);
      return;
   }

   // AS: If the user clicked outside all tracks, make nothing
   //  selected.
   if ((event.ButtonDown() || event.ButtonDClick()) &&
       !(pTrack || pControlTrack)) {
      SelectNone();
      Refresh(false);
      return;
   }

   //Determine if user clicked on the track's left-hand label
   if (!mCapturedTrack && event.m_x < GetLeftOffset()) {
      if (event.m_x >= GetVRulerOffset()) {
         if( !event.Dragging() ) // JKC: Only want the mouse down event.
            HandleVZoom(event);
         HandleCursor(event);
      }
      else {
         HandleLabelClick(event);
         HandleCursor(event);
      }
      return;
   }

   //Determine if user clicked on a label track.
   //If so, use MouseDown handler for the label track.
   if (pTrack && (pTrack->GetKind() == Track::Label))
   {
      if (HandleLabelTrackClick((LabelTrack *)pTrack, rTrack, event))
         return;
   }

   bool handled = false;

   if (pTrack && (pTrack->GetKind() == Track::Wave) &&
      (mMouseCapture == IsUncaptured || mMouseCapture == WasOverCutLine))
      handled = HandleTrackLocationMouseEvent((WaveTrack *)pTrack, rTrack, event);

   ToolsToolBar * pTtb = mListener->TP_GetToolsToolBar();
   if( !handled && pTtb != NULL )
   {
      int toolToUse = DetermineToolToUse(pTtb, event);

      switch (toolToUse) {
      case selectTool:
         HandleSelect(event);
         break;
      case envelopeTool:
         if (!unsafe)
            HandleEnvelope(event);
         break;
      case slideTool:
         if (!unsafe)
            HandleSlide(event);
         break;
      case zoomTool:
         HandleZoom(event);
         break;
      case drawTool:
         if (!unsafe)
            HandleSampleEditing(event);
         break;
      }
   }

   if ((event.Moving() || event.LeftUp())  &&
       (mMouseCapture == IsUncaptured ))
//       (mMouseCapture != IsSelecting ) &&
//       (mMouseCapture != IsEnveloping) &&
//       (mMouseCapture != IsSliding) )
   {
      HandleCursor(event);
   }
   if (event.LeftUp()) {
      mCapturedTrack = NULL;
   }
}

/// If we are in multimode, looks at the type of track and where we are on it to
/// determine what object we are hovering over and hence what tool to use.
/// @param pTtb - A pointer to the tools tool bar
/// @param event - Mouse event, with info about position and what mouse buttons are down.
int TrackPanel::DetermineToolToUse( ToolsToolBar * pTtb, const wxMouseEvent & event)
{
   int currentTool = pTtb->GetCurrentTool();

   // Unless in Multimode keep using the current tool.
   if( !pTtb->IsDown(multiTool) )
      return currentTool;

   // We NEVER change tools whilst we are dragging.
   if( event.Dragging() || event.LeftUp() )
      return currentTool;

   // Just like dragging.
   // But, this event might be the final button up
   // so keep the same tool.
//   if( mIsSliding || mIsSelecting || mIsEnveloping )
   if( mMouseCapture != IsUncaptured )
      return currentTool;

   // So now we have to find out what we are near to..
   wxRect rect;

   Track *pTrack = FindTrack(event.m_x, event.m_y, false, false, &rect);
   if( !pTrack )
      return currentTool;

   int trackKind = pTrack->GetKind();
   currentTool = selectTool; // the default.

   if (event.ButtonIsDown(wxMOUSE_BTN_RIGHT) || event.RightUp()){
      currentTool = zoomTool;
   }
   else if (trackKind == Track::Time){
      currentTool = envelopeTool;
   } else if( trackKind == Track::Label ){
      currentTool = selectTool;
   } else if( trackKind != Track::Wave) {
      currentTool = selectTool;
   // So we are in a wave track?  Not necessarily. 
   // FIXME: Possibly not in wave track. Haven't checked Track::Note (#if defined(USE_MIDI)).
   // From here on the order in which we hit test determines
   // which tool takes priority in the rare cases where it
   // could be more than one.
   } else if (event.CmdDown()){
      // msmeyer: If control is down, slide single clip
      // msmeyer: If control and shift are down, slide all clips
      currentTool = slideTool;
   } else if( HitTestEnvelope( pTrack, rect, event ) ){
      currentTool = envelopeTool;
   } else if( HitTestSlide( pTrack, rect, event )){
      currentTool = slideTool;
   } else if( HitTestSamples( pTrack, rect, event )){
      currentTool = drawTool;
   }

   //Use the false argument since in multimode we don't
   //want the button indicating which tool is in use to be updated.
   pTtb->SetCurrentTool( currentTool, false );
   return currentTool;
}


#ifdef USE_MIDI
bool TrackPanel::HitTestStretch(Track *track, wxRect &rect, const wxMouseEvent & event)
{
   // later, we may want a different policy, but for now, stretch is
   // selected when the cursor is near the center of the track and
   // within the selection
   if (!track || !track->GetSelected() || track->GetKind() != Track::Note ||
       IsUnsafe()) {
      return false;
   }
   int center = rect.y + rect.height / 2;
   int distance = abs(event.m_y - center);
   const int yTolerance = 10;
   wxInt64 leftSel = mViewInfo->TimeToPosition(mViewInfo->selectedRegion.t0(), rect.x);
   wxInt64 rightSel = mViewInfo->TimeToPosition(mViewInfo->selectedRegion.t1(), rect.x);
   // Something is wrong if right edge comes before left edge
   wxASSERT(!(rightSel < leftSel));
   return (leftSel <= event.m_x && event.m_x <= rightSel &&
           distance < yTolerance);
}
#endif


/// method that tells us if the mouse event landed on an
/// envelope boundary.
bool TrackPanel::HitTestEnvelope(Track *track, wxRect &rect, const wxMouseEvent & event)
{
   wxASSERT(track);
   if( track->GetKind() != Track::Wave )
      return false;
   WaveTrack *wavetrack = (WaveTrack *)track;
   Envelope *envelope = wavetrack->GetEnvelopeAtX(event.GetX());

   if (!envelope)
      return false;

   const int displayType = wavetrack->GetDisplay();
   // Not an envelope hit, unless we're using a type of wavetrack display
   // suitable for envelopes operations, ie one of the Wave displays.
   if ( displayType != WaveTrack::Waveform)
      return false;  // No envelope, not a hit, so return.

   // Get envelope point, range 0.0 to 1.0
   const bool dB = !wavetrack->GetWaveformSettings().isLinear();
   // Convert x to time.
   const double envValue = envelope->GetValue(mViewInfo->PositionToTime(event.m_x, rect.x));

   float zoomMin, zoomMax;
   wavetrack->GetDisplayBounds(&zoomMin, &zoomMax);

   const double dBRange = wavetrack->GetWaveformSettings().dBRange;

   // Get y position of envelope point.
   int yValue = GetWaveYPos( envValue,
      zoomMin, zoomMax,
      rect.height, dB, true, dBRange, false) + rect.y;

   // Get y position of center line
   int ctr = GetWaveYPos( 0.0,
      zoomMin, zoomMax,
      rect.height, dB, true, dBRange, false) + rect.y;

   // Get y distance of mouse from center line (in pixels).
   int yMouse = abs(ctr - event.m_y);
   // Get y distance of envelope from center line (in pixels)
   yValue = abs(ctr-yValue);

   // JKC: It happens that the envelope is actually drawn offset from its
   // 'true' position (it is 3 pixels wide).  yMisalign is really a fudge
   // factor to allow us to hit it exactly, but I wouldn't dream of
   // calling it yFudgeFactor :)
   const int yMisalign = 2;
   // Perhaps yTolerance should be put into preferences?
   const int yTolerance = 5; // how far from envelope we may be and count as a hit.
   int distance;

   // For amplification using the envelope we introduced the idea of contours.
   // The contours have the same shape as the envelope, which may be partially off-screen.
   // The contours are closer in to the center line.
   int ContourSpacing = (int) (rect.height / (2* (zoomMax-zoomMin)));
   const int MaxContours = 2;

   // Adding ContourSpacing/2 selects a region either side of the contour.
   int yDisplace = yValue - yMisalign - yMouse  + ContourSpacing/2;
   if (yDisplace > (MaxContours * ContourSpacing))
      return false;
   // Subtracting the ContourSpacing/2 we added earlier ensures distance is centred on the contour.
   distance = abs( ( yDisplace % ContourSpacing ) - ContourSpacing/2);
   return( distance < yTolerance );
}

/// method that tells us if the mouse event landed on an
/// editable sample
bool TrackPanel::HitTestSamples(Track *track, wxRect &rect, const wxMouseEvent & event)
{
   wxASSERT(track);
   if( track->GetKind() != Track::Wave )
      return false;

   WaveTrack *wavetrack = (WaveTrack *)track;
   //Get rate in order to calculate the critical zoom threshold
   double rate = wavetrack->GetRate();
   const double dBRange = wavetrack->GetWaveformSettings().dBRange;

   const int displayType = wavetrack->GetDisplay();
   if (WaveTrack::Waveform != displayType)
      return false;  // Not a wave, so return.
   const bool dB = !wavetrack->GetWaveformSettings().isLinear();

   const double tt = mViewInfo->PositionToTime(event.m_x, rect.x);
   int width;
   GetTracksUsableArea(&width, NULL);
   if (!SampleResolutionTest(*mViewInfo, wavetrack, tt, rate, width))
      return false;

   // Just get one sample.
   float oneSample;
   auto s0 = (sampleCount)(tt * rate + 0.5);
   wavetrack->Get((samplePtr)&oneSample, floatSample, s0, 1);

   // Get y distance of envelope point from center line (in pixels).
   float zoomMin, zoomMax;

   wavetrack->GetDisplayBounds(&zoomMin, &zoomMax);

   double envValue = 1.0;
   Envelope* env = wavetrack->GetEnvelopeAtX(event.GetX());
   if (env)
      envValue = env->GetValue(tt);

   int yValue = GetWaveYPos( oneSample * envValue,
      zoomMin, zoomMax,
      rect.height, dB, true, dBRange, false) + rect.y;

   // Get y position of mouse (in pixels)
   int yMouse = event.m_y;

   // Perhaps yTolerance should be put into preferences?
   const int yTolerance = 10; // More tolerance on samples than on envelope.
   return( abs( yValue -  yMouse ) < yTolerance );
}

/// method that tells us if the mouse event landed on a
/// time-slider that allows us to time shift the sequence.
bool TrackPanel::HitTestSlide(Track * WXUNUSED(track), wxRect &rect, const wxMouseEvent & event)
{
   // Perhaps we should delegate this to TrackArtist as only TrackArtist
   // knows what the real sizes are??

   // The drag Handle width includes border, width and a little extra margin.
   const int adjustedDragHandleWidth = 14;
   // The hotspot for the cursor isn't at its centre.  Adjust for this.
   const int hotspotOffset = 5;

   // We are doing an approximate test here - is the mouse in the right or left border?
   if( event.m_x + hotspotOffset < rect.x + adjustedDragHandleWidth)
      return true;

   if( event.m_x + hotspotOffset > rect.x + rect.width - adjustedDragHandleWidth)
      return true;

   return false;
}

double TrackPanel::GetMostRecentXPos()
{
   return mViewInfo->PositionToTime(mMouseMostRecentX, GetLabelWidth());
}

void TrackPanel::RefreshTrack(Track *trk, bool refreshbacking)
{
   Track *link = trk->GetLink();

   if (link && !trk->GetLinked()) {
      trk = link;
      link = trk->GetLink();
   }

   // subtract insets and shadows from the rectangle, but not border
   // This matters because some separators do paint over the border
   wxRect rect(kLeftInset,
            -mViewInfo->vpos + trk->GetY() + kTopInset,
            GetRect().GetWidth() - kLeftInset - kRightInset - kShadowThickness,
            trk->GetHeight() - kTopInset - kShadowThickness);

   if (link) {
      rect.height += link->GetHeight();
   }

#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
   else if(MONO_WAVE_PAN(trk)){
      rect.height += trk->GetHeight(true);
   }
#endif

   if( refreshbacking )
   {
      mRefreshBacking = true;
   }

   Refresh( false, &rect );
}


/// This method overrides Refresh() of wxWindow so that the
/// boolean play indictaor can be set to false, so that an old play indicator that is
/// no longer there won't get  XORed (to erase it), thus redrawing it on the
/// TrackPanel
void TrackPanel::Refresh(bool eraseBackground /* = TRUE */,
                         const wxRect *rect /* = NULL */)
{
   // Tell OnPaint() to refresh the backing bitmap.
   //
   // Originally I had the check within the OnPaint() routine and it
   // was working fine.  That was until I found that, even though a full
   // refresh was requested, Windows only set the onscreen portion of a
   // window as damaged.
   //
   // So, if any part of the trackpanel was off the screen, full refreshes
   // didn't work and the display got corrupted.
   if( !rect || ( *rect == GetRect() ) )
   {
      mRefreshBacking = true;
   }
   wxWindow::Refresh(eraseBackground, rect);
   DisplaySelection();
}

/// Draw the actual track areas.  We only draw the borders
/// and the little buttons and menues and whatnot here, the
/// actual contents of each track are drawn by the TrackArtist.
void TrackPanel::DrawTracks(wxDC * dc)
{
   wxRegion region = GetUpdateRegion();

   wxRect clip = GetRect();

   wxRect panelRect = clip;
   panelRect.y = -mViewInfo->vpos;

   wxRect tracksRect = panelRect;
   tracksRect.x += GetLabelWidth();
   tracksRect.width -= GetLabelWidth();

   ToolsToolBar *pTtb = mListener->TP_GetToolsToolBar();
   bool bMultiToolDown = pTtb->IsDown(multiTool);
   bool envelopeFlag   = pTtb->IsDown(envelopeTool) || bMultiToolDown;
   bool bigPointsFlag  = pTtb->IsDown(drawTool) || bMultiToolDown;
   bool sliderFlag     = bMultiToolDown;

   // The track artist actually draws the stuff inside each track
   mTrackArtist->DrawTracks(GetTracks(), GetProject()->GetFirstVisible(),
                            *dc, region, tracksRect, clip,
                            mViewInfo->selectedRegion, *mViewInfo,
                            envelopeFlag, bigPointsFlag, sliderFlag);

   DrawEverythingElse(dc, region, clip);
}

/// Draws 'Everything else'.  In particular it draws:
///  - Drop shadow for tracks and vertical rulers.
///  - Zooming Indicators.
///  - Fills in space below the tracks.
void TrackPanel::DrawEverythingElse(wxDC * dc,
                                    const wxRegion &region,
                                    const wxRect & clip)
{
   // We draw everything else

   wxRect focusRect(-1, -1, 0, 0);
   wxRect trackRect = clip;
   trackRect.height = 0;   // for drawing background in no tracks case.

   VisibleTrackIterator iter(GetProject());
   for (Track *t = iter.First(); t; t = iter.Next()) {
      trackRect.y = t->GetY() - mViewInfo->vpos;
      trackRect.height = t->GetHeight();

      // If this track is linked to the next one, display a common
      // border for both, otherwise draw a normal border
      wxRect rect = trackRect;
      bool skipBorder = false;
      Track *l = t->GetLink();

      if (t->GetLinked()) {
         rect.height += l->GetHeight();
      }
      else if (l && trackRect.y >= 0) {
         skipBorder = true;
      }

#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
      if(MONO_WAVE_PAN(t)){
         rect.height += t->GetHeight(true);
      }
#endif

      // If the previous track is linked to this one but isn't on the screen
      // (and thus would have been skipped by VisibleTrackIterator) we need to
      // draw that track's border instead.
      Track *borderTrack = t;
      wxRect borderRect = rect, borderTrackRect = trackRect;

      if (l && !t->GetLinked() && trackRect.y < 0)
      {
         borderTrack = l;

         borderTrackRect.y = l->GetY() - mViewInfo->vpos;
         borderTrackRect.height = l->GetHeight();

         borderRect = borderTrackRect;
         borderRect.height += t->GetHeight();
      }

      if (!skipBorder) {
         if (mAx->IsFocused(t)) {
            focusRect = borderRect;
         }
         DrawOutside(borderTrack, dc, borderRect, borderTrackRect);
      }

      // Believe it or not, we can speed up redrawing if we don't
      // redraw the vertical ruler when only the waveform data has
      // changed.  An example is during recording.

#if DEBUG_DRAW_TIMING
//      wxRect rbox = region.GetBox();
//      wxPrintf(wxT("Update Region: %d %d %d %d\n"),
//             rbox.x, rbox.y, rbox.width, rbox.height);
#endif

      if (region.Contains(0, trackRect.y, GetLeftOffset(), trackRect.height)) {
         wxRect rect = trackRect;
         rect.x += GetVRulerOffset();
         rect.y += kTopMargin;
         rect.width = GetVRulerWidth();
         rect.height -= (kTopMargin + kBottomMargin);
         mTrackArtist->DrawVRuler(t, dc, rect);
      }

#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
      if(MONO_WAVE_PAN(t)){
         trackRect.y = t->GetY(true) - mViewInfo->vpos;
         trackRect.height = t->GetHeight(true);
         if (region.Contains(0, trackRect.y, GetLeftOffset(), trackRect.height)) {
            wxRect rect = trackRect;
            rect.x += GetVRulerOffset();
            rect.y += kTopMargin;
            rect.width = GetVRulerWidth();
            rect.height -= (kTopMargin + kBottomMargin);
            mTrackArtist->DrawVRuler(t, dc, rect);
         }
      }
#endif
   }

   if ((mMouseCapture == IsZooming || mMouseCapture == IsVZooming) &&
       IsDragZooming()
       // note track zooming now works like audio track
       //#ifdef USE_MIDI
       //       && mCapturedTrack && mCapturedTrack->GetKind() != Track::Note
       //#endif
       ) {
      DrawZooming(dc, clip);
   }

   // Paint over the part below the tracks
   trackRect.y += trackRect.height;
   if (trackRect.y < clip.GetBottom()) {
      AColor::TrackPanelBackground(dc, false);
      dc->DrawRectangle(trackRect.x,
                        trackRect.y,
                        trackRect.width,
                        clip.height - trackRect.y);
   }

   // Sometimes highlight is not drawn on backing bitmap. I thought
   // it was because FindFocus did not return "this" on Mac, but
   // when I removed that test, yielding this condition:
   //     if (GetFocusedTrack() != NULL) {
   // the highlight was reportedly drawn even when something else
   // was the focus and no highlight should be drawn. -RBD
   if (GetFocusedTrack() != NULL && wxWindow::FindFocus() == this) {
      HighlightFocusedTrack(dc, focusRect);
   }

   // Draw snap guidelines if we have any
   if (mSnapManager && (mSnapLeft >= 0 || mSnapRight >= 0)) {
      AColor::SnapGuidePen(dc);
      if (mSnapLeft >= 0) {
         AColor::Line(*dc, (int)mSnapLeft, 0, mSnapLeft, 30000);
      }
      if (mSnapRight >= 0) {
         AColor::Line(*dc, (int)mSnapRight, 0, mSnapRight, 30000);
      }
   }
}

/// Draw zooming indicator that shows the region that will
/// be zoomed into when the user clicks and drags with a
/// zoom cursor.  Handles both vertical and horizontal
/// zooming.
void TrackPanel::DrawZooming(wxDC * dc, const wxRect & clip)
{
   wxRect rect;

   dc->SetBrush(*wxTRANSPARENT_BRUSH);
   dc->SetPen(*wxBLACK_DASHED_PEN);

   if (mMouseCapture==IsVZooming) {
      rect.y = std::min(mZoomStart, mZoomEnd);
      rect.height = 1 + abs(mZoomEnd - mZoomStart);

      rect.x = GetVRulerOffset();
      rect.SetRight(GetSize().x - kRightMargin); // extends into border rect
   }
   else {
      rect.x = std::min(mZoomStart, mZoomEnd);
      rect.width = 1 + abs(mZoomEnd - mZoomStart);

      rect.y = -1;
      rect.height = clip.height + 2;
   }

   dc->DrawRectangle(rect);
}


void TrackPanel::DrawOutside(Track * t, wxDC * dc, const wxRect & rec,
                             const wxRect & trackRect)
{
   wxRect rect = rec;
   int labelw = GetLabelWidth();
   int vrul = GetVRulerOffset();

   DrawOutsideOfTrack(t, dc, rect);

   rect.x += kLeftInset;
   rect.y += kTopInset;
   rect.width -= kLeftInset * 2;
   rect.height -= kTopInset;

   mTrackInfo.SetTrackInfoFont(dc);
   dc->SetTextForeground(theTheme.Colour(clrTrackPanelText));

   bool bIsWave = (t->GetKind() == Track::Wave);
#ifdef USE_MIDI
   bool bIsNote = (t->GetKind() == Track::Note);
#endif
   // don't enable bHasMuteSolo for Note track because it will draw in the
   // wrong place.
   mTrackInfo.DrawBackground(dc, rect, t->GetSelected(), bIsWave, labelw, vrul);

   // Vaughan, 2010-08-24: No longer doing this.
   // Draw sync-lock tiles in ruler area.
   //if (t->IsSyncLockSelected()) {
   //   wxRect tileFill = rect;
   //   tileFill.x = GetVRulerOffset();
   //   tileFill.width = GetVRulerWidth();
   //   TrackArtist::DrawSyncLockTiles(dc, tileFill);
   //}

   DrawBordersAroundTrack(t, dc, rect, labelw, vrul);
   DrawShadow(t, dc, rect);

   rect.width = mTrackInfo.GetTrackInfoWidth();
   bool captured = (t == mCapturedTrack);
   mTrackInfo.DrawCloseBox(dc, rect, (captured && mMouseCapture==IsClosing));
   mTrackInfo.DrawTitleBar(dc, rect, t, (captured && mMouseCapture==IsPopping));

   mTrackInfo.DrawMinimize(dc, rect, t, (captured && mMouseCapture==IsMinimizing));

   // Draw the sync-lock indicator if this track is in a sync-lock selected group.
   if (t->IsSyncLockSelected())
   {
      wxRect syncLockIconRect;
      mTrackInfo.GetSyncLockIconRect(rect, syncLockIconRect);
      wxBitmap syncLockBitmap(theTheme.Image(bmpSyncLockIcon));
      // Icon is 12x12 and syncLockIconRect is 16x16.
      dc->DrawBitmap(syncLockBitmap,
                     syncLockIconRect.x + 3,
                     syncLockIconRect.y + 2,
                     true);
   }

   mTrackInfo.DrawBordersWithin( dc, rect, bIsWave );

   auto wt = bIsWave ? static_cast<WaveTrack*>(t) : nullptr;
   if (bIsWave) {
      mTrackInfo.DrawMuteSolo(dc, rect, t, (captured && mMouseCapture == IsMuting), false, HasSoloButton());
      mTrackInfo.DrawMuteSolo(dc, rect, t, (captured && mMouseCapture == IsSoloing), true, HasSoloButton());

      mTrackInfo.DrawSliders(dc, (WaveTrack *)t, rect, captured);
      if (!t->GetMinimized()) {

         int offset = 8;
         if (rect.y + 22 + 12 < rec.y + rec.height - 19)
            dc->DrawText(TrackSubText(wt),
                         trackRect.x + offset,
                         trackRect.y + 22);

         if (rect.y + 38 + 12 < rec.y + rec.height - 19)
            dc->DrawText(GetSampleFormatStr(((WaveTrack *) t)->GetSampleFormat()),
                         trackRect.x + offset,
                         trackRect.y + 38);
      }
   }

#ifdef USE_MIDI
   else if (bIsNote) {
      // Note tracks do not have text, e.g. "Mono, 44100Hz, 32-bit float", so
      // Mute & Solo button goes higher. To preserve existing AudioTrack code,
      // we move the buttons up by pretending track is higher (at lower y)
      rect.y -= 34;
      rect.height += 34;
      wxRect midiRect;
      mTrackInfo.GetTrackControlsRect(trackRect, midiRect);
      // Offset by height of Solo/Mute buttons:
      midiRect.y += 15;
      midiRect.height -= 21; // allow room for minimize button at bottom

#ifdef EXPERIMENTAL_MIDI_OUT
         // the offset 2 is just to leave a little space between channel buttons
         // and velocity slider (if any)
         int h = ((NoteTrack *) t)->DrawLabelControls(*dc, midiRect) + 2;

         // Draw some lines for MuteSolo buttons:
         if (rect.height > 84) {
            AColor::Line(*dc, rect.x+48 , rect.y+50, rect.x+48, rect.y + 66);
            // bevel below mute/solo
            AColor::Line(*dc, rect.x, rect.y + 66, mTrackInfo.GetTrackInfoWidth(), rect.y + 66);
         }
         mTrackInfo.DrawMuteSolo(dc, rect, t,
               (captured && mMouseCapture == IsMuting), false, HasSoloButton());
         mTrackInfo.DrawMuteSolo(dc, rect, t,
               (captured && mMouseCapture == IsSoloing), true, HasSoloButton());

         // place a volume control below channel buttons (this will
         // control an offset to midi velocity).
         // DrawVelocitySlider places slider assuming this is a Wave track
         // and using a large offset to leave room for other things,
         // so here we make a fake rectangle as if it is for a Wave
         // track, but it is offset to place the slider properly in
         // a Note track. This whole placement thing should be redesigned
         // to lay out different types of tracks and controls
         wxRect gr; // gr is gain rectangle where slider is drawn
         mTrackInfo.GetGainRect(rect, gr);
         rect.y = rect.y + h - gr.y; // ultimately want slider at rect.y + h
         rect.height = rect.height - h + gr.y;
         // save for mouse hit detect:
         ((NoteTrack *) t)->SetGainPlacementRect(rect);
         mTrackInfo.DrawVelocitySlider(dc, (NoteTrack *) t, rect);
#endif
   }
#endif // USE_MIDI
}

void TrackPanel::DrawOutsideOfTrack(Track * t, wxDC * dc, const wxRect & rect)
{
   // Fill in area outside of the track
   AColor::TrackPanelBackground(dc, false);
   wxRect side;

   // Area between panel border and left track border
   side = rect;
   side.width = kLeftInset;
   dc->DrawRectangle(side);

   // Area between panel border and top track border
   side = rect;
   side.height = kTopInset;
   dc->DrawRectangle(side);

   // Area between panel border and right track border
   side = rect;
   side.x += side.width - kTopInset;
   side.width = kTopInset;
   dc->DrawRectangle(side);

   // Area between tracks of stereo group
#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
   if (t->GetLinked() || MONO_WAVE_PAN(t)) {
      side = rect;
      side.y += t->GetHeight() - 1;
      side.height = kTopInset + 1;
      dc->DrawRectangle(side);
   }
#else
   if (t->GetLinked()) {
      // Paint the channel separator over (what would be) the shadow of the top
      // channel, and the top inset of the bottom channel
      side = rect;
      side.y += t->GetHeight() - kShadowThickness;
      side.height = kTopInset + kShadowThickness;
      dc->DrawRectangle(side);
   }
#endif
}

/// Draw a three-level highlight gradient around the focused track.
void TrackPanel::HighlightFocusedTrack(wxDC * dc, const wxRect & rect)
{
   wxRect theRect = rect;
   theRect.x += kLeftInset;
   theRect.y += kTopInset;
   theRect.width -= kLeftInset * 2;
   theRect.height -= kTopInset;

   dc->SetBrush(*wxTRANSPARENT_BRUSH);

   AColor::TrackFocusPen(dc, 0);
   dc->DrawRectangle(theRect.x - 1, theRect.y - 1, theRect.width + 2, theRect.height + 2);

   AColor::TrackFocusPen(dc, 1);
   dc->DrawRectangle(theRect.x - 2, theRect.y - 2, theRect.width + 4, theRect.height + 4);

   AColor::TrackFocusPen(dc, 2);
   dc->DrawRectangle(theRect.x - 3, theRect.y - 3, theRect.width + 6, theRect.height + 6);
}

void TrackPanel::UpdateVRulers()
{
   TrackListOfKindIterator iter(Track::Wave, GetTracks());
   for (Track *t = iter.First(); t; t = iter.Next()) {
      UpdateTrackVRuler(t);
   }

   UpdateVRulerSize();
}

void TrackPanel::UpdateVRuler(Track *t)
{
   UpdateTrackVRuler(t);

   UpdateVRulerSize();
}

void TrackPanel::UpdateTrackVRuler(Track *t)
{
   wxASSERT(t);
   if (!t)
      return;

   wxRect rect(GetVRulerOffset(),
            kTopMargin,
            GetVRulerWidth(),
            t->GetHeight() - (kTopMargin + kBottomMargin));

   mTrackArtist->UpdateVRuler(t, rect);
   Track *l = t->GetLink();
   if (l)
   {
      rect.height = l->GetHeight() - (kTopMargin + kBottomMargin);
      mTrackArtist->UpdateVRuler(l, rect);
   }
#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
   else if(MONO_WAVE_PAN(t)){
      rect.height = t->GetHeight(true) - (kTopMargin + kBottomMargin);
      mTrackArtist->UpdateVRuler(t, rect);
   }
#endif
}

void TrackPanel::UpdateVRulerSize()
{
   TrackListIterator iter(GetTracks());
   Track *t = iter.First();
   if (t) {
      wxSize s = t->vrulerSize;
      while (t) {
         s.IncTo(t->vrulerSize);
         t = iter.Next();
      }
      if (vrulerSize != s) {
         vrulerSize = s;
         mRuler->SetLeftOffset(GetLeftOffset());  // bevel on AdornedRuler
         mRuler->Refresh();
      }
   }
   Refresh(false);
}

/// The following method moves to the previous track
/// selecting and unselecting depending if you are on the start of a
/// block or not.

/// \todo Merge related methods, TrackPanel::OnPrevTrack and
/// TrackPanel::OnNextTrack.
void TrackPanel::OnPrevTrack( bool shift )
{
   TrackListIterator iter( GetTracks() );
   Track* t = GetFocusedTrack();
   if( t == NULL )   // if there isn't one, focus on last
   {
      t = iter.Last();
      SetFocusedTrack( t );
      EnsureVisible( t );
      MakeParentModifyState(false);
      return;
   }

   Track* p = NULL;
   bool tSelected = false;
   bool pSelected = false;
   if( shift )
   {
      p = mTracks->GetPrev( t, true ); // Get previous track
      if( p == NULL )   // On first track
      {
         // JKC: wxBell() is probably for accessibility, so a blind
         // user knows they were at the top track.
         wxBell();
         if( mCircularTrackNavigation )
         {
            TrackListIterator iter( GetTracks() );
            p = iter.Last();
         }
         else
         {
            EnsureVisible( t );
            return;
         }
      }
      tSelected = t->GetSelected();
      if (p)
         pSelected = p->GetSelected();
      if( tSelected && pSelected )
      {
         SelectTrack(t, false, false);
         SetFocusedTrack( p );   // move focus to next track down
         EnsureVisible( p );
         MakeParentModifyState(false);
         return;
      }
      if( tSelected && !pSelected )
      {
         SelectTrack(p, true, false);
         SetFocusedTrack( p );   // move focus to next track down
         EnsureVisible( p );
         MakeParentModifyState(false);
         return;
      }
      if( !tSelected && pSelected )
      {
         SelectTrack(p, false, false);
         SetFocusedTrack( p );   // move focus to next track down
         EnsureVisible( p );
         MakeParentModifyState(false);
         return;
      }
      if( !tSelected && !pSelected )
      {
         SelectTrack(t, true, false);
         SetFocusedTrack( p );   // move focus to next track down
         EnsureVisible( p );
         MakeParentModifyState(false);
         return;
      }
   }
   else
   {
      p = mTracks->GetPrev( t, true ); // Get next track
      if( p == NULL )   // On last track so stay there?
      {
         wxBell();
         if( mCircularTrackNavigation )
         {
            TrackListIterator iter( GetTracks() );
            for( Track *d = iter.First(); d; d = iter.Next( true ) )
            {
               p = d;
            }
            SetFocusedTrack( p );   // Wrap to the first track
            EnsureVisible( p );
            MakeParentModifyState(false);
            return;
         }
         else
         {
            EnsureVisible( t );
            return;
         }
      }
      else
      {
         SetFocusedTrack( p );   // move focus to next track down
         EnsureVisible( p );
         MakeParentModifyState(false);
         return;
      }
   }
}

/// The following method moves to the next track,
/// selecting and unselecting depending if you are on the start of a
/// block or not.
void TrackPanel::OnNextTrack( bool shift )
{
   Track *t;
   Track *n;
   TrackListIterator iter( GetTracks() );
   bool tSelected,nSelected;

   t = GetFocusedTrack();   // Get currently focused track
   if( t == NULL )   // if there isn't one, focus on first
   {
      t = iter.First();
      SetFocusedTrack( t );
      EnsureVisible( t );
      MakeParentModifyState(false);
      return;
   }

   if( shift )
   {
      n = mTracks->GetNext( t, true ); // Get next track
      if( n == NULL )   // On last track so stay there
      {
         wxBell();
         if( mCircularTrackNavigation )
         {
            TrackListIterator iter( GetTracks() );
            n = iter.First();
         }
         else
         {
            EnsureVisible( t );
            return;
         }
      }
      tSelected = t->GetSelected();
      nSelected = n->GetSelected();
      if( tSelected && nSelected )
      {
         SelectTrack(t, false, false);
         SetFocusedTrack( n );   // move focus to next track down
         EnsureVisible( n );
         MakeParentModifyState(false);
         return;
      }
      if( tSelected && !nSelected )
      {
         SelectTrack(n, true, false);
         SetFocusedTrack( n );   // move focus to next track down
         EnsureVisible( n );
         MakeParentModifyState(false);
         return;
      }
      if( !tSelected && nSelected )
      {
         SelectTrack(n, false, false);
         SetFocusedTrack( n );   // move focus to next track down
         EnsureVisible( n );
         MakeParentModifyState(false);
         return;
      }
      if( !tSelected && !nSelected )
      {
         SelectTrack(t, true, false);
         SetFocusedTrack( n );   // move focus to next track down
         EnsureVisible( n );
         MakeParentModifyState(false);
         return;
      }
   }
   else
   {
      n = mTracks->GetNext( t, true ); // Get next track
      if( n == NULL )   // On last track so stay there
      {
         wxBell();
         if( mCircularTrackNavigation )
         {
            TrackListIterator iter( GetTracks() );
            n = iter.First();
            SetFocusedTrack( n );   // Wrap to the first track
            EnsureVisible( n );
            MakeParentModifyState(false);
            return;
         }
         else
         {
            EnsureVisible( t );
            return;
         }
      }
      else
      {
         SetFocusedTrack( n );   // move focus to next track down
         EnsureVisible( n );
         MakeParentModifyState(false);
         return;
      }
   }
}

void TrackPanel::OnFirstTrack()
{
   Track *t = GetFocusedTrack();
   if (!t)
      return;

   TrackListIterator iter(GetTracks());
   Track *f = iter.First();
   if (t != f)
   {
      SetFocusedTrack(f);
      MakeParentModifyState(false);
   }
   EnsureVisible(f);
}

void TrackPanel::OnLastTrack()
{
   Track *t = GetFocusedTrack();
   if (!t)
      return;

   TrackListIterator iter(GetTracks());
   Track *l = iter.Last();
   if (t != l)
   {
      SetFocusedTrack(l);
      MakeParentModifyState(false);
   }
   EnsureVisible(l);
}

void TrackPanel::OnToggle()
{
   Track *t;

   t = GetFocusedTrack();   // Get currently focused track
   if (!t)
      return;

   SelectTrack( t, !t->GetSelected() );
   EnsureVisible( t );
   MakeParentModifyState(false);

   mAx->Updated();

   return;
}

// Make sure selection edge is in view
void TrackPanel::ScrollIntoView(double pos)
{
   int w;
   GetTracksUsableArea( &w, NULL );

   int pixel = mViewInfo->TimeToPosition(pos);
   if (pixel < 0 || pixel >= w)
   {
      mListener->TP_ScrollWindow
         (mViewInfo->OffsetTimeByPixels(pos, -(w / 2)));
      Refresh(false);
   }
}

void TrackPanel::ScrollIntoView(int x)
{
   ScrollIntoView(mViewInfo->PositionToTime(x, GetLeftOffset()));
}

void TrackPanel::OnTrackMenu(Track *t)
{
   BuildMenusIfNeeded();

   if(!t) {
      t = GetFocusedTrack();
      if(!t) return;
   }

   mPopupMenuTarget = t;

   Track *next = mTracks->GetNext(t);

   wxMenu *theMenu = NULL;
   if (t->GetKind() == Track::Time) {
      theMenu = mTimeTrackMenu.get();

      TimeTrack *tt = (TimeTrack*) t;

      theMenu->Check(OnTimeTrackLogIntID, tt->GetInterpolateLog());
   }

   if (t->GetKind() == Track::Wave) {
      theMenu = mWaveTrackMenu.get();
      const bool isMono = !t->GetLinked();
      const bool canMakeStereo =
         (next && isMono && !next->GetLinked() &&
          next->GetKind() == Track::Wave);

      theMenu->Enable(OnSwapChannelsID, t->GetLinked());
      theMenu->Enable(OnMergeStereoID, canMakeStereo);
      theMenu->Enable(OnSplitStereoID, t->GetLinked());
      theMenu->Enable(OnSplitStereoMonoID, t->GetLinked());

      // We only need to set check marks. Clearing checks causes problems on Linux (bug 851)
      // + Setting unchecked items to false is to get round a linux bug
      switch (t->GetChannel()) {
      case Track::LeftChannel:
         theMenu->Check(OnChannelLeftID, true);
         theMenu->Check(OnChannelRightID, false);
         theMenu->Check(OnChannelMonoID, false);
         break;
      case Track::RightChannel:
         theMenu->Check(OnChannelRightID, true);
         theMenu->Check(OnChannelLeftID, false);
         theMenu->Check(OnChannelMonoID, false);
         break;
      default:
         theMenu->Check(OnChannelMonoID, true);
         theMenu->Check(OnChannelLeftID, false);
         theMenu->Check(OnChannelRightID, false);
      }

      theMenu->Enable(OnChannelMonoID, !t->GetLinked());
      theMenu->Enable(OnChannelLeftID, !t->GetLinked());
      theMenu->Enable(OnChannelRightID, !t->GetLinked());

      WaveTrack *const track = (WaveTrack *)t;
      const int display = track->GetDisplay();
      theMenu->Check(
         (display == WaveTrack::Waveform)
         ? (track->GetWaveformSettings().isLinear() ? OnWaveformID : OnWaveformDBID)
         : OnSpectrumID,
         true
      );
      // Bug 1253.  Shouldn't open preferences if audio is busy.
      // We can't change them on the fly yet anyway.
      const bool bAudioBusy = gAudioIO->IsBusy();
      theMenu->Enable(OnSpectrogramSettingsID, 
         (display == WaveTrack::Spectrum) && !bAudioBusy);

      SetMenuCheck(*mRateMenu, IdOfRate((int) track->GetRate()));
      SetMenuCheck(*mFormatMenu, IdOfFormat(track->GetSampleFormat()));

      bool unsafe = IsUnsafe();
      for (int i = OnRate8ID; i <= OnFloatID; i++) {
         theMenu->Enable(i, !unsafe);
      }
   }

#if defined(USE_MIDI)
   if (t->GetKind() == Track::Note)
      theMenu = mNoteTrackMenu.get();
#endif

   if (t->GetKind() == Track::Label){
       theMenu = mLabelTrackMenu.get();
   }

   if (theMenu) {
      theMenu->Enable(OnMoveUpID, mTracks->CanMoveUp(t));
      theMenu->Enable(OnMoveDownID, mTracks->CanMoveDown(t));
      theMenu->Enable(OnMoveTopID, mTracks->CanMoveUp(t));
      theMenu->Enable(OnMoveBottomID, mTracks->CanMoveDown(t));

      //We need to find the location of the menu rectangle.
      wxRect rect = FindTrackRect(t,true);
      wxRect titleRect;
      mTrackInfo.GetTitleBarRect(rect,titleRect);

      PopupMenu(theMenu, titleRect.x + 1,
                  titleRect.y + titleRect.height + 1);
   }

   mPopupMenuTarget = NULL;

   SetCapturedTrack(NULL);

   Refresh(false);
}

void TrackPanel::OnVRulerMenu(Track *t, wxMouseEvent *pEvent)
{
   if (!t) {
      t = GetFocusedTrack();
      if (!t)
         return;
   }

   if (t->GetKind() != Track::Wave)
      return;

   WaveTrack *const wt = static_cast<WaveTrack*>(t);

   const int display = wt->GetDisplay();
   wxMenu *theMenu;
   if (display == WaveTrack::Waveform) {
      theMenu = mRulerWaveformMenu.get();
      const int id =
         OnFirstWaveformScaleID + (int)(wt->GetWaveformSettings().scaleType);
      theMenu->Check(id, true);
   }
   else {
      theMenu = mRulerSpectrumMenu.get();
      const int id =
         OnFirstSpectrumScaleID + (int)(wt->GetSpectrogramSettings().scaleType);
      theMenu->Check(id, true);
   }

   int x, y;
   if (pEvent)
      x = pEvent->m_x, y = pEvent->m_y;
   else {
      // If no event given, pop up the menu at the same height
      // as for the track control menu
      const wxRect rect = FindTrackRect(wt, true);
      wxRect titleRect;
      mTrackInfo.GetTitleBarRect(rect, titleRect);
      x = GetVRulerOffset(), y = titleRect.y + titleRect.height + 1;
   }

   // So that IsDragZooming() returns false, and if we zoom in, we do so
   // centered where the mouse is now:
   mZoomStart = mZoomEnd = pEvent->m_y;

   mPopupMenuTarget = wt;
   PopupMenu(theMenu, x, y);
   mPopupMenuTarget = NULL;
}


Track * TrackPanel::GetFirstSelectedTrack()
{

   TrackListIterator iter(GetTracks());

   Track * t;
   for ( t = iter.First();t!=NULL;t=iter.Next())
      {
         //Find the first selected track
         if(t->GetSelected())
            {
               return t;
            }

      }
   //if nothing is selected, return the first track
   t = iter.First();

   if(t)
      return t;
   else
      return NULL;
}

void TrackPanel::EnsureVisible(Track * t)
{
   TrackListIterator iter(GetTracks());
   Track *it = NULL;
   Track *nt = NULL;

   SetFocusedTrack(t);

   int trackTop = 0;
   int trackHeight =0;

   for (it = iter.First(); it; it = iter.Next()) {
      trackTop += trackHeight;
      trackHeight =  it->GetHeight();

      //find the second track if this is stereo
      if (it->GetLinked()) {
         nt = iter.Next();
         trackHeight += nt->GetHeight();
      }
#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
      else if(MONO_WAVE_PAN(it)){
         trackHeight += it->GetHeight(true);
      }
#endif
      else {
         nt = it;
      }

      //We have found the track we want to ensure is visible.
      if ((it == t) || (nt == t)) {

         //Get the size of the trackpanel.
         int width, height;
         GetSize(&width, &height);

         if (trackTop < mViewInfo->vpos) {
            height = mViewInfo->vpos - trackTop + mViewInfo->scrollStep;
            height /= mViewInfo->scrollStep;
            mListener->TP_ScrollUpDown(-height);
         }
         else if (trackTop + trackHeight > mViewInfo->vpos + height) {
            height = (trackTop + trackHeight) - (mViewInfo->vpos + height);
            height = (height + mViewInfo->scrollStep + 1) / mViewInfo->scrollStep;
            mListener->TP_ScrollUpDown(height);
         }

         break;
      }
   }
   Refresh(false);
}

void TrackPanel::DrawBordersAroundTrack(Track * t, wxDC * dc,
                                        const wxRect & rect, const int vrul,
                                        const int labelw)
{
   // Border around track and label area
   dc->SetBrush(*wxTRANSPARENT_BRUSH);
   dc->SetPen(*wxBLACK_PEN);
   dc->DrawRectangle(rect.x, rect.y, rect.width - 1, rect.height - 1);

   AColor::Line(*dc, labelw, rect.y, labelw, rect.y + rect.height - 1);       // between vruler and TrackInfo

   // The lines at bottom of 1st track and top of second track of stereo group
   // Possibly replace with DrawRectangle to add left border.
#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
   if (t->GetLinked() || MONO_WAVE_PAN(t)) {
      int h1 = rect.y + t->GetHeight() - kTopInset;
      AColor::Line(*dc, vrul, h1 - 2, rect.x + rect.width - 1, h1 - 2);
      AColor::Line(*dc, vrul, h1 + kTopInset, rect.x + rect.width - 1, h1 + kTopInset);
   }
#else
   if (t->GetLinked()) {
      // The given rect has had the top inset subtracted
      int h1 = rect.y + t->GetHeight() - kTopInset;
      // h1 is the top coordinate of the second tracks' rectangle
      // Draw (part of) the bottom border of the top channel and top border of the bottom
      AColor::Line(*dc, vrul, h1 - kBottomMargin, rect.x + rect.width - 1, h1 - kBottomMargin);
      AColor::Line(*dc, vrul, h1 + kTopInset, rect.x + rect.width - 1, h1 + kTopInset);
   }
#endif
}

void TrackPanel::DrawShadow(Track * /* t */ , wxDC * dc, const wxRect & rect)
{
   int right = rect.x + rect.width - 1;
   int bottom = rect.y + rect.height - 1;

   // shadow
   dc->SetPen(*wxBLACK_PEN);

   // bottom
   AColor::Line(*dc, rect.x, bottom, right, bottom);
   // right
   AColor::Line(*dc, right, rect.y, right, bottom);

   // background
   AColor::Dark(dc, false);

   // bottom
   AColor::Line(*dc, rect.x, bottom, rect.x + 1, bottom);
   // right
   AColor::Line(*dc, right, rect.y, right, rect.y + 1);
}

/// Returns the string to be displayed in the track label
/// indicating whether the track is mono, left, right, or
/// stereo and what sample rate it's using.
wxString TrackPanel::TrackSubText(WaveTrack * t)
{
   wxString s = wxString::Format(wxT("%dHz"), (int) (t->GetRate() + 0.5));
#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
   if (t->GetLinked() && t->GetChannel() != Track::MonoChannel)
      s = _("Stereo, ") + s;
#else
   if (t->GetLinked())
      s = _("Stereo, ") + s;
#endif
   else {
      if (t->GetChannel() == Track::MonoChannel)
         s = _("Mono, ") + s;
      else if (t->GetChannel() == Track::LeftChannel)
         s = _("Left, ") + s;
      else if (t->GetChannel() == Track::RightChannel)
         s = _("Right, ") + s;
   }

   return s;
}

/// Handle the menu options that change a track between
/// left channel, right channel, and mono.
static int channels[] = { Track::LeftChannel, Track::RightChannel,
   Track::MonoChannel
};

static const wxChar *channelmsgs[] = { _("Left Channel"), _("Right Channel"),
   _("Mono")
};

void TrackPanel::OnChannelChange(wxCommandEvent & event)
{
   int id = event.GetId();
   wxASSERT(id >= OnChannelLeftID && id <= OnChannelMonoID);
   wxASSERT(mPopupMenuTarget);
   mPopupMenuTarget->SetChannel(channels[id - OnChannelLeftID]);
   MakeParentPushState(wxString::Format(_("Changed '%s' to %s"),
                        mPopupMenuTarget->GetName().c_str(),
                        channelmsgs[id - OnChannelLeftID]),
                        _("Channel"));
   Refresh(false);
}

/// Swap the left and right channels of a stero track...
void TrackPanel::OnSwapChannels(wxCommandEvent & WXUNUSED(event))
{
   Track *partner = mPopupMenuTarget->GetLink();
   Track *const focused = GetFocusedTrack();
   const bool hasFocus =
      (focused == mPopupMenuTarget || focused == partner);

   SplitStereo(true);
   mPopupMenuTarget->SetChannel(Track::RightChannel);
   partner->SetChannel(Track::LeftChannel);

   (mTracks->MoveUp(partner));
   partner->SetLinked(true);

   MixerBoard* pMixerBoard = this->GetMixerBoard();
   if (pMixerBoard) {
      pMixerBoard->UpdateTrackClusters();
   }

   if (hasFocus)
      SetFocusedTrack(partner);

   MakeParentPushState(wxString::Format(_("Swapped Channels in '%s'"),
                                        mPopupMenuTarget->GetName().c_str()),
                       _("Swap Channels"));

}

/// Split a stereo track into two tracks...
void TrackPanel::OnSplitStereo(wxCommandEvent & WXUNUSED(event))
{
   SplitStereo(true);
   MakeParentPushState(wxString::Format(_("Split stereo track '%s'"),
                                        mPopupMenuTarget->GetName().c_str()),
                       _("Split"));
}

/// Split a stereo track into two mono tracks...
void TrackPanel::OnSplitStereoMono(wxCommandEvent & WXUNUSED(event))
{
   SplitStereo(false);
   MakeParentPushState(wxString::Format(_("Split Stereo to Mono '%s'"),
                                        mPopupMenuTarget->GetName().c_str()),
                       _("Split to Mono"));
}

/// Split a stereo track into two tracks...
void TrackPanel::SplitStereo(bool stereo)
{
   wxASSERT(mPopupMenuTarget);

   if (!stereo)
      mPopupMenuTarget->SetChannel(Track::MonoChannel);

   // Assume partner is present, and is wave
   auto partner = static_cast<WaveTrack*>(mPopupMenuTarget->GetLink());
   wxASSERT(partner);
   if (!partner)
      return;

#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
   if(!stereo && MONO_WAVE_PAN(mPopupMenuTarget))
      // Come here only from wave track menu
      static_cast<WaveTrack*>(mPopupMenuTarget)->SetVirtualState(true,true);
   if(!stereo && MONO_WAVE_PAN(partner))
      partner->SetVirtualState(true,true);
#endif

   if (partner)
   {
      partner->SetName(mPopupMenuTarget->GetName());
      if (!stereo)
         partner->SetChannel(Track::MonoChannel);  // Keep original stereo track name.

      //On Demand - have each channel add it's own.
      if (ODManager::IsInstanceCreated() && partner->GetKind() == Track::Wave)
         ODManager::Instance()->MakeWaveTrackIndependent(partner);
   }

   mPopupMenuTarget->SetLinked(false);
   //make sure neither track is smaller than its minimum height
   if (mPopupMenuTarget->GetHeight() < mPopupMenuTarget->GetMinimizedHeight())
      mPopupMenuTarget->SetHeight(mPopupMenuTarget->GetMinimizedHeight());
   if (partner)
   {
      if (partner->GetHeight() < partner->GetMinimizedHeight())
         partner->SetHeight(partner->GetMinimizedHeight());

      // Make tracks the same height
      if (mPopupMenuTarget->GetHeight() != partner->GetHeight())
      {
         mPopupMenuTarget->SetHeight(((mPopupMenuTarget->GetHeight())+(partner->GetHeight())) / 2.0);
         partner->SetHeight(mPopupMenuTarget->GetHeight());
      }
   }

   Refresh(false);
}

/// Merge two tracks into one stereo track ??
void TrackPanel::OnMergeStereo(wxCommandEvent & WXUNUSED(event))
{
   wxASSERT(mPopupMenuTarget);
   mPopupMenuTarget->SetLinked(true);
   Track *partner = mPopupMenuTarget->GetLink();

#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
   if(MONO_WAVE_PAN(mPopupMenuTarget))
      ((WaveTrack*)mPopupMenuTarget)->SetVirtualState(false);
   if(MONO_WAVE_PAN(partner))
      ((WaveTrack*)partner)->SetVirtualState(false);
#endif

   if (partner) {
      // Set partner's parameters to match target.
      partner->Merge(*mPopupMenuTarget);

      mPopupMenuTarget->SetChannel(Track::LeftChannel);
      partner->SetChannel(Track::RightChannel);

      // Set NEW track heights and minimized state
      bool bBothMinimizedp=((mPopupMenuTarget->GetMinimized())&&(partner->GetMinimized()));
      mPopupMenuTarget->SetMinimized(false);
      partner->SetMinimized(false);
      int AverageHeight=(mPopupMenuTarget->GetHeight() + partner->GetHeight())/ 2;
      mPopupMenuTarget->SetHeight(AverageHeight);
      partner->SetHeight(AverageHeight);
      mPopupMenuTarget->SetMinimized(bBothMinimizedp);
      partner->SetMinimized(bBothMinimizedp);

      //On Demand - join the queues together.
      WaveTrack *wt, *pwt;
      if(ODManager::IsInstanceCreated() &&
         // Assume linked track is wave or null
         nullptr != (pwt = static_cast<WaveTrack*>(partner)) &&
         // Come here only from the wave track menu
         nullptr != (wt = static_cast<WaveTrack*>(mPopupMenuTarget)))
         if(!ODManager::Instance()->MakeWaveTrackDependent(pwt, wt))
         {
            ;
            //TODO: in the future, we will have to check the return value of MakeWaveTrackDependent -
            //if the tracks cannot merge, it returns false, and in that case we should not allow a merging.
            //for example it returns false when there are two different types of ODTasks on each track's queue.
            //we will need to display this to the user.
         }

      MakeParentPushState(wxString::Format(_("Made '%s' a stereo track"),
                                           mPopupMenuTarget->GetName().
                                           c_str()),
                          _("Make Stereo"));
   } else
      mPopupMenuTarget->SetLinked(false);

   Refresh(false);
}

class ViewSettingsDialog final : public PrefsDialog
{
public:
   ViewSettingsDialog
      (wxWindow *parent, const wxString &title, PrefsDialog::Factories &factories,
       int page)
      : PrefsDialog(parent, title, factories)
      , mPage(page)
   {
   }

   long GetPreferredPage() override
   {
      return mPage;
   }

   void SavePreferredPage() override
   {
   }

private:
   const int mPage;
};

void TrackPanel::OnSpectrogramSettings(wxCommandEvent &)
{

   if (gAudioIO->IsBusy()){
      wxMessageBox(_("To change Spectrogram Settings, stop any\n."
         "playing or recording first."), 
         _("Stop the Audio First"), wxOK | wxICON_EXCLAMATION | wxCENTRE);
      return;
   }
   // Get here only from the wave track menu
   const auto wt = static_cast<WaveTrack*>(mPopupMenuTarget);
   // WaveformPrefsFactory waveformFactory(wt);
   SpectrumPrefsFactory spectrumFactory(wt);

   PrefsDialog::Factories factories;
   // factories.push_back(&waveformFactory);
   factories.push_back(&spectrumFactory);
   const int page = (wt->GetDisplay() == WaveTrack::Spectrum)
      ? 1 : 0;

   wxString title(wt->GetName() + wxT(": "));
   ViewSettingsDialog dialog(this, title, factories, page);

   if (0 != dialog.ShowModal()) {
      MakeParentModifyState(true);
      // Redraw
      Refresh(false);
   }
}

///  Set the Display mode based on the menu choice in the Track Menu.
///  Note that gModes MUST BE IN THE SAME ORDER AS THE MENU CHOICES!!
///  const wxChar *gModes[] = { wxT("waveform"), wxT("waveformDB"),
///  wxT("spectrum"), wxT("pitch") };
void TrackPanel::OnSetDisplay(wxCommandEvent & event)
{
   int idInt = event.GetId();
   wxASSERT(idInt >= OnWaveformID && idInt <= OnSpectrumID);
   wxASSERT(mPopupMenuTarget
            && mPopupMenuTarget->GetKind() == Track::Wave);

   bool linear = false;
   WaveTrack::WaveTrackDisplay id;
   switch (idInt) {
   default:
   case OnWaveformID:
      linear = true, id = WaveTrack::Waveform; break;
   case OnWaveformDBID:
      id = WaveTrack::Waveform; break;
   case OnSpectrumID:
      id = WaveTrack::Spectrum; break;
   }
   WaveTrack *wt = (WaveTrack *) mPopupMenuTarget;
   const bool wrongType = wt->GetDisplay() != id;
   const bool wrongScale =
      (id == WaveTrack::Waveform &&
       wt->GetWaveformSettings().isLinear() != linear);
   if (wrongType || wrongScale) {
      wt->SetLastScaleType();
      wt->SetDisplay(WaveTrack::WaveTrackDisplay(id));
      if (wrongScale)
         wt->GetIndependentWaveformSettings().scaleType = linear
         ? WaveformSettings::stLinear
         : WaveformSettings::stLogarithmic;

      // Assume linked track is wave or null
      const auto l = static_cast<WaveTrack*>(wt->GetLink());
      if (l) {
         l->SetLastScaleType();
         l->SetDisplay(WaveTrack::WaveTrackDisplay(id));
         if (wrongScale)
            l->GetIndependentWaveformSettings().scaleType = linear
            ? WaveformSettings::stLinear
            : WaveformSettings::stLogarithmic;
   }
#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
      if (wt->GetDisplay() == WaveTrack::WaveformDisplay) {
         wt->SetVirtualState(false);
      }else if (id == WaveTrack::WaveformDisplay) {
         wt->SetVirtualState(true);
      }
#endif
      UpdateVRuler(wt);

      MakeParentModifyState(true);
      Refresh(false);
   }
}

/// Sets the sample rate for a track, and if it is linked to
/// another track, that one as well.
void TrackPanel::SetRate(WaveTrack * wt, double rate)
{
   wt->SetRate(rate);
   // Assume linked track is wave or null
   const auto partner = static_cast<WaveTrack*>(mTracks->GetLink(wt));
   if (partner)
      partner->SetRate(rate);
   // Separate conversion of "rate" enables changing the decimals without affecting i18n
   wxString rateString = wxString::Format(wxT("%.3f"), rate);
   MakeParentPushState(wxString::Format(_("Changed '%s' to %s Hz"),
                                        wt->GetName().c_str(), rateString.c_str()),
                       _("Rate Change"));
}

/// Handles the selection from the Format submenu of the
/// track menu.
void TrackPanel::OnFormatChange(wxCommandEvent & event)
{
   BuildMenusIfNeeded();

   int id = event.GetId();
   wxASSERT(id >= On16BitID && id <= OnFloatID);
   wxASSERT(mPopupMenuTarget
            && mPopupMenuTarget->GetKind() == Track::Wave);

   sampleFormat newFormat = int16Sample;

   switch (id) {
   case On16BitID:
      newFormat = int16Sample;
      break;
   case On24BitID:
      newFormat = int24Sample;
      break;
   case OnFloatID:
      newFormat = floatSample;
      break;
   default:
      // ERROR -- should not happen
      wxASSERT(false);
      break;
   }
   if (newFormat == ((WaveTrack*)mPopupMenuTarget)->GetSampleFormat())
      return; // Nothing to do.

   bool bResult = ((WaveTrack*)mPopupMenuTarget)->ConvertToSampleFormat(newFormat);
   wxASSERT(bResult); // TO DO: Actually handle this.
   // Assume linked track is wave or null
   const auto partner =
      static_cast<WaveTrack*>(mTracks->GetLink(mPopupMenuTarget));
   if (partner)
   {
      bResult = partner->ConvertToSampleFormat(newFormat);
      wxASSERT(bResult); // TO DO: Actually handle this.
   }

   MakeParentPushState(wxString::Format(_("Changed '%s' to %s"),
                                        mPopupMenuTarget->GetName().
                                        c_str(),
                                        GetSampleFormatStr(newFormat)),
                       _("Format Change"));

   SetMenuCheck( *mFormatMenu, id );
   MakeParentRedrawScrollbars();
   Refresh(false);
}

/// Converts a format enumeration to a wxWidgets menu item Id.
int TrackPanel::IdOfFormat( int format )
{
   switch (format) {
   case int16Sample:
      return On16BitID;
   case int24Sample:
      return On24BitID;
   case floatSample:
      return OnFloatID;
   default:
      // ERROR -- should not happen
      wxASSERT( false );
      break;
   }
   return OnFloatID;// Compiler food.
}

/// Puts a check mark at a given position in a menu.
void TrackPanel::SetMenuCheck( wxMenu & menu, int newId )
{
   auto & list = menu.GetMenuItems();

   for ( auto item : list )
   {
      auto id = item->GetId();
      // We only need to set check marks. Clearing checks causes problems on Linux (bug 851)
      if (id==newId)
         menu.Check( id, true );
   }
}

const int nRates=12;

///  gRates MUST CORRESPOND DIRECTLY TO THE RATES AS LISTED IN THE MENU!!
///  IN THE SAME ORDER!!
static int gRates[nRates] = { 8000, 11025, 16000, 22050, 44100, 48000, 88200, 96000,
                       176400, 192000, 352800, 384000 };

/// This method handles the selection from the Rate
/// submenu of the track menu, except for "Other" (/see OnRateOther).
void TrackPanel::OnRateChange(wxCommandEvent & event)
{
   BuildMenusIfNeeded();

   int id = event.GetId();
   wxASSERT(id >= OnRate8ID && id <= OnRate384ID);

   SetMenuCheck( *mRateMenu, id );
   // Come here only from wave track menu
   SetRate(static_cast<WaveTrack*>(mPopupMenuTarget), gRates[id - OnRate8ID]);

   MakeParentRedrawScrollbars();

   Refresh(false);
}

/// Converts a sampling rate to a wxWidgets menu item id
int TrackPanel::IdOfRate( int rate )
{
   for(int i=0;i<nRates;i++) {
      if( gRates[i] == rate )
         return i+OnRate8ID;
   }
   return OnRateOtherID;
}

void TrackPanel::OnRateOther(wxCommandEvent &event)
{
   BuildMenusIfNeeded();

   // Come here only from the wave track menu
   const auto wt = static_cast<WaveTrack*>(mPopupMenuTarget);
   wxASSERT(wt);

   int newRate;

   /// \todo Remove artificial constants!!
   /// \todo Make a real dialog box out of this!!
   while (true)
   {
      wxDialogWrapper dlg(this, wxID_ANY, wxString(_("Set Rate")));
      dlg.SetName(dlg.GetTitle());
      ShuttleGui S(&dlg, eIsCreating);
      wxString rate;
      wxArrayString rates;
      wxComboBox *cb;

      rate.Printf(wxT("%ld"), lrint(((WaveTrack *) mPopupMenuTarget)->GetRate()));

      rates.Add(wxT("8000"));
      rates.Add(wxT("11025"));
      rates.Add(wxT("16000"));
      rates.Add(wxT("22050"));
      rates.Add(wxT("44100"));
      rates.Add(wxT("48000"));
      rates.Add(wxT("88200"));
      rates.Add(wxT("96000"));
      rates.Add(wxT("176400"));
      rates.Add(wxT("192000"));
      rates.Add(wxT("352800"));
      rates.Add(wxT("384000"));

      S.StartVerticalLay(true);
      {
         S.SetBorder(10);
         S.StartHorizontalLay(wxEXPAND, false);
         {
            cb = S.AddCombo(_("New sample rate (Hz):"),
                            rate,
                            &rates);
#if defined(__WXMAC__)
            // As of wxMac-2.8.12, setting manually is required
            // to handle rates not in the list.  See: Bug #427
            cb->SetValue(rate);
#endif
         }
         S.EndHorizontalLay();
         S.AddStandardButtons();
      }
      S.EndVerticalLay();

      dlg.SetClientSize(dlg.GetSizer()->CalcMin());
      dlg.Center();

      if (dlg.ShowModal() != wxID_OK)
      {
         return;  // user cancelled dialog
      }

      long lrate;
      if (cb->GetValue().ToLong(&lrate) && lrate >= 1 && lrate <= 1000000)
      {
         newRate = (int)lrate;
         break;
      }

      wxMessageBox(_("The entered value is invalid"), _("Error"),
                   wxICON_ERROR, this);
   }

   SetMenuCheck( *mRateMenu, event.GetId() );
   SetRate(wt, newRate);

   MakeParentRedrawScrollbars();
   Refresh(false);
}

void TrackPanel::OnSetTimeTrackRange(wxCommandEvent & /*event*/)
{
   TimeTrack *t = (TimeTrack*)mPopupMenuTarget;

   if (t) {
      long lower = (long) (t->GetRangeLower() * 100.0 + 0.5);
      long upper = (long) (t->GetRangeUpper() * 100.0 + 0.5);

      // MB: these lower/upper limits match the maximum allowed range of the time track
      // envelope, but this is not strictly required
      lower = wxGetNumberFromUser(_("Change lower speed limit (%) to:"),
                                  _("Lower speed limit"),
                                  _("Lower speed limit"),
                                  lower,
                                  10,
                                  1000);

      upper = wxGetNumberFromUser(_("Change upper speed limit (%) to:"),
                                  _("Upper speed limit"),
                                  _("Upper speed limit"),
                                  upper,
                                  lower+1,
                                  1000);

      if( lower >= 10 && upper <= 1000 && lower < upper ) {
         t->SetRangeLower((double)lower / 100.0);
         t->SetRangeUpper((double)upper / 100.0);
         MakeParentPushState(wxString::Format(_("Set range to '%ld' - '%ld'"),
                                              lower,
                                              upper),
      /* i18n-hint: (verb)*/

                             _("Set Range"));
         Refresh(false);
      }
   }
}

void TrackPanel::OnTimeTrackLin(wxCommandEvent & /*event*/)
{
   // Come here only from the time track menu
   const auto t = static_cast<TimeTrack*>(mPopupMenuTarget);
   t->SetDisplayLog(false);
   UpdateVRuler(t);
   MakeParentPushState(_("Set time track display to linear"), _("Set Display"));
   Refresh(false);
}

void TrackPanel::OnTimeTrackLog(wxCommandEvent & /*event*/)
{
   // Come here only from the time track menu
   const auto t = static_cast<TimeTrack*>(mPopupMenuTarget);
   t->SetDisplayLog(true);
   UpdateVRuler(t);
   MakeParentPushState(_("Set time track display to logarithmic"), _("Set Display"));
   Refresh(false);
}

void TrackPanel::OnTimeTrackLogInt(wxCommandEvent & /*event*/)
{
   // Come here only from the time track menu
   const auto t = static_cast<TimeTrack*>(mPopupMenuTarget);
   if(t->GetInterpolateLog()) {
      t->SetInterpolateLog(false);
      MakeParentPushState(_("Set time track interpolation to linear"), _("Set Interpolation"));
   } else {
      t->SetInterpolateLog(true);
      MakeParentPushState(_("Set time track interpolation to logarithmic"), _("Set Interpolation"));
   }
   Refresh(false);
}

void TrackPanel::OnWaveformScaleType(wxCommandEvent &evt)
{
   // Get here only from vertical ruler menu for wave tracks
   const auto wt = static_cast<WaveTrack *>(mPopupMenuTarget);
   // Assume linked track is wave or null
   const auto partner = static_cast<WaveTrack*>(wt->GetLink());
   const WaveformSettings::ScaleType newScaleType =
      WaveformSettings::ScaleType(
         std::max(0,
            std::min((int)(WaveformSettings::stNumScaleTypes) - 1,
               evt.GetId() - OnFirstWaveformScaleID
      )));
   if (wt->GetWaveformSettings().scaleType != newScaleType) {
      wt->GetIndependentWaveformSettings().scaleType = newScaleType;
      if (partner)
         partner->GetIndependentWaveformSettings().scaleType = newScaleType;
      UpdateVRuler(wt); // Is this really needed?
      MakeParentModifyState(true);
      Refresh(false);
   }
}

void TrackPanel::OnSpectrumScaleType(wxCommandEvent &evt)
{
   // Get here only from vertical ruler menu for wave tracks
   const auto wt = static_cast<WaveTrack *>(mPopupMenuTarget);
   // Assume linked track is wave or null
   const auto partner = static_cast<WaveTrack*>(wt->GetLink());
   const SpectrogramSettings::ScaleType newScaleType =
      SpectrogramSettings::ScaleType(
         std::max(0,
            std::min((int)(SpectrogramSettings::stNumScaleTypes) - 1,
               evt.GetId() - OnFirstSpectrumScaleID
      )));
   if (wt->GetSpectrogramSettings().scaleType != newScaleType) {
      wt->GetIndependentSpectrogramSettings().scaleType = newScaleType;
      if (partner)
         partner->GetIndependentSpectrogramSettings().scaleType = newScaleType;
      UpdateVRuler(wt); // Is this really needed?
      MakeParentModifyState(true);
      Refresh(false);
   }
}

void TrackPanel::OnZoomInVertical(wxCommandEvent &)
{
   // Get here only from vertical ruler menu for wave tracks
   HandleWaveTrackVZoom(static_cast<WaveTrack*>(mPopupMenuTarget), false, false);
}

void TrackPanel::OnZoomOutVertical(wxCommandEvent &)
{
   // Get here only from vertical ruler menu for wave tracks
   HandleWaveTrackVZoom(static_cast<WaveTrack*>(mPopupMenuTarget), true, false);
}

void TrackPanel::OnZoomFitVertical(wxCommandEvent &)
{
   // Get here only from vertical ruler menu for wave tracks
   HandleWaveTrackVZoom(static_cast<WaveTrack*>(mPopupMenuTarget), true, true);
}

/// Move a track up, down, to top or to bottom.

void TrackPanel::OnMoveTrack(wxCommandEvent &event)
{
   AudacityProject::MoveChoice choice;
   switch (event.GetId()) {
   default:
      wxASSERT(false);
   case OnMoveUpID:
      choice = AudacityProject::OnMoveUpID; break;
   case OnMoveDownID:
      choice = AudacityProject::OnMoveDownID; break;
   case OnMoveTopID:
      choice = AudacityProject::OnMoveTopID; break;
   case OnMoveBottomID:
      choice = AudacityProject::OnMoveBottomID; break;
   }

   GetProject()->MoveTrack(mPopupMenuTarget, choice);
}

/// This only applies to MIDI tracks.  Presumably, it shifts the
/// whole sequence by an octave.
void TrackPanel::OnChangeOctave(wxCommandEvent & event)
{
#if defined(USE_MIDI)
   wxASSERT(event.GetId() == OnUpOctaveID
            || event.GetId() == OnDownOctaveID);
   wxASSERT(mPopupMenuTarget->GetKind() == Track::Note);
   NoteTrack *t = (NoteTrack *) mPopupMenuTarget;

   bool bDown = (OnDownOctaveID == event.GetId());
   t->SetBottomNote(t->GetBottomNote() + ((bDown) ? -12 : 12));

   MakeParentModifyState(true);
   Refresh(false);
#endif
}

void TrackPanel::OnSetName(wxCommandEvent & WXUNUSED(event))
{
   Track *t = mPopupMenuTarget;
   if (t)
   {
      wxString oldName = t->GetName();
      wxString newName =
         wxGetTextFromUser(_("Change track name to:"),
                           _("Track Name"), oldName);
      if (newName != wxT("")) // wxGetTextFromUser returns empty string on Cancel.
      {
         t->SetName(newName);
         // if we have a linked channel this name should change as well
         // (otherwise sort by name and time will crash).
         if (t->GetLinked())
            t->GetLink()->SetName(newName);

         MixerBoard* pMixerBoard = this->GetMixerBoard();
         if (pMixerBoard && (t->GetKind() == Track::Wave))
            pMixerBoard->UpdateName((WaveTrack*)t);

         MakeParentPushState(wxString::Format(_("Renamed '%s' to '%s'"),
                                              oldName.c_str(),
                                              newName.c_str()),
                             _("Name Change"));

         Refresh(false);
      }
   }
}

// Small helper class to enumerate all fonts in the system
// We use this because the default implementation of
// wxFontEnumerator::GetFacenames() has changed between wx2.6 and 2.8
class TrackPanelFontEnumerator final : public wxFontEnumerator
{
public:
   TrackPanelFontEnumerator(wxArrayString* fontNames) :
      mFontNames(fontNames) {}

   bool OnFacename(const wxString& font) override
   {
      mFontNames->Add(font);
      return true;
   }

private:
   wxArrayString* mFontNames;
};

void TrackPanel::OnSetFont(wxCommandEvent & WXUNUSED(event))
{
   wxArrayString facenames;
   TrackPanelFontEnumerator fontEnumerator(&facenames);
   fontEnumerator.EnumerateFacenames(wxFONTENCODING_SYSTEM, false);

   wxString facename = gPrefs->Read(wxT("/GUI/LabelFontFacename"), wxT(""));

   // Correct for empty facename, or bad preference file:
   // get the name of a really existing font, to highlight by default
   // in the list box
   facename = LabelTrack::GetFont(facename).GetFaceName();

   long fontsize = gPrefs->Read(wxT("/GUI/LabelFontSize"),
                                LabelTrack::DefaultFontSize);

   /* i18n-hint: (noun) This is the font for the label track.*/
   wxDialogWrapper dlg(this, wxID_ANY, wxString(_("Label Track Font")));
   dlg.SetName(dlg.GetTitle());
   ShuttleGui S(&dlg, eIsCreating);
   wxListBox *lb;
   wxSpinCtrl *sc;

   S.StartVerticalLay(true);
   {
      S.StartMultiColumn(2, wxEXPAND);
      {
         S.SetStretchyRow(0);
         S.SetStretchyCol(1);

         /* i18n-hint: (noun) The name of the typeface*/
         S.AddPrompt(_("Face name"));
         lb = safenew wxListBox(&dlg, wxID_ANY,
                            wxDefaultPosition,
                            wxDefaultSize,
                            facenames,
                            wxLB_SINGLE);

         lb->SetName(_("Face name"));
         lb->SetSelection(facenames.Index(facename));
         S.AddWindow(lb, wxALIGN_LEFT | wxEXPAND | wxALL);

         /* i18n-hint: (noun) The size of the typeface*/
         S.AddPrompt(_("Face size"));
         sc = safenew wxSpinCtrl(&dlg, wxID_ANY,
                             wxString::Format(wxT("%ld"), fontsize),
                             wxDefaultPosition,
                             wxDefaultSize,
                             wxSP_ARROW_KEYS,
                             8, 48, fontsize);
         sc->SetName(_("Face size"));
         S.AddWindow(sc, wxALIGN_LEFT | wxALL);
      }
      S.EndMultiColumn();
      S.AddStandardButtons();
   }
   S.EndVerticalLay();

   dlg.Layout();
   dlg.Fit();
   dlg.CenterOnParent();
   if (dlg.ShowModal() == wxID_CANCEL) {
      return;
   }

   gPrefs->Write(wxT("/GUI/LabelFontFacename"), lb->GetStringSelection());
   gPrefs->Write(wxT("/GUI/LabelFontSize"), sc->GetValue());
   gPrefs->Flush();

   LabelTrack::ResetFont();

   Refresh(false);
}

/// Determines which track is under the mouse
///  @param mouseX - mouse X position.
///  @param mouseY - mouse Y position.
///  @param label  - true iff the X Y position is relative to side-panel with the labels in it.
///  @param link - true iff we should consider a hit in any linked track as a hit.
///  @param *trackRect - returns track rectangle.
Track *TrackPanel::FindTrack(int mouseX, int mouseY, bool label, bool link,
                              wxRect * trackRect)
{
   // If label is true, resulting rectangle OMITS left and top insets.
   // If label is false, resulting rectangle INCLUDES right and top insets.

   wxRect rect;
   rect.x = 0;
   rect.y = -mViewInfo->vpos;
   rect.y += kTopInset;
   GetSize(&rect.width, &rect.height);

   if (label) {
      rect.width = GetLeftOffset();
   } else {
      rect.x = GetLeftOffset();
      rect.width -= GetLeftOffset();
   }

   VisibleTrackIterator iter(GetProject());
   for (Track * t = iter.First(); t; t = iter.Next()) {
      rect.y = t->GetY() - mViewInfo->vpos + kTopInset;
      rect.height = t->GetHeight();

      if (link && t->GetLink()) {
         Track *l = t->GetLink();
         int h = l->GetHeight();
         if (!t->GetLinked()) {
            t = l;
            rect.y = t->GetY() - mViewInfo->vpos + kTopInset;
         }
         rect.height += h;
      }
#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
      else if(link && MONO_WAVE_PAN(t))
      {
         rect.height += t->GetHeight(true);
      }
#endif
      //Determine whether the mouse is inside
      //the current rectangle.  If so, recalculate
      //the proper dimensions and return.
      if (rect.Contains(mouseX, mouseY)) {
#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
         t->SetVirtualStereo(false);
#endif
         if (trackRect) {
            rect.y -= kTopInset;
            if (label) {
               rect.x += kLeftInset;
               rect.width -= kLeftInset;
               rect.y += kTopInset;
               rect.height -= kTopInset;
            }
            *trackRect = rect;
         }

         return t;
      }
#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
      if(!link && MONO_WAVE_PAN(t)){
         rect.y = t->GetY(true) - mViewInfo->vpos + kTopInset;
         rect.height = t->GetHeight(true);
         if (rect.Contains(mouseX, mouseY)) {
            t->SetVirtualStereo(true);
            if (trackRect) {
               rect.y -= kTopInset;
               if (label) {
                  rect.x += kLeftInset;
                  rect.width -= kLeftInset;
                  rect.y += kTopInset;
                  rect.height -= kTopInset;
               }
               *trackRect = rect;
            }
            return t;
         }
      }
#endif // EXPERIMENTAL_OUTPUT_DISPLAY
   }

   return NULL;
}

/// This finds the rectangle of a given track, either the
/// of the label 'adornment' or the track itself
wxRect TrackPanel::FindTrackRect(Track * target, bool label)
{
   if (!target) {
      return wxRect(0,0,0,0);
   }

   wxRect rect(0,
            target->GetY() - mViewInfo->vpos,
            GetSize().GetWidth(),
            target->GetHeight());

   // The check for a null linked track is necessary because there's
   // a possible race condition between the time the 2 linked tracks
   // are added and when wxAccessible methods are called.  This is
   // most evident when using Jaws.
   if (target->GetLinked() && target->GetLink()) {
      rect.height += target->GetLink()->GetHeight();
   }

   if (label) {
      rect.x += kLeftInset;
      rect.width -= kLeftInset;
      rect.y += kTopInset;
      rect.height -= kTopInset;
   }

   return rect;
}

int TrackPanel::GetVRulerWidth() const
{
   return vrulerSize.x;
}

/// Displays the bounds of the selection in the status bar.
void TrackPanel::DisplaySelection()
{
   if (!mListener)
      return;

   // DM: Note that the Selection Bar can actually MODIFY the selection
   // if snap-to mode is on!!!
   mListener->TP_DisplaySelection();
}

Track *TrackPanel::GetFocusedTrack()
{
   return mAx->GetFocus();
}

void TrackPanel::SetFocusedTrack( Track *t )
{
   // Make sure we always have the first linked track of a stereo track
   if (t && !t->GetLinked() && t->GetLink())
      t = (WaveTrack*)t->GetLink();

   if (t && AudacityProject::GetKeyboardCaptureHandler()) {
      AudacityProject::ReleaseKeyboard(this);
   }

   if (t) {
      AudacityProject::CaptureKeyboard(this);
   }

   mAx->SetFocus( t );
   Refresh( false );
}

void TrackPanel::OnSetFocus(wxFocusEvent & WXUNUSED(event))
{
   SetFocusedTrack( GetFocusedTrack() );
   Refresh( false );
}

void TrackPanel::OnKillFocus(wxFocusEvent & WXUNUSED(event))
{
   if (AudacityProject::HasKeyboardCapture(this))
   {
      AudacityProject::ReleaseKeyboard(this);
   }
   Refresh( false);
}

/**********************************************************************

  TrackInfo code is destined to move out of this file.
  Code should become a lot cleaner when we have sizers.

**********************************************************************/

TrackInfo::TrackInfo(TrackPanel * pParentIn)
{
   pParent = pParentIn;

   wxRect rect(0, 0, 1000, 1000);
   wxRect sliderRect;

   GetGainRect(rect, sliderRect);

   /* i18n-hint: Title of the Gain slider, used to adjust the volume */
   mGain = std::make_unique<LWSlider>(pParent, _("Gain"),
                        wxPoint(sliderRect.x, sliderRect.y),
                        wxSize(sliderRect.width, sliderRect.height),
                        DB_SLIDER);
   mGain->SetDefaultValue(1.0);
   mGainCaptured = std::make_unique<LWSlider>(pParent, _("Gain"),
                                wxPoint(sliderRect.x, sliderRect.y),
                                wxSize(sliderRect.width, sliderRect.height),
                                DB_SLIDER);
   mGainCaptured->SetDefaultValue(1.0);

   GetPanRect(rect, sliderRect);

   /* i18n-hint: Title of the Pan slider, used to move the sound left or right */
   mPan = std::make_unique<LWSlider>(pParent, _("Pan"),
                       wxPoint(sliderRect.x, sliderRect.y),
                       wxSize(sliderRect.width, sliderRect.height),
                       PAN_SLIDER);
   mPan->SetDefaultValue(0.0);
   mPanCaptured = std::make_unique<LWSlider>(pParent, _("Pan"),
                               wxPoint(sliderRect.x, sliderRect.y),
                               wxSize(sliderRect.width, sliderRect.height),
                               PAN_SLIDER);
   mPanCaptured->SetDefaultValue(0.0);

   UpdatePrefs();
}

TrackInfo::~TrackInfo()
{
}

int TrackInfo::GetTrackInfoWidth() const
{
   return kTrackInfoWidth;
}

void TrackInfo::GetCloseBoxRect(const wxRect & rect, wxRect & dest) const
{
   dest.x = rect.x;
   dest.y = rect.y;
   dest.width = kTrackInfoBtnSize;
   dest.height = kTrackInfoBtnSize;
}

void TrackInfo::GetTitleBarRect(const wxRect & rect, wxRect & dest) const
{
   dest.x = rect.x + kTrackInfoBtnSize; // to right of CloseBoxRect
   dest.y = rect.y;
   dest.width = kTrackInfoWidth - rect.x - kTrackInfoBtnSize; // to right of CloseBoxRect
   dest.height = kTrackInfoBtnSize;
}

void TrackInfo::GetMuteSoloRect(const wxRect & rect, wxRect & dest, bool solo, bool bHasSoloButton) const
{
   dest.x = rect.x ;
   dest.y = rect.y + 50;
   dest.width = 48;
   dest.height = kTrackInfoBtnSize;

   if( !bHasSoloButton )
   {
      dest.width +=48;
   }
   else if (solo)
   {
      dest.x += 48;
   }
}

void TrackInfo::GetGainRect(const wxRect & rect, wxRect & dest) const
{
   dest.x = rect.x + 7;
   dest.y = rect.y + 70;
   dest.width = 84;
   dest.height = 25;
}

void TrackInfo::GetPanRect(const wxRect & rect, wxRect & dest) const
{
   dest.x = rect.x + 7;
   dest.y = rect.y + 100;
   dest.width = 84;
   dest.height = 25;
}

void TrackInfo::GetMinimizeRect(const wxRect & rect, wxRect &dest) const
{
   const int kBlankWidth = kTrackInfoBtnSize + 4;
   dest.x = rect.x + kBlankWidth;
   dest.y = rect.y + rect.height - 19;
   // Width is kTrackInfoWidth less space on left for track select and on right for sync-lock icon.
   dest.width = kTrackInfoWidth - (2 * kBlankWidth);
   dest.height = kTrackInfoBtnSize;
}

void TrackInfo::GetSyncLockIconRect(const wxRect & rect, wxRect &dest) const
{
   dest.x = rect.x + kTrackInfoWidth - kTrackInfoBtnSize - 4; // to right of minimize button
   dest.y = rect.y + rect.height - 19;
   dest.width = kTrackInfoBtnSize;
   dest.height = kTrackInfoBtnSize;
}


/// \todo Probably should move to 'Utils.cpp'.
void TrackInfo::SetTrackInfoFont(wxDC * dc) const
{
   dc->SetFont(mFont);
}

void TrackInfo::DrawBordersWithin(wxDC* dc, const wxRect & rect, bool bHasMuteSolo) const
{
   AColor::Dark(dc, false); // same color as border of toolbars (ToolBar::OnPaint())

   // below close box and title bar
   AColor::Line(*dc, rect.x, rect.y + kTrackInfoBtnSize, kTrackInfoWidth, rect.y + kTrackInfoBtnSize);

   // between close box and title bar
   AColor::Line(*dc, rect.x + kTrackInfoBtnSize, rect.y, rect.x + kTrackInfoBtnSize, rect.y + kTrackInfoBtnSize);

   if( bHasMuteSolo && (rect.height > (66+18) ))
   {
      AColor::Line(*dc, rect.x, rect.y + 50, kTrackInfoWidth, rect.y + 50);   // above mute/solo
      AColor::Line(*dc, rect.x + 48 , rect.y + 50, rect.x + 48, rect.y + 66);    // between mute/solo
      AColor::Line(*dc, rect.x, rect.y + 66, kTrackInfoWidth, rect.y + 66);   // below mute/solo
   }

   // left of and above minimize button
   wxRect minimizeRect;
   this->GetMinimizeRect(rect, minimizeRect);
   AColor::Line(*dc, minimizeRect.x - 1, minimizeRect.y,
                  minimizeRect.x - 1, minimizeRect.y + minimizeRect.height);
   AColor::Line(*dc, minimizeRect.x, minimizeRect.y - 1,
                  minimizeRect.x + minimizeRect.width, minimizeRect.y - 1);
}

//#define USE_BEVELS

#ifdef USE_BEVELS
void TrackInfo::DrawBackground(wxDC * dc, const wxRect & rect, bool bSelected,
   bool bHasMuteSolo, const int labelw, const int vrul) const
#else
void TrackInfo::DrawBackground(wxDC * dc, const wxRect & rect, bool bSelected,
   bool WXUNUSED(bHasMuteSolo), const int labelw, const int WXUNUSED(vrul)) const
#endif
{
   // fill in label
   wxRect fill = rect;
   fill.width = labelw-4;
   AColor::MediumTrackInfo(dc, bSelected);
   dc->DrawRectangle(fill);


#ifdef USE_BEVELS
   if( bHasMuteSolo )
   {
      int ylast = rect.height-20;
      int ybutton = wxMin(32,ylast-17);
      int ybuttonEnd = 67;

      fill=wxRect( rect.x+1, rect.y+17, vrul-6, ybutton);
      AColor::BevelTrackInfo( *dc, true, fill );
   
      if( ybuttonEnd < ylast ){
         fill=wxRect( rect.x+1, rect.y+ybuttonEnd, fill.width, ylast - ybuttonEnd);
         AColor::BevelTrackInfo( *dc, true, fill );
      }
   }
   else
   {
      fill=wxRect( rect.x+1, rect.y+17, vrul-6, rect.height-37);
      AColor::BevelTrackInfo( *dc, true, fill );
   }
#endif
}

void TrackInfo::GetTrackControlsRect(const wxRect & rect, wxRect & dest) const
{
   wxRect top;
   wxRect bot;

   GetTitleBarRect(rect, top);
   GetMinimizeRect(rect, bot);

   dest.x = rect.x;
   dest.width = kTrackInfoWidth - dest.x;
   dest.y = top.GetBottom() + 2; // BUG
   dest.height = bot.GetTop() - top.GetBottom() - 2;
}


void TrackInfo::DrawCloseBox(wxDC * dc, const wxRect & rect, bool down) const
{
   wxRect bev;
   GetCloseBoxRect(rect, bev);

#ifdef EXPERIMENTAL_THEMING
   wxPen pen( theTheme.Colour( clrTrackPanelText ));
   dc->SetPen( pen );
#else
   dc->SetPen(*wxBLACK_PEN);
#endif

   // Draw the "X"
   const int s = 6;

   int ls = bev.x + ((bev.width - s) / 2);
   int ts = bev.y + ((bev.height - s) / 2);
   int rs = ls + s;
   int bs = ts + s;

   AColor::Line(*dc, ls,     ts, rs,     bs);
   AColor::Line(*dc, ls + 1, ts, rs + 1, bs);
   AColor::Line(*dc, rs,     ts, ls,     bs);
   AColor::Line(*dc, rs + 1, ts, ls + 1, bs);

   bev.Inflate(-1, -1);
   AColor::BevelTrackInfo(*dc, !down, bev);
}

void TrackInfo::DrawTitleBar(wxDC * dc, const wxRect & rect, Track * t,
                              bool down) const
{
   wxRect bev;
   GetTitleBarRect(rect, bev);
   bev.Inflate(-1, -1);

   // Draw title text
   SetTrackInfoFont(dc);
   wxString titleStr = t->GetName();
   int allowableWidth = kTrackInfoWidth - 38 - kLeftInset;

   wxCoord textWidth, textHeight;
   dc->GetTextExtent(titleStr, &textWidth, &textHeight);
   while (textWidth > allowableWidth) {
      titleStr = titleStr.Left(titleStr.Length() - 1);
      dc->GetTextExtent(titleStr, &textWidth, &textHeight);
   }

   // wxGTK leaves little scraps (antialiasing?) of the
   // characters if they are repeatedly drawn.  This
   // happens when holding down mouse button and moving
   // in and out of the title bar.  So clear it first.
   AColor::MediumTrackInfo(dc, t->GetSelected());
   dc->DrawRectangle(bev);
   dc->DrawText(titleStr, bev.x + 2, bev.y + (bev.height - textHeight) / 2);

   // Pop-up triangle
#ifdef EXPERIMENTAL_THEMING
   wxColour c = theTheme.Colour( clrTrackPanelText );
#else
   wxColour c = *wxBLACK;
#endif

   dc->SetPen(c);
   dc->SetBrush(c);

   int s = 10; // Width of dropdown arrow...height is half of width
   AColor::Arrow(*dc,
                 bev.GetRight() - s - 3, // 3 to offset from right border
                 bev.y + ((bev.height - (s / 2)) / 2),
                 s);

   AColor::BevelTrackInfo(*dc, !down, bev);
}

/// Draw the Mute or the Solo button, depending on the value of solo.
void TrackInfo::DrawMuteSolo(wxDC * dc, const wxRect & rect, Track * t,
                              bool down, bool solo, bool bHasSoloButton) const
{
   wxRect bev;
   if( solo && !bHasSoloButton )
      return;
   GetMuteSoloRect(rect, bev, solo, bHasSoloButton);
   bev.Inflate(-1, -1);

   if (bev.y + bev.height >= rect.y + rect.height - 19)
      return; // don't draw mute and solo buttons, because they don't fit into track label

   AColor::MediumTrackInfo( dc, t->GetSelected());
   if( solo )
   {
      if( t->GetSolo() )
      {
         AColor::Solo(dc, t->GetSolo(), t->GetSelected());
      }
   }
   else
   {
      if( t->GetMute() )
      {
         AColor::Mute(dc, t->GetMute(), t->GetSelected(), t->GetSolo());
      }
   }
   //(solo) ? AColor::Solo(dc, t->GetSolo(), t->GetSelected()) :
   //    AColor::Mute(dc, t->GetMute(), t->GetSelected(), t->GetSolo());
   dc->SetPen( *wxTRANSPARENT_PEN );//No border!
   dc->DrawRectangle(bev);

   wxCoord textWidth, textHeight;
   wxString str = (solo) ?
      /* i18n-hint: This is on a button that will silence this track.*/
      _("Solo") :
      /* i18n-hint: This is on a button that will silence all the other tracks.*/
      _("Mute");

   SetTrackInfoFont(dc);
   dc->GetTextExtent(str, &textWidth, &textHeight);
   dc->DrawText(str, bev.x + (bev.width - textWidth) / 2, bev.y + (bev.height - textHeight) / 2);

   AColor::BevelTrackInfo(*dc, (solo?t->GetSolo():t->GetMute()) == down, bev);

   if (solo && !down) {
      // Update the mute button, which may be grayed out depending on
      // the state of the solo button.
      DrawMuteSolo(dc, rect, t, false, false, bHasSoloButton);
   }
}

// Draw the minimize button *and* the sync-lock track icon, if necessary.
void TrackInfo::DrawMinimize(wxDC * dc, const wxRect & rect, Track * t, bool down) const
{
   wxRect bev;
   GetMinimizeRect(rect, bev);

   // Clear background to get rid of previous arrow
   AColor::MediumTrackInfo(dc, t->GetSelected());
   dc->DrawRectangle(bev);

#ifdef EXPERIMENTAL_THEMING
   wxColour c = theTheme.Colour(clrTrackPanelText);
   dc->SetBrush(c);
   dc->SetPen(c);
#else
   AColor::Dark(dc, t->GetSelected());
#endif

   AColor::Arrow(*dc,
                 bev.x - 5 + bev.width / 2,
                 bev.y - 2 + bev.height / 2,
                 10,
                 t->GetMinimized());

   AColor::BevelTrackInfo(*dc, !down, bev);
}

#ifdef EXPERIMENTAL_MIDI_OUT
void TrackInfo::DrawVelocitySlider(wxDC *dc, NoteTrack *t, wxRect rect) const
{
    wxRect gainRect;
    int index = t->GetIndex();

    //EnsureSufficientSliders(index);

    GetGainRect(rect, gainRect);
    if (gainRect.y + gainRect.height < rect.y + rect.height - 19) {
       auto &gain = mGain; // mGains[index];
       gain->SetStyle(VEL_SLIDER);
       GainSlider(index)->Move(wxPoint(gainRect.x, gainRect.y));
       GainSlider(index)->Set(t->GetGain());
       GainSlider(index)->OnPaint(*dc
          // , t->GetSelected()
       );
    }
}
#endif

void TrackInfo::DrawSliders(wxDC *dc, WaveTrack *t, wxRect rect, bool captured) const
{
   wxRect sliderRect;

   GetGainRect(rect, sliderRect);
   if (sliderRect.y + sliderRect.height < rect.y + rect.height - 19) {
      GainSlider(t, captured)->OnPaint(*dc);
   }

   GetPanRect(rect, sliderRect);
   if (sliderRect.y + sliderRect.height < rect.y + rect.height - 19) {
      PanSlider(t, captured)->OnPaint(*dc);
   }
}

LWSlider * TrackInfo::GainSlider(WaveTrack *t, bool captured) const
{
   wxRect rect(kLeftInset, t->GetY() - pParent->GetViewInfo()->vpos + kTopInset, 1, t->GetHeight());
   wxRect sliderRect;
   GetGainRect(rect, sliderRect);

   wxPoint pos = sliderRect.GetPosition();
   float gain = t->GetGain();

   mGain->Move(pos);
   mGain->Set(gain);
   mGainCaptured->Move(pos);
   mGainCaptured->Set(gain);

   return (captured ? mGainCaptured : mGain).get();
}

LWSlider * TrackInfo::PanSlider(WaveTrack *t, bool captured) const
{
   wxRect rect(kLeftInset, t->GetY() - pParent->GetViewInfo()->vpos + kTopInset, 1, t->GetHeight());
   wxRect sliderRect;
   GetPanRect(rect, sliderRect);

   wxPoint pos = sliderRect.GetPosition();
   float pan = t->GetPan();

   mPan->Move(pos);
   mPan->Set(pan);
   mPanCaptured->Move(pos);
   mPanCaptured->Set(pan);

   return (captured ? mPanCaptured : mPan).get();
}

void TrackInfo::UpdatePrefs()
{
   // Calculation of best font size depends on language, so it should be redone in case
   // the language preference changed.

   int fontSize = 10;
   mFont.Create(fontSize, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL);

   int allowableWidth = GetTrackInfoWidth() - 2; // 2 to allow for left/right borders
   int textWidth, textHeight;
   do {
      mFont.SetPointSize(fontSize);
      pParent->GetTextExtent(_("Stereo, 999999Hz"),
                             &textWidth,
                             &textHeight,
                             NULL,
                             NULL,
                             &mFont);
      fontSize--;
   } while (textWidth >= allowableWidth);
}

TrackPanelCellIterator::TrackPanelCellIterator(TrackPanel *trackPanel, bool begin)
   : mPanel(trackPanel)
   , mIter(trackPanel->GetProject())
   , mpCell(begin ? mIter.First() : NULL)
{
}

TrackPanelCellIterator &TrackPanelCellIterator::operator++ ()
{
   mpCell = mIter.Next();
   return *this;
}

TrackPanelCellIterator TrackPanelCellIterator::operator++ (int)
{
   TrackPanelCellIterator copy(*this);
   ++ *this;
   return copy;
}

auto TrackPanelCellIterator::operator* () const -> value_type
{
   if (!mpCell)
      return std::make_pair((Track*)nullptr, wxRect());

   // Convert virtual coordinate to physical
   int width;
   mPanel->GetTracksUsableArea(&width, NULL);
   int y = mpCell->GetY() - mPanel->GetViewInfo()->vpos;
   return std::make_pair(
      mpCell,
      wxRect(
         mPanel->GetLeftOffset(),
         y + kTopMargin,
         width,
         mpCell->GetHeight() - (kTopMargin + kBottomMargin)
      )
   );
}

static TrackPanel * TrackPanelFactory(wxWindow * parent,
   wxWindowID id,
   const wxPoint & pos,
   const wxSize & size,
   const std::shared_ptr<TrackList> &tracks,
   ViewInfo * viewInfo,
   TrackPanelListener * listener,
   AdornedRulerPanel * ruler)
{
   wxASSERT(parent); // to justify safenew
   return safenew TrackPanel(
      parent,
      id,
      pos,
      size,
      tracks,
      viewInfo,
      listener,
      ruler);
}


// Declare the static factory function.
// We defined it in the class.
TrackPanel *(*TrackPanel::FactoryFunction)(
              wxWindow * parent,
              wxWindowID id,
              const wxPoint & pos,
              const wxSize & size,
              const std::shared_ptr<TrackList> &tracks,
              ViewInfo * viewInfo,
              TrackPanelListener * listener,
              AdornedRulerPanel * ruler) = TrackPanelFactory;

