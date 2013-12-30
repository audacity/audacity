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
// put the following lengthy description of refactoring on a new page 
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

\class TrackClip
\brief One clip (i.e short section) of a WaveTrack.

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
   || TrackInfo | +-----------------------------------+|
   ||            | +-----------------------------------+|
   ||            | | (R)  GuiWaveTrack                 ||
   |+------------+ +-----------------------------------+|
   +-------- GuiStereoTrack ----------------------------+
   +----------------------------------------------------+
   |+------------+ +-----------------------------------+|
   ||            | | (L)  GuiWaveTrack                 ||
   || TrackInfo | +-----------------------------------+|
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
  Have deliberately not created new files for the new classes 
  such as AdornedRulerPanel and TrackInfo - yet.

*//*****************************************************************/


#include "Audacity.h"
#include "Experimental.h"
#include "TrackPanel.h"

#include <math.h>
#include <stdlib.h>

//#define DEBUG_DRAW_TIMING 1

#include <wx/combobox.h>
#include <wx/dcclient.h>
#include <wx/dcbuffer.h>
#include <wx/dcmemory.h>
#include <wx/font.h>
#include <wx/fontenum.h>
#include <wx/log.h>
#include <wx/menu.h>
#include <wx/msgdlg.h>
#include <wx/textdlg.h>
#include <wx/numdlg.h>
#include <wx/choicdlg.h>
#include <wx/spinctrl.h>
#include <wx/listbox.h>
#include <wx/textctrl.h>
#include <wx/intl.h>
#include <wx/image.h>

#include "AColor.h"
#include "AllThemeResources.h"
#include "AudacityApp.h"
#include "AudioIO.h"
#include "Envelope.h"
#include "Experimental.h"
#include "float_cast.h"
#include "Internat.h"
#include "LabelTrack.h"
#include "Lyrics.h"
#include "LyricsWindow.h"
#include "MixerBoard.h"

#include "NoteTrack.h"
#include "Prefs.h"
#include "Project.h"
#include "Snap.h"
#include "Theme.h"
#include "TimeTrack.h"
#include "Track.h"
#include "TrackArtist.h"
#include "TrackPanelAx.h"
#include "ViewInfo.h"
#include "WaveTrack.h"

#include "ondemand/ODManager.h"

#include "toolbars/ControlToolBar.h"
#include "toolbars/ToolManager.h"
#include "toolbars/ToolsToolBar.h"

#include "widgets/ASlider.h"
#include "widgets/Ruler.h"
#include "widgets/TimeTextCtrl.h"

#include <wx/arrimpl.cpp>

#define ZOOMLIMIT 0.001

WX_DEFINE_OBJARRAY(TrackClipArray);

//This loads the appropriate set of cursors, depending on platform.
#include "../images/Cursors.h"
#include <iostream>

#ifdef _DEBUG
    #ifdef _MSC_VER
        #undef THIS_FILE
        static char*THIS_FILE= __FILE__;
        #define new new(_NORMAL_BLOCK, THIS_FILE, __LINE__)
    #endif
#endif

#define kLeftInset 4
#define kTopInset 4
#define kTimerInterval 50 // milliseconds

// Is the distance between A and B less than D?
template < class A, class B, class DIST > bool within(A a, B b, DIST d)
{
   return (a > b - d) && (a < b + d);
}

template < class LOW, class MID, class HIGH >
    bool between_inclusive(LOW l, MID m, HIGH h)
{
   return (m >= l && m <= h);
}

template < class LOW, class MID, class HIGH >
    bool between_exclusive(LOW l, MID m, HIGH h)
{
   return (m > l && m < h);
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
   OnSpectrumLogID,
   OnPitchID,

   OnSplitStereoID,
   OnSplitStereoMonoID,
   OnMergeStereoID,
   OnSwapChannelsID,

   OnSetTimeTrackRangeID,
   OnCutSelectedTextID,
   OnCopySelectedTextID,
   OnPasteSelectedTextID,

   OnTimeTrackLinID,
   OnTimeTrackLogID,
   OnTimeTrackLogIntID,
};

BEGIN_EVENT_TABLE(TrackPanel, wxWindow)
    EVT_MOUSE_EVENTS(TrackPanel::OnMouseEvent)
    EVT_MOUSE_CAPTURE_LOST(TrackPanel::OnCaptureLost)
    EVT_COMMAND(wxID_ANY, EVT_CAPTURE_KEY, TrackPanel::OnCaptureKey)
    EVT_KEY_DOWN(TrackPanel::OnKeyDown)
    EVT_CHAR(TrackPanel::OnChar)
    EVT_SIZE(TrackPanel::OnSize)
    EVT_ERASE_BACKGROUND(TrackPanel::OnErase)
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
    EVT_MENU_RANGE(OnWaveformID, OnPitchID, TrackPanel::OnSetDisplay)
    EVT_MENU_RANGE(OnRate8ID, OnRate384ID, TrackPanel::OnRateChange)
    EVT_MENU_RANGE(On16BitID, OnFloatID, TrackPanel::OnFormatChange)
    EVT_MENU(OnRateOtherID, TrackPanel::OnRateOther)
    EVT_MENU(OnSwapChannelsID, TrackPanel::OnSwapChannels)
    EVT_MENU(OnSplitStereoID, TrackPanel::OnSplitStereo)
    EVT_MENU(OnSplitStereoMonoID, TrackPanel::OnSplitStereoMono)
    EVT_MENU(OnMergeStereoID, TrackPanel::OnMergeStereo)

    EVT_MENU(OnCutSelectedTextID, TrackPanel::OnCutSelectedText)
    EVT_MENU(OnCopySelectedTextID, TrackPanel::OnCopySelectedText)
    EVT_MENU(OnPasteSelectedTextID, TrackPanel::OnPasteSelectedText)

    EVT_MENU(OnTimeTrackLinID, TrackPanel::OnTimeTrackLin)
    EVT_MENU(OnTimeTrackLogID, TrackPanel::OnTimeTrackLog)
    EVT_MENU(OnTimeTrackLogIntID, TrackPanel::OnTimeTrackLogInt)
END_EVENT_TABLE()

/// Makes a cursor from an XPM, uses CursorId as a fallback.
static wxCursor * MakeCursor( int WXUNUSED(CursorId), const char * pXpm[36],  int HotX, int HotY )
{
   wxCursor * pCursor;

#ifdef CURSORS_SIZE32
   const int HotAdjust =0;
#else
   const int HotAdjust =8;
#endif

   wxImage Image = wxImage(wxBitmap(pXpm).ConvertToImage());
   Image.SetMaskColour(255,0,0);
   Image.SetMask();// Enable mask.

#ifdef __WXGTK__
   //
   // Kludge: the wxCursor Image constructor is broken in wxGTK.
   // This code, based loosely on the broken code from the wxGTK source,
   // works around the problem by constructing a 1-bit bitmap and
   // calling the other custom cursor constructor.
   //
   // -DMM
   //

   unsigned char *rgbBits = Image.GetData();
   int w = Image.GetWidth() ;
   int h = Image.GetHeight();
   int imagebitcount = (w*h)/8;

   unsigned char *bits = new unsigned char [imagebitcount];
   unsigned char *maskBits = new unsigned char [imagebitcount];

   int i, j, i8;
   unsigned char cMask;
   for (i=0; i<imagebitcount; i++) {
      bits[i] = 0;
      i8 = i * 8;

      cMask = 1;
      for (j=0; j<8; j++) {
         if (rgbBits[(i8+j)*3+2] < 127)
            bits[i] = bits[i] | cMask;
         cMask = cMask * 2;
      }
   }

   for (i=0; i<imagebitcount; i++) {
      maskBits[i] = 0x0;
      i8 = i * 8;

      cMask = 1;
      for (j=0; j<8; j++) {
         if (rgbBits[(i8+j)*3] < 127 || rgbBits[(i8+j)*3+1] > 127)
            maskBits[i] = maskBits[i] | cMask;
         cMask = cMask * 2;
      }
   }

   pCursor = new wxCursor((const char *)bits, w, h,
                          HotX-HotAdjust, HotY-HotAdjust,
                          (const char *)maskBits);

   delete [] bits;
   delete [] maskBits;

#else
   Image.SetOption( wxIMAGE_OPTION_CUR_HOTSPOT_X, HotX-HotAdjust );
   Image.SetOption( wxIMAGE_OPTION_CUR_HOTSPOT_Y, HotY-HotAdjust );
   pCursor = new wxCursor( Image );
#endif

   return pCursor;
}



// Don't warn us about using 'this' in the base member initializer list.
#ifndef __WXGTK__ //Get rid if this pragma for gtk
#pragma warning( disable: 4355 )
#endif
TrackPanel::TrackPanel(wxWindow * parent, wxWindowID id,
                       const wxPoint & pos,
                       const wxSize & size,
                       TrackList * tracks,
                       ViewInfo * viewInfo,
                       TrackPanelListener * listener,
                       AdornedRulerPanel * ruler)
   : wxPanel(parent, id, pos, size, wxWANTS_CHARS | wxNO_BORDER),
     mTrackInfo(this),
     mListener(listener),
     mTracks(tracks),
     mViewInfo(viewInfo),
     mRuler(ruler),
     mTrackArtist(NULL),
     mBacking(NULL),
     mRefreshBacking(false),
     mAutoScrolling(false),
     mVertScrollRemainder(0),
     vrulerSize(36,0)
#ifndef __WXGTK__   //Get rid if this pragma for gtk
#pragma warning( default: 4355 )
#endif
{
   SetLabel(_("Track Panel"));
   SetName(_("Track Panel"));

   mAx = new TrackPanelAx( this );
#if wxUSE_ACCESSIBILITY
   SetAccessible( mAx );
#endif
   mMouseCapture = IsUncaptured;
   mSlideUpDownOnly = false;
   mLabelTrackStartXPos=-1;
   mCircularTrackNavigation = false;

   UpdatePrefs();

   mRedrawAfterStop = false;
   mIndicatorShowing = false;

   mPencilCursor  = MakeCursor( wxCURSOR_PENCIL,    DrawCursorXpm,    12, 22);
   mSelectCursor  = MakeCursor( wxCURSOR_IBEAM,     IBeamCursorXpm,   17, 16);
   mEnvelopeCursor= MakeCursor( wxCURSOR_ARROW,     EnvCursorXpm,     16, 16);
   mDisabledCursor= MakeCursor( wxCURSOR_NO_ENTRY,  DisabledCursorXpm,16, 16);
   mSlideCursor   = MakeCursor( wxCURSOR_SIZEWE,    TimeCursorXpm,    16, 16);
   mZoomInCursor  = MakeCursor( wxCURSOR_MAGNIFIER, ZoomInCursorXpm,  19, 15);
   mZoomOutCursor = MakeCursor( wxCURSOR_MAGNIFIER, ZoomOutCursorXpm, 19, 15);
   mLabelCursorLeft  = MakeCursor( wxCURSOR_ARROW,  LabelCursorLeftXpm, 19, 15);
   mLabelCursorRight = MakeCursor( wxCURSOR_ARROW,  LabelCursorRightXpm, 16, 16);
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

   mArrowCursor = new wxCursor(wxCURSOR_ARROW);
   mSmoothCursor = new wxCursor(wxCURSOR_SPRAYCAN);
   mResizeCursor = new wxCursor(wxCURSOR_SIZENS);
   mRearrangeCursor = new wxCursor(wxCURSOR_HAND);
   mAdjustLeftSelectionCursor = new wxCursor(wxCURSOR_POINT_LEFT);
   mAdjustRightSelectionCursor = new wxCursor(wxCURSOR_POINT_RIGHT);

   mWaveTrackMenu = NULL;
   mNoteTrackMenu = NULL;
   mLabelTrackMenu = NULL;
   mLabelTrackInfoMenu = NULL;
   mTimeTrackMenu = NULL;

   BuildMenus();

   mTrackArtist = new TrackArtist();
   mTrackArtist->SetInset(1, kTopInset + 1, kLeftInset + 2, 2);

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

   //Initialize the last selection adjustment time.
   mLastSelectionAdjustment = ::wxGetLocalTimeMillis();

   // This is used to snap the cursor to the nearest track that
   // lines up with it.
   mSnapManager = NULL;
   mSnapLeft = -1;
   mSnapRight = -1;

   mLastCursor = -1;
   mLastIndicator = -1;

   // Register for tracklist updates
   mTracks->Connect(EVT_TRACKLIST_RESIZED,
                    wxCommandEventHandler(TrackPanel::OnTrackListResized),
                    NULL,
                    this);
   mTracks->Connect(EVT_TRACKLIST_UPDATED,
                    wxCommandEventHandler(TrackPanel::OnTrackListUpdated),
                    NULL,
                    this);
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

   if (mBacking)
   {
      mBackingDC.SelectObject( wxNullBitmap );
      delete mBacking;
   }
   delete mTrackArtist;

   delete mArrowCursor;
   delete mPencilCursor;
   delete mSelectCursor;
   delete mEnvelopeCursor;
   delete mDisabledCursor;
   delete mSlideCursor;
   delete mResizeCursor;
   delete mSmoothCursor;
   delete mZoomInCursor;
   delete mZoomOutCursor;
   delete mLabelCursorLeft;
   delete mLabelCursorRight;
   delete mRearrangeCursor;
   delete mAdjustLeftSelectionCursor;
   delete mAdjustRightSelectionCursor;
#if USE_MIDI
   delete mStretchCursor;
   delete mStretchLeftCursor;
   delete mStretchRightCursor;
#endif

   delete mSnapManager;

   DeleteMenus();

#if !wxUSE_ACCESSIBILITY
   delete mAx;
#endif
}

void TrackPanel::BuildMenus(void)
{
   // Get rid of existing menus
   DeleteMenus();

   // Use AppendCheckItem so we can have ticks beside the items.
   // We would use AppendRadioItem but it only currently works on windows and GTK.
   mRateMenu = new wxMenu();
   mRateMenu->AppendCheckItem(OnRate8ID, wxT("8000 Hz"));
   mRateMenu->AppendCheckItem(OnRate11ID, wxT("11025 Hz"));
   mRateMenu->AppendCheckItem(OnRate16ID, wxT("16000 Hz"));
   mRateMenu->AppendCheckItem(OnRate22ID, wxT("22050 Hz"));
   mRateMenu->AppendCheckItem(OnRate44ID, wxT("44100 Hz"));
   mRateMenu->AppendCheckItem(OnRate48ID, wxT("48000 Hz"));
   mRateMenu->AppendCheckItem(OnRate88ID, wxT("88200 Hz"));
   mRateMenu->AppendCheckItem(OnRate96ID, wxT("96000 Hz"));
   mRateMenu->AppendCheckItem(OnRate176ID, wxT("176400 Hz"));
   mRateMenu->AppendCheckItem(OnRate192ID, wxT("192000 Hz"));
   mRateMenu->AppendCheckItem(OnRate352ID, wxT("352800 Hz"));
   mRateMenu->AppendCheckItem(OnRate384ID, wxT("384000 Hz"));
   mRateMenu->AppendCheckItem(OnRateOtherID, _("&Other..."));

   mFormatMenu = new wxMenu();
   mFormatMenu->AppendCheckItem(On16BitID, GetSampleFormatStr(int16Sample));
   mFormatMenu->AppendCheckItem(On24BitID, GetSampleFormatStr(int24Sample));
   mFormatMenu->AppendCheckItem(OnFloatID, GetSampleFormatStr(floatSample));

   /* build the pop-down menu used on wave (sampled audio) tracks */
   mWaveTrackMenu = new wxMenu();
   BuildCommonDropMenuItems(mWaveTrackMenu);	// does name, up/down etc
   mWaveTrackMenu->Append(OnWaveformID, _("Wa&veform"));
   mWaveTrackMenu->Append(OnWaveformDBID, _("&Waveform (dB)"));
   mWaveTrackMenu->Append(OnSpectrumID, _("&Spectrogram"));
   /* i18n-hint: short form of 'logarithm'*/
   mWaveTrackMenu->Append(OnSpectrumLogID, _("Spectrogram l&og(f)")); 
   mWaveTrackMenu->Append(OnPitchID, _("Pitc&h (EAC)"));
   mWaveTrackMenu->AppendSeparator();
   mWaveTrackMenu->AppendCheckItem(OnChannelMonoID, _("&Mono"));
   mWaveTrackMenu->AppendCheckItem(OnChannelLeftID, _("&Left Channel"));
   mWaveTrackMenu->AppendCheckItem(OnChannelRightID, _("&Right Channel"));
   mWaveTrackMenu->Append(OnMergeStereoID, _("Ma&ke Stereo Track"));
   mWaveTrackMenu->Append(OnSwapChannelsID, _("Swap Stereo &Channels"));
   mWaveTrackMenu->Append(OnSplitStereoID, _("Spl&it Stereo Track"));
   mWaveTrackMenu->Append(OnSplitStereoMonoID, _("Split Stereo to Mo&no"));
   mWaveTrackMenu->AppendSeparator();
   mWaveTrackMenu->Append(0, _("Set Sample &Format"), mFormatMenu);
   mWaveTrackMenu->AppendSeparator();
   mWaveTrackMenu->Append(0, _("Set Rat&e"), mRateMenu);

   /* build the pop-down menu used on note (MIDI) tracks */
   mNoteTrackMenu = new wxMenu();
   BuildCommonDropMenuItems(mNoteTrackMenu);	// does name, up/down etc
   mNoteTrackMenu->Append(OnUpOctaveID, _("Up &Octave"));
   mNoteTrackMenu->Append(OnDownOctaveID, _("Down Octa&ve"));

   /* build the pop-down menu used on label tracks */
   mLabelTrackMenu = new wxMenu();
   BuildCommonDropMenuItems(mLabelTrackMenu);	// does name, up/down etc
   mLabelTrackMenu->Append(OnSetFontID, _("&Font..."));

   /* build the pop-down menu used on time warping tracks */
   mTimeTrackMenu = new wxMenu();
   BuildCommonDropMenuItems(mTimeTrackMenu);	// does name, up/down etc
   mTimeTrackMenu->Append(OnTimeTrackLinID, _("&Linear"));
   mTimeTrackMenu->Append(OnTimeTrackLogID, _("L&ogarithmic"));
   mTimeTrackMenu->AppendSeparator();
   mTimeTrackMenu->Append(OnSetTimeTrackRangeID, _("Set Ra&nge..."));
   mTimeTrackMenu->AppendCheckItem(OnTimeTrackLogIntID, _("Logarithmic &Interpolation"));

   mLabelTrackInfoMenu = new wxMenu();
   mLabelTrackInfoMenu->Append(OnCutSelectedTextID, _("Cut"));
   mLabelTrackInfoMenu->Append(OnCopySelectedTextID, _("Copy"));
   mLabelTrackInfoMenu->Append(OnPasteSelectedTextID, _("Paste"));
}

void TrackPanel::BuildCommonDropMenuItems(wxMenu * menu)
{
   menu->Append(OnSetNameID, _("N&ame..."));
   menu->AppendSeparator();
   menu->Append(OnMoveUpID, _("Move Track &Up"));
   menu->Append(OnMoveDownID, _("Move Track &Down"));
   menu->Append(OnMoveTopID, _("Move Track to &Top"));
   menu->Append(OnMoveBottomID, _("Move Track to &Bottom"));
   menu->AppendSeparator();

}

void TrackPanel::DeleteMenus(void)
{
   // Note that the submenus (mRateMenu, ...)
   // are deleted by their parent
   if (mWaveTrackMenu) {
      delete mWaveTrackMenu;
      mWaveTrackMenu = NULL;
   }

   if (mNoteTrackMenu) {
      delete mNoteTrackMenu;
      mNoteTrackMenu = NULL;
   }

   if (mLabelTrackMenu) {
      delete mLabelTrackMenu;
      mLabelTrackMenu = NULL;
   }

   if (mLabelTrackInfoMenu) {
      delete mLabelTrackInfoMenu;
      mLabelTrackInfoMenu = NULL;
   }

   if (mTimeTrackMenu) {
      delete mTimeTrackMenu;
      mTimeTrackMenu = NULL;
   }
}

#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
void TrackPanel::UpdateVirtualStereoOrder()
{
   TrackListIterator iter(mTracks);
   Track *t;
   int temp;

   for (t = iter.First(); t; t = iter.Next()) {
      if(t->GetKind() == Track::Wave && t->GetChannel() == Track::MonoChannel){
         WaveTrack *wt = (WaveTrack*)t;

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
   mdBr = gPrefs->Read(wxT("/GUI/EnvdBRange"), ENV_DB_RANGE);
   gPrefs->Read(wxT("/GUI/AutoScroll"), &mViewInfo->bUpdateTrackIndicator,
               true);
   gPrefs->Read(wxT("/GUI/AdjustSelectionEdges"), &mAdjustSelectionEdges,
               true);
   gPrefs->Read(wxT("/GUI/CircularTrackNavigation"), &mCircularTrackNavigation,
               false);
   gPrefs->Read(wxT("/GUI/Solo"), &mSoloPref, wxT("Standard") );
   gPrefs->Read(wxT("/AudioIO/SeekShortPeriod"), &mSeekShort,
               1.0);
   gPrefs->Read(wxT("/AudioIO/SeekLongPeriod"), &mSeekLong,
               15.0);

#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
   bool temp = WaveTrack::mMonoAsVirtualStereo;
   gPrefs->Read(wxT("/GUI/MonoAsVirtualStereo"), &WaveTrack::mMonoAsVirtualStereo,
               false);

   if(WaveTrack::mMonoAsVirtualStereo != temp)
      UpdateVirtualStereoOrder();
#endif

   if (mTrackArtist) {
      mTrackArtist->UpdatePrefs();
   }

   // All vertical rulers must be recalculated since the minimum and maximum
   // frequences may have been changed.
   UpdateVRulers();
   Refresh();
}

void TrackPanel::SetStop(bool bStopped)
{
   mViewInfo->bIsPlaying = !bStopped;
   Refresh(false);
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
   TrackListIterator iter(mTracks);
   Track *t = iter.First();
   while (t) {
      t->SetSelected(false);
      if (t->GetKind() == Track::Label)
         ((LabelTrack *) t)->Unselect();
      t = iter.Next();
   }
}

/// Select all tracks marked by the label track lt
void TrackPanel::SelectTracksByLabel( LabelTrack *lt )
{
   TrackListIterator iter(mTracks);
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
      t->SetSelected( true );
      t = iter.Next();
   }
}

// Set selection length to the length of a track -- but if sync-lock is turned
// on, use the largest possible selection in the sync-lock group.  
// If it's a stereo track, do the same for the stereo channels.
void TrackPanel::SelectTrackLength(Track *t)
{
   AudacityProject *p = GetActiveProject();
   SyncLockedTracksIterator it(mTracks);
   Track *t1 = it.First(t);
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

   mViewInfo->sel0 = minOffset;
   mViewInfo->sel1 = maxEnd;
}

void TrackPanel::GetTracksUsableArea(int *width, int *height) const
{
   GetSize(width, height);
   *width -= GetLabelWidth();
   // AS: MAGIC NUMBER: What does 2 represent?
   *width -= 2 + kLeftInset;
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
void TrackPanel::OnTimer()
{
   mTimeCount++;
   // AS: If the user is dragging the mouse and there is a track that
   //  has captured the mouse, then scroll the screen, as necessary.
   if ((mMouseCapture==IsSelecting) && mCapturedTrack) {
      ScrollDuringDrag();
   }

   wxCommandEvent dummyEvent;
   AudacityProject *p = GetProject();

   if ((p->GetAudioIOToken() > 0) &&
         gAudioIO->IsStreamActive(p->GetAudioIOToken()))
   {
      // Update lyrics display. 
      LyricsWindow* pLyricsWindow = p->GetLyricsWindow();
      if (pLyricsWindow) 
      {
         Lyrics* pLyricsPanel = pLyricsWindow->GetLyricsPanel();
         pLyricsPanel->Update(gAudioIO->GetStreamTime());
      }
   }

   //v Vaughan, 2011-02-25: Moved this update back here from audacityAudioCallback.
   //    See note there. 
   // Vaughan, 2010-01-30: 
   //    Since all we're doing here is updating the meters, I moved it to 
   //    audacityAudioCallback where it calls gAudioIO->mOutputMeter->UpdateDisplay().
   MixerBoard* pMixerBoard = this->GetMixerBoard();
   if (pMixerBoard && 
         (p->GetAudioIOToken() > 0) &&
         gAudioIO->IsStreamActive(p->GetAudioIOToken()))
   {
      pMixerBoard->UpdateMeters(gAudioIO->GetStreamTime(), 
                                 (p->mLastPlayMode == loopedPlay));
   }

   // Check whether we were playing or recording, but the stream has stopped.
   if (p->GetAudioIOToken()>0 &&
         !gAudioIO->IsStreamActive(p->GetAudioIOToken())) 
   {  
      //the stream may have been started up after this one finished (by some other project)
      //in that case reset the buttons don't stop the stream
      p->GetControlToolBar()->StopPlaying(!gAudioIO->IsStreamActive());

      // Reset lyrics display. 
      LyricsWindow* pLyricsWindow = p->GetLyricsWindow();
      if (pLyricsWindow) 
      {
         Lyrics* pLyricsPanel = pLyricsWindow->GetLyricsPanel();
         pLyricsPanel->Update(p->GetSel0());
      }

      // Vaughan, 2011-01-28: No longer doing this on timer. 
      //   Now it's in AudioIO::SetMeters() and AudioIO::StopStream(), as with Meter Toolbar meters.
      //if (pMixerBoard) 
      //   pMixerBoard->ResetMeters(false);
   }

   // Next, check to see if we were playing or recording
   // audio, but now Audio I/O is completely finished.
   // The main point of this is to properly push the state
   // and flush the tracks once we've completely finished
   // recording new state.
   if (p->GetAudioIOToken()>0 &&
         !gAudioIO->IsAudioTokenActive(p->GetAudioIOToken())) 
   {
      if (gAudioIO->GetNumCaptureChannels() > 0) {
         // Tracks are buffered during recording.  This flushes
         // them so that there's nothing left in the append
         // buffers.
         TrackListIterator iter(mTracks);
         for (Track * t = iter.First(); t; t = iter.Next()) {
            if (t->GetKind() == Track::Wave) {
               ((WaveTrack *)t)->Flush();
            }
         }
         MakeParentPushState(_("Recorded Audio"), _("Record"));
         if(p->IsTimerRecordCancelled())
         {
            p->OnUndo();
            p->ResetTimerRecordFlag();
         }
      }
      mRedrawAfterStop = false;

      MakeParentRedrawScrollbars();
      p->SetAudioIOToken(0);
      p->RedrawProject();
      //ANSWER-ME: Was DisplaySelection added to solve a repaint problem?
      DisplaySelection();
   }

   // AS: The "indicator" is the little graphical mark shown in the ruler
   //  that indicates where the current play/record position is. (This also
   //  draws the moving vertical line.)
   if (!gAudioIO->IsPaused() &&
       ( mIndicatorShowing || gAudioIO->IsStreamActive(p->GetAudioIOToken())))
   {
      DrawIndicator();
   }

   if(gAudioIO->IsStreamActive(p->GetAudioIOToken()) &&
      gAudioIO->GetNumCaptureChannels()) {

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

///  This updates the indicator (on a timer tick) that shows
///  where the current play or record position is.  To do this,
///  we cheat a little.  The indicator is drawn during the ruler
///  drawing process (that should probably change, but...), so
///  we create a memory DC and tell the ruler to draw itself there,
///  and then just blit that to the screen.
///  The indicator is a small triangle, red for record, green for play.
void TrackPanel::DrawIndicator()
{
   wxClientDC dc( this );
   DoDrawIndicator( dc );
}

/// Second level DrawIndicator()
void TrackPanel::DoDrawIndicator(wxDC & dc, bool repairOld /* = false */)
{
   bool onScreen;
   int x;
   double pos;

   if (!repairOld)
   {
      // Erase the old indicator.
      if( mLastIndicator != -1 )
      {
         onScreen = between_inclusive( mViewInfo->h,
                                      mLastIndicator,
                                      mViewInfo->h + mViewInfo->screen );
         if( onScreen )
         {
            x = GetLeftOffset() + int ( ( mLastIndicator - mViewInfo->h) * mViewInfo->zoom );

            // LL:  Keep from trying to blit outsize of the source DC.  This results in a crash on
            //      OSX due to allocating memory using negative sizes and can be caused by resizing
            //      the project window while recording or playing.
            int w = dc.GetSize().GetWidth();
            if (x >= w) {
               x = w - 1;
            }

            dc.Blit( x, 0, 1, mBacking->GetHeight(), &mBackingDC, x, 0 );
         }

         // Nothing's ever perfect...
         //
         // Redraw the cursor since we may have just wiped it out
         if( mLastCursor == mLastIndicator )
         {
            DoDrawCursor( dc );
         }

         mLastIndicator = -1;
      }

      // The stream time can be < 0 if the audio is currently stopped
      pos = gAudioIO->GetStreamTime();

      AudacityProject *p = GetProject();
      bool audioActive = ( gAudioIO->IsStreamActive( p->GetAudioIOToken() ) != 0 );
      onScreen = between_inclusive( mViewInfo->h,
                                   pos,
                                   mViewInfo->h + mViewInfo->screen );

      // This displays the audio time, too...
      DisplaySelection();

      // BG: Scroll screen if option is set
      // msmeyer: But only if not playing looped or in one-second mode
      if( mViewInfo->bUpdateTrackIndicator &&
         p->mLastPlayMode != loopedPlay &&
         p->mLastPlayMode != oneSecondPlay && 
         audioActive &&
         pos >= 0 &&
         !onScreen &&
         !gAudioIO->IsPaused() )
      {
         mListener->TP_ScrollWindow( pos );
      }

      // Always update scrollbars even if not scrolling the window. This is
      // important when new audio is recorded, because this can change the
      // length of the project and therefore the appearance of the scrollbar.
      MakeParentRedrawScrollbars();

      mIndicatorShowing = ( onScreen && audioActive );

      // Remember it
      mLastIndicator = pos;
   } else {
      pos = mLastIndicator;
   }

   // Set play/record color
   bool rec = (gAudioIO->GetNumCaptureChannels() > 0);
   AColor::IndicatorColor( &dc, !rec);

   // Calculate the horizontal position of the indicator
   x = GetLeftOffset() + int ( ( pos - mViewInfo->h ) * mViewInfo->zoom );

   mRuler->DrawIndicator( pos, rec );

   // Ensure that we don't draw through the TrackInfo or vertical ruler.
   wxRect clip = GetRect();
   int leftCutoff = clip.x + GetLabelWidth();
   int rightInset = kLeftInset + 2; // See the call to SetInset
   int rightCutoff = clip.x + clip.width - rightInset;
   if (!between_inclusive(leftCutoff, x, rightCutoff))
   {
      return;
   }

   // Draw indicator in all visible tracks
   VisibleTrackIterator iter( GetProject() );
   for( Track *t = iter.First(); t; t = iter.Next() )
   {
      // Don't draw the indicator in label tracks
      if( t->GetKind() == Track::Label )
      {
         continue;
      }

      // Convert virtual coordinate to physical
      int y = t->GetY() - mViewInfo->vpos;

      // Draw the new indicator in its new location
      AColor::Line(dc,
                   x,
                   y + kTopInset + 1,
                   x,
                   y + t->GetHeight() - 3 );
   }
}

/// This method draws the cursor things, both in the
/// ruler as seen at the top of the screen, but also in each of the
/// selected tracks.
/// These are the 'vertical lines' through waves, notes, and ruler.
void TrackPanel::DrawCursor()
{
   wxClientDC dc( this );
   DoDrawCursor( dc );
}

/// Second level DrawCursor()
void TrackPanel::DoDrawCursor(wxDC & dc)
{
   bool onScreen;
   int x;

   if( mLastCursor != -1 )
   {
      onScreen = between_inclusive( mViewInfo->h,
                                    mLastCursor,
                                    mViewInfo->h + mViewInfo->screen );
      if( onScreen )
      {
         x = GetLeftOffset() + int ( ( mLastCursor - mViewInfo->h) * mViewInfo->zoom );

         dc.Blit( x, 0, 1, mBacking->GetHeight(), &mBackingDC, x, 0 );
      }

      mLastCursor = -1;
   }

   mLastCursor = mViewInfo->sel0;

   onScreen = between_inclusive( mViewInfo->h,
                                 mLastCursor,
                                 mViewInfo->h + mViewInfo->screen );

   if( !onScreen )
   {
      return;
   }

   AColor::CursorColor( &dc );

   x = GetLeftOffset() +
       int ( ( mLastCursor - mViewInfo->h ) * mViewInfo->zoom );

   // Draw cursor in all selected tracks
   VisibleTrackIterator iter( GetProject() );
   for( Track *t = iter.First(); t; t = iter.Next() )
   {
      if( t->GetSelected() || mAx->IsFocused( t ) )
      {
         int y = t->GetY() - mViewInfo->vpos + 1;
         wxCoord top = y + kTopInset;
         wxCoord bottom = y + t->GetHeight() - kTopInset;

         // MB: warp() is not needed here as far as I know, in fact it creates a bug. Removing it fixes that.
         AColor::Line( dc, x, top, x, bottom ); // <-- The whole point of this routine.

#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
         if(MONO_WAVE_PAN(t)){
            y = t->GetY(true) - mViewInfo->vpos + 1;
            top = y + kTopInset;
            bottom = y + t->GetHeight(true) - kTopInset;
            AColor::Line( dc, x, top, x, bottom );
         }
#endif

      }
   }

   // AS: Ah, no, this is where we draw the blinky thing in the ruler.
   mRuler->DrawCursor( mLastCursor );

   DisplaySelection();
}

/// OnSize() is called when the panel is resized
void TrackPanel::OnSize(wxSizeEvent & /* event */)
{
   int width, height;
   GetSize( &width, &height );

   // wxMac doesn't like zero dimensions, so protect against it
   width = width == 0 ? 1 : width;
   height = height == 0 ? 1 : height;

   // (Re)allocate the backing bitmap
   if( mBacking )
   {
      mBackingDC.SelectObject( wxNullBitmap );
      delete mBacking;
   }

   mBacking = new wxBitmap( width, height );
   mBackingDC.SelectObject( *mBacking );

   // Refresh the entire area.  Really only need to refresh when
   // expanding...is it worth the trouble?
   Refresh( false );
}

/// OnErase( ) is called during the normal course of 
/// completing an erase operation.
void TrackPanel::OnErase(wxEraseEvent & /* event */)
{
   // Ignore it for now.  This reduces flashing when dragging windows
   // over track area while playing or recording.
   //
   // However, if artifacts or the like are discovered later, then
   // we could blit the backing bitmap here.
}

/// AS: OnPaint( ) is called during the normal course of 
///  completing a repaint operation.
void TrackPanel::OnPaint(wxPaintEvent & /* event */)
{
#if DEBUG_DRAW_TIMING
   wxStopWatch sw;
#endif

   // Construct the paint DC on the heap so that it may be deleted
   // early
   wxDC *dc = new wxPaintDC( this );

   // Retrieve the damage rectangle
   wxRect box = GetUpdateRegion().GetBox();

   // Recreate the backing bitmap if we have a full refresh
   // (See TrackPanel::Refresh())
   if( mRefreshBacking || ( box == GetRect() ) )
   {
      // Update visible sliders
      mTrackInfo.UpdateSliderOffset(mViewInfo->track);

      // Reset (should a mutex be used???)
      mRefreshBacking = false;

      // Redraw the backing bitmap
      DrawTracks( &mBackingDC );

      // Copy it to the display
      dc->Blit( 0, 0, mBacking->GetWidth(), mBacking->GetHeight(), &mBackingDC, 0, 0 );
   }
   else
   {
      // Copy full, possibly clipped, damage rectange
      dc->Blit( box.x, box.y, box.width, box.height, &mBackingDC, box.x, box.y );
   } 

   // Done with the clipped DC
   delete dc;

   // Drawing now goes directly to the client area
   wxClientDC cdc( this );

   // Update the indicator in case it was damaged if this project is playing
   AudacityProject* p = GetProject();
   if (!gAudioIO->IsPaused() &&
       ( mIndicatorShowing || gAudioIO->IsStreamActive(p->GetAudioIOToken())))
   {
      // We just want to repair, not update the old, so set the second param to true.
      // This is important because this onPaint could be for just some of the tracks.
      DoDrawIndicator( cdc, true);
   }

   // Draw the cursor
   if( mViewInfo->sel0 == mViewInfo->sel1)
      DoDrawCursor( cdc );

#if DEBUG_DRAW_TIMING
   sw.Pause();
   wxLogDebug(wxT("Total: %d milliseconds"), sw.Time());
   wxPrintf(wxT("Total: %d milliseconds\n"), sw.Time());
#endif
}

/// Makes our Parent (well, whoever is listening to us) push their state.
/// this causes application state to be preserved on a stack for undo ops.
void TrackPanel::MakeParentPushState(wxString desc, wxString shortDesc,
                                     int flags)
{
   mListener->TP_PushState(desc, shortDesc, flags);
}

void TrackPanel::MakeParentModifyState(bool bWantsAutoSave)
{
   mListener->TP_ModifyState(bWantsAutoSave);
}

void TrackPanel::MakeParentRedrawScrollbars()
{
   mListener->TP_RedrawScrollbars();
}

void TrackPanel::MakeParentResize()
{
   mListener->TP_HandleResize();
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
   mListener->TP_ScrollWindow(mViewInfo->h + mViewInfo->screen);
}

void TrackPanel::HandlePageDownKey()
{
   mListener->TP_ScrollWindow(mViewInfo->h - mViewInfo->screen);
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
   AudacityProject *p = GetProject();
   bool bUnsafe = (p->GetAudioIOToken()>0 &&
                  gAudioIO->IsStreamActive(p->GetAudioIOToken()));
   return bUnsafe;
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
      return true;
#ifdef USE_MIDI
   case IsStretching:
      SetCursor( unsafe ? *mDisabledCursor : *mStretchCursor);
      return true;
#endif
   case IsOverCutLine:
      SetCursor( unsafe ? *mDisabledCursor : *mArrowCursor);
   default:
      break;
   }
   return false;
}

/// When in the "label" (TrackInfo or vertical ruler), we can either vertical zoom or re-order tracks.
/// Dont't change cursor/tip to zoom if display is not waveform (either linear of dB) or Spectrum
void TrackPanel::SetCursorAndTipWhenInLabel( Track * t,
         wxMouseEvent &event, const wxChar ** ppTip )
{
   if (event.m_x >= GetVRulerOffset() &&
         (t->GetKind() == Track::Wave) &&
         (((WaveTrack *) t)->GetDisplay() <= WaveTrack::SpectrumDisplay ||
            ((WaveTrack *) t)->GetDisplay() <= WaveTrack::SpectrumLogDisplay)) 
   {
      *ppTip = _("Click to vertically zoom in. Shift-click to zoom out. Drag to specify a zoom region.");
      SetCursor(event.ShiftDown()? *mZoomOutCursor : *mZoomInCursor);
   }
#ifdef USE_MIDI
   else if (event.m_x >= GetVRulerOffset() && t->GetKind() == Track::Note) {
      *ppTip = _("Click to verticaly zoom in, Shift-click to zoom out, Drag to create a particular zoom region.");
      SetCursor(event.ShiftDown() ? *mZoomOutCursor : *mZoomInCursor);
   }
#endif
   else {
      // Set a status message if over TrackInfo.
      *ppTip = _("Drag the track vertically to change the order of the tracks.");
      SetCursor(*mArrowCursor);
   }
}

/// When in the resize area we can adjust size or relative size.
void TrackPanel::SetCursorAndTipWhenInVResizeArea( Track * label, 
         bool bLinked, const wxChar ** ppTip )
{
   // Check to see whether it is the first channel of a stereo track
   if (bLinked) {
      // If we are in the label we got here 'by mistake' and we're 
      // not actually in the resize area at all.  (The resize area 
      // is shorter when it is between stereo tracks).

      // IF we are in the label THEN return.
      // Subsequently called methods can detect that a tip and 
      // cursor are still needed.
      if (label) 
         return;
      *ppTip = _("Click and drag to adjust relative size of stereo tracks.");
      SetCursor(*mResizeCursor);
   } else {
      *ppTip = _("Click and drag to resize the track.");
      SetCursor(*mResizeCursor);
   }
}

/// When in a label track, find out if we've hit anything that 
/// would cause a cursor change.
void TrackPanel::SetCursorAndTipWhenInLabelTrack( LabelTrack * pLT, 
       wxMouseEvent & event, const wxChar ** ppTip )
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
      *ppTip=
         (pLT->mbHitCenter ) ?
         _("Drag one or more label boundaries") :
         _("Drag label boundary");
   }
}

// The select tool can have different cursors and prompts depending on what
// we hover over, most notably when hovering over the selction boundaries.
// Determine and set the cursor and tip accordingly.
void TrackPanel::SetCursorAndTipWhenSelectTool( Track * t, 
        wxMouseEvent & event, wxRect &r, bool bMultiToolMode, const wxChar ** ppTip )
{
   SetCursor(*mSelectCursor);

   //In Multi-tool mode, give multitool prompt if no-special-hit.
   if( bMultiToolMode ) {
      #ifdef __WXMAC__
      /* i18n-hint: This string is for the Mac OS, which uses Command-, as the shortcut for Preferences */
      *ppTip = _("Multi-Tool Mode: Cmd-, for Mouse and Keyboard Preferences");
      #else
      /* i18n-hint: This string is for Windows and Linux, which uses Control-P as the shortcut for Preferences */
      *ppTip = _("Multi-Tool Mode: Ctrl-P for Mouse and Keyboard Preferences");
      #endif
   }

   //Make sure we are within the selected track
   if (!t || !t->GetSelected()) 
   {
      if(t)
      {
         //For OD regions, we need to override and display the percent complete for this task.
         //first, make sure it's a wavetrack.
         if(t->GetKind() == Track::Wave)
         {
            //see if the wavetrack exists in the ODManager (if the ODManager exists)
            if(ODManager::IsInstanceCreated())
            {
               //ask the wavetrack for the corresponding tip - it may not change **pptip, but that's fine.
               ODManager::Instance()->FillTipForWaveTrack((WaveTrack*)t,ppTip);
            }
         }
      }
      return;
   }
   wxInt64 leftSel = TimeToPosition(mViewInfo->sel0, r.x);
   wxInt64 rightSel = TimeToPosition(mViewInfo->sel1, r.x);

   // Something is wrong if right edge comes before left edge
   wxASSERT(!(rightSel < leftSel));

   // Adjusting the selection edges can be turned off in
   // the preferences...
   if (!mAdjustSelectionEdges) {
      //For OD regions, we need to override and display the percent complete for this task.
      //first, make sure it's a wavetrack.
      if(t->GetKind() == Track::Wave)
      {
         //see if the wavetrack exists in the ODManager (if the ODManager exists)
         if(ODManager::IsInstanceCreated())
         {
            //ask the wavetrack for the corresponding tip - it may not change **pptip, but that's fine.
            ODManager::Instance()->FillTipForWaveTrack((WaveTrack*)t,ppTip);
         }
      }
   }
   // Is the cursor over the left selection boundary?
   else if (within(event.m_x, leftSel, SELECTION_RESIZE_REGION)) {
#ifdef USE_MIDI
      if (HitTestStretch(t, r, event)) {
         *ppTip = _("Click and drag to stretch selected region.");
         SetCursor(*mStretchLeftCursor);
         return;
      }
#endif
      *ppTip = _("Click and drag to move left selection boundary.");
      SetCursor(*mAdjustLeftSelectionCursor);
   }
   // Is the cursor over the right selection boundary?
   else if (within(event.m_x, rightSel, SELECTION_RESIZE_REGION)) {
#ifdef USE_MIDI
      if (HitTestStretch(t, r, event)) {
         *ppTip = _("Click and drag to stretch selected region.");
         SetCursor(*mStretchRightCursor);
         return;
      }
#endif
      *ppTip = _("Click and drag to move right selection boundary.");
      SetCursor(*mAdjustRightSelectionCursor);
   }
#ifdef USE_MIDI
   else if (HitTestStretch(t, r, event)) {
      *ppTip = _("Click and drag to stretch within selected region.");
      SetCursor(*mStretchCursor);
   }
#endif
   else
   {
      //For OD regions, we need to override and display the percent complete for this task.
      //first, make sure it's a wavetrack.
      if(t->GetKind() == Track::Wave)
      {
         //see if the wavetrack exists in the ODManager (if the ODManager exists)
         if(ODManager::IsInstanceCreated())
         {
            //ask the wavetrack for the corresponding tip - it may not change **pptip, but that's fine.
            ODManager::Instance()->FillTipForWaveTrack((WaveTrack*)t,ppTip);
         }
      }
   }

   // It's possible we didn't set the tip and cursor.
   // Subsequently called methods can detect this.
}

/// In this method we know what tool we are using,
/// so set the cursor accordingly.
void TrackPanel::SetCursorAndTipByTool( int tool, 
         wxMouseEvent & event, const wxChar ** /*ppTip*/ )
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
void TrackPanel::HandleCursor(wxMouseEvent & event)
{
   mLastMouseEvent = event;

   // (1), If possible, set the cursor based on the current activity
   //      ( leave the StatusBar alone ).
   if( SetCursorByActivity() )
      return;

   // (2) If we are not over a track at all, set the cursor to Arrow and 
   //     clear the StatusBar, 
   wxRect r;
   Track *label = FindTrack(event.m_x, event.m_y, true, true, &r);
   Track *nonlabel = FindTrack(event.m_x, event.m_y, false, false, &r);
   Track *t = label ? label : nonlabel;

   if (!t) {
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
   const wxChar *tip = NULL;

   if (label) {
      SetCursorAndTipWhenInLabel( label, event, &tip );
   }

   // Are we within the vertical resize area?
   if ((tip == NULL ) && within(event.m_y, r.y + r.height, TRACK_RESIZE_REGION)) 
   {
      SetCursorAndTipWhenInVResizeArea( label, t->GetLinked(), &tip );
      // tip may still be NULL at this point, in which case we go on looking.
   }

   // Otherwise, we must be over a track of some kind 
   // Is it a label track?
   if ((tip==NULL) && (t->GetKind() == Track::Label))
   {
      // We are over a label track
      SetCursorAndTipWhenInLabelTrack( (LabelTrack*)t, event, &tip );
      // ..and if we haven't yet determined the cursor, 
      // we go on to do all the standard track hit tests.
   }  

   if( tip==NULL )
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

      tip = ttb->GetMessageForTool( tool );

      // We don't include the select tool in 
      // SetCursorAndTipByTool() because it's more complex than
      // the other tool cases.
      if( tool != selectTool )
      {
         SetCursorAndTipByTool( tool, event, &tip);
      }
      else
      {
         bool bMultiToolMode = ttb->IsDown(multiTool);
         SetCursorAndTipWhenSelectTool( t, event, r, bMultiToolMode, &tip );
      }
   }

   if (tip)
      mListener->TP_DisplayStatusMessage(tip);
}


/// This method handles various ways of starting and extending
/// selections.  These are the selections you make by clicking and
/// dragging over a waveform.
void TrackPanel::HandleSelect(wxMouseEvent & event)
{
   wxRect r;
   Track *t = FindTrack(event.m_x, event.m_y, false, false, &r);

   // AS: Ok, did the user just click the mouse, release the mouse,
   //  or drag?
   if (event.LeftDown()) {
      // AS: Now, did they click in a track somewhere?  If so, we want
      //  to extend the current selection or start a new selection, 
      //  depending on the shift key.  If not, cancel all selections.
      if (t)
         SelectionHandleClick(event, t, r);
      else {
         SelectNone();
         Refresh(false);
      }

   } else if (event.LeftUp() || event.RightUp()) {
      if (mSnapManager) {
         delete mSnapManager;
         mSnapManager = NULL;
      }
      mSnapLeft = -1;
      mSnapRight = -1;

      SetCapturedTrack( NULL );
      //Send the new selection state to the undo/redo stack:
      MakeParentModifyState(false);

   } else if (event.LeftDClick() && !event.ShiftDown()) {
      if (!mCapturedTrack) {
         wxRect r;
         mCapturedTrack =
            FindTrack(event.m_x, event.m_y, false, false, &r);
         if (!mCapturedTrack)
            return;
      }

      // Deselect all other tracks and select this one.
      SelectNone();

      mTracks->Select(mCapturedTrack);

      // Default behavior: select whole track
      SelectTrackLength(mCapturedTrack);

      // Special case: if we're over a clip in a WaveTrack,
      // select just that clip
      if (mCapturedTrack->GetKind() == Track::Wave) {
         WaveTrack *w = (WaveTrack *)mCapturedTrack;
         WaveClip *selectedClip = w->GetClipAtX(event.m_x);
         if (selectedClip) {
            mViewInfo->sel0 = selectedClip->GetOffset();
            mViewInfo->sel1 = selectedClip->GetEndTime();
         }
         //Also, capture this track for dragging until we up-click.
         mCapturedClipArray.Add(TrackClip(w, selectedClip));

         mMouseCapture = IsSliding;

         Refresh(false);
         StartSlide(event);
         goto done;
      }

      Refresh(false);
      SetCapturedTrack( NULL );
      MakeParentModifyState(false);
   } 
 done:
   SelectionHandleDrag(event, t);

   // Update lyrics display for new selection.
   AudacityProject* pProj = GetActiveProject();
   LyricsWindow* pLyricsWindow = pProj->GetLyricsWindow();
   if (pLyricsWindow && pLyricsWindow->IsShown()) 
   {
      Lyrics* pLyricsPanel = pLyricsWindow->GetLyricsPanel();
      pLyricsPanel->Update(pProj->GetSel0());
   }
}

/// This method gets called when we're handling selection
/// and the mouse was just clicked.
void TrackPanel::SelectionHandleClick(wxMouseEvent & event,
                                      Track * pTrack, wxRect r)
{
   Track *rightTrack = NULL;
   bool nextTrackIsLinkFromPTrack = false;
   mCapturedTrack = pTrack;
   mCapturedRect = r;

   mMouseClickX = event.m_x;
   mMouseClickY = event.m_y;
   bool startNewSelection = true;
   mMouseCapture=IsSelecting;

   // We create a new snap manager in case any snap-points have changed
   if (mSnapManager)
      delete mSnapManager;

   mSnapManager = new SnapManager(mTracks, NULL,
                                  mViewInfo->zoom,
                                  4); // pixel tolerance

   mSnapLeft = -1;
   mSnapRight = -1;

#ifdef USE_MIDI
   mStretching = false;
   bool stretch = HitTestStretch(pTrack, r, event);
#endif

   if (event.ShiftDown()
#ifdef USE_MIDI
                         && !stretch
#endif
                                   ) {
      // If the shift button is down and no track is selected yet,
      // at least select the track we clicked into.
      bool isAtLeastOneTrackSelected = false;
      double selend = PositionToTime(event.m_x, r.x);

      TrackListIterator iter(mTracks);
      for (Track *t = iter.First(); t; t = iter.Next()) {
         if (t->GetSelected()) {
            isAtLeastOneTrackSelected = true;
            break;
         } else if (t == pTrack && t->GetLinked()) {
            nextTrackIsLinkFromPTrack = true;
         } else if (nextTrackIsLinkFromPTrack) {
            rightTrack = t;
            nextTrackIsLinkFromPTrack = false;
         }
      }

      if (!isAtLeastOneTrackSelected) {
         pTrack->SetSelected(true);
         if (rightTrack)
            rightTrack->SetSelected(true);
         else if (pTrack->GetLink())
            pTrack->GetLink()->SetSelected(true);
      }

      // Edit the selection boundary nearest the mouse click.
      if (fabs(selend - mViewInfo->sel0) < fabs(selend - mViewInfo->sel1))
         mSelStart = mViewInfo->sel1;
      else
         mSelStart = mViewInfo->sel0;

      // If the shift button is down, extend the current selection.
      ExtendSelection(event.m_x, r.x, pTrack);
      MakeParentModifyState(false);
      return;
   }


   // A control-click will set just the indicator to the clicked spot,
   // and turn playback on.  
   else if(event.CmdDown()
#ifdef USE_MIDI
                           && !stretch
#endif
                                     ) {
         AudacityProject *p = GetActiveProject();
         if (p) {

            double clicktime = PositionToTime(event.m_x, GetLeftOffset());
            double endtime = clicktime < mViewInfo->sel1? mViewInfo->sel1: mViewInfo->total ;

         //Behavior should differ depending upon whether we are
         //currently in playback mode or not.

         bool busy = gAudioIO->IsBusy();
         if(!busy)
            {
               //If we aren't currently playing back, start playing back at 
               //the clicked point
               ControlToolBar * ctb = p->GetControlToolBar();
               ctb->SetPlay(true);
               ctb->PlayPlayRegion(clicktime, endtime,false) ;

            }
            else
            {
               //If we are playing back, stop and move playback
               //to the clicked point.
               //This unpauses paused audio as well.  The right thing to do might be to
               //leave it paused but move the point.  This would probably
               //require a new method in ControlToolBar: SetPause();
               ControlToolBar * ctb = p->GetControlToolBar();
               ctb->StopPlaying();
               ctb->PlayPlayRegion(clicktime,endtime,false) ;
            }

         }

         return;  
      }  
   //Make sure you are within the selected track
   if (pTrack && pTrack->GetSelected()) {
      wxInt64 leftSel = TimeToPosition(mViewInfo->sel0, r.x);
      wxInt64 rightSel = TimeToPosition(mViewInfo->sel1, r.x);
      wxASSERT(leftSel <= rightSel);
      // Adjusting selection edges can be turned off in the
      // preferences now
      if (!mAdjustSelectionEdges) {
      }
      // Is the cursor over the left selection boundary?
      else if (within(event.m_x, leftSel, SELECTION_RESIZE_REGION)) {
         // Pin the right selection boundary
         mSelStart = mViewInfo->sel1;
         startNewSelection = false;
      }
      // Is the cursor over the right selection boundary?
      else if (within(event.m_x, rightSel, SELECTION_RESIZE_REGION)) {
         // Pin the left selection boundary
         mSelStart = mViewInfo->sel0;
         startNewSelection = false;
      }
   }

   //Determine if user clicked on a label track. 
   if (pTrack && (pTrack->GetKind() == Track::Label)) 
   {
      LabelTrack *lt = (LabelTrack *) pTrack;
      if (lt->HandleMouse(event, r,//mCapturedRect,
                          mViewInfo->h, mViewInfo->zoom,
                          &mViewInfo->sel0, &mViewInfo->sel1)) {
         MakeParentPushState(_("Modified Label"),
                             _("Label Edit"),
                             PUSH_CONSOLIDATE);
      }

      // IF the user clicked a label, THEN select all other tracks by Label
      if (lt->IsSelected()) {
         mTracks->Select( lt );
         SelectTracksByLabel( lt );
         DisplaySelection();
         return;
      }
   }

#ifdef USE_MIDI
   if (stretch) {
      NoteTrack *nt = (NoteTrack *) pTrack;
      // find nearest beat to sel0, sel1
      double minPeriod = 0.05; // minimum beat period
      double qBeat0, qBeat1;
      double centerBeat = 0.0f;
      mStretchSel0 = nt->NearestBeatTime(mViewInfo->sel0, &qBeat0);
      mStretchSel1 = nt->NearestBeatTime(mViewInfo->sel1, &qBeat1);

      // If there is not (almost) a beat to stretch that is slower
      // than 20 beats per second, don't stretch
      if (within(qBeat0, qBeat1, 0.9) ||
          (mStretchSel1 - mStretchSel0) / (qBeat1 - qBeat0) < minPeriod) return;

      if (startNewSelection) { // mouse is not at an edge, but after
         // quantization, we could be indicating the selection edge
         mSelStart = PositionToTime(event.m_x, r.x);
         mStretchStart = nt->NearestBeatTime(mSelStart, &centerBeat);
         if (within(qBeat0, centerBeat, 0.1)) {
            mListener->TP_DisplayStatusMessage(
                    _("Click and drag to stretch selected region."));
            SetCursor(*mStretchLeftCursor);
            // mStretchMode = stretchLeft;
            mSelStart = mViewInfo->sel1; // condition that implies stretchLeft
            startNewSelection = false;
         } else if (within(qBeat1, centerBeat, 0.1)) {
            mListener->TP_DisplayStatusMessage(
                    _("Click and drag to stretch selected region."));
            SetCursor(*mStretchRightCursor);
            // mStretchMode = stretchRight;
            mSelStart = mViewInfo->sel0; // condition that implies stretchRight
            startNewSelection = false;
         }
      }

      if (startNewSelection) {
         mStretchMode = stretchCenter;
         mStretchLeftBeats = qBeat1 - centerBeat;
         mStretchRightBeats = centerBeat - qBeat0;
      } else if (mViewInfo->sel1 == mSelStart) {
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
      mViewInfo->sel0 = mStretchSel0;
      mViewInfo->sel1 = mStretchSel1;
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
      // If we didn't move a selection boundary, start a new selection
      SelectNone();
      StartSelection(event.m_x, r.x);
      mTracks->Select(pTrack);
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
   mSelStart = mViewInfo->h + ((mouseXCoordinate - trackLeftEdge)
                               / mViewInfo->zoom);

   double s = mSelStart;

   if (mSnapManager) {
      mSnapLeft = -1;
      mSnapRight = -1;
      bool snappedPoint, snappedTime;
      if (mSnapManager->Snap(mCapturedTrack, mSelStart, false,
                             &s, &snappedPoint, &snappedTime)) {
         if (snappedPoint)
            mSnapLeft = TimeToPosition(s, trackLeftEdge);
      }
   }

   mViewInfo->sel0 = s;
   mViewInfo->sel1 = s;

   SonifyBeginModifyState();
   MakeParentModifyState(false);
   SonifyEndModifyState();
}

/// Extend the existing selection
void TrackPanel::ExtendSelection(int mouseXCoordinate, int trackLeftEdge,
                                 Track *pTrack)
{
   double selend = PositionToTime(mouseXCoordinate, trackLeftEdge);
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
            mSnapLeft = TimeToPosition(sel0, trackLeftEdge);
      }
      if (mSnapManager->Snap(mCapturedTrack, sel1, true,
                             &sel1, &snappedPoint, &snappedTime)) {
         if (snappedPoint)
            mSnapRight = TimeToPosition(sel1, trackLeftEdge);
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

   mViewInfo->sel0 = sel0;
   mViewInfo->sel1 = sel1;

   //On-Demand: check to see if there is an OD thing associated with this track.  If so we want to update the focal point for the task.
   if (pTrack && (pTrack->GetKind() == Track::Wave) && ODManager::IsInstanceCreated())
      ODManager::Instance()->DemandTrackUpdate((WaveTrack*)pTrack,sel0); //sel0 is sometimes less than mSelStart

   // Full refresh since the label area may need to indicate
   // newly selected tracks.
   Refresh(false);

   // Make sure the ruler follows suit.
   mRuler->DrawSelection();

   // As well as the SelectionBar.
   DisplaySelection();
}

#ifdef USE_MIDI
void TrackPanel::Stretch(int mouseXCoordinate, int trackLeftEdge,
                         Track *pTrack)
{
   if (mStretched) { // Undo stretch and redo it with new mouse coordinates
      // Drag handling was not originally implemented with Undo in mind --
      // there are saved pointers to tracks that are not supposed to change.
      // Undo will change tracks, so convert pTrack, mCapturedTrack to index
      // values, then look them up after the Undo
      TrackListIterator iter(mTracks);
      int pTrackIndex = pTrack->GetIndex();
      int capturedTrackIndex = 
         (mCapturedTrack ? mCapturedTrack->GetIndex() : 0);

      GetProject()->OnUndo();

      // Undo brings us back to the pre-click state, but we want to 
      // quantize selected region to integer beat boundaries. These
      // were saved in mStretchSel[12] variables:
      mViewInfo->sel0 = mStretchSel0;
      mViewInfo->sel1 = mStretchSel1;

      mStretched = false;
      int index = 0;
      for (Track *t = iter.First(mTracks); t; t = iter.Next()) {
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
   double moveto = PositionToTime(mouseXCoordinate, trackLeftEdge);

   // check to make sure tempo is not higher than 20 beats per second
   // (In principle, tempo can be higher, but not infinity.)
   double minPeriod = 0.05; // minimum beat period
   double qBeat0, qBeat1;
   pNt->NearestBeatTime(mViewInfo->sel0, &qBeat0); // get beat
   pNt->NearestBeatTime(mViewInfo->sel1, &qBeat1);

   // We could be moving 3 things: left edge, right edge, a point between
   switch (mStretchMode) {
   case stretchLeft: {
      // make sure target duration is not too short
      double dur = mViewInfo->sel1 - moveto;
      if (dur < mStretchRightBeats * minPeriod) {
         dur = mStretchRightBeats * minPeriod;
         moveto = mViewInfo->sel1 - dur;
      }
      if (pNt->StretchRegion(mStretchSel0, mStretchSel1, dur)) {
         pNt->SetOffset(pNt->GetOffset() + moveto - mStretchSel0);
         mViewInfo->sel0 = moveto;
      }
      break;
   }
   case stretchRight: {
      // make sure target duration is not too short
      double dur = moveto - mViewInfo->sel0;
      if (dur < mStretchLeftBeats * minPeriod) {
         dur = mStretchLeftBeats * minPeriod;
         moveto = mStretchSel0 + dur;
      }
      if (pNt->StretchRegion(mStretchSel0, mStretchSel1, dur)) {
         mViewInfo->sel1 = moveto;
      }
      break;
   }
   case stretchCenter: {
      // make sure both left and right target durations are not too short
      double left_dur = moveto - mViewInfo->sel0;
      double right_dur = mViewInfo->sel1 - moveto;
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
      PUSH_CONSOLIDATE | PUSH_AUTOSAVE | PUSH_CALC_SPACE);
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
   if ((!event.Dragging() && !mAutoScrolling) || event.CmdDown())
      return;

   wxRect r      = mCapturedRect;
   Track *pTrack = mCapturedTrack;

   // AS: Note that FindTrack will replace r's value.
   if (!pTrack)
      pTrack = FindTrack(event.m_x, event.m_y, false, false, &r);

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
   wxInt64 SelStart=TimeToPosition( mSelStart, r.x); //cvt time to pixels.
   // Abandon this drag if selecting < 5 pixels.
   if (wxLongLong(SelStart-x).Abs() < minimumSizedSelection
#ifdef USE_MIDI        // limiting selection size is good, and not starting
       && !mStretching // stretch unless mouse moves 5 pixels is good, but
#endif                 // once stretching starts, it's ok to move even 1 pixel 
       )
       return;

   // Handle which tracks are selected
   if (Track *eTrack = FindTrack(x, y, false, false, NULL)) {
      // Swap the track pointers if needed
      if (eTrack->GetIndex() < pTrack->GetIndex()) {
         Track *t = eTrack;
         eTrack = pTrack;
         pTrack = t;
      }

      TrackListIterator iter(mTracks);
      pTrack = iter.StartWith(pTrack);
      do {
         mTracks->Select(pTrack);
         if (pTrack == eTrack) {
            break;
         }

         pTrack = iter.Next();
      } while (pTrack);
   }
#ifdef USE_MIDI
   if (mStretching) {
      // the following is also in ExtendSelection, called below
      // probably a good idea to "hoist" the code to before this "if" stmt
      if (clickedTrack == NULL && mCapturedTrack != NULL)
         clickedTrack = mCapturedTrack;
      Stretch(x, r.x, clickedTrack);
      return;
   }
#endif
   ExtendSelection(x, r.x, clickedTrack);
}

/// Converts a position (mouse X coordinate) to 
/// project time, in seconds.  Needs the left edge of
/// the track as an additional parameter.
double TrackPanel::PositionToTime(wxInt64 mouseXCoordinate,
                                  wxInt64 trackLeftEdge) const
{
   return mViewInfo->h + ((mouseXCoordinate - trackLeftEdge)
                          / mViewInfo->zoom);
}


/// STM: Converts a project time to screen x position.
wxInt64 TrackPanel::TimeToPosition(double projectTime,
                                   wxInt64 trackLeftEdge) const
{
   return static_cast <
       wxInt64 >(mViewInfo->zoom * (projectTime - mViewInfo->h) +
                 trackLeftEdge);
}

/// HandleEnvelope gets called when the user is changing the
/// amplitude envelope on a track.
void TrackPanel::HandleEnvelope(wxMouseEvent & event)
{
   if (event.LeftDown()) {
      wxRect r;
      mCapturedTrack = FindTrack(event.m_x, event.m_y, false, false, &r);

      if (!mCapturedTrack)
         return;

      if (mCapturedTrack->GetKind() == Track::Wave)
      {
         mCapturedEnvelope =
            ((WaveTrack*)mCapturedTrack)->GetEnvelopeAtX(event.GetX());
      } else {
         mCapturedEnvelope = NULL;
      }

      mCapturedRect = r;
      mCapturedRect.y += kTopInset;
      mCapturedRect.height -= kTopInset;
   }
   // AS: if there's actually a selected track, then forward all of the
   //  mouse events to its envelope.
   if (mCapturedTrack)
      ForwardEventToEnvelope(event);

   if (event.LeftUp()) {
      mCapturedTrack = NULL;
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
   TimeTrack *ptimetrack = (TimeTrack *) mCapturedTrack;
   Envelope *pspeedenvelope = ptimetrack->GetEnvelope();

   wxRect envRect = mCapturedRect;
   envRect.y++;
   envRect.height -= 2;
   double lower = ptimetrack->GetRangeLower(), upper = ptimetrack->GetRangeUpper();
   if(ptimetrack->GetDisplayLog()) {
      // MB: silly way to undo the work of GetWaveYPos while still getting a logarithmic scale
      double dBRange = gPrefs->Read(wxT("/GUI/EnvdBRange"), ENV_DB_RANGE);
      lower = 20.0 * log10(std::max(1.0e-7, lower)) / dBRange + 1.0;
      upper = 20.0 * log10(std::max(1.0e-7, upper)) / dBRange + 1.0;
   }
   bool needUpdate =
      pspeedenvelope->MouseEvent(
         event, envRect,
         mViewInfo->h, mViewInfo->zoom,
         ptimetrack->GetDisplayLog(), lower, upper);
   if (needUpdate) {
      RefreshTrack(mCapturedTrack);
   }
}

/// We've established we're a wave track.  
/// send events for its envelope.
void TrackPanel::ForwardEventToWaveTrackEnvelope(wxMouseEvent & event)
{
   WaveTrack *pwavetrack = (WaveTrack *) mCapturedTrack;
   Envelope *penvelope = mCapturedEnvelope;

   // Possibly no-envelope, for example when in spectrum view mode.
   // if so, then bail out.
   if (!penvelope)
      return;

   // AS: WaveTracks can be displayed in several different formats.
   //  This asks which one is in use. (ie, Wave, Spectrum, etc)
   int display = pwavetrack->GetDisplay();

   // AS: If we're using the right type of display for envelope operations
   //  ie one of the Wave displays
   if (display <= 1) {
      bool dB = (display == 1);
      bool needUpdate = false;

      // AS: Then forward our mouse event to the envelope.
      // It'll recalculate and then tell us whether or not to redraw.
      wxRect envRect = mCapturedRect;
      envRect.y++;
      envRect.height -= 2;
      float zoomMin, zoomMax;
      pwavetrack->GetDisplayBounds(&zoomMin, &zoomMax);
      needUpdate = penvelope->MouseEvent(
         event, envRect,
         mViewInfo->h, mViewInfo->zoom,
         dB, zoomMin, zoomMax);

      // If this track is linked to another track, make the identical
      // change to the linked envelope:
      WaveTrack *link = (WaveTrack *) mCapturedTrack->GetLink();
      if (link) {
         Envelope *e2 = link->GetEnvelopeAtX(event.GetX());
         // There isn't necessarily an envelope there; no guarantee a
         // linked track has the same WaveClip structure...
         bool updateNeeded = false;
         if (e2) {
            wxRect envRect = mCapturedRect;
            envRect.y++;
            envRect.height -= 2;
            float zoomMin, zoomMax;
            pwavetrack->GetDisplayBounds(&zoomMin, &zoomMax);
            updateNeeded = e2->MouseEvent(event, envRect,
                                         mViewInfo->h, mViewInfo->zoom, dB,
                                         zoomMin, zoomMax);
            needUpdate |= updateNeeded;
         }
         if(!e2 || !updateNeeded)   // no envelope found at this x point, or found but not updated
         {
            if( (e2 = link->GetActiveEnvelope()) != 0 )  // search for any active DragPoint
            {
               wxRect envRect = mCapturedRect;
               envRect.y++;
               envRect.height -= 2;
               float zoomMin, zoomMax;
               pwavetrack->GetDisplayBounds(&zoomMin, &zoomMax);
               needUpdate |= e2->MouseEvent(event, envRect,
                                            mViewInfo->h, mViewInfo->zoom, dB,
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
      if (mDidSlideVertically && mCapturedTrack) 
         // Now that user has dropped the clip into a different track, 
         // make sure the sample rate matches the destination track (mCapturedTrack).
         for (size_t i = 0; i < mCapturedClipArray.GetCount(); i++) 
            if (mCapturedTrack->GetKind() == Track::Wave) // Should always be true here, but make sure.
            {
               WaveClip* pWaveClip = mCapturedClipArray[i].clip;
               // Note that per TrackPanel::AddClipsToCaptured(Track *t, double t0, double t1), 
               // in the non-WaveTrack case, the code adds a NULL clip to mCapturedClipArray, 
               // so we have to check for that any time we're going to deref it. 
               // Previous code did not check it here, and that caused bug 367 crash.
               if (pWaveClip) 
               {
                  pWaveClip->Resample(((WaveTrack*)mCapturedTrack)->GetRate());
                  pWaveClip->MarkChanged();
               }
            }

      SetCapturedTrack( NULL );

      if (mSnapManager) {
         delete mSnapManager;
         mSnapManager = NULL;
      }
      mSnapLeft = -1;
      mSnapRight = -1;

      if (!mDidSlideVertically && mHSlideAmount==0)
         return;

      MakeParentRedrawScrollbars();

      wxString msg;
      bool consolidate;
      if (mDidSlideVertically) {
         msg.Printf(_("Moved clip to another track"));
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
         consolidate ? (PUSH_CONSOLIDATE) : (PUSH_AUTOSAVE|PUSH_CALC_SPACE));
   }
}

/// Prepare for sliding.
void TrackPanel::StartSlide(wxMouseEvent & event)
{
   wxRect r;

   mHSlideAmount = 0.0;
   mDidSlideVertically = false;

   Track *vt = FindTrack(event.m_x, event.m_y, false, false, &r);
   if (!vt) 
      return;

   ToolsToolBar * ttb = mListener->TP_GetToolsToolBar();
   bool multiToolModeActive = (ttb && ttb->IsDown(multiTool));

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

      mCapturedClipArray.Clear();

      double clickTime = 
         PositionToTime(event.m_x, GetLeftOffset());
      bool clickedInSelection =
         (vt->GetSelected() &&
          clickTime > mViewInfo->sel0 &&
          clickTime < mViewInfo->sel1);

      // First, if click was in selection, capture selected clips; otherwise
      // just the clicked-on clip
      if (clickedInSelection) {
         mCapturedClipIsSelection = true;

         TrackListIterator iter(mTracks);
         for (Track *t = iter.First(); t; t = iter.Next()) {
            if (t->GetSelected()) {
               AddClipsToCaptured(t, true);
            }
         }
      }
      else {
         mCapturedClipIsSelection = false;
         mCapturedClipArray.Add(TrackClip(vt, mCapturedClip));

         // Check for stereo partner
         Track *partner = mTracks->GetLink(vt);
         if (mCapturedClip && partner && partner->GetKind() == Track::Wave) {
            // WaveClip::GetClipAtX doesn't work unless the clip is on the screen and can return bad info otherwise
            // instead calculate the time manually
            double rate = ((WaveTrack*)partner)->GetRate();
            double pps = mViewInfo->zoom;
            double tt = (event.m_x - GetLeftOffset()) / pps + mViewInfo->h;
            sampleCount s0 = (sampleCount)(tt * rate + 0.5);

            if (s0 >= 0) {
               WaveClip *clip = ((WaveTrack *)partner)->GetClipAtSample(s0);
               if (clip) {
                  mCapturedClipArray.Add(TrackClip(partner, clip));
               }
            }
         }
      }

      // Now, if sync-lock is enabled, capture any clip that's linked to a
      // captured clip.
      if (GetProject()->IsSyncLocked()) {
         // AWD: mCapturedClipArray expands as the loop runs, so newly-added
         // clips are considered (the effect is like recursion and terminates
         // because AddClipsToCaptured doesn't add duplicate clips); to remove
         // this behavior just store the array size beforehand.
         for (unsigned int i = 0; i < mCapturedClipArray.GetCount(); ++i) {
            // Capture based on tracks that have clips -- that means we
            // don't capture based on links to label tracks for now (until
            // we can treat individual labels as clips)
            if (mCapturedClipArray[i].clip) {
               // Iterate over sync-lock group tracks.
               SyncLockedTracksIterator git(mTracks);
               for ( Track *t = git.First(mCapturedClipArray[i].track);
                     t; t = git.Next() )
               {
                  AddClipsToCaptured(t,
                        mCapturedClipArray[i].clip->GetStartTime(),
                        mCapturedClipArray[i].clip->GetEndTime() );
               }
            }
#ifdef USE_MIDI
            // Capture additional clips from NoteTracks
            Track *nt = mCapturedClipArray[i].track;
            if (nt->GetKind() == Track::Note) {
               // Iterate over sync-lock group tracks.
               SyncLockedTracksIterator git(mTracks);
               for (Track *t = git.First(nt); t; t = git.Next())
               {
                  AddClipsToCaptured(t, nt->GetStartTime(), nt->GetEndTime());
               }
            }
#endif
         }
      }

   } else {
      mCapturedClip = NULL;
      mCapturedClipArray.Clear();
   }

   mSlideUpDownOnly = event.CmdDown() && !multiToolModeActive;

   mCapturedTrack = vt;
   mCapturedRect = r;

   mMouseClickX = event.m_x;
   mMouseClickY = event.m_y;

   mSelStart = mViewInfo->h + ((event.m_x - r.x) / mViewInfo->zoom);

   if (mSnapManager)
      delete mSnapManager;
   mSnapManager = new SnapManager(mTracks,
                                  &mCapturedClipArray,
                                  mViewInfo->zoom,
                                  4,     // pixel tolerance
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
      AddClipsToCaptured(t, mViewInfo->sel0, mViewInfo->sel1);
   else
      AddClipsToCaptured(t, t->GetStartTime(), t->GetEndTime());
}

// Adds a track's clips to mCapturedClipArray within a specified time
void TrackPanel::AddClipsToCaptured(Track *t, double t0, double t1)
{
   if (t->GetKind() == Track::Wave)
   {
      WaveClipList::compatibility_iterator it =
         ((WaveTrack *)t)->GetClipIterator();
      while (it)
      {
         WaveClip *clip = it->GetData();

         if ( ! clip->AfterClip(t0) && ! clip->BeforeClip(t1) )
         {
            // Avoid getting clips that were already captured
            bool newClip = true;
            for (unsigned int i = 0; i < mCapturedClipArray.GetCount(); ++i) {
               if (mCapturedClipArray[i].clip == clip) {
                  newClip = false;
                  break;
               }
            }

            if (newClip)
               mCapturedClipArray.Add(TrackClip(t, clip));
         }
         it = it->GetNext();
      }
   }
   else
   {
      // This handles label tracks rather heavy-handedly -- it would be nice to
      // treat individual labels like clips

      // Avoid adding a track twice
      bool newClip = true;
      for (unsigned int i = 0; i < mCapturedClipArray.GetCount(); ++i) {
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
         mCapturedClipArray.Add(TrackClip(t, NULL));
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
#ifdef USE_MIDI
   Track *mouseTrack = FindTrack(event.m_x, event.m_y, false, false, NULL);
   if (!mouseTrack || (mouseTrack->GetKind() != Track::Wave &&
                       mouseTrack->GetKind() != Track::Note)) {
#else
   WaveTrack *mouseTrack =
      (WaveTrack *)FindTrack(event.m_x, event.m_y, false, false, NULL);
   if (!mouseTrack || mouseTrack->GetKind() != Track::Wave) {
#endif
      return;
   }

   // Start by undoing the current slide amount; everything
   // happens relative to the original horizontal position of
   // each clip...
#ifdef USE_MIDI
   if (mCapturedClipArray.GetCount()) {
#else
   if (mCapturedClip) {
#endif
      for(i=0; i<mCapturedClipArray.GetCount(); i++) {
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
      mViewInfo->sel0 -= mHSlideAmount;
      mViewInfo->sel1 -= mHSlideAmount;
   }
   mHSlideAmount = 0.0;

   double desiredSlideAmount = (event.m_x - mMouseClickX) / mViewInfo->zoom;
#ifdef USE_MIDI
   if (mouseTrack->GetKind() == Track::Wave) {
      WaveTrack *mtw = (WaveTrack *) mouseTrack;
      desiredSlideAmount = rint(mtw->GetRate() * desiredSlideAmount) / 
                           mtw->GetRate();  // set it to a sample point
   }
   // Adjust desiredSlideAmount using SnapManager
   if (mSnapManager && mCapturedClipArray.GetCount()) {
      double clipLeft; 
      double clipRight;
      if (mCapturedClip) {
         clipLeft = mCapturedClip->GetStartTime() + desiredSlideAmount;
         clipRight = mCapturedClip->GetEndTime() + desiredSlideAmount;
      }
      else {
         clipLeft = mCapturedTrack->GetStartTime() + desiredSlideAmount;
         clipRight = mCapturedTrack->GetEndTime() + desiredSlideAmount;
      }
#else
   desiredSlideAmount = rint(mouseTrack->GetRate() * desiredSlideAmount) / 
                        mouseTrack->GetRate();  // set it to a sample point
   if (mSnapManager && mCapturedClip) {
      double clipLeft = mCapturedClip->GetStartTime() + desiredSlideAmount;
      double clipRight = mCapturedClip->GetEndTime() + desiredSlideAmount;
#endif

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

      // Take whichever one snapped (if any) and compute the new desiredSlideAmount
      mSnapLeft = -1;
      mSnapRight = -1;
      if (newClipLeft != clipLeft) {
         double difference = (newClipLeft - clipLeft);
         desiredSlideAmount += difference;
         mSnapLeft = TimeToPosition(newClipLeft, GetLeftOffset());
      }
      else if (newClipRight != clipRight) {
         double difference = (newClipRight - clipRight);
         desiredSlideAmount += difference;
         mSnapRight = TimeToPosition(newClipRight, GetLeftOffset());
      }
   }

   // Implement sliding within the track(s)
   if (mSlideUpDownOnly) {
      desiredSlideAmount = 0.0;
   }

   // Scroll during vertical drag.
   // EnsureVisible(mouseTrack); //vvv Gale says this has problems on Linux, per bug 393 thread. Revert for 2.0.2.

   //If the mouse is over a track that isn't the captured track,
   //drag the clip to the mousetrack
   if (mCapturedClip && mouseTrack != mCapturedTrack /*&&
       !mCapturedClipIsSelection*/)
   {
      // Make sure we always have the first linked track of a stereo track
      if (!mouseTrack->GetLinked() && mTracks->GetLink(mouseTrack))
         mouseTrack = 
#ifndef USE_MIDI
                      (WaveTrack *)
#endif
                                    mTracks->GetLink(mouseTrack);

      // Temporary apply the offset because we want to see if the
      // track fits with the desired offset
      for(i=0; i<mCapturedClipArray.GetCount(); i++)
         if (mCapturedClipArray[i].clip)
            mCapturedClipArray[i].clip->Offset(desiredSlideAmount);
      // See if it can be moved
      if (MoveClipToTrack(mCapturedClip, 
                          (WaveTrack*)mouseTrack)) {
         mCapturedTrack = mouseTrack;
         mDidSlideVertically = true;

         if (mCapturedClipIsSelection) {
            // Slide the selection, too
            mViewInfo->sel0 += desiredSlideAmount;
            mViewInfo->sel1 += desiredSlideAmount;
         }

         // Make the offset permanent; start from a "clean slate"
         mHSlideAmount = 0.0;
         desiredSlideAmount = 0.0;
         mMouseClickX = event.m_x;
      }
      else {
         // Undo the offset
         for(i=0; i<mCapturedClipArray.GetCount(); i++)
            if (mCapturedClipArray[i].clip)
               mCapturedClipArray[i].clip->Offset(-desiredSlideAmount);
      }

      Refresh(false);
   }

   // Implement sliding within the track(s)
   if (mSlideUpDownOnly)
      return;

   // Determine desired amount to slide
   mHSlideAmount = desiredSlideAmount;

   if (mHSlideAmount == 0.0) {
      Refresh(false);
      return;
   }

#ifdef USE_MIDI
   if (mCapturedClipArray.GetCount()) {
#else
   if (mCapturedClip) {
#endif
      double allowed;
      double initialAllowed;
      double safeBigDistance = 1000 + 2.0 * (mTracks->GetEndTime() -
                                             mTracks->GetStartTime());

      do { // loop to compute allowed, does not actually move anything yet
         initialAllowed = mHSlideAmount;

         unsigned int i, j;
         for(i=0; i<mCapturedClipArray.GetCount(); i++) {
            WaveTrack *track = (WaveTrack *)mCapturedClipArray[i].track;
            WaveClip *clip = mCapturedClipArray[i].clip;

            if (clip) { // only audio clips are used to compute allowed
               // Move all other selected clips totally out of the way
               // temporarily because they're all moving together and
               // we want to find out if OTHER clips are in the way,
               // not one of the moving ones
               for(j=0; j<mCapturedClipArray.GetCount(); j++) {
                  WaveClip *clip2 = mCapturedClipArray[j].clip;
                  if (clip2 && clip2 != clip)
                     clip2->Offset(-safeBigDistance);
               }

               if (track->CanOffsetClip(clip, mHSlideAmount, &allowed)) {
                  mHSlideAmount = allowed;
               }
               else
                  mHSlideAmount = 0.0;

               for(j=0; j<mCapturedClipArray.GetCount(); j++) {
                  WaveClip *clip2 = mCapturedClipArray[j].clip;
                  if (clip2 && clip2 != clip)
                     clip2->Offset(safeBigDistance);
               }
            }
         }
      } while (mHSlideAmount != initialAllowed);

      if (mHSlideAmount != 0.0) { // finally, here is where clips are moved
         unsigned int i;
         for(i=0; i<mCapturedClipArray.GetCount(); i++) {
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
      // For non wavetracks, specifically label tracks ...
      mCapturedTrack->Offset(mHSlideAmount);
      Track* link = mTracks->GetLink(mCapturedTrack);
      if (link)
         link->Offset(mHSlideAmount);
   }
   if (mCapturedClipIsSelection) {
      // Slide the selection, too
      mViewInfo->sel0 += mHSlideAmount;
      mViewInfo->sel1 += mHSlideAmount;
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
   int left, width, height;

   left = GetLeftOffset();
   GetTracksUsableArea(&width, &height);

   mZoomEnd = event.m_x;

   if (event.m_x < left) {
      mZoomEnd = left;
   }
   else if (event.m_x >= left + width - 1) {
      mZoomEnd = left + width - 1;
   }

   if (IsDragZooming()) {
      Refresh(false);
   }
}

/// Zoom button up
void TrackPanel::HandleZoomButtonUp(wxMouseEvent & event)
{
   if (mZoomEnd < mZoomStart) {
      int temp = mZoomEnd;
      mZoomEnd = mZoomStart;
      mZoomStart = temp;
   }

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
bool TrackPanel::IsDragZooming()
{
   return (abs(mZoomEnd - mZoomStart) > DragThreshold);
}

/// Determines if the a modal tool is active
bool TrackPanel::IsMouseCaptured()
{
   return (mMouseCapture != IsUncaptured);
}


///  This actually sets the Zoom value when you're done doing
///  a drag zoom.
void TrackPanel::DragZoom(wxMouseEvent & event, int trackLeftEdge)
{
   double left = PositionToTime(mZoomStart, trackLeftEdge);
   double right = PositionToTime(mZoomEnd, trackLeftEdge);

   if (event.ShiftDown())
      mViewInfo->zoom /= mViewInfo->screen / (right - left);
   else
      mViewInfo->zoom *= mViewInfo->screen / (right - left);

   if (mViewInfo->zoom > gMaxZoom)
      mViewInfo->zoom = gMaxZoom;
   if (mViewInfo->zoom <= gMinZoom)
      mViewInfo->zoom = gMinZoom;

   mViewInfo->h = left;
}

/// This handles normal Zoom In/Out, if you just clicked;
/// IOW, if you were NOT dragging to zoom an area.
/// \todo MAGIC NUMBER: We've got several in this method.
void TrackPanel::DoZoomInOut(wxMouseEvent & event, int trackLeftEdge)
{
   double center_h = PositionToTime(event.m_x, trackLeftEdge);

   if (event.RightUp() || event.RightDClick() || event.ShiftDown())
      mViewInfo->zoom = wxMax(mViewInfo->zoom / 2.0, gMinZoom);
   else
      mViewInfo->zoom = wxMin(mViewInfo->zoom * 2.0, gMaxZoom);

   if (event.MiddleUp() || event.MiddleDClick())
      mViewInfo->zoom = 44100.0 / 512.0;        // AS: Reset zoom.

   double new_center_h = PositionToTime(event.m_x, trackLeftEdge);

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

   // don't do anything if track is not wave or Spectrum/log Spectrum
   if (((mCapturedTrack->GetKind() == Track::Wave) && 
            (((WaveTrack*)mCapturedTrack)->GetDisplay() <= WaveTrack::SpectrumLogDisplay))
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
   int minBins = 0;
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
   WaveTrack *track = (WaveTrack *)mCapturedTrack;
   WaveTrack *partner = (WaveTrack *)mTracks->GetLink(track);
   int height = track->GetHeight();
   int ypos = mCapturedRect.y;

   // Ensure start and end are in order (swap if not).
   if (mZoomEnd < mZoomStart) {
      int temp = mZoomEnd;
      mZoomEnd = mZoomStart;
      mZoomStart = temp;
   }

   float min, max, c, l, binSize = 0.0;
   bool spectrum, spectrumLog;
   int windowSize; 
#ifdef EXPERIMENTAL_FFT_SKIP_POINTS
   int fftSkipPoints=0; 
#endif //EXPERIMENTAL_FFT_SKIP_POINTS
   double rate = ((WaveTrack *)track)->GetRate();
   spectrum = ((WaveTrack *) track)->GetDisplay() == WaveTrack::SpectrumDisplay;
   spectrumLog=(((WaveTrack *) track)->GetDisplay() == WaveTrack::SpectrumLogDisplay);
   if(spectrum) {
      min = mTrackArtist->GetSpectrumMinFreq(0);
      if(min < 0)
         min = 0;
      max = mTrackArtist->GetSpectrumMaxFreq(8000);
      if(max > rate/2.)
         max = rate/2.;
      windowSize = mTrackArtist->GetSpectrumWindowSize();
#ifdef EXPERIMENTAL_FFT_SKIP_POINTS
      fftSkipPoints = mTrackArtist->GetSpectrumFftSkipPoints();
#endif //EXPERIMENTAL_FFT_SKIP_POINTS
      binSize = rate / windowSize;
      minBins = wxMin(10, windowSize/2); //minimum 10 freq bins, unless there are less
   }
   else
      if(spectrumLog) {
         min = mTrackArtist->GetSpectrumLogMinFreq(lrint(rate/1000.0));
         if(min < 1)
            min = 1;
         max = mTrackArtist->GetSpectrumLogMaxFreq(lrint(rate/2.));
         if(max > rate/2.)
            max = rate/2.;
         windowSize = mTrackArtist->GetSpectrumWindowSize();
#ifdef EXPERIMENTAL_FFT_SKIP_POINTS
         fftSkipPoints = mTrackArtist->GetSpectrumFftSkipPoints();
#endif //EXPERIMENTAL_FFT_SKIP_POINTS
         binSize = rate / windowSize;
         minBins = wxMin(10, windowSize/2); //minimum 10 freq bins, unless there are less
      }
      else
         track->GetDisplayBounds(&min, &max);
   if (IsDragZooming()) {
      // Drag Zoom
      float p1, p2, tmin, tmax;
      tmin=min;
      tmax=max;

      if(spectrumLog) {
         double xmin = 1-(mZoomEnd - ypos) / (float)height;
         double xmax = 1-(mZoomStart - ypos) / (float)height;
         double lmin=log10(tmin), lmax=log10(tmax);
         double d=lmax-lmin;
         min=wxMax(1.0, pow(10, xmin*d+lmin));
         max=wxMin(rate/2.0, pow(10, xmax*d+lmin));
         // Enforce vertical zoom limits
         // done in the linear freq domain for now, but not too far out
         if(max < min + minBins * binSize)
            max = min + minBins * binSize;
         if(max > rate/2.) {
            max = rate/2.;
            min = max - minBins * binSize;
         }
      }
      else {
         p1 = (mZoomStart - ypos) / (float)height;
         p2 = (mZoomEnd - ypos) / (float)height;
         max = (tmax * (1.0-p1) + tmin * p1);
         min = (tmax * (1.0-p2) + tmin * p2);

         // Enforce vertical zoom limits
         if(spectrum) {
            if(min < 0.)
               min = 0.;
            if(max < min + minBins * binSize)
               max = min + minBins * binSize;
            if(max > rate/2.) {
               max = rate/2.;
               min = max - minBins * binSize;
            }
         }
         else {
            // Waveform view - allow zooming down to a range of ZOOMLIMIT
            if (max - min < ZOOMLIMIT) {     // if user attempts to go smaller...
               c = (min+max)/2;           // ...set centre of view to centre of dragged area and top/bottom to ZOOMLIMIT/2 above/below
               min = c - ZOOMLIMIT/2.0;
               max = c + ZOOMLIMIT/2.0;
            }
         }
      }
   }
   else if (event.ShiftDown() || event.RightUp()) {
      // Zoom OUT
      // Zoom out to -1.0...1.0 first, then, and only
      // then, if they click again, allow one more
      // zoom out.
      if(spectrum) {
         if (event.ShiftDown() && event.RightUp()) {
			   // Zoom out full
			   min = 0.0;
			   max = rate/2.;
		   }
         else {
            // Zoom out
            c = 0.5*(min+max);
            l = (c - min);
            if(c - 2*l <= 0) {
               min = 0.0;
               max = wxMin( rate/2., 2. * max);
            }
            else {
               min = wxMax( 0.0, c - 2*l);
               max = wxMin( rate/2., c + 2*l);
            }
         }
      }
      else {
         if(spectrumLog) {
            if (event.ShiftDown() && event.RightUp()) {
                // Zoom out full
                min = 1.0;
                max = rate/2.;
            }
            else {
               // Zoom out
               float p1;
               p1 = (mZoomStart - ypos) / (float)height;
               c = 1.0-p1;
               double xmin = c - 1.;
               double xmax = c + 1.;
               double lmin = log10(min), lmax = log10(max);
               double d = lmax-lmin;
               min = wxMax(1,pow(10, xmin*d+lmin));
               max = wxMin(rate/2., pow(10, xmax*d+lmin));
            }
         }
         else {
            if (event.ShiftDown() && event.RightUp()) {
				   // Zoom out full
				   min = -1.0;
				   max = 1.0;
			   }
            else {
               // Zoom out
               if (min <= -1.0 && max >= 1.0) {
                  min = -2.0;
                  max = 2.0;
               }
               else {
                  c = 0.5*(min+max);
                  l = (c - min);
                  min = wxMax( -1.0, c - 2*l);  // limit to +/-1 range
                  max = wxMin(  1.0, c + 2*l);
               }
            }
         }
      }
   }
   else {
      // Zoom IN
      float p1;
      if(spectrum) {
         c = 0.5*(min+max);
         // Enforce maximum vertical zoom
         l = wxMax( minBins * binSize, (c - min));

         p1 = (mZoomStart - ypos) / (float)height;
         c = (max * (1.0-p1) + min * p1);
         min = wxMax( 0.0, c - 0.5*l);
         max = wxMin( rate/2., min + l);
      }
      else {
         if(spectrumLog) {
            p1 = (mZoomStart - ypos) / (float)height;
            c = 1.0-p1;
            double xmin = c - 0.25;
            double xmax = c + 0.25;
            double lmin = log10(min), lmax = log10(max);
            double d = lmax-lmin;
            min = wxMax(1,pow(10, xmin*d+lmin));
            max = wxMin(rate/2., pow(10, xmax*d+lmin));
            // Enforce vertical zoom limits
            // done in the linear freq domain for now, but not too far out
            if(max < min + minBins * binSize)
               max = min + minBins * binSize;
            if(max > rate/2.) {
               max = rate/2.;
               min = max - minBins * binSize;
            }
         }
         else {
            // Zoom in centered on cursor
            if (min < -1.0 || max > 1.0) {
               min = -1.0;
               max = 1.0;
            }
            else {
               c = 0.5*(min+max);
               // Enforce maximum vertical zoom
               l = wxMax( ZOOMLIMIT, (c - min));

               p1 = (mZoomStart - ypos) / (float)height;
               c = (max * (1.0-p1) + min * p1);
               min = c - 0.5*l;
               max = c + 0.5*l;
            }
         }
      }
   }

   if(spectrum) {
      mTrackArtist->SetSpectrumMaxFreq(max);
      mTrackArtist->SetSpectrumMinFreq(min);
      mTrackArtist->InvalidateSpectrumCache(mTracks);
   }
   else if(spectrumLog) {
      mTrackArtist->SetSpectrumLogMaxFreq(max);
      mTrackArtist->SetSpectrumLogMinFreq(min);
      mTrackArtist->InvalidateSpectrumCache(mTracks);
   }
   else {
      track->SetDisplayBounds(min, max);
      if (partner)
         partner->SetDisplayBounds(min, max);
   }

   mZoomEnd = mZoomStart = 0;
   UpdateVRuler(mCapturedTrack);
   Refresh(false);
   mCapturedTrack = NULL;
   MakeParentModifyState(true);
}

/// Determines if we can edit samples in a wave track.
/// Also pops up warning messages in certain cases where we can't.
///  @return true if we can edit the samples, false otherwise.
bool TrackPanel::IsSampleEditingPossible( wxMouseEvent & WXUNUSED(event), Track * t )
{
   //Exit if we don't have a track
   if(!t)
      return false;

   //Exit if it's not a WaveTrack
   if(t->GetKind() != Track::Wave)
      return false;

   //Get out of here if we shouldn't be drawing right now:
   //If we aren't displaying the waveform, Display a message dialog
   if(((WaveTrack *)t)->GetDisplay() != WaveTrack::WaveformDisplay)
   {
      wxMessageBox(_("To use Draw, choose 'Waveform' in the Track Drop-down Menu."), wxT("Draw Tool"));
      return false;
   }

   //Get rate in order to calculate the critical zoom threshold
   //Find out the zoom level
   double rate = ((WaveTrack *)t)->GetRate();
   bool showPoints = (mViewInfo->zoom / rate > 3.0);

   //If we aren't zoomed in far enough, show a message dialog.
   if(!showPoints)
   {
      // Release capture so user will be able to click OK on Linux
      bool hadCapture = HasCapture();
      if (hadCapture)
         ReleaseMouse();

      wxMessageBox(_("To use Draw, zoom in further until you can see the individual samples."), wxT("Draw Tool"));

      // Re-aquire capture
      if (hadCapture)
         CaptureMouse();
      return false;
   }
   return true;
}

/// We're in a track view and zoomed enough to see the samples.
/// Someone has just clicked the mouse.  What do we do?
void TrackPanel::HandleSampleEditingClick( wxMouseEvent & event )
{
   //declare a rectangle to determine clicking position
   wxRect r;
   Track *t;

   //Get the track the mouse is over, and save it away for future events
   mDrawingTrack = NULL;
   t = FindTrack(event.m_x, event.m_y, false, false, &r);

   if (!t || (t->GetKind() != Track::Wave))
      return;

   if( !IsSampleEditingPossible( event, t ) )
   {
      if( HasCapture() )
         ReleaseMouse();
      return;
   }

   /// \todo Should mCapturedTrack take the place of mDrawingTrack??
   mDrawingTrack = t;
   mDrawingTrackTop=r.y;

   //If we are still around, we are drawing in earnest.  Set some member data structures up:
   //First, calculate the starting sample.  To get this, we need the time
   double t0 = PositionToTime(event.m_x, GetLeftOffset());
   double rate = ((WaveTrack *)mDrawingTrack)->GetRate();

   // Default to zero for ALT case, so it doesn't cause a runtime fault on MSVC in 
   // the mDrawingLastDragSampleValue assignment at the bottom of this method. 
   float newLevel = 0.0f;   //Declare this for use later

   //convert t0 to samples
   mDrawingStartSample = (sampleCount) (double)(t0 * rate + 0.5 );

   //Now, figure out what the value of that sample is.
   //First, get the sequence of samples so you can mess with it
   //Sequence *seq = ((WaveTrack *)mDrawingTrack)->GetSequence();


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
      ((WaveTrack*)mDrawingTrack)->Get((samplePtr)sampleRegion, floatSample, 
                                       (int)mDrawingStartSample - SMOOTHING_KERNEL_RADIUS - SMOOTHING_BRUSH_RADIUS,
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


      // Now that the new sample levels are determined, go through each and mix it appropriately
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
      ((WaveTrack*)mDrawingTrack)->Set((samplePtr)newSampleRegion, floatSample, mDrawingStartSample - SMOOTHING_BRUSH_RADIUS, 1 + 2 * SMOOTHING_BRUSH_RADIUS);

      //Clean this up right away to avoid a memory leak
      delete[] sampleRegion;
      delete[] newSampleRegion;
   } 
   else 
   {
      //*************************************************
      //***   PLAIN DOWN-CLICK (NORMAL DRAWING)       ***
      //*************************************************

      //Otherwise (e.g., the alt button is not down) do normal redrawing, based on the mouse position.
      // Calculate where the mouse is located vertically (between +/- 1)
      ((WaveTrack*)mDrawingTrack)->Get((samplePtr)&mDrawingStartSampleValue, floatSample,(int) mDrawingStartSample, 1);
      float zoomMin, zoomMax;
      ((WaveTrack *)mDrawingTrack)->GetDisplayBounds(&zoomMin, &zoomMax);
      newLevel = zoomMax -
         ((event.m_y - mDrawingTrackTop)/(float)mDrawingTrack->GetHeight()) *
         (zoomMax - zoomMin);

      //Take the envelope into account
      Envelope *env = ((WaveTrack *)mDrawingTrack)->GetEnvelopeAtX(event.GetX());

      if (env)
      {
         double envValue = env->GetValue(t0);
         if (envValue > 0)
            newLevel /= envValue;
         else
            newLevel = 0;

         //Make sure the new level is between +/-1
         newLevel = newLevel >  1.0 ?  1.0: newLevel;
         newLevel = newLevel < -1.0 ? -1.0: newLevel;
      }

      //Set the sample to the point of the mouse event
      ((WaveTrack*)mDrawingTrack)->Set((samplePtr)&newLevel, floatSample, mDrawingStartSample, 1);
   }

   //Set the member data structures for drawing
   mDrawingLastDragSample=mDrawingStartSample;
   mDrawingLastDragSampleValue = newLevel;

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
   if (event.m_altDown)
      return;

   //Get the rate of the sequence, for use later
   float rate = ((WaveTrack *)mDrawingTrack)->GetRate();
   sampleCount s0;     //declare this for use below.  It designates the sample number which to draw.

   // Figure out what time the click was at
   double t0 = PositionToTime(event.m_x, GetLeftOffset());
   float newLevel;
   //Find the point that we want to redraw at. If the control button is down, 
   //adjust only the originally clicked-on sample 

   //*************************************************
   //***   CTRL-DOWN (Hold Initial Sample Constant ***
   //*************************************************

   if( event.m_controlDown) {
      s0 = mDrawingStartSample;
   } 
   else 
   {
      //*************************************************
      //***    Normal CLICK-drag  (Normal drawing)    ***
      //*************************************************

      //Otherwise, adjust the sample you are dragging over right now.
      //convert this to samples
      s0 = (sampleCount) (double)(t0 * rate + 0.5);
   }

   //Sequence *seq = ((WaveTrack *)mDrawingTrack)->GetSequence();
   ((WaveTrack*)mDrawingTrack)->Get((samplePtr)&mDrawingStartSampleValue, floatSample, (int)mDrawingStartSample, 1);

   //Otherwise, do normal redrawing, based on the mouse position.
   // Calculate where the mouse is located vertically (between +/- 1)


   float zoomMin, zoomMax;
   ((WaveTrack *)mDrawingTrack)->GetDisplayBounds(&zoomMin, &zoomMax);
   newLevel = zoomMax -
      ((event.m_y - mDrawingTrackTop)/(float)mDrawingTrack->GetHeight()) *
      (zoomMax - zoomMin);

   //Take the envelope into account
   Envelope *env = ((WaveTrack *)mDrawingTrack)->GetEnvelopeAtX(event.GetX());
   if (env)
   {
      double envValue = env->GetValue(t0);
      if (envValue > 0)
         newLevel /= envValue;
      else
         newLevel = 0;

      //Make sure the new level is between +/-1
      newLevel = newLevel >  1.0 ?  1.0: newLevel;
      newLevel = newLevel < -1.0 ? -1.0: newLevel;
   }

   //Now, redraw all samples between current and last redrawn sample

   float tmpvalue;
   //Handle cases of 0 or 1 special, to improve speed
   //JKC I don't think this makes any noticeable difference to speed
   // whatsoever!  The real reason for the special case is probably to 
   // avoid division by zero....
#define LLABS(n) ((n) < 0 ? -(n) : (n))
   if(LLABS(s0 - mDrawingLastDragSample) <= 1){
      ((WaveTrack*)mDrawingTrack)->Set((samplePtr)&newLevel,  floatSample, s0, 1);
   }
   else 
   {
      //Go from the smaller to larger sample. 
      int start = wxMin( s0, mDrawingLastDragSample) +1;
      int end   = wxMax( s0, mDrawingLastDragSample);
      for(sampleCount i= start; i<= end; i++) {
         //This interpolates each sample linearly:
         tmpvalue=mDrawingLastDragSampleValue + (newLevel - mDrawingLastDragSampleValue)  * 
            (float)(i-mDrawingLastDragSample)/(s0-mDrawingLastDragSample );
         ((WaveTrack*)mDrawingTrack)->Set((samplePtr)&tmpvalue, floatSample, i, 1);
      }
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

   //On up-click, send the state to the undo stack
   mDrawingTrack=NULL;       //Set this to NULL so it will catch improper drag events.
   MakeParentPushState(_("Moved Sample"),
                       _("Sample Edit"),
                       PUSH_CONSOLIDATE|PUSH_AUTOSAVE|PUSH_CALC_SPACE);
}


/// This handles adjusting individual samples by hand using the draw tool(s)
///
/// There are several member data structure for handling drawing:
///  - mDrawingTrack:               keeps track of which track you clicked down on, so drawing doesn't 
///                                 jump to a new track
///  - mDrawingTrackTop:            The top position of the drawing track--makes drawing easier.
///  - mDrawingStartSample:         The sample you clicked down on, so that you can hold it steady
///  - mDrawingStartSampleValue:    The original value of the initial sample
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
   AudacityProject *p = GetProject(); //lda

   Track *t = mCapturedTrack;
   wxRect r = mCapturedRect;

   wxRect closeRect;
   mTrackInfo.GetCloseBoxRect(r, closeRect);

   wxClientDC dc(this);

   if (event.Dragging())
      mTrackInfo.DrawCloseBox(&dc, r, closeRect.Contains(event.m_x, event.m_y));
   else if (event.LeftUp()) {
      mTrackInfo.DrawCloseBox(&dc, r, false);
      if (closeRect.Contains(event.m_x, event.m_y)) {
         if (!gAudioIO->IsStreamActive(p->GetAudioIOToken()))
            RemoveTrack(t);
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
      mViewInfo->zoom = 44100.0 / 512.0;

      //STM: Set selection to 0,0
      mViewInfo->sel0 = 0.0;
      mViewInfo->sel1 = 0.0;

      mListener->TP_RedrawScrollbars();
      mListener->TP_DisplayStatusMessage(wxT("")); //STM: Clear message if all tracks are removed
   }
}

/// Removes the specified track.  Called from HandleClosing.
void TrackPanel::RemoveTrack(Track * toRemove)
{
   // If it was focused, reassign focus to the next or, if
   // unavailable, the previous track.
   if (GetFocusedTrack() == toRemove) {
      Track *t = mTracks->GetNext(toRemove, true);
      if (t == NULL) {
         t = mTracks->GetPrev( toRemove, true );
      }
      SetFocusedTrack(t);  // It's okay if this is NULL
   }

   wxString name = toRemove->GetName();
   Track *partner = toRemove->GetLink();

   if (toRemove->GetKind() == Track::Wave)
   {
      // Update mixer board displayed tracks.
      MixerBoard* pMixerBoard = this->GetMixerBoard(); 
      if (pMixerBoard)
         pMixerBoard->RemoveTrackCluster((WaveTrack*)toRemove); // Will remove partner shown in same cluster.
   }

   mTracks->Remove(toRemove, true);
   if (partner) {
      mTracks->Remove(partner, true);
   }

   if (mTracks->IsEmpty()) {
      SetFocusedTrack( NULL );
   }

   MakeParentPushState(
      wxString::Format(_("Removed track '%s.'"),
      name.c_str()),
      _("Track Remove"));
   MakeParentRedrawScrollbars();
   MakeParentResize();
   Refresh(false);
}

void TrackPanel::HandlePopping(wxMouseEvent & event)
{
   Track *t = mCapturedTrack;
   wxRect r = mCapturedRect;

   if( t==NULL ){
      SetCapturedTrack( NULL );
      return;
   }

   wxRect titleRect;
   mTrackInfo.GetTitleBarRect(r, titleRect);

   wxClientDC dc(this);

   if (event.Dragging()) {
      mTrackInfo.DrawTitleBar(&dc, r, t, titleRect.Contains(event.m_x, event.m_y));
   }
   else if (event.LeftUp()) {
      if (titleRect.Contains(event.m_x, event.m_y))
      {
         OnTrackMenu(t);
      }

      SetCapturedTrack( NULL );

      mTrackInfo.DrawTitleBar(&dc, r, t, false);
   }
}

/// Handle when the mute or solo button is pressed for some track.
void TrackPanel::HandleMutingSoloing(wxMouseEvent & event, bool solo)
{
   Track *t = mCapturedTrack;
   wxRect r = mCapturedRect;

   if( t==NULL ){
      wxASSERT(false);// Soloing or muting but no captured track!
      SetCapturedTrack( NULL );
      return;
   }

   wxRect buttonRect;
   mTrackInfo.GetMuteSoloRect(r, buttonRect, solo, HasSoloButton());

   wxClientDC dc(this);

   if (event.Dragging()){
         mTrackInfo.DrawMuteSolo(&dc, r, t, buttonRect.Contains(event.m_x, event.m_y),
                   solo, HasSoloButton());
   }
   else if (event.LeftUp() )
   {
      if (buttonRect.Contains(event.m_x, event.m_y)) 
      {
         // For either, MakeParentPushState to make the track state dirty.
         if(solo)
            OnTrackSolo(event.ShiftDown(),t);
         else
            OnTrackMute(event.ShiftDown(),t);
      }  
      SetCapturedTrack( NULL );
      // mTrackInfo.DrawMuteSolo(&dc, r, t, false, solo);
      Refresh(false);
   }
}

void TrackPanel::HandleMinimizing(wxMouseEvent & event)
{
   Track *t = mCapturedTrack;
   wxRect r = mCapturedRect;

   if (t == NULL) {
      SetCapturedTrack(NULL);
      return;
   }

   wxRect buttonRect;
   mTrackInfo.GetMinimizeRect(r, buttonRect);

   wxClientDC dc(this);

   if (event.Dragging()) {
      mTrackInfo.DrawMinimize(&dc, r, t, buttonRect.Contains(event.m_x, event.m_y));
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

      mTrackInfo.DrawMinimize(&dc, r, t, false);
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

   if (pan)
      slider = mTrackInfo.PanSlider(mCapturedTrack->GetIndex());
   else
      slider = mTrackInfo.GainSlider(mCapturedTrack->GetIndex());

   slider->OnMouseEvent(event);

   //If we have a double-click, do this...
   if (event.LeftDClick())
      mMouseCapture = IsUncaptured;

   float newValue = slider->Get();
   MixerBoard* pMixerBoard = this->GetMixerBoard(); // Update mixer board, too.
   #ifdef EXPERIMENTAL_MIDI_OUT
  if (mCapturedTrack->GetKind() == Track::Wave) {
   #endif
   WaveTrack *link = (WaveTrack *)mTracks->GetLink(mCapturedTrack);

   if (pan) {
#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
      panZero = ((WaveTrack *)mCapturedTrack)->SetPan(newValue);
#else
      ((WaveTrack *)mCapturedTrack)->SetPan(newValue);
#endif
      if (link)
         link->SetPan(newValue);

#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
      if(panZero) MakeParentRedrawScrollbars();
#endif

      if (pMixerBoard) 
         pMixerBoard->UpdatePan((WaveTrack*)mCapturedTrack);
   }
   else {
      ((WaveTrack *)mCapturedTrack)->SetGain(newValue);
      if (link)
         link->SetGain(newValue);

      if (pMixerBoard) 
         pMixerBoard->UpdateGain((WaveTrack*)mCapturedTrack);
   }
   #ifdef EXPERIMENTAL_MIDI_OUT
  } else { // Note: funny indentation to match "if" about 20 lines back
      if (!pan) {
         ((NoteTrack *) mCapturedTrack)->SetGain(newValue);
         #ifdef EXPERIMENTAL_MIXER_BOARD
            if (pMixerBoard)
               // probably should modify UpdateGain to take a track that is
               // either a WaveTrack or a NoteTrack.
               pMixerBoard->UpdateGain((WaveTrack*)mCapturedTrack);
         #endif
      }
   }
   #endif

   VisibleTrackIterator iter(GetProject());
   for (Track *t = iter.First(); t; t = iter.Next())
   {
      RefreshTrack(t);
   }

   if (event.ButtonUp()) {
   #ifdef EXPERIMENTAL_MIDI_OUT
    if (mCapturedTrack->GetKind() == Track::Wave) {
   #endif
      MakeParentPushState(pan ? _("Moved pan slider") : _("Moved gain slider"),
                          pan ? _("Pan") : _("Gain"),
                          PUSH_CONSOLIDATE);
   #ifdef EXPERIMENTAL_MIDI_OUT
    } else {
      MakeParentPushState(_("Moved velocity slider"), _("Velocity"), true);
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

   AudacityProject *p = GetProject();
   bool unsafe = (p->GetAudioIOToken()>0 &&
                  gAudioIO->IsStreamActive(p->GetAudioIOToken()));

   wxRect r;

   Track *t = FindTrack(event.m_x, event.m_y, true, true, &r);

   // AS: If the user clicked outside all tracks, make nothing
   //  selected.
   if (!t) {
      SelectNone();
      Refresh(false);
      return;
   }

   // LL: Check close box
   if (isleft && CloseFunc(t, r, event.m_x, event.m_y))
      return;

   // LL: Check title bar for popup
   if (isleft && PopupFunc(t, r, event.m_x, event.m_y))
      return;

   // VJ: Check sync-lock icon and the blank area to the left of the minimize button. 
   // Have to do it here, because if track is shrunk such that these areas occlude controls, 
   // e.g., mute/solo, don't want the "Funcs" below to set up handling. 
   // Only result of doing so is to select the track. Don't care whether isleft.
   bool bTrackSelClick = this->TrackSelFunc(t, r, event.m_x, event.m_y);
   if (!bTrackSelClick) 
   {
      // MM: Check minimize buttons on WaveTracks. Must be before
      //     solo/mute buttons, sliders etc.
      if (isleft && MinimizeFunc(t, r, event.m_x, event.m_y))
         return;

      if (isleft && t->GetKind() == Track::Wave)
      {
         // DM: Check Mute and Solo buttons on WaveTracks:
         if (MuteSoloFunc(t, r, event.m_x, event.m_y, false) ||
            MuteSoloFunc(t, r, event.m_x, event.m_y, true))
            return;

         if (GainFunc(t, r, event, event.m_x, event.m_y))
            return;

         if (PanFunc(t, r, event, event.m_x, event.m_y))
            return;
      }
      #ifdef USE_MIDI
      // DM: If it's a NoteTrack, it has special controls
      else if (t->GetKind() == Track::Note)
      {
         wxRect midiRect;
         #ifdef EXPERIMENTAL_MIDI_OUT
            // this is an awful hack: make a new rectangle at an offset because
            // MuteSoloFunc thinks buttons are located below some text, e.g.
            // "Mono, 44100Hz 32-bit float", but this is not true for a Note track
            wxRect muteSoloRect(r);
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
         mTrackInfo.GetTrackControlsRect(r, midiRect);
         if (midiRect.Contains(event.m_x, event.m_y) &&
             ((NoteTrack *) t)->LabelClick(midiRect, event.m_x, event.m_y,
                                      event.Button(wxMOUSE_BTN_RIGHT))) {
            Refresh(false);
            return;
         }
      }
      #endif // USE_MIDI

      if (!isleft) {
         return;
      }
   }

   // DM: If they weren't clicking on a particular part of a track label,
   //  deselect other tracks and select this one.

   // JH: also, capture the current track for rearranging, so the user
   //  can drag the track up or down to swap it with others
   if (!unsafe) {
      SetCapturedTrack( t, IsRearranging );
      TrackPanel::CalculateRearrangingThresholds(event);
   }

   // AS: If the shift button is being held down, invert 
   //  the selection on this track.
   if (event.ShiftDown()) {
      mTracks->Select(t, !t->GetSelected());
      Refresh(false);
      MixerBoard* pMixerBoard = this->GetMixerBoard();
      if (pMixerBoard && (t->GetKind() == Track::Wave))
         pMixerBoard->RefreshTrackCluster((WaveTrack*)t);
      return;
   }

   SelectNone();
   mTracks->Select(t);
   SetFocusedTrack(t);
   SelectTrackLength(t);

   this->Refresh(false);  
   MixerBoard* pMixerBoard = this->GetMixerBoard();
   if (pMixerBoard)
      pMixerBoard->RefreshTrackClusters();

   if (!unsafe)
      MakeParentModifyState(true);
}

/// The user is dragging one of the tracks: change the track order
/// accordingly
void TrackPanel::HandleRearrange(wxMouseEvent & event)
{
   // are we finishing the drag?
   if (event.LeftUp()) {
      SetCapturedTrack( NULL );
      SetCursor(*mArrowCursor);
      return;
   }

   MixerBoard* pMixerBoard = this->GetMixerBoard(); // Update mixer board, too.
   wxString dir;
   if (event.m_y < mMoveUpThreshold || event.m_y < 0) {
      mTracks->MoveUp(mCapturedTrack);
      dir = _("up");
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
      /* i18n-hint: a direction as in up or down.*/
      dir = _("down");
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

   MakeParentPushState(wxString::Format(_("Moved '%s' %s"),
                                        mCapturedTrack->GetName().c_str(),
                                        dir.c_str()),
                       _("Move Track"));

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

bool TrackPanel::GainFunc(Track * t, wxRect r, wxMouseEvent &event,
                          int x, int y)
{
   wxRect sliderRect;
   mTrackInfo.GetGainRect(r, sliderRect);
   if (!sliderRect.Contains(x, y)) 
      return false;

   SetCapturedTrack( t, IsGainSliding);
   mCapturedRect = r;
   HandleSliders(event, false);

   return true;
}

bool TrackPanel::PanFunc(Track * t, wxRect r, wxMouseEvent &event,
                         int x, int y)
{
   wxRect sliderRect;
   mTrackInfo.GetPanRect(r, sliderRect);
   if (!sliderRect.Contains(x, y))
      return false;

   SetCapturedTrack( t, IsPanSliding);
   mCapturedRect = r;
   HandleSliders(event, true);

   return true;
}

/// Mute or solo the given track (t).  If solo is true, we're 
/// soloing, otherwise we're muting.  Basically, check and see 
/// whether x and y fall within the  area of the appropriate button.
bool TrackPanel::MuteSoloFunc(Track * t, wxRect r, int x, int y,
                              bool solo)
{
   wxRect buttonRect;
   mTrackInfo.GetMuteSoloRect(r, buttonRect, solo, HasSoloButton());
   if (!buttonRect.Contains(x, y)) 
      return false;

   wxClientDC dc(this);
   SetCapturedTrack( t, solo ? IsSoloing : IsMuting);
   mCapturedRect = r;

   mTrackInfo.DrawMuteSolo(&dc, r, t, true, solo, HasSoloButton());
   return true;
}

bool TrackPanel::TrackSelFunc(Track * WXUNUSED(t), wxRect r, int x, int y)
{
   // First check the blank space to left of minimize button.
   wxRect selRect;
   mTrackInfo.GetMinimizeRect(r, selRect); // for y and height
   selRect.x = r.x;
   selRect.width = 16; // (kTrackInfoBtnSize)
   selRect.height++;
   if (selRect.Contains(x, y))
      return true;

   // Try the sync-lock rect.
   mTrackInfo.GetSyncLockIconRect(r, selRect);
   selRect.height++;
   return selRect.Contains(x, y);
}

bool TrackPanel::MinimizeFunc(Track * t, wxRect r, int x, int y)
{
   wxRect buttonRect;
   mTrackInfo.GetMinimizeRect(r, buttonRect);
   if (!buttonRect.Contains(x, y)) 
      return false;

   SetCapturedTrack(t, IsMinimizing);
   mCapturedRect = r;

   wxClientDC dc(this);
   mTrackInfo.DrawMinimize(&dc, r, t, true);

   return true;
}

bool TrackPanel::CloseFunc(Track * t, wxRect r, int x, int y)
{
   wxRect closeRect;
   mTrackInfo.GetCloseBoxRect(r, closeRect);

   if (!closeRect.Contains(x, y))
      return false;

   wxClientDC dc(this);
   SetCapturedTrack( t, IsClosing );
   mCapturedRect = r;

   mTrackInfo.DrawCloseBox(&dc, r, true);
   return true;
}

bool TrackPanel::PopupFunc(Track * t, wxRect r, int x, int y)
{
   wxRect titleRect;
   mTrackInfo.GetTitleBarRect(r, titleRect);
   if (!titleRect.Contains(x, y)) 
      return false;

   wxClientDC dc(this);
   SetCapturedTrack( t, IsPopping );
   mCapturedRect = r;

   mTrackInfo.DrawTitleBar(&dc, r, t, true);
   return true;
}

///  ButtonDown means they just clicked and haven't released yet.
///  We use this opportunity to save which track they clicked on,
///  and the initial height of the track, so as they drag we can
///  update the track size.
void TrackPanel::HandleResizeClick( wxMouseEvent & event )
{
   wxRect r;
   wxRect rLabel;

   // DM: Figure out what track is about to be resized
   Track *t = FindTrack(event.m_x, event.m_y, false, false, &r);
   Track *label = FindTrack(event.m_x, event.m_y, true, true, &rLabel);

   // If the click is at the bottom of a non-linked track label, we
   // treat it as a normal resize.  If the label is of a linked track,
   // we ignore the click.

   if (label && !label->GetLinked()) {
      t = label;
   }

   if (!t) {
      return;
   }

   mMouseClickX = event.m_x;
   mMouseClickY = event.m_y;

#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
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
         mInitialUpperTrackHeight = prev->GetHeight();
         SetCapturedTrack(t, IsResizingBelowLinkedTracks);
      }
      else if (next && t->GetLink() == next) {
         // mCapturedTrack is the upper track
         mInitialTrackHeight = next->GetHeight();
         mInitialUpperTrackHeight = t->GetHeight();
         SetCapturedTrack(t, IsResizingBetweenLinkedTracks);
      }
      else {
         // DM: Save the initial mouse location and the initial height
         mInitialTrackHeight = t->GetHeight();
         SetCapturedTrack(t, IsResizing);
      }
   }
#else // EXPERIMENTAL_OUTPUT_DISPLAY
   Track *prev = mTracks->GetPrev(t);
   Track *next = mTracks->GetNext(t);

   //STM:  Determine whether we should rescale one or two tracks
   if (prev && prev->GetLink() == t) {
      // mCapturedTrack is the lower track
      mInitialTrackHeight = t->GetHeight();
      mInitialUpperTrackHeight = prev->GetHeight();
      SetCapturedTrack(t, IsResizingBelowLinkedTracks);
   }
   else if (next && t->GetLink() == next) {
      // mCapturedTrack is the upper track
      mInitialTrackHeight = next->GetHeight();
      mInitialUpperTrackHeight = t->GetHeight();
      SetCapturedTrack(t, IsResizingBetweenLinkedTracks);
   }
   else {
      // DM: Save the initial mouse location and the initial height
      mInitialTrackHeight = t->GetHeight();
      SetCapturedTrack(t, IsResizing);
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
   double steps = event.m_wheelRotation /
      (event.m_wheelDelta > 0 ? (double)event.m_wheelDelta : 120.0);

   if (event.ShiftDown())
   {
      // MM: Scroll left/right when used with Shift key down
      mListener->TP_ScrollWindow(
         mViewInfo->h +
         50.0 * -steps / mViewInfo->zoom);
   } else if (event.CmdDown())
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
      double center_h = PositionToTime(event.m_x, trackLeftEdge);
      // Time corresponding to last (most far right) audio.
      double audioEndTime = mTracks->GetEndTime();

      // When zooming in in empty space, it's easy to 'lose' the waveform.
      // This prevents it.
      // IF zooming in
      if( steps > 0) 
      {
         // IF mouse is to right of audio
         if( center_h > audioEndTime )
            // Zooming brings far right of audio to mouse.
            center_h = audioEndTime;
      }

      // Constrain maximum as well as minimum zoom.
      mViewInfo->zoom = wxMax( gMinZoom, wxMin(mViewInfo->zoom * pow(2.0, steps), gMaxZoom));

      double new_center_h = PositionToTime(event.m_x, trackLeftEdge);
      mViewInfo->h += (center_h - new_center_h);

      MakeParentRedrawScrollbars();
      Refresh(false);
   } else
   {
      // MM: Scroll up/down when used without modifier keys
      double lines = steps * 4 + mVertScrollRemainder;
      mVertScrollRemainder = lines - floor(lines);
      lines = floor(lines);
      mListener->TP_ScrollUpDown((int)-lines);
   }
}

/// Filter captured keys typed into LabelTracks.
void TrackPanel::OnCaptureKey(wxCommandEvent & event)
{
   // Only deal with LabelTracks
   Track *t = GetFocusedTrack();
   if (!t || t->GetKind() != Track::Label) {
      event.Skip();
      return;
   }
   wxKeyEvent *kevent = (wxKeyEvent *)event.GetEventObject();

   event.Skip(!((LabelTrack *)t)->CaptureKey(*kevent));
}

/// Allow typing into LabelTracks.
void TrackPanel::OnKeyDown(wxKeyEvent & event)
{
   // Only deal with LabelTracks
   Track *t = GetFocusedTrack();
   if (!t || t->GetKind() != Track::Label) {
      event.Skip();
      return;
   }

   LabelTrack *lt = (LabelTrack *)t;
   double bkpSel0 = mViewInfo->sel0, bkpSel1 = mViewInfo->sel1;

   // Pass keystroke to labeltrack's handler and add to history if any
   // updates were done
   if (lt->OnKeyDown(mViewInfo->sel0, mViewInfo->sel1, event))
      MakeParentPushState(_("Modified Label"),
                          _("Label Edit"),
                          PUSH_CONSOLIDATE);

   // Make sure caret is in view
   int x;
   if (lt->CalcCursorX(this, &x)) {
      ScrollIntoView(x);
   }

   // If selection modified, refresh
   // Otherwise, refresh track display if the keystroke was handled
   if( bkpSel0 != mViewInfo->sel0 || bkpSel1 != mViewInfo->sel1 )
      Refresh( false );
   else if (!event.GetSkipped()) 
      RefreshTrack(t);
}

/// Allow typing into LabelTracks.
void TrackPanel::OnChar(wxKeyEvent & event)
{
#if defined (__WXGTK__)
   // AWD: workaround for bug 154
   bool restore = wxGetApp().inKbdHandler;
   wxGetApp().inKbdHandler = true;
#endif

   // Only deal with LabelTracks
   Track *t = GetFocusedTrack();
   if (!t || t->GetKind() != Track::Label) {
      event.Skip();
      return;
   }

   double bkpSel0 = mViewInfo->sel0, bkpSel1 = mViewInfo->sel1;
   // Pass keystroke to labeltrack's handler and add to history if any
   // updates were done
   if (((LabelTrack *)t)->OnChar(mViewInfo->sel0, mViewInfo->sel1, event))
      MakeParentPushState(_("Modified Label"),
                          _("Label Edit"),
                          PUSH_CONSOLIDATE);

   // If selection modified, refresh
   // Otherwise, refresh track display if the keystroke was handled
   if( bkpSel0 != mViewInfo->sel0 || bkpSel1 != mViewInfo->sel1 )
      Refresh( false );
   else if (!event.GetSkipped()) 
      RefreshTrack(t);

#if defined (__WXGTK__)
   // AWD: workaround for bug 154
   wxGetApp().inKbdHandler = restore;
#endif
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
   if (event.m_wheelRotation != 0)
      HandleWheelRotation(event);

   if (!mAutoScrolling) {
      mMouseMostRecentX = event.m_x;
      mMouseMostRecentY = event.m_y;
   }

   if (event.LeftDown()) {
      mCapturedTrack = NULL;

      // The activate event is used to make the 
      // parent window 'come alive' if it didn't have focus.
      wxActivateEvent e;
      GetParent()->ProcessEvent(e);

      // wxTimers seem to be a little unreliable, so this
      // "primes" it to make sure it keeps going for a while...

      // When this timer fires, we call TrackPanel::OnTimer and
      // possibly update the screen for offscreen scrolling.
      mTimer.Stop();
      mTimer.Start(kTimerInterval, FALSE);
   }

   if (event.ButtonDown()) {
      SetFocus();
      CaptureMouse();
   }
   else if (event.ButtonUp()) {
      if (HasCapture())
         ReleaseMouse();
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
      HandleLabelTrackMouseEvent((LabelTrack *)mCapturedTrack, mCapturedRect, event);
      break;
   default: //includes case of IsUncaptured
      HandleTrackSpecificMouseEvent(event);
      break;
   }

   //EnsureVisible should be called after the up-click.
   if (event.ButtonUp()) {
      wxRect r;

      Track *t = FindTrack(event.m_x, event.m_y, false, false, &r);
      if (t)
         EnsureVisible(t);
   }
}

bool TrackPanel::HandleTrackLocationMouseEvent(WaveTrack * track, wxRect &r, wxMouseEvent &event)
{
   // FIXME: Disable this and return true when CutLines aren't showing?
   // (Don't use gPrefs-> for the fix as registry access is slow).

   if (mMouseCapture == WasOverCutLine)
   {
      // Needed to avoid select events right after IsOverCutLine event on button up
      if (event.ButtonUp()) {
         mMouseCapture = IsUncaptured;
         return false;
      }
      return true;
   }

   if (mMouseCapture == IsOverCutLine)
   {
      if (!mCapturedTrackLocationRect.Contains(event.m_x, event.m_y))
      {
         SetCapturedTrack( NULL );
         SetCursorByActivity();
         return false;
      }

      bool handled = false;

      if (event.LeftDown())
      {
         if (mCapturedTrackLocation.typ == WaveTrack::locationCutLine)
         {
            // When user presses left button on cut line, expand the line again
            double cutlineStart = 0, cutlineEnd = 0;

            // Release capture so user will be able to click OK on Linux
            if (HasCapture())
               ReleaseMouse();

            if (track->ExpandCutLine(mCapturedTrackLocation.pos, &cutlineStart, &cutlineEnd))
            {
               WaveTrack* linked = (WaveTrack*)mTracks->GetLink(track);
               if (linked && 
                     !linked->ExpandCutLine(mCapturedTrackLocation.pos))
                        return false;

               mViewInfo->sel0 = cutlineStart;
               mViewInfo->sel1 = cutlineEnd;
               DisplaySelection();
               MakeParentPushState(_("Expanded Cut Line"), _("Expand"));
               handled = true;
            }
         } else if (mCapturedTrackLocation.typ == WaveTrack::locationMergePoint)
         {
            if (!track->MergeClips(mCapturedTrackLocation.clipidx1, mCapturedTrackLocation.clipidx2))
               return false;

            WaveTrack* linked = (WaveTrack*)mTracks->GetLink(track);
            if (linked && 
                  !linked->MergeClips(mCapturedTrackLocation.clipidx1, mCapturedTrackLocation.clipidx2))
                     return false;

            MakeParentPushState(_("Merged Clips"),_("Merge"), PUSH_CONSOLIDATE|PUSH_CALC_SPACE);
            handled = true;
         }
      }

      if (!handled && event.RightDown())
      {
         track->RemoveCutLine(mCapturedTrackLocation.pos);
         WaveTrack* linked = (WaveTrack*)mTracks->GetLink(track);
         if (linked)
            linked->RemoveCutLine(mCapturedTrackLocation.pos);
         MakeParentPushState(_("Removed Cut Line"), _("Remove") );
         handled = true;
      }

      if (handled)
      {
         SetCapturedTrack( NULL );
         mMouseCapture = WasOverCutLine; 
         RefreshTrack(track);
         return true;
      }

      return true;
   }

   for (int i=0; i<track->GetNumCachedLocations(); i++)
   {
      WaveTrack::Location loc = track->GetCachedLocation(i);

      double x = (loc.pos - mViewInfo->h) * mViewInfo->zoom;
      if (x >= 0 && x < r.width)
      {
         wxRect locRect;
         locRect.x = int( r.x + x ) - 5;
         locRect.width = 11;
         locRect.y = r.y;
         locRect.height = r.height;
         if (locRect.Contains(event.m_x, event.m_y))
         {
            SetCapturedTrack(track, IsOverCutLine);
            mCapturedTrackLocation = loc;
            mCapturedTrackLocationRect = locRect;
            SetCursorByActivity();
            return true;
         }
      }
   }

   return false;
}


/// Event has happened on a track and it has been determined to be a label track.
bool TrackPanel::HandleLabelTrackMouseEvent(LabelTrack * lTrack, wxRect &r, wxMouseEvent & event)
{
   /// \todo This method is one of a large number of methods in 
   /// TrackPanel which suitably modified belong in other classes.
   if(event.LeftDown())
   {
      TrackListIterator iter(mTracks);
      Track *n = iter.First();

      while (n) {
         if (n->GetKind() == Track::Label && lTrack != n) {
            ((LabelTrack *)n)->ResetFlags();
            ((LabelTrack *)n)->Unselect();
         }
         n = iter.Next();
      }

      //If the button was pressed, check to see if we are over
      //a glyph (this is the second of three calls to the method).
      //std::cout << ((LabelTrack*)pTrack)->OverGlyph(event.m_x, event.m_y) << std::endl;
      if(lTrack->OverGlyph(event.m_x, event.m_y))
      {
         SetCapturedTrack(lTrack, IsAdjustingLabel);
         mCapturedRect = r; 
         mCapturedRect.x += kLeftInset;
         mCapturedRect.width -= kLeftInset;
      }
   } else if (event.Dragging()) {
      ;
   } else if( event.LeftUp()) {
      SetCapturedTrack( NULL );
   }

   if (lTrack->HandleMouse(event, mCapturedRect,
      mViewInfo->h, mViewInfo->zoom, &mViewInfo->sel0, &mViewInfo->sel1)) {

      MakeParentPushState(_("Modified Label"),
                          _("Label Edit"),
                          PUSH_CONSOLIDATE);
   }


   if (event.RightUp()) {
      // popup menu for editing
      RefreshTrack(lTrack);

      if ((lTrack->getSelectedIndex() != -1) && lTrack->OverTextBox(lTrack->GetLabel(lTrack->getSelectedIndex()), event.m_x, event.m_y)) {
         mPopupMenuTarget = lTrack;
         mLabelTrackInfoMenu->Enable(OnCutSelectedTextID, lTrack->IsTextSelected());
         mLabelTrackInfoMenu->Enable(OnCopySelectedTextID, lTrack->IsTextSelected());
         mLabelTrackInfoMenu->Enable(OnPasteSelectedTextID, lTrack->IsTextClipSupported());
         PopupMenu(mLabelTrackInfoMenu, event.m_x + 1, event.m_y + 1);
         // it's an invalid dragging event
         lTrack->SetWrongDragging(true);
      }
      return true;
   }

   //If we are adjusting a label on a labeltrack, do not do anything 
   //that follows. Instead, redraw the track.
   if(mMouseCapture == IsAdjustingLabel)
   {
      RefreshTrack(lTrack);
      return true;
   }

   // handle dragging
   if(event.Dragging()) {
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
            return true;
         }
      }
   }

   // handle mouse left button up
   if (event.LeftUp()) {
      mLabelTrackStartXPos = -1;
   }

   // handle shift+ctrl down
   /*if (event.ShiftDown()) { // && event.ControlDown()) {
      lTrack->SetHighlightedByKey(true);
      Refresh(false);
      return;
   }*/

   // handle shift+mouse left button
   if (event.ShiftDown() && event.ButtonDown() && (lTrack->getSelectedIndex() != -1)) {
      // if the mouse is clicked in text box, set flags
      if (lTrack->OverTextBox(lTrack->GetLabel(lTrack->getSelectedIndex()), event.m_x, event.m_y)) {
         lTrack->SetInBox(true);
         lTrack->SetDragXPos(event.m_x);
         lTrack->SetResetCursorPos(true);
         RefreshTrack(lTrack);
         return true;
      }
   }
   // return false, there is more to do...
   return false;
}

// AS: I don't really understand why this code is sectioned off
//  from the other OnMouseEvent code.
void TrackPanel::HandleTrackSpecificMouseEvent(wxMouseEvent & event)
{
   Track * pTrack;
   wxRect r;
   wxRect rLabel;

   AudacityProject *p = GetProject();
   bool unsafe = (p->GetAudioIOToken()>0 &&
                  gAudioIO->IsStreamActive(p->GetAudioIOToken()));

   FindTrack(event.m_x, event.m_y, true, true, &rLabel);
   pTrack = FindTrack(event.m_x, event.m_y, false, false, &r);

   //call HandleResize if I'm over the border area 
   if (event.LeftDown() &&
       (within(event.m_y, r.y + r.height, TRACK_RESIZE_REGION)
        || within(event.m_y, rLabel.y + rLabel.height,
                  TRACK_RESIZE_REGION))) {
      HandleResize(event);
      HandleCursor(event);
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
      if(HandleLabelTrackMouseEvent( (LabelTrack *) pTrack, r, event ))
         return;
   }

   if (pTrack && (pTrack->GetKind() == Track::Wave) && 
       (mMouseCapture == IsUncaptured || mMouseCapture == IsOverCutLine ||
        mMouseCapture == WasOverCutLine))
   {
      if (HandleTrackLocationMouseEvent( (WaveTrack *) pTrack, r, event ))
         return;
   }

   ToolsToolBar * pTtb = mListener->TP_GetToolsToolBar();
   if( pTtb == NULL )
      return;

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
int TrackPanel::DetermineToolToUse( ToolsToolBar * pTtb, wxMouseEvent & event)
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
   wxRect r;

   Track *pTrack = FindTrack(event.m_x, event.m_y, false, false, &r);
   if( !pTrack )
      return currentTool;

   int trackKind = pTrack->GetKind();
   currentTool = selectTool; // the default.

   if( event.ButtonIsDown(3) || event.RightUp()){
      currentTool = zoomTool;
   } else if( trackKind == Track::Time ){
      currentTool = envelopeTool;
   } else if( trackKind == Track::Label ){
      currentTool = selectTool;
   } else if( trackKind != Track::Wave) {
      currentTool = selectTool;
   // So we are in a wave track.
   //FIXME: Not necessarily. Haven't checked Track::Note (#if defined(USE_MIDI)).
   // From here on the order in which we hit test determines 
   // which tool takes priority in the rare cases where it
   // could be more than one.
   } else if (event.CmdDown()){
      // msmeyer: If control is down, slide single clip
      // msmeyer: If control and shift are down, slide all clips
      currentTool = slideTool;
   } else if( HitTestEnvelope( pTrack, r, event ) ){
      currentTool = envelopeTool;
   } else if( HitTestSlide( pTrack, r, event )){
      currentTool = slideTool;
   } else if( HitTestSamples( pTrack, r, event )){
      currentTool = drawTool;
   }

   //Use the false argument since in multimode we don't 
   //want the button indicating which tool is in use to be updated.
   pTtb->SetCurrentTool( currentTool, false );
   return currentTool;
}


#ifdef USE_MIDI
bool TrackPanel::HitTestStretch(Track *track, wxRect &r, wxMouseEvent & event)
{
   // later, we may want a different policy, but for now, stretch is
   // selected when the cursor is near the center of the track and
   // within the selection
   if (!track || !track->GetSelected() || track->GetKind() != Track::Note ||
       gAudioIO->IsStreamActive( GetProject()->GetAudioIOToken())) {
      return false;
   }
   int center = r.y + r.height / 2;
   int distance = abs(event.m_y - center);
   const int yTolerance = 10;
   wxInt64 leftSel = TimeToPosition(mViewInfo->sel0, r.x);
   wxInt64 rightSel = TimeToPosition(mViewInfo->sel1, r.x);
   // Something is wrong if right edge comes before left edge
   wxASSERT(!(rightSel < leftSel));
   return (leftSel <= event.m_x && event.m_x <= rightSel &&
           distance < yTolerance);
}
#endif


/// method that tells us if the mouse event landed on an 
/// envelope boundary.
bool TrackPanel::HitTestEnvelope(Track *track, wxRect &r, wxMouseEvent & event)
{
   wxASSERT(track);
   if( track->GetKind() != Track::Wave )
      return false;
   WaveTrack *wavetrack = (WaveTrack *)track;
   Envelope *envelope = wavetrack->GetEnvelopeAtX(event.GetX());

   if (!envelope)
      return false;

   int displayType = wavetrack->GetDisplay();
   // Not an envelope hit, unless we're using a type of wavetrack display 
   // suitable for envelopes operations, ie one of the Wave displays.
   if ( displayType > 1) 
      return false;  // No envelope, not a hit, so return.

   // Get envelope point, range 0.0 to 1.0
   bool dB = (displayType == 1);
   double envValue = envelope->GetValueAtX( 
      event.m_x, r, mViewInfo->h, mViewInfo->zoom );

   float zoomMin, zoomMax;
   wavetrack->GetDisplayBounds(&zoomMin, &zoomMax);

   // Get y position of envelope point.
   int yValue = GetWaveYPos( envValue,
      zoomMin, zoomMax,
      r.height, dB, true, mdBr, false ) + r.y;

   // Get y position of center line
   int ctr = GetWaveYPos( 0.0,
      zoomMin, zoomMax,
      r.height, dB, true, mdBr, false ) + r.y;

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
   int ContourSpacing = (int) (r.height / (2* (zoomMax-zoomMin)));
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
bool TrackPanel::HitTestSamples(Track *track, wxRect &r, wxMouseEvent & event)
{
   wxASSERT(track);
   if( track->GetKind() != Track::Wave )
      return false;

   WaveTrack *wavetrack = (WaveTrack *)track;
   //Get rate in order to calculate the critical zoom threshold
   double rate = wavetrack->GetRate();

   //Find out the zoom level
   bool showPoints = (mViewInfo->zoom / rate > 3.0);
   if( !showPoints )
      return false;

   int displayType = wavetrack->GetDisplay();
   if ( displayType > 1) 
      return false;  // Not a wave, so return.

   float oneSample;
   double pps = mViewInfo->zoom;
   double tt = (event.m_x - r.x) / pps + mViewInfo->h;
   sampleCount s0 = (sampleCount)(tt * rate + 0.5);

   // Just get one sample.
   wavetrack->Get((samplePtr)&oneSample, floatSample, s0, 1);

   // Get y distance of envelope point from center line (in pixels).
   bool dB = (displayType == 1);
   float zoomMin, zoomMax;

   wavetrack->GetDisplayBounds(&zoomMin, &zoomMax);

   double envValue = 1.0;
   Envelope* env = wavetrack->GetEnvelopeAtX(event.GetX());
   if (env)
      envValue = env->GetValue(tt);

   int yValue = GetWaveYPos( oneSample * envValue, 
      zoomMin, zoomMax,
      r.height, dB, true, mdBr, false) + r.y;

   // Get y position of mouse (in pixels)
   int yMouse = event.m_y;

   // Perhaps yTolerance should be put into preferences?
   const int yTolerance = 10; // More tolerance on samples than on envelope.
   return( abs( yValue -  yMouse ) < yTolerance );
}

/// method that tells us if the mouse event landed on a 
/// time-slider that allows us to time shift the sequence.
bool TrackPanel::HitTestSlide(Track * WXUNUSED(track), wxRect &r, wxMouseEvent & event)
{
   // Perhaps we should delegate this to TrackArtist as only TrackArtist
   // knows what the real sizes are??

   // The drag Handle width includes border, width and a little extra margin.
   const int adjustedDragHandleWidth = 14;
   // The hotspot for the cursor isn't at its centre.  Adjust for this. 
   const int hotspotOffset = 5;

   // We are doing an approximate test here - is the mouse in the right or left border?
   if( event.m_x + hotspotOffset < r.x + adjustedDragHandleWidth)
      return true;

   if( event.m_x + hotspotOffset > r.x + r.width - adjustedDragHandleWidth)
      return true;

   return false;
}

double TrackPanel::GetMostRecentXPos()
{
   return mViewInfo->h +
      (mMouseMostRecentX - GetLabelWidth()) / mViewInfo->zoom;
}

void TrackPanel::RefreshTrack(Track *trk, bool refreshbacking)
{
   Track *link = trk->GetLink();

   if (link && !trk->GetLinked()) {
      trk = link;
      link = trk->GetLink();
   }

   wxRect r(kLeftInset,
            -mViewInfo->vpos + trk->GetY() + kTopInset,
            GetRect().GetWidth() - kLeftInset * 2 - 1,
            trk->GetHeight() - kTopInset - 1);

   if (link) {
      r.height += link->GetHeight();
   }

#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
   else if(MONO_WAVE_PAN(trk)){
      r.height += trk->GetHeight(true);
   }
#endif

   if( refreshbacking )
   {
      mRefreshBacking = true;
   }

   Refresh( false, &r );
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
   bool samplesFlag    = pTtb->IsDown(drawTool) || bMultiToolDown;
   bool sliderFlag     = bMultiToolDown;

   // The track artist actually draws the stuff inside each track
   mTrackArtist->DrawTracks(mTracks, GetProject()->GetFirstVisible(),
                            *dc, region, tracksRect, clip, mViewInfo, 
                            envelopeFlag, samplesFlag, sliderFlag);

   DrawEverythingElse(dc, region, panelRect, clip);
}

/// Draws 'Everything else'.  In particular it draws:
///  - Drop shadow for tracks and vertical rulers.
///  - Zooming Indicators.
///  - Fills in space below the tracks. 
void TrackPanel::DrawEverythingElse(wxDC * dc,
                                    const wxRegion region,
                                    const wxRect WXUNUSED(panelRect),
                                    const wxRect clip)
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
      wxRect r = trackRect;
      bool skipBorder = false;
      Track *l = t->GetLink();

      if (t->GetLinked()) {
         r.height += l->GetHeight();
      }
      else if (l && trackRect.y >= 0) {
         skipBorder = true;
      }

#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
      if(MONO_WAVE_PAN(t)){
         r.height += t->GetHeight(true);
      }
#endif

      // If the previous track is linked to this one but isn't on the screen
      // (and thus would have been skipped by VisibleTrackIterator) we need to
      // draw that track's border instead.
      Track *borderTrack = t;
      wxRect borderRect = r, borderTrackRect = trackRect;

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
         wxRect r = trackRect;
         r.x += GetVRulerOffset();
         r.y += kTopInset;
         r.width = GetVRulerWidth();
         r.height -= (kTopInset + 2);
         mTrackArtist->DrawVRuler(t, dc, r);
      }

#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
      if(MONO_WAVE_PAN(t)){
         trackRect.y = t->GetY(true) - mViewInfo->vpos;
         trackRect.height = t->GetHeight(true);
         if (region.Contains(0, trackRect.y, GetLeftOffset(), trackRect.height)) {
            wxRect r = trackRect;
            r.x += GetVRulerOffset();
            r.y += kTopInset;
            r.width = GetVRulerWidth();
            r.height -= (kTopInset + 2);
            mTrackArtist->DrawVRuler(t, dc, r);
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
void TrackPanel::DrawZooming(wxDC * dc, const wxRect clip)
{
   wxRect r;

   dc->SetBrush(*wxTRANSPARENT_BRUSH);
   dc->SetPen(*wxBLACK_DASHED_PEN);

   if (mMouseCapture==IsVZooming) {
      int width, height;
      GetTracksUsableArea(&width, &height);

      r.y = mZoomStart;
      r.x = GetVRulerOffset();
      r.width = width + GetVRulerWidth() + 1; //+1 extends into border rect
      r.height = mZoomEnd - mZoomStart;
   }
   else {
      r.x = mZoomStart;
      r.y = -1;
      r.width = mZoomEnd - mZoomStart;
      r.height = clip.height + 2;
   }

   dc->DrawRectangle(r);
}


void TrackPanel::DrawOutside(Track * t, wxDC * dc, const wxRect rec,
                             const wxRect trackRect)
{
   wxRect r = rec;
   int labelw = GetLabelWidth();
   int vrul = GetVRulerOffset();

   DrawOutsideOfTrack(t, dc, r);

   r.x += kLeftInset;
   r.y += kTopInset;
   r.width -= kLeftInset * 2;
   r.height -= kTopInset;

   mTrackInfo.SetTrackInfoFont(dc);
   dc->SetTextForeground(theTheme.Colour(clrTrackPanelText));

   bool bIsWave = (t->GetKind() == Track::Wave);
#ifdef USE_MIDI
   bool bIsNote = (t->GetKind() == Track::Note);
#endif
   // don't enable bHasMuteSolo for Note track because it will draw in the 
   // wrong place.
   mTrackInfo.DrawBackground(dc, r, t->GetSelected(), bIsWave, labelw, vrul);

   // Vaughan, 2010-08-24: No longer doing this.
   // Draw sync-lock tiles in ruler area.
   //if (t->IsSyncLockSelected()) {
   //   wxRect tileFill = r;
   //   tileFill.x = GetVRulerOffset();
   //   tileFill.width = GetVRulerWidth();
   //   TrackArtist::DrawSyncLockTiles(dc, tileFill);
   //}

   DrawBordersAroundTrack(t, dc, r, labelw, vrul);
   DrawShadow(t, dc, r);

   r.width = mTrackInfo.GetTrackInfoWidth();
   bool captured = (t == mCapturedTrack);
   mTrackInfo.DrawCloseBox(dc, r, (captured && mMouseCapture==IsClosing));
   mTrackInfo.DrawTitleBar(dc, r, t, (captured && mMouseCapture==IsPopping));

   mTrackInfo.DrawMinimize(dc, r, t, (captured && mMouseCapture==IsMinimizing));

   // Draw the sync-lock indicator if this track is in a sync-lock selected group.
   if (t->IsSyncLockSelected()) 
   {
      wxRect syncLockIconRect;
      mTrackInfo.GetSyncLockIconRect(r, syncLockIconRect);
      wxBitmap syncLockBitmap(theTheme.Image(bmpSyncLockIcon)); 
      // Icon is 12x12 and syncLockIconRect is 16x16.
      dc->DrawBitmap(syncLockBitmap, 
                     syncLockIconRect.x + 3, 
                     syncLockIconRect.y + 2, 
                     true);
   }

   mTrackInfo.DrawBordersWithin( dc, r, bIsWave );

   if (bIsWave) {
      mTrackInfo.DrawMuteSolo(dc, r, t, (captured && mMouseCapture == IsMuting), false, HasSoloButton());
      mTrackInfo.DrawMuteSolo(dc, r, t, (captured && mMouseCapture == IsSoloing), true, HasSoloButton());

      mTrackInfo.DrawSliders(dc, (WaveTrack *)t, r);
      if (!t->GetMinimized()) {
         int offset = 8;

         if (r.y + 22 + 12 < rec.y + rec.height - 19)
            dc->DrawText(TrackSubText(t),
                         trackRect.x + offset,
                         trackRect.y + 22);

         if (r.y + 38 + 12 < rec.y + rec.height - 19)
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
      r.y -= 34;
      r.height += 34;
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
         if (r.height > 84) {
            AColor::Line(*dc, r.x+48 , r.y+50, r.x+48, r.y + 66); 
            // bevel below mute/solo
            AColor::Line(*dc, r.x, r.y + 66, mTrackInfo.GetTrackInfoWidth(), r.y + 66);
         }
         mTrackInfo.DrawMuteSolo(dc, r, t, 
               (captured && mMouseCapture == IsMuting), false, HasSoloButton());
         mTrackInfo.DrawMuteSolo(dc, r, t, 
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
         mTrackInfo.GetGainRect(r, gr);
         r.y = r.y + h - gr.y; // ultimately want slider at r.y + h
         r.height = r.height - h + gr.y;
         // save for mouse hit detect:
         ((NoteTrack *) t)->SetGainPlacementRect(r); 
         mTrackInfo.DrawVelocitySlider(dc, (NoteTrack *) t, r);
      #endif
   }
   #endif // USE_MIDI
}

void TrackPanel::DrawOutsideOfTrack(Track * t, wxDC * dc, const wxRect r)
{
   // Fill in area outside of the track
   AColor::TrackPanelBackground(dc, false);
   wxRect side;

   // Area between panel border and left track border
   side = r;
   side.width = kLeftInset;
   dc->DrawRectangle(side);

   // Area between panel border and top track border
   side = r;
   side.height = kTopInset;
   dc->DrawRectangle(side);

   // Area between panel border and right track border
   side = r;
   side.x += side.width - kTopInset;
   side.width = kTopInset;
   dc->DrawRectangle(side);

   // Area between tracks of stereo group
#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
   if (t->GetLinked() || MONO_WAVE_PAN(t)) {
      side = r;
      side.y += t->GetHeight() - 1;
      side.height = kTopInset + 1;
      dc->DrawRectangle(side);
   }
#else
   if (t->GetLinked()) {
      side = r;
      side.y += t->GetHeight() - 1;
      side.height = kTopInset + 1;
      dc->DrawRectangle(side);
   }
#endif
}

/// Draw a three-level highlight gradient around the focused track.
void TrackPanel::HighlightFocusedTrack(wxDC * dc, const wxRect r)
{
   wxRect rect = r;
   rect.x += kLeftInset;
   rect.y += kTopInset;
   rect.width -= kLeftInset * 2;
   rect.height -= kTopInset;

   dc->SetBrush(*wxTRANSPARENT_BRUSH);

   AColor::TrackFocusPen(dc, 0);
   dc->DrawRectangle(rect.x - 1, rect.y - 1, rect.width + 2, rect.height + 2);

   AColor::TrackFocusPen(dc, 1);
   dc->DrawRectangle(rect.x - 2, rect.y - 2, rect.width + 4, rect.height + 4);

   AColor::TrackFocusPen(dc, 2);
   dc->DrawRectangle(rect.x - 3, rect.y - 3, rect.width + 6, rect.height + 6);
}

void TrackPanel::UpdateVRulers()
{
   TrackListOfKindIterator iter(Track::Wave, mTracks);
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

   wxRect r(GetVRulerOffset(),
            kTopInset,
            GetVRulerWidth(),
            t->GetHeight() - (kTopInset + 2));

   mTrackArtist->UpdateVRuler(t, r);
   Track *l = t->GetLink();
   if (l) 
   {
      r.height = l->GetHeight() - (kTopInset + 2);
      mTrackArtist->UpdateVRuler(l, r);
   }
#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
   else if(MONO_WAVE_PAN(t)){
      r.height = t->GetHeight(true) - (kTopInset + 2);
      mTrackArtist->UpdateVRuler(t, r);
   }
#endif
}

void TrackPanel::UpdateVRulerSize()
{
   TrackListIterator iter(mTracks);
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
   TrackListIterator iter( mTracks );
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
            TrackListIterator iter( mTracks );
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
         mTracks->Select( t, false );
         SetFocusedTrack( p );   // move focus to next track down
         EnsureVisible( p );
         MakeParentModifyState(false);
         return;
      }
      if( tSelected && !pSelected )
      {
         mTracks->Select( p, true );
         SetFocusedTrack( p );   // move focus to next track down
         EnsureVisible( p );
         MakeParentModifyState(false);
         return;
      }
      if( !tSelected && pSelected )
      {
         mTracks->Select( p, false );
         SetFocusedTrack( p );   // move focus to next track down
         EnsureVisible( p );
         MakeParentModifyState(false);
         return;
      }
      if( !tSelected && !pSelected )
      {
         mTracks->Select( t, true );
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
            TrackListIterator iter( mTracks );
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
   TrackListIterator iter( mTracks );
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
            TrackListIterator iter( mTracks );
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
         mTracks->Select( t, false );
         SetFocusedTrack( n );   // move focus to next track down
         EnsureVisible( n );
         MakeParentModifyState(false);
         return;
      }
      if( tSelected && !nSelected )
      {
         mTracks->Select( n, true );
         SetFocusedTrack( n );   // move focus to next track down
         EnsureVisible( n );
         MakeParentModifyState(false);
         return;
      }
      if( !tSelected && nSelected )
      {
         mTracks->Select( n, false );
         SetFocusedTrack( n );   // move focus to next track down
         EnsureVisible( n );
         MakeParentModifyState(false);
         return;
      }
      if( !tSelected && !nSelected )
      {
         mTracks->Select( t, true );
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
            TrackListIterator iter( mTracks );
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

void TrackPanel::OnToggle()
{
   Track *t;

   t = GetFocusedTrack();   // Get currently focused track
   if (!t)
      return;

   mTracks->Select( t, !t->GetSelected() );
   EnsureVisible( t );
   MakeParentModifyState(false);

   mAx->Updated();

   return;
}

// Make sure selection edge is in view
void TrackPanel::ScrollIntoView(double pos)
{
   int w, h;
   GetTracksUsableArea( &w, &h );

   if( ( pos < mViewInfo->h ) ||
       ( pos > mViewInfo->h + ( ( w - 1 ) / mViewInfo->zoom ) ) )
   {
      mListener->TP_ScrollWindow( pos - ( ( w / 2 ) / mViewInfo->zoom ) );
      Refresh(false);
   }
}

void TrackPanel::ScrollIntoView(int x)
{
   ScrollIntoView(PositionToTime(x, GetLeftOffset()));
}

void TrackPanel::OnCursorLeft( bool shift, bool ctrl, bool keyup )
{
   if( keyup )
   {
      int token = GetProject()->GetAudioIOToken();
      if( token > 0 && gAudioIO->IsStreamActive( token ) )
      {
         return;
      }

      MakeParentModifyState(false);
      return;
   }

   // If the last adjustment was very recent, we are
   // holding the key down and should move faster.
   wxLongLong curtime = ::wxGetLocalTimeMillis();
   int multiplier = 1;
   if( curtime - mLastSelectionAdjustment < 50 )
   {
      multiplier = 4;
   }
   mLastSelectionAdjustment = curtime;

   int snapToTime = GetActiveProject()->GetSnapTo();

   // Contract selection from the right to the left
   if( shift && ctrl )
   {
      // Reduce and constrain (counter-intuitive)
      if (snapToTime) {
         mViewInfo->sel1 = GridMove(mViewInfo->sel1, -multiplier);
      }
      else {
         mViewInfo->sel1 -= multiplier / mViewInfo->zoom;
      }
      if( mViewInfo->sel1 < mViewInfo->sel0 )
      {
         mViewInfo->sel1 = mViewInfo->sel0;
      }

      // Make sure it's visible.
      ScrollIntoView( mViewInfo->sel1 );
      Refresh( false );
   }
   // Extend selection toward the left
   else if( shift )
   {
      // If playing, reposition a long amount of time
      int token = GetProject()->GetAudioIOToken();
      if( token > 0 && gAudioIO->IsStreamActive( token ) )
      {
         gAudioIO->SeekStream(-mSeekLong);
         return;
      }

      // Expand and constrain
      if (snapToTime) {
         mViewInfo->sel0 = GridMove(mViewInfo->sel0, -multiplier);
      }
      else {
         mViewInfo->sel0 -= multiplier / mViewInfo->zoom;
      }
      if( mViewInfo->sel0 < 0.0 )
      {
         mViewInfo->sel0 = 0.0;
      }

      // Make sure it's visible.
      ScrollIntoView( mViewInfo->sel0 );
      Refresh( false );
   }
   // Move the cursor toward the left
   else
   {
      // If playing, reposition a short amount of time
      int token = GetProject()->GetAudioIOToken();
      if( token > 0 && gAudioIO->IsStreamActive( token ) )
      {
         gAudioIO->SeekStream(-mSeekShort);
         return;
      }

      // Already in cursor mode?
      if( mViewInfo->sel0 == mViewInfo->sel1 )
      {
         // Move and constrain
         if (snapToTime) {
            mViewInfo->sel0 = GridMove(mViewInfo->sel0, -multiplier);
         }
         else {
            mViewInfo->sel0 -= multiplier / mViewInfo->zoom;
         }
         if( mViewInfo->sel0 < 0.0 )
         {
            mViewInfo->sel0 = 0.0;
         }
         mViewInfo->sel1 = mViewInfo->sel0;

         // Move the visual cursor
         DrawCursor();
      }
      else
      {
         // Transition to cursor mode.
         mViewInfo->sel1 = mViewInfo->sel0;
         Refresh( false );
      }

      // Make sure it's visible
      ScrollIntoView( mViewInfo->sel0 );
   }
}

void TrackPanel::OnCursorRight( bool shift, bool ctrl, bool keyup )
{
   if( keyup )
   {
      int token = GetProject()->GetAudioIOToken();
      if( token > 0 && gAudioIO->IsStreamActive( token ) )
      {
         return;
      }

      MakeParentModifyState(false);
      return;
   }

   // If the last adjustment was very recent, we are
   // holding the key down and should move faster.
   wxLongLong curtime = ::wxGetLocalTimeMillis();
   int multiplier = 1;
   if( curtime - mLastSelectionAdjustment < 50 )
   {
      multiplier = 4;
   }
   mLastSelectionAdjustment = curtime;

   int snapToTime = GetActiveProject()->GetSnapTo();

   // Contract selection from the left to the right
   if( shift && ctrl )
   {
      // Reduce and constrain (counter-intuitive)
      if (snapToTime) {
         mViewInfo->sel0 = GridMove(mViewInfo->sel0, multiplier);
      }
      else {
         mViewInfo->sel0 += multiplier / mViewInfo->zoom;
      }
      if( mViewInfo->sel0 > mViewInfo->sel1 )
      {
         mViewInfo->sel0 = mViewInfo->sel1;
      }

      // Make sure new position is in view.
      ScrollIntoView( mViewInfo->sel0 );
      Refresh( false );
   }
   // Extend selection toward the right
   else if( shift )
   {
      // If playing, reposition a long amount of time
      int token = GetProject()->GetAudioIOToken();
      if( token > 0 && gAudioIO->IsStreamActive( token ) )
      {
         gAudioIO->SeekStream(mSeekLong);
         return;
      }

      // Expand and constrain
      if (snapToTime) {
         mViewInfo->sel1 = GridMove(mViewInfo->sel1, multiplier);
      }
      else {
         mViewInfo->sel1 += multiplier/mViewInfo->zoom;
      }
      double end = mTracks->GetEndTime();
      if( mViewInfo->sel1 > end )
      {
         mViewInfo->sel1 = end;
      }

      // Make sure new position is in view.
      ScrollIntoView( mViewInfo->sel1 );
      Refresh( false );
   }
   // Move the cursor toward the right
   else
   {
      // If playing, reposition a short amount of time
      int token = GetProject()->GetAudioIOToken();
      if( token > 0 && gAudioIO->IsStreamActive( token ) )
      {
         gAudioIO->SeekStream(mSeekShort);
         return;
      }

      // Already in cursor mode?
      if (mViewInfo->sel0 == mViewInfo->sel1)
      {
         // Move and constrain
         if (snapToTime) {
            mViewInfo->sel1 = GridMove(mViewInfo->sel1, multiplier);
         }
         else {
            mViewInfo->sel1 += multiplier / mViewInfo->zoom;
         }
         double end = mTracks->GetEndTime();
         if( mViewInfo->sel1 > end )
         {
            mViewInfo->sel1 = end;
         }
         mViewInfo->sel0 = mViewInfo->sel1;

         // Move the visual cursor
         DrawCursor();
      }
      else
      {
         // Transition to cursor mode.
         mViewInfo->sel0 = mViewInfo->sel1;
         Refresh( false );
      }

      // Make sure new position is in view
      ScrollIntoView( mViewInfo->sel1 );
   }
}

// Handles moving a selection edge with the keyboard in snap-to-time mode;
// returns the moved value.
// Will move at least minPix pixels -- set minPix positive to move forward,
// negative to move backward.
double TrackPanel::GridMove(double t, int minPix)
{
   TimeTextCtrl ttc(this, wxID_ANY, wxT(""), 0.0, GetProject()->GetRate());
   ttc.SetFormatName(GetProject()->GetSelectionFormat());
   ttc.SetTimeValue(t);

   // Try incrementing/decrementing the value; if we've moved far enough we're
   // done
   double result;
   minPix >= 0 ? ttc.Increment() : ttc.Decrement();
   result = ttc.GetTimeValue();
   if (fabs(result - t) * mViewInfo->zoom >= fabs((double)minPix)) {
      return result;
   }

   // Otherwise, move minPix pixels, then snap to the time.
   result = t + minPix / mViewInfo->zoom;
   ttc.SetTimeValue(result);
   result = ttc.GetTimeValue();
   return result;
}

void TrackPanel::OnBoundaryMove(bool left, bool boundaryContract)
{
  // Move the left/right selection boundary, to either expand or contract the selection
  // left=true: operate on left boundary; left=false: operate on right boundary
  // boundaryContract=true: contract region; boundaryContract=false: expand region.

   // If the last adjustment was very recent, we are
   // holding the key down and should move faster.
   wxLongLong curtime = ::wxGetLocalTimeMillis();
   int multiplier = 1;
   if( curtime - mLastSelectionAdjustment < 50 )
   {
      multiplier = 4;
   }
   mLastSelectionAdjustment = curtime;

   int token = GetProject()->GetAudioIOToken();
   if( token > 0 && gAudioIO->IsStreamActive( token ) )
   {
      double indicator = gAudioIO->GetStreamTime();
      if (left) {
         mViewInfo->sel0 = indicator;
         if(mViewInfo->sel1 < mViewInfo->sel0)
            mViewInfo->sel1 = mViewInfo->sel0;
      } 
      else
      {
         mViewInfo->sel1 = indicator;
      }

      MakeParentModifyState(false);
      Refresh(false);
   }
   else
   { 
      // BOUNDARY MOVEMENT
      // Contract selection from the right to the left
      if( boundaryContract )
      {
         if (left) {
            // Reduce and constrain left boundary (counter-intuitive)
            mViewInfo->sel0 += multiplier / mViewInfo->zoom;
            if( mViewInfo->sel0 > mViewInfo->sel1 )
            {
               mViewInfo->sel0 = mViewInfo->sel1;
            }
            // Make sure it's visible
            ScrollIntoView( mViewInfo->sel0 );
         } 
         else 
         {
            // Reduce and constrain right boundary (counter-intuitive)
            mViewInfo->sel1 -= multiplier / mViewInfo->zoom;
            if( mViewInfo->sel1 < mViewInfo->sel0 )
            {
               mViewInfo->sel1 = mViewInfo->sel0;
            }
            // Make sure it's visible
            ScrollIntoView( mViewInfo->sel1 );
         }
      }
      // BOUNDARY MOVEMENT
      // Extend selection toward the left
      else 
      {
         if (left) {
            // Expand and constrain left boundary
            mViewInfo->sel0 -= multiplier / mViewInfo->zoom;
            if( mViewInfo->sel0 < 0.0 )
            {
               mViewInfo->sel0 = 0.0;
            }
            // Make sure it's visible
            ScrollIntoView( mViewInfo->sel0 );
         }
         else
         {
            // Expand and constrain right boundary
            mViewInfo->sel1 += multiplier/mViewInfo->zoom;
            double end = mTracks->GetEndTime();
            if( mViewInfo->sel1 > end )
            {
               mViewInfo->sel1 = end;
            }
         }
      }
      Refresh( false );
      MakeParentModifyState(false);
   }
}

// Move the cursor forward or backward, while paused or while playing. 
// forward=true: Move cursor forward; forward=false: Move cursor backwards
// jump=false: Move cursor determined by zoom; jump=true: Use seek times
// longjump=false: Use mSeekShort; longjump=true: Use mSeekLong
void TrackPanel::OnCursorMove(bool forward, bool jump, bool longjump )
{
   // If the last adjustment was very recent, we are
   // holding the key down and should move faster.
   wxLongLong curtime = ::wxGetLocalTimeMillis();
   int multiplier = 1;
   if( curtime - mLastSelectionAdjustment < 50 )
   {
      multiplier = 4;
   }
   mLastSelectionAdjustment = curtime;

   float direction = -1;
   if (forward) {
      direction = 1;
   }

   float mSeek;
   if (jump) {
      if (!longjump) {
         mSeek = mSeekShort;
      } else {
         mSeek = mSeekLong;
      }
   } else {
      mSeek = multiplier / mViewInfo->zoom;
   }
   mSeek *= direction;

   // If playing, reposition a short amount of time
   int token = GetProject()->GetAudioIOToken();
   if( token > 0 && gAudioIO->IsStreamActive( token ) )
   {
      gAudioIO->SeekStream(mSeek);
   } 
   else 
   {
      // Already in cursor mode?
      if( mViewInfo->sel0 == mViewInfo->sel1 )
      {
         // Move and constrain
         mViewInfo->sel0 += mSeek;
         if( !forward && mViewInfo->sel0 < 0.0 )
         {
            mViewInfo->sel0 = 0.0;
         }
         double end = mTracks->GetEndTime();
         if( forward && mViewInfo->sel0 > end)
         {
            mViewInfo->sel0 = end;
         }
         mViewInfo->sel1 = mViewInfo->sel0;

         // Move the visual cursor
         DrawCursor();
      }
      else
      {
         // Transition to cursor mode.
         mViewInfo->sel1 = mViewInfo->sel0;
         Refresh( false );
      }

      // Make sure it's visible
      ScrollIntoView( mViewInfo->sel0 );

      MakeParentModifyState(false);
   }
}

//The following methods operate controls on specified tracks,
//This will pop up the track panning dialog for specified track
void TrackPanel::OnTrackPan()
{
   Track *t = GetFocusedTrack();
   if (!t || (t->GetKind() != Track::Wave)) {
      return;
   }

   LWSlider *slider = mTrackInfo.PanSlider(t->GetIndex());
   if (slider->ShowDialog()) {
      SetTrackPan(t, slider);
   }
}

void TrackPanel::OnTrackPanLeft()
{
   Track *t = GetFocusedTrack();
   if (!t || (t->GetKind() != Track::Wave)) {
      return;
   }

   LWSlider *slider = mTrackInfo.PanSlider(t->GetIndex());
   slider->Decrease(1);
   SetTrackPan(t, slider);
}

void TrackPanel::OnTrackPanRight()
{
   Track *t = GetFocusedTrack();
   if (!t || (t->GetKind() != Track::Wave)) {
      return;
   }

   LWSlider *slider = mTrackInfo.PanSlider(t->GetIndex());
   slider->Increase(1);
   SetTrackPan(t, slider);
}

void TrackPanel::SetTrackPan(Track * t, LWSlider * s)
{
   wxASSERT(t);
   if( t->GetKind() != Track::Wave )
      return;
   float newValue = s->Get();

   WaveTrack *link = (WaveTrack *)mTracks->GetLink(t);
   ((WaveTrack*)t)->SetPan(newValue);
   if (link)
      link->SetPan(newValue);

   MakeParentPushState(_("Adjusted Pan"), _("Pan"), PUSH_CONSOLIDATE );

   RefreshTrack(t);
}

/// This will pop up the track gain dialog for specified track
void TrackPanel::OnTrackGain()
{
   Track *t = GetFocusedTrack();
   if (!t || (t->GetKind() != Track::Wave)) {
      return;
   }

   LWSlider *slider = mTrackInfo.GainSlider(t->GetIndex());
   if (slider->ShowDialog()) {
      SetTrackGain(t, slider);
   }
}

void TrackPanel::OnTrackGainInc()
{
   Track *t = GetFocusedTrack();
   if (!t || (t->GetKind() != Track::Wave)) {
      return;
   }

   LWSlider *slider = mTrackInfo.GainSlider(t->GetIndex());
   slider->Increase(1);
   SetTrackGain(t, slider);
}

void TrackPanel::OnTrackGainDec()
{
   Track *t = GetFocusedTrack();
   if (!t || (t->GetKind() != Track::Wave)) {
      return;
   }

   LWSlider *slider = mTrackInfo.GainSlider(t->GetIndex());
   slider->Decrease(1);
   SetTrackGain(t, slider);
}

void TrackPanel::SetTrackGain(Track * t, LWSlider * s)
{
   wxASSERT(t);
   if( t->GetKind() != Track::Wave )
      return ;
   float newValue = s->Get();

   WaveTrack *link = (WaveTrack *)mTracks->GetLink(t);
   ((WaveTrack*)t)->SetGain(newValue);
   if (link)
      link->SetGain(newValue);

   MakeParentPushState(_("Adjusted gain"), _("Gain"), PUSH_CONSOLIDATE);

   RefreshTrack(t);
}

void TrackPanel::OnTrackMenu(Track *t)
{
   if(!t) {
      t = GetFocusedTrack();
      if(!t) return;
   }

   mPopupMenuTarget = t;

   bool canMakeStereo = false;
   Track *next = mTracks->GetNext(t);

   wxMenu *theMenu = NULL;
   if (t->GetKind() == Track::Time) {
      theMenu = mTimeTrackMenu;

      TimeTrack *tt = (TimeTrack*) t;

      theMenu->Enable(OnTimeTrackLinID, tt->GetDisplayLog());
      theMenu->Enable(OnTimeTrackLogID, !tt->GetDisplayLog());
      theMenu->Check(OnTimeTrackLogIntID, tt->GetInterpolateLog());
   }

   if (t->GetKind() == Track::Wave) {
      theMenu = mWaveTrackMenu;
      if (next && !t->GetLinked() && !next->GetLinked()
            && t->GetKind() == Track::Wave
            && next->GetKind() == Track::Wave)
         canMakeStereo = true;

      theMenu->Enable(OnSwapChannelsID, t->GetLinked());
      theMenu->Enable(OnMergeStereoID, canMakeStereo);
      theMenu->Enable(OnSplitStereoID, t->GetLinked());
      theMenu->Enable(OnSplitStereoMonoID, t->GetLinked());
      theMenu->Check(OnChannelMonoID, t->GetChannel() == Track::MonoChannel);
      theMenu->Check(OnChannelLeftID, t->GetChannel() == Track::LeftChannel);
      theMenu->Check(OnChannelRightID, t->GetChannel() == Track::RightChannel);
      theMenu->Enable(OnChannelMonoID, !t->GetLinked());
      theMenu->Enable(OnChannelLeftID, !t->GetLinked());
      theMenu->Enable(OnChannelRightID, !t->GetLinked());

      int display = ((WaveTrack *) t)->GetDisplay();

      theMenu->Enable(OnWaveformID, display != WaveTrack::WaveformDisplay);
      theMenu->Enable(OnWaveformDBID,
                        display != WaveTrack::WaveformDBDisplay);
      theMenu->Enable(OnSpectrumID, display != WaveTrack::SpectrumDisplay);
      theMenu->Enable(OnSpectrumLogID, display != WaveTrack::SpectrumLogDisplay);
      theMenu->Enable(OnPitchID, display != WaveTrack::PitchDisplay);

      WaveTrack * track = (WaveTrack *)t;
      SetMenuCheck(*mRateMenu, IdOfRate((int) track->GetRate()));
      SetMenuCheck(*mFormatMenu, IdOfFormat(track->GetSampleFormat()));

      bool unsafe = IsUnsafe();
      for (int i = OnRate8ID; i <= OnFloatID; i++) {
         theMenu->Enable(i, !unsafe);
      }
   }

#if defined(USE_MIDI)
   if (t->GetKind() == Track::Note)
      theMenu = mNoteTrackMenu;
#endif

   if (t->GetKind() == Track::Label){
       theMenu = mLabelTrackMenu;
   }

   if (theMenu) {
      theMenu->Enable(OnMoveUpID, mTracks->CanMoveUp(t));
      theMenu->Enable(OnMoveDownID, mTracks->CanMoveDown(t));
      theMenu->Enable(OnMoveTopID, mTracks->CanMoveUp(t));
      theMenu->Enable(OnMoveBottomID, mTracks->CanMoveDown(t));

      //We need to find the location of the menu rectangle.
      wxRect r = FindTrackRect(t,true);
      wxRect titleRect;
      mTrackInfo.GetTitleBarRect(r,titleRect);

      PopupMenu(theMenu, titleRect.x + 1,
                  titleRect.y + titleRect.height + 1);
   }

   SetCapturedTrack(NULL);

   Refresh(false);
}

void TrackPanel::OnTrackMute(bool shiftDown, Track *t)
{
   if (!t) {
      t = GetFocusedTrack();
      if (!t || (t->GetKind() != Track::Wave)) 
         return;
   }
   GetProject()->HandleTrackMute(t, shiftDown);

   // Update mixer board, too.
   MixerBoard* pMixerBoard = this->GetMixerBoard(); 
   if (pMixerBoard) 
   {
      pMixerBoard->UpdateMute(); // Update for all tracks.
      pMixerBoard->UpdateSolo(); // Update for all tracks.
   }

   mAx->Updated();
   Refresh(false);
}


void TrackPanel::OnTrackSolo(bool shiftDown, Track *t)
{
   if (!t) 
   {
      t = GetFocusedTrack();
      if (!t || (t->GetKind() != Track::Wave)) 
         return;
   }
   GetProject()->HandleTrackSolo(t, shiftDown);

   // Update mixer board, too.
   MixerBoard* pMixerBoard = this->GetMixerBoard(); 
   if (pMixerBoard) 
   {
      pMixerBoard->UpdateMute(); // Update for all tracks.
      pMixerBoard->UpdateSolo(); // Update for all tracks.
   }

   mAx->Updated();
   Refresh(false);
}

void TrackPanel::OnTrackClose()
{
   Track *t = GetFocusedTrack();
   if(!t) return;

   if( gAudioIO->IsStreamActive( GetProject()->GetAudioIOToken() ) )
   {
      mListener->TP_DisplayStatusMessage( _( "Can't delete track with active audio" ) );
      wxBell();
      return;
   }

   RemoveTrack( t );

   SetCapturedTrack( NULL );

   // BG: There are no more tracks on screen
   if( mTracks->IsEmpty() )
   {
      //BG: Set zoom to normal
      mViewInfo->zoom = 44100.0 / 512.0;

      //STM: Set selection to 0,0
      mViewInfo->sel0 = 0.0;
      mViewInfo->sel1 = 0.0;

      mListener->TP_RedrawScrollbars();
      mListener->TP_DisplayStatusMessage( wxT( "" ) ); //STM: Clear message if all tracks are removed
   }

   Refresh( false );
}


Track * TrackPanel::GetFirstSelectedTrack()
{

   TrackListIterator iter(mTracks);

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
   TrackListIterator iter(mTracks);
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
                                        const wxRect r, const int vrul,
                                        const int labelw)
{
   // Border around track and label area
   dc->SetBrush(*wxTRANSPARENT_BRUSH);
   dc->SetPen(*wxBLACK_PEN);
   dc->DrawRectangle(r.x, r.y, r.width - 1, r.height - 1);

   AColor::Line(*dc, labelw, r.y, labelw, r.y + r.height - 1);       // between vruler and TrackInfo

   // The lines at bottom of 1st track and top of second track of stereo group
   // Possibly replace with DrawRectangle to add left border.
#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
   if (t->GetLinked() || MONO_WAVE_PAN(t)) {
      int h1 = r.y + t->GetHeight() - kTopInset;
      AColor::Line(*dc, vrul, h1 - 2, r.x + r.width - 1, h1 - 2);
      AColor::Line(*dc, vrul, h1 + kTopInset, r.x + r.width - 1, h1 + kTopInset);
   }
#else
   if (t->GetLinked()) {
      int h1 = r.y + t->GetHeight() - kTopInset;
      AColor::Line(*dc, vrul, h1 - 2, r.x + r.width - 1, h1 - 2);
      AColor::Line(*dc, vrul, h1 + kTopInset, r.x + r.width - 1, h1 + kTopInset);
   }
#endif
}

void TrackPanel::DrawShadow(Track * /* t */ , wxDC * dc, const wxRect r)
{
   int right = r.x + r.width - 1;
   int bottom = r.y + r.height - 1;

   // shadow
   dc->SetPen(*wxBLACK_PEN);

   // bottom
   AColor::Line(*dc, r.x, bottom, right, bottom);
   // right
   AColor::Line(*dc, right, r.y, right, bottom);

   // background
   AColor::Dark(dc, false);

   // bottom
   AColor::Line(*dc, r.x, bottom, r.x + 1, bottom);
   // right
   AColor::Line(*dc, right, r.y, right, r.y + 1);
}

/// Returns the string to be displayed in the track label
/// indicating whether the track is mono, left, right, or 
/// stereo and what sample rate it's using.
wxString TrackPanel::TrackSubText(Track * t)
{
   wxString s = wxString::Format(wxT("%dHz"),
                                 (int) (((WaveTrack *) t)->GetRate() +
                                        0.5));
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
   mPopupMenuTarget = NULL;
   Refresh(false);
}

/// Swap the left and right channels of a stero track...
void TrackPanel::OnSwapChannels(wxCommandEvent & WXUNUSED(event))
{
   Track *partner = mPopupMenuTarget->GetLink();
   SplitStereo(true);
   mPopupMenuTarget->SetChannel(Track::RightChannel);
   partner->SetChannel(Track::LeftChannel);

   (mTracks->MoveUp(partner));
   partner->SetLinked(true);

   MixerBoard* pMixerBoard = this->GetMixerBoard();
   if (pMixerBoard) {
      pMixerBoard->UpdateTrackClusters();
   }

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

   Track *partner = mPopupMenuTarget->GetLink();

#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
   if(!stereo && MONO_WAVE_PAN(mPopupMenuTarget))
      ((WaveTrack*)mPopupMenuTarget)->SetVirtualState(true,true);
   if(!stereo && MONO_WAVE_PAN(partner))
      ((WaveTrack*)partner)->SetVirtualState(true,true);
#endif

   if (partner)
   {
      partner->SetName(mPopupMenuTarget->GetName());
      if (!stereo)
         partner->SetChannel(Track::MonoChannel);  // Keep original stereo track name.

      //On Demand - have each channel add it's own.
      if (ODManager::IsInstanceCreated() && partner->GetKind() == Track::Wave)
         ODManager::Instance()->MakeWaveTrackIndependent((WaveTrack*)partner);
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

      // Set new track heights and minimized state
      bool bBothMinimizedp=((mPopupMenuTarget->GetMinimized())&&(partner->GetMinimized()));
      mPopupMenuTarget->SetMinimized(false);
      partner->SetMinimized(false);
      int AverageHeight=(mPopupMenuTarget->GetHeight() + partner->GetHeight())/ 2;
      mPopupMenuTarget->SetHeight(AverageHeight);
      partner->SetHeight(AverageHeight);
      mPopupMenuTarget->SetMinimized(bBothMinimizedp);
      partner->SetMinimized(bBothMinimizedp);

      //On Demand - join the queues together.
      if(ODManager::IsInstanceCreated() && partner->GetKind() == Track::Wave && mPopupMenuTarget->GetKind() == Track::Wave )
         if(!ODManager::Instance()->MakeWaveTrackDependent((WaveTrack*)partner,(WaveTrack*)mPopupMenuTarget))
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

///  Set the Display mode based on the menu choice in the Track Menu.
///  Note that gModes MUST BE IN THE SAME ORDER AS THE MENU CHOICES!!
///  const wxChar *gModes[] = { wxT("waveform"), wxT("waveformDB"),
///  wxT("spectrum"), wxT("pitch") };
void TrackPanel::OnSetDisplay(wxCommandEvent & event)
{
   int id = event.GetId();
   wxASSERT(id >= OnWaveformID && id <= OnPitchID);
   wxASSERT(mPopupMenuTarget
            && mPopupMenuTarget->GetKind() == Track::Wave);

   id -= OnWaveformID;
   WaveTrack *wt = (WaveTrack *) mPopupMenuTarget;
   if (wt->GetDisplay() != id) {
      wt->SetDisplay(id);
      mTrackArtist->InvalidateSpectrumCache(wt);

      WaveTrack *l = (WaveTrack *) wt->GetLink();
      if (l) {
         l->SetDisplay(id);
         mTrackArtist->InvalidateSpectrumCache(l);
      }
#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
      if (wt->GetDisplay() == WaveTrack::WaveformDisplay) {
         wt->SetVirtualState(false);
      }else if (id == WaveTrack::WaveformDisplay) {
         wt->SetVirtualState(true);
      }
#endif
      UpdateVRuler(wt);
   }
   MakeParentModifyState(true);
   mPopupMenuTarget = NULL;
   Refresh(false);
}

/// Sets the sample rate for a track, and if it is linked to
/// another track, that one as well.
void TrackPanel::SetRate(Track * pTrack, double rate)
{
   ((WaveTrack *) pTrack)->SetRate(rate);
   Track *partner = mTracks->GetLink(pTrack);
   if (partner)
      ((WaveTrack *) partner)->SetRate(rate);
   MakeParentPushState(wxString::Format(_("Changed '%s' to %d Hz"),
                                        pTrack->GetName().c_str(), rate),
                       _("Rate Change"));
}

/// Handles the selection from the Format submenu of the
/// track menu.
void TrackPanel::OnFormatChange(wxCommandEvent & event)
{
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
   Track *partner = mTracks->GetLink(mPopupMenuTarget);
   if (partner)
   {
      bResult = ((WaveTrack*)partner)->ConvertToSampleFormat(newFormat);
      wxASSERT(bResult); // TO DO: Actually handle this.
   }

   MakeParentPushState(wxString::Format(_("Changed '%s' to %s"),
                                        mPopupMenuTarget->GetName().
                                        c_str(),
                                        GetSampleFormatStr(newFormat)),
                       _("Format Change"));

   SetMenuCheck( *mFormatMenu, id );
   mPopupMenuTarget = NULL;
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

/// Puts a check mark at a given position in a menu, clearing all other check marks.
void TrackPanel::SetMenuCheck( wxMenu & menu, int newId )
{
   wxMenuItemList & list = menu.GetMenuItems();
   wxMenuItem * item;
   int id;

   for ( wxMenuItemList::compatibility_iterator node = list.GetFirst(); node; node = node->GetNext() )
   {
      item = node->GetData();
      id = item->GetId();
      menu.Check( id, id==newId );
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
   int id = event.GetId();
   wxASSERT(id >= OnRate8ID && id <= OnRate384ID);
   wxASSERT(mPopupMenuTarget
            && mPopupMenuTarget->GetKind() == Track::Wave);

   SetMenuCheck( *mRateMenu, id );
   SetRate(mPopupMenuTarget, gRates[id - OnRate8ID]);

   mPopupMenuTarget = NULL;
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
   wxASSERT(mPopupMenuTarget
            && mPopupMenuTarget->GetKind() == Track::Wave);

   int newRate;

   /// \todo Remove artificial constants!!
   /// \todo Make a real dialog box out of this!!
   while (true)
   {
      wxDialog dlg(this, wxID_ANY, wxString(_("Set Rate")));
      ShuttleGui S(&dlg, eIsCreating);
      wxString rate;
      wxArrayString rates;
      wxComboBox *cb;

      rate.Printf(wxT("%d"), lrint(((WaveTrack *) mPopupMenuTarget)->GetRate()));

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
         S.StartHorizontalLay(wxCENTER, false);
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

      dlg.SetSize(dlg.GetSizer()->GetMinSize());
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
   SetRate(mPopupMenuTarget, newRate);

   mPopupMenuTarget = NULL;
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
         MakeParentPushState(wxString::Format(_("Set range to '%d' - '%d'"),
                                              lower,
                                              upper),
      /* i18n-hint: (verb)*/

                             _("Set Range"));
         Refresh(false);
      }
   }
   mPopupMenuTarget = NULL;
}

void TrackPanel::OnTimeTrackLin(wxCommandEvent & /*event*/)
{
   TimeTrack *t = (TimeTrack*)mPopupMenuTarget;
   t->SetDisplayLog(false);
   UpdateVRuler(t);
   MakeParentPushState(_("Set time track display to linear"), _("Set Display"));
   Refresh(false);
}

void TrackPanel::OnTimeTrackLog(wxCommandEvent & /*event*/)
{
   TimeTrack *t = (TimeTrack*)mPopupMenuTarget;
   t->SetDisplayLog(true);
   UpdateVRuler(t);
   MakeParentPushState(_("Set time track display to logarithmic"), _("Set Display"));
   Refresh(false);
}

void TrackPanel::OnTimeTrackLogInt(wxCommandEvent & /*event*/)
{
   TimeTrack *t = (TimeTrack*)mPopupMenuTarget;
   if(t->GetInterpolateLog()) {
      t->SetInterpolateLog(false);
      MakeParentPushState(_("Set time track interpolation to linear"), _("Set Interpolation"));
   } else {
      t->SetInterpolateLog(true);
      MakeParentPushState(_("Set time track interpolation to logarithmic"), _("Set Interpolation"));
   }
   Refresh(false);
}

/// Move a track up, down, to top or to bottom.
void TrackPanel::OnMoveTrack(wxCommandEvent & event)
{
   wxASSERT(event.GetId() == OnMoveUpID || event.GetId() == OnMoveDownID ||
            event.GetId() == OnMoveTopID || event.GetId() == OnMoveBottomID);

   wxString direction;
   
   switch (event.GetId())
   {
   case OnMoveTopID :
      /* i18n-hint: where the track is moving to.*/
      direction = _("to Top");

      while (mTracks->CanMoveUp(mPopupMenuTarget)) {
         if (mTracks->Move(mPopupMenuTarget, true)) {
            MixerBoard* pMixerBoard = this->GetMixerBoard(); // Update mixer board.
            if (pMixerBoard && (mPopupMenuTarget->GetKind() == Track::Wave))
               pMixerBoard->MoveTrackCluster((WaveTrack*)mPopupMenuTarget, true);
         }
      }
      break;
   case OnMoveBottomID :
      /* i18n-hint: where the track is moving to.*/
      direction = _("to Bottom");

      while (mTracks->CanMoveDown(mPopupMenuTarget)) {
         if (mTracks->Move(mPopupMenuTarget, false)) {
            MixerBoard* pMixerBoard = this->GetMixerBoard(); // Update mixer board.
            if (pMixerBoard && (mPopupMenuTarget->GetKind() == Track::Wave))
               pMixerBoard->MoveTrackCluster((WaveTrack*)mPopupMenuTarget, false);
         }
      }
      break;
   default:
      bool bUp = (OnMoveUpID == event.GetId());
      /* i18n-hint: a direction.*/
      direction = bUp ? _("Up") : _("Down");

      if (mTracks->Move(mPopupMenuTarget, bUp)) {
         MixerBoard* pMixerBoard = this->GetMixerBoard();
         if (pMixerBoard && (mPopupMenuTarget->GetKind() == Track::Wave)) {
            pMixerBoard->MoveTrackCluster((WaveTrack*)mPopupMenuTarget, bUp);
         }
      }
   }

   /* i18n-hint: Past tense of 'to move', as in 'moved audio track up'.*/
   wxString longDesc = (_("Moved"));
   /* i18n-hint: The direction of movement will be up, down, to top or to bottom.. */
   wxString shortDesc = (_("Move Track"));

   longDesc = (wxString::Format(wxT("%s '%s' %s"), longDesc.c_str(), 
                                mPopupMenuTarget->GetName().c_str(), direction.c_str()));
   shortDesc = (wxString::Format(wxT("%s %s"), shortDesc.c_str(), direction.c_str()));

   MakeParentPushState(longDesc, shortDesc);

   Refresh(false);
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

/// Cut selected text if cut menu item is selected
void TrackPanel::OnCutSelectedText(wxCommandEvent & WXUNUSED(event))
{
   LabelTrack *lt = (LabelTrack *)mPopupMenuTarget;
   if (lt->CutSelectedText()) {
      MakeParentPushState(_("Modified Label"),
                          _("Label Edit"),
                          PUSH_CONSOLIDATE);
   }
   RefreshTrack(lt);
}

/// Copy selected text if copy menu item is selected
void TrackPanel::OnCopySelectedText(wxCommandEvent & WXUNUSED(event))
{
   LabelTrack *lt = (LabelTrack *)mPopupMenuTarget;
   lt->CopySelectedText();
   RefreshTrack(lt);
}

/// paste selected text if p`aste menu item is selected
void TrackPanel::OnPasteSelectedText(wxCommandEvent & WXUNUSED(event))
{
   LabelTrack *lt = (LabelTrack *)mPopupMenuTarget;
   if (lt->PasteSelectedText(mViewInfo->sel0, mViewInfo->sel1)) {
      MakeParentPushState(_("Modified Label"),
                          _("Label Edit"),
                          true /* consolidate */);
   }
   RefreshTrack(lt);
}

// Small helper class to enumerate all fonts in the system
// We use this because the default implementation of
// wxFontEnumerator::GetFacenames() has changed between wx2.6 and 2.8
class TrackPanelFontEnumerator : public wxFontEnumerator
{
public:
   TrackPanelFontEnumerator(wxArrayString* fontNames) :
      mFontNames(fontNames) {}

   virtual bool OnFacename(const wxString& font)
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
   long fontsize = gPrefs->Read(wxT("/GUI/LabelFontSize"), 12);

   /* i18n-hint: (noun) This is the font for the label track.*/
   wxDialog dlg(this, wxID_ANY, wxString(_("Label Track Font")));
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
         lb = new wxListBox(&dlg, wxID_ANY,
                            wxDefaultPosition,
                            wxDefaultSize,
                            facenames,
                            wxLB_SINGLE);

         lb->SetName(_("Face name"));
         lb->SetSelection(facenames.Index(facename));
         S.AddWindow(lb, wxALIGN_LEFT | wxEXPAND | wxALL);

         /* i18n-hint: (noun) The size of the typeface*/
         S.AddPrompt(_("Face size"));
         sc = new wxSpinCtrl(&dlg, wxID_ANY,
                             wxString::Format(wxT("%d"), fontsize),
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
   wxRect r;
   r.x = 0;
   r.y = -mViewInfo->vpos;
   r.y += kTopInset;
   GetSize(&r.width, &r.height);

   if (label) {
      r.width = GetLeftOffset();
   } else {
      r.x = GetLeftOffset();
      r.width -= GetLeftOffset();
   }

   VisibleTrackIterator iter(GetProject());
   for (Track * t = iter.First(); t; t = iter.Next()) {
      r.y = t->GetY() - mViewInfo->vpos + kTopInset;
      r.height = t->GetHeight();

      if (link && t->GetLink()) {
         Track *l = t->GetLink();
         int h = l->GetHeight();
         if (!t->GetLinked()) {
            t = l;
            r.y = t->GetY() - mViewInfo->vpos + kTopInset;
         }
         r.height += h;
      }
#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
      else if(link && MONO_WAVE_PAN(t))
      {
         r.height += t->GetHeight(true);
      }
#endif
      //Determine whether the mouse is inside 
      //the current rectangle.  If so, recalculate
      //the proper dimensions and return.
      if (r.Contains(mouseX, mouseY)) {
#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
         t->SetVirtualStereo(false);
#endif
         if (trackRect) {
            r.y -= kTopInset;
            if (label) {
               r.x += kLeftInset;
               r.width -= kLeftInset;
               r.y += kTopInset;
               r.height -= kTopInset;
            }
            *trackRect = r;
         }

         return t;
      }
#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
      if(!link && MONO_WAVE_PAN(t)){
         r.y = t->GetY(true) - mViewInfo->vpos + kTopInset;
         r.height = t->GetHeight(true);
         if (r.Contains(mouseX, mouseY)) {
            t->SetVirtualStereo(true);
            if (trackRect) {
               r.y -= kTopInset;
               if (label) {
                  r.x += kLeftInset;
                  r.width -= kLeftInset;
                  r.y += kTopInset;
                  r.height -= kTopInset;
               }
               *trackRect = r;
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

   wxRect r(0,
            target->GetY() - mViewInfo->vpos,
            GetSize().GetWidth(),
            target->GetHeight());

   // The check for a null linked track is necessary because there's
   // a possible race condition between the time the 2 linked tracks
   // are added and when wxAccessible methods are called.  This is
   // most evident when using Jaws.
   if (target->GetLinked() && target->GetLink()) {
      r.height += target->GetLink()->GetHeight();
   }

   if (label) {
      r.x += kLeftInset;
      r.width -= kLeftInset;
      r.y += kTopInset;
      r.height -= kTopInset;
   }

   return r;
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

bool TrackPanel::MoveClipToTrack(WaveClip *clip, WaveTrack* dst)
{
   WaveTrack *src  = NULL;
   WaveClip *clip2 = NULL;
   WaveTrack *src2 = NULL;
   WaveTrack *dst2 = NULL;
   size_t i;

#ifdef USE_MIDI
   // dst could be a note track. Can't move clip to a note track.
   // EXPLAIN: How could dst be a note track (pointer)? It's declared to be a WaveTrack*. I think this test is pointless.
   if (dst->GetKind() != Track::Wave) return false;
#endif

   for (i = 0; i < mCapturedClipArray.GetCount(); i++) {
      if (clip == mCapturedClipArray[i].clip) {
         src = (WaveTrack*)mCapturedClipArray[i].track;
         break;
      }
   }

   if (!src)
      return false;

   // Make sure we have the first track of two stereo tracks
   // with both source and destination
   if (!src->GetLinked() && mTracks->GetLink(src)) {
      // set it to NULL in case there is no L channel clip
      clip = NULL;

      // find the first track by getting the linked track from src
      // assumes that mCapturedArray[i].clip and .track is not NULL.
      for (i = 0; i < mCapturedClipArray.GetCount(); i++) {
         if (mTracks->GetLink(src) == mCapturedClipArray[i].track) {
            clip = mCapturedClipArray[i].clip;
            break;
         }
      }

      src = (WaveTrack*)mTracks->GetLink(src);
   }
   if (!dst->GetLinked() && mTracks->GetLink(dst))
      dst = (WaveTrack*)mTracks->GetLink(dst);

   // Get the second track of two stereo tracks
   src2 = (WaveTrack*)mTracks->GetLink(src);
   dst2 = (WaveTrack*)mTracks->GetLink(dst);

   for (i = 0; i < mCapturedClipArray.GetCount(); i++) {
      if (mCapturedClipArray[i].track == src2) {
         clip2 = mCapturedClipArray[i].clip;
         break;
      }
   }

   if ((src2 && !dst2) || (dst2 && !src2))
      return false; // cannot move stereo- to mono track or other way around

   // if only the right clip of a stereo pair is being dragged, use clip instead of clip2 to get mono behavior.
   if (!clip && clip2) {
      clip  = clip2;
      src   = src2;
      dst   = dst2;
      clip2 = NULL;
      src2 = dst2 = NULL;
   }

   if (!dst->CanInsertClip(clip))
      return false;

   if (clip2) {
      // we should have a source and dest track
      if (!dst2 || !src2)
         return false;

      if (!dst2->CanInsertClip(clip2))
         return false;
   }

   src->MoveClipToTrack(clip, dst);
   if (src2)
      src2->MoveClipToTrack(clip2, dst2);

   // update the captured clip array.
   for (i = 0; i < mCapturedClipArray.GetCount(); i++) {
      if (clip && mCapturedClipArray[i].clip == clip) {
         mCapturedClipArray[i].track = dst;
      } else if (clip2 && mCapturedClipArray[i].clip == clip2) {
         mCapturedClipArray[i].track = dst2;
      }
   }

   return true;
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

   AudacityProject *p = GetActiveProject();

   if (p && p->HasKeyboardCapture()) {
      wxCommandEvent e(EVT_RELEASE_KEYBOARD);
      e.SetEventObject(this);
      GetParent()->GetEventHandler()->ProcessEvent(e);
   }

   if (t && t->GetKind() == Track::Label) {
      wxCommandEvent e(EVT_CAPTURE_KEYBOARD);
      e.SetEventObject(this);
      GetParent()->GetEventHandler()->ProcessEvent(e);
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
   Refresh( false);
}

/**********************************************************************

  TrackInfo code is destined to move out of this file.
  Code should become a lot cleaner when we have sizers.  

**********************************************************************/

TrackInfo::TrackInfo(wxWindow * pParentIn)
{

   pParent = pParentIn;

   // Populate sliders array
   for (unsigned int i = 0; i < kInitialSliders; ++i) {
      MakeMoreSliders();
   }
   mSliderOffset = 0;

   int fontSize = 10;
   mFont.Create(fontSize, wxSWISS, wxNORMAL, wxNORMAL);

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

TrackInfo::~TrackInfo()
{
   unsigned int i;
   for(i=0; i<mGains.Count(); i++)
      delete mGains[i];
   for(i=0; i<mPans.Count(); i++)
      delete mPans[i];
}

static const int kTrackInfoWidth = 100; 
static const int kTrackInfoBtnSize = 16; // widely used dimension, usually height

int TrackInfo::GetTrackInfoWidth() const 
{
   return kTrackInfoWidth;
}

void TrackInfo::GetCloseBoxRect(const wxRect r, wxRect & dest) const
{
   dest.x = r.x;
   dest.y = r.y;
   dest.width = kTrackInfoBtnSize; 
   dest.height = kTrackInfoBtnSize;
}

void TrackInfo::GetTitleBarRect(const wxRect r, wxRect & dest) const
{
   dest.x = r.x + kTrackInfoBtnSize; // to right of CloseBoxRect
   dest.y = r.y;
   dest.width = kTrackInfoWidth - r.x - kTrackInfoBtnSize; // to right of CloseBoxRect
   dest.height = kTrackInfoBtnSize;
}

void TrackInfo::GetMuteSoloRect(const wxRect r, wxRect & dest, bool solo, bool bHasSoloButton) const
{
   dest.x = r.x ;
   dest.y = r.y + 50;
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

void TrackInfo::GetGainRect(const wxRect r, wxRect & dest) const
{
   dest.x = r.x + 7;
   dest.y = r.y + 70;
   dest.width = 84;
   dest.height = 25;
}

void TrackInfo::GetPanRect(const wxRect r, wxRect & dest) const
{
   dest.x = r.x + 7;
   dest.y = r.y + 100;
   dest.width = 84;
   dest.height = 25;
}

void TrackInfo::GetMinimizeRect(const wxRect r, wxRect &dest) const
{
   const int kBlankWidth = kTrackInfoBtnSize + 4;
   dest.x = r.x + kBlankWidth;
   dest.y = r.y + r.height - 19;
   // Width is kTrackInfoWidth less space on left for track select and on right for sync-lock icon.
   dest.width = kTrackInfoWidth - (2 * kBlankWidth);
   dest.height = kTrackInfoBtnSize; 
}

void TrackInfo::GetSyncLockIconRect(const wxRect r, wxRect &dest) const
{
   dest.x = r.x + kTrackInfoWidth - kTrackInfoBtnSize - 4; // to right of minimize button
   dest.y = r.y + r.height - 19;
   dest.width = kTrackInfoBtnSize;
   dest.height = kTrackInfoBtnSize;
}


/// \todo Probably should move to 'Utils.cpp'.
void TrackInfo::SetTrackInfoFont(wxDC * dc)
{
   dc->SetFont(mFont);
}

void TrackInfo::DrawBordersWithin(wxDC* dc, const wxRect r, bool bHasMuteSolo)
{
   AColor::Dark(dc, false); // same color as border of toolbars (ToolBar::OnPaint())

   // below close box and title bar
   AColor::Line(*dc, r.x, r.y + kTrackInfoBtnSize, kTrackInfoWidth, r.y + kTrackInfoBtnSize); 

   // between close box and title bar
   AColor::Line(*dc, r.x + kTrackInfoBtnSize, r.y, r.x + kTrackInfoBtnSize, r.y + kTrackInfoBtnSize); 

   if( bHasMuteSolo && (r.height > (66+18) ))
   {
      AColor::Line(*dc, r.x, r.y + 50, kTrackInfoWidth, r.y + 50);   // above mute/solo
      AColor::Line(*dc, r.x + 48 , r.y + 50, r.x + 48, r.y + 66);    // between mute/solo
      AColor::Line(*dc, r.x, r.y + 66, kTrackInfoWidth, r.y + 66);   // below mute/solo
   }

   // left of and above minimize button
   wxRect minimizeRect;
   this->GetMinimizeRect(r, minimizeRect);
   AColor::Line(*dc, minimizeRect.x - 1, minimizeRect.y, 
                  minimizeRect.x - 1, minimizeRect.y + minimizeRect.height);  
   AColor::Line(*dc, minimizeRect.x, minimizeRect.y - 1, 
                  minimizeRect.x + minimizeRect.width, minimizeRect.y - 1);  
}

void TrackInfo::DrawBackground(wxDC * dc, const wxRect r, bool bSelected,
   bool WXUNUSED(bHasMuteSolo), const int labelw, const int WXUNUSED(vrul))
{
   // fill in label
   wxRect fill = r;
   fill.width = labelw-4;
   AColor::MediumTrackInfo(dc, bSelected);
   dc->DrawRectangle(fill); 

   // Vaughan, 2010-09-16: No more bevels around controls area. Now only around buttons.
   //if( bHasMuteSolo )
   //{
   //   fill=wxRect( r.x+1, r.y+17, vrul-6, 32); 
   //   AColor::BevelTrackInfo( *dc, true, fill );
   //
   //   fill=wxRect( r.x+1, r.y+67, fill.width, r.height-87); 
   //   AColor::BevelTrackInfo( *dc, true, fill );
   //}
   //else
   //{
   //   fill=wxRect( r.x+1, r.y+17, vrul-6, r.height-37); 
   //   AColor::BevelTrackInfo( *dc, true, fill );
   //}
}

void TrackInfo::GetTrackControlsRect(const wxRect r, wxRect & dest) const
{
   wxRect top;
   wxRect bot;

   GetTitleBarRect(r, top);
   GetMinimizeRect(r, bot);

   dest.x = r.x;
   dest.width = kTrackInfoWidth - dest.x;
   dest.y = top.GetBottom() + 2; // BUG
   dest.height = bot.GetTop() - top.GetBottom() - 2;
}


void TrackInfo::DrawCloseBox(wxDC * dc, const wxRect r, bool down)
{
   wxRect bev;
   GetCloseBoxRect(r, bev);

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

void TrackInfo::DrawTitleBar(wxDC * dc, const wxRect r, Track * t,
                              bool down)
{
   wxRect bev;
   GetTitleBarRect(r, bev);
   bev.Inflate(-1, -1);

   // Draw title text
   SetTrackInfoFont(dc);
   wxString titleStr = t->GetName();
   int allowableWidth = kTrackInfoWidth - 38 - kLeftInset;

   long textWidth, textHeight;
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
   dc->DrawText(titleStr, r.x + kTrackInfoBtnSize + 3, r.y + 2);

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
void TrackInfo::DrawMuteSolo(wxDC * dc, const wxRect r, Track * t,
                              bool down, bool solo, bool bHasSoloButton)
{
   wxRect bev;
   if( solo && !bHasSoloButton )
      return;
   GetMuteSoloRect(r, bev, solo, bHasSoloButton);
   bev.Inflate(-1, -1);

   if (bev.y + bev.height >= r.y + r.height - 19)
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

   long textWidth, textHeight;
   wxString str = (solo) ?
      /* i18n-hint: This is on a button that will silence this track.*/
      _("Solo") : 
      /* i18n-hint: This is on a button that will silence all the other tracks.*/
      _("Mute");

   SetTrackInfoFont(dc);
   dc->GetTextExtent(str, &textWidth, &textHeight);
   dc->DrawText(str, bev.x + (bev.width - textWidth) / 2, bev.y);

   AColor::BevelTrackInfo(*dc, (solo?t->GetSolo():t->GetMute()) == down, bev);

   if (solo && !down) {
      // Update the mute button, which may be grayed out depending on
      // the state of the solo button.
      DrawMuteSolo(dc, r, t, false, false, bHasSoloButton);
   }
}

// Draw the minimize button *and* the sync-lock track icon, if necessary.
void TrackInfo::DrawMinimize(wxDC * dc, const wxRect r, Track * t, bool down)
{
   wxRect bev;
   GetMinimizeRect(r, bev);

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

void TrackInfo::MakeMoreSliders()
{
   wxRect r(0, 0, 1000, 1000);
   wxRect gainRect;
   wxRect panRect;

   GetGainRect(r, gainRect);
   GetPanRect(r, panRect);

   /* i18n-hint: Title of the Gain slider, used to adjust the volume */
   LWSlider *slider = new LWSlider(pParent, _("Gain"),
                                   wxPoint(gainRect.x, gainRect.y),
                                   wxSize(gainRect.width, gainRect.height),
                                   DB_SLIDER);
   slider->SetDefaultValue(1.0);
   mGains.Add(slider);

   /* i18n-hint: Title of the Pan slider, used to move the sound left or right */
   slider = new LWSlider(pParent, _("Pan"),
                         wxPoint(panRect.x, panRect.y),
                         wxSize(panRect.width, panRect.height),
                         PAN_SLIDER);
   slider->SetDefaultValue(0.0);
   mPans.Add(slider);
}

// This covers the case where kInitialSliders - kSliderPageFlip is not big
// enough
void TrackInfo::EnsureSufficientSliders(int index)
{
   while (mGains.Count() < (unsigned int)index - mSliderOffset + 1 ||
          mPans.Count() < (unsigned int)index - mSliderOffset + 1)
      MakeMoreSliders();
}

#ifdef EXPERIMENTAL_MIDI_OUT
void TrackInfo::DrawVelocitySlider(wxDC *dc, NoteTrack *t, wxRect r)
{
    wxRect gainRect;
    int index = t->GetIndex();
    EnsureSufficientSliders(index);
    GetGainRect(r, gainRect);
    if (gainRect.y + gainRect.height < r.y + r.height - 19) {
       mGains[index]->SetStyle(VEL_SLIDER);
       GainSlider(index)->Move(wxPoint(gainRect.x, gainRect.y));
       GainSlider(index)->Set(t->GetGain());
       GainSlider(index)->OnPaint(*dc, t->GetSelected());
    }
}
#endif

void TrackInfo::DrawSliders(wxDC *dc, WaveTrack *t, wxRect r)
{
   wxRect gainRect;
   wxRect panRect;
   int index = t->GetIndex();

   EnsureSufficientSliders( index );

   GetGainRect(r, gainRect);
   GetPanRect(r, panRect);

   if (gainRect.y + gainRect.height < r.y + r.height - 19) {
#ifdef EXPERIMENTAL_MIDI_OUT
      GainSlider(index)->SetStyle(DB_SLIDER);
#endif
      GainSlider(index)->Move(wxPoint(gainRect.x, gainRect.y));
      GainSlider(index)->Set(t->GetGain());
      GainSlider(index)->OnPaint(*dc, t->GetSelected());
   }

   if (panRect.y + panRect.height < r.y + r.height - 19) {
      PanSlider(index)->Move(wxPoint(panRect.x, panRect.y));
      PanSlider(index)->Set(t->GetPan());
      PanSlider(index)->OnPaint(*dc, t->GetSelected());
   }
}

void TrackInfo::UpdateSliderOffset(Track *t)
{
   if (!t)
      return;

   // Ensure that the specified track is: (a) at least the second track in the
   // arrays (if it's the second track in a stereo pair the first must be in);
   // (b) no farther in than kSliderPageFlip
   int newSliderOffset = (int)mSliderOffset;
   if ((unsigned int)t->GetIndex() < mSliderOffset + 1 ||
         (unsigned int)t->GetIndex() > mSliderOffset + kSliderPageFlip) {
      newSliderOffset = t->GetIndex() - (int)kSliderPageFlip / 2;
   }
   // Slider offset can't be negative
   if (newSliderOffset < 0) newSliderOffset = 0;

   // Rotate the array values if necessary
   int delta = newSliderOffset - (int)mSliderOffset;

   // If the rotation is greater than the array size, none of the old values
   // are preserved, so don't bother rotating.
   if (abs(delta) >= (int)mGains.Count())
      delta = 0;

   if (delta > 0) {
      // Circularly shift values down in the arrays (temp arrays needed to
      // avoid reading written-over values)
      LWSliderArray tempGains;
      LWSliderArray tempPans;
      tempGains.SetCount(delta);
      tempPans.SetCount(delta);

      for (int i = 0; i < (int)mGains.Count(); ++i) {
         int src = (i + delta) % (int)mGains.Count();
         if (src < 0) src += (int)mGains.Count();

         if (i < delta) {
            // Save a copy of the first `delta` values
            tempGains[i] = mGains[i];
            tempPans[i] = mPans[i];
         }
         if (src >= delta) {
            // These values have not been overwritten
            mGains[i] = mGains[src];
            mPans[i] = mPans[src];
         }
         else {
            // These ones have
            mGains[i] = tempGains[src];
            mPans[i] = tempPans[src];
         }
      }
   }
   else if (delta < 0) {
      // Circularly shift values up in the arrays
      LWSliderArray tempGains;
      LWSliderArray tempPans;
      tempGains.SetCount(mGains.Count());
      tempPans.SetCount(mPans.Count());

      // Iterating backwards to do this
      for (int i = mGains.Count() - 1; i >= 0; --i) {
         int src = (i + delta) % (int)mGains.Count();
         if (src < 0) src += (int)mGains.Count();

         if (i >= (int)mGains.Count() + delta) {
            // Save a copy of the last `delta` values
            tempGains[i] = mGains[i];
            tempPans[i] = mPans[i];
         }
         if (src < (int)mGains.Count() + delta) {
            // These values have not been overwritten
            mGains[i] = mGains[src];
            mPans[i] = mPans[src];
         }
         else {
            // These ones have
            mGains[i] = tempGains[src];
            mPans[i] = tempPans[src];
         }
      }
   }

   mSliderOffset = newSliderOffset;
}

LWSlider * TrackInfo::GainSlider(int trackIndex)
{
   return mGains[trackIndex - mSliderOffset];
}

LWSlider * TrackInfo::PanSlider(int trackIndex)
{
   return mPans[trackIndex - mSliderOffset];
}

static TrackPanel * TrackPanelFactory(wxWindow * parent,
   wxWindowID id,
   const wxPoint & pos,
   const wxSize & size,
   TrackList * tracks,
   ViewInfo * viewInfo,
   TrackPanelListener * listener,
   AdornedRulerPanel * ruler)
{
   return new TrackPanel(
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
              TrackList * tracks,
              ViewInfo * viewInfo,
              TrackPanelListener * listener,
              AdornedRulerPanel * ruler) = TrackPanelFactory;

