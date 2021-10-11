/**********************************************************************

  Audacity: A Digital Audio Editor

  Lyrics.cpp

  Dominic Mazzoni
  Vaughan Johnson

**********************************************************************/

#include "Lyrics.h"

#include <math.h>

#include <wx/app.h>
#include <wx/dcclient.h>
#include <wx/defs.h>
#include <wx/dcmemory.h>
#include <wx/frame.h>
#include <wx/mimetype.h>

#include "AudioIO.h"
#include "Project.h"
#include "ProjectWindowBase.h"
#include "LabelTrack.h"
#include "commands/CommandManager.h"
#include "UndoManager.h"
#include "ViewInfo.h"


BEGIN_EVENT_TABLE(HighlightTextCtrl, wxTextCtrl)
   EVT_MOUSE_EVENTS(HighlightTextCtrl::OnMouseEvent)
END_EVENT_TABLE()

HighlightTextCtrl::HighlightTextCtrl(LyricsPanel* parent,
                                       wxWindowID id,
                                       const wxString& value /* = {} */,
                                       const wxPoint& pos /*= wxDefaultPosition*/,
                                       const wxSize& size /*= wxDefaultSize*/)
: wxTextCtrl(parent, id, // wxWindow* parent, wxWindowID id,
               value, // const wxString& value = {},
               pos, // const wxPoint& pos = wxDefaultPosition,
               size, // const wxSize& size = wxDefaultSize,
               wxTE_MULTILINE | wxTE_READONLY | wxTE_RICH | wxTE_RICH2 | wxTE_AUTO_URL | wxTE_NOHIDESEL), //v | wxHSCROLL)
   mLyricsPanel(parent)
{
}

void HighlightTextCtrl::OnMouseEvent(wxMouseEvent& event)
{
   if (event.ButtonUp())
   {
      long from, to;
      this->GetSelection(&from, &to);

      int nCurSyl = mLyricsPanel->GetCurrentSyllableIndex();
      int nNewSyl = mLyricsPanel->FindSyllable(from);
      if (nNewSyl != nCurSyl)
      {
         Syllable* pCurSyl = mLyricsPanel->GetSyllable(nNewSyl);
         auto pProj = FindProjectFromWindow( this );
         auto &selectedRegion = ViewInfo::Get( *pProj ).selectedRegion;
         selectedRegion.setT0( pCurSyl->t );

         //v Should probably select to end as in
         // SelectUtilities::Handler::OnSelectCursorEnd,
         // but better to generalize that in AudacityProject methods.
         selectedRegion.setT1( pCurSyl->t );
      }
   }

   event.Skip();
}


//v static const kHighlightTextCtrlID = 7654;

BEGIN_EVENT_TABLE(LyricsPanel, wxPanelWrapper)
   EVT_KEY_DOWN(LyricsPanel::OnKeyEvent)
   EVT_PAINT(LyricsPanel::OnPaint)
   EVT_SIZE(LyricsPanel::OnSize)

   //v Doesn't seem to be a way to capture a selection event in a read-only wxTextCtrl.
   //    EVT_COMMAND_LEFT_CLICK(kHighlightTextCtrlID, LyricsPanel::OnHighlightTextCtrl)
END_EVENT_TABLE()

IMPLEMENT_CLASS(LyricsPanel, wxPanel)

LyricsPanel::LyricsPanel(wxWindow* parent, wxWindowID id,
               AudacityProject *project,
               const wxPoint& pos /*= wxDefaultPosition*/,
               const wxSize& size /*= wxDefaultSize*/) :
   wxPanelWrapper(parent, id, pos, size, wxWANTS_CHARS),
   mWidth(size.x), mHeight(size.y)
   , mProject(project)
{
   mKaraokeHeight = mHeight;
   mLyricsStyle = kBouncingBallLyrics; // default
   mKaraokeFontSize = this->GetDefaultFontSize(); // Call only after mLyricsPanelStyle is set.

   this->SetBackgroundColour(*wxWHITE);

   mHighlightTextCtrl =
      safenew HighlightTextCtrl(this, -1, // wxWindow* parent, wxWindowID id,
                              wxT(""), // const wxString& value = {},
                              wxPoint(0, 0), // const wxPoint& pos = wxDefaultPosition,
                              size); // const wxSize& size = wxDefaultSize
   this->SetHighlightFont();
   mHighlightTextCtrl->Show(mLyricsStyle == kHighlightLyrics); // test, in case we conditionalize the default, above


   mT = 0.0;

   Clear();
   Finish(0.0);

   #ifdef __WXMAC__
      wxSizeEvent dummyEvent;
      OnSize(dummyEvent);
   #endif

   parent->Bind(wxEVT_SHOW, &LyricsPanel::OnShow, this);

   project->Bind(EVT_UNDO_PUSHED, &LyricsPanel::UpdateLyrics, this);
   project->Bind(EVT_UNDO_MODIFIED, &LyricsPanel::UpdateLyrics, this);
   project->Bind(EVT_UNDO_OR_REDO, &LyricsPanel::UpdateLyrics, this);
   project->Bind(EVT_UNDO_RESET, &LyricsPanel::UpdateLyrics, this);

   wxTheApp->Bind(EVT_AUDIOIO_PLAYBACK, &LyricsPanel::OnStartStop, this);
   wxTheApp->Bind(EVT_AUDIOIO_CAPTURE, &LyricsPanel::OnStartStop, this);
}

LyricsPanel::~LyricsPanel()
{
}

#define I_FIRST_REAL_SYLLABLE 2

void LyricsPanel::Clear()
{
   mSyllables.clear();
   mText = wxT("");

   // Add two dummy syllables at the beginning
   mSyllables.push_back(Syllable());
   mSyllables[0].t = -2.0;
   mSyllables.push_back(Syllable());
   mSyllables[1].t = -1.0;

   mHighlightTextCtrl->Clear();
}

void LyricsPanel::AddLabels(const LabelTrack *pLT)
{
   const size_t numLabels = pLT->GetNumLabels();
   wxString highlightText;
   for (size_t ii = 0; ii < numLabels; ++ii) {
      const LabelStruct *const pLabel = pLT->GetLabel(ii);
      Add(pLabel->getT0(), pLabel->title, highlightText);
   }
   mHighlightTextCtrl->AppendText(highlightText);
}

void LyricsPanel::Add(double t, const wxString &syllable, wxString &highlightText)
{
   int i = mSyllables.size();

   {
      Syllable &prevSyllable = mSyllables[i - 1];

      if (prevSyllable.t == t) {
         // We can't have two syllables with the same time, so append
         // this to the end of the previous one if they're at the
         // same time.
         prevSyllable.text += syllable;
         prevSyllable.textWithSpace += syllable;
         prevSyllable.char1 += syllable.length();
         return;
      }
   }

   mSyllables.push_back(Syllable());
   Syllable &thisSyllable = mSyllables[i];
   thisSyllable.t = t;
   thisSyllable.text = syllable;

   thisSyllable.char0 = mText.length();

   // Put a space between syllables unless the previous one
   // ended in a hyphen
   if (i > 0 &&
         // mSyllables[i-1].text.length() > 0 &&
         mSyllables[i - 1].text.Right(1) != wxT("-"))
      thisSyllable.textWithSpace = wxT(" ") + syllable;
   else
      thisSyllable.textWithSpace = syllable;

   mText += thisSyllable.textWithSpace;
   thisSyllable.char1 = mText.length();

   int nTextLen = thisSyllable.textWithSpace.length();
   if ((nTextLen > 0) && (thisSyllable.textWithSpace.Right(1) == wxT("_")))
      highlightText += (thisSyllable.textWithSpace.Left(nTextLen - 1) + wxT("\n"));
   else
      highlightText += thisSyllable.textWithSpace;
}

void LyricsPanel::Finish(double finalT)
{
   // Add 3 dummy syllables at the end
   int i = mSyllables.size();
   mSyllables.push_back(Syllable());
   mSyllables[i].t = finalT + 1.0;
   mSyllables.push_back(Syllable());
   mSyllables[i+1].t = finalT + 2.0;
   mSyllables.push_back(Syllable());
   mSyllables[i+2].t = finalT + 3.0;

   // Mark measurements as invalid
   mMeasurementsDone = false; // only for drawn text
   mCurrentSyllable = 0;
   mHighlightTextCtrl->ShowPosition(0);
}

// Binary-search for the syllable whose char0 <= startChar <= char1.
int LyricsPanel::FindSyllable(long startChar)
{
   int i1, i2;

   i1 = 0;
   i2 = mSyllables.size();
   while (i2 > i1+1) {
      int pmid = (i1+i2)/2;
      if (mSyllables[pmid].char0 > startChar)
         i2 = pmid;
      else
         i1 = pmid;
   }

   if (i1 < 2)
      i1 = 2;
   if (i1 > (int)(mSyllables.size()) - 3)
      i1 = mSyllables.size() - 3;

   return i1;
}

void LyricsPanel::SetLyricsStyle(const LyricsStyle newLyricsStyle)
{
   if (mLyricsStyle == newLyricsStyle)
      return;

   mLyricsStyle = newLyricsStyle;
   mHighlightTextCtrl->Show(mLyricsStyle == kHighlightLyrics);

   wxSizeEvent ignore;
   this->OnSize(ignore);
}


unsigned int LyricsPanel::GetDefaultFontSize() const
{
   return (mLyricsStyle == kBouncingBallLyrics) ? 48 : 10;
}

void LyricsPanel::SetDrawnFont(wxDC *dc)
{
   dc->SetFont(wxFont(mKaraokeFontSize, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL));
}

void LyricsPanel::SetHighlightFont() // for kHighlightLyrics
{
   wxFont newFont(mKaraokeFontSize, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL);
   mHighlightTextCtrl->SetDefaultStyle(wxTextAttr(wxNullColour, wxNullColour, newFont));
   mHighlightTextCtrl->SetStyle(0, mHighlightTextCtrl->GetLastPosition(),
                                 wxTextAttr(wxNullColour, wxNullColour, newFont));
}

void LyricsPanel::Measure(wxDC *dc) // only for drawn text
{
   this->SetDrawnFont(dc);
   int width = 0, height = 0;

   const int kIndent = 4;
   int x = 2*kIndent;

   unsigned int i;
   for(i = 0; i < mSyllables.size(); i++) {
      if ((i < I_FIRST_REAL_SYLLABLE) || // Clear() starts the list with I_FIRST_REAL_SYLLABLE dummies.
            (i >= mSyllables.size() - 3)) // Finish() ends with 3 dummies.
      {
         dc->GetTextExtent(wxT("DUMMY"), &width, &height); // Get the correct height even if we're at i=0.
         width = 0;
      }
      else {
         dc->GetTextExtent(mSyllables[i].textWithSpace, &width, &height);
      }

      // Add some space between words; the space is normally small but
      // when there's a long pause relative to the previous word, insert
      // extra space.
      int extraWidth;
      if (i >= I_FIRST_REAL_SYLLABLE && i < mSyllables.size() - 2)
      {
         double deltaThis = mSyllables[i+1].t - mSyllables[i].t;
         double deltaPrev = mSyllables[i].t - mSyllables[i-1].t;

         double ratio;
         if (deltaPrev > 0.0)
            ratio = deltaThis / deltaPrev;
         else
            ratio = deltaThis;

         if (ratio > 2.0)
            extraWidth = 15 + (int)(15.0 * ratio);
         else
            extraWidth = 15;
      }
      else
         extraWidth = 20;

      mSyllables[i].width = width + extraWidth;
      mSyllables[i].leftX = x;
      mSyllables[i].x = x + width/2;
      x += mSyllables[i].width;
   }

   mTextHeight = height;

   mMeasurementsDone = true;
}

// Binary-search for the syllable with the largest time not greater than t
int LyricsPanel::FindSyllable(double t)
{
   int i1, i2;

   i1 = 0;
   i2 = mSyllables.size();
   while (i2 > i1+1) {
      int pmid = (i1+i2)/2;
      if (mSyllables[pmid].t > t)
         i2 = pmid;
      else
         i1 = pmid;
   }

   if (i1 < 2)
      i1 = 2;
   if (i1 > (int)(mSyllables.size()) - 3)
      i1 = mSyllables.size() - 3;

   return i1;
}

// Bouncing Ball:
// Given the current time t, returns the x/y position of the scrolling
// karaoke display.  For some syllable i, when t==mSyllables[i].t,
// it will return mSyllables[i].x for outX and 0 for outY.
// In-between words, outX is interpolated using smooth acceleration
// between the two neighboring words, and y is a positive number indicating
// the bouncing ball height
void LyricsPanel::GetKaraokePosition(double t,
                                int *outX, double *outY)
{
   *outX = 0;
   *outY = 0;

   if (t < mSyllables[I_FIRST_REAL_SYLLABLE].t || t > mSyllables[mSyllables.size() - 3].t)
      return;

   int i0, i1, i2, i3;
   int x0, x1, x2, x3;
   double t0, t1, t2, t3;
   i1 = FindSyllable(t);
   i2 = i1 + 1;

   // Because we've padded the syllables with two dummies at the beginning
   // and end, we know that i0...i3 will always exist.  Also, we've made
   // sure that we don't ever have two of the same time, so t2>t1 strictly.
   //
   //                          t
   //                          \/
   // time:  t0           t1                 t2              t3
   // pos:   x0           x1                 x2              x3
   // index: i0           i1                 i2              i3
   // vel:               vel1               vel2

   i0 = i1 - 1;
   i3 = i2 + 1;

   x0 = mSyllables[i0].x;
   x1 = mSyllables[i1].x;
   x2 = mSyllables[i2].x;
   x3 = mSyllables[i3].x;

   t0 = mSyllables[i0].t;
   t1 = mSyllables[i1].t;
   t2 = mSyllables[i2].t;
   t3 = mSyllables[i3].t;

   double linear_vel0 = (x1 - x0) / (t1 - t0);
   double linear_vel1 = (x2 - x1) / (t2 - t1);
   double linear_vel2 = (x3 - x2) / (t3 - t2);

   // average velocities
   double v1 = (linear_vel0 + linear_vel1) / 2;
   double v2 = (linear_vel1 + linear_vel2) / 2;

   // Solve a cubic equation f(t) = at^3 + bt^2 + ct + d
   // which gives the position x as a function of
   // (t - t1), by constraining f(0), f'(0), f(t2-t1), f'(t2-t1)
   double delta_t = t2 - t1;
   double delta_x = x2 - x1;
   v1 *= delta_t;
   v2 *= delta_t;
   double a = v1 + v2 - 2*delta_x;
   double b = 3*delta_x - 2*v1 - v2;
   double c = v1;
   double d = x1;

   t = (t - t1) / (t2 - t1);
   double xx = a*t*t*t + b*t*t + c*t + d;

   // Unfortunately sometimes our cubic goes backwards.  This is a quick
   // hack to stop that from happening.
   if (xx < x1)
      xx = x1;

   *outX = (int)xx;

   // The y position is a simple cosine curve; the max height is a
   // function of the time.
   double height = t2 - t1 > 4.0? 1.0: sqrt((t2-t1)/4.0);
   *outY = height * sin(M_PI * t);
}

void LyricsPanel::Update(double t)
{
   if (t < 0.0)
   {
      // TrackPanel::OnTimer passes gAudioIO->GetStreamTime(), which is -DBL_MAX if !IsStreamActive().
      // In that case, use the selection start time.
      auto pProj = FindProjectFromWindow( this );
      const auto &selectedRegion = ViewInfo::Get( *pProj ).selectedRegion;
      mT = selectedRegion.t0();
   }
   else
      mT = t;

   if (mLyricsStyle == kBouncingBallLyrics)
   {
      wxRect karaokeRect(0, 0, mWidth, mKaraokeHeight);
      this->Refresh(false, &karaokeRect);
   }

   int i = FindSyllable(mT);
   if (i == mCurrentSyllable)
      return;

   mCurrentSyllable = i;

   if (mLyricsStyle == kHighlightLyrics)
   {
      mHighlightTextCtrl->SetSelection(mSyllables[i].char0, mSyllables[i].char1);

      //v No trail for now.
      //// Leave a trail behind the selection, by highlighting.
      //if (i == I_FIRST_REAL_SYLLABLE)
      //   // Reset the trail to zero.
      //   mHighlightTextCtrl->SetStyle(0, mHighlightTextCtrl->GetLastPosition(), wxTextAttr(wxNullColour, *wxWHITE));
      //// Mark the trail for mSyllables[i].
      //mHighlightTextCtrl->SetStyle(mSyllables[i].char0, mSyllables[i].char1, wxTextAttr(wxNullColour, *wxLIGHT_GREY));

      //v Too much flicker:   mHighlightTextCtrl->ShowPosition(mSyllables[i].char0);
   }
}

void LyricsPanel::UpdateLyrics(wxEvent &e)
{
   e.Skip();

   // It's crucial to not do that repopulating during playback.
   auto gAudioIO = AudioIOBase::Get();
   if (gAudioIO->IsStreamActive()) {
      mDelayedUpdate = true;
      return;
   }

   Clear();

   if (!mProject)
      return;

   // Lyrics come from only the first label track.
   auto pLabelTrack =
      *TrackList::Get( *mProject ).Any< const LabelTrack >().begin();
   if (!pLabelTrack)
      return;

   // The code that updates the lyrics is rather expensive when there
   // are a lot of labels.
   // So - bail out early if the lyrics window is not visible.
   // We will later force an update when the lyrics window is made visible.
   auto parent = dynamic_cast<wxFrame*>(GetParent());
   if( !(parent && parent->IsVisible()) )
      return;

   AddLabels(pLabelTrack);
   Finish(pLabelTrack->GetEndTime());
   const auto &selectedRegion = ViewInfo::Get( *mProject ).selectedRegion;
   Update(selectedRegion.t0());
}

void LyricsPanel::OnStartStop(wxCommandEvent &e)
{
   e.Skip();
   if ( !e.GetInt() && mDelayedUpdate ) {
      mDelayedUpdate = false;
      UpdateLyrics( e );
   }
}

void LyricsPanel::OnShow(wxShowEvent &e)
{
   e.Skip();
   if (e.IsShown())
      UpdateLyrics(e);
}

void LyricsPanel::OnKeyEvent(wxKeyEvent & event)
{
   auto project = FindProjectFromWindow( this );
   auto &commandManager = CommandManager::Get( *project );
   commandManager.FilterKeyEvent(project, event, true);
   event.Skip();
}

void LyricsPanel::OnPaint(wxPaintEvent & WXUNUSED(event))
{
   wxPaintDC dc(this);
   DoPaint(dc);
}

void LyricsPanel::DoPaint(wxDC &dc)
{
   if (!this->GetParent()->IsShown())
      return;

   if (mLyricsStyle == kBouncingBallLyrics)
   {
      if (!mMeasurementsDone)
         Measure(&dc);

      #ifdef __WXMAC__
         // Mac OS X automatically double-buffers the screen for you,
         // so our bitmap is unnecessary
         HandlePaint(dc);
      #else
         wxBitmap bitmap(mWidth, mKaraokeHeight);
         wxMemoryDC memDC;
         memDC.SelectObject(bitmap);
         HandlePaint(memDC);
         dc.Blit(0, 0, mWidth, mKaraokeHeight, &memDC, 0, 0, wxCOPY, FALSE);
      #endif
   }
   else // (mLyricsStyle == kHighlightLyrics)
   {
      //v causes flicker in ported version
      //    this->SetHighlightFont();
   }
}

void LyricsPanel::OnSize(wxSizeEvent & WXUNUSED(event))
{
   GetClientSize(&mWidth, &mHeight);

   mKaraokeHeight = mHeight;

   mKaraokeFontSize =
      (int)((float)(this->GetDefaultFontSize() * mHeight) / (float)LYRICS_DEFAULT_HEIGHT);
   // Usually don't get the size window we want, usually less than
   // LYRICS_DEFAULT_HEIGHT, so bump it a little.
   mKaraokeFontSize += 2;

   if (mLyricsStyle == kBouncingBallLyrics)
   {
      mMeasurementsDone = false;
      wxClientDC dc(this);
      this->DoPaint(dc);
   }
   else // (mLyricsStyle == kHighlightLyrics)
   {
      mHighlightTextCtrl->SetSize(mWidth, mKaraokeHeight);
      this->SetHighlightFont();
   }

   this->Refresh(false);
}

//v Doesn't seem to be a way to capture a selection event in a read-only wxTextCtrl.
//void LyricsPanel::OnHighlightTextCtrl(wxCommandEvent & event)
//{
//   long from, to;
//
//   mHighlightTextCtrl->GetSelection(&from, &to);
//   // TODO: Find the start time of the corresponding syllable and set playback to start there.
//}

void LyricsPanel::HandlePaint(wxDC &dc)
{
   wxASSERT(mLyricsStyle == kBouncingBallLyrics);
   dc.SetBrush(*wxWHITE_BRUSH);
   dc.DrawRectangle(0, 0, mWidth, mKaraokeHeight);

   this->HandlePaint_BouncingBall(dc);
}

void LyricsPanel::HandlePaint_BouncingBall(wxDC &dc)
{
   int ctr = mWidth / 2;
   int x;
   double y;
   GetKaraokePosition(mT, &x, &y);

   dc.SetTextForeground(wxColour(238, 0, 102));
   bool changedColor = false;

   SetDrawnFont(&dc);
   unsigned int i;
   wxCoord yTextTop = mKaraokeHeight - mTextHeight - 4;
   for(i = 0; i < mSyllables.size(); i++) {
      if (mSyllables[i].x + mSyllables[i].width < (x - ctr))
         continue;
      if (mSyllables[i].x > x + ctr)
         continue;

      if (!changedColor && mSyllables[i].x >= x) {
         dc.SetTextForeground(*wxBLACK);
         changedColor = true;
      }

      wxString text = mSyllables[i].text;
      if (text.length() > 0 && text.Right(1) == wxT("_")) {
         text = text.Left(text.length() - 1);
      }

      dc.DrawText(text,
                  mSyllables[i].leftX + ctr - x,
                  yTextTop);
   }

   int ballRadius = (int)(mTextHeight / 8.0);
   int bounceTop = ballRadius * 2;
   int bounceHeight = yTextTop - bounceTop;
   int yi = (int)(yTextTop - 4 - (y * bounceHeight));

   if (mT >= 0.0) {
      wxRect ball(ctr - ballRadius, yi - ballRadius, 2 * ballRadius, 2 * ballRadius);
      dc.SetBrush(wxBrush(wxColour(238, 0, 102), wxBRUSHSTYLE_SOLID));
      dc.DrawEllipse(ball);
   }
}

