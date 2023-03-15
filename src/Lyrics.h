/**********************************************************************

  Audacity: A Digital Audio Editor

  Lyrics.h

  Dominic Mazzoni
  Vaughan Johnson

**********************************************************************/

#ifndef __AUDACITY_LYRICS__
#define __AUDACITY_LYRICS__



#include <vector>
#include <wx/textctrl.h> // to inherit
#include "commands/CommandManagerWindowClasses.h"
#include "wxPanelWrapper.h" // to inherit
#include "Observer.h"

class AudacityProject;
struct AudioIOEvent;
class LabelTrack;

#define LYRICS_DEFAULT_WIDTH 608
#define LYRICS_DEFAULT_HEIGHT 280

/// \brief used in LyricsPanel, a Syllable gives positional information to
/// be used with the bouncing ball effect.
struct Syllable {
   Syllable() = default;
   Syllable( const Syllable& ) = default;
   Syllable& operator= ( const Syllable& ) = default;
   //Syllable( Syllable && ) = default;
   //Syllable& operator= ( Syllable&& ) = default;

   double t;
   wxString text;
   wxString textWithSpace;
   int char0; // index of first char of syllable in LyricsPanel::mText, used only for kHighlightLyrics
   int char1; // index of last  char of syllable in LyricsPanel::mText, used only for kHighlightLyrics
   int width;
   int leftX;
   int x; // centerX, used only for kBouncingBallLyrics
};

class LyricsPanel;

// Override wxTextCtrl to handle selection events, which the parent ignores if the control is read-only.
class HighlightTextCtrl final : public wxTextCtrl
{
public:
   HighlightTextCtrl(LyricsPanel* parent,
                     wxWindowID id,
                     const wxString& value = {},
                     const wxPoint& pos = wxDefaultPosition,
                     const wxSize& size = wxDefaultSize);
   virtual ~HighlightTextCtrl() {};

   void OnMouseEvent(wxMouseEvent &evt);

private:
   LyricsPanel* mLyricsPanel;

   DECLARE_EVENT_TABLE()
};


/**************************************************************//**

\brief LyricsPanel is a panel that paints the bouncing
ball and the lyrics text.
*******************************************************************/
class LyricsPanel final
   : public wxPanelWrapper
   , public NonKeystrokeInterceptingWindow
{
   DECLARE_DYNAMIC_CLASS(LyricsPanel)

   enum LyricsStyle {
      kBouncingBallLyrics, // Lyrics move from right to left with bouncing ball.
      // kGuitarTab,       //v <<future>> Guitar Tablature moves from right to left.
      kHighlightLyrics,    // Lyrics show in scrolling page and syllables highlight successively.
   };

 public:
   LyricsPanel(wxWindow* parent, wxWindowID id,
          AudacityProject *project,
          const wxPoint& pos = wxDefaultPosition,
          const wxSize& size = wxDefaultSize);
   virtual ~LyricsPanel();

   int FindSyllable(long startChar); // Find the syllable whose char0 <= startChar <= char1.
   int GetCurrentSyllableIndex() { return mCurrentSyllable; };
   Syllable* GetSyllable(int nSyl) { return &(mSyllables[nSyl]); };
   void SetCurrentSyllableIndex(int nSyl) { mCurrentSyllable = nSyl; };

   LyricsStyle GetLyricsStyle() { return mLyricsStyle; };
   void SetLyricsStyle(const LyricsStyle newLyricsStyle);

   void Update(double t);
   void UpdateLyrics(struct UndoRedoMessage);
   void DoUpdateLyrics();
   void OnShow(wxShowEvent& e);
   void OnStartStop(AudioIOEvent);

   //
   // Event handlers
   //
   void OnKeyEvent(wxKeyEvent & event);
   void DoPaint(wxDC &dc);
   void OnPaint(wxPaintEvent &evt);
   void OnSize(wxSizeEvent &evt);

   // Doesn't seem to be a way to capture a selection event in a read-only wxTextCtrl.
   // Thus the HighlightTextCtrl class.
   //    void OnHighlightTextCtrl(wxCommandEvent & event);

   void HandlePaint(wxDC &dc);
   void HandlePaint_BouncingBall(wxDC &dc);

   void HandleLayout();

private:
   void Clear();
   void AddLabels(const LabelTrack *pLT);
   void Finish(double finalT);

   void Add(double t, const wxString &syllable, wxString &highlightText);

   unsigned int GetDefaultFontSize() const; // Depends on mLyricsStyle. Call only after mLyricsStyle is set.

   void SetDrawnFont(wxDC *dc); // for kBouncingBallLyrics
   void SetHighlightFont(); // for kHighlightLyrics

   void Measure(wxDC *dc);
   int FindSyllable(double t);
   void GetKaraokePosition(double t, int *outX, double *outY);

private:
   Observer::Subscription mAudioIOSubscription
      , mUndoSubscription
   ;

   int            mWidth;  // client width
   int            mHeight; // client height

   int            mKaraokeHeight; //v mHeight - mBrandingHeight (so just mHeight now that Branding is removed).
   unsigned int   mKaraokeFontSize;

   LyricsStyle          mLyricsStyle; // default kHighlightLyrics
   HighlightTextCtrl*   mHighlightTextCtrl; // only for kHighlightLyrics

   double         mT;

   int            mCurrentSyllable;
   std::vector<Syllable>  mSyllables;
   wxString       mText;

   int            mTextHeight; // only for drawn text
   bool           mMeasurementsDone; // only for drawn text

   wxWeakRef<AudacityProject> mProject;
   bool           mDelayedUpdate{ false };

   DECLARE_EVENT_TABLE()
};

#endif // __AUDACITY_LYRICS__
