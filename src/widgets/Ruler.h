/**********************************************************************

  Audacity: A Digital Audio Editor

  Ruler.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_RULER__
#define __AUDACITY_RULER__

#include <wx/dc.h>
#include <wx/event.h>
#include <wx/font.h>
#include <wx/panel.h>
#include <wx/window.h>
#include "../Envelope.h"
#include "../Experimental.h"

struct ViewInfo;
class AudacityProject;
class TimeTrack;

class AUDACITY_DLL_API Ruler {
 public:

   enum RulerFormat {
      IntFormat,
      RealFormat,
      RealLogFormat,
      TimeFormat,
      LinearDBFormat,
   };

   //
   // Constructor / Destructor
   //

   Ruler();
   ~Ruler();

   //
   // Required Ruler Parameters
   //

   void SetBounds(int left, int top, int right, int bottom);

   // wxHORIZONTAL || wxVERTICAL
   void SetOrientation(int orient);

   // min is the value at (x, y)
   // max is the value at (x+width, y+height)
   // (at the center of the pixel, in both cases)
   void SetRange(double min, double max);

   //
   // Optional Ruler Parameters
   //

   // IntFormat, RealFormat, or TimeFormat
   void SetFormat(RulerFormat format);

   // Specify the name of the units (like "dB") if you
   // want numbers like "1.6" formatted as "1.6 dB".
   void SetUnits(wxString units);

   // Logarithmic
   void SetLog(bool log);

   // Minimum number of pixels between labels
   void SetSpacing(int spacing);

   // If this is true, the edges of the ruler will always
   // receive a label.  If not, the nearest round number is
   // labeled (which may or may not be the edge).
   void SetLabelEdges(bool labelEdges);

   // Makes a vertical ruler hug the left side (instead of right)
   // and a horizontal ruler hug the top (instead of bottom)
   void SetFlip(bool flip);

   // Set it to false if you don't want minor labels.
   void SetMinor(bool value);

   // Good defaults are provided, but you can override here
   void SetFonts(const wxFont &minorFont, const wxFont &majorFont, const wxFont &minorMinorFont);

   // The ruler will not draw text within this (pixel) range.
   // Use this if you have another graphic object obscuring part
   // of the ruler's area.  The values start and end are interpreted
   // relative to the Ruler's local coordinates.
   void OfflimitsPixels(int start, int end);

   //
   // Calculates and returns the maximum size required by the ruler
   //
   void GetMaxSize(wxCoord *width, wxCoord *height);


   // The following functions should allow a custom ruler setup:
   // autosize is a GREAT thing, but for some applications it's
   // useful the definition of a label array and label step by
   // the user.
   void SetCustomMode(bool value);
   // If this is the case, you should provide a wxString array of labels, start
   // label position, and labels step. The range eventually specified will be
   // ignored.
   void SetCustomMajorLabels(wxArrayString *label, int numLabel, int start, int step);
   void SetCustomMinorLabels(wxArrayString *label, int numLabel, int start, int step);

   //
   // Drawing
   //

   // Note that it will not erase for you...
   void Draw(wxDC& dc);
   void Draw(wxDC& dc, TimeTrack* timetrack);
   // If length <> 0, draws lines perpendiculars to ruler corresponding
   // to selected ticks (major, minor, or both), in an adjacent window.
   // You may need to use the offsets if you are using part of the dc for rulers, borders etc.
   void DrawGrid(wxDC& dc, int length, bool minor = true, bool major = true, int xOffset = 0, int yOffset = 0);

   // So we can have white ticks on black...
   void SetTickColour( const wxColour & colour)
   { mTickColour = colour; mPen.SetColour( colour );}

 private:
   void Invalidate();
   void Update();
   void Update(TimeTrack* timetrack);
   void FindTickSizes();
   void FindLinearTickSizes(double UPP);
   wxString LabelString(double d, bool major);

   void Tick(int pos, double d, bool major, bool minor);

   // Another tick generator for custom ruler case (noauto) .
   void TickCustom(int labelIdx, bool major, bool minor);

public:
   bool mbTicksOnly; // true => no line the length of the ruler
   bool mbTicksAtExtremes;
   wxRect mRect;

private:
   wxColour mTickColour;
   wxPen mPen;

   int          mMaxWidth, mMaxHeight;
   int          mLeft, mTop, mRight, mBottom, mLead;
   int          mLength;
   int          mLengthOld;
   wxDC        *mDC;

   wxFont      *mMinorFont, *mMajorFont;
   wxFont      *mMinorMinorFont;
   bool         mUserFonts;

   double       mMin, mMax;

   double       mMajor;
   double       mMinor;

   int          mDigits;

   int         *mUserBits;
   int         *mBits;
   int          mUserBitLen;

   bool         mValid;

   class Label {
    public:
      int pos;
      int lx, ly;
      wxString text;
   };

   int          mNumMajor;
   Label       *mMajorLabels;
   int          mNumMinor;
   Label       *mMinorLabels;
   int          mNumMinorMinor;
   Label       *mMinorMinorLabels;

   // Returns 'zero' label coordinate (for grid drawing)
   int FindZero(Label * label, int len);

   public:
   int GetZeroPosition();

   private:
   int          mOrientation;
   int          mSpacing;
   bool         mHasSetSpacing;
   bool         mLabelEdges;
   RulerFormat  mFormat;
   bool         mLog;
   bool         mFlip;
   bool         mCustom;
   bool         mbMinor;
   bool         mMajorGrid;      //  for grid drawing
   bool         mMinorGrid;      //         .
   int          mGridLineLength; //        end
   wxString     mUnits;
};

class AUDACITY_DLL_API RulerPanel : public wxPanel {
   DECLARE_DYNAMIC_CLASS(RulerPanel)

 public:
   RulerPanel(wxWindow* parent, wxWindowID id,
              const wxPoint& pos = wxDefaultPosition,
              const wxSize& size = wxDefaultSize);

   ~RulerPanel();

   void DoSetSize(int x, int y,
                  int width, int height,
                  int sizeFlags = wxSIZE_AUTO);

   void OnErase(wxEraseEvent &evt);
   void OnPaint(wxPaintEvent &evt);
   void OnSize(wxSizeEvent &evt);

   // We don't need or want to accept focus.
   bool AcceptsFocus() const { return false; }

 public:

   Ruler  ruler;

private:
    DECLARE_EVENT_TABLE()
};

// This is an Audacity Specific ruler panel which additionally
// has border, selection markers, play marker.
// Once TrackPanel uses wxSizers, we will derive it from some
// wxWindow and the GetSize and SetSize functions
// will then be wxWidgets functions instead.
class AUDACITY_DLL_API AdornedRulerPanel : public wxPanel
{
public:
   AdornedRulerPanel(wxWindow* parent,
                     wxWindowID id,
                     const wxPoint& pos = wxDefaultPosition,
                     const wxSize& size = wxDefaultSize,
                     ViewInfo *viewinfo = NULL);

   ~AdornedRulerPanel();

public:
   static int GetRulerHeight() { return 28; }
   void SetLeftOffset(int offset){ mLeftOffset = offset; }

   void DrawCursor(double pos);
   void DrawIndicator(double pos, bool rec);
   void DrawSelection();
   void ClearIndicator();

   void SetPlayRegion(double playRegionStart, double playRegionEnd);
   void ClearPlayRegion();
   void GetPlayRegion(double* playRegionStart, double* playRegionEnd);

   void SetProject(AudacityProject* project) {mProject = project;};
   void GetMaxSize(wxCoord *width, wxCoord *height);

private:
   void OnErase(wxEraseEvent &evt);
   void OnPaint(wxPaintEvent &evt);
   void OnSize(wxSizeEvent &evt);
   void OnMouseEvents(wxMouseEvent &evt);
   void OnCaptureLost(wxMouseCaptureLostEvent &evt);

   void DoDrawBorder(wxDC * dc);
   void DoDrawMarks(wxDC * dc, bool /*text */ );
   void DoDrawCursor(wxDC * dc);
   void DoDrawSelection(wxDC * dc);
   void DoDrawIndicator(wxDC * dc);
   void DoDrawPlayRegion(wxDC * dc);

   double Pos2Time(int p);
   int Time2Pos(double t);

   bool IsWithinMarker(int mousePosX, double markerTime);

   Ruler  ruler;
   ViewInfo *mViewInfo;
   AudacityProject *mProject;

   wxBitmap *mBuffer;

   wxRect mOuter;
   wxRect mInner;

   int mLeftOffset;  // Number of pixels before we hit the 'zero position'.

   double mCurPos;

   int mIndType;     // -1 = No indicator, 0 = Play, 1 = Record
   double mIndPos;

   double mPlayRegionStart;
   double mPlayRegionEnd;

   enum MouseEventState {
      mesNone,
      mesDraggingPlayRegionStart,
      mesDraggingPlayRegionEnd,
      mesSelectingPlayRegionClick,
      mesSelectingPlayRegionRange
   };

   MouseEventState mMouseEventState;
   int mButtonDownMousePos;
   int mLastMouseX;

   DECLARE_EVENT_TABLE()
};

#endif //define __AUDACITY_RULER__
