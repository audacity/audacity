/**********************************************************************

  Audacity: A Digital Audio Editor

  LabelTrack.h

  Dominic Mazzoni
  James Crook
  Jun Wan

**********************************************************************/

#ifndef _LABELTRACK_
#define _LABELTRACK_

#include "SelectedRegion.h"
#include "Track.h"

#include <wx/brush.h>
#include <wx/event.h>
#include <wx/font.h>
#include <wx/pen.h>
#include <wx/dynarray.h>
#include <wx/string.h>
#include <wx/clipbrd.h>


class wxKeyEvent;
class wxMouseEvent;
class wxTextFile;
class wxWindow;
class wxIcon;
class wxBitmap;
class TrackList;

class AudacityProject;
class DirManager;
class TimeWarper;
class ZoomInfo;


class LabelStruct
{
public:
   // Copies region
   LabelStruct(const SelectedRegion& region, const wxString &aTitle);
   // Copies region but then overwrites other times
   LabelStruct(const SelectedRegion& region, double t0, double t1,
               const wxString &aTitle);
   void DrawLines( wxDC & dc, const wxRect & r) const;
   void DrawGlyphs
      ( wxDC & dc, const wxRect & r, int GlyphLeft, int GlyphRight) const;
   void DrawText( wxDC & dc, const wxRect & r) const;
   void DrawTextBox( wxDC & dc, const wxRect & r) const;
   void DrawHighlight( wxDC & dc, int xPos1, int xPos2, int charHeight) const;
   void getXPos( wxDC & dc, int * xPos1, int cursorPos) const;
   const SelectedRegion &getSelectedRegion() const { return selectedRegion; }
   double getDuration() const { return selectedRegion.duration(); }
   double getT0() const { return selectedRegion.t0(); }
   double getT1() const { return selectedRegion.t1(); }
   // Returns true iff the label got inverted:
   bool AdjustEdge( int iEdge, double fNewTime);
   void MoveLabel( int iEdge, double fNewTime);

   struct BadFormatException {};
   static LabelStruct Import(wxTextFile &file, int &index);

   void Export(wxTextFile &file) const;

   /// Relationships between selection region and labels
   enum TimeRelations
   {
       BEFORE_LABEL,
       AFTER_LABEL,
       SURROUNDS_LABEL,
       WITHIN_LABEL,
       BEGINS_IN_LABEL,
       ENDS_IN_LABEL
   };

   /// Returns relationship between a region described and this label; if
   /// parent is set, it will consider point labels at the very beginning
   /// and end of parent to be within a region that borders them (this makes
   /// it possible to DELETE capture all labels with a Select All).
   TimeRelations RegionRelation(double reg_t0, double reg_t1,
                                const LabelTrack *parent = NULL) const;

public:
   SelectedRegion selectedRegion;
   wxString title; /// Text of the label.
   mutable int width; /// width of the text in pixels.

// Working storage for on-screen layout.
   mutable int x;     /// Pixel position of left hand glyph
   mutable int x1;    /// Pixel position of right hand glyph
   mutable int xText; /// Pixel position of left hand side of text box
   mutable int y;     /// Pixel position of label.

   bool updated;                  /// flag to tell if the label times were updated
};

using LabelArray = std::vector<LabelStruct>;

const int NUM_GLYPH_CONFIGS = 3;
const int NUM_GLYPH_HIGHLIGHTS = 4;
const int MAX_NUM_ROWS =80;


class AUDACITY_DLL_API LabelTrack final : public Track
{
   friend class LabelStruct;

 public:
   bool IsGoodLabelFirstKey(const wxKeyEvent & evt);
   bool IsGoodLabelEditKey(const wxKeyEvent & evt);
   bool IsTextSelected();

   void CreateCustomGlyphs();
   LabelTrack(const std::shared_ptr<DirManager> &projDirManager);
   LabelTrack(const LabelTrack &orig);

   virtual ~ LabelTrack();
   void SetOffset(double dOffset) override;

   static const int DefaultFontSize = 12;

   static wxFont GetFont(const wxString &faceName, int size = DefaultFontSize);
   static void ResetFont();

   void Draw(wxDC & dc, const wxRect & r,
             const SelectedRegion &selectedRegion,
             const ZoomInfo &zoomInfo) const;

   int getSelectedIndex() const { return mSelIndex; }
   bool IsAdjustingLabel() const { return mIsAdjustingLabel; }

   int GetKind() const override { return Label; }

   double GetOffset() const override;
   double GetStartTime() const override;
   double GetEndTime() const override;

   using Holder = std::unique_ptr<LabelTrack>;
   Track::Holder Duplicate() const override;

   void SetSelected(bool s) override;

   bool HandleXMLTag(const wxChar *tag, const wxChar **attrs) override;
   XMLTagHandler *HandleXMLChild(const wxChar *tag) override;
   void WriteXML(XMLWriter &xmlFile) override;

#if LEGACY_PROJECT_FILE_SUPPORT
   bool Load(wxTextFile * in, DirManager * dirManager) override;
   bool Save(wxTextFile * out, bool overwrite) override;
#endif

   Track::Holder Cut  (double t0, double t1) override;
   // JKC Do not add the const modifier to Copy(), Clear()
   // or Paste() because then it
   // is no longer recognised as a virtual function matching the
   // one in Track.
   Track::Holder Copy (double t0, double t1) const override;
   bool Clear(double t0, double t1) override;
   bool Paste(double t, const Track * src) override;
   bool Repeat(double t0, double t1, int n);

   bool Silence(double t0, double t1) override;
   bool InsertSilence(double t, double len) override;
   int OverGlyph(int x, int y);
   static wxBitmap & GetGlyph( int i);


   void ResetFlags();
   int OverATextBox(int xx, int yy) const;
   bool OverTextBox(const LabelStruct *pLabel, int x, int y) const;
   bool CutSelectedText();
   bool CopySelectedText();
   bool PasteSelectedText(double sel0, double sel1);
   static bool IsTextClipSupported();

   void HandleClick(const wxMouseEvent & evt, const wxRect & r, const ZoomInfo &zoomInfo,
      SelectedRegion *newSel);
   bool HandleGlyphDragRelease(const wxMouseEvent & evt, wxRect & r, const ZoomInfo &zoomInfo,
      SelectedRegion *newSel);
   void HandleTextDragRelease(const wxMouseEvent & evt);

   bool CaptureKey(wxKeyEvent & event);
   bool OnKeyDown(SelectedRegion &sel, wxKeyEvent & event);
   bool OnChar(SelectedRegion &sel, wxKeyEvent & event);

   void Import(wxTextFile & f);
   void Export(wxTextFile & f) const;

   void Unselect();

   bool IsSelected() const;

   int GetNumLabels() const;
   const LabelStruct *GetLabel(int index) const;

   //This returns the index of the label we just added.
   int AddLabel(const SelectedRegion &region, const wxString &title = wxT(""),
      int restoreFocus = -1);
   //And this tells us the index, if there is a label already there.
   int GetLabelIndex(double t, double t1);

   //This deletes the label at given index.
   void DeleteLabel(int index);

   //get current cursor position,
   // relative to the left edge of the track panel
   bool CalcCursorX(int * x) const;

   void CalcHighlightXs(int *x1, int *x2) const;

   void MayAdjustLabel( int iLabel, int iEdge, bool bAllowSwapping, double fNewTime);
   void MayMoveLabel( int iLabel, int iEdge, double fNewTime);

   // This pastes labels without shifting existing ones
   bool PasteOver(double t, const Track *src);

   // PRL:  These functions were not used because they were not overrides!  Was that right?
   //Track::Holder SplitCut(double b, double e) /* not override */;
   //bool SplitDelete(double b, double e) /* not override */;

   void ShiftLabelsOnInsert(double length, double pt);
   void ChangeLabelsOnReverse(double b, double e);
   void ScaleLabels(double b, double e, double change);
   double AdjustTimeStampOnScale(double t, double b, double e, double change);
   void WarpLabels(const TimeWarper &warper);

   // Returns tab-separated text of all labels completely within given region
   wxString GetTextOfLabels(double t0, double t1) const;

 public:
   void SortLabels();
   //These two are used by a TrackPanel KLUDGE, which is why they are public.
   bool mbHitCenter;
   //The edge variable tells us what state the icon is in.
   //mOldEdge is useful for telling us when there has been a state change.
   int mOldEdge;
 private:
   void ShowContextMenu();
   void OnContextMenu(wxCommandEvent & evt);

   int mSelIndex;              /// Keeps track of the currently selected label
   int mMouseOverLabelLeft;    /// Keeps track of which left label the mouse is currently over.
   int mMouseOverLabelRight;   /// Keeps track of which right label the mouse is currently over.
   int mxMouseDisplacement;    /// Displacement of mouse cursor from the centre being dragged.
   LabelArray mLabels;

   static int mIconHeight;
   static int mIconWidth;
   static int mTextHeight;
   static bool mbGlyphsReady;
   static wxBitmap mBoundaryGlyphs[NUM_GLYPH_CONFIGS * NUM_GLYPH_HIGHLIGHTS];

   static int mFontHeight;
   int mCurrentCursorPos;                      /// current cursor position
   int mInitialCursorPos;                      /// initial cursor position

   bool mRightDragging;                        /// flag to tell if it's a valid dragging
   bool mDrawCursor;                           /// flag to tell if drawing the
                                                  /// cursor or not
   int mRestoreFocus;                          /// Restore focus to this track
                                                  /// when done editing

   // Set in copied label tracks
   double mClipLen;

   void ComputeLayout(const wxRect & r, const ZoomInfo &zoomInfo) const;
   void ComputeTextPosition(const wxRect & r, int index) const;

public:
   int FindCurrentCursorPosition(int xPos);
   void SetCurrentCursorPosition(int xPos);

private:
   void calculateFontHeight(wxDC & dc) const;
   void RemoveSelectedText();

   bool mIsAdjustingLabel;
   bool mbIsMoving;

   static wxFont msFont;
};

#endif
