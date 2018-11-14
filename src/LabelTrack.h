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
#include <wx/pen.h>
#include <wx/clipbrd.h>


class wxFont;
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


struct LabelTrackHit;
struct TrackPanelDrawingContext;

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

class LabelGlyphHandle;
class LabelTextHandle;

class AUDACITY_DLL_API LabelTrack final : public Track
{
   friend class LabelStruct;

 public:
   static void DoEditLabels(
      AudacityProject &project, LabelTrack *lt = nullptr, int index = -1);
   static int DialogForLabelName(
      AudacityProject &project, const SelectedRegion& region,
      const wxString& initialValue, wxString& value);

   bool IsGoodLabelFirstKey(const wxKeyEvent & evt);
   bool IsGoodLabelEditKey(const wxKeyEvent & evt);
   bool IsTextSelected();

   void CreateCustomGlyphs();
   LabelTrack(const std::shared_ptr<DirManager> &projDirManager);
   LabelTrack(const LabelTrack &orig);

   virtual ~ LabelTrack();

   std::vector<UIHandlePtr> DetailedHitTest
      (const TrackPanelMouseState &state,
       const AudacityProject *pProject, int currentTool, bool bMultiTool)
      override;

   bool DoCaptureKey(wxKeyEvent &event);
   unsigned CaptureKey
     (wxKeyEvent &event, ViewInfo &viewInfo, wxWindow *pParent) override;

   unsigned KeyDown
      (wxKeyEvent &event, ViewInfo &viewInfo, wxWindow *pParent) override;

   unsigned Char
      (wxKeyEvent &event, ViewInfo &viewInfo, wxWindow *pParent) override;

   void SetOffset(double dOffset) override;

   static const int DefaultFontSize = 12;

   static wxFont GetFont(const wxString &faceName, int size = DefaultFontSize);
   static void ResetFont();

   void Draw( TrackPanelDrawingContext &context, const wxRect & r ) const;

   int getSelectedIndex() const { return mSelIndex; }

   double GetOffset() const override;
   double GetStartTime() const override;
   double GetEndTime() const override;

   using Holder = std::shared_ptr<LabelTrack>;
   Track::Holder Duplicate() const override;

   void SetSelected(bool s) override;

   bool HandleXMLTag(const wxChar *tag, const wxChar **attrs) override;
   XMLTagHandler *HandleXMLChild(const wxChar *tag) override;
   void WriteXML(XMLWriter &xmlFile) const override;

#if LEGACY_PROJECT_FILE_SUPPORT
   bool Load(wxTextFile * in, DirManager * dirManager) override;
   bool Save(wxTextFile * out, bool overwrite) override;
#endif

   Track::Holder Cut  (double t0, double t1) override;
   Track::Holder Copy (double t0, double t1, bool forClipboard = true) const override;
   void Clear(double t0, double t1) override;
   void Paste(double t, const Track * src) override;
   bool Repeat(double t0, double t1, int n);

   void Silence(double t0, double t1) override;
   void InsertSilence(double t, double len) override;
   void OverGlyph(LabelTrackHit &hit, int x, int y) const;
   static wxBitmap & GetGlyph( int i);


   struct Flags {
      int mInitialCursorPos, mCurrentCursorPos, mSelIndex;
      bool mRightDragging, mDrawCursor;
   };
   void ResetFlags();
   Flags SaveFlags() const
   {
      return {
         mInitialCursorPos, mCurrentCursorPos, mSelIndex,
         mRightDragging, mDrawCursor
      };
   }
   void RestoreFlags( const Flags& flags );

   int OverATextBox(int xx, int yy) const;
   bool OverTextBox(const LabelStruct *pLabel, int x, int y) const;
   bool CutSelectedText();
   bool CopySelectedText();
   bool PasteSelectedText(double sel0, double sel1);
   static bool IsTextClipSupported();

   void HandleGlyphClick
      (LabelTrackHit &hit,
       const wxMouseEvent & evt, const wxRect & r, const ZoomInfo &zoomInfo,
       SelectedRegion *newSel);
   void HandleTextClick
      (const wxMouseEvent & evt, const wxRect & r, const ZoomInfo &zoomInfo,
       SelectedRegion *newSel);
   bool HandleGlyphDragRelease
      (LabelTrackHit &hit,
       const wxMouseEvent & evt, wxRect & r, const ZoomInfo &zoomInfo,
       SelectedRegion *newSel);
   void HandleTextDragRelease(const wxMouseEvent & evt);

   bool OnKeyDown(SelectedRegion &sel, wxKeyEvent & event);
   bool OnChar(SelectedRegion &sel, wxKeyEvent & event);

   void Import(wxTextFile & f);
   void Export(wxTextFile & f) const;

   void Unselect();

   // Whether any label box is selected -- not, whether the track is selected.
   bool HasSelection() const;

   int GetNumLabels() const;
   const LabelStruct *GetLabel(int index) const;

   //This returns the index of the label we just added.
   int AddLabel(const SelectedRegion &region, const wxString &title = {},
      int restoreFocus = -1);
   //And this tells us the index, if there is a label already there.
   int GetLabelIndex(double t, double t1);

   //This deletes the label at given index.
   void DeleteLabel(int index);

   //get current cursor position,
   // relative to the left edge of the track panel
   bool CalcCursorX(int * x) const;

   void CalcHighlightXs(int *x1, int *x2) const;

   void MayAdjustLabel
      ( LabelTrackHit &hit,
        int iLabel, int iEdge, bool bAllowSwapping, double fNewTime);
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

   int FindNextLabel(const SelectedRegion& currentSelection);
   int FindPrevLabel(const SelectedRegion& currentSelection);

 public:
   void SortLabels(LabelTrackHit *pHit = nullptr);
 private:
   TrackKind GetKind() const override { return TrackKind::Label; }

   void ShowContextMenu();
   void OnContextMenu(wxCommandEvent & evt);

   int mSelIndex;              /// Keeps track of the currently selected label
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

   int miLastLabel;                 // used by FindNextLabel and FindPrevLabel

   void ComputeLayout(const wxRect & r, const ZoomInfo &zoomInfo) const;
   void ComputeTextPosition(const wxRect & r, int index) const;

public:
   int FindCurrentCursorPosition(int xPos);
   void SetCurrentCursorPosition(int xPos);

private:
   void calculateFontHeight(wxDC & dc) const;
   void RemoveSelectedText();

   static wxFont msFont;

   std::weak_ptr<LabelGlyphHandle> mGlyphHandle;
   std::weak_ptr<LabelTextHandle> mTextHandle;

protected:
   std::shared_ptr<TrackControls> DoGetControls() override;
   std::shared_ptr<TrackVRulerControls> DoGetVRulerControls() override;
   friend class GetInfoCommand; // to get labels.
   friend class SetLabelCommand; // to set labels.
};

#endif
