/**********************************************************************

Audacity: A Digital Audio Editor

StretchHandle.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_STRETCH_HANDLE__
#define __AUDACITY_STRETCH_HANDLE__

#include "../../../../UIHandle.h"

#include "../../../../MemoryX.h"

class Alg_seq;
struct HitTestResult;
class NoteTrack;
class Track;
class ViewInfo;
class wxCursor;

class StretchHandle : public UIHandle
{
public:
   enum StretchEnum {
      stretchNone = 0, // false value!
      stretchLeft,
      stretchCenter,
      stretchRight
   };

   // Stretching applies to a selected region after quantizing the
   // region to beat boundaries (subbeat stretching is not supported,
   // but maybe it should be enabled with shift or ctrl or something)
   // Stretching can drag the left boundary (the right stays fixed),
   // the right boundary (the left stays fixed), or the center (splits
   // the selection into two parts: when left part grows, the right
   // part shrinks, keeping the leftmost and rightmost boundaries
   // fixed.
   struct StretchState {
      StretchEnum mMode { stretchCenter }; // remembers what to drag

      using QuantizedTimeAndBeat = std::pair< double, double >;

      QuantizedTimeAndBeat mBeatCenter { 0, 0 };
      QuantizedTimeAndBeat mBeat0 { 0, 0 };
      QuantizedTimeAndBeat mBeat1 { 0, 0 };
      double mLeftBeats {}; // how many beats from left to cursor
      double mRightBeats {}; // how many beats from cursor to right

      double mOrigSel0Quantized { -1 }, mOrigSel1Quantized { -1 };
   };
   
private:
   StretchHandle();
   StretchHandle(const StretchHandle&);
   StretchHandle &operator=(const StretchHandle&);
   static StretchHandle& Instance();
   static HitTestPreview HitPreview(StretchEnum stretchMode, bool unsafe);

public:
   static HitTestResult HitTest
      ( const TrackPanelMouseEvent &event, const AudacityProject *pProject,
        NoteTrack *pTrack, StretchState &state );

   virtual ~StretchHandle();

   virtual Result Click
      (const TrackPanelMouseEvent &event, AudacityProject *pProject);

   virtual Result Drag
      (const TrackPanelMouseEvent &event, AudacityProject *pProject);

   virtual HitTestPreview Preview
      (const TrackPanelMouseEvent &event, const AudacityProject *pProject);

   virtual Result Release
      (const TrackPanelMouseEvent &event, AudacityProject *pProject,
      wxWindow *pParent);

   virtual Result Cancel(AudacityProject *pProject);

   bool StopsOnKeystroke() override { return true; }

private:
   void Stretch
      (AudacityProject *pProject, int mouseXCoordinate, int trackLeftEdge, Track *pTrack);

   NoteTrack *mpTrack{};
   int mLeftEdge{ -1 };

   StretchState mStretchState{};
};

#endif
