/**********************************************************************

Audacity: A Digital Audio Editor

BrushHandle.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_BRUSH_HANDLE__
#define __AUDACITY_BRUSH_HANDLE__

#include <vector>
#include "ProjectSettings.h"
#include "tracks/playabletrack/wavetrack/ui/SpectrumView.h"
#include "../../UIHandle.h"
#include "SelectedRegion.h"

class AudacityProject;
class SelectionStateChanger;
class SpectrumAnalyst;
class Track;
class ChannelView;
class TrackList;
class ViewInfo;
class WaveTrack;
class wxMouseState;

class AUDACITY_DLL_API BrushHandle : public UIHandle
{
   BrushHandle(const BrushHandle&);

public:
   /*! Callbacks from the brush handle */
   // Call Init() on click; Commit() on release; destroy on cancel
   class StateSaver {
   public:
      virtual ~StateSaver();
      virtual void Init( AudacityProject &project, bool clearAll ) = 0;

      void Commit()
      {
         mCommitted = true;
      }

   protected:
      bool mCommitted = false;
   };

   explicit BrushHandle(
      std::shared_ptr<StateSaver> pStateSaver,
      const std::shared_ptr<ChannelView> &pChannelView,
      const TrackList &trackList,
      const TrackPanelMouseState &st, const ViewInfo &viewInfo,
      const std::shared_ptr<SpectralData> &pSpectralData,
      const ProjectSettings &pSettings);

   BrushHandle &operator=(const BrushHandle&) = default;
   
   virtual ~BrushHandle();

   std::shared_ptr<const Track> FindTrack() const override;

   bool IsDragging() const override;

   void Enter(bool forward, AudacityProject *pProject) override;

   bool Escape(AudacityProject *pProject) override;

   void HandleHopBinData(int hopNum, int freqBinNum);

   Result Click
      (const TrackPanelMouseEvent &event, AudacityProject *pProject) override;

   Result Drag
      (const TrackPanelMouseEvent &event, AudacityProject *pProject) override;

   HitTestPreview Preview
      (const TrackPanelMouseState &state, AudacityProject *pProject)
      override;

   Result Release
      (const TrackPanelMouseEvent &event, AudacityProject *pProject,
       wxWindow *pParent) override;

   Result Cancel(AudacityProject*) override;

private:
   std::shared_ptr<StateSaver> mpStateSaver;
   std::shared_ptr<SpectralData> mpSpectralData;

   std::shared_ptr<WaveChannel> FindChannel();

   // TrackPanelDrawable implementation
   void Draw(
      TrackPanelDrawingContext &context,
      const wxRect &rect, unsigned iPass ) override;

   std::weak_ptr<ChannelView> mpView;
   wxRect mRect{};

   // Example: For window size of 1024, with ratio 0.01
   // It searches for (+-) 1024*0.01 = 10 bins
   double mFreqSnappingRatio { 0.01 };

   // Overtones bin will be selected according to the 0.999x of the dragged area
   double mOvertonesThreshold { 0.999 };

   bool mIsSmartSelection, mIsOvertones;
   long long mSampleCountUpperBound, mSampleCountLowerBound;
   wxInt64 mFreqUpperBound, mFreqLowerBound;
   int mMostRecentX{ -1 }, mMostRecentY{ -1 };
   int mBrushRadius;
   bool mbCtrlDown;

   std::shared_ptr<SelectionStateChanger> mSelectionStateChanger;
};
#endif
