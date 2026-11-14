/**********************************************************************

Audacity: A Digital Audio Editor

CommonChannelView.h

Paul Licameli split from class TrackView (now called ChannelView)

**********************************************************************/

#ifndef __AUDACITY_COMMON_TRACK_VIEW__
#define __AUDACITY_COMMON_TRACK_VIEW__

#include "ChannelView.h" // to inherit

class Envelope;
class SelectHandle;
class TimeShiftHandle;
class ZoomInfo;

//! Implements some hit-testing shared by many ChannelView subtypes
class AUDACITY_DLL_API CommonChannelView /* not final */ : public ChannelView
{
public:
    using ChannelView::ChannelView;

    // Delegates the handling to the related TCP cell
    std::shared_ptr<TrackPanelCell> ContextMenuDelegate() override;

    // Cause certain overriding tool modes (Zoom; future ones?) to behave
    // uniformly in all tracks, disregarding track contents.
    // Do not further override this...
    std::vector<UIHandlePtr> HitTest(const TrackPanelMouseState&, const AudacityProject* pProject)
    final;

    void TimeShiftHitTest();

    virtual int GetMinimizedHeight() const override;

    /** \brief Get many envelope points for pixel columns at once,
     * but don't assume uniform time per pixel.
    */
    static void GetEnvelopeValues(const Envelope& env, double aligned_time, double sampleDur, double* buffer, int bufferLen, int leftOffset,
                                  const ZoomInfo& zoomInfo);

protected:
    // Rather override this for subclasses:
    virtual std::vector<UIHandlePtr> DetailedHitTest(const TrackPanelMouseState&, const AudacityProject* pProject, int currentTool,
                                                     bool bMultiTool)
    = 0;

    std::weak_ptr<SelectHandle> mSelectHandle;

public:
    std::weak_ptr<TimeShiftHandle> mTimeShiftHandle;
};

#endif
