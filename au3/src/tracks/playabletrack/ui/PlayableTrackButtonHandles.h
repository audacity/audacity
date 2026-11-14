/**********************************************************************

Audacity: A Digital Audio Editor

PlayableTrackButtonHandles.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_PLAYABLE_TRACK_BUTTON_HANDLES__
#define __AUDACITY_PLAYABLE_TRACK_BUTTON_HANDLES__

#include "../../ui/ButtonHandle.h"
class wxMouseState;

class AUDACITY_DLL_API MuteButtonHandle final : public ButtonHandle
{
    MuteButtonHandle(const MuteButtonHandle&) = delete;

public:
    explicit MuteButtonHandle(const std::shared_ptr<Track>& pTrack, const wxRect& rect);

    MuteButtonHandle& operator=(const MuteButtonHandle&) = default;

    virtual ~MuteButtonHandle();

protected:
    Result CommitChanges(const wxMouseEvent& event, AudacityProject* pProject, wxWindow* pParent)
    override;

    TranslatableString Tip(
        const wxMouseState& state, AudacityProject&) const override;

    bool StopsOnKeystroke() override { return true; }

public:
    static UIHandlePtr HitTest(std::weak_ptr<MuteButtonHandle>& holder, const wxMouseState& state, const wxRect& rect,
                               const AudacityProject* pProject, const std::shared_ptr<Track>& pTrack);
};

////////////////////////////////////////////////////////////////////////////////

class AUDACITY_DLL_API SoloButtonHandle final : public ButtonHandle
{
    SoloButtonHandle(const SoloButtonHandle&) = delete;

public:
    explicit SoloButtonHandle(const std::shared_ptr<Track>& pTrack, const wxRect& rect);

    SoloButtonHandle& operator=(const SoloButtonHandle&) = default;

    virtual ~SoloButtonHandle();

protected:
    Result CommitChanges(const wxMouseEvent& event, AudacityProject* pProject, wxWindow* pParent)
    override;

    TranslatableString Tip(
        const wxMouseState& state, AudacityProject&) const override;

    bool StopsOnKeystroke() override { return true; }

public:
    static UIHandlePtr HitTest(std::weak_ptr<SoloButtonHandle>& holder, const wxMouseState& state, const wxRect& rect,
                               const AudacityProject* pProject, const std::shared_ptr<Track>& pTrack);
};

////////////////////////////////////////////////////////////////////////////////

class EffectsButtonHandle final : public ButtonHandle
{
    EffectsButtonHandle(const EffectsButtonHandle&) = delete;

public:
    explicit EffectsButtonHandle(const std::shared_ptr<Track>& pTrack, const wxRect& rect);

    EffectsButtonHandle& operator=(const EffectsButtonHandle&) = default;

    virtual ~EffectsButtonHandle();

protected:
    Result CommitChanges(const wxMouseEvent& event, AudacityProject* pProject, wxWindow* pParent)
    override;

    TranslatableString Tip(
        const wxMouseState& state, AudacityProject&) const override;

    bool StopsOnKeystroke() override { return true; }

public:
    static UIHandlePtr HitTest(std::weak_ptr<EffectsButtonHandle>& holder, const wxMouseState& state, const wxRect& rect,
                               const AudacityProject* pProject, const std::shared_ptr<Track>& pTrack);
};

#endif
