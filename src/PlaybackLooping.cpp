/*!********************************************************************
*
 Audacity: A Digital Audio Editor

 PlaybackLooping.cpp

 Anton Gerasimov

 **********************************************************************/

#include "PlaybackLooping.h"

#include "Theme.h"
#include "AllThemeResources.h"

#include "ZoomInfo.h"
#include "HitTestResult.h"
#include "RefreshCode.h"
#include "TrackPanelMouseEvent.h"
#include "SelectUtilities.h"
#include "Track.h"

#include <wx/event.h>

PlaybackLooping::PlaybackLooping()
{
	;
}

PlaybackLooping& PlaybackLooping::GetInstance()
{
	static PlaybackLooping instance;

	return instance;
}

bool PlaybackLooping::IsLoopingEnabled() const
{
	return mLoopingEnabled;
}

void PlaybackLooping::EnableLooping(bool enable)
{
	mLoopingEnabled = enable;
}

bool PlaybackLooping::IsProcessing() const
{
	return mProcessingState != ProcessingStates::kIdle;
}

bool PlaybackLooping::LoopingProcessor(AudacityProject* pProject, wxCoord& mousePosX, ProcessingEvents processingEvents)
{
	// This is empty project.
	if (TrackList::Get(*pProject).size() == 0) // NOTE: need check on record?
		return false;

	switch (mProcessingState)
	{
	case ProcessingStates::kIdle:
		if (processingEvents == ProcessingEvents::kClick)
		{
			const auto& viewInfo = ViewInfo::Get(*pProject);
			const auto& playRegion = viewInfo.playRegion;
			mOldPlayRegion = playRegion;
			//SelectUtilities::LockPlayRegion(*pProject);
			EnableLooping(true);

			auto loopingPos = viewInfo.PositionToTime(mousePosX, viewInfo.GetLeftOffset(), false);
			mLoopedPlayRegion.SetStart(loopingPos);

			mProcessingState = ProcessingStates::kEnterLooping;
		}
		break;

	case ProcessingStates::kEnterLooping:
		if (processingEvents == ProcessingEvents::kDrag)
		{
			const auto& viewInfo = ViewInfo::Get(*pProject);
			const auto& playRegion = viewInfo.playRegion;

			if (!IsLoopingEnabled() /*&& !playRegion.Locked()*/)
				return false;

			auto width = viewInfo.GetTracksUsableWidth();
			mousePosX = std::max(mousePosX, viewInfo.GetLeftOffset());
			mousePosX = std::min(mousePosX, viewInfo.GetLeftOffset() + width - 1);

			auto loopingPos = viewInfo.PositionToTime(mousePosX, viewInfo.GetLeftOffset(), false);
			mLoopedPlayRegion.SetEnd(loopingPos);
		}
		break;
	}

	return true;
}

/* PlaybackLoopingHandle implementation */

PlaybackLoopingHandle::PlaybackLoopingHandle()
{
	;
}

UIHandle::Result PlaybackLoopingHandle::Click (const TrackPanelMouseEvent& event, AudacityProject* pProject)
{
	if (event.event.LeftIsDown())
	{
		if (!PlaybackLooping::GetInstance().LoopingProcessor(pProject, event.event.m_x, PlaybackLooping::ProcessingEvents::kClick))
		{
			return RefreshCode::RefreshNone;
		}
	}

	return RefreshCode::RefreshCell;
}

UIHandle::Result PlaybackLoopingHandle::Drag (const TrackPanelMouseEvent& event, AudacityProject* pProject)
{
	if (event.event.LeftIsDown())
	{
		if (!PlaybackLooping::GetInstance().LoopingProcessor(pProject, event.event.m_x, PlaybackLooping::ProcessingEvents::kDrag))
		{
			return RefreshCode::RefreshNone;
		}
	}

	return RefreshCode::RefreshCell;
}

HitTestPreview PlaybackLoopingHandle::Preview(const TrackPanelMouseState& state, AudacityProject* pProject)
{
	// NOTE: need add the check on enter cursor to current loop region if looping not IDLE.

	auto message = XO("PlaybackLoopingHandle message");
	auto tooltip = XO("PlaybackLoopingHandle tooltip");

	static wxCursor cursorArrow{ wxCURSOR_ARROW };

	return {
	   message,
	   &cursorArrow,
	   tooltip,
	};
}

UIHandle::Result PlaybackLoopingHandle::Release (const TrackPanelMouseEvent& event, AudacityProject* pProject, wxWindow* pParent)
{
	// Set looping region to play region or back old paly region.
	const auto& viewInfo = ViewInfo::Get(*pProject);

	const auto& selectedRegion = viewInfo.selectedRegion;

	const auto& playRegion = viewInfo.playRegion;

	auto start = playRegion.GetStart();
	auto end = playRegion.GetEnd();

	return RefreshCode::RefreshCell;
}

UIHandle::Result PlaybackLoopingHandle::Cancel(AudacityProject* pProject)
{
	return RefreshCode::RefreshCell;
}

/* PlaybackLoopingCell implementation */

PlaybackLoopingCell::PlaybackLoopingCell()
{
	;
}

std::vector<UIHandlePtr> PlaybackLoopingCell::HitTest(const TrackPanelMouseState& state, const AudacityProject* pProject)
{
	mTimeBarBoundaries = state.rect;

	std::vector<UIHandlePtr> uiHandlerList;

	auto uiHandlerPointer = std::make_shared<PlaybackLoopingHandle>();
	uiHandlerPointer = AssignUIHandlePtr(mUiHandlerPointer, uiHandlerPointer);
	uiHandlerList.push_back(uiHandlerPointer);
	
	return uiHandlerList;
}

void PlaybackLoopingCell::DrawLoopingRange(wxDC* dc, ViewInfo& viewInfo)
{
	dc->SetBrush(wxBrush(theTheme.Colour(clrRecordingPen))); // NOTE: check on looping enable and set the fit color.
	dc->SetPen(wxPen(theTheme.Colour(clrPlaybackPen)));

	auto& loopingPlayRegion = PlaybackLooping::GetInstance().mLoopedPlayRegion;

	// NOTE: need store this positions into PlaybackLooping service and then just get these.
	const int p0 = std::max(1, static_cast<int>(viewInfo.TimeToPosition(loopingPlayRegion.GetStart(), viewInfo.GetLeftOffset())));
	const int p1 = std::min(mTimeBarBoundaries.width, static_cast<int>(viewInfo.TimeToPosition(loopingPlayRegion.GetEnd(), viewInfo.GetLeftOffset())));

	wxRect r;
	r.x = p0;
	r.y = mTimeBarBoundaries.y;
	r.width = p1 - p0 - 1;
	r.height = mTimeBarBoundaries.height;
	dc->DrawRectangle(r);

	// Draw side borders.

	// Draw side triangles.
}
