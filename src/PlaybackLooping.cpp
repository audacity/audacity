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
#include "ProjectWindow.h"
#include "CellularPanel.h"
#include "AColor.h"
#include "tracks/ui/TrackView.h"

#include <wx/event.h>
#include <wx/graphics.h>

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

	const auto& viewInfo = ViewInfo::Get(*pProject);
	const auto& playRegion = viewInfo.playRegion;

	switch (mProcessingState)
	{
	case ProcessingStates::kIdle:
		if (processingEvents == ProcessingEvents::kClick)
		{
			mOldPlayRegion = playRegion;
			//SelectUtilities::LockPlayRegion(*pProject);
			EnableLooping(true);

			auto loopingPos = viewInfo.PositionToTime(mousePosX, viewInfo.GetLeftOffset(), false);
			mLoopedPlayRegion.SetStart(loopingPos);

			p0 = mousePosX;
			p1 = mousePosX;

			mProcessingState = ProcessingStates::kEnterLooping;
		}
		break;

	case ProcessingStates::kEnterLooping:
		if (!IsLoopingEnabled() /*&& !playRegion.Locked()*/)
			return false;

		if (processingEvents == ProcessingEvents::kDrag)
		{
			auto width = viewInfo.GetTracksUsableWidth();
			mousePosX = std::max(mousePosX, viewInfo.GetLeftOffset());
			mousePosX = std::min(mousePosX, viewInfo.GetLeftOffset() + width - 1);

			auto loopingPos = viewInfo.PositionToTime(mousePosX, viewInfo.GetLeftOffset(), false);
			mLoopedPlayRegion.SetEnd(loopingPos);

			p1 = mousePosX;
		}
		else if (processingEvents == ProcessingEvents::kRelease)
		{
			auto loopingPos = viewInfo.PositionToTime(mousePosX, viewInfo.GetLeftOffset(), false);
			mLoopedPlayRegion.SetEnd(loopingPos);

			p1 = mousePosX;

			mProcessingState = ProcessingStates::kLooping;
		}
		break;

	case ProcessingStates::kLooping:
		// NOTE: if cursor point to looping range processing like kDragging (dragging looping area).

		// Cursor point to outside looping range -> create new looping range or cancel (Click -> Release).
		if (processingEvents == ProcessingEvents::kClick)
		{
			//SelectUtilities::LockPlayRegion(*pProject);
			EnableLooping(true);

			auto loopingPos = viewInfo.PositionToTime(mousePosX, viewInfo.GetLeftOffset(), false);
			mLoopedPlayRegion.SetStart(loopingPos);

			mProcessingState = ProcessingStates::kEnterLooping;
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
	if (event.event.LeftIsDown())
	{
		if (!PlaybackLooping::GetInstance().LoopingProcessor(pProject, event.event.m_x, PlaybackLooping::ProcessingEvents::kRelease))
		{
			return RefreshCode::RefreshNone;
		}
	}

	return RefreshCode::RefreshCell;
}

UIHandle::Result PlaybackLoopingHandle::Cancel(AudacityProject* pProject)
{
	return RefreshCode::RefreshCell;
}

/* PlaybackLoopingIndication implementation */

PlaybackLoopingIndication::PlaybackLoopingIndication(wxRect timeBarBoundaries)
	: mTimeBarBoundaries(timeBarBoundaries)
{
	;
}

unsigned PlaybackLoopingIndication::SequenceNumber() const
{
	return 30;
}

std::pair<wxRect, bool> PlaybackLoopingIndication::DoGetRectangle(wxSize size)
{
	const int p0 = PlaybackLooping::GetInstance().p0;
	const int p1 = PlaybackLooping::GetInstance().p1;

	wxRect r;
	r.x = p0;
	r.y = mTimeBarBoundaries.y;
	r.width = p1 - p0 - 1;
	r.height = mTimeBarBoundaries.height;

	return std::make_pair<wxRect, bool>(wxRect(r), p0 != p1 && PlaybackLooping::GetInstance().IsProcessing());
}

void PlaybackLoopingIndication::Draw(OverlayPanel& panel, wxDC& dc)
{
	const int p0 = PlaybackLooping::GetInstance().p0;
	const int p1 = PlaybackLooping::GetInstance().p1;

	if (p0 != p1 && PlaybackLooping::GetInstance().IsProcessing())
	{
		//if (dynamic_cast<PlaybackLoopingCell*>(panel.GetFirst()))
		//auto& backDC = panel.GetBackingDCForRepaint();
		{
			// Create graphics context from it
			//wxGraphicsContext* gc = wxGraphicsContext::Create(&dc);

			dc.SetBrush(wxBrush(theTheme.Colour(clrRecordingPen))); // NOTE: check on looping enable and set the fit color.
			dc.SetPen(wxPen(theTheme.Colour(clrPlaybackPen)));
			
			wxRect r;
			r.x = p0;
			r.y = mTimeBarBoundaries.y;
			r.width = p1 - p0 - 1;
			r.height = mTimeBarBoundaries.height;
			dc.DrawRectangle(r);
		}

#if 0
		auto pCellularPanel = dynamic_cast<CellularPanel*>(&panel);
		if (!pCellularPanel) {
			wxASSERT(false);
			return;
		}
		pCellularPanel->VisitCells(
			[&](const wxRect& rect, TrackPanelCell& cell)
			{
				const auto pTrackView = dynamic_cast<TrackView*>(&cell);
				if (!pTrackView)
					return;
				
				// Draw the NEW indicator in its NEW location
				AColor::Line(dc, p0, mTimeBarBoundaries.y, p1, mTimeBarBoundaries.height);
			}
		);
#endif

		//AColor::IndicatorColor(&dc, true);

#if 0
		wxRect r;
		r.x = p0;
		r.y = mTimeBarBoundaries.y;
		r.width = p1 - p0 - 1;
		r.height = mTimeBarBoundaries.height;
		dc.DrawRectangle(r);
#endif
	}
}

/* PlaybackLoopingCell implementation */

PlaybackLoopingCell::PlaybackLoopingCell()
{
	;
}

std::vector<UIHandlePtr> PlaybackLoopingCell::HitTest(const TrackPanelMouseState& state, const AudacityProject* pProject)
{
	mTimeBarBoundaries = state.rect;

#if 0
	if (!mOverlay)
	{
		mOverlay = std::make_shared<PlaybackLoopingIndication>();
		auto pCellularPanel = dynamic_cast<CellularPanel*>(&GetProjectPanel(*pProject));

		if (!pCellularPanel) {
			wxASSERT(false);
		}
		else {
			pCellularPanel->AddOverlay(mOverlay);
		}
	}
#endif

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
