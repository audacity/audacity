#include "TimelineContext.h"

#include "log.h"

TimelineContext::TimelineContext(QQuickItem* parent)
   : QQuickItem(parent)
{

}

TimelineContext::~TimelineContext() = default;

qint64 TimelineContext::timeToPosition(double time) const
{
   double t = 0.5 + mZoom * (time - mOffset);
   if( t < INT64_MIN )
      return INT64_MIN;
   if( t > INT64_MAX )
      return INT64_MAX;
   t = floor( t );
   return static_cast<qint64>(t);
}

double TimelineContext::positionToTime(qint64 position) const
{
   return mOffset + position / mZoom;
}

double TimelineContext::offset() const
{
   return mOffset;
}

void TimelineContext::setOffset(double newOffset)
{
   if(mOffset != newOffset)
   {
      mOffset = newOffset;
      emit offsetChanged();
   }
}

double TimelineContext::zoom() const
{
   return mZoom;
}

void TimelineContext::setZoom(double zoom)
{
   if(mZoom != zoom)
   {
      mZoom = zoom;
      emit zoomChanged();
   }
}

double TimelineContext::selectionStartTime() const
{
   return mSelecitonStartTime;
}

void TimelineContext::setSelectionStartTime(double time)
{
   if(mSelecitonStartTime != time)
   {
      mSelecitonStartTime = time;
      emit selectionEndTimeChanged();
   }
}

double TimelineContext::selectionEndTime() const
{
   return mSelectionEndTime;
}

void TimelineContext::setSelectionEndTime(double time)
{
   if(mSelectionEndTime != time)
   {
      mSelectionEndTime = time;
      emit selectionEndTimeChanged();
   }
}

int TimelineContext::tracksOriginOffset() const
{
   return mTracksOriginOffset;
}

void TimelineContext::setTracksOriginOffset(int offset)
{
   if(mTracksOriginOffset != offset)
   {
      mTracksOriginOffset = offset;
      emit tracksOriginOffsetChanged();
   }
}
