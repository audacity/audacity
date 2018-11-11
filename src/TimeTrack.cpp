/**********************************************************************

  Audacity: A Digital Audio Editor

  TimeTrack.cpp

  Dr William Bland

*******************************************************************//*!

\class TimeTrack
\brief A kind of Track used to 'warp time'

*//*******************************************************************/

#include "Audacity.h"
#include "TimeTrack.h"

#include "Experimental.h"

#include <cfloat>
#include <wx/intl.h>
#include "AColor.h"
#include "widgets/Ruler.h"
#include "Envelope.h"
#include "Prefs.h"
#include "Project.h"
#include "TrackArtist.h"
#include "Internat.h"
#include "ViewInfo.h"
#include "AllThemeResources.h"

//TODO-MB: are these sensible values?
#define TIMETRACK_MIN 0.01
#define TIMETRACK_MAX 10.0

std::shared_ptr<TimeTrack> TrackFactory::NewTimeTrack()
{
   return std::make_shared<TimeTrack>(mDirManager, mZoomInfo);
}

TimeTrack::TimeTrack(const std::shared_ptr<DirManager> &projDirManager, const ZoomInfo *zoomInfo):
   Track(projDirManager)
   , mZoomInfo(zoomInfo)
{
   mHeight = 100;

   mRangeLower = 0.9;
   mRangeUpper = 1.1;
   mDisplayLog = false;

   mEnvelope = std::make_unique<Envelope>(true, TIMETRACK_MIN, TIMETRACK_MAX, 1.0);
   mEnvelope->SetTrackLen(DBL_MAX);
   mEnvelope->SetOffset(0);

   SetDefaultName(_("Time Track"));
   SetName(GetDefaultName());

   mRuler = std::make_unique<Ruler>();
   mRuler->SetUseZoomInfo(0, mZoomInfo);
   mRuler->SetLabelEdges(false);
   mRuler->SetFormat(Ruler::TimeFormat);
}

TimeTrack::TimeTrack(const TimeTrack &orig, double *pT0, double *pT1)
   : Track(orig)
   , mZoomInfo(orig.mZoomInfo)
{
   Init(orig);	// this copies the TimeTrack metadata (name, range, etc)

   auto len = DBL_MAX;
   if (pT0 && pT1) {
      len = *pT1 - *pT0;
      mEnvelope = std::make_unique<Envelope>( *orig.mEnvelope, *pT0, *pT1 );
   }
   else
      mEnvelope = std::make_unique<Envelope>( *orig.mEnvelope );
   mEnvelope->SetTrackLen( len );
   mEnvelope->SetOffset(0);

   ///@TODO: Give Ruler:: a copy-constructor instead of this?
   mRuler = std::make_unique<Ruler>();
   mRuler->SetUseZoomInfo(0, mZoomInfo);
   mRuler->SetLabelEdges(false);
   mRuler->SetFormat(Ruler::TimeFormat);
}

// Copy the track metadata but not the contents.
void TimeTrack::Init(const TimeTrack &orig)
{
   Track::Init(orig);
   SetDefaultName(orig.GetDefaultName());
   SetName(orig.GetName());
   SetRangeLower(orig.GetRangeLower());
   SetRangeUpper(orig.GetRangeUpper());
   SetDisplayLog(orig.GetDisplayLog());
}

TimeTrack::~TimeTrack()
{
}

Track::Holder TimeTrack::Cut( double t0, double t1 )
{
   auto result = Copy( t0, t1, false );
   Clear( t0, t1 );
   return result;
}

Track::Holder TimeTrack::Copy( double t0, double t1, bool ) const
{
   return std::make_shared<TimeTrack>( *this, &t0, &t1 );
}

void TimeTrack::Clear(double t0, double t1)
{
   auto sampleTime = 1.0 / GetActiveProject()->GetRate();
   mEnvelope->CollapseRegion( t0, t1, sampleTime );
}

void TimeTrack::Paste(double t, const Track * src)
{
   bool bOk = src && src->TypeSwitch< bool >( [&] (const TimeTrack *tt) {
      auto sampleTime = 1.0 / GetActiveProject()->GetRate();
      mEnvelope->PasteEnvelope
         (t, tt->mEnvelope.get(), sampleTime);
      return true;
   } );

   if (! bOk )
      // THROW_INCONSISTENCY_EXCEPTION // ?
      (void)0;// intentionally do nothing.
}

void TimeTrack::Silence(double WXUNUSED(t0), double WXUNUSED(t1))
{
}

void TimeTrack::InsertSilence(double t, double len)
{
   mEnvelope->InsertSpace(t, len);
}

Track::Holder TimeTrack::Duplicate() const
{
   return std::make_shared<TimeTrack>(*this);
}

bool TimeTrack::GetInterpolateLog() const
{
   return mEnvelope->GetExponential();
}

void TimeTrack::SetInterpolateLog(bool interpolateLog) {
   mEnvelope->SetExponential(interpolateLog);
}

//Compute the (average) warp factor between two non-warped time points
double TimeTrack::ComputeWarpFactor(double t0, double t1) const
{
   return GetEnvelope()->AverageOfInverse(t0, t1);
}

double TimeTrack::ComputeWarpedLength(double t0, double t1) const
{
   return GetEnvelope()->IntegralOfInverse(t0, t1);
}

double TimeTrack::SolveWarpedLength(double t0, double length) const
{
   return GetEnvelope()->SolveIntegralOfInverse(t0, length);
}

bool TimeTrack::HandleXMLTag(const wxChar *tag, const wxChar **attrs)
{
   if (!wxStrcmp(tag, wxT("timetrack"))) {
      mRescaleXMLValues = true; // will be set to false if upper/lower is found
      long nValue;
      while(*attrs) {
         const wxChar *attr = *attrs++;
         const wxChar *value = *attrs++;

         if (!value)
            break;

         const wxString strValue = value;
         if (!wxStrcmp(attr, wxT("name")) && XMLValueChecker::IsGoodString(strValue))
            mName = strValue;
         else if (!wxStrcmp(attr, wxT("height")) &&
                  XMLValueChecker::IsGoodInt(strValue) && strValue.ToLong(&nValue))
            mHeight = nValue;
         else if (!wxStrcmp(attr, wxT("minimized")) &&
                  XMLValueChecker::IsGoodInt(strValue) && strValue.ToLong(&nValue))
            mMinimized = (nValue != 0);
         else if (!wxStrcmp(attr, wxT("rangelower")))
         {
            mRangeLower = Internat::CompatibleToDouble(value);
            mRescaleXMLValues = false;
         }
         else if (!wxStrcmp(attr, wxT("rangeupper")))
         {
            mRangeUpper = Internat::CompatibleToDouble(value);
            mRescaleXMLValues = false;
         }
         else if (!wxStrcmp(attr, wxT("displaylog")) &&
                  XMLValueChecker::IsGoodInt(strValue) && strValue.ToLong(&nValue))
         {
            SetDisplayLog(nValue != 0);
            //TODO-MB: This causes a graphical glitch, TrackPanel should probably be Refresh()ed after loading.
            //         I don't know where to do this though.
         }
         else if (!wxStrcmp(attr, wxT("interpolatelog")) &&
                  XMLValueChecker::IsGoodInt(strValue) && strValue.ToLong(&nValue))
         {
            SetInterpolateLog(nValue != 0);
         }

      } // while
      if(mRescaleXMLValues)
         mEnvelope->SetRange(0.0, 1.0); // this will be restored to the actual range later
      return true;
   }

   return false;
}

void TimeTrack::HandleXMLEndTag(const wxChar * WXUNUSED(tag))
{
   if(mRescaleXMLValues)
   {
      mRescaleXMLValues = false;
      mEnvelope->RescaleValues(mRangeLower, mRangeUpper);
      mEnvelope->SetRange(TIMETRACK_MIN, TIMETRACK_MAX);
   }
}

XMLTagHandler *TimeTrack::HandleXMLChild(const wxChar *tag)
{
   if (!wxStrcmp(tag, wxT("envelope")))
      return mEnvelope.get();

  return NULL;
}

void TimeTrack::WriteXML(XMLWriter &xmlFile) const
// may throw
{
   xmlFile.StartTag(wxT("timetrack"));

   xmlFile.WriteAttr(wxT("name"), mName);
   //xmlFile.WriteAttr(wxT("channel"), mChannel);
   //xmlFile.WriteAttr(wxT("offset"), mOffset, 8);
   xmlFile.WriteAttr(wxT("height"), GetActualHeight());
   xmlFile.WriteAttr(wxT("minimized"), GetMinimized());
   xmlFile.WriteAttr(wxT("rangelower"), mRangeLower, 12);
   xmlFile.WriteAttr(wxT("rangeupper"), mRangeUpper, 12);
   xmlFile.WriteAttr(wxT("displaylog"), GetDisplayLog());
   xmlFile.WriteAttr(wxT("interpolatelog"), GetInterpolateLog());

   mEnvelope->WriteXML(xmlFile);

   xmlFile.EndTag(wxT("timetrack"));
}

#include "TrackPanelDrawingContext.h"
#include "tracks/ui/EnvelopeHandle.h"

void TimeTrack::Draw
( TrackPanelDrawingContext &context, const wxRect & r ) const
{
   auto &dc = context.dc;
   const auto artist = TrackArtist::Get( context );
   const auto &zoomInfo = *artist->pZoomInfo;

   bool highlight = false;
#ifdef EXPERIMENTAL_TRACK_PANEL_HIGHLIGHTING
   auto target = dynamic_cast<EnvelopeHandle*>(context.target.get());
   highlight = target && target->GetEnvelope() == this->GetEnvelope();
#endif

   double min = zoomInfo.PositionToTime(0);
   double max = zoomInfo.PositionToTime(r.width);
   if (min > max)
   {
      wxASSERT(false);
      min = max;
   }

   AColor::UseThemeColour( &dc, clrUnselected );
   dc.DrawRectangle(r);

   //copy this rectangle away for future use.
   wxRect mid = r;

   // Draw the Ruler
   mRuler->SetBounds(r.x, r.y, r.x + r.width - 1, r.y + r.height - 1);
   mRuler->SetRange(min, max);
   mRuler->SetFlip(false);  // If we don't do this, the Ruler doesn't redraw itself when the envelope is modified.
                            // I have no idea why!
                            //
                            // LL:  It's because the ruler only Invalidate()s when the NEW value is different
                            //      than the current value.
   mRuler->SetFlip(GetHeight() > 75 ? true : true); // MB: so why don't we just call Invalidate()? :)
   mRuler->SetTickColour( theTheme.Colour( clrTrackPanelText ));
   mRuler->Draw(dc, this);

   Doubles envValues{ size_t(mid.width) };
   GetEnvelope()->GetValues
      ( 0, 0, envValues.get(), mid.width, 0, zoomInfo );

   wxPen &pen = highlight ? AColor::uglyPen : AColor::envelopePen;
   dc.SetPen( pen );

   double logLower = log(std::max(1.0e-7, mRangeLower)), logUpper = log(std::max(1.0e-7, mRangeUpper));
   for (int x = 0; x < mid.width; x++)
      {
         double y;
         if(mDisplayLog)
            y = (double)mid.height * (logUpper - log(envValues[x])) / (logUpper - logLower);
         else
            y = (double)mid.height * (mRangeUpper - envValues[x]) / (mRangeUpper - mRangeLower);
         int thisy = r.y + (int)y;
         AColor::Line(dc, mid.x + x, thisy - 1, mid.x + x, thisy+2);
      }
}

void TimeTrack::testMe()
{
   GetEnvelope()->Flatten(0.0);
   GetEnvelope()->InsertOrReplace(0.0, 0.2);
   GetEnvelope()->InsertOrReplace(5.0 - 0.001, 0.2);
   GetEnvelope()->InsertOrReplace(5.0 + 0.001, 1.3);
   GetEnvelope()->InsertOrReplace(10.0, 1.3);

   double value1 = GetEnvelope()->Integral(2.0, 13.0);
   double expected1 = (5.0 - 2.0) * 0.2 + (13.0 - 5.0) * 1.3;
   double value2 = GetEnvelope()->IntegralOfInverse(2.0, 13.0);
   double expected2 = (5.0 - 2.0) / 0.2 + (13.0 - 5.0) / 1.3;
   if( fabs(value1 - expected1) > 0.01 )
     {
       wxPrintf( "TimeTrack:  Integral failed! expected %f got %f\n", expected1, value1);
     }
   if( fabs(value2 - expected2) > 0.01 )
     {
       wxPrintf( "TimeTrack:  IntegralOfInverse failed! expected %f got %f\n", expected2, value2);
     }

   /*double reqt0 = 10.0 - .1;
   double reqt1 = 10.0 + .1;
   double t0 = warp( reqt0 );
   double t1 = warp( reqt1 );
   if( t0 > t1 )
     {
       wxPrintf( "TimeTrack:  Warping reverses an interval! [%.2f,%.2f] -> [%.2f,%.2f]\n",
          reqt0, reqt1,
          t0, t1 );
     }*/
}

