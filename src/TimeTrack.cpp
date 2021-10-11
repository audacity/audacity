/**********************************************************************

  Audacity: A Digital Audio Editor

  TimeTrack.cpp

  Dr William Bland

*******************************************************************//*!

\class TimeTrack
\brief A kind of Track used to 'warp time'

*//*******************************************************************/


#include "TimeTrack.h"

#include "ActiveProject.h"
#include <cfloat>
#include <wx/wxcrtvararg.h>
#include <wx/dc.h>
#include <wx/intl.h>
#include "widgets/Ruler.h"
#include "Envelope.h"
#include "Project.h"
#include "ProjectRate.h"
#include "ViewInfo.h"

#include "tracks/ui/TrackView.h"
#include "tracks/ui/TrackControls.h"


//TODO-MB: are these sensible values?
#define TIMETRACK_MIN 0.01
#define TIMETRACK_MAX 10.0

static ProjectFileIORegistry::ObjectReaderEntry readerEntry{
   wxT( "timetrack" ),
   []( AudacityProject &project ){
      auto &tracks = TrackList::Get( project );
      auto &viewInfo = ViewInfo::Get( project );
      auto result = tracks.Add(std::make_shared<TimeTrack>(&viewInfo));
      TrackView::Get( *result );
      TrackControls::Get( *result );
      return result;
   }
};

TimeTrack::TimeTrack(const ZoomInfo *zoomInfo):
   Track()
   , mZoomInfo(zoomInfo)
{
   CleanState();
}

void TimeTrack::CleanState()
{
   mEnvelope = std::make_unique<BoundedEnvelope>(true, TIMETRACK_MIN, TIMETRACK_MAX, 1.0);

   SetRangeLower( 0.9 );
   SetRangeUpper( 1.1 );
   mDisplayLog = false;

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
      mEnvelope = std::make_unique<BoundedEnvelope>( *orig.mEnvelope, *pT0, *pT1 );
   }
   else
      mEnvelope = std::make_unique<BoundedEnvelope>( *orig.mEnvelope );

   SetRangeLower(orig.GetRangeLower());
   SetRangeUpper(orig.GetRangeUpper());

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
   SetDisplayLog(orig.GetDisplayLog());
}

TimeTrack::~TimeTrack()
{
}

double TimeTrack::GetRangeLower() const
{
   return mEnvelope->GetRangeLower();
}

double TimeTrack::GetRangeUpper() const
{
   return mEnvelope->GetRangeUpper();
}

void TimeTrack::SetRangeLower(double lower)
{
   mEnvelope->SetRangeLower( lower );
}

void TimeTrack::SetRangeUpper(double upper)
{
   mEnvelope->SetRangeUpper( upper );
}

bool TimeTrack::SupportsBasicEditing() const
{
   return false;
}

Track::Holder TimeTrack::PasteInto( AudacityProject &project ) const
{
   // Maintain uniqueness of the time track!
   std::shared_ptr<TimeTrack> pNewTrack;
   if( auto pTrack = *TrackList::Get( project ).Any<TimeTrack>().begin() )
      pNewTrack = pTrack->SharedPointer<TimeTrack>();
   else
      pNewTrack = std::make_shared<TimeTrack>( &ViewInfo::Get( project ) );

   // Should come here only for .aup3 import, not for paste (because the
   // track is skipped in cut/copy commands)
   // And for import we agree to replace the track contents completely
   pNewTrack->CleanState();
   pNewTrack->Init(*this);
   pNewTrack->Paste(0.0, this);
   pNewTrack->SetRangeLower(this->GetRangeLower());
   pNewTrack->SetRangeUpper(this->GetRangeUpper());
   return pNewTrack;
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

namespace {
double GetRate() {
   auto pProject = GetActiveProject().lock();
   return pProject
      ? ProjectRate::Get( *pProject ).GetRate()
      : 44100.0;
}
}

void TimeTrack::Clear(double t0, double t1)
{
   auto sampleTime = 1.0 / GetRate();
   mEnvelope->CollapseRegion( t0, t1, sampleTime );
}

void TimeTrack::Paste(double t, const Track * src)
{
   bool bOk = src && src->TypeSwitch< bool >( [&] (const TimeTrack *tt) {
      auto sampleTime = 1.0 / GetRate();
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

Track::Holder TimeTrack::Clone() const
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
         if (this->Track::HandleCommonXMLAttribute(attr, strValue))
            ;
         else if (!wxStrcmp(attr, wxT("rangelower")))
         {
            SetRangeLower( Internat::CompatibleToDouble(value) );
            mRescaleXMLValues = false;
         }
         else if (!wxStrcmp(attr, wxT("rangeupper")))
         {
            SetRangeUpper( Internat::CompatibleToDouble(value) );
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
      mEnvelope->RescaleValues(GetRangeLower(), GetRangeUpper());
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
   this->Track::WriteCommonXMLAttributes( xmlFile );

   //xmlFile.WriteAttr(wxT("channel"), mChannel);
   //xmlFile.WriteAttr(wxT("offset"), mOffset, 8);
   xmlFile.WriteAttr(wxT("rangelower"), GetRangeLower(), 12);
   xmlFile.WriteAttr(wxT("rangeupper"), GetRangeUpper(), 12);
   xmlFile.WriteAttr(wxT("displaylog"), GetDisplayLog());
   xmlFile.WriteAttr(wxT("interpolatelog"), GetInterpolateLog());

   mEnvelope->WriteXML(xmlFile);

   xmlFile.EndTag(wxT("timetrack"));
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

