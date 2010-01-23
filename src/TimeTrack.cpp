/**********************************************************************

  Audacity: A Digital Audio Editor

  TimeTrack.cpp

  Dr William Bland

*******************************************************************//*!

\class TimeTrack
\brief A kind of Track used to 'warp time'

*//*******************************************************************/


#include <wx/intl.h>
#include "Audacity.h"
#include "AColor.h"
#include "TimeTrack.h"
#include "widgets/Ruler.h"
#include "Prefs.h"
#include "Internat.h"
#include "Resample.h"

TimeTrack *TrackFactory::NewTimeTrack()
{
   return new TimeTrack(mDirManager);
}

TimeTrack::TimeTrack(DirManager *projDirManager):
   Track(projDirManager)
{
   mHeight = 50;

   mRangeLower = 90;
   mRangeUpper = 110;

   mEnvelope = new Envelope();
   mEnvelope->SetTrackLen(1000000000.0);
   mEnvelope->SetInterpolateDB(false);
   mEnvelope->Flatten(0.5);
   mEnvelope->Mirror(false);
   SetDefaultName(_("Time Track"));
   SetName(GetDefaultName());

   mRuler = new Ruler();
   mRuler->SetLabelEdges(false);
   mRuler->SetFormat(Ruler::TimeFormat);

   blankBrush.SetColour(214, 214, 214);
   blankPen.SetColour(214, 214, 214);
}

TimeTrack::TimeTrack(TimeTrack &orig):
   Track(orig)
{
   Init(orig);

   mHeight = 50;

   mRangeLower = 90;
   mRangeUpper = 110;

   mEnvelope = new Envelope();
   mEnvelope->SetTrackLen(1000000000.0);
   mEnvelope->SetInterpolateDB(false);
   mEnvelope->Flatten(0.5);
   mEnvelope->Mirror(false);
   mEnvelope->Paste(0.0, orig.mEnvelope);
   mEnvelope->SetOffset(0);

   mRuler = new Ruler();
   mRuler->SetLabelEdges(false);
   mRuler->SetFormat(Ruler::TimeFormat);

   blankBrush.SetColour(214, 214, 214);
   blankPen.SetColour(214, 214, 214);
}

// Copy the track metadata but not the contents.
void TimeTrack::Init(const TimeTrack &orig)
{
   Track::Init(orig);
   SetDefaultName(orig.GetDefaultName());
   SetName(orig.GetName());
}

TimeTrack::~TimeTrack()
{
   delete mEnvelope;
   delete mRuler;
}

Track *TimeTrack::Duplicate()
{
   return new TimeTrack(*this);
}

// Our envelope represents the playback speed, which is the rate of change of
// playback position.  We want to find the playback position at time t, so
// we have to integrate the playback speed.
double TimeTrack::warp( double t )
{
   double result = GetEnvelope()->Integral( 0.0, t,
                                            GetRangeLower()/100.0,
                                            GetRangeUpper()/100.0 );
   //printf( "Warping %.2f to %.2f\n", t, result );
   return result;
}

bool TimeTrack::HandleXMLTag(const wxChar *tag, const wxChar **attrs)
{
   if (!wxStrcmp(tag, wxT("timetrack"))) {
      double dblValue;
      long nValue;
      while(*attrs) {
         const wxChar *attr = *attrs++;
         const wxChar *value = *attrs++;
         
         if (!value)
            break;
         
         const wxString strValue = value;
         if (!wxStrcmp(attr, wxT("offset"))) 
         {
            if (!XMLValueChecker::IsGoodString(strValue) || 
                  !Internat::CompatibleToDouble(strValue, &dblValue))
               return false;
            mOffset = dblValue;
            mEnvelope->SetOffset(mOffset);
         }
         else if (!wxStrcmp(attr, wxT("name")) && XMLValueChecker::IsGoodString(strValue))
            mName = strValue;
         else if (!wxStrcmp(attr, wxT("channel")))
         {
            if (!XMLValueChecker::IsGoodInt(strValue) || !strValue.ToLong(&nValue) || 
                  !XMLValueChecker::IsValidChannel(nValue))
               return false;
            mChannel = nValue;
         }
         else if (!wxStrcmp(attr, wxT("height")) && 
                  XMLValueChecker::IsGoodInt(strValue) && strValue.ToLong(&nValue))
            mHeight = nValue;
         else if (!wxStrcmp(attr, wxT("minimized")) && 
                  XMLValueChecker::IsGoodInt(strValue) && strValue.ToLong(&nValue))
            mMinimized = (nValue != 0);
         
      } // while
      return true;
   }

   return false;
}

XMLTagHandler *TimeTrack::HandleXMLChild(const wxChar *tag)
{
   if (!wxStrcmp(tag, wxT("envelope")))
      return mEnvelope;

  return NULL;
}

void TimeTrack::WriteXML(XMLWriter &xmlFile)
{
   xmlFile.StartTag(wxT("timetrack"));

   xmlFile.WriteAttr(wxT("name"), mName);
   xmlFile.WriteAttr(wxT("channel"), mChannel);
   xmlFile.WriteAttr(wxT("offset"), mOffset, 8);
   xmlFile.WriteAttr(wxT("height"), this->GetActualHeight());
   xmlFile.WriteAttr(wxT("minimized"), this->GetMinimized());

   mEnvelope->WriteXML(xmlFile);

   xmlFile.EndTag(wxT("timetrack"));
}

void TimeTrack::Draw(wxDC & dc, const wxRect & r, double h, double pps)
{
   double tstep = 1.0 / pps;                     // Seconds per point
   double t0 = h;
   double t1 = h + (r.width * tstep);

   //Make sure t1 (the right bound) is greater than 0
   if (t1 < 0.0)
      t1 = 0.0;

   //make sure t1 is greater than t0
   if (t0 > t1)
      t0 = t1;

   dc.SetBrush(blankBrush);
   dc.SetPen(blankPen);
   dc.DrawRectangle(r);

   //copy this rectangle away for future use.
   wxRect mid = r;

   // Draw the Ruler
   mRuler->SetBounds(r.x, r.y, r.x + r.width - 1, r.y + r.height - 1);
   double min = t0;
   double max = min + r.width / pps;
   mRuler->SetRange(min, max);
   mRuler->SetFlip(false);  // If we don't do this, the Ruler doesn't redraw itself when the envelope is modified.
                            // I have no idea why!
                            //
                            // LL:  It's because the ruler only Invalidate()s when the new value is different
                            //      than the current value.
   mRuler->SetFlip(GetHeight() > 75 ? true : true);
   mRuler->Draw(dc, GetEnvelope(), GetRangeLower(), GetRangeUpper());

   int *heights = new int[mid.width];
   double *envValues = new double[mid.width];
   GetEnvelope()->GetValues(envValues, mid.width, t0, tstep);

   double t = t0;
   int x;
   for (x = 0; x < mid.width; x++)
      {
         heights[x] = (int)(mid.height * (1 - envValues[x]));
         t += tstep;
      }

   dc.SetPen(AColor::envelopePen);

   for (x = 0; x < mid.width; x++)
      {
         int thisy = r.y + heights[x];
         AColor::Line(dc, mid.x + x, thisy, mid.x + x, thisy+3);
      }

   if (heights)
      delete[]heights;
   if (envValues)
      delete[]envValues;
}

void TimeTrack::testMe()
{
   GetEnvelope()->SetDefaultValue(0.5);
   GetEnvelope()->Flatten(0.0);
   GetEnvelope()->Insert( 0.0, 0.0 );
   GetEnvelope()->Insert( 5.0, 1.0 );
   GetEnvelope()->Insert( 10.0, 0.0 );

   double reqt0 = 10.0 - .1;
   double reqt1 = 10.0 + .1;
   double t0 = warp( reqt0 );
   double t1 = warp( reqt1 );
   if( t0 > t1 )
     {
       printf( "TimeTrack:  Warping reverses an interval! [%.2f,%.2f] -> [%.2f,%.2f]\n",
	       reqt0, reqt1,
	       t0, t1 );
     }
}

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 8622daf1-c09a-4dcd-8b71-615d194343c7

