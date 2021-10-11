/**********************************************************************

  Audacity: A Digital Audio Editor

  LabelTrack.cpp

  Dominic Mazzoni
  James Crook
  Jun Wan

*******************************************************************//**

\class LabelTrack
\brief A LabelTrack is a Track that holds labels (LabelStruct).

These are used to annotate a waveform.
Each label has a start time and an end time.
The text of the labels is editable and the
positions of the end points are draggable.

*//****************************************************************//**

\class LabelStruct
\brief A LabelStruct holds information for ONE label in a LabelTrack

LabelStruct also has label specific functions, mostly functions
for drawing different aspects of the label and its text box.

*//*******************************************************************/


#include "LabelTrack.h"

#include "tracks/ui/TrackView.h"
#include "tracks/ui/TrackControls.h"



#include <stdio.h>
#include <algorithm>
#include <limits.h>
#include <float.h>

#include <wx/log.h>
#include <wx/tokenzr.h>

#include "Prefs.h"
#include "Project.h"
#include "prefs/ImportExportPrefs.h"

#include "effects/TimeWarper.h"
#include "widgets/AudacityMessageBox.h"

wxDEFINE_EVENT(EVT_LABELTRACK_ADDITION, LabelTrackEvent);
wxDEFINE_EVENT(EVT_LABELTRACK_DELETION, LabelTrackEvent);
wxDEFINE_EVENT(EVT_LABELTRACK_PERMUTED, LabelTrackEvent);
wxDEFINE_EVENT(EVT_LABELTRACK_SELECTION, LabelTrackEvent);

static ProjectFileIORegistry::ObjectReaderEntry readerEntry{
   wxT( "labeltrack" ),
   []( AudacityProject &project ){
      auto &tracks = TrackList::Get( project );
      auto result = tracks.Add(std::make_shared<LabelTrack>());
      TrackView::Get( *result );
      TrackControls::Get( *result );
      return result;
   }
};

LabelTrack::LabelTrack():
   Track(),
   mClipLen(0.0),
   miLastLabel(-1)
{
   SetDefaultName(_("Label Track"));
   SetName(GetDefaultName());
}

LabelTrack::LabelTrack(const LabelTrack &orig) :
   Track(orig),
   mClipLen(0.0)
{
   for (auto &original: orig.mLabels) {
      LabelStruct l { original.selectedRegion, original.title };
      mLabels.push_back(l);
   }
}

Track::Holder LabelTrack::PasteInto( AudacityProject & ) const
{
   auto pNewTrack = std::make_shared<LabelTrack>();
   pNewTrack->Paste(0.0, this);
   return pNewTrack;
}

template<typename IntervalType>
static IntervalType DoMakeInterval(const LabelStruct &label, size_t index)
{
   return {
      label.getT0(), label.getT1(),
      std::make_unique<LabelTrack::IntervalData>( index ) };
}

auto LabelTrack::MakeInterval( size_t index ) const -> ConstInterval
{
   return DoMakeInterval<ConstInterval>(mLabels[index], index);
}

auto LabelTrack::MakeInterval( size_t index ) -> Interval
{
   return DoMakeInterval<Interval>(mLabels[index], index);
}

template< typename Container, typename LabelTrack >
static Container DoMakeIntervals(LabelTrack &track)
{
   Container result;
   for (size_t ii = 0, nn = track.GetNumLabels(); ii < nn; ++ii)
      result.emplace_back( track.MakeInterval( ii ) );
   return result;
}

auto LabelTrack::GetIntervals() const -> ConstIntervals
{
   return DoMakeIntervals<ConstIntervals>(*this);
}

auto LabelTrack::GetIntervals() -> Intervals
{
   return DoMakeIntervals<Intervals>(*this);
}

void LabelTrack::SetLabel( size_t iLabel, const LabelStruct &newLabel )
{
   if( iLabel >= mLabels.size() ) {
      wxASSERT( false );
      mLabels.resize( iLabel + 1 );
   }
   mLabels[ iLabel ] = newLabel;
}

LabelTrack::~LabelTrack()
{
}

void LabelTrack::SetOffset(double dOffset)
{
   for (auto &labelStruct: mLabels)
      labelStruct.selectedRegion.move(dOffset);
}

void LabelTrack::Clear(double b, double e)
{
   // May DELETE labels, so use subscripts to iterate
   for (size_t i = 0; i < mLabels.size(); ++i) {
      auto &labelStruct = mLabels[i];
      LabelStruct::TimeRelations relation =
                        labelStruct.RegionRelation(b, e, this);
      if (relation == LabelStruct::BEFORE_LABEL)
         labelStruct.selectedRegion.move(- (e-b));
      else if (relation == LabelStruct::SURROUNDS_LABEL) {
         DeleteLabel( i );
         --i;
      }
      else if (relation == LabelStruct::ENDS_IN_LABEL)
         labelStruct.selectedRegion.setTimes(
            b,
            labelStruct.getT1() - (e - b));
      else if (relation == LabelStruct::BEGINS_IN_LABEL)
         labelStruct.selectedRegion.setT1(b);
      else if (relation == LabelStruct::WITHIN_LABEL)
         labelStruct.selectedRegion.moveT1( - (e-b));
   }
}

#if 0
//used when we want to use clear only on the labels
bool LabelTrack::SplitDelete(double b, double e)
{
   // May DELETE labels, so use subscripts to iterate
   for (size_t i = 0, len = mLabels.size(); i < len; ++i) {
      auto &labelStruct = mLabels[i];
      LabelStruct::TimeRelations relation =
                        labelStruct.RegionRelation(b, e, this);
      if (relation == LabelStruct::SURROUNDS_LABEL) {
         DeleteLabel(i);
         --i;
      }
      else if (relation == LabelStruct::WITHIN_LABEL)
         labelStruct.selectedRegion.moveT1( - (e-b));
      else if (relation == LabelStruct::ENDS_IN_LABEL)
         labelStruct.selectedRegion.setT0(e);
      else if (relation == LabelStruct::BEGINS_IN_LABEL)
         labelStruct.selectedRegion.setT1(b);
   }

   return true;
}
#endif

void LabelTrack::ShiftLabelsOnInsert(double length, double pt)
{
   for (auto &labelStruct: mLabels) {
      LabelStruct::TimeRelations relation =
                        labelStruct.RegionRelation(pt, pt, this);

      if (relation == LabelStruct::BEFORE_LABEL)
         labelStruct.selectedRegion.move(length);
      else if (relation == LabelStruct::WITHIN_LABEL)
         labelStruct.selectedRegion.moveT1(length);
   }
}

void LabelTrack::ChangeLabelsOnReverse(double b, double e)
{
   for (auto &labelStruct: mLabels) {
      if (labelStruct.RegionRelation(b, e, this) ==
                                    LabelStruct::SURROUNDS_LABEL)
      {
         double aux     = b + (e - labelStruct.getT1());
         labelStruct.selectedRegion.setTimes(
            aux,
            e - (labelStruct.getT0() - b));
      }
   }
   SortLabels();
}

void LabelTrack::ScaleLabels(double b, double e, double change)
{
   for (auto &labelStruct: mLabels) {
      labelStruct.selectedRegion.setTimes(
         AdjustTimeStampOnScale(labelStruct.getT0(), b, e, change),
         AdjustTimeStampOnScale(labelStruct.getT1(), b, e, change));
   }
}

double LabelTrack::AdjustTimeStampOnScale(double t, double b, double e, double change)
{
//t is the time stamp we'll be changing
//b and e are the selection start and end

   if (t < b){
      return t;
   }else if (t > e){
      double shift = (e-b)*change - (e-b);
      return t + shift;
   }else{
      double shift = (t-b)*change - (t-b);
      return t + shift;
   }
}

// Move the labels in the track according to the given TimeWarper.
// (If necessary this could be optimised by ignoring labels that occur before a
// specified time, as in most cases they don't need to move.)
void LabelTrack::WarpLabels(const TimeWarper &warper) {
   for (auto &labelStruct: mLabels) {
      labelStruct.selectedRegion.setTimes(
         warper.Warp(labelStruct.getT0()),
         warper.Warp(labelStruct.getT1()));
   }

   // This should not be needed, assuming the warper is nondecreasing, but
   // let's not assume too much.
   SortLabels();
}

LabelStruct::LabelStruct(const SelectedRegion &region,
                         const wxString& aTitle)
: selectedRegion(region)
, title(aTitle)
{
   updated = false;
   width = 0;
   x = 0;
   x1 = 0;
   xText = 0;
   y = 0;
}

LabelStruct::LabelStruct(const SelectedRegion &region,
                         double t0, double t1,
                         const wxString& aTitle)
: selectedRegion(region)
, title(aTitle)
{
   // Overwrite the times
   selectedRegion.setTimes(t0, t1);

   updated = false;
   width = 0;
   x = 0;
   x1 = 0;
   xText = 0;
   y = 0;
}

void LabelTrack::SetSelected( bool s )
{
   bool selected = GetSelected();
   Track::SetSelected( s );
   if ( selected != GetSelected() ) {
      LabelTrackEvent evt{
         EVT_LABELTRACK_SELECTION, SharedPointer<LabelTrack>(), {}, -1, -1
      };
      ProcessEvent( evt );
   }
}

double LabelTrack::GetOffset() const
{
   return mOffset;
}

double LabelTrack::GetStartTime() const
{
   if (mLabels.empty())
      return 0.0;
   else
      return mLabels[0].getT0();
}

double LabelTrack::GetEndTime() const
{
   //we need to scan through all the labels, because the last
   //label might not have the right-most end (if there is overlap).
   if (mLabels.empty())
      return 0.0;

   double end = 0.0;
   for (auto &labelStruct: mLabels) {
      const double t1 = labelStruct.getT1();
      if(t1 > end)
         end = t1;
   }
   return end;
}

Track::Holder LabelTrack::Clone() const
{
   return std::make_shared<LabelTrack>( *this );
}

// Adjust label's left or right boundary, depending which is requested.
// Return true iff the label flipped.
bool LabelStruct::AdjustEdge( int iEdge, double fNewTime)
{
   updated = true;
   if( iEdge < 0 )
      return selectedRegion.setT0(fNewTime);
   else
      return selectedRegion.setT1(fNewTime);
}

// We're moving the label.  Adjust both left and right edge.
void LabelStruct::MoveLabel( int iEdge, double fNewTime)
{
   double fTimeSpan = getDuration();

   if( iEdge < 0 )
   {
      selectedRegion.setTimes(fNewTime, fNewTime+fTimeSpan);
   }
   else
   {
      selectedRegion.setTimes(fNewTime-fTimeSpan, fNewTime);
   }
   updated = true;
}

LabelStruct LabelStruct::Import(wxTextFile &file, int &index)
{
   SelectedRegion sr;
   wxString title;
   static const wxString continuation{ wxT("\\") };

   wxString firstLine = file.GetLine(index++);

   {
      // Assume tab is an impossible character within the exported text
      // of the label, so can be only a delimiter.  But other white space may
      // be part of the label text.
      wxStringTokenizer toker { firstLine, wxT("\t") };

      //get the timepoint of the left edge of the label.
      auto token = toker.GetNextToken();

      double t0;
      if (!Internat::CompatibleToDouble(token, &t0))
         throw BadFormatException{};

      token = toker.GetNextToken();

      double t1;
      if (!Internat::CompatibleToDouble(token, &t1))
         //s1 is not a number.
         t1 = t0;  //This is a one-sided label; t1 == t0.
      else
         token = toker.GetNextToken();

      sr.setTimes( t0, t1 );
      
      title = token;
   }

   // Newer selection fields are written on additional lines beginning with
   // '\' which is an impossible numerical character that older versions of
   // audacity will ignore.  Test for the presence of such a line and then
   // parse it if we can.

   // There may also be additional continuation lines from future formats that
   // we ignore.

   // Advance index over all continuation lines first, before we might throw
   // any exceptions.
   int index2 = index;
   while (index < (int)file.GetLineCount() &&
          file.GetLine(index).StartsWith(continuation))
      ++index;

   if (index2 < index) {
      wxStringTokenizer toker { file.GetLine(index2++), wxT("\t") };
      auto token = toker.GetNextToken();
      if (token != continuation)
         throw BadFormatException{};

      token = toker.GetNextToken();
      double f0;
      if (!Internat::CompatibleToDouble(token, &f0))
         throw BadFormatException{};

      token = toker.GetNextToken();
      double f1;
      if (!Internat::CompatibleToDouble(token, &f1))
         throw BadFormatException{};

      sr.setFrequencies(f0, f1);
   }

   return LabelStruct{ sr, title };
}

void LabelStruct::Export(wxTextFile &file) const
{
   file.AddLine(wxString::Format(wxT("%s\t%s\t%s"),
      Internat::ToString(getT0(), FLT_DIG),
      Internat::ToString(getT1(), FLT_DIG),
      title
   ));

   // Do we need more lines?
   auto f0 = selectedRegion.f0();
   auto f1 = selectedRegion.f1();
   if ((f0 == SelectedRegion::UndefinedFrequency &&
      f1 == SelectedRegion::UndefinedFrequency) ||
      ImportExportPrefs::LabelStyleSetting.ReadEnum())
      return;

   // Write a \ character at the start of a second line,
   // so that earlier versions of Audacity ignore it.
   file.AddLine(wxString::Format(wxT("\\\t%s\t%s"),
      Internat::ToString(f0, FLT_DIG),
      Internat::ToString(f1, FLT_DIG)
   ));

   // Additional lines in future formats should also start with '\'.
}

auto LabelStruct::RegionRelation(
      double reg_t0, double reg_t1, const LabelTrack * WXUNUSED(parent)) const
-> TimeRelations
{
   bool retainLabels = false;

   wxASSERT(reg_t0 <= reg_t1);
   gPrefs->Read(wxT("/GUI/RetainLabels"), &retainLabels);

   if(retainLabels) {

      // Desired behavior for edge cases: The length of the selection is smaller
      // than the length of the label if the selection is within the label or
      // matching exactly a (region) label.

      if (reg_t0 < getT0() && reg_t1 > getT1())
         return SURROUNDS_LABEL;
      else if (reg_t1 < getT0())
         return BEFORE_LABEL;
      else if (reg_t0 > getT1())
         return AFTER_LABEL;

      else if (reg_t0 >= getT0() && reg_t0 <= getT1() &&
               reg_t1 >= getT0() && reg_t1 <= getT1())
         return WITHIN_LABEL;

      else if (reg_t0 >= getT0() && reg_t0 <= getT1())
         return BEGINS_IN_LABEL;
      else
         return ENDS_IN_LABEL;

   } else {

      // AWD: Desired behavior for edge cases: point labels bordered by the
      // selection are included within it. Region labels are included in the
      // selection to the extent that the selection covers them; specifically,
      // they're not included at all if the selection borders them, and they're
      // fully included if the selection covers them fully, even if it just
      // borders their endpoints. This is just one of many possible schemes.

      // The first test catches bordered point-labels and selected-through
      // region-labels; move it to third and selection edges become inclusive
      // WRT point-labels.
      if (reg_t0 <= getT0() && reg_t1 >= getT1())
         return SURROUNDS_LABEL;
      else if (reg_t1 <= getT0())
         return BEFORE_LABEL;
      else if (reg_t0 >= getT1())
         return AFTER_LABEL;

      // At this point, all point labels should have returned.

      else if (reg_t0 > getT0() && reg_t0 < getT1() &&
               reg_t1 > getT0() && reg_t1 < getT1())
         return WITHIN_LABEL;

      // Knowing that none of the other relations match simplifies remaining
      // tests
      else if (reg_t0 > getT0() && reg_t0 < getT1())
         return BEGINS_IN_LABEL;
      else
         return ENDS_IN_LABEL;

   }
}

/// Export labels including label start and end-times.
void LabelTrack::Export(wxTextFile & f) const
{
   // PRL: to do: export other selection fields
   for (auto &labelStruct: mLabels)
      labelStruct.Export(f);
}

/// Import labels, handling files with or without end-times.
void LabelTrack::Import(wxTextFile & in)
{
   int lines = in.GetLineCount();

   mLabels.clear();
   mLabels.reserve(lines);

   //Currently, we expect a tag file to have two values and a label
   //on each line. If the second token is not a number, we treat
   //it as a single-value label.
   bool error = false;
   for (int index = 0; index < lines;) {
      try {
         // Let LabelStruct::Import advance index
         LabelStruct l { LabelStruct::Import(in, index) };
         mLabels.push_back(l);
      }
      catch(const LabelStruct::BadFormatException&) { error = true; }
   }
   if (error)
      ::AudacityMessageBox( XO("One or more saved labels could not be read.") );
   SortLabels();
}

bool LabelTrack::HandleXMLTag(const wxChar *tag, const wxChar **attrs)
{
   if (!wxStrcmp(tag, wxT("label"))) {

      SelectedRegion selectedRegion;
      wxString title;

      // loop through attrs, which is a null-terminated list of
      // attribute-value pairs
      while(*attrs) {
         const wxChar *attr = *attrs++;
         const wxChar *value = *attrs++;

         if (!value)
            break;

         const wxString strValue = value;
         // Bug 1905 was about long label strings.
         if (!XMLValueChecker::IsGoodLongString(strValue))
         {
            return false;
         }

         if (selectedRegion.HandleXMLAttribute(attr, value, wxT("t"), wxT("t1")))
            ;
         else if (!wxStrcmp(attr, wxT("title")))
            title = strValue;

      } // while

      // Handle files created by Audacity 1.1.   Labels in Audacity 1.1
      // did not have separate start- and end-times.
      // PRL: this superfluous now, given class SelectedRegion's internal
      // consistency guarantees
      //if (selectedRegion.t1() < 0)
      //   selectedRegion.collapseToT0();

      LabelStruct l { selectedRegion, title };
      mLabels.push_back(l);

      return true;
   }
   else if (!wxStrcmp(tag, wxT("labeltrack"))) {
      long nValue = -1;
      while (*attrs) {
         const wxChar *attr = *attrs++;
         const wxChar *value = *attrs++;

         if (!value)
            return true;

         const wxString strValue = value;
         if (this->Track::HandleCommonXMLAttribute(attr, strValue))
            ;
         else if (!wxStrcmp(attr, wxT("numlabels")) &&
                     XMLValueChecker::IsGoodInt(strValue) && strValue.ToLong(&nValue))
         {
            if (nValue < 0)
            {
               wxLogWarning(wxT("Project shows negative number of labels: %d"), nValue);
               return false;
            }
            mLabels.clear();
            mLabels.reserve(nValue);
         }
      }

      return true;
   }

   return false;
}

XMLTagHandler *LabelTrack::HandleXMLChild(const wxChar *tag)
{
   if (!wxStrcmp(tag, wxT("label")))
      return this;
   else
      return NULL;
}

void LabelTrack::WriteXML(XMLWriter &xmlFile) const
// may throw
{
   int len = mLabels.size();

   xmlFile.StartTag(wxT("labeltrack"));
   this->Track::WriteCommonXMLAttributes( xmlFile );
   xmlFile.WriteAttr(wxT("numlabels"), len);

   for (auto &labelStruct: mLabels) {
      xmlFile.StartTag(wxT("label"));
      labelStruct.getSelectedRegion()
         .WriteXMLAttributes(xmlFile, wxT("t"), wxT("t1"));
      // PRL: to do: write other selection fields
      xmlFile.WriteAttr(wxT("title"), labelStruct.title);
      xmlFile.EndTag(wxT("label"));
   }

   xmlFile.EndTag(wxT("labeltrack"));
}

Track::Holder LabelTrack::Cut(double t0, double t1)
{
   auto tmp = Copy(t0, t1);

   Clear(t0, t1);

   return tmp;
}

#if 0
Track::Holder LabelTrack::SplitCut(double t0, double t1)
{
   // SplitCut() == Copy() + SplitDelete()

   Track::Holder tmp = Copy(t0, t1);

   if (!SplitDelete(t0, t1))
      return {};

   return tmp;
}
#endif

Track::Holder LabelTrack::Copy(double t0, double t1, bool) const
{
   auto tmp = std::make_shared<LabelTrack>();
   const auto lt = static_cast<LabelTrack*>(tmp.get());

   for (auto &labelStruct: mLabels) {
      LabelStruct::TimeRelations relation =
                        labelStruct.RegionRelation(t0, t1, this);
      if (relation == LabelStruct::SURROUNDS_LABEL) {
         LabelStruct l {
            labelStruct.selectedRegion,
            labelStruct.getT0() - t0,
            labelStruct.getT1() - t0,
            labelStruct.title
         };
         lt->mLabels.push_back(l);
      }
      else if (relation == LabelStruct::WITHIN_LABEL) {
         LabelStruct l {
            labelStruct.selectedRegion,
            0,
            t1-t0,
            labelStruct.title
         };
         lt->mLabels.push_back(l);
      }
      else if (relation == LabelStruct::BEGINS_IN_LABEL) {
         LabelStruct l {
            labelStruct.selectedRegion,
            0,
            labelStruct.getT1() - t0,
            labelStruct.title
         };
         lt->mLabels.push_back(l);
      }
      else if (relation == LabelStruct::ENDS_IN_LABEL) {
         LabelStruct l {
            labelStruct.selectedRegion,
            labelStruct.getT0() - t0,
            t1 - t0,
            labelStruct.title
         };
         lt->mLabels.push_back(l);
      }
   }
   lt->mClipLen = (t1 - t0);

   return tmp;
}


bool LabelTrack::PasteOver(double t, const Track * src)
{
   auto result = src->TypeSwitch< bool >( [&](const LabelTrack *sl) {
      int len = mLabels.size();
      int pos = 0;

      while (pos < len && mLabels[pos].getT0() < t)
         pos++;

      for (auto &labelStruct: sl->mLabels) {
         LabelStruct l {
            labelStruct.selectedRegion,
            labelStruct.getT0() + t,
            labelStruct.getT1() + t,
            labelStruct.title
         };
         mLabels.insert(mLabels.begin() + pos++, l);
      }

      return true;
   } );

   if (! result )
      // THROW_INCONSISTENCY_EXCEPTION; // ?
      (void)0;// intentionally do nothing

   return result;
}

void LabelTrack::Paste(double t, const Track *src)
{
   bool bOk = src->TypeSwitch< bool >( [&](const LabelTrack *lt) {
      double shiftAmt = lt->mClipLen > 0.0 ? lt->mClipLen : lt->GetEndTime();

      ShiftLabelsOnInsert(shiftAmt, t);
      PasteOver(t, src);

      return true;
   } );

   if ( !bOk )
      // THROW_INCONSISTENCY_EXCEPTION; // ?
      (void)0;// intentionally do nothing
}

// This repeats the labels in a time interval a specified number of times.
bool LabelTrack::Repeat(double t0, double t1, int n)
{
   // Sanity-check the arguments
   if (n < 0 || t1 < t0)
      return false;

   double tLen = t1 - t0;

   // Insert space for the repetitions
   ShiftLabelsOnInsert(tLen * n, t1);

   // mLabels may resize as we iterate, so use subscripting
   for (unsigned int i = 0; i < mLabels.size(); ++i)
   {
      LabelStruct::TimeRelations relation =
                        mLabels[i].RegionRelation(t0, t1, this);
      if (relation == LabelStruct::SURROUNDS_LABEL)
      {
         // Label is completely inside the selection; duplicate it in each
         // repeat interval
         unsigned int pos = i; // running label insertion position in mLabels

         for (int j = 1; j <= n; j++)
         {
            const LabelStruct &label = mLabels[i];
            LabelStruct l {
               label.selectedRegion,
               label.getT0() + j * tLen,
               label.getT1() + j * tLen,
               label.title
            };

            // Figure out where to insert
            while (pos < mLabels.size() &&
                   mLabels[pos].getT0() < l.getT0())
               pos++;
            mLabels.insert(mLabels.begin() + pos, l);
         }
      }
      else if (relation == LabelStruct::BEGINS_IN_LABEL)
      {
         // Label ends inside the selection; ShiftLabelsOnInsert() hasn't touched
         // it, and we need to extend it through to the last repeat interval
         mLabels[i].selectedRegion.moveT1(n * tLen);
      }

      // Other cases have already been handled by ShiftLabelsOnInsert()
   }

   return true;
}

void LabelTrack::SyncLockAdjust(double oldT1, double newT1)
{
   if (newT1 > oldT1) {
      // Insert space within the track

      if (oldT1 > GetEndTime())
         return;

      //Clear(oldT1, newT1);
      ShiftLabelsOnInsert(newT1 - oldT1, oldT1);
   }
   else if (newT1 < oldT1) {
      // Remove from the track
      Clear(newT1, oldT1);
   }
}


void LabelTrack::Silence(double t0, double t1)
{
   int len = mLabels.size();

   // mLabels may resize as we iterate, so use subscripting
   for (int i = 0; i < len; ++i) {
      LabelStruct::TimeRelations relation =
                        mLabels[i].RegionRelation(t0, t1, this);
      if (relation == LabelStruct::WITHIN_LABEL)
      {
         // Split label around the selection
         const LabelStruct &label = mLabels[i];
         LabelStruct l {
            label.selectedRegion,
            t1,
            label.getT1(),
            label.title
         };

         mLabels[i].selectedRegion.setT1(t0);

         // This might not be the right place to insert, but we sort at the end
         ++i;
         mLabels.insert(mLabels.begin() + i, l);
      }
      else if (relation == LabelStruct::ENDS_IN_LABEL)
      {
         // Beginning of label to selection end
         mLabels[i].selectedRegion.setT0(t1);
      }
      else if (relation == LabelStruct::BEGINS_IN_LABEL)
      {
         // End of label to selection beginning
         mLabels[i].selectedRegion.setT1(t0);
      }
      else if (relation == LabelStruct::SURROUNDS_LABEL)
      {
         DeleteLabel( i );
         len--;
         i--;
      }
   }

   SortLabels();
}

void LabelTrack::InsertSilence(double t, double len)
{
   for (auto &labelStruct: mLabels) {
      double t0 = labelStruct.getT0();
      double t1 = labelStruct.getT1();
      if (t0 >= t)
         t0 += len;

      if (t1 >= t)
         t1 += len;
      labelStruct.selectedRegion.setTimes(t0, t1);
   }
}

int LabelTrack::GetNumLabels() const
{
   return mLabels.size();
}

const LabelStruct *LabelTrack::GetLabel(int index) const
{
   return &mLabels[index];
}

int LabelTrack::AddLabel(const SelectedRegion &selectedRegion,
                         const wxString &title)
{
   LabelStruct l { selectedRegion, title };

   int len = mLabels.size();
   int pos = 0;

   while (pos < len && mLabels[pos].getT0() < selectedRegion.t0())
      pos++;

   mLabels.insert(mLabels.begin() + pos, l);

   LabelTrackEvent evt{
      EVT_LABELTRACK_ADDITION, SharedPointer<LabelTrack>(), title, -1, pos
   };
   ProcessEvent( evt );

   return pos;
}

void LabelTrack::DeleteLabel(int index)
{
   wxASSERT((index < (int)mLabels.size()));
   auto iter = mLabels.begin() + index;
   const auto title = iter->title;
   mLabels.erase(iter);

   LabelTrackEvent evt{
      EVT_LABELTRACK_DELETION, SharedPointer<LabelTrack>(), title, index, -1
   };
   ProcessEvent( evt );
}

/// Sorts the labels in order of their starting times.
/// This function is called often (whilst dragging a label)
/// We expect them to be very nearly in order, so insertion
/// sort (with a linear search) is a reasonable choice.
void LabelTrack::SortLabels()
{
   const auto begin = mLabels.begin();
   const auto nn = (int)mLabels.size();
   int i = 1;
   while (true)
   {
      // Find the next disorder
      while (i < nn && mLabels[i - 1].getT0() <= mLabels[i].getT0())
         ++i;
      if (i >= nn)
         break;

      // Where must element i sink to?  At most i - 1, maybe less
      int j = i - 2;
      while( (j >= 0) && (mLabels[j].getT0() > mLabels[i].getT0()) )
         --j;
      ++j;

      // Now fix the disorder
      std::rotate(
         begin + j,
         begin + i,
         begin + i + 1
      );

      // Let listeners update their stored indices
      LabelTrackEvent evt{
         EVT_LABELTRACK_PERMUTED, SharedPointer<LabelTrack>(),
         mLabels[j].title, i, j
      };
      ProcessEvent( evt );
   }
}

wxString LabelTrack::GetTextOfLabels(double t0, double t1) const
{
   bool firstLabel = true;
   wxString retVal;

   for (auto &labelStruct: mLabels) {
      if (labelStruct.getT0() >= t0 &&
          labelStruct.getT1() <= t1)
      {
         if (!firstLabel)
            retVal += '\t';
         firstLabel = false;
         retVal += labelStruct.title;
      }
   }

   return retVal;
}

int LabelTrack::FindNextLabel(const SelectedRegion& currentRegion)
{
   int i = -1;

   if (!mLabels.empty()) {
      int len = (int) mLabels.size();
      if (miLastLabel >= 0 && miLastLabel + 1 < len
         && currentRegion.t0() == mLabels[miLastLabel].getT0()
         && currentRegion.t0() == mLabels[miLastLabel + 1].getT0() ) {
         i = miLastLabel + 1;
      }
      else {
         i = 0;
         if (currentRegion.t0() < mLabels[len - 1].getT0()) {
            while (i < len &&
                  mLabels[i].getT0() <= currentRegion.t0()) {
               i++;
            }
         }
      }
   }

   miLastLabel = i;
   return i;
}

 int LabelTrack::FindPrevLabel(const SelectedRegion& currentRegion)
{
   int i = -1;

   if (!mLabels.empty()) {
      int len = (int) mLabels.size();
      if (miLastLabel > 0 && miLastLabel < len
         && currentRegion.t0() == mLabels[miLastLabel].getT0()
         && currentRegion.t0() == mLabels[miLastLabel - 1].getT0() ) {
         i = miLastLabel - 1;
      }
      else {
         i = len - 1;
         if (currentRegion.t0() > mLabels[0].getT0()) {
            while (i >=0  &&
                  mLabels[i].getT0() >= currentRegion.t0()) {
               i--;
            }
         }
      }
   }

   miLastLabel = i;
   return i;
}
