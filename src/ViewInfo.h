/**********************************************************************

  Audacity: A Digital Audio Editor

  ViewInfo.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_VIEWINFO__
#define __AUDACITY_VIEWINFO__

#include "SelectedRegion.h"

const double gMaxZoom = 6000000,
             gMinZoom = 0.001;

class Track;


// The subset of ViewInfo information (other than selection)
// that is sufficient for purposes of TrackArtist,
// and for computing conversions between track times and pixel positions.
class AUDACITY_DLL_API ZoomInfo
{
public:
   ZoomInfo(double start, double duration, double pixelsPerSecond);
   ~ZoomInfo();

   int vpos;                    // vertical scroll pos

   double h;                    // h pos in secs

   double screen;               // screen width in secs
   double zoom;                 // pixels per second
};

class AUDACITY_DLL_API ViewInfo
   : public ZoomInfo
{
public:
   ViewInfo(double start, double screenDuration, double pixelsPerSecond);

   // Current selection

   SelectedRegion selectedRegion;

   // Scroll info

   Track *track;                // first visible track

   double total;                // total width in secs
   // Current horizontal scroll bar positions, in pixels
   wxInt64 sbarH;
   wxInt64 sbarScreen;
   wxInt64 sbarTotal;

   // Internal wxScrollbar positions are only int in range, so multiply
   // the above values with the following member to get the actual
   // scroll bar positions as reported by the horizontal wxScrollbar's members
   // i.e. units are scroll increments per pixel
   double sbarScale;

   // Vertical scroll step
   int scrollStep;

   // Other stuff, mainly states (true or false) related to autoscroll and
   // drawing the waveform. Maybe this should be put somewhere else?

   bool bUpdateTrackIndicator;

   void WriteXMLAttributes(XMLWriter &xmlFile);
   bool ReadXMLAttribute(const wxChar *attr, const wxChar *value);
};

#endif
