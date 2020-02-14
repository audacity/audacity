/**********************************************************************

   Audacity: A Digital Audio Editor

   Plot.h

   Max Maisel

   This class is a generic plot.

**********************************************************************/

#ifndef __AUDACITY_PLOT__
#define __AUDACITY_PLOT__

#include "wxPanelWrapper.h" // to inherit

#include "MemoryX.h"

class Ruler;

struct PlotData
{
   std::unique_ptr<wxPen> pen;
   std::vector<float> xdata;
   std::vector<float> ydata;
};

class Plot : public wxPanelWrapper
{
   public:
      Plot(wxWindow *parent, wxWindowID winid,
         float x_min, float x_max, float y_min, float y_max,
         const TranslatableString& xlabel, const TranslatableString& ylabel,
         int xformat = 1, int yformat = 1, //Ruler::RealFormat
         int count = 1, const wxPoint& pos = wxDefaultPosition,
         const wxSize& size = wxDefaultSize,
         long style = wxTAB_TRAVERSAL | wxNO_BORDER);

      inline PlotData* GetPlotData(int id)
         { return &m_plots[id]; }

   private:
      void OnPaint(wxPaintEvent & evt);
      void OnSize(wxSizeEvent & evt);

      float m_xmin, m_xmax;
      float m_ymin, m_ymax;
      std::vector<PlotData> m_plots;
      std::unique_ptr<Ruler> m_xruler, m_yruler;

      int XToScreen(float x, wxRect& rect);
      int YToScreen(float y, wxRect& rect);

      DECLARE_EVENT_TABLE()
};

#endif
