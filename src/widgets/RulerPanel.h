/**********************************************************************

  Audacity: A Digital Audio Editor

  RulerPanel.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_RULER_PANEL__
#define __AUDACITY_RULER_PANEL__

#include "Ruler.h"

class AUDACITY_DLL_API RulerPanel final : public wxPanelWrapper {
   DECLARE_DYNAMIC_CLASS(RulerPanel)

 public:
   using Range = std::pair<double, double>;

   struct Options {
      bool log { false };
      bool flip { false };
      bool labelEdges { false };
      bool ticksAtExtremes { false };
      bool hasTickColour{ false };
      wxColour tickColour;

      Options() {}

      Options &Log( bool l )
      { log = l; return *this; }

      Options &Flip( bool f )
      { flip = f; return *this; }

      Options &LabelEdges( bool l )
      { labelEdges = l; return *this; }

      Options &TicksAtExtremes( bool t )
      { ticksAtExtremes = t; return *this; }

      Options &TickColour( const wxColour c )
      { tickColour = c; hasTickColour = true; return *this; }
   };

   RulerPanel(wxWindow* parent, wxWindowID id,
              wxOrientation orientation,
              const wxSize &bounds,
              const Range &range,
              RulerFormat format,
              const TranslatableString &units,
              const Options &options = {},
              const wxPoint& pos = wxDefaultPosition,
              const wxSize& size = wxDefaultSize);

   ~RulerPanel();

   void DoSetSize(int x, int y,
                  int width, int height,
                  int sizeFlags = wxSIZE_AUTO) override;

   void OnErase(wxEraseEvent &evt);
   void OnPaint(wxPaintEvent &evt);
   void OnSize(wxSizeEvent &evt);
   void SetTickColour( wxColour & c){ ruler.SetTickColour( c );}

   // We don't need or want to accept focus.
   bool AcceptsFocus() const override  { return false; }
   // So that wxPanel is not included in Tab traversal - see wxWidgets bug 15581
   bool AcceptsFocusFromKeyboard() const override { return false; }

 public:

   Ruler  ruler;

private:
   DECLARE_EVENT_TABLE()
};

#endif //define __AUDACITY_RULER_PANEL__
