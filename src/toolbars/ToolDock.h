/**********************************************************************

  Audacity: A Digital Audio Editor

  ToolDock.h

  Dominic Mazzoni
  Shane T. Mueller
  Leland Lucius

**********************************************************************/

#ifndef __AUDACITY_TOOLDOCK__
#define __AUDACITY_TOOLDOCK__

#include <wx/defs.h>
#include <wx/panel.h>

#include "ToolBar.h"

class wxArrayPtrVoid;
class wxCommandEvent;
class wxEraseEvent;
class wxSizeEvent;
class wxPaintEvent;
class wxPoint;
class wxRect;
class wxWindow;

class GrabberEvent;
class ToolManager;

////////////////////////////////////////////////////////////
/// class ToolDock
////////////////////////////////////////////////////////////

//
// ToolDock IDs
//
enum
{
   NoDockID = 0,
   TopDockID,
   BotDockID,
   DockCount = 2
};

class ToolBarConfiguration : public wxArrayPtrVoid
{
public:
   using Position = int;
   static const Position UnspecifiedPosition = -1;

   struct Place {
      ToolBar *pBar {};
      Position position { UnspecifiedPosition };
   };

   class Iterator
      : public std::iterator<std::forward_iterator_tag, Place>
   {
   public:
      const Place &operator * () const { return mPlace; }
      const Place *operator -> () const { return &**this; }
      Iterator &operator ++ ()
      {
         ++mIter;
         // This is a feature:  advance position even at the end
         ++mPlace.position;
         if (mIter != mEnd)
            mPlace.pBar = static_cast<ToolBar*>(*mIter);
         else
            mPlace.pBar = nullptr;
         return *this;
      }

      friend inline bool operator ==
      (const Iterator &lhs, const Iterator &rhs)
      {
         return lhs.mIter == rhs.mIter;
      }

      friend inline bool operator !=
      (const Iterator &lhs, const Iterator &rhs)
      {
         return !(lhs == rhs);
      }

   private:
      friend ToolBarConfiguration;
      using iterator = wxArrayPtrVoid::const_iterator;
      explicit Iterator(iterator iter, iterator end)
         : mIter(iter)
         , mEnd(end)
      {
         mPlace.position = 0;
         if (mIter != mEnd)
            mPlace.pBar = static_cast<ToolBar*>(*mIter);
      }

      iterator mIter, mEnd;
      Place mPlace;
   };

   Iterator begin() const
      { return Iterator { wxArrayPtrVoid::begin(), wxArrayPtrVoid::end() }; }
   Iterator end() const
      { return Iterator { wxArrayPtrVoid::end(), wxArrayPtrVoid::end() }; }

   Position Find(const ToolBar *bar) const
   {
      return Index(const_cast<ToolBar*>(bar));
   }

   bool Contains(const ToolBar *bar) const
   {
      return Find(bar) != UnspecifiedPosition;
   }

   // Default position inserts at the end
   void Insert(ToolBar *bar,
               Position position = UnspecifiedPosition);
   void Remove(const ToolBar *bar);

   // Future: might allow a state that the configuration remembers
   // a hidden bar, but for now, it's equivalent to Contains():
   bool Shows(const ToolBar *bar) const { return Contains(bar); }

   void Show(ToolBar *bar);
   void Hide(ToolBar *bar);

private:
   Iterator FindPlace(const ToolBar *bar) const;
};

class ToolDock final : public wxPanel
{

 public:

   ToolDock( ToolManager *manager, wxWindow *parent, int dockid );
   ~ToolDock();

   bool AcceptsFocus() const override { return false; };

   void LayoutToolBars();
   void Expose( int type, bool show );
   int GetOrder( ToolBar *bar );
   void Dock( ToolBar *bar, bool deflate,
              ToolBarConfiguration::Position ndx
                 = ToolBarConfiguration::UnspecifiedPosition);
   void Undock( ToolBar *bar );
   ToolBarConfiguration::Position
      PositionBar( ToolBar *t, const wxPoint & pos, wxRect & rect );

   ToolBarConfiguration &GetConfiguration()
   { return mConfiguration; }

 protected:

   void OnErase( wxEraseEvent & event );
   void OnSize( wxSizeEvent & event );
   void OnPaint( wxPaintEvent & event );
   void OnGrabber( GrabberEvent & event );
   void OnMouseEvents(wxMouseEvent &event);

 private:

   void Updated();

   int mTotalToolBarHeight;
   wxWindow *mParent;

   ToolManager *mManager;

   // Stores adjacency relations that we want to realize in the dock layout
   ToolBarConfiguration mConfiguration;

   ToolBar *mBars[ ToolBarCount ];

 public:

   DECLARE_CLASS( ToolDock );
   DECLARE_EVENT_TABLE();
};

#endif
