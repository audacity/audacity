/**********************************************************************

  Audacity: A Digital Audio Editor

  ToolDock.h

  Dominic Mazzoni
  Shane T. Mueller
  Leland Lucius

**********************************************************************/

#ifndef __AUDACITY_TOOLDOCK__
#define __AUDACITY_TOOLDOCK__

#include <vector>
#include "../MemoryX.h" // for std::move
#include <wx/defs.h>

#include "ToolBar.h"

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

class ToolBarConfiguration
{
   struct Tree;
   using Forest = std::vector<Tree>;

public:

   void Swap(ToolBarConfiguration &that)
   {
      mForest.swap(that.mForest);
   }

   void Clear()
   {
      mForest.clear();
   }

   struct Position {
      ToolBar *rightOf {};
      ToolBar *below {};
      bool adopt {true};
      bool valid {true};

      // Default constructor
      Position() {}

      Position(
         ToolBar *r,
         ToolBar *b = nullptr,
         bool shouldAdopt = true
      )
         : rightOf{ r }, below{ b }, adopt{ shouldAdopt }
      {}

      // Constructor for the invalid value
      explicit Position(bool /* dummy */) : valid{ false } {}

      friend inline bool operator ==
      (const Position &lhs, const Position &rhs)
      { return lhs.valid == rhs.valid &&
         (!lhs.valid ||
          (lhs.rightOf == rhs.rightOf
           && lhs.below == rhs.below
           && lhs.adopt == rhs.adopt
         ));
      }

      friend inline bool operator !=
      (const Position &lhs, const Position &rhs)
      { return !(lhs == rhs); }
   };

   static const Position UnspecifiedPosition;

   struct Place {
      Tree *pTree {};
      Position position;
   };

   // This iterator visits the nodes of the forest in pre-order, and at each
   // stop, makes the parent, previous sibling, and children accessible.
   class Iterator
      : public std::iterator<std::forward_iterator_tag, Place>
   {
   public:
      const Place &operator * () const { return mPlace; }
      const Place *operator -> () const { return &**this; }
      Iterator &operator ++ ()
      {
         // This is a feature:  advance position even at the end
         mPlace.position =
            { mPlace.pTree ? mPlace.pTree->pBar : nullptr };

         if (!mIters.empty())
         {
            auto triple = &mIters.back();
            auto &children = triple->current->children;
            if (children.empty()) {
               while (++triple->current == triple->end) {
                  mIters.pop_back();
                  if (mIters.empty())
                     break;
                  triple = &mIters.back();
               }
            }
            else {
               auto b = children.begin();
               mIters.push_back( Triple { b, b, children.end() } );
            }
         }

         if (mIters.empty()) {
            mPlace.pTree = nullptr;
            // Leave mPlace.position as above
         }
         else {
            const auto &triple = mIters.back();
            mPlace.pTree = &*triple.current;

            if (mIters.size() == 1)
               mPlace.position.rightOf = nullptr;
            else
               mPlace.position.rightOf = (mIters.rbegin() + 1)->current->pBar;

            if (triple.begin == triple.current)
               mPlace.position.below = nullptr;
            else
               mPlace.position.below = (triple.current - 1)->pBar;
         }

         return *this;
      }

      // This may be called on the end iterator, and then returns empty
      std::vector<int> GetPath() const
      {
         std::vector<int> path;
         path.reserve(mIters.size());
         for (const auto &triple : mIters)
            path.push_back(triple.current - triple.begin);
         return path;
      }

      friend inline bool operator ==
      (const Iterator &lhs, const Iterator &rhs)
      {
         const auto &li = lhs.mIters;
         const auto &ri = rhs.mIters;
         return li.size() == ri.size() &&
            std::equal(li.begin(), li.end(), ri.begin());
      }

      friend inline bool operator !=
      (const Iterator &lhs, const Iterator &rhs)
      {
         return !(lhs == rhs);
      }

   private:
      friend ToolBarConfiguration;
      Iterator () {}
      explicit Iterator(ToolBarConfiguration &conf)
      {
         auto &forest = conf.mForest;
         if (!forest.empty()) {
            auto b = forest.begin();
            mIters.push_back( Triple { b, b, forest.end() } );
            mPlace.pTree = &*b;
         }
      }

      Place mPlace;

      using FIter = Forest::iterator;
      struct Triple
      {
         Triple (FIter b, FIter c, FIter e)
            : begin{b}, current{c}, end{e} {}
         FIter begin, current, end;

         friend inline bool operator ==
         (const Triple &lhs, const Triple &rhs)
         {
            // Really need only to compare current
            return
               // lhs.begin == rhs.begin &&
               lhs.current == rhs.current
               // lhs.end == rhs.end
            ;
         }
      };
      std::vector<Triple> mIters;
   };

   Iterator begin() { return Iterator { *this }; }
   Iterator end() const { return Iterator {}; }

   Position Find(const ToolBar *bar) const;

   bool Contains(const ToolBar *bar) const
   {
      return Find(bar) != UnspecifiedPosition;
   }

   // Default position inserts at the end
   void Insert(ToolBar *bar,
               Position position = UnspecifiedPosition);
   void InsertAtPath(ToolBar *bar, const std::vector<int> &path);
   void Remove(const ToolBar *bar);

   // Future: might allow a state that the configuration remembers
   // a hidden bar, but for now, it's equivalent to Contains():
   bool Shows(const ToolBar *bar) const { return Contains(bar); }

   void Show(ToolBar *bar);
   void Hide(ToolBar *bar);

   bool IsRightmost(const ToolBar *bar) const;

   struct Legacy {
      std::vector<ToolBar*> bars;
   };

   static bool Read
      (ToolBarConfiguration *pConfiguration,
       Legacy *pLegacy,
       ToolBar *bar, bool &visible, bool defaultVisible);
   void PostRead(Legacy &legacy);

   static void Write
      (const ToolBarConfiguration *pConfiguration, const ToolBar *bar);

private:

   void Remove(Forest &forest, Forest::iterator iter);
   void RemoveNulls(Forest &forest);

   struct Tree
   {
      ToolBar *pBar {};
      Forest children;

      void swap(Tree &that)
      {
         std::swap(pBar, that.pBar);
         children.swap(that.children);
      }
   };

   Iterator FindPlace(const ToolBar *bar) const;
   std::pair<Forest*, Forest::iterator> FindParent(const ToolBar *bar);

   Forest mForest;
};

class ToolDock final : public wxPanelWrapper
{
public:

   ToolDock( ToolManager *manager, wxWindow *parent, int dockid );
   ~ToolDock();

   bool AcceptsFocus() const override { return false; };

   void LoadConfig();
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

   // backup gets old contents of the configuration;  the configuration is
   // set to the wrapped configuration.
   void WrapConfiguration(ToolBarConfiguration &backup);

   // Reverse what was done by WrapConfiguration.
   void RestoreConfiguration(ToolBarConfiguration &backup);

 protected:

   void OnErase( wxEraseEvent & event );
   void OnSize( wxSizeEvent & event );
   void OnPaint( wxPaintEvent & event );
   void OnGrabber( GrabberEvent & event );
   void OnMouseEvents(wxMouseEvent &event);

 private:
   class LayoutVisitor;
   void VisitLayout(LayoutVisitor &visitor,
                    ToolBarConfiguration *pWrappedConfiguration = nullptr);

   void Updated();

   int mTotalToolBarHeight;
   wxWindow *mParent;

   ToolManager *mManager;

   // Stores adjacency relations that we want to realize in the dock layout
   ToolBarConfiguration mConfiguration;

   // Configuration as modified by the constraint of the main window width
   ToolBarConfiguration mWrappedConfiguration;

   ToolBar *mBars[ ToolBarCount ];

 public:

   DECLARE_CLASS( ToolDock )
   DECLARE_EVENT_TABLE()
};

#endif
