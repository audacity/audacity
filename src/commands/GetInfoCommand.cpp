/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxWidgets

   Dan Horgan

******************************************************************//**

\file GetInfoCommand.cpp
\brief Contains definitions for GetInfoCommand class.
This class now handles the GetAll script command, which can 
- Get all keycodes
- Get all menus
- Get all boxes

*//*******************************************************************/

#include "GetInfoCommand.h"
#include "../Project.h"
#include "CommandManager.h"
#include "../effects/EffectManager.h"
#include "../widgets/Overlay.h"
#include "../widgets/OverlayPanel.h"
#include "../Track.h"
#include "../TrackPanel.h"
#include "../WaveTrack.h"
#include "CommandContext.h"

#include "SelectCommand.h"
#include "../Project.h"
#include "../Track.h"
#include "../ShuttleGui.h"
#include "CommandContext.h"

const int nTypes =7;
static const wxString kTypes[nTypes] =
{
   XO("Commands"),
   XO("Menus"),
   XO("Tracks"),
   XO("Clips"),
   XO("Labels"),
   XO("Keycodes"),
   XO("Boxes")
};

enum {
   kCommands,
   kMenus,
   kTracks,
   kClips,
   kLabels,
   kKeycodes,
   kBoxes
};


const int nFormats =3;
static const wxString kFormats[nFormats] =
{
   XO("JSON"),
   XO("LISP"),
   XO("Other")
};

enum {
   kJson ,
   kLisp,
   kOther
};


bool GetInfoCommand::DefineParams( ShuttleParams & S ){
   wxArrayString types( nTypes, kTypes );
   wxArrayString formats( nFormats, kFormats );
   S.DefineEnum( mInfoType, wxT("Type"), 0, types );
   S.DefineEnum( mFormat, wxT("Format"), 0, formats );
   return true;
}

void GetInfoCommand::PopulateOrExchange(ShuttleGui & S)
{
   wxArrayString types( nTypes, kTypes );
   wxArrayString formats( nFormats, kFormats );
   S.AddSpace(0, 5);

   S.StartMultiColumn(2, wxALIGN_CENTER);
   {
      S.TieChoice( _("Type:"), mInfoType, &types);
      S.TieChoice( _("Format:"), mFormat, &formats);
   }
   S.EndMultiColumn();
}

bool GetInfoCommand::Apply(const CommandContext &context)
{
   if( mFormat == kJson )
      return ApplyInner( context );

   if( mFormat == kLisp )
   {
      CommandContext LispyContext( 
         *(context.GetProject()), 
         std::make_unique<LispifiedCommandOutputTarget>( *context.pOutput.get() )
         );
      return ApplyInner( LispyContext );
   }

   if( mFormat == kOther )
   {
      CommandContext DeformattedContext( 
         *(context.GetProject()), 
         std::make_unique<DeformattedCommandOutputTarget>( *context.pOutput.get() )
         );
      return ApplyInner( DeformattedContext );
   }

   return false;
}

bool GetInfoCommand::ApplyInner(const CommandContext &context)
{
   switch( mInfoType  ){
      case kCommands : return SendCommands( context );
      case kMenus    : return SendMenus( context );
      case kTracks   : return SendTracks( context );
      case kClips    : return SendClips( context );
      case kLabels   : return SendLabels( context );
      case kKeycodes : return SendKeycodes( context );
      case kBoxes    : return SendBoxes( context );
      default:
         context.Status( "Command options not recognised" );
   }
   return false;
}

bool GetInfoCommand::SendMenus(const CommandContext &context)
{
   wxMenuBar * pBar = context.GetProject()->GetMenuBar();
   if(!pBar ){
      wxLogDebug("No menus");
      return false;
   }

   size_t cnt = pBar->GetMenuCount();
   size_t i;
   wxString Label;
   context.StartArray();
   for(i=0;i<cnt;i++)
   {
      Label = pBar->GetMenuLabelText( i );
      context.StartArray();
      context.AddItem( 0 );
      context.AddItem( 0 );
      context.AddItem( Label );
      context.AddItem( "" );
      context.EndArray();
      ExploreMenu( context, pBar->GetMenu( i ), pBar->GetId(), 1 );
   }
   context.EndArray();
   return true;
}

/**
 Send the list of commands.
 */
bool GetInfoCommand::SendCommands(const CommandContext &context )
{
   context.StartArray();
   PluginManager & pm = PluginManager::Get();
   EffectManager & em = EffectManager::Get();
   {
      const PluginDescriptor *plug = pm.GetFirstPlugin(PluginTypeEffect | PluginTypeGeneric);
      while (plug)
      {
         auto command = em.GetCommandIdentifier(plug->GetID());
         if (!command.IsEmpty()){
            em.GetCommandDefinition( plug->GetID(), context );
         }
         plug = pm.GetNextPlugin(PluginTypeEffect | PluginTypeGeneric );
      }
   }
   context.EndArray();
   return true;
}

#if 0
// Old version which gives enabled/disabled status too.
bool GetInfoCommand::SendMenus(const CommandContext &context)
{
   bool bShowStatus = true;
   wxArrayString names;
   CommandManager *cmdManager = context.GetProject()->GetCommandManager();
   cmdManager->GetAllCommandNames(names, false);
   wxArrayString::iterator iter;
   for (iter = names.begin(); iter != names.end(); ++iter)
   {
      wxString name = *iter;
      wxString out = name;
      if (bShowStatus)
      {
         out += wxT("\t");
         out += cmdManager->GetEnabled(name) ? wxT("Enabled") : wxT("Disabled");
      }
      context.Status(out);
   }
   return true;
}
#endif

bool GetInfoCommand::SendBoxes(const CommandContext &context)
{
   //context.Status("Boxes");
   wxWindow * pWin = context.GetProject();

   context.StartArray();
   wxRect R = pWin->GetScreenRect();

   //R.SetPosition( wxPoint(0,0) );
   
   //wxString Name = pWin->GetName();
   context.StartArray();
   context.AddItem( 0 );
   context.AddItem( R.GetLeft() );
   context.AddItem( R.GetTop() );
   context.AddItem( R.GetRight() );
   context.AddItem( R.GetBottom() );
   context.AddItem( "Audacity Window" ); 
   context.EndArray( );

   ExploreAdornments( context, pWin->GetPosition()+wxSize( 6,-1), pWin, pWin->GetId(), 1 );
   ExploreWindows( context, pWin->GetPosition()+wxSize( 6,-1), pWin, pWin->GetId(), 1 );
   context.EndArray( );
   return true;
}

bool GetInfoCommand::SendTracks(const CommandContext & context)
{
   TrackList *projTracks = context.GetProject()->GetTracks();
   TrackListIterator iter(projTracks);
   Track *trk = iter.First();
   context.StartArray();
   while (trk)
   {
      context.StartStruct();
      context.AddItem( trk->GetName(), "name" );
      auto t = dynamic_cast<WaveTrack*>( trk );
      if( t )
      {
         context.AddItem( t->GetStartTime(), "start" );
         context.AddItem( t->GetEndTime(), "end" );
         context.AddItem( t->GetPan() , "pan");
         context.AddItem( t->GetGain() , "gain");
         context.AddBool( t->GetSelected(), "selected" );
         context.AddBool( t->GetLinked(), "linked");
         context.AddBool( t->GetSolo(), "solo" );
         context.AddBool( t->GetMute(), "mute");
      }
      TrackPanel *panel = context.GetProject()->GetTrackPanel();
      Track * fTrack = panel->GetFocusedTrack();
      context.AddBool( (trk == fTrack), "focused");
      context.EndStruct();
      trk=iter.Next();
   }
   context.EndArray();
   return true;
}

bool GetInfoCommand::SendClips(const CommandContext &context)
{
   context.Status("Clips - Not yet");
   return true;
}

bool GetInfoCommand::SendLabels(const CommandContext &context)
{
   context.Status("Labels - Not yet");
   return true;
}

bool GetInfoCommand::SendKeycodes(const CommandContext &context)
{
   context.Status("Keycodes - Not yet");
   return true;
}

/*******************************************************************
The various Explore functions are called from the Send functions,
and may be recursive.  'Send' is the top level.
*******************************************************************/

void GetInfoCommand::ExploreMenu( const CommandContext &context, wxMenu * pMenu, int Id, int depth ){
   Id;//compiler food.
   if( !pMenu )
      return;

   wxMenuItemList list = pMenu->GetMenuItems();
   size_t lcnt = list.GetCount();
   wxMenuItem * item;
   wxString Label;
   wxString Accel;

   for (size_t lndx = 0; lndx < lcnt; lndx++) {
      item = list.Item(lndx)->GetData();
      Label = item->GetItemLabelText();
      Accel = item->GetItemLabel();
      if( Accel.Contains("\t") )
         Accel = Accel.AfterLast('\t');
      else
         Accel = "";
      if( item->IsSeparator() )
         Label = "----";
      int flags = 0;
      if (item->IsSubMenu())
         flags +=1;
      if (item->IsCheck() && item->IsChecked())
         flags +=2;

      context.StartArray();
      context.AddItem( depth );
      context.AddItem( flags );
      context.AddItem( Label );
      context.AddItem( Accel );
      context.EndArray();

      if (item->IsSubMenu()) {
         pMenu = item->GetSubMenu();
         ExploreMenu( context, pMenu, item->GetId(), depth+1 );
      }
   }
}

void GetInfoCommand::ExploreAdornments( const CommandContext &context,
   wxPoint WXUNUSED(P), wxWindow * pWin, int WXUNUSED(Id), int depth )
{
   // Dang! wxMenuBar returns bogus screen rect.
   // We're going to have to fake it instead.
   //wxMenuBar * pBar = context.GetProject()->GetMenuBar();
   //wxRect R = pBar->GetScreenRect();
   //R.SetPosition( R.GetPosition() - P );

   wxRect R1 = pWin->GetScreenRect();
   wxSize s = pWin->GetWindowBorderSize();
   wxRect R( 2,32, R1.GetWidth() - s.GetWidth() * 2 -16, 22 );

   context.StartArray();
   context.AddItem( depth );
   context.AddItem( R.GetLeft() );
   context.AddItem( R.GetTop() );
   context.AddItem( R.GetRight() );
   context.AddItem( R.GetBottom() );
   context.AddItem( "MenuBar" ); 
   context.EndArray();
}

void GetInfoCommand::ExploreTrackPanel( const CommandContext &context,
   wxPoint P, wxWindow * pWin, int WXUNUSED(Id), int depth )
{
   AudacityProject * pProj = context.GetProject();
   TrackPanel * pTP = pProj->GetTrackPanel();

   wxRect trackRect = pWin->GetRect();

   VisibleTrackIterator iter(pProj);
   for (Track *t = iter.First(); t; t = iter.Next()) {
      trackRect.y = t->GetY() - pTP->mViewInfo->vpos;
      trackRect.height = t->GetHeight();

#if 0
      // Work in progress on getting the TCP button positions and sizes.
      wxRect rect = trackRect;
      Track *l = t->GetLink();

      if (t->GetLinked()) {
         rect.height += l->GetHeight();
      }

      switch (t->GetKind()) {
         case Track::Wave:
         {
            break;
         }
#ifdef USE_MIDI
         case Track::Note:
         {
            break;
         }
#endif // USE_MIDI
         case Track::Label:
            break;
         case Track::Time:
            break;
      }
      {
         // Start with whole track rect
         wxRect R = trackRect;

         // Now exclude left, right, and top insets
         R.x += kLeftInset;
         R.y += kTopInset;
         R.width -= kLeftInset * 2;
         R.height -= kTopInset;

         int labelw = pTP->GetLabelWidth();
         int vrul = pTP->GetVRulerOffset();
         bool bIsWave = true;
         //mTrackInfo.DrawBackground(dc, R, t->GetSelected(), bIsWave, labelw, vrul);


         for (Overlay * pOverlay : pTP->mOverlays) {
            auto R2(pOverlay->GetRectangle(trackRect.GetSize()).first);
            context.Status( wxString::Format("  [ %2i, %3i, %3i, %3i, %3i, \"%s\" ],", 
               depth, R2.GetLeft(), R2.GetTop(), R2.GetRight(), R2.GetBottom(), "Overthing" )); 
         }
      }
#endif

      // The VRuler.
      {  
         wxRect R = trackRect;
         R.x += pTP->GetVRulerOffset();
         R.y += kTopMargin;
         R.width = pTP->GetVRulerWidth();
         R.height -= (kTopMargin + kBottomMargin);
         R.SetPosition( R.GetPosition() + P );

         context.StartArray();
         context.AddItem( depth );
         context.AddItem( R.GetLeft() );
         context.AddItem( R.GetTop() );
         context.AddItem( R.GetRight() );
         context.AddItem( R.GetBottom() );
         context.AddItem( "VRuler" ); 
         context.EndArray();
      }
   }
}


void GetInfoCommand::ExploreWindows( const CommandContext &context,
   wxPoint P, wxWindow * pWin, int Id, int depth )
{
   Id;//Compiler food.

   if( pWin->GetName() == "Track Panel" )
   {
      wxRect R = pWin->GetScreenRect();
      ExploreTrackPanel(  context, R.GetPosition()-P, pWin, Id, depth );
      return;
   }
   wxWindowList list = pWin->GetChildren();
   size_t lcnt = list.GetCount();

   for (size_t lndx = 0; lndx < lcnt; lndx++) {
      wxWindow * item = list[lndx];
      if( !item->IsShown() )
         continue;
      wxRect R = item->GetScreenRect();
      R.SetPosition( R.GetPosition() - P );
      wxString Name = item->GetName();
      // Ignore staticLine and StaticBitmap.
      if( Name.StartsWith( "static" ) )
         continue;
      // Ignore anonymous panels.
      if( Name == "panel"  )
         continue;
      if( Name.IsEmpty() )
         Name = wxString("*") + item->GetToolTipText();

      context.StartArray();
      context.AddItem( depth );
      context.AddItem( R.GetLeft() );
      context.AddItem( R.GetTop() );
      context.AddItem( R.GetRight() );
      context.AddItem( R.GetBottom() );
      context.AddItem( Name ); 
      context.EndArray();

      ExploreWindows( context, P, item, item->GetId(), depth+1 );
   }
}

