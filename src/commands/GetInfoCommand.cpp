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
#include "../TrackPanel.h"
#include "CommandContext.h"

#include "SelectCommand.h"
#include "../Project.h"
#include "../Track.h"
#include "../ShuttleGui.h"
#include "CommandContext.h"

const int nTypes =5;
static const wxString kTypes[nTypes] =
{
   XO("Commands"),
   XO("Menus"),
   XO("Clips"),
   XO("Keycodes"),
   XO("Boxes")
};

enum {
   kCommands,
   kMenus,
   kClips,
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
   kJson =0*nTypes,
   kOther =1*nTypes
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
   switch( mInfoType + nTypes * mFormat ){
      case kCommands + kJson  : return SendCommandsAsJson( context);
      case kCommands + kOther : return SendCommandsAsJson( context);
      case kMenus    + kJson  : return SendMenusAsJson( context); 
      case kMenus    + kOther : return SendMenus( context );
      case kClips    + kJson  : return SendClips( context); 
      case kClips    + kOther : return SendClips( context );
      case kKeycodes + kJson  : return SendKeycodes( context); 
      case kKeycodes + kOther : return SendKeycodes( context );
      case kBoxes    + kJson  : return SendBoxesAsJson( context); 
      case kBoxes    + kOther : return SendBoxesAsJson( context );
      default:
         context.Status( "Command options not recognised" );
   }
   return false;
}

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

      context.Status( wxString::Format("  [ %2i, %2i, \"%s\", \"%s\" ],", depth, flags, Label,Accel )); 
      if (item->IsSubMenu()) {
         pMenu = item->GetSubMenu();
         ExploreMenu( context, pMenu, item->GetId(), depth+1 );
      }
   }
}

bool GetInfoCommand::SendMenusAsJson(const CommandContext &context)
{
   wxMenuBar * pBar = context.GetProject()->GetMenuBar();
   if(!pBar ){
      wxLogDebug("No menus");
      return false;
   }

   size_t cnt = pBar->GetMenuCount();
   size_t i;
   wxString Label;
   context.Status( "[" );
   for(i=0;i<cnt;i++)
   {
      Label = pBar->GetMenuLabelText( i );
      context.Status( wxString::Format("  [ %2i, %2i, \"%s\", \"%s\" ],", 0, 0, Label, "" )); 
      ExploreMenu( context, pBar->GetMenu( i ), pBar->GetId(), 1 );
   }
   context.Status( "]" );
   return true;
}

/**
 Send the list of commands.
 */
bool GetInfoCommand::SendCommandsAsJson(const CommandContext &context )
{
   PluginManager & pm = PluginManager::Get();
   EffectManager & em = EffectManager::Get();
   {
      wxString out="";
      wxString maybeComma="";
      const PluginDescriptor *plug = pm.GetFirstPlugin(PluginTypeEffect | PluginTypeGeneric);
      while (plug)
      {
         auto command = em.GetCommandIdentifier(plug->GetID());
         if (!command.IsEmpty()){
            // delayed sending of previous string, so we can maybe add a comma.
            if( !out.IsEmpty() )
            {
               context.Status(out+maybeComma);
               maybeComma = ",";
            }
            out = em.GetCommandDefinition( plug->GetID() );
         }
         plug = pm.GetNextPlugin(PluginTypeEffect | PluginTypeGeneric );
      }
      if( !out.IsEmpty() )
         context.Status(out);  // Last one does not have a comma.
   }
   return true;
}

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

bool GetInfoCommand::SendClips(const CommandContext &context)
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

bool GetInfoCommand::SendKeycodes(const CommandContext &context)
{
   context.Status("Keycodes");
   return true;
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

   context.Status( wxString::Format("  [ %2i, %3i, %3i, %3i, %3i, \"%s\" ],", 
      depth, R.GetLeft(), R.GetTop(), R.GetRight(), R.GetBottom(), "MenuBar" )); 
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

         context.Status( wxString::Format("  [ %2i, %3i, %3i, %3i, %3i, \"%s\" ],", 
            depth, R.GetLeft(), R.GetTop(), R.GetRight(), R.GetBottom(), "VRuler" )); 
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
      context.Status( wxString::Format("  [ %2i, %3i, %3i, %3i, %3i, \"%s\" ],", 
         depth, R.GetLeft(), R.GetTop(), R.GetRight(), R.GetBottom(), Name )); 
      ExploreWindows( context, P, item, item->GetId(), depth+1 );
   }
}


bool GetInfoCommand::SendBoxesAsJson(const CommandContext &context)
{
   context.Status("Boxes");
   wxWindow * pWin = context.GetProject();

   context.Status( "[" );
   wxRect R = pWin->GetScreenRect();

   //R.SetPosition( wxPoint(0,0) );
   
   //wxString Name = pWin->GetName();
   context.Status( wxString::Format("  [ %2i, %3i, %3i, %3i, %3i, \"%s\" ],", 
         0, R.GetLeft(), R.GetTop(), R.GetRight(), R.GetBottom(), "Audacity Window" )); 
   ExploreAdornments( context, pWin->GetPosition()+wxSize( 6,-1), pWin, pWin->GetId(), 1 );
   ExploreWindows( context, pWin->GetPosition()+wxSize( 6,-1), pWin, pWin->GetId(), 1 );
   context.Status( "]" );
   return true;
}




