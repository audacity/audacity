/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxWidgets

   Dan Horgan

******************************************************************//**

\file AutomationCommands.cpp
\brief Contains definitions for AutomationCommands class.
This class now handles the GetAll script command, which can 
- Get all keycodes
- Get all menus
- Get all boxes

*//*******************************************************************/

#include "AutomationCommands.h"
#include "../Project.h"
#include "CommandManager.h"
#include "../widgets/Overlay.h"
#include "../widgets/OverlayPanel.h"
#include "../TrackPanel.h"

wxString AutomationCommandsType::BuildName()
{
   return mCustomName;
}

void AutomationCommandsType::BuildSignature(CommandSignature &signature)
{
   auto infoTypeValidator = make_movable<OptionValidator>();
   infoTypeValidator->AddOption(wxT("Menus"));
   infoTypeValidator->AddOption(wxT("Menus+"));
   infoTypeValidator->AddOption(wxT("Keycodes"));
   infoTypeValidator->AddOption(wxT("Boxes"));

   signature.AddParameter(wxT("Type"), "Menus", std::move(infoTypeValidator));
}

CommandHolder AutomationCommandsType::Create(std::unique_ptr<CommandOutputTarget> &&target)
{
   return std::make_shared<AutomationCommands>(*this, std::move(target));
}



void AutomationCommands::ExploreMenu( wxMenu * pMenu, int Id, int depth ){
   static_cast<void>(Id);//compiler food.
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

      Status( wxString::Format("  [ %2i, %2i, \"%s\", \"%s\" ],", depth, flags, Label,Accel )); 
      if (item->IsSubMenu()) {
         pMenu = item->GetSubMenu();
         ExploreMenu( pMenu, item->GetId(), depth+1 );
      }
   }
}

bool AutomationCommands::SendMenusPlus(CommandExecutionContext context)
{
   wxMenuBar * pBar = context.GetProject()->GetMenuBar();
   if(!pBar ){
      wxLogDebug("No menus");
      return false;
   }

   size_t cnt = pBar->GetMenuCount();
   size_t i;
   wxString Label;
   Status( "AudacityMenus[" );
   for(i=0;i<cnt;i++)
   {
      Label = pBar->GetMenuLabelText( i );
      Status( wxString::Format("  [ %2i, %2i, \"%s\", \"%s\" ],", 0, 0, Label, "" )); 
      ExploreMenu( pBar->GetMenu( i ), pBar->GetId(), 1 );
   }
   Status( "];" );
   return true;
}



bool AutomationCommands::SendMenus(CommandExecutionContext context)
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
      Status(out);
   }
   return true;
}

bool AutomationCommands::SendClips(CommandExecutionContext context)
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
      Status(out);
   }
   return true;
}

bool AutomationCommands::SendKeycodes(CommandExecutionContext WXUNUSED(context))
{
   Status("Keycodes");
   return true;
}

void AutomationCommands::ExploreAdornments( CommandExecutionContext WXUNUSED(context),
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

   Status( wxString::Format("  [ %2i, %3i, %3i, %3i, %3i, \"%s\" ],", 
      depth, R.GetLeft(), R.GetTop(), R.GetRight(), R.GetBottom(), "MenuBar" )); 
}

void AutomationCommands::ExploreTrackPanel( CommandExecutionContext context,
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
            Status( wxString::Format("  [ %2i, %3i, %3i, %3i, %3i, \"%s\" ],", 
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

         Status( wxString::Format("  [ %2i, %3i, %3i, %3i, %3i, \"%s\" ],", 
            depth, R.GetLeft(), R.GetTop(), R.GetRight(), R.GetBottom(), "VRuler" )); 
      }
   }
}


void AutomationCommands::ExploreWindows( CommandExecutionContext context,
   wxPoint P, wxWindow * pWin, int Id, int depth )
{
   static_cast<void>(Id);//Compiler food.

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
      Status( wxString::Format("  [ %2i, %3i, %3i, %3i, %3i, \"%s\" ],", 
         depth, R.GetLeft(), R.GetTop(), R.GetRight(), R.GetBottom(), Name )); 
      ExploreWindows( context, P, item, item->GetId(), depth+1 );
   }
}


bool AutomationCommands::SendBoxes(CommandExecutionContext context)
{
   Status("Boxes");
   wxWindow * pWin = context.GetProject();

   Status( "AudacityBoxes[" );
   wxRect R = pWin->GetScreenRect();

   //R.SetPosition( wxPoint(0,0) );
   
   //wxString Name = pWin->GetName();
   Status( wxString::Format("  [ %2i, %3i, %3i, %3i, %3i, \"%s\" ],", 
         0, R.GetLeft(), R.GetTop(), R.GetRight(), R.GetBottom(), "Audacity Window" )); 
   ExploreAdornments( context, pWin->GetPosition()+wxSize( 6,-1), pWin, pWin->GetId(), 1 );
   ExploreWindows( context, pWin->GetPosition()+wxSize( 6,-1), pWin, pWin->GetId(), 1 );
   Status( "];" );
   return true;
}



bool AutomationCommands::Apply(CommandExecutionContext context)
{
   wxString mode = mMode;

   bool bOK = false;
   if (mode.IsSameAs(wxT("GetMenus")))
   {
      bOK = SendMenus( context );
   }
   if (mode.IsSameAs(wxT("GetMenusPlus")))
   {
      bOK = SendMenusPlus( context );
   }
   else if (mode.IsSameAs(wxT("GetClips")))
   {
      bOK = SendClips( context );
   }
   else if (mode.IsSameAs(wxT("GetKeycodes")))
   {
      bOK = SendKeycodes( context );
   }
   else if (mode.IsSameAs(wxT("GetBoxes")))
   {
      bOK = SendBoxes( context );
   }

   return bOK;
}
