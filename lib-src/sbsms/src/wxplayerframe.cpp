#include "wxplayerframe.h"
#include <wx/progdlg.h>
#include <wx/filename.h>
#include "wxconvertdlg.h"
#include "convert.h"

BEGIN_EVENT_TABLE(wxPlayerFrame, wxFrame)
  EVT_MENU(wxID_OPEN,wxPlayerFrame::OnOpen)
  EVT_MENU(wxID_SAVE,wxPlayerFrame::OnSave)
  EVT_MENU(wxID_EXIT,wxPlayerFrame::OnExit)
END_EVENT_TABLE()

bool progressCB(int progress, const char *msg, void *data)
{
  wxProgressDialog *progressDlg = (wxProgressDialog*)data;
  return progressDlg->Update(progress);
}

void wxPlayerFrame::OnOpen(wxCommandEvent& WXUNUSED(event))
{
  wxFileDialog dlg(this, wxT("Choose a file"), wxT("."), wxT(""), wxT("All filetypes (*.sbsms;*.mp3;*.wav;*.aif;*.aiff;*.pcm)|*.sbsms;*.mp3;*.wav;*.aif;*.aiff;*.pcm"),wxOPEN);
  if(dlg.ShowModal() == wxID_OK) {
    wxString pathStr = dlg.GetPath();
    wxFileName path(pathStr);
    
    player->close();
    if(path.GetExt().Cmp(wxT("sbsms")) == 0) {
      if(player->open(path.GetFullPath().fn_str()))
	openFileName = path.GetFullPath();
    } else {
      ConvertDialog convertDlg(this);
      if(convertDlg.ShowModal() == wxID_OK) {
	bool bPreAnalyze = convertDlg.getPreAnalyze();
	int quality = convertDlg.getQuality();
	
	wxFileName sbsmsPath(path.GetPath(wxPATH_GET_VOLUME|wxPATH_GET_SEPARATOR) + path.GetName() + wxT(".sbsms"));
	wxProgressDialog progress(wxT("Progress"),wxT("Converting to ") + sbsmsPath.GetFullName());
	bool status = sbsms_convert(path.GetFullPath().fn_str(),sbsmsPath.GetFullPath().fn_str(),true,false,bPreAnalyze,quality,progressCB,&progress,1.0f,1.0f,1.0f,1.0f,1.0f);
	if(status) {
	  if(player->open(sbsmsPath.GetFullPath().fn_str()))
	    openFileName = sbsmsPath.GetFullPath();
	} else {
	  wxMessageDialog msg(this,wxT("Conversion Failed"),wxT(""),wxOK);
	}
      }
    }
  }
}

void wxPlayerFrame::OnSave(wxCommandEvent& WXUNUSED(event))
{
  if(openFileName.Cmp(wxT("")) != 0) {
    wxFileDialog dlg(this, wxT("Choose a file"), wxT("."), wxT(""), wxT("*.wav"),wxSAVE|wxOVERWRITE_PROMPT);
    if(dlg.ShowModal() == wxID_OK) {
      wxString pathStr = dlg.GetPath();
      wxFileName path(pathStr);
      
      wxProgressDialog progress(wxT("Progress"),wxT("Converting to ") + path.GetFullName());
      real rate = player->getRate();
      real ratio = player->getRatio(); 
      real volume = player->getVolume();
      bool status = sbsms_convert(openFileName.fn_str(),path.GetFullPath().fn_str(),false,true,false,1,progressCB,&progress,rate,rate,ratio,ratio,volume);
      if(!status) {
      wxMessageDialog msg(this,wxT("Conversion Failed"),wxT(""),wxOK);
      }
    }
  }
}

void wxPlayerFrame::OnExit(wxCommandEvent& WXUNUSED(event))
{
  Close();
}

wxPlayerFrame::wxPlayerFrame(sbsmsplayer *player) 
  : wxFrame( (wxFrame*)NULL, -1, wxT("SBSMS"))
{
  this->player = player;
  openFileName = wxT("");
  
  wxMenu* file_menu = new wxMenu;
  file_menu->Append( wxID_OPEN, _("Open") );
  file_menu->AppendSeparator();
  file_menu->Append( wxID_SAVE, _("Save") );
  file_menu->AppendSeparator();
  file_menu->Append( wxID_EXIT, _("Exit") );

  wxMenuBar *menu_bar = new wxMenuBar;
  menu_bar->Append( file_menu,	_("&File") );
  
  SetMenuBar( menu_bar );
#ifdef __WXMSW__
      wxIcon ic(wxICON(SBSMSLogo));
	  SetIcon(ic);
#endif 
  wxBoxSizer *hs = new wxBoxSizer( wxHORIZONTAL );
  playCtrl = new PlayCtrl(this,player);
  playCtrl->Show(true);
  hs->Add(playCtrl);

  SetSizer(hs);
  hs->Fit(this);
  Layout();
}

void wxPlayerFrame :: OnPause()
{
  GetMenuBar()->Enable(wxID_OPEN,true);
  GetMenuBar()->Enable(wxID_SAVE,true);
  GetMenuBar()->Enable(wxID_EXIT,true);
}

void wxPlayerFrame :: OnPlay()
{
  GetMenuBar()->Enable(wxID_OPEN,false);
  GetMenuBar()->Enable(wxID_SAVE,false);
  GetMenuBar()->Enable(wxID_EXIT,false);
}

wxPlayerFrame::~wxPlayerFrame() 
{
}
