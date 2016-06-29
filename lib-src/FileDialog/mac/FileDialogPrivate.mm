///
// Copied from wxWidgets 3.0.2 and modified to support additional features
//
/////////////////////////////////////////////////////////////////////////////
// Name:        src/cocoa/filedlg.mm
// Purpose:     wxFileDialog for wxCocoa
// Author:      Ryan Norton
// Modified by: Leland Lucius
// Created:     2004-10-02
// Copyright:   (c) Ryan Norton
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

// ============================================================================
// declarations
// ============================================================================

// ----------------------------------------------------------------------------
// headers
// ----------------------------------------------------------------------------

// For compilers that support precompilation, includes "wx.h".
#include <wx/wxprec.h>

#include "FileDialog.h"

#ifndef WX_PRECOMP
    #include "wx/msgdlg.h"
    #include "wx/app.h"
    #include "wx/sizer.h"
    #include "wx/stattext.h"
    #include "wx/choice.h"
#endif

#include "wx/clipbrd.h"
#include "wx/filename.h"
#include "wx/tokenzr.h"
#include "wx/evtloop.h"

#include "wx/osx/private.h"
#include "wx/sysopt.h"
#include "wx/modalhook.h"

#include <mach-o/dyld.h>

// ============================================================================
// implementation
// ============================================================================

@interface OSPanelDelegate : NSObject wxOSX_10_6_AND_LATER(<NSOpenSavePanelDelegate>)
{
    FileDialog* _dialog;
}

- (FileDialog*) fileDialog;
- (void) setFileDialog:(FileDialog*) dialog;

- (void)panel:(id)sender didChangeToDirectoryURL:(NSURL *)url AVAILABLE_MAC_OS_X_VERSION_10_6_AND_LATER;
- (void)panelSelectionDidChange:(id)sender AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER;
- (NSString *)panel:(id)sender userEnteredFilename:(NSString *)filename confirmed:(BOOL)okFlag;

- (void)viewResized:(NSNotification *)notification;

@end

@implementation OSPanelDelegate
- (void)viewResized:(NSNotification *)notification
{
   _dialog->DoViewResized([notification object]);
}

- (id) init
{
    self = [super init];
    _dialog = NULL;
    return self;
}

- (FileDialog*) fileDialog
{
    return _dialog;
}

- (void) setFileDialog:(FileDialog*) dialog
{
    _dialog = dialog;
}

- (void)panel:(id)sender didChangeToDirectoryURL:(NSURL *)url AVAILABLE_MAC_OS_X_VERSION_10_6_AND_LATER
{
    wxString path = wxCFStringRef::AsStringWithNormalizationFormC( [url path] );

    _dialog->DoSendFolderChangedEvent(sender, path);
}

- (void)panelSelectionDidChange:(id)sender AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER
{
    _dialog->DoSendSelectionChangedEvent(sender);
}

- (NSString *)panel:(id)sender userEnteredFilename:(NSString *)filename confirmed:(BOOL)okFlag;
{
    if (okFlag == YES)
    {
        wxString name = wxCFStringRef::AsStringWithNormalizationFormC( filename );

        wxCFStringRef cfname( _dialog->DoCaptureFilename( sender, name ) );

        return [[NSString alloc] initWithString:cfname.AsNSString()];
    }

    return filename;
}
@end

IMPLEMENT_CLASS(FileDialog, FileDialogBase)

FileDialog::FileDialog()
:   FileDialogBase()
{
    Init();
}

FileDialog::FileDialog(wxWindow *parent,
                       const wxString& message,
                       const wxString& defaultDir,
                       const wxString& defaultFile,
                       const wxString& wildCard,
                       long style,
                       const wxPoint& pos,
                       const wxSize& sz,
                       const wxString& name)
:   FileDialogBase()
{
    Init();

    Create(parent,message,defaultDir,defaultFile,wildCard,style,pos,sz,name);
}

void FileDialog::Init()
{
    m_filterIndex = -1;
    m_delegate = nil;
    m_sheetDelegate = nil;
    m_filterPanel = NULL;
    m_filterChoice = NULL;
}

void FileDialog::Create(
    wxWindow *parent, const wxString& message,
    const wxString& defaultDir, const wxString& defaultFileName, const wxString& wildCard,
    long style, const wxPoint& pos, const wxSize& sz, const wxString& name)
{
    FileDialogBase::Create(parent, message, defaultDir, defaultFileName, wildCard, style, pos, sz, name);

    m_sheetDelegate = [[ModalDialogDelegate alloc] init];
    [(ModalDialogDelegate*)m_sheetDelegate setImplementation: this];
}

FileDialog::~FileDialog()
{
    [m_sheetDelegate release];
}

bool FileDialog::SupportsExtraControl() const
{
    return true;
}

NSArray* GetTypesFromExtension( const wxString extensiongroup, wxArrayString& extensions )
{
    NSMutableArray* types = nil;
    extensions.Clear();

    wxStringTokenizer tokenizer( extensiongroup, wxT(";") ) ;
    while ( tokenizer.HasMoreTokens() )
    {
        wxString extension = tokenizer.GetNextToken() ;
        // Remove leading '*'
        if ( extension.length() && (extension.GetChar(0) == '*') )
            extension = extension.Mid( 1 );

        // Remove leading '.'
        if ( extension.length() && (extension.GetChar(0) == '.') )
            extension = extension.Mid( 1 );

        // Remove leading '*', this is for handling *.*
        if ( extension.length() && (extension.GetChar(0) == '*') )
            extension = extension.Mid( 1 );

        if ( extension.IsEmpty() )
        {
            extensions.Clear();
            [types release];
            types = nil;
            return nil;
        }

        if ( types == nil )
            types = [[NSMutableArray alloc] init];

        extensions.Add(extension.Lower());
        wxCFStringRef cfext(extension);
        [types addObject: (NSString*)cfext.AsNSString()  ];
#if 0
        // add support for classic fileType / creator here
        wxUint32 fileType, creator;
        // extension -> mactypes
#endif
    }
    [types autorelease];
    return types;
}

NSArray* GetTypesFromFilter( const wxString& filter, wxArrayString& names, wxArrayString& extensiongroups )
{
    NSMutableArray* types = nil;
    bool allowAll = false;

    names.Clear();
    extensiongroups.Clear();

    if ( !filter.empty() )
    {
        wxStringTokenizer tokenizer( filter, wxT("|") );
        int numtokens = (int)tokenizer.CountTokens();
        if(numtokens == 1)
        {
            // we allow for compatibility reason to have a single filter expression (like *.*) without
            // an explanatory text, in that case the first part is name and extension at the same time
            wxString extension = tokenizer.GetNextToken();
            names.Add( extension );
            extensiongroups.Add( extension );
        }
        else
        {
            int numextensions = numtokens / 2;
            for(int i = 0; i < numextensions; i++)
            {
                wxString name = tokenizer.GetNextToken();
                wxString extension = tokenizer.GetNextToken();
                names.Add( name );
                extensiongroups.Add( extension );
            }
        }

        const size_t extCount = extensiongroups.GetCount();
        wxArrayString extensions;
        for ( size_t i = 0 ; i < extCount; i++ )
        {
            NSArray* exttypes = GetTypesFromExtension(extensiongroups[i], extensions);
            if ( exttypes != nil )
            {
                if ( allowAll == false )
                {
                    if ( types == nil )
                        types = [[NSMutableArray alloc] init];

                    [types addObjectsFromArray:exttypes];
                }
            }
            else
            {
                allowAll = true;
                [types release];
                types = nil;
            }
        }
    }
    [types autorelease];
    return types;
}

void FileDialog::ShowWindowModal()
{
    wxCFStringRef cf( m_message );
    wxCFStringRef dir( m_dir );
    wxCFStringRef file( m_fileName );

    m_noOverwritePromptFilename = wxEmptyString;
    m_path = wxEmptyString;
    m_fileNames.Clear();
    m_paths.Clear();

    wxNonOwnedWindow* parentWindow = NULL;
    
    m_modality = wxDIALOG_MODALITY_WINDOW_MODAL;

    if (GetParent())
        parentWindow = dynamic_cast<wxNonOwnedWindow*>(wxGetTopLevelParent(GetParent()));

    wxASSERT_MSG(parentWindow, "Window modal display requires parent.");

    NSArray* types = GetTypesFromFilter( m_wildCard, m_filterNames, m_filterExtensions ) ;
    if ( HasFlag(wxFD_SAVE) )
    {
        NSSavePanel* sPanel = [NSSavePanel savePanel];

        SetupExtraControls(sPanel);

        // makes things more convenient:
        [sPanel setCanCreateDirectories:YES];
        [sPanel setMessage:cf.AsNSString()];
        // if we should be able to descend into pacakges we must somehow
        // be able to pass this in
        [sPanel setTreatsFilePackagesAsDirectories:NO];
        [sPanel setCanSelectHiddenExtension:YES];
        [sPanel setAllowedFileTypes:types];
        [sPanel setAllowsOtherFileTypes:NO];
        
        NSWindow* nativeParent = parentWindow->GetWXWindow();
        [sPanel beginSheetForDirectory:dir.AsNSString() file:file.AsNSString()
            modalForWindow: nativeParent modalDelegate: m_sheetDelegate
            didEndSelector: @selector(sheetDidEnd:returnCode:contextInfo:)
            contextInfo: nil];
    }
    else 
    {
        NSOpenPanel* oPanel = [NSOpenPanel openPanel];
        
        SetupExtraControls(oPanel);

        [oPanel setTreatsFilePackagesAsDirectories:NO];
        [oPanel setCanChooseDirectories:NO];
        [oPanel setResolvesAliases:YES];
        [oPanel setCanChooseFiles:YES];
        [oPanel setMessage:cf.AsNSString()];
        [oPanel setAllowsMultipleSelection: (HasFlag(wxFD_MULTIPLE) ? YES : NO )];
        
        NSWindow* nativeParent = parentWindow->GetWXWindow();
        [oPanel beginSheetForDirectory:dir.AsNSString() file:file.AsNSString()
            types: types modalForWindow: nativeParent
            modalDelegate: m_sheetDelegate
            didEndSelector: @selector(sheetDidEnd:returnCode:contextInfo:)
            contextInfo: nil];
    }
}

void FileDialog::DoOnFilterSelected(int index)
{
    if (index == wxNOT_FOUND)
    {
      return;
    }

    NSArray* types = GetTypesFromExtension(m_filterExtensions[index],m_currentExtensions);
    NSSavePanel* panel = (NSSavePanel*) GetWXWindow();
    [panel setAllowedFileTypes:types];

    m_filterIndex = index;

    wxFileCtrlEvent event( wxEVT_FILECTRL_FILTERCHANGED, this, GetId() );
    event.SetFilterIndex( m_filterIndex );
    GetEventHandler()->ProcessEvent( event );
}

// An item has been selected in the file filter wxChoice:
void FileDialog::OnFilterSelected( wxCommandEvent &WXUNUSED(event) )
{
    DoOnFilterSelected( m_filterChoice->GetSelection() );
}

void FileDialog::DoViewResized(void* object)
{
   m_filterPanel->Layout();
}

void FileDialog::DoSendFolderChangedEvent(void* panel, const wxString & path)
{
    m_dir = wxPathOnly( path );

    wxFileCtrlEvent event( wxEVT_FILECTRL_FOLDERCHANGED, this, GetId() );

    event.SetDirectory( m_dir );

    GetEventHandler()->ProcessEvent( event );
}

void FileDialog::DoSendSelectionChangedEvent(void* panel)
{
    if ( HasFlag( wxFD_SAVE ) )
    {
        NSSavePanel* sPanel = (NSSavePanel*) panel;
        NSString* path = [[sPanel URL] path];

        m_path = wxCFStringRef::AsStringWithNormalizationFormC( path );
        m_fileName = wxFileNameFromPath( m_path );
        m_dir = wxPathOnly( m_path );
        m_fileNames.Add( m_fileName );
    }
    else
    {
        NSOpenPanel* oPanel = (NSOpenPanel*) panel;
        m_paths.Clear();
        m_fileNames.Clear();

        NSArray* urls = [oPanel URLs];
        for ( size_t i = 0 ; i < [urls count] ; ++ i )
        {
            NSString *path = [[urls objectAtIndex:i] path];
            wxString fnstr = wxCFStringRef::AsStringWithNormalizationFormC( path );
            m_paths.Add( fnstr );
            m_fileNames.Add( wxFileNameFromPath( fnstr ) );
            if ( i == 0 )
            {
                m_path = fnstr;
                m_fileName = wxFileNameFromPath( fnstr );
                m_dir = wxPathOnly( fnstr );
            }
        }
    }

    wxFileCtrlEvent event( wxEVT_FILECTRL_SELECTIONCHANGED, this, GetId() );

    event.SetDirectory( m_dir );
    event.SetFiles( m_fileNames );

    GetEventHandler()->ProcessEvent( event );
}

wxString FileDialog::DoCaptureFilename(void* panel, const wxString & name)
{
    if ( HasFlag( wxFD_SAVE ) )
    {
        if ( !HasFlag(wxFD_OVERWRITE_PROMPT) )
        {
            NSSavePanel* sPanel = (NSSavePanel*) panel;
            NSString* dir = [[sPanel directoryURL] path];
   
            wxFileName fn;
            fn.SetPath(wxCFStringRef::AsStringWithNormalizationFormC( dir ));
            fn.SetFullName(name);
   
            m_currentlySelectedFilename = fn.GetFullPath();

            fn.SetName(wxT("NoOverwritePrompt"));
            fn.AssignTempFileName(fn.GetFullPath());
            m_noOverwritePromptFilename = fn.GetFullPath();
 
            return fn.GetFullName();
        }
    }

    return name;
}

void FileDialog::SetupExtraControls(WXWindow nativeWindow)
{
    NSSavePanel* panel = (NSSavePanel*) nativeWindow;
    // for sandboxed app we cannot access the outer structures
    // this leads to problems with extra controls, so as a temporary
    // workaround for crashes we don't support those yet
    if ( [panel contentView] == nil || getenv("APP_SANDBOX_CONTAINER_ID") != NULL )
        return;

    OSPanelDelegate* del = [[OSPanelDelegate alloc]init];
    [del setFileDialog:this];
    [panel setDelegate:del];
    m_delegate = del;

    wxNonOwnedWindow::Create( GetParent(), nativeWindow );

    m_filterPanel = NULL;
    m_filterChoice = NULL;
    NSView* accView = nil;

    if ( m_useFileTypeFilter || HasUserPaneCreator() )
    {
        wxBoxSizer *verticalSizer = new wxBoxSizer( wxVERTICAL );

        m_filterPanel = new wxPanel( this, wxID_ANY );
        accView = m_filterPanel->GetHandle();

        NSNotificationCenter *center = [NSNotificationCenter defaultCenter];
        [center addObserver:del
                   selector:@selector(viewResized:)
                       name:NSViewFrameDidChangeNotification
                     object:accView];

        if ( m_useFileTypeFilter )
        {
            wxBoxSizer *horizontalSizer = new wxBoxSizer( wxHORIZONTAL );

            wxStaticText *stattext = new wxStaticText( m_filterPanel, wxID_ANY, _("File type:") );
            horizontalSizer->Add( stattext, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5 );

            m_filterChoice = new wxChoice( m_filterPanel, wxID_ANY );
            m_filterChoice->Append( m_filterNames );
            if ( m_filterNames.GetCount() > 0 )
            {
                if ( m_firstFileTypeFilter >= 0 )
                    m_filterChoice->SetSelection( m_firstFileTypeFilter );
            }
            m_filterChoice->Connect( wxEVT_CHOICE, wxCommandEventHandler( FileDialog::OnFilterSelected ), NULL, this );

            horizontalSizer->Add( m_filterChoice, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5 );
            verticalSizer->Add( horizontalSizer, 0, wxALIGN_CENTER_HORIZONTAL|wxALL, 5 );
        }
            
        if ( HasUserPaneCreator() )
        {
            wxPanel *userpane = new wxPanel( m_filterPanel, wxID_ANY );
            CreateUserPane( userpane );

            wxBoxSizer *horizontalSizer = new wxBoxSizer( wxHORIZONTAL );
            horizontalSizer->Add( userpane, 1, wxEXPAND, 0 );
            verticalSizer->Add( horizontalSizer, 1, wxEXPAND, 0 );
        }

        m_filterPanel->SetSizer( verticalSizer );
        m_filterPanel->Layout();

        wxSize ws = m_filterPanel->GetBestSize();
        m_filterPanel->SetSize(ws);
        m_filterPanel->SetMinSize(ws);
    }

    if ( accView != nil )
    {
        [accView removeFromSuperview];
        [accView setAutoresizingMask:NSViewWidthSizable];

        [panel setAccessoryView:accView];
    }
}

int FileDialog::ShowModal()
{
    WX_HOOK_MODAL_DIALOG();

    wxCFEventLoopPauseIdleEvents pause;

    wxMacAutoreleasePool autoreleasepool;
    
    wxCFStringRef cf( m_message );

    wxCFStringRef dir( m_dir );
    wxCFStringRef file( m_fileName );

    m_noOverwritePromptFilename = wxEmptyString;
    m_path = wxEmptyString;
    m_fileNames.Clear();
    m_paths.Clear();

    wxNonOwnedWindow* parentWindow = NULL;
    int returnCode = -1;

    if (GetParent())
    {
        parentWindow = dynamic_cast<wxNonOwnedWindow*>(wxGetTopLevelParent(GetParent()));
    }

    NSArray* types = GetTypesFromFilter( m_wildCard, m_filterNames, m_filterExtensions ) ;

    m_useFileTypeFilter = m_filterExtensions.GetCount() > 0;

#if defined(we_always_want_the_types)
    if( HasFlag(wxFD_OPEN) )
    {
        if ( !(wxSystemOptions::HasOption( wxOSX_FILEDIALOG_ALWAYS_SHOW_TYPES ) && (wxSystemOptions::GetOptionInt( wxOSX_FILEDIALOG_ALWAYS_SHOW_TYPES ) == 1)) )
            m_useFileTypeFilter = false;            
    }
#endif

    m_firstFileTypeFilter = wxNOT_FOUND;

    if ( m_useFileTypeFilter
        && m_filterIndex >= 0 && m_filterIndex < m_filterExtensions.GetCount() )
    {
        m_firstFileTypeFilter = m_filterIndex;
    }
    else if ( m_useFileTypeFilter )
    {
        types = nil;
        bool useDefault = true;
        for ( size_t i = 0; i < m_filterExtensions.GetCount(); ++i )
        {
            types = GetTypesFromExtension(m_filterExtensions[i], m_currentExtensions);
            if ( m_currentExtensions.GetCount() == 0 )
            {
                useDefault = false;
                m_firstFileTypeFilter = i;
                break;
            }
            
            for ( size_t j = 0; j < m_currentExtensions.GetCount(); ++j )
            {
                if ( m_fileName.EndsWith(m_currentExtensions[j]) )
                {
                    m_firstFileTypeFilter = i;
                    useDefault = false;
                    break;
                }
            }
            if ( !useDefault )
                break;
        }
        if ( useDefault )
        {
            types = GetTypesFromExtension(m_filterExtensions[0], m_currentExtensions);
            m_firstFileTypeFilter = 0;
        }
    }

    if ( HasFlag(wxFD_SAVE) )
    {
        NSSavePanel* sPanel = [NSSavePanel savePanel];

        SetupExtraControls(sPanel);

        // PRL:
        // Hack for bug 1300:  intercept key down events, implement a
        // Command+V handler, but it's a bit crude.  It always pastes
        // the entire text field, ignoring the insertion cursor, and ignoring
        // which control really has the focus.
        id handler;
        if (wxTheClipboard->IsSupported(wxDF_TEXT)) {
           handler = [
              NSEvent addLocalMonitorForEventsMatchingMask:NSKeyDownMask
              handler:^NSEvent *(NSEvent *event)
              {
                 if ([event modifierFlags] & NSCommandKeyMask)
                 {
                    auto chars = [event charactersIgnoringModifiers];
                    auto character = [chars characterAtIndex:0];
                    if (character == 'v')
                    {
                       if (wxTheClipboard->Open()) {
                          wxTextDataObject data;
                          wxTheClipboard->GetData(data);
                          wxTheClipboard->Close();
                          wxString text = data.GetText();
                          auto rawText = text.utf8_str();
                          auto length = text.Length();
                          NSString *myString = [[NSString alloc]
                             initWithBytes:rawText.data()
                              length: rawText.length()
                              encoding: NSUTF8StringEncoding
                          ];
                          [sPanel setNameFieldStringValue:myString];
                          [myString release];
                          return nil;
                       }
                    }
                 }

                 return event;
              }
           ];
        }

        // makes things more convenient:
        [sPanel setCanCreateDirectories:YES];
        [sPanel setMessage:cf.AsNSString()];
        // if we should be able to descend into pacakges we must somehow
        // be able to pass this in
        [sPanel setTreatsFilePackagesAsDirectories:NO];
        [sPanel setCanSelectHiddenExtension:YES];
        [sPanel setAllowedFileTypes:types];
        [sPanel setAllowsOtherFileTypes:NO];

        if ( HasFlag(wxFD_OVERWRITE_PROMPT) )
        {
        }

        /*
        Let the file dialog know what file type should be used initially.
        If this is not done then when setting the filter index
        programmatically to 1 the file will still have the extension
        of the first file type instead of the second one. E.g. when file
        types are foo and bar, a filename "myletter" with SetDialogIndex(1)
        would result in saving as myletter.foo, while we want myletter.bar.
        */
//        if(m_firstFileTypeFilter > 0)
        {
            DoOnFilterSelected(m_firstFileTypeFilter);
        }

        returnCode = [sPanel runModalForDirectory: m_dir.IsEmpty() ? nil : dir.AsNSString() file:file.AsNSString() ];
        ModalFinishedCallback(sPanel, returnCode);

        if (wxTheClipboard->IsSupported(wxDF_TEXT))
           [NSEvent removeMonitor:handler];
    }
    else
    {
        NSOpenPanel* oPanel = [NSOpenPanel openPanel];
        
        SetupExtraControls(oPanel);
                
        [oPanel setTreatsFilePackagesAsDirectories:NO];
        [oPanel setCanChooseDirectories:NO];
        [oPanel setResolvesAliases:YES];
        [oPanel setCanChooseFiles:YES];
        [oPanel setMessage:cf.AsNSString()];
        [oPanel setAllowsMultipleSelection: (HasFlag(wxFD_MULTIPLE) ? YES : NO )];

        [oPanel setAllowedFileTypes:types];
        if ( !m_dir.IsEmpty() )
            [oPanel setDirectoryURL:[NSURL fileURLWithPath:dir.AsNSString() 
                                               isDirectory:YES]];

        {
            DoOnFilterSelected(m_firstFileTypeFilter);
        }

        returnCode = [oPanel runModal];
            
        ModalFinishedCallback(oPanel, returnCode);
    }

    return GetReturnCode();
}

void FileDialog::ModalFinishedCallback(void* panel, int returnCode)
{
    m_paths.Clear();
    m_fileNames.Clear();

    int result = wxID_CANCEL;
    if (HasFlag(wxFD_SAVE))
    {
        NSSavePanel* sPanel = (NSSavePanel*)panel;
        if (returnCode == NSOKButton )
        {
            result = wxID_OK;

            m_path = wxCFStringRef::AsStringWithNormalizationFormC([sPanel filename]);
            if (!HasFlag(wxFD_OVERWRITE_PROMPT))
            {
                wxASSERT(!m_noOverwritePromptFilename.IsEmpty());
                if (!m_noOverwritePromptFilename.IsEmpty())
                {
                    wxRemoveFile(m_noOverwritePromptFilename);
                    m_path = m_currentlySelectedFilename;
                }
            }
            m_fileName = wxFileNameFromPath(m_path);
            m_dir = wxPathOnly( m_path );
            if (m_filterChoice)
            {
                m_filterIndex = m_filterChoice->GetSelection();
            }
        }
        [sPanel setDelegate:nil];
    }
    else
    {
        NSOpenPanel* oPanel = (NSOpenPanel*)panel;
        if (returnCode == NSOKButton )
        {
            panel = oPanel;
            result = wxID_OK;
            NSArray* filenames = [oPanel filenames];
            for ( size_t i = 0 ; i < [filenames count] ; ++ i )
            {
                wxString fnstr = wxCFStringRef::AsStringWithNormalizationFormC([filenames objectAtIndex:i]);
                m_paths.Add( fnstr );
                m_fileNames.Add( wxFileNameFromPath(fnstr) );
                if ( i == 0 )
                {
                    m_path = fnstr;
                    m_fileName = wxFileNameFromPath(fnstr);
                    m_dir = wxPathOnly( fnstr );
                }
            }
        }
        [oPanel setDelegate:nil];
    }

    if ( m_delegate )
    {
        [[NSNotificationCenter defaultCenter] removeObserver:m_delegate];
         
        [m_delegate release];
        m_delegate = nil;
    }

    SetReturnCode(result);
    
    if (GetModality() == wxDIALOG_MODALITY_WINDOW_MODAL)
        SendWindowModalDialogEvent ( wxEVT_WINDOW_MODAL_DIALOG_CLOSED  );
    
    // workaround for sandboxed app, see above
    if ( m_isNativeWindowWrapper )
        UnsubclassWin();
    [(NSSavePanel*) panel setAccessoryView:nil];
}
