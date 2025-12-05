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

#include "Internat.h"
#include "../FileDialog.h"

#include <wx/app.h>
#include <wx/choice.h>
#include <wx/clipbrd.h>
#include <wx/evtloop.h>
#include <wx/filectrl.h>
#include <wx/filename.h>
#include <wx/modalhook.h>
#include <wx/sizer.h>
#include <wx/sysopt.h>
#include <wx/stattext.h>
#include <wx/tokenzr.h>

#include <wx/osx/core/private.h>

#include <AppKit/AppKit.h>

// ============================================================================
// implementation
// ============================================================================

@interface OSPanelDelegate : NSObject <NSOpenSavePanelDelegate>
{
    FileDialog* _dialog;
}

- (FileDialog*) fileDialog;
- (void) setFileDialog:(FileDialog*) dialog;

- (void)panel:(id)sender didChangeToDirectoryURL:(NSURL*)url;
- (void)panelSelectionDidChange:(id)sender;
- (BOOL)panel:(id)sender validateURL:(NSURL*)url error:(NSError* _Nullable*)outError;

- (void)viewResized:(NSNotification*)notification;

@end

@implementation OSPanelDelegate
- (void)viewResized:(NSNotification*)notification
{
    _dialog->DoViewResized([notification object]);
}

- (id) init
{
    if (self = [super init]) {
        _dialog = NULL;
    }
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

- (void)panel:(id)sender didChangeToDirectoryURL:(NSURL*)url AVAILABLE_MAC_OS_X_VERSION_10_6_AND_LATER
{
    wxString path = wxCFStringRef::AsStringWithNormalizationFormC([url path]);

    _dialog->DoSendFolderChangedEvent(sender, path);
}

- (void)panelSelectionDidChange:(id)sender AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER
{
    _dialog->DoSendSelectionChangedEvent(sender);
}

// Do NOT remove this method.  For an explanation, refer to:
//
//    http://bugzilla.audacityteam.org/show_bug.cgi?id=2371
//
- (BOOL)panel:(id)sender validateURL:(NSURL*)url error:(NSError* _Nullable*)outError;
{
    // We handle filename validation after the panel closes
    return YES;
}

@end

wxIMPLEMENT_CLASS(FileDialog, FileDialogBase)

FileDialog::FileDialog()
    :   FileDialogBase()
{
    Init();
}

FileDialog::FileDialog(wxWindow* parent,
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

    Create(parent, message, defaultDir, defaultFile, wildCard, style, pos, sz, name);
}

void FileDialog::Init()
{
    m_filterIndex = -1;
    m_delegate = nil;
    m_filterPanel = NULL;
    m_filterChoice = NULL;
}

void FileDialog::Create(
    wxWindow* parent, const wxString& message,
    const wxString& defaultDir, const wxString& defaultFileName, const wxString& wildCard,
    long style, const wxPoint& pos, const wxSize& sz, const wxString& name)
{
    FileDialogBase::Create(parent, message, defaultDir, defaultFileName, wildCard, style, pos, sz, name);
}

FileDialog::~FileDialog()
{
}

bool FileDialog::SupportsExtraControl() const
{
    return true;
}

NSArray* GetTypesFromExtension(const wxString extensiongroup, wxArrayString& extensions)
{
    NSMutableArray* types = nil;
    extensions.Clear();

    wxStringTokenizer tokenizer(extensiongroup, wxT(";"));
    while (tokenizer.HasMoreTokens())
    {
        wxString extension = tokenizer.GetNextToken();
        // Remove leading '*'
        if (extension.length() && (extension.GetChar(0) == '*')) {
            extension = extension.Mid(1);
        }

        // Remove leading '.'
        if (extension.length() && (extension.GetChar(0) == '.')) {
            extension = extension.Mid(1);
        }

        // Remove leading '*', this is for handling *.*
        if (extension.length() && (extension.GetChar(0) == '*')) {
            extension = extension.Mid(1);
        }

        if (extension.IsEmpty()) {
            extensions.Clear();
            [types release];
            types = nil;
            return nil;
        }

        if (types == nil) {
            types = [[NSMutableArray alloc] init];
        }

        extensions.Add(extension.Lower());
        wxCFStringRef cfext(extension);
        [types addObject: (NSString*)cfext.AsNSString()  ];
    }

    [types autorelease];
    return types;
}

NSArray* GetTypesFromFilter(const wxString& filter, wxArrayString& names, wxArrayString& extensiongroups)
{
    NSMutableArray* types = nil;
    bool allowAll = false;

    names.Clear();
    extensiongroups.Clear();

    if (!filter.empty()) {
        wxStringTokenizer tokenizer(filter, wxT("|"));
        int numtokens = (int)tokenizer.CountTokens();
        if (numtokens == 1) {
            // we allow for compatibility reason to have a single filter expression (like *.*) without
            // an explanatory text, in that case the first part is name and extension at the same time
            wxString extension = tokenizer.GetNextToken();
            names.Add(extension);
            extensiongroups.Add(extension);
        } else {
            int numextensions = numtokens / 2;
            for (int i = 0; i < numextensions; i++) {
                wxString name = tokenizer.GetNextToken();
                wxString extension = tokenizer.GetNextToken();
                names.Add(name);
                extensiongroups.Add(extension);
            }
        }

        const size_t extCount = extensiongroups.GetCount();
        wxArrayString extensions;
        for ( size_t i = 0; i < extCount; i++ ) {
            NSArray* exttypes = GetTypesFromExtension(extensiongroups[i], extensions);
            if (exttypes != nil) {
                if (allowAll == false) {
                    if (types == nil) {
                        types = [[NSMutableArray alloc] init];
                    }

                    [types addObjectsFromArray:exttypes];
                }
            } else {
                allowAll = true;
                [types release];
                types = nil;
            }
        }
    }
    [types autorelease];
    return types;
}

void FileDialog::DoOnFilterSelected(int index)
{
    if (index == wxNOT_FOUND) {
        return;
    }

    NSArray* types = GetTypesFromExtension(m_filterExtensions[index], m_currentExtensions);
    NSSavePanel* panel = (NSSavePanel*)GetWXWindow();
    [panel setAllowedFileTypes:types];

    m_filterIndex = index;

    wxFileCtrlEvent event(wxEVT_FILECTRL_FILTERCHANGED, this, GetId());
    event.SetFilterIndex(m_filterIndex);
    GetEventHandler()->ProcessEvent(event);
}

// An item has been selected in the file filter wxChoice:
void FileDialog::OnFilterSelected(wxCommandEvent& WXUNUSED(event))
{
    DoOnFilterSelected(m_filterChoice->GetSelection());
}

void FileDialog::DoViewResized(void* object)
{
    m_filterPanel->Layout();
}

void FileDialog::DoSendFolderChangedEvent(void* panel, const wxString& path)
{
    m_dir = path;

    wxFileCtrlEvent event(wxEVT_FILECTRL_FOLDERCHANGED, this, GetId());

    event.SetDirectory(m_dir);

    GetEventHandler()->ProcessEvent(event);
}

void FileDialog::DoSendSelectionChangedEvent(void* panel)
{
    if (HasFlag(wxFD_SAVE)) {
        NSSavePanel* sPanel = (NSSavePanel*)panel;
        NSString* path = [[sPanel URL] path];
        wxFileName fn(wxCFStringRef::AsStringWithNormalizationFormC(path));
        if (!fn.GetFullPath().empty()) {
            m_path = fn.GetFullPath();
            m_dir = fn.GetPath();
            m_fileName = fn.GetFullName();
            m_fileNames.Clear();
            m_fileNames.Add(m_fileName);
        }
    } else {
        NSOpenPanel* oPanel = (NSOpenPanel*)panel;
        m_paths.Clear();
        m_fileNames.Clear();

        NSArray* urls = [oPanel URLs];
        for ( size_t i = 0; i < [urls count]; ++i ) {
            NSString* path = [[urls objectAtIndex:i] path];
            wxString fnstr = wxCFStringRef::AsStringWithNormalizationFormC(path);
            m_paths.Add(fnstr);
            m_fileNames.Add(wxFileNameFromPath(fnstr));
            if (i == 0) {
                m_path = fnstr;
                m_fileName = wxFileNameFromPath(fnstr);
                m_dir = wxPathOnly(fnstr);
            }
        }
    }

    wxFileCtrlEvent event(wxEVT_FILECTRL_SELECTIONCHANGED, this, GetId());

    event.SetDirectory(m_dir);
    event.SetFiles(m_fileNames);

    GetEventHandler()->ProcessEvent(event);
}

void FileDialog::SetupExtraControls(WXWindow nativeWindow)
{
    NSSavePanel* panel = (NSSavePanel*)nativeWindow;
    // for sandboxed app we cannot access the outer structures
    // this leads to problems with extra controls, so as a temporary
    // workaround for crashes we don't support those yet
    if ([panel contentView] == nil || getenv("APP_SANDBOX_CONTAINER_ID") != NULL) {
        return;
    }

    OSPanelDelegate* del = [[OSPanelDelegate alloc]init];
    [del setFileDialog:this];
    [panel setDelegate:del];
    m_delegate = del;

    wxNonOwnedWindow::Create(GetParent(), nativeWindow);

    m_filterPanel = NULL;
    m_filterChoice = NULL;
    NSView* accView = nil;

    if (m_useFileTypeFilter || HasUserPaneCreator()) {
        wxBoxSizer* verticalSizer = new wxBoxSizer(wxVERTICAL);

        // FINALLY FOUND IT! Creating the panel with "this" as the parent causes
        // an exception and stack trace to be printed to stderr:
        //
        //   2021-02-17 13:52:14.550 Audacity[69217:891282] warning: <NSRemoteView: 0x7f92f4e67410 com.apple.appkit.xpc.openAndSavePanelService ((null)) NSSavePanelService> ignoring attempt to mutate its subviews (
        //      0   ViewBridge                          0x00007fff6596685d -[NSRemoteView _announceSubviewMutationDisallowed] + 29
        //      1   libwx_osx_cocoau_debug_core-3.1.3.0 0x0000000111c3abf1 _ZN17wxWidgetCocoaImpl5EmbedEP12wxWidgetImpl + 177
        //
        // It's because wxPanel tries to embed the wxPanel into the NSSavePanel and
        // that's not allowed. Everything still works fine, so it can be ignored.
        // But, if you want to dig into it further, changing the "this" parent to
        // GetParent() gets rid of the exception. However, events from the extra
        // controls in the accessory view do not get handled correctly.

        m_filterPanel = new wxPanel(this, wxID_ANY);
        accView = m_filterPanel->GetHandle();

        NSNotificationCenter* center = [NSNotificationCenter defaultCenter];
        [center addObserver:del
         selector:@selector(viewResized:)
         name:NSViewFrameDidChangeNotification
         object:accView];

        if (m_useFileTypeFilter) {
            wxBoxSizer* horizontalSizer = new wxBoxSizer(wxHORIZONTAL);

            wxStaticText* stattext = new wxStaticText(m_filterPanel, wxID_ANY, XO("File type:").Translation());
            horizontalSizer->Add(stattext, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);

            m_filterChoice = new wxChoice(m_filterPanel, wxID_ANY);
            m_filterChoice->Append(m_filterNames);
            if (m_filterNames.GetCount() > 0) {
                if (m_firstFileTypeFilter >= 0) {
                    m_filterChoice->SetSelection(m_firstFileTypeFilter);
                }
            }
            m_filterChoice->Bind(wxEVT_CHOICE, &FileDialog::OnFilterSelected, this);

            horizontalSizer->Add(m_filterChoice, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);
            verticalSizer->Add(horizontalSizer, 0, wxALIGN_CENTER_HORIZONTAL | wxALL, 5);
        }

        if (HasUserPaneCreator()) {
            wxPanel* userpane = new wxPanel(m_filterPanel, wxID_ANY);
            CreateUserPane(userpane);

            wxBoxSizer* horizontalSizer = new wxBoxSizer(wxHORIZONTAL);
            horizontalSizer->Add(userpane, 1, wxEXPAND, 0);
            verticalSizer->Add(horizontalSizer, 1, wxEXPAND, 0);
        }

        m_filterPanel->SetSizer(verticalSizer);
        m_filterPanel->Layout();

        wxSize ws = m_filterPanel->GetBestSize();
        m_filterPanel->SetSize(ws);
        m_filterPanel->SetMinSize(ws);
    }

    if (accView != nil) {
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

    wxCFStringRef cf(m_message);

    wxCFStringRef dir(m_dir);
    wxCFStringRef file(m_fileName);

    m_path.clear();
    m_fileNames.Clear();
    m_paths.Clear();

    wxNonOwnedWindow* parentWindow = NULL;
    int returnCode = -1;

    if (GetParent()) {
        parentWindow = dynamic_cast<wxNonOwnedWindow*>(wxGetTopLevelParent(GetParent()));
    }

    NSArray* types = GetTypesFromFilter(m_wildCard, m_filterNames, m_filterExtensions);

    m_useFileTypeFilter = m_filterExtensions.GetCount() > 0;

#if defined(we_always_want_the_types)
    if (HasFlag(wxFD_OPEN)) {
        if (!(wxSystemOptions::HasOption(wxOSX_FILEDIALOG_ALWAYS_SHOW_TYPES)
              && (wxSystemOptions::GetOptionInt(wxOSX_FILEDIALOG_ALWAYS_SHOW_TYPES) == 1))) {
            m_useFileTypeFilter = false;
        }
    }
#endif

    m_firstFileTypeFilter = wxNOT_FOUND;

    if (m_useFileTypeFilter
        && m_filterIndex >= 0 && m_filterIndex < m_filterExtensions.GetCount()) {
        m_firstFileTypeFilter = m_filterIndex;
    } else if (m_useFileTypeFilter) {
        types = nil;
        bool useDefault = true;
        for ( size_t i = 0; i < m_filterExtensions.GetCount(); ++i ) {
            types = GetTypesFromExtension(m_filterExtensions[i], m_currentExtensions);
            if (m_currentExtensions.GetCount() == 0) {
                useDefault = false;
                m_firstFileTypeFilter = i;
                break;
            }

            for ( size_t j = 0; j < m_currentExtensions.GetCount(); ++j ) {
                if (m_fileName.EndsWith(m_currentExtensions[j])) {
                    m_firstFileTypeFilter = i;
                    useDefault = false;
                    break;
                }
            }
            if (!useDefault) {
                break;
            }
        }
        if (useDefault) {
            types = GetTypesFromExtension(m_filterExtensions[0], m_currentExtensions);
            m_firstFileTypeFilter = 0;
        }
    }

    OSXBeginModalDialog();

    if (HasFlag(wxFD_SAVE)) {
        NSSavePanel* sPanel = [NSSavePanel savePanel];

        SetupExtraControls(sPanel);

        // PRL:
        // Hack for bugs 1300/1579: Intercept key down events, implementing
        // copy/cut/paste, by invoking appropriate selectors. This is done
        // because we do not use the wxWidgets IDs for the menu equivalents.
        id handler;
        if (wxTheClipboard->IsSupported(wxDF_UNICODETEXT)) {
            handler = [
                NSEvent addLocalMonitorForEventsMatchingMask:NSKeyDownMask
                handler:^NSEvent*(NSEvent* event)
                {
                    auto app = [NSApplication sharedApplication];
                    if ([event modifierFlags] & NSCommandKeyMask) {
                        auto chars = [event charactersIgnoringModifiers];
                        if ([chars isEqualToString:@"a"]) {
                            [app sendAction:@selector(selectAll:) to:nil from:nil];
                        } else if ([chars isEqualToString:@"c"]) {
                            [app sendAction:@selector(copy:) to:nil from:nil];
                        } else if ([chars isEqualToString:@"x"]) {
                            [app sendAction:@selector(cut:) to:nil from:nil];
                        } else if ([chars isEqualToString:@"v"]) {
                            [app sendAction:@selector(paste:) to:nil from:nil];
                        }
                    }
                    return event;
                }
                      ];
        }

        // makes things more convenient:
        [sPanel setCanCreateDirectories:YES];
        [sPanel setMessage:cf.AsNSString()];
        // if we should be able to descend into packages we must somehow
        // be able to pass this in
        [sPanel setTreatsFilePackagesAsDirectories:NO];
        [sPanel setCanSelectHiddenExtension:YES];
        [sPanel setExtensionHidden:NO];
        [sPanel setAllowedFileTypes:types];
        [sPanel setAllowsOtherFileTypes:YES];

        if (HasFlag(wxFD_OVERWRITE_PROMPT)) {
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

        [sPanel setDirectoryURL:[NSURL fileURLWithPath:dir.AsNSString()]];
        [sPanel setNameFieldStringValue:file.AsNSString()];
        returnCode = [sPanel runModal];
        ModalFinishedCallback(sPanel, returnCode);
        if (wxTheClipboard->IsSupported(wxDF_UNICODETEXT)) {
            [NSEvent removeMonitor:handler];
        }
    } else {
        NSOpenPanel* oPanel = [NSOpenPanel openPanel];

        SetupExtraControls(oPanel);

        [oPanel setTreatsFilePackagesAsDirectories:NO];
        [oPanel setCanChooseDirectories:NO];
        [oPanel setResolvesAliases:YES];
        [oPanel setCanChooseFiles:YES];
        [oPanel setMessage:cf.AsNSString()];
        [oPanel setAllowsMultipleSelection: (HasFlag(wxFD_MULTIPLE) ? YES : NO)];

        // Note that the test here is intentionally different from the one
        // above, in the wxFD_SAVE case: we need to call DoOnFilterSelected()
        // even for m_firstFileTypeFilter == 0, i.e. when using the default
        // filter.
        if (m_firstFileTypeFilter >= 0) {
            DoOnFilterSelected(m_firstFileTypeFilter);
        } else {
            [oPanel setAllowedFileTypes: (m_delegate == nil ? types : nil)];
        }
        if (!m_dir.IsEmpty()) {
            [oPanel setDirectoryURL:[NSURL fileURLWithPath:dir.AsNSString()
                                     isDirectory:YES]];
        }

        {
            DoOnFilterSelected(m_firstFileTypeFilter);
        }

        returnCode = [oPanel runModal];

        ModalFinishedCallback(oPanel, returnCode);
    }

    OSXEndModalDialog();

    return GetReturnCode();
}

void FileDialog::ModalFinishedCallback(void* panel, int returnCode)
{
    m_paths.Clear();
    m_fileNames.Clear();

    int result = wxID_CANCEL;
    if (HasFlag(wxFD_SAVE)) {
        NSSavePanel* sPanel = (NSSavePanel*)panel;
        if (returnCode == NSOKButton) {
            result = wxID_OK;

            NSString* path = [[sPanel URL] path];
            wxFileName fn(wxCFStringRef::AsStringWithNormalizationFormC(path));
            m_dir = fn.GetPath();
            m_fileName = fn.GetFullName();
            m_path = fn.GetFullPath();

            if (m_filterChoice) {
                m_filterIndex = m_filterChoice->GetSelection();
            }
        }
        [sPanel setDelegate:nil];
    } else {
        NSOpenPanel* oPanel = (NSOpenPanel*)panel;
        if (returnCode == NSOKButton) {
            panel = oPanel;
            result = wxID_OK;

            if (m_filterChoice) {
                m_filterIndex = m_filterChoice->GetSelection();
            }

            NSArray* filenames = [oPanel URLs];
            for ( size_t i = 0; i < [filenames count]; ++i ) {
                wxString fnstr = wxCFStringRef::AsStringWithNormalizationFormC([[filenames objectAtIndex:i] path]);
                m_paths.Add(fnstr);
                m_fileNames.Add(wxFileNameFromPath(fnstr));
                if (i == 0) {
                    m_path = fnstr;
                    m_fileName = wxFileNameFromPath(fnstr);
                    m_dir = wxPathOnly(fnstr);
                }
            }
        }
        [oPanel setDelegate:nil];
    }

    if (m_delegate) {
        [[NSNotificationCenter defaultCenter] removeObserver:m_delegate];

        [m_delegate release];
        m_delegate = nil;
    }

    SetReturnCode(result);

    if (GetModality() == wxDIALOG_MODALITY_WINDOW_MODAL) {
        SendWindowModalDialogEvent(wxEVT_WINDOW_MODAL_DIALOG_CLOSED);
    }

    // workaround for sandboxed app, see above
    if (m_isNativeWindowWrapper) {
        UnsubclassWin();
    }
    [(NSSavePanel*) panel setAccessoryView:nil];
}

// Change the currently displayed extension
void FileDialog::SetFileExtension(const wxString& extension)
{
    NSSavePanel* sPanel = (NSSavePanel*)GetWXWindow();
    m_filterExtensions[m_filterIndex] = extension;
    NSArray* types = GetTypesFromExtension(m_filterExtensions[m_filterIndex], m_currentExtensions);
    [sPanel setAllowedFileTypes:types];
}
