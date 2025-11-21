//
// Copied from wxWidgets 3.0.2 and modified for Audacity
//
/////////////////////////////////////////////////////////////////////////////
// Name:        src/gtk/filedlg.cpp
// Purpose:     native implementation of wxFileDialog
// Author:      Robert Roebling, Zbigniew Zagorski, Mart Raudsepp
// Copyright:   (c) 1998 Robert Roebling, 2004 Zbigniew Zagorski, 2005 Mart Raudsepp
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

#include <gtk/gtk.h>

#include "Internat.h"
#include "BasicUI.h"
#include "../FileDialog.h"

#ifdef __UNIX__
#include <unistd.h> // chdir
#endif

#include <wx/filename.h> // wxFilename
#include <wx/tokenzr.h> // wxStringTokenizer
#include <wx/filefn.h> // ::wxGetCwd
#include <wx/modalhook.h>
#include <wx/sizer.h>

#define wxGTK_CONV(s) (s).utf8_str()
#define wxGTK_CONV_FN(s) (s).fn_str()

// ----------------------------------------------------------------------------
// Convenience class for g_freeing a gchar* on scope exit automatically
// ----------------------------------------------------------------------------

class wxGtkString
{
public:
    explicit wxGtkString(gchar* s)
        : m_str(s) { }
    ~wxGtkString() { g_free(m_str); }

    const gchar* c_str() const { return m_str; }

    operator gchar*() const {
        return m_str;
    }

private:
    gchar* m_str;

    wxDECLARE_NO_COPY_CLASS(wxGtkString);
};

//-----------------------------------------------------------------------------
// "clicked" for OK-button
//-----------------------------------------------------------------------------

extern "C" {
static void gtk_filedialog_ok_callback(GtkWidget* widget, FileDialog* dialog)
{
    int style = dialog->GetWindowStyle();
    wxGtkString filename(gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(widget)));

    // gtk version numbers must be identical with the one in ctor (that calls set_do_overwrite_confirmation)
#ifndef __WXGTK3__
#if GTK_CHECK_VERSION(2, 7, 3)
    if (gtk_check_version(2, 7, 3) != NULL)
#endif
    {
        if ((style & wxFD_SAVE) && (style & wxFD_OVERWRITE_PROMPT)) {
            if (g_file_test(filename, G_FILE_TEST_EXISTS)) {
                using namespace BasicUI;
                auto result = ShowMessageBox(
                    XO("File '%s' already exists, do you really want to overwrite it?")
                    .Format(wxString::FromUTF8(filename)),
                    MessageBoxOptions {}
                    .Caption(XO("Confirm"))
                    .IconStyle(Icon::Question)
                    .ButtonStyle(Button::YesNo));
                if (result != MessageBoxResult::Yes) {
                    return;
                }
            }
        }
    }
#endif

    if (style & wxFD_FILE_MUST_EXIST) {
        if (!g_file_test(filename, G_FILE_TEST_EXISTS)) {
            using namespace BasicUI;
            ShowMessageBox(XO("Please choose an existing file."),
                           MessageBoxOptions {}
                           .Caption(XO("Error"))
                           .ButtonStyle(Button::Ok)
                           .IconStyle(Icon::Error));
            return;
        }
    }

    // change to the directory where the user went if asked
    if (style & wxFD_CHANGE_DIR) {
        // Use chdir to not care about filename encodings
        wxGtkString folder(g_path_get_dirname(filename));
        chdir(folder);
    }

    wxCommandEvent event(wxEVT_BUTTON, wxID_OK);
    event.SetEventObject(dialog);
    dialog->HandleWindowEvent(event);
}
}

//-----------------------------------------------------------------------------
// "clicked" for Cancel-button
//-----------------------------------------------------------------------------

extern "C"
{
static void
gtk_filedialog_cancel_callback(GtkWidget* WXUNUSED(w), FileDialog* dialog)
{
    wxCommandEvent event(wxEVT_COMMAND_BUTTON_CLICKED, wxID_CANCEL);
    event.SetEventObject(dialog);
    dialog->HandleWindowEvent(event);
}

static void gtk_filedialog_response_callback(GtkWidget* w,
                                             gint response,
                                             FileDialog* dialog)
{
    if (response == GTK_RESPONSE_ACCEPT) {
        gtk_filedialog_ok_callback(w, dialog);
    } else { // GTK_RESPONSE_CANCEL or GTK_RESPONSE_NONE
        gtk_filedialog_cancel_callback(w, dialog);
    }
}

static void gtk_filedialog_selchanged_callback(GtkFileChooser* chooser,
                                               FileDialog* dialog)
{
    wxGtkString filename(gtk_file_chooser_get_preview_filename(chooser));

    dialog->GTKSelectionChanged(wxString::FromUTF8(filename));
}

static void gtk_filedialog_update_preview_callback(GtkFileChooser* chooser,
                                                   gpointer user_data)
{
    GtkWidget* preview = GTK_WIDGET(user_data);

    wxGtkString filename(gtk_file_chooser_get_preview_filename(chooser));

    if (!filename) {
        return;
    }

    GdkPixbuf* pixbuf = gdk_pixbuf_new_from_file_at_size(filename, 128, 128, NULL);
    gboolean have_preview = pixbuf != NULL;

    gtk_image_set_from_pixbuf(GTK_IMAGE(preview), pixbuf);
    if (pixbuf) {
        g_object_unref(pixbuf);
    }

    gtk_file_chooser_set_preview_widget_active(chooser, have_preview);
}

static void gtk_filedialog_folderchanged_callback(GtkFileChooser* chooser,
                                                  FileDialog* dialog)
{
    dialog->GTKFolderChanged();
}

static void gtk_filedialog_filterchanged_callback(GtkFileChooser* chooser,
                                                  GParamSpec* pspec,
                                                  FileDialog* dialog)
{
    dialog->GTKFilterChanged();
}

static GtkWidget* find_widget(GtkWidget* parent, const gchar* name, int depth)
{
    // printf("%*.*c%s\n", depth, depth, ' ', gtk_widget_get_name(parent));

    GtkWidget* widget = NULL;
    if (g_ascii_strncasecmp(gtk_widget_get_name(parent), name, strlen(name)) == 0) {
        return parent;
    }

    if (GTK_IS_BIN(parent)) {
        return find_widget(gtk_bin_get_child(GTK_BIN(parent)), name, depth + 1);
    }

    if (GTK_IS_CONTAINER(parent)) {
        GList* list = gtk_container_get_children(GTK_CONTAINER(parent));
        for (GList* node = list; node; node = node->next) {
            widget = find_widget(GTK_WIDGET(node->data), name, depth + 1);
            if (widget) {
                break;
            }
        }
        g_list_free(list);
    }

    return widget;
}
} // extern "C"

void FileDialog::AddChildGTK(wxWindowGTK* child)
{
    // allow dialog to be resized smaller horizontally
    gtk_widget_set_size_request(
        child->m_widget, child->GetMinWidth(), child->m_height);

//    In GTK 3+, adding our container as the extra widget can cause the
//    the filter combo to grow to the same height as our container.  This
//    makes for a very odd looking filter combo.  So, we manually add our
//    container below the action bar.
#if GTK_CHECK_VERSION(3, 0, 0)
    GtkWidget* actionbar = find_widget(m_widget, "GtkActionBar", 0);
    if (actionbar) {
        GtkWidget* vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
        gtk_container_add(GTK_CONTAINER(vbox), child->m_widget);
        gtk_box_set_child_packing(GTK_BOX(vbox), child->m_widget, TRUE, TRUE, 0, GTK_PACK_START);
        gtk_widget_show(vbox);

        GtkWidget* abparent = gtk_widget_get_parent(actionbar);
        gtk_container_add(GTK_CONTAINER(abparent), vbox);
        gtk_box_set_child_packing(GTK_BOX(abparent), vbox, FALSE, FALSE, 0, GTK_PACK_END);
        gtk_box_reorder_child(GTK_BOX(abparent), actionbar, -2);
    }
#else
    gtk_file_chooser_set_extra_widget(
        GTK_FILE_CHOOSER(m_widget), child->m_widget);
#endif
}

//-----------------------------------------------------------------------------
// FileDialog
//-----------------------------------------------------------------------------

IMPLEMENT_DYNAMIC_CLASS(FileDialog, FileDialogBase)

BEGIN_EVENT_TABLE(FileDialog, FileDialogBase)
EVT_BUTTON(wxID_OK, FileDialog::OnFakeOk)
EVT_SIZE(FileDialog::OnSize)
END_EVENT_TABLE()

FileDialog::FileDialog(wxWindow* parent, const wxString& message,
                       const wxString& defaultDir,
                       const wxString& defaultFileName,
                       const wxString& wildCard,
                       long style, const wxPoint& pos,
                       const wxSize& sz,
                       const wxString& name)
    : FileDialogBase()
{
    Create(parent, message, defaultDir, defaultFileName, wildCard, style, pos, sz, name);
}

bool FileDialog::Create(wxWindow* parent, const wxString& message,
                        const wxString& defaultDir,
                        const wxString& defaultFileName,
                        const wxString& wildCard,
                        long style, const wxPoint& pos,
                        const wxSize& sz,
                        const wxString& name)
{
    parent = GetParentForModalDialog(parent, style);

    if (!FileDialogBase::Create(parent, message, defaultDir, defaultFileName,
                                wildCard, style, pos, sz, name)) {
        return false;
    }

    if (!PreCreation(parent, pos, wxDefaultSize)
        || !CreateBase(parent, wxID_ANY, pos, wxDefaultSize, style,
                       wxDefaultValidator, wxT("filedialog"))) {
        wxFAIL_MSG(wxT("FileDialog creation failed"));
        return false;
    }

    GtkFileChooserAction gtk_action;
    GtkWindow* gtk_parent = NULL;
    if (parent) {
        gtk_parent = GTK_WINDOW(gtk_widget_get_toplevel(parent->m_widget));
    }

    const gchar* ok_btn_stock;
    if (style & wxFD_SAVE) {
        gtk_action = GTK_FILE_CHOOSER_ACTION_SAVE;
        ok_btn_stock = GTK_STOCK_SAVE;
    } else {
        gtk_action = GTK_FILE_CHOOSER_ACTION_OPEN;
        ok_btn_stock = GTK_STOCK_OPEN;
    }

    m_widget = gtk_file_chooser_dialog_new(
        wxGTK_CONV(m_message),
        gtk_parent,
        gtk_action,
        GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
        ok_btn_stock, GTK_RESPONSE_ACCEPT,
        NULL);
    g_object_ref(m_widget);
    GtkFileChooser* file_chooser = GTK_FILE_CHOOSER(m_widget);

    m_fc.SetWidget(file_chooser);

    gtk_dialog_set_default_response(GTK_DIALOG(m_widget), GTK_RESPONSE_ACCEPT);

    if (style & wxFD_MULTIPLE) {
        gtk_file_chooser_set_select_multiple(file_chooser, true);
    }

    // local-only property could be set to false to allow non-local files to be
    // loaded. In that case get/set_uri(s) should be used instead of
    // get/set_filename(s) everywhere and the GtkFileChooserDialog should
    // probably also be created with a backend, e.g. "gnome-vfs", "default", ...
    // (gtk_file_chooser_dialog_new_with_backend). Currently local-only is kept
    // as the default - true:
    // gtk_file_chooser_set_local_only(GTK_FILE_CHOOSER(m_widget), true);

    g_signal_connect(m_widget, "response",
                     G_CALLBACK(gtk_filedialog_response_callback), this);

    g_signal_connect(m_widget, "selection-changed",
                     G_CALLBACK(gtk_filedialog_selchanged_callback), this);

    g_signal_connect(m_widget, "current-folder-changed",
                     G_CALLBACK(gtk_filedialog_folderchanged_callback), this);

    g_signal_connect(m_widget, "notify::filter",
                     G_CALLBACK(gtk_filedialog_filterchanged_callback), this);

    // deal with extensions/filters
    SetWildcard(wildCard);

    wxString defaultFileNameWithExt = defaultFileName;
    if (!wildCard.empty() && !defaultFileName.empty()
        && !wxFileName(defaultFileName).HasExt()) {
        // append the default extension, if any, to the initial file name: GTK
        // won't do it for us by default (unlike e.g. MSW)
        const wxFileName fnWC(m_fc.GetCurrentWildCard());
        if (fnWC.HasExt()) {
            // Notice that we shouldn't append the extension if it's a wildcard
            // because this is not useful: the user would need to change it to use
            // some fixed extension anyhow.
            const wxString& ext = fnWC.GetExt();
            if (ext.find_first_of("?*") == wxString::npos) {
                defaultFileNameWithExt << "." << ext;
            }
        }
    }

    // if defaultDir is specified it should contain the directory and
    // defaultFileName should contain the default name of the file, however if
    // directory is not given, defaultFileName contains both
    wxFileName fn;
    if (defaultDir.empty()) {
        fn.Assign(defaultFileNameWithExt);
    } else if (!defaultFileNameWithExt.empty()) {
        fn.Assign(defaultDir, defaultFileNameWithExt);
    } else {
        fn.AssignDir(defaultDir);
    }

    // set the initial file name and/or directory
    fn.MakeAbsolute(); // GTK+ needs absolute path
    const wxString dir = fn.GetPath();
    if (!dir.empty()) {
        gtk_file_chooser_set_current_folder(file_chooser, wxGTK_CONV_FN(dir));
    }

    const wxString fname = fn.GetFullName();
    if (style & wxFD_SAVE) {
        if (!fname.empty()) {
            gtk_file_chooser_set_current_name(file_chooser, wxGTK_CONV_FN(fname));
        }

#if GTK_CHECK_VERSION(2, 7, 3)
        if ((style & wxFD_OVERWRITE_PROMPT)
#ifndef __WXGTK3__
            && gtk_check_version(2, 7, 3) == NULL
#endif
            ) {
            gtk_file_chooser_set_do_overwrite_confirmation(file_chooser, true);
        }
#endif
    } else { // wxFD_OPEN
        if (!fname.empty()) {
            gtk_file_chooser_set_filename(file_chooser,
                                          wxGTK_CONV_FN(fn.GetFullPath()));
        }
    }

    if (style & wxFD_PREVIEW) {
        GtkWidget* previewImage = gtk_image_new();

        gtk_file_chooser_set_preview_widget(file_chooser, previewImage);
        g_signal_connect(m_widget, "update-preview",
                         G_CALLBACK(gtk_filedialog_update_preview_callback),
                         previewImage);
    }

    return true;
}

FileDialog::~FileDialog()
{
    if (m_extraControl) {
        // get chooser to drop its reference right now, allowing wxWindow dtor
        // to verify that ref count drops to zero
        gtk_file_chooser_set_extra_widget(
            GTK_FILE_CHOOSER(m_widget), NULL);
    }
}

void FileDialog::OnFakeOk(wxCommandEvent& WXUNUSED(event))
{
    // Update the current directory from here, accessing it later may not work
    // due to the strange way GtkFileChooser works.
    wxGtkString
        str(gtk_file_chooser_get_current_folder(GTK_FILE_CHOOSER(m_widget)));
    m_dir = wxString::FromUTF8(str);

    EndDialog(wxID_OK);
}

int FileDialog::ShowModal()
{
    WX_HOOK_MODAL_DIALOG();

    // Create the root window
    wxBoxSizer* verticalSizer = new wxBoxSizer(wxVERTICAL);
    wxPanel* root = new wxPanel(this, wxID_ANY);

    if (HasUserPaneCreator()) {
        wxPanel* userpane = new wxPanel(root, wxID_ANY);
        CreateUserPane(userpane);

        wxBoxSizer* horizontalSizer = new wxBoxSizer(wxHORIZONTAL);
        horizontalSizer->Add(userpane, 1, wxEXPAND, 0);
        verticalSizer->Add(horizontalSizer, 1, wxEXPAND | wxALL, 0);
    }

    root->SetSizer(verticalSizer);
    root->Layout();
    verticalSizer->SetSizeHints(root);

    // Send an initial filter changed event
    GTKFilterChanged();

    return wxDialog::ShowModal();
}

// Change the currently displayed extension
void FileDialog::SetFileExtension(const wxString& extension)
{
    wxString filename;

#if defined(__WXGTK3__)
    filename = wxString::FromUTF8(gtk_file_chooser_get_current_name(GTK_FILE_CHOOSER(m_widget)));
#else
    GtkWidget* entry = find_widget(m_widget, "GtkFileChooserEntry", 0);
    if (entry) {
        filename = wxString::FromUTF8(gtk_entry_get_text(GTK_ENTRY(entry)));
    }
#endif

    if (filename == wxEmptyString) {
        filename = m_fc.GetFilename();
    }

    if (filename != wxEmptyString) {
        wxFileName fn(filename);
        fn.SetExt(extension);

        gtk_file_chooser_set_current_name(GTK_FILE_CHOOSER(m_widget), fn.GetFullName().utf8_str());
    }
}

void FileDialog::DoSetSize(int WXUNUSED(x), int WXUNUSED(y),
                           int WXUNUSED(width), int WXUNUSED(height),
                           int WXUNUSED(sizeFlags))
{
}

void FileDialog::OnSize(wxSizeEvent&)
{
    // avoid calling DoLayout(), which will set the (wrong) size of
    // m_extraControl, its size is managed by GtkFileChooser
}

wxString FileDialog::GetPath() const
{
    return m_fc.GetPath();
}

void FileDialog::GetFilenames(wxArrayString& files) const
{
    m_fc.GetFilenames(files);
}

void FileDialog::GetPaths(wxArrayString& paths) const
{
    m_fc.GetPaths(paths);
}

void FileDialog::SetMessage(const wxString& message)
{
    m_message = message;
    SetTitle(message);
}

void FileDialog::SetPath(const wxString& path)
{
    FileDialogBase::SetPath(path);

    // Don't do anything if no path is specified, in particular don't set the
    // path to m_dir below as this would result in opening the dialog in the
    // parent directory of this one instead of m_dir itself.
    if (path.empty()) {
        return;
    }

    // we need an absolute path for GTK native chooser so ensure that we have
    // it: use the initial directory if it was set or just CWD otherwise (this
    // is the default behaviour if m_dir is empty)
    wxFileName fn(path);
    fn.MakeAbsolute(m_dir);
    m_fc.SetPath(fn.GetFullPath());
}

void FileDialog::SetDirectory(const wxString& dir)
{
    FileDialogBase::SetDirectory(dir);

    m_fc.SetDirectory(dir);
}

void FileDialog::SetFilename(const wxString& name)
{
    FileDialogBase::SetFilename(name);

    if (HasFdFlag(wxFD_SAVE)) {
        gtk_file_chooser_set_current_name(GTK_FILE_CHOOSER(m_widget), wxGTK_CONV(name));
    } else {
        wxString path(GetDirectory());
        if (path.empty()) {
            // SetPath() fires an assert if fed other than filepaths
            return;
        }
        SetPath(wxFileName(path, name).GetFullPath());
    }
}

wxString FileDialog::GetFilename() const
{
    wxString currentFilename(m_fc.GetFilename());
    if (currentFilename.empty()) {
        // m_fc.GetFilename() will return empty until the dialog has been shown
        // in which case use any previously provided value
        currentFilename = m_fileName;
    }
    return currentFilename;
}

void FileDialog::SetWildcard(const wxString& wildCard)
{
    FileDialogBase::SetWildcard(wildCard);
    m_fc.SetWildcard(GetWildcard());
}

void FileDialog::SetFilterIndex(int filterIndex)
{
    m_fc.SetFilterIndex(filterIndex);
}

int FileDialog::GetFilterIndex() const
{
    return m_fc.GetFilterIndex();
}

void FileDialog::GTKSelectionChanged(const wxString& filename)
{
    m_currentlySelectedFilename = filename;

    wxFileCtrlEvent event(wxEVT_FILECTRL_SELECTIONCHANGED, this, GetId());

    wxArrayString filenames;
    GetFilenames(filenames);

    event.SetDirectory(GetDirectory());
    event.SetFiles(filenames);

    GetEventHandler()->ProcessEvent(event);
}

void FileDialog::GTKFolderChanged()
{
    wxFileCtrlEvent event(wxEVT_FILECTRL_FOLDERCHANGED, this, GetId());

    event.SetDirectory(GetDirectory());

    GetEventHandler()->ProcessEvent(event);
}

void FileDialog::GTKFilterChanged()
{
    wxFileName filename;

#if defined(__WXGTK3__)
    filename.SetFullName(wxString::FromUTF8(gtk_file_chooser_get_current_name(GTK_FILE_CHOOSER(m_widget))));
#else
    GtkWidget* entry = find_widget(m_widget, "GtkFileChooserEntry", 0);
    if (entry) {
        filename.SetFullName(wxString::FromUTF8(gtk_entry_get_text(GTK_ENTRY(entry))));
    }
#endif

    if (filename.HasName()) {
        wxString ext = m_fc.GetCurrentWildCard().AfterLast(wxT('.')).Lower();
        if (!ext.empty() && ext != wxT("*") && ext != filename.GetExt()) {
            SetFileExtension(ext);
        }
    }

    wxFileCtrlEvent event(wxEVT_FILECTRL_FILTERCHANGED, this, GetId());

    event.SetFilterIndex(GetFilterIndex());

    GetEventHandler()->ProcessEvent(event);
}
