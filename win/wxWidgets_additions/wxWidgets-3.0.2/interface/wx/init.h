/////////////////////////////////////////////////////////////////////////////
// Name:        init.h
// Purpose:     interface of global functions
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


/**
    @class wxInitializer

    Create an object of this class on the stack to initialize/cleanup the library
    automatically.

    @library{wxbase}
    @category{appmanagement}

    @see wxGLContext
*/
class wxInitializer
{
public:
    /**
        Initializes the library.
        Calls wxInitialize().
    */
    wxInitializer(int argc = 0, wxChar **argv = NULL);

    /**
        Has the initialization been successful? (explicit test)
    */
    bool IsOk() const;

    /**
        This dtor only does clean up if we initialized the library properly.
        Calls wxUninitialize().
    */
    ~wxInitializer();
};



/** @addtogroup group_funcmacro_appinitterm */
//@{

/**
    This function can be used to perform the initialization of wxWidgets if you
    can't use the default initialization code for any reason.

    If the function returns true, the initialization was successful and the
    global wxApp object ::wxTheApp has been created. Moreover, wxEntryCleanup()
    must be called afterwards. If the function returns false, a catastrophic
    initialization error occurred and (at least the GUI part of) the library
    can't be used at all.

    Notice that parameters @c argc and @c argv may be modified by this
    function.

    @header{wx/init.h}
*/
bool wxEntryStart(int& argc, wxChar** argv);

/**
    See wxEntryStart(int&,wxChar**) for more info about this function.

    This is an additional overload of wxEntryStart() provided under MSW only.
    It is meant to be called with the parameters passed to WinMain().

    @note Under Windows CE platform, and only there, the type of @a pCmdLine is
    @c wchar_t *, otherwise it is @c char *, even in Unicode build.

    @onlyfor{wxmsw}

    @header{wx/init.h}
*/
bool wxEntryStart(HINSTANCE hInstance,
                  HINSTANCE hPrevInstance = NULL,
                  char* pCmdLine = NULL,
                  int nCmdShow = SW_SHOWNORMAL);

/**
    Free resources allocated by a successful call to wxEntryStart().

    @header{wx/init.h}
*/
void wxEntryCleanup();

/**
    Initialize the library (may be called as many times as needed, but each
    call to wxInitialize() must be matched by wxUninitialize()).

    With this function you may avoid wxDECLARE_APP() and wxIMPLEMENT_APP() macros
    and use wxInitialize() and wxUninitialize() dynamically in the
    program startup and termination.

    @header{wx/init.h}
*/
bool wxInitialize(int argc = 0, wxChar **argv = NULL);

/**
    Clean up; the library can't be used any more after the last call to
    wxUninitialize().

    See wxInitialize() for more info.

    @header{wx/init.h}
*/
void wxUninitialize();

/**
    Prevents wxWidgets from setting HighDPI awareness mode.

    wxEntry calls SetDPIProcessAware() early during initialization on Windows.
    To prevent this (e.g. because wx is embedded in native code and disabling
    DPI awareness in the manifest is not an option), call this function
    *before* wxEntry() is called.

    @onlyfor{wxmsw}

    @header{wx/init.h}

    @since 3.0.3, but only available in 3.0.x, not 3.1+ which doesn't make
           the SetDPIProcessAware() call anymore.
*/
void wxMSWDisableSettingHighDPIAware();

//@}

