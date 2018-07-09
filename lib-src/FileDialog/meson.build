project('FileDialog',
  'cpp',
  version: '1.0.0',
  meson_version: '>=0.47.0',
)

project_mail = 'feedback@audacityteam.org'

conf = configuration_data()

# Get wxWidgets
wx_args = []
if get_option('buildtype').contains('deb')
  wx_args += '--debug=yes'
  add_project_arguments('-DwxDEBUG_LEVEL=1', language: 'cpp')
else
  add_project_arguments('-DwxDEBUG_LEVEL=0', language: 'cpp')
endif
if get_option('wx-version') != ''
  wx_args += '--version=' + get_option('wx-version')
endif
if get_option('force-gtk3')
  wx_args += '--toolkit=gtk3'
endif

wxwidgets = dependency('wxwidgets', modules: wx_args)



# OS-specific configuration
if    host_machine.system().contains('darwin')
  add_languages('objc')
  implementation = 'mac'

  private_srcs = files('mac/FileDialogPrivate.mm')
  private_deps = []

elif  host_machine.system().contains('windows')
  implementation = 'win'

  private_srcs = files('win/FileDialogPrivate.cpp')
  private_deps = []

else
  implementation = 'gtk'

  # Check for gtk2/3
  use_gtk3 = meson.get_compiler('cpp').run('''
  #include <wx/wx.h>
  int main() {
  #if defined(__WXGTK3__)
    return 0;
  #else
    return 1;
  #endif
  }
  ''', dependencies: wxwidgets).returncode() == 0

  gtk_dep_name    = use_gtk3 ? 'gtk+-3.0' : 'gtk+-2.0'
  gtk_dep_version = use_gtk3 ? '>=3.10.0' : '>=2.24.0'
  gtk = dependency(gtk_dep_name, version: gtk_dep_version)
  add_project_arguments('-DHAVE_GTK', language: 'cpp')

  private_srcs = files('gtk/FileDialogPrivate.cpp')
  private_deps = [ gtk, ]

endif

message('Implementation to use: @0@'.format(implementation))


libfiledialog = static_library('filedialog',
  [
    'FileDialog.cpp',
    private_srcs,
  ],
  dependencies: [
    wxwidgets,
    private_deps,
  ],
)

filedialog_dep = declare_dependency(
  include_directories: include_directories('.'),
  link_with: libfiledialog,
  version: meson.project_version(),
  dependencies: [],
)
