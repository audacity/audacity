import os
import sys

from waflib import Logs
from waflib import Options

def options(opt):
    conf_opts = opt.get_option_group('Configuration options')
    conf_opts.add_option('--lv2-user', action='store_true', default=False, dest='lv2_user',
                         help='install LV2 bundles to user location')
    conf_opts.add_option('--lv2dir', type='string',
                         help='LV2 bundles [Default: LIBDIR/lv2]')

def register_lv2_path(conf, path):
    """Return the default LV2_PATH to use for this system"""
    if 'LV2_PATH' not in conf.run_env and 'LV2_PATH' not in os.environ:
        conf.run_env['LV2_PATH'] = [conf.env['LV2DIR']]

    conf.run_env.append_unique('LV2_PATH', path)

def default_lv2_path(conf):
    """Return the default LV2_PATH for the build target as a list"""
    if conf.env.DEST_OS == 'darwin':
        return ['~/Library/Audio/Plug-Ins/LV2',
                '~/.lv2',
                '/usr/local/lib/lv2',
                '/usr/lib/lv2',
                '/Library/Audio/Plug-Ins/LV2']
    elif conf.env.DEST_OS == 'haiku':
        return ['~/.lv2',
                '/boot/common/add-ons/lv2']
    elif conf.env.DEST_OS == 'win32':
        return ['%APPDATA%\\\\LV2',
                '%COMMONPROGRAMFILES%\\\\LV2']
    else:
        libdirname = os.path.basename(conf.env.LIBDIR)
        return ['~/.lv2',
                '/usr/%s/lv2' % libdirname,
                '/usr/local/%s/lv2' % libdirname]
    
def configure(conf):
    def env_path(parent_dir_var, name):
        parent = os.getenv(parent_dir_var)
        if parent:
            return os.path.join(parent, name)
        else:
            Logs.warn('Environment variable %s unset, using LIBDIR\n' % parent_dir_var)
            return os.path.join(conf.env['LIBDIR'], name)

    def normpath(path):
        if sys.platform == 'win32':
            return os.path.normpath(path).replace('\\', '/')
        else:
            return os.path.normpath(path)

    if Options.options.lv2dir:
        conf.env['LV2DIR'] = Options.options.lv2dir
    elif Options.options.lv2_user:
        if conf.env.DEST_OS == 'darwin':
            conf.env['LV2DIR'] = env_path('HOME', 'Library/Audio/Plug-Ins/LV2')
        elif conf.env.DEST_OS == 'win32':
            conf.env['LV2DIR'] = env_path('APPDATA', 'LV2')
        else:
            conf.env['LV2DIR'] = env_path('HOME', '.lv2')
    else:
        if conf.env.DEST_OS == 'darwin':
            conf.env['LV2DIR'] = '/Library/Audio/Plug-Ins/LV2'
        elif conf.env.DEST_OS == 'win32':
            conf.env['LV2DIR'] = env_path('COMMONPROGRAMFILES', 'LV2')
        else:
            conf.env['LV2DIR'] = os.path.join(conf.env['LIBDIR'], 'lv2')

    # Add default LV2_PATH to runtime environment for tests that use plugins
    if 'LV2_PATH' not in os.environ:
        conf.run_env['LV2_PATH'] = default_lv2_path(conf)
