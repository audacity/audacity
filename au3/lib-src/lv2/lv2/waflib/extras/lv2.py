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

    conf.env['LV2DIR'] = normpath(conf.env['LV2DIR'])

