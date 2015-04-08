#! /usr/bin/env python
# encoding: utf-8
# WARNING! Do not edit! http://waf.googlecode.com/git/docs/wafbook/single.html#_obtaining_the_waf_file

import os,sys,imp,types
from waflib.Tools import ccroot
from waflib import Utils,Configure
from waflib.Logs import debug
c_compiler={'win32':['msvc','gcc'],'cygwin':['gcc'],'darwin':['gcc'],'aix':['xlc','gcc'],'linux':['gcc','icc'],'sunos':['suncc','gcc'],'irix':['gcc','irixcc'],'hpux':['gcc'],'gnu':['gcc'],'java':['gcc','msvc','icc'],'default':['gcc'],}
def configure(conf):
	try:test_for_compiler=conf.options.check_c_compiler
	except AttributeError:conf.fatal("Add options(opt): opt.load('compiler_c')")
	for compiler in test_for_compiler.split():
		conf.env.stash()
		conf.start_msg('Checking for %r (c compiler)'%compiler)
		try:
			conf.load(compiler)
		except conf.errors.ConfigurationError ,e:
			conf.env.revert()
			conf.end_msg(False)
			debug('compiler_c: %r'%e)
		else:
			if conf.env['CC']:
				conf.end_msg(conf.env.get_flat('CC'))
				conf.env['COMPILER_CC']=compiler
				break
			conf.end_msg(False)
	else:
		conf.fatal('could not configure a c compiler!')
def options(opt):
	opt.load_special_tools('c_*.py',ban=['c_dumbpreproc.py'])
	global c_compiler
	build_platform=Utils.unversioned_sys_platform()
	possible_compiler_list=c_compiler[build_platform in c_compiler and build_platform or'default']
	test_for_compiler=' '.join(possible_compiler_list)
	cc_compiler_opts=opt.add_option_group("C Compiler Options")
	cc_compiler_opts.add_option('--check-c-compiler',default="%s"%test_for_compiler,help='On this platform (%s) the following C-Compiler will be checked by default: "%s"'%(build_platform,test_for_compiler),dest="check_c_compiler")
	for x in test_for_compiler.split():
		opt.load('%s'%x)
