#! /usr/bin/env python
# encoding: utf-8
# WARNING! Do not edit! http://waf.googlecode.com/git/docs/wafbook/single.html#_obtaining_the_waf_file

import os,sys,imp,types
from waflib.Tools import ccroot
from waflib import Utils,Configure
from waflib.Logs import debug
cxx_compiler={'win32':['msvc','g++'],'cygwin':['g++'],'darwin':['g++'],'aix':['xlc++','g++'],'linux':['g++','icpc'],'sunos':['sunc++','g++'],'irix':['g++'],'hpux':['g++'],'gnu':['g++'],'java':['g++','msvc','icpc'],'default':['g++']}
def configure(conf):
	try:test_for_compiler=conf.options.check_cxx_compiler
	except AttributeError:conf.fatal("Add options(opt): opt.load('compiler_cxx')")
	for compiler in test_for_compiler.split():
		conf.env.stash()
		conf.start_msg('Checking for %r (c++ compiler)'%compiler)
		try:
			conf.load(compiler)
		except conf.errors.ConfigurationError ,e:
			conf.env.revert()
			conf.end_msg(False)
			debug('compiler_cxx: %r'%e)
		else:
			if conf.env['CXX']:
				conf.end_msg(conf.env.get_flat('CXX'))
				conf.env['COMPILER_CXX']=compiler
				break
			conf.end_msg(False)
	else:
		conf.fatal('could not configure a c++ compiler!')
def options(opt):
	opt.load_special_tools('cxx_*.py')
	global cxx_compiler
	build_platform=Utils.unversioned_sys_platform()
	possible_compiler_list=cxx_compiler[build_platform in cxx_compiler and build_platform or'default']
	test_for_compiler=' '.join(possible_compiler_list)
	cxx_compiler_opts=opt.add_option_group('C++ Compiler Options')
	cxx_compiler_opts.add_option('--check-cxx-compiler',default="%s"%test_for_compiler,help='On this platform (%s) the following C++ Compiler will be checked by default: "%s"'%(build_platform,test_for_compiler),dest="check_cxx_compiler")
	for x in test_for_compiler.split():
		opt.load('%s'%x)
