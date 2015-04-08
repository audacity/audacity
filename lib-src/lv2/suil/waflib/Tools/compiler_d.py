#! /usr/bin/env python
# encoding: utf-8
# WARNING! Do not edit! http://waf.googlecode.com/git/docs/wafbook/single.html#_obtaining_the_waf_file

import os,sys,imp,types
from waflib import Utils,Configure,Options,Logs
def configure(conf):
	for compiler in conf.options.dcheck.split(','):
		conf.env.stash()
		conf.start_msg('Checking for %r (d compiler)'%compiler)
		try:
			conf.load(compiler)
		except conf.errors.ConfigurationError ,e:
			conf.env.revert()
			conf.end_msg(False)
			Logs.debug('compiler_d: %r'%e)
		else:
			if conf.env.D:
				conf.end_msg(conf.env.get_flat('D'))
				conf.env['COMPILER_D']=compiler
				break
			conf.end_msg(False)
	else:
		conf.fatal('no suitable d compiler was found')
def options(opt):
	d_compiler_opts=opt.add_option_group('D Compiler Options')
	d_compiler_opts.add_option('--check-d-compiler',default='gdc,dmd,ldc2',action='store',help='check for the compiler [Default:gdc,dmd,ldc2]',dest='dcheck')
	for d_compiler in['gdc','dmd','ldc2']:
		opt.load('%s'%d_compiler)
