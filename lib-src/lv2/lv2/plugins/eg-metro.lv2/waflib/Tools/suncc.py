#! /usr/bin/env python
# encoding: utf-8
# WARNING! Do not edit! http://waf.googlecode.com/git/docs/wafbook/single.html#_obtaining_the_waf_file

import os
from waflib import Utils
from waflib.Tools import ccroot,ar
from waflib.Configure import conf
@conf
def find_scc(conf):
	v=conf.env
	cc=None
	if v['CC']:cc=v['CC']
	elif'CC'in conf.environ:cc=conf.environ['CC']
	if not cc:cc=conf.find_program('cc',var='CC')
	if not cc:conf.fatal('Could not find a Sun C compiler')
	cc=conf.cmd_to_list(cc)
	try:
		conf.cmd_and_log(cc+['-flags'])
	except Exception:
		conf.fatal('%r is not a Sun compiler'%cc)
	v['CC']=cc
	v['CC_NAME']='sun'
	conf.get_suncc_version(cc)
@conf
def scc_common_flags(conf):
	v=conf.env
	v['CC_SRC_F']=[]
	v['CC_TGT_F']=['-c','-o']
	if not v['LINK_CC']:v['LINK_CC']=v['CC']
	v['CCLNK_SRC_F']=''
	v['CCLNK_TGT_F']=['-o']
	v['CPPPATH_ST']='-I%s'
	v['DEFINES_ST']='-D%s'
	v['LIB_ST']='-l%s'
	v['LIBPATH_ST']='-L%s'
	v['STLIB_ST']='-l%s'
	v['STLIBPATH_ST']='-L%s'
	v['SONAME_ST']='-Wl,-h,%s'
	v['SHLIB_MARKER']='-Bdynamic'
	v['STLIB_MARKER']='-Bstatic'
	v['cprogram_PATTERN']='%s'
	v['CFLAGS_cshlib']=['-Kpic','-DPIC']
	v['LINKFLAGS_cshlib']=['-G']
	v['cshlib_PATTERN']='lib%s.so'
	v['LINKFLAGS_cstlib']=['-Bstatic']
	v['cstlib_PATTERN']='lib%s.a'
def configure(conf):
	conf.find_scc()
	conf.find_ar()
	conf.scc_common_flags()
	conf.cc_load_tools()
	conf.cc_add_flags()
	conf.link_add_flags()
