#! /usr/bin/env python
# encoding: utf-8
# WARNING! Do not edit! http://waf.googlecode.com/git/docs/wafbook/single.html#_obtaining_the_waf_file

import os,re
from waflib import Configure,TaskGen,Task,Utils,Runner,Options,Build,Logs
import waflib.Tools.ccroot
from waflib.TaskGen import feature,before_method
from waflib.Logs import error
@before_method('process_source')
@feature('intltool_in')
def apply_intltool_in_f(self):
	try:self.meths.remove('process_source')
	except ValueError:pass
	if not self.env.LOCALEDIR:
		self.env.LOCALEDIR=self.env.PREFIX+'/share/locale'
	for i in self.to_list(self.source):
		node=self.path.find_resource(i)
		podir=getattr(self,'podir','po')
		podirnode=self.path.find_dir(podir)
		if not podirnode:
			error("could not find the podir %r"%podir)
			continue
		cache=getattr(self,'intlcache','.intlcache')
		self.env['INTLCACHE']=os.path.join(self.path.bldpath(),podir,cache)
		self.env['INTLPODIR']=podirnode.bldpath()
		self.env['INTLFLAGS']=getattr(self,'flags',['-q','-u','-c'])
		task=self.create_task('intltool',node,node.change_ext(''))
		inst=getattr(self,'install_path','${LOCALEDIR}')
		if inst:
			self.bld.install_files(inst,task.outputs)
@feature('intltool_po')
def apply_intltool_po(self):
	try:self.meths.remove('process_source')
	except ValueError:pass
	if not self.env.LOCALEDIR:
		self.env.LOCALEDIR=self.env.PREFIX+'/share/locale'
	appname=getattr(self,'appname','set_your_app_name')
	podir=getattr(self,'podir','')
	inst=getattr(self,'install_path','${LOCALEDIR}')
	linguas=self.path.find_node(os.path.join(podir,'LINGUAS'))
	if linguas:
		file=open(linguas.abspath())
		langs=[]
		for line in file.readlines():
			if not line.startswith('#'):
				langs+=line.split()
		file.close()
		re_linguas=re.compile('[-a-zA-Z_@.]+')
		for lang in langs:
			if re_linguas.match(lang):
				node=self.path.find_resource(os.path.join(podir,re_linguas.match(lang).group()+'.po'))
				task=self.create_task('po',node,node.change_ext('.mo'))
				if inst:
					filename=task.outputs[0].name
					(langname,ext)=os.path.splitext(filename)
					inst_file=inst+os.sep+langname+os.sep+'LC_MESSAGES'+os.sep+appname+'.mo'
					self.bld.install_as(inst_file,task.outputs[0],chmod=getattr(self,'chmod',Utils.O644),env=task.env)
	else:
		Logs.pprint('RED',"Error no LINGUAS file found in po directory")
class po(Task.Task):
	run_str='${MSGFMT} -o ${TGT} ${SRC}'
	color='BLUE'
class intltool(Task.Task):
	run_str='${INTLTOOL} ${INTLFLAGS} ${INTLCACHE} ${INTLPODIR} ${SRC} ${TGT}'
	color='BLUE'
def configure(conf):
	conf.find_program('msgfmt',var='MSGFMT')
	conf.find_perl_program('intltool-merge',var='INTLTOOL')
	prefix=conf.env.PREFIX
	datadir=conf.env.DATADIR
	if not datadir:
		datadir=os.path.join(prefix,'share')
	conf.define('LOCALEDIR',os.path.join(datadir,'locale').replace('\\','\\\\'))
	conf.define('DATADIR',datadir.replace('\\','\\\\'))
	if conf.env.CC or conf.env.CXX:
		conf.check(header_name='locale.h')
