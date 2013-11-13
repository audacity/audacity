#! /usr/bin/env python
# encoding: utf-8
# WARNING! Do not edit! http://waf.googlecode.com/git/docs/wafbook/single.html#_obtaining_the_waf_file

from fnmatch import fnmatchcase
import os,os.path,re,stat
from waflib import Task,Utils,Node,Logs
from waflib.TaskGen import feature
DOXY_STR='${DOXYGEN} - '
DOXY_FMTS='html latex man rft xml'.split()
DOXY_FILE_PATTERNS='*.'+' *.'.join('''
c cc cxx cpp c++ java ii ixx ipp i++ inl h hh hxx hpp h++ idl odl cs php php3
inc m mm py f90c cc cxx cpp c++ java ii ixx ipp i++ inl h hh hxx
'''.split())
re_rl=re.compile('\\\\\r*\n',re.MULTILINE)
re_nl=re.compile('\r*\n',re.M)
def parse_doxy(txt):
	tbl={}
	txt=re_rl.sub('',txt)
	lines=re_nl.split(txt)
	for x in lines:
		x=x.strip()
		if not x or x.startswith('#')or x.find('=')<0:
			continue
		if x.find('+=')>=0:
			tmp=x.split('+=')
			key=tmp[0].strip()
			if key in tbl:
				tbl[key]+=' '+'+='.join(tmp[1:]).strip()
			else:
				tbl[key]='+='.join(tmp[1:]).strip()
		else:
			tmp=x.split('=')
			tbl[tmp[0].strip()]='='.join(tmp[1:]).strip()
	return tbl
class doxygen(Task.Task):
	vars=['DOXYGEN','DOXYFLAGS']
	color='BLUE'
	def runnable_status(self):
		'''
		self.pars are populated in runnable_status - because this function is being
		run *before* both self.pars "consumers" - scan() and run()

		set output_dir (node) for the output
		'''
		for x in self.run_after:
			if not x.hasrun:
				return Task.ASK_LATER
		if not getattr(self,'pars',None):
			txt=self.inputs[0].read()
			self.pars=parse_doxy(txt)
			if not self.pars.get('OUTPUT_DIRECTORY'):
				self.pars['OUTPUT_DIRECTORY']=self.inputs[0].parent.get_bld().abspath()
			self.doxy_inputs=getattr(self,'doxy_inputs',[])
			if not self.pars.get('INPUT'):
				self.doxy_inputs.append(self.inputs[0].parent)
			else:
				for i in self.pars.get('INPUT').split():
					if os.path.isabs(i):
						node=self.generator.bld.root.find_node(i)
					else:
						node=self.generator.path.find_node(i)
					if not node:
						self.generator.bld.fatal('Could not find the doxygen input %r'%i)
					self.doxy_inputs.append(node)
		if not getattr(self,'output_dir',None):
			bld=self.generator.bld
			self.output_dir=bld.root.find_dir(self.pars['OUTPUT_DIRECTORY'])
			if not self.output_dir:
				self.output_dir=bld.path.find_or_declare(self.pars['OUTPUT_DIRECTORY'])
		self.signature()
		return Task.Task.runnable_status(self)
	def scan(self):
		if self.pars.get('RECURSIVE')=='YES':
			Logs.warn("Doxygen RECURSIVE dependencies are not supported")
		exclude_patterns=self.pars.get('EXCLUDE_PATTERNS','').split()
		file_patterns=self.pars.get('FILE_PATTERNS','').split()
		if not file_patterns:
			file_patterns=DOXY_FILE_PATTERNS
		nodes=[]
		names=[]
		for node in self.doxy_inputs:
			if os.path.isdir(node.abspath()):
				for m in node.ant_glob(file_patterns):
					nodes.append(m)
			else:
				nodes.append(node)
		return(nodes,names)
	def run(self):
		dct=self.pars.copy()
		dct['INPUT']=' '.join([x.abspath()for x in self.doxy_inputs])
		code='\n'.join(['%s = %s'%(x,dct[x])for x in self.pars])
		code=code
		cmd=Utils.subst_vars(DOXY_STR,self.env)
		env=self.env.env or None
		proc=Utils.subprocess.Popen(cmd,shell=True,stdin=Utils.subprocess.PIPE,env=env,cwd=self.generator.bld.path.get_bld().abspath())
		proc.communicate(code)
		return proc.returncode
	def post_run(self):
		nodes=self.output_dir.ant_glob('**/*',quiet=True)
		for x in nodes:
			x.sig=Utils.h_file(x.abspath())
		self.outputs+=nodes
		return Task.Task.post_run(self)
class tar(Task.Task):
	run_str='${TAR} ${TAROPTS} ${TGT} ${SRC}'
	color='RED'
	after=['doxygen']
	def runnable_status(self):
		for x in getattr(self,'input_tasks',[]):
			if not x.hasrun:
				return Task.ASK_LATER
		if not getattr(self,'tar_done_adding',None):
			self.tar_done_adding=True
			for x in getattr(self,'input_tasks',[]):
				self.set_inputs(x.outputs)
			if not self.inputs:
				return Task.SKIP_ME
		return Task.Task.runnable_status(self)
	def __str__(self):
		tgt_str=' '.join([a.nice_path(self.env)for a in self.outputs])
		return'%s: %s\n'%(self.__class__.__name__,tgt_str)
@feature('doxygen')
def process_doxy(self):
	if not getattr(self,'doxyfile',None):
		self.generator.bld.fatal('no doxyfile??')
	node=self.doxyfile
	if not isinstance(node,Node.Node):
		node=self.path.find_resource(node)
	if not node:
		raise ValueError('doxygen file not found')
	dsk=self.create_task('doxygen',node)
	if getattr(self,'doxy_tar',None):
		tsk=self.create_task('tar')
		tsk.input_tasks=[dsk]
		tsk.set_outputs(self.path.find_or_declare(self.doxy_tar))
		if self.doxy_tar.endswith('bz2'):
			tsk.env['TAROPTS']=['cjf']
		elif self.doxy_tar.endswith('gz'):
			tsk.env['TAROPTS']=['czf']
		else:
			tsk.env['TAROPTS']=['cf']
def configure(conf):
	conf.find_program('doxygen',var='DOXYGEN')
	conf.find_program('tar',var='TAR')
