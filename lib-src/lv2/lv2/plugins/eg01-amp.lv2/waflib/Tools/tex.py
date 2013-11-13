#! /usr/bin/env python
# encoding: utf-8
# WARNING! Do not edit! http://waf.googlecode.com/git/docs/wafbook/single.html#_obtaining_the_waf_file

import os,re
from waflib import Utils,Task,Errors,Logs
from waflib.TaskGen import feature,before_method
re_bibunit=re.compile(r'\\(?P<type>putbib)\[(?P<file>[^\[\]]*)\]',re.M)
def bibunitscan(self):
	node=self.inputs[0]
	nodes=[]
	if not node:return nodes
	code=node.read()
	for match in re_bibunit.finditer(code):
		path=match.group('file')
		if path:
			for k in['','.bib']:
				Logs.debug('tex: trying %s%s'%(path,k))
				fi=node.parent.find_resource(path+k)
				if fi:
					nodes.append(fi)
			else:
				Logs.debug('tex: could not find %s'%path)
	Logs.debug("tex: found the following bibunit files: %s"%nodes)
	return nodes
exts_deps_tex=['','.ltx','.tex','.bib','.pdf','.png','.eps','.ps']
exts_tex=['.ltx','.tex']
re_tex=re.compile(r'\\(?P<type>include|bibliography|putbib|includegraphics|input|import|bringin|lstinputlisting)(\[[^\[\]]*\])?{(?P<file>[^{}]*)}',re.M)
g_bibtex_re=re.compile('bibdata',re.M)
class tex(Task.Task):
	bibtex_fun,_=Task.compile_fun('${BIBTEX} ${BIBTEXFLAGS} ${SRCFILE}',shell=False)
	bibtex_fun.__doc__="""
	Execute the program **bibtex**
	"""
	makeindex_fun,_=Task.compile_fun('${MAKEINDEX} ${MAKEINDEXFLAGS} ${SRCFILE}',shell=False)
	makeindex_fun.__doc__="""
	Execute the program **makeindex**
	"""
	def exec_command(self,cmd,**kw):
		bld=self.generator.bld
		try:
			if not kw.get('cwd',None):
				kw['cwd']=bld.cwd
		except AttributeError:
			bld.cwd=kw['cwd']=bld.variant_dir
		return Utils.subprocess.Popen(cmd,**kw).wait()
	def scan_aux(self,node):
		nodes=[node]
		re_aux=re.compile(r'\\@input{(?P<file>[^{}]*)}',re.M)
		def parse_node(node):
			code=node.read()
			for match in re_aux.finditer(code):
				path=match.group('file')
				found=node.parent.find_or_declare(path)
				if found and found not in nodes:
					Logs.debug('tex: found aux node '+found.abspath())
					nodes.append(found)
					parse_node(found)
		parse_node(node)
		return nodes
	def scan(self):
		node=self.inputs[0]
		nodes=[]
		names=[]
		seen=[]
		if not node:return(nodes,names)
		def parse_node(node):
			if node in seen:
				return
			seen.append(node)
			code=node.read()
			global re_tex
			for match in re_tex.finditer(code):
				for path in match.group('file').split(','):
					if path:
						add_name=True
						found=None
						for k in exts_deps_tex:
							Logs.debug('tex: trying %s%s'%(path,k))
							found=node.parent.find_resource(path+k)
							for tsk in self.generator.tasks:
								if not found or found in tsk.outputs:
									break
							else:
								nodes.append(found)
								add_name=False
								for ext in exts_tex:
									if found.name.endswith(ext):
										parse_node(found)
										break
						if add_name:
							names.append(path)
		parse_node(node)
		for x in nodes:
			x.parent.get_bld().mkdir()
		Logs.debug("tex: found the following : %s and names %s"%(nodes,names))
		return(nodes,names)
	def check_status(self,msg,retcode):
		if retcode!=0:
			raise Errors.WafError("%r command exit status %r"%(msg,retcode))
	def bibfile(self):
		for aux_node in self.aux_nodes:
			try:
				ct=aux_node.read()
			except(OSError,IOError):
				Logs.error('Error reading %s: %r'%aux_node.abspath())
				continue
			if g_bibtex_re.findall(ct):
				Logs.warn('calling bibtex')
				self.env.env={}
				self.env.env.update(os.environ)
				self.env.env.update({'BIBINPUTS':self.TEXINPUTS,'BSTINPUTS':self.TEXINPUTS})
				self.env.SRCFILE=aux_node.name[:-4]
				self.check_status('error when calling bibtex',self.bibtex_fun())
	def bibunits(self):
		try:
			bibunits=bibunitscan(self)
		except OSError:
			Logs.error('error bibunitscan')
		else:
			if bibunits:
				fn=['bu'+str(i)for i in xrange(1,len(bibunits)+1)]
				if fn:
					Logs.warn('calling bibtex on bibunits')
				for f in fn:
					self.env.env={'BIBINPUTS':self.TEXINPUTS,'BSTINPUTS':self.TEXINPUTS}
					self.env.SRCFILE=f
					self.check_status('error when calling bibtex',self.bibtex_fun())
	def makeindex(self):
		try:
			idx_path=self.idx_node.abspath()
			os.stat(idx_path)
		except OSError:
			Logs.warn('index file %s absent, not calling makeindex'%idx_path)
		else:
			Logs.warn('calling makeindex')
			self.env.SRCFILE=self.idx_node.name
			self.env.env={}
			self.check_status('error when calling makeindex %s'%idx_path,self.makeindex_fun())
	def bibtopic(self):
		p=self.inputs[0].parent.get_bld()
		if os.path.exists(os.path.join(p.abspath(),'btaux.aux')):
			self.aux_nodes+=p.ant_glob('*[0-9].aux')
	def run(self):
		env=self.env
		if not env['PROMPT_LATEX']:
			env.append_value('LATEXFLAGS','-interaction=batchmode')
			env.append_value('PDFLATEXFLAGS','-interaction=batchmode')
			env.append_value('XELATEXFLAGS','-interaction=batchmode')
		fun=self.texfun
		node=self.inputs[0]
		srcfile=node.abspath()
		texinputs=self.env.TEXINPUTS or''
		self.TEXINPUTS=node.parent.get_bld().abspath()+os.pathsep+node.parent.get_src().abspath()+os.pathsep+texinputs+os.pathsep
		self.cwd=self.inputs[0].parent.get_bld().abspath()
		Logs.warn('first pass on %s'%self.__class__.__name__)
		self.env.env={}
		self.env.env.update(os.environ)
		self.env.env.update({'TEXINPUTS':self.TEXINPUTS})
		self.env.SRCFILE=srcfile
		self.check_status('error when calling latex',fun())
		self.aux_nodes=self.scan_aux(node.change_ext('.aux'))
		self.idx_node=node.change_ext('.idx')
		self.bibtopic()
		self.bibfile()
		self.bibunits()
		self.makeindex()
		hash=''
		for i in range(10):
			prev_hash=hash
			try:
				hashes=[Utils.h_file(x.abspath())for x in self.aux_nodes]
				hash=Utils.h_list(hashes)
			except(OSError,IOError):
				Logs.error('could not read aux.h')
				pass
			if hash and hash==prev_hash:
				break
			Logs.warn('calling %s'%self.__class__.__name__)
			self.env.env={}
			self.env.env.update(os.environ)
			self.env.env.update({'TEXINPUTS':self.TEXINPUTS})
			self.env.SRCFILE=srcfile
			self.check_status('error when calling %s'%self.__class__.__name__,fun())
class latex(tex):
	texfun,vars=Task.compile_fun('${LATEX} ${LATEXFLAGS} ${SRCFILE}',shell=False)
class pdflatex(tex):
	texfun,vars=Task.compile_fun('${PDFLATEX} ${PDFLATEXFLAGS} ${SRCFILE}',shell=False)
class xelatex(tex):
	texfun,vars=Task.compile_fun('${XELATEX} ${XELATEXFLAGS} ${SRCFILE}',shell=False)
class dvips(Task.Task):
	run_str='${DVIPS} ${DVIPSFLAGS} ${SRC} -o ${TGT}'
	color='BLUE'
	after=['latex','pdflatex','xelatex']
class dvipdf(Task.Task):
	run_str='${DVIPDF} ${DVIPDFFLAGS} ${SRC} ${TGT}'
	color='BLUE'
	after=['latex','pdflatex','xelatex']
class pdf2ps(Task.Task):
	run_str='${PDF2PS} ${PDF2PSFLAGS} ${SRC} ${TGT}'
	color='BLUE'
	after=['latex','pdflatex','xelatex']
@feature('tex')
@before_method('process_source')
def apply_tex(self):
	if not getattr(self,'type',None)in['latex','pdflatex','xelatex']:
		self.type='pdflatex'
	tree=self.bld
	outs=Utils.to_list(getattr(self,'outs',[]))
	self.env['PROMPT_LATEX']=getattr(self,'prompt',1)
	deps_lst=[]
	if getattr(self,'deps',None):
		deps=self.to_list(self.deps)
		for filename in deps:
			n=self.path.find_resource(filename)
			if not n:
				self.bld.fatal('Could not find %r for %r'%(filename,self))
			if not n in deps_lst:
				deps_lst.append(n)
	for node in self.to_nodes(self.source):
		if self.type=='latex':
			task=self.create_task('latex',node,node.change_ext('.dvi'))
		elif self.type=='pdflatex':
			task=self.create_task('pdflatex',node,node.change_ext('.pdf'))
		elif self.type=='xelatex':
			task=self.create_task('xelatex',node,node.change_ext('.pdf'))
		task.env=self.env
		if deps_lst:
			try:
				lst=tree.node_deps[task.uid()]
				for n in deps_lst:
					if not n in lst:
						lst.append(n)
			except KeyError:
				tree.node_deps[task.uid()]=deps_lst
		v=dict(os.environ)
		p=node.parent.abspath()+os.pathsep+self.path.abspath()+os.pathsep+self.path.get_bld().abspath()+os.pathsep+v.get('TEXINPUTS','')+os.pathsep
		v['TEXINPUTS']=p
		if self.type=='latex':
			if'ps'in outs:
				tsk=self.create_task('dvips',task.outputs,node.change_ext('.ps'))
				tsk.env.env=dict(v)
			if'pdf'in outs:
				tsk=self.create_task('dvipdf',task.outputs,node.change_ext('.pdf'))
				tsk.env.env=dict(v)
		elif self.type=='pdflatex':
			if'ps'in outs:
				self.create_task('pdf2ps',task.outputs,node.change_ext('.ps'))
	self.source=[]
def configure(self):
	v=self.env
	for p in'tex latex pdflatex xelatex bibtex dvips dvipdf ps2pdf makeindex pdf2ps'.split():
		try:
			self.find_program(p,var=p.upper())
		except self.errors.ConfigurationError:
			pass
	v['DVIPSFLAGS']='-Ppdf'
