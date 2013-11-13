#! /usr/bin/env python
# encoding: utf-8
# WARNING! Do not edit! http://waf.googlecode.com/git/docs/wafbook/single.html#_obtaining_the_waf_file

import os,shlex,sys,time
from waflib import ConfigSet,Utils,Options,Logs,Context,Build,Errors
try:
	from urllib import request
except ImportError:
	from urllib import urlopen
else:
	urlopen=request.urlopen
BREAK='break'
CONTINUE='continue'
WAF_CONFIG_LOG='config.log'
autoconfig=False
conf_template='''# project %(app)s configured on %(now)s by
# waf %(wafver)s (abi %(abi)s, python %(pyver)x on %(systype)s)
# using %(args)s
#'''
def download_check(node):
	pass
def download_tool(tool,force=False,ctx=None):
	for x in Utils.to_list(Context.remote_repo):
		for sub in Utils.to_list(Context.remote_locs):
			url='/'.join((x,sub,tool+'.py'))
			try:
				web=urlopen(url)
				try:
					if web.getcode()!=200:
						continue
				except AttributeError:
					pass
			except Exception:
				continue
			else:
				tmp=ctx.root.make_node(os.sep.join((Context.waf_dir,'waflib','extras',tool+'.py')))
				tmp.write(web.read(),'wb')
				Logs.warn('Downloaded %s from %s'%(tool,url))
				download_check(tmp)
				try:
					module=Context.load_tool(tool)
				except Exception:
					Logs.warn('The tool %s from %s is unusable'%(tool,url))
					try:
						tmp.delete()
					except Exception:
						pass
					continue
				return module
	raise Errors.WafError('Could not load the Waf tool')
class ConfigurationContext(Context.Context):
	'''configures the project'''
	cmd='configure'
	error_handlers=[]
	def __init__(self,**kw):
		super(ConfigurationContext,self).__init__(**kw)
		self.environ=dict(os.environ)
		self.all_envs={}
		self.top_dir=None
		self.out_dir=None
		self.tools=[]
		self.hash=0
		self.files=[]
		self.tool_cache=[]
		self.setenv('')
	def setenv(self,name,env=None):
		if name not in self.all_envs or env:
			if not env:
				env=ConfigSet.ConfigSet()
				self.prepare_env(env)
			else:
				env=env.derive()
			self.all_envs[name]=env
		self.variant=name
	def get_env(self):
		return self.all_envs[self.variant]
	def set_env(self,val):
		self.all_envs[self.variant]=val
	env=property(get_env,set_env)
	def init_dirs(self):
		top=self.top_dir
		if not top:
			top=Options.options.top
		if not top:
			top=getattr(Context.g_module,Context.TOP,None)
		if not top:
			top=self.path.abspath()
		top=os.path.abspath(top)
		self.srcnode=(os.path.isabs(top)and self.root or self.path).find_dir(top)
		assert(self.srcnode)
		out=self.out_dir
		if not out:
			out=Options.options.out
		if not out:
			out=getattr(Context.g_module,Context.OUT,None)
		if not out:
			out=Options.lockfile.replace('.lock-waf_%s_'%sys.platform,'').replace('.lock-waf','')
		self.bldnode=(os.path.isabs(out)and self.root or self.path).make_node(out)
		self.bldnode.mkdir()
		if not os.path.isdir(self.bldnode.abspath()):
			conf.fatal('Could not create the build directory %s'%self.bldnode.abspath())
	def execute(self):
		self.init_dirs()
		self.cachedir=self.bldnode.make_node(Build.CACHE_DIR)
		self.cachedir.mkdir()
		path=os.path.join(self.bldnode.abspath(),WAF_CONFIG_LOG)
		self.logger=Logs.make_logger(path,'cfg')
		app=getattr(Context.g_module,'APPNAME','')
		if app:
			ver=getattr(Context.g_module,'VERSION','')
			if ver:
				app="%s (%s)"%(app,ver)
		now=time.ctime()
		pyver=sys.hexversion
		systype=sys.platform
		args=" ".join(sys.argv)
		wafver=Context.WAFVERSION
		abi=Context.ABI
		self.to_log(conf_template%vars())
		self.msg('Setting top to',self.srcnode.abspath())
		self.msg('Setting out to',self.bldnode.abspath())
		if id(self.srcnode)==id(self.bldnode):
			Logs.warn('Setting top == out (remember to use "update_outputs")')
		elif id(self.path)!=id(self.srcnode):
			if self.srcnode.is_child_of(self.path):
				Logs.warn('Are you certain that you do not want to set top="." ?')
		super(ConfigurationContext,self).execute()
		self.store()
		Context.top_dir=self.srcnode.abspath()
		Context.out_dir=self.bldnode.abspath()
		env=ConfigSet.ConfigSet()
		env['argv']=sys.argv
		env['options']=Options.options.__dict__
		env.run_dir=Context.run_dir
		env.top_dir=Context.top_dir
		env.out_dir=Context.out_dir
		env['hash']=self.hash
		env['files']=self.files
		env['environ']=dict(self.environ)
		if not self.env.NO_LOCK_IN_RUN:
			env.store(Context.run_dir+os.sep+Options.lockfile)
		if not self.env.NO_LOCK_IN_TOP:
			env.store(Context.top_dir+os.sep+Options.lockfile)
		if not self.env.NO_LOCK_IN_OUT:
			env.store(Context.out_dir+os.sep+Options.lockfile)
	def prepare_env(self,env):
		if not env.PREFIX:
			if Options.options.prefix or Utils.is_win32:
				env.PREFIX=os.path.abspath(os.path.expanduser(Options.options.prefix))
			else:
				env.PREFIX=''
		if not env.BINDIR:
			env.BINDIR=Utils.subst_vars('${PREFIX}/bin',env)
		if not env.LIBDIR:
			env.LIBDIR=Utils.subst_vars('${PREFIX}/lib',env)
	def store(self):
		n=self.cachedir.make_node('build.config.py')
		n.write('version = 0x%x\ntools = %r\n'%(Context.HEXVERSION,self.tools))
		if not self.all_envs:
			self.fatal('nothing to store in the configuration context!')
		for key in self.all_envs:
			tmpenv=self.all_envs[key]
			tmpenv.store(os.path.join(self.cachedir.abspath(),key+Build.CACHE_SUFFIX))
	def load(self,input,tooldir=None,funs=None,download=True):
		tools=Utils.to_list(input)
		if tooldir:tooldir=Utils.to_list(tooldir)
		for tool in tools:
			mag=(tool,id(self.env),funs)
			if mag in self.tool_cache:
				self.to_log('(tool %s is already loaded, skipping)'%tool)
				continue
			self.tool_cache.append(mag)
			module=None
			try:
				module=Context.load_tool(tool,tooldir)
			except ImportError ,e:
				if Options.options.download:
					module=download_tool(tool,ctx=self)
					if not module:
						self.fatal('Could not load the Waf tool %r or download a suitable replacement from the repository (sys.path %r)\n%s'%(tool,sys.path,e))
				else:
					self.fatal('Could not load the Waf tool %r from %r (try the --download option?):\n%s'%(tool,sys.path,e))
			except Exception ,e:
				self.to_log('imp %r (%r & %r)'%(tool,tooldir,funs))
				self.to_log(Utils.ex_stack())
				raise
			if funs is not None:
				self.eval_rules(funs)
			else:
				func=getattr(module,'configure',None)
				if func:
					if type(func)is type(Utils.readf):func(self)
					else:self.eval_rules(func)
			self.tools.append({'tool':tool,'tooldir':tooldir,'funs':funs})
	def post_recurse(self,node):
		super(ConfigurationContext,self).post_recurse(node)
		self.hash=hash((self.hash,node.read('rb')))
		self.files.append(node.abspath())
	def eval_rules(self,rules):
		self.rules=Utils.to_list(rules)
		for x in self.rules:
			f=getattr(self,x)
			if not f:self.fatal("No such method '%s'."%x)
			try:
				f()
			except Exception ,e:
				ret=self.err_handler(x,e)
				if ret==BREAK:
					break
				elif ret==CONTINUE:
					continue
				else:
					raise
	def err_handler(self,fun,error):
		pass
def conf(f):
	def fun(*k,**kw):
		mandatory=True
		if'mandatory'in kw:
			mandatory=kw['mandatory']
			del kw['mandatory']
		try:
			return f(*k,**kw)
		except Errors.ConfigurationError:
			if mandatory:
				raise
	setattr(ConfigurationContext,f.__name__,fun)
	setattr(Build.BuildContext,f.__name__,fun)
	return f
@conf
def add_os_flags(self,var,dest=None):
	try:self.env.append_value(dest or var,shlex.split(self.environ[var]))
	except KeyError:pass
@conf
def cmd_to_list(self,cmd):
	if isinstance(cmd,str)and cmd.find(' '):
		try:
			os.stat(cmd)
		except OSError:
			return shlex.split(cmd)
		else:
			return[cmd]
	return cmd
@conf
def check_waf_version(self,mini='1.6.99',maxi='1.8.0'):
	self.start_msg('Checking for waf version in %s-%s'%(str(mini),str(maxi)))
	ver=Context.HEXVERSION
	if Utils.num2ver(mini)>ver:
		self.fatal('waf version should be at least %r (%r found)'%(Utils.num2ver(mini),ver))
	if Utils.num2ver(maxi)<ver:
		self.fatal('waf version should be at most %r (%r found)'%(Utils.num2ver(maxi),ver))
	self.end_msg('ok')
@conf
def find_file(self,filename,path_list=[]):
	for n in Utils.to_list(filename):
		for d in Utils.to_list(path_list):
			p=os.path.join(d,n)
			if os.path.exists(p):
				return p
	self.fatal('Could not find %r'%filename)
@conf
def find_program(self,filename,**kw):
	exts=kw.get('exts',Utils.is_win32 and'.exe,.com,.bat,.cmd'or',.sh,.pl,.py')
	environ=kw.get('environ',os.environ)
	ret=''
	filename=Utils.to_list(filename)
	var=kw.get('var','')
	if not var:
		var=filename[0].upper()
	if self.env[var]:
		ret=self.env[var]
	elif var in environ:
		ret=environ[var]
	path_list=kw.get('path_list','')
	if not ret:
		if path_list:
			path_list=Utils.to_list(path_list)
		else:
			path_list=environ.get('PATH','').split(os.pathsep)
		if not isinstance(filename,list):
			filename=[filename]
		for a in exts.split(','):
			if ret:
				break
			for b in filename:
				if ret:
					break
				for c in path_list:
					if ret:
						break
					x=os.path.expanduser(os.path.join(c,b+a))
					if os.path.isfile(x):
						ret=x
	if not ret and Utils.winreg:
		ret=Utils.get_registry_app_path(Utils.winreg.HKEY_CURRENT_USER,filename)
	if not ret and Utils.winreg:
		ret=Utils.get_registry_app_path(Utils.winreg.HKEY_LOCAL_MACHINE,filename)
	self.msg('Checking for program '+','.join(filename),ret or False)
	self.to_log('find program=%r paths=%r var=%r -> %r'%(filename,path_list,var,ret))
	if not ret:
		self.fatal(kw.get('errmsg','')or'Could not find the program %s'%','.join(filename))
	if var:
		self.env[var]=ret
	return ret
@conf
def find_perl_program(self,filename,path_list=[],var=None,environ=None,exts=''):
	try:
		app=self.find_program(filename,path_list=path_list,var=var,environ=environ,exts=exts)
	except Exception:
		self.find_program('perl',var='PERL')
		app=self.find_file(filename,os.environ['PATH'].split(os.pathsep))
		if not app:
			raise
		if var:
			self.env[var]=Utils.to_list(self.env['PERL'])+[app]
	self.msg('Checking for %r'%filename,app)
