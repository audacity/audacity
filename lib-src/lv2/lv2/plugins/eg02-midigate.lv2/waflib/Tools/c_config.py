#! /usr/bin/env python
# encoding: utf-8
# WARNING! Do not edit! http://waf.googlecode.com/git/docs/wafbook/single.html#_obtaining_the_waf_file

import os,re,shlex,sys
from waflib import Build,Utils,Task,Options,Logs,Errors,ConfigSet,Runner
from waflib.TaskGen import after_method,feature
from waflib.Configure import conf
WAF_CONFIG_H='config.h'
DEFKEYS='define_key'
INCKEYS='include_key'
cfg_ver={'atleast-version':'>=','exact-version':'==','max-version':'<=',}
SNIP_FUNCTION='''
int main(int argc, char **argv) {
	void *p;
	(void)argc; (void)argv;
	p=(void*)(%s);
	return 0;
}
'''
SNIP_TYPE='''
int main(int argc, char **argv) {
	(void)argc; (void)argv;
	if ((%(type_name)s *) 0) return 0;
	if (sizeof (%(type_name)s)) return 0;
	return 1;
}
'''
SNIP_EMPTY_PROGRAM='''
int main(int argc, char **argv) {
	(void)argc; (void)argv;
	return 0;
}
'''
SNIP_FIELD='''
int main(int argc, char **argv) {
	char *off;
	(void)argc; (void)argv;
	off = (char*) &((%(type_name)s*)0)->%(field_name)s;
	return (size_t) off < sizeof(%(type_name)s);
}
'''
MACRO_TO_DESTOS={'__linux__':'linux','__GNU__':'gnu','__FreeBSD__':'freebsd','__NetBSD__':'netbsd','__OpenBSD__':'openbsd','__sun':'sunos','__hpux':'hpux','__sgi':'irix','_AIX':'aix','__CYGWIN__':'cygwin','__MSYS__':'msys','_UWIN':'uwin','_WIN64':'win32','_WIN32':'win32','__ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__':'darwin','__ENVIRONMENT_IPHONE_OS_VERSION_MIN_REQUIRED__':'darwin','__QNX__':'qnx','__native_client__':'nacl'}
MACRO_TO_DEST_CPU={'__x86_64__':'x86_64','__amd64__':'x86_64','__i386__':'x86','__ia64__':'ia','__mips__':'mips','__sparc__':'sparc','__alpha__':'alpha','__aarch64__':'aarch64','__thumb__':'thumb','__arm__':'arm','__hppa__':'hppa','__powerpc__':'powerpc','__ppc__':'powerpc','__convex__':'convex','__m68k__':'m68k','__s390x__':'s390x','__s390__':'s390','__sh__':'sh',}
@conf
def parse_flags(self,line,uselib_store,env=None,force_static=False):
	assert(isinstance(line,str))
	env=env or self.env
	app=env.append_value
	appu=env.append_unique
	lex=shlex.shlex(line,posix=False)
	lex.whitespace_split=True
	lex.commenters=''
	lst=list(lex)
	uselib=uselib_store
	while lst:
		x=lst.pop(0)
		st=x[:2]
		ot=x[2:]
		if st=='-I'or st=='/I':
			if not ot:ot=lst.pop(0)
			appu('INCLUDES_'+uselib,[ot])
		elif st=='-include':
			tmp=[x,lst.pop(0)]
			app('CFLAGS',tmp)
			app('CXXFLAGS',tmp)
		elif st=='-D'or(env.CXX_NAME=='msvc'and st=='/D'):
			if not ot:ot=lst.pop(0)
			app('DEFINES_'+uselib,[ot])
		elif st=='-l':
			if not ot:ot=lst.pop(0)
			prefix=force_static and'STLIB_'or'LIB_'
			appu(prefix+uselib,[ot])
		elif st=='-L':
			if not ot:ot=lst.pop(0)
			appu('LIBPATH_'+uselib,[ot])
		elif x.startswith('/LIBPATH:'):
			appu('LIBPATH_'+uselib,[x.replace('/LIBPATH:','')])
		elif x=='-pthread'or x.startswith('+')or x.startswith('-std'):
			app('CFLAGS_'+uselib,[x])
			app('CXXFLAGS_'+uselib,[x])
			app('LINKFLAGS_'+uselib,[x])
		elif x=='-framework':
			appu('FRAMEWORK_'+uselib,[lst.pop(0)])
		elif x.startswith('-F'):
			appu('FRAMEWORKPATH_'+uselib,[x[2:]])
		elif x.startswith('-Wl'):
			app('LINKFLAGS_'+uselib,[x])
		elif x.startswith('-m')or x.startswith('-f')or x.startswith('-dynamic'):
			app('CFLAGS_'+uselib,[x])
			app('CXXFLAGS_'+uselib,[x])
		elif x.startswith('-bundle'):
			app('LINKFLAGS_'+uselib,[x])
		elif x.startswith('-undefined'):
			arg=lst.pop(0)
			app('LINKFLAGS_'+uselib,[x,arg])
		elif x.startswith('-arch')or x.startswith('-isysroot'):
			tmp=[x,lst.pop(0)]
			app('CFLAGS_'+uselib,tmp)
			app('CXXFLAGS_'+uselib,tmp)
			app('LINKFLAGS_'+uselib,tmp)
		elif x.endswith('.a')or x.endswith('.so')or x.endswith('.dylib')or x.endswith('.lib'):
			appu('LINKFLAGS_'+uselib,[x])
@conf
def ret_msg(self,f,kw):
	if isinstance(f,str):
		return f
	return f(kw)
@conf
def validate_cfg(self,kw):
	if not'path'in kw:
		if not self.env.PKGCONFIG:
			self.find_program('pkg-config',var='PKGCONFIG')
		kw['path']=self.env.PKGCONFIG
	if'atleast_pkgconfig_version'in kw:
		if not'msg'in kw:
			kw['msg']='Checking for pkg-config version >= %r'%kw['atleast_pkgconfig_version']
		return
	if not'okmsg'in kw:
		kw['okmsg']='yes'
	if not'errmsg'in kw:
		kw['errmsg']='not found'
	if'modversion'in kw:
		if not'msg'in kw:
			kw['msg']='Checking for %r version'%kw['modversion']
		return
	for x in cfg_ver.keys():
		y=x.replace('-','_')
		if y in kw:
			if not'package'in kw:
				raise ValueError('%s requires a package'%x)
			if not'msg'in kw:
				kw['msg']='Checking for %r %s %s'%(kw['package'],cfg_ver[x],kw[y])
			return
	if not'msg'in kw:
		kw['msg']='Checking for %r'%(kw['package']or kw['path'])
@conf
def exec_cfg(self,kw):
	def define_it():
		self.define(self.have_define(kw.get('uselib_store',kw['package'])),1,0)
	if'atleast_pkgconfig_version'in kw:
		cmd=[kw['path'],'--atleast-pkgconfig-version=%s'%kw['atleast_pkgconfig_version']]
		self.cmd_and_log(cmd)
		if not'okmsg'in kw:
			kw['okmsg']='yes'
		return
	for x in cfg_ver:
		y=x.replace('-','_')
		if y in kw:
			self.cmd_and_log([kw['path'],'--%s=%s'%(x,kw[y]),kw['package']])
			if not'okmsg'in kw:
				kw['okmsg']='yes'
			define_it()
			break
	if'modversion'in kw:
		version=self.cmd_and_log([kw['path'],'--modversion',kw['modversion']]).strip()
		self.define('%s_VERSION'%Utils.quote_define_name(kw.get('uselib_store',kw['modversion'])),version)
		return version
	lst=[kw['path']]
	defi=kw.get('define_variable',None)
	if not defi:
		defi=self.env.PKG_CONFIG_DEFINES or{}
	for key,val in defi.items():
		lst.append('--define-variable=%s=%s'%(key,val))
	if'variables'in kw:
		env=kw.get('env',self.env)
		uselib=kw.get('uselib_store',kw['package'].upper())
		vars=Utils.to_list(kw['variables'])
		for v in vars:
			val=self.cmd_and_log(lst+['--variable='+v]).strip()
			var='%s_%s'%(uselib,v)
			env[var]=val
		if not'okmsg'in kw:
			kw['okmsg']='yes'
		return
	static=False
	if'args'in kw:
		args=Utils.to_list(kw['args'])
		if'--static'in args or'--static-libs'in args:
			static=True
		lst+=args
	lst.extend(Utils.to_list(kw['package']))
	ret=self.cmd_and_log(lst)
	if not'okmsg'in kw:
		kw['okmsg']='yes'
	define_it()
	self.parse_flags(ret,kw.get('uselib_store',kw['package'].upper()),kw.get('env',self.env),force_static=static)
	return ret
@conf
def check_cfg(self,*k,**kw):
	if k:
		lst=k[0].split()
		kw['package']=lst[0]
		kw['args']=' '.join(lst[1:])
	self.validate_cfg(kw)
	if'msg'in kw:
		self.start_msg(kw['msg'])
	ret=None
	try:
		ret=self.exec_cfg(kw)
	except self.errors.WafError:
		if'errmsg'in kw:
			self.end_msg(kw['errmsg'],'YELLOW')
		if Logs.verbose>1:
			raise
		else:
			self.fatal('The configuration failed')
	else:
		kw['success']=ret
		if'okmsg'in kw:
			self.end_msg(self.ret_msg(kw['okmsg'],kw))
	return ret
@conf
def validate_c(self,kw):
	if not'env'in kw:
		kw['env']=self.env.derive()
	env=kw['env']
	if not'compiler'in kw and not'features'in kw:
		kw['compiler']='c'
		if env['CXX_NAME']and Task.classes.get('cxx',None):
			kw['compiler']='cxx'
			if not self.env['CXX']:
				self.fatal('a c++ compiler is required')
		else:
			if not self.env['CC']:
				self.fatal('a c compiler is required')
	if not'compile_mode'in kw:
		kw['compile_mode']='c'
		if'cxx'in Utils.to_list(kw.get('features',[]))or kw.get('compiler','')=='cxx':
			kw['compile_mode']='cxx'
	if not'type'in kw:
		kw['type']='cprogram'
	if not'features'in kw:
		kw['features']=[kw['compile_mode'],kw['type']]
	else:
		kw['features']=Utils.to_list(kw['features'])
	if not'compile_filename'in kw:
		kw['compile_filename']='test.c'+((kw['compile_mode']=='cxx')and'pp'or'')
	def to_header(dct):
		if'header_name'in dct:
			dct=Utils.to_list(dct['header_name'])
			return''.join(['#include <%s>\n'%x for x in dct])
		return''
	if'framework_name'in kw:
		fwkname=kw['framework_name']
		if not'uselib_store'in kw:
			kw['uselib_store']=fwkname.upper()
		if not kw.get('no_header',False):
			if not'header_name'in kw:
				kw['header_name']=[]
			fwk='%s/%s.h'%(fwkname,fwkname)
			if kw.get('remove_dot_h',None):
				fwk=fwk[:-2]
			kw['header_name']=Utils.to_list(kw['header_name'])+[fwk]
		kw['msg']='Checking for framework %s'%fwkname
		kw['framework']=fwkname
	if'function_name'in kw:
		fu=kw['function_name']
		if not'msg'in kw:
			kw['msg']='Checking for function %s'%fu
		kw['code']=to_header(kw)+SNIP_FUNCTION%fu
		if not'uselib_store'in kw:
			kw['uselib_store']=fu.upper()
		if not'define_name'in kw:
			kw['define_name']=self.have_define(fu)
	elif'type_name'in kw:
		tu=kw['type_name']
		if not'header_name'in kw:
			kw['header_name']='stdint.h'
		if'field_name'in kw:
			field=kw['field_name']
			kw['code']=to_header(kw)+SNIP_FIELD%{'type_name':tu,'field_name':field}
			if not'msg'in kw:
				kw['msg']='Checking for field %s in %s'%(field,tu)
			if not'define_name'in kw:
				kw['define_name']=self.have_define((tu+'_'+field).upper())
		else:
			kw['code']=to_header(kw)+SNIP_TYPE%{'type_name':tu}
			if not'msg'in kw:
				kw['msg']='Checking for type %s'%tu
			if not'define_name'in kw:
				kw['define_name']=self.have_define(tu.upper())
	elif'header_name'in kw:
		if not'msg'in kw:
			kw['msg']='Checking for header %s'%kw['header_name']
		l=Utils.to_list(kw['header_name'])
		assert len(l)>0,'list of headers in header_name is empty'
		kw['code']=to_header(kw)+SNIP_EMPTY_PROGRAM
		if not'uselib_store'in kw:
			kw['uselib_store']=l[0].upper()
		if not'define_name'in kw:
			kw['define_name']=self.have_define(l[0])
	if'lib'in kw:
		if not'msg'in kw:
			kw['msg']='Checking for library %s'%kw['lib']
		if not'uselib_store'in kw:
			kw['uselib_store']=kw['lib'].upper()
	if'stlib'in kw:
		if not'msg'in kw:
			kw['msg']='Checking for static library %s'%kw['stlib']
		if not'uselib_store'in kw:
			kw['uselib_store']=kw['stlib'].upper()
	if'fragment'in kw:
		kw['code']=kw['fragment']
		if not'msg'in kw:
			kw['msg']='Checking for code snippet'
		if not'errmsg'in kw:
			kw['errmsg']='no'
	for(flagsname,flagstype)in[('cxxflags','compiler'),('cflags','compiler'),('linkflags','linker')]:
		if flagsname in kw:
			if not'msg'in kw:
				kw['msg']='Checking for %s flags %s'%(flagstype,kw[flagsname])
			if not'errmsg'in kw:
				kw['errmsg']='no'
	if not'execute'in kw:
		kw['execute']=False
	if kw['execute']:
		kw['features'].append('test_exec')
	if not'errmsg'in kw:
		kw['errmsg']='not found'
	if not'okmsg'in kw:
		kw['okmsg']='yes'
	if not'code'in kw:
		kw['code']=SNIP_EMPTY_PROGRAM
	if self.env[INCKEYS]:
		kw['code']='\n'.join(['#include <%s>'%x for x in self.env[INCKEYS]])+'\n'+kw['code']
	if not kw.get('success'):kw['success']=None
	if'define_name'in kw:
		self.undefine(kw['define_name'])
	assert'msg'in kw,'invalid parameters, read http://freehackers.org/~tnagy/wafbook/single.html#config_helpers_c'
@conf
def post_check(self,*k,**kw):
	is_success=0
	if kw['execute']:
		if kw['success']is not None:
			if kw.get('define_ret',False):
				is_success=kw['success']
			else:
				is_success=(kw['success']==0)
	else:
		is_success=(kw['success']==0)
	if'define_name'in kw:
		if'header_name'in kw or'function_name'in kw or'type_name'in kw or'fragment'in kw:
			if kw['execute']and kw.get('define_ret',None)and isinstance(is_success,str):
				self.define(kw['define_name'],is_success,quote=kw.get('quote',1))
			else:
				self.define_cond(kw['define_name'],is_success)
		else:
			self.define_cond(kw['define_name'],is_success)
	if'header_name'in kw:
		if kw.get('auto_add_header_name',False):
			self.env.append_value(INCKEYS,Utils.to_list(kw['header_name']))
	if is_success and'uselib_store'in kw:
		from waflib.Tools import ccroot
		_vars=set([])
		for x in kw['features']:
			if x in ccroot.USELIB_VARS:
				_vars|=ccroot.USELIB_VARS[x]
		for k in _vars:
			lk=k.lower()
			if k=='INCLUDES':lk='includes'
			if k=='DEFINES':lk='defines'
			if lk in kw:
				val=kw[lk]
				if isinstance(val,str):
					val=val.rstrip(os.path.sep)
				self.env.append_unique(k+'_'+kw['uselib_store'],val)
	return is_success
@conf
def check(self,*k,**kw):
	self.validate_c(kw)
	self.start_msg(kw['msg'])
	ret=None
	try:
		ret=self.run_c_code(*k,**kw)
	except self.errors.ConfigurationError:
		self.end_msg(kw['errmsg'],'YELLOW')
		if Logs.verbose>1:
			raise
		else:
			self.fatal('The configuration failed')
	else:
		kw['success']=ret
	ret=self.post_check(*k,**kw)
	if not ret:
		self.end_msg(kw['errmsg'],'YELLOW')
		self.fatal('The configuration failed %r'%ret)
	else:
		self.end_msg(self.ret_msg(kw['okmsg'],kw))
	return ret
class test_exec(Task.Task):
	color='PINK'
	def run(self):
		if getattr(self.generator,'rpath',None):
			if getattr(self.generator,'define_ret',False):
				self.generator.bld.retval=self.generator.bld.cmd_and_log([self.inputs[0].abspath()])
			else:
				self.generator.bld.retval=self.generator.bld.exec_command([self.inputs[0].abspath()])
		else:
			env=self.env.env or{}
			env.update(dict(os.environ))
			for var in('LD_LIBRARY_PATH','DYLD_LIBRARY_PATH','PATH'):
				env[var]=self.inputs[0].parent.abspath()+os.path.pathsep+env.get(var,'')
			if getattr(self.generator,'define_ret',False):
				self.generator.bld.retval=self.generator.bld.cmd_and_log([self.inputs[0].abspath()],env=env)
			else:
				self.generator.bld.retval=self.generator.bld.exec_command([self.inputs[0].abspath()],env=env)
@feature('test_exec')
@after_method('apply_link')
def test_exec_fun(self):
	self.create_task('test_exec',self.link_task.outputs[0])
CACHE_RESULTS=1
COMPILE_ERRORS=2
@conf
def run_c_code(self,*k,**kw):
	lst=[str(v)for(p,v)in kw.items()if p!='env']
	h=Utils.h_list(lst)
	dir=self.bldnode.abspath()+os.sep+(not Utils.is_win32 and'.'or'')+'conf_check_'+Utils.to_hex(h)
	try:
		os.makedirs(dir)
	except OSError:
		pass
	try:
		os.stat(dir)
	except OSError:
		self.fatal('cannot use the configuration test folder %r'%dir)
	cachemode=getattr(Options.options,'confcache',None)
	if cachemode==CACHE_RESULTS:
		try:
			proj=ConfigSet.ConfigSet(os.path.join(dir,'cache_run_c_code'))
		except OSError:
			pass
		else:
			ret=proj['cache_run_c_code']
			if isinstance(ret,str)and ret.startswith('Test does not build'):
				self.fatal(ret)
			return ret
	bdir=os.path.join(dir,'testbuild')
	if not os.path.exists(bdir):
		os.makedirs(bdir)
	self.test_bld=bld=Build.BuildContext(top_dir=dir,out_dir=bdir)
	bld.init_dirs()
	bld.progress_bar=0
	bld.targets='*'
	if kw['compile_filename']:
		node=bld.srcnode.make_node(kw['compile_filename'])
		node.write(kw['code'])
	bld.logger=self.logger
	bld.all_envs.update(self.all_envs)
	bld.env=kw['env']
	o=bld(features=kw['features'],source=kw['compile_filename'],target='testprog')
	for k,v in kw.items():
		setattr(o,k,v)
	self.to_log("==>\n%s\n<=="%kw['code'])
	bld.targets='*'
	ret=-1
	try:
		try:
			bld.compile()
		except Errors.WafError:
			ret='Test does not build: %s'%Utils.ex_stack()
			self.fatal(ret)
		else:
			ret=getattr(bld,'retval',0)
	finally:
		proj=ConfigSet.ConfigSet()
		proj['cache_run_c_code']=ret
		proj.store(os.path.join(dir,'cache_run_c_code'))
	return ret
@conf
def check_cxx(self,*k,**kw):
	kw['compiler']='cxx'
	return self.check(*k,**kw)
@conf
def check_cc(self,*k,**kw):
	kw['compiler']='c'
	return self.check(*k,**kw)
@conf
def define(self,key,val,quote=True):
	assert key and isinstance(key,str)
	if val is True:
		val=1
	elif val in(False,None):
		val=0
	if isinstance(val,int)or isinstance(val,float):
		s='%s=%s'
	else:
		s=quote and'%s="%s"'or'%s=%s'
	app=s%(key,str(val))
	ban=key+'='
	lst=self.env['DEFINES']
	for x in lst:
		if x.startswith(ban):
			lst[lst.index(x)]=app
			break
	else:
		self.env.append_value('DEFINES',app)
	self.env.append_unique(DEFKEYS,key)
@conf
def undefine(self,key):
	assert key and isinstance(key,str)
	ban=key+'='
	lst=[x for x in self.env['DEFINES']if not x.startswith(ban)]
	self.env['DEFINES']=lst
	self.env.append_unique(DEFKEYS,key)
@conf
def define_cond(self,key,val):
	assert key and isinstance(key,str)
	if val:
		self.define(key,1)
	else:
		self.undefine(key)
@conf
def is_defined(self,key):
	assert key and isinstance(key,str)
	ban=key+'='
	for x in self.env['DEFINES']:
		if x.startswith(ban):
			return True
	return False
@conf
def get_define(self,key):
	assert key and isinstance(key,str)
	ban=key+'='
	for x in self.env['DEFINES']:
		if x.startswith(ban):
			return x[len(ban):]
	return None
@conf
def have_define(self,key):
	return(self.env.HAVE_PAT or'HAVE_%s')%Utils.quote_define_name(key)
@conf
def write_config_header(self,configfile='',guard='',top=False,env=None,defines=True,headers=False,remove=True,define_prefix=''):
	if env:
		Logs.warn('Cannot pass env to write_config_header')
	if not configfile:configfile=WAF_CONFIG_H
	waf_guard=guard or'W_%s_WAF'%Utils.quote_define_name(configfile)
	node=top and self.bldnode or self.path.get_bld()
	node=node.make_node(configfile)
	node.parent.mkdir()
	lst=['/* WARNING! All changes made to this file will be lost! */\n']
	lst.append('#ifndef %s\n#define %s\n'%(waf_guard,waf_guard))
	lst.append(self.get_config_header(defines,headers,define_prefix=define_prefix))
	lst.append('\n#endif /* %s */\n'%waf_guard)
	node.write('\n'.join(lst))
	self.env.append_unique(Build.CFG_FILES,[node.abspath()])
	if remove:
		for key in self.env[DEFKEYS]:
			self.undefine(key)
		self.env[DEFKEYS]=[]
@conf
def get_config_header(self,defines=True,headers=False,define_prefix=''):
	lst=[]
	if headers:
		for x in self.env[INCKEYS]:
			lst.append('#include <%s>'%x)
	if defines:
		for x in self.env[DEFKEYS]:
			if self.is_defined(x):
				val=self.get_define(x)
				lst.append('#define %s %s'%(define_prefix+x,val))
			else:
				lst.append('/* #undef %s */'%(define_prefix+x))
	return"\n".join(lst)
@conf
def cc_add_flags(conf):
	conf.add_os_flags('CPPFLAGS','CFLAGS')
	conf.add_os_flags('CFLAGS')
@conf
def cxx_add_flags(conf):
	conf.add_os_flags('CPPFLAGS','CXXFLAGS')
	conf.add_os_flags('CXXFLAGS')
@conf
def link_add_flags(conf):
	conf.add_os_flags('LINKFLAGS')
	conf.add_os_flags('LDFLAGS','LINKFLAGS')
@conf
def cc_load_tools(conf):
	if not conf.env.DEST_OS:
		conf.env.DEST_OS=Utils.unversioned_sys_platform()
	conf.load('c')
@conf
def cxx_load_tools(conf):
	if not conf.env.DEST_OS:
		conf.env.DEST_OS=Utils.unversioned_sys_platform()
	conf.load('cxx')
@conf
def get_cc_version(conf,cc,gcc=False,icc=False):
	cmd=cc+['-dM','-E','-']
	env=conf.env.env or None
	try:
		p=Utils.subprocess.Popen(cmd,stdin=Utils.subprocess.PIPE,stdout=Utils.subprocess.PIPE,stderr=Utils.subprocess.PIPE,env=env)
		p.stdin.write('\n')
		out=p.communicate()[0]
	except Exception:
		conf.fatal('Could not determine the compiler version %r'%cmd)
	if not isinstance(out,str):
		out=out.decode(sys.stdout.encoding or'iso8859-1')
	if gcc:
		if out.find('__INTEL_COMPILER')>=0:
			conf.fatal('The intel compiler pretends to be gcc')
		if out.find('__GNUC__')<0 and out.find('__clang__')<0:
			conf.fatal('Could not determine the compiler type')
	if icc and out.find('__INTEL_COMPILER')<0:
		conf.fatal('Not icc/icpc')
	k={}
	if icc or gcc:
		out=out.splitlines()
		for line in out:
			lst=shlex.split(line)
			if len(lst)>2:
				key=lst[1]
				val=lst[2]
				k[key]=val
		def isD(var):
			return var in k
		def isT(var):
			return var in k and k[var]!='0'
		if not conf.env.DEST_OS:
			conf.env.DEST_OS=''
		for i in MACRO_TO_DESTOS:
			if isD(i):
				conf.env.DEST_OS=MACRO_TO_DESTOS[i]
				break
		else:
			if isD('__APPLE__')and isD('__MACH__'):
				conf.env.DEST_OS='darwin'
			elif isD('__unix__'):
				conf.env.DEST_OS='generic'
		if isD('__ELF__'):
			conf.env.DEST_BINFMT='elf'
		elif isD('__WINNT__')or isD('__CYGWIN__')or isD('_WIN32'):
			conf.env.DEST_BINFMT='pe'
			conf.env.LIBDIR=conf.env.BINDIR
		elif isD('__APPLE__'):
			conf.env.DEST_BINFMT='mac-o'
		if not conf.env.DEST_BINFMT:
			conf.env.DEST_BINFMT=Utils.destos_to_binfmt(conf.env.DEST_OS)
		for i in MACRO_TO_DEST_CPU:
			if isD(i):
				conf.env.DEST_CPU=MACRO_TO_DEST_CPU[i]
				break
		Logs.debug('ccroot: dest platform: '+' '.join([conf.env[x]or'?'for x in('DEST_OS','DEST_BINFMT','DEST_CPU')]))
		if icc:
			ver=k['__INTEL_COMPILER']
			conf.env['CC_VERSION']=(ver[:-2],ver[-2],ver[-1])
		else:
			if isD('__clang__'):
				conf.env['CC_VERSION']=(k['__clang_major__'],k['__clang_minor__'],k['__clang_patchlevel__'])
			else:
				conf.env['CC_VERSION']=(k['__GNUC__'],k['__GNUC_MINOR__'],k['__GNUC_PATCHLEVEL__'])
	return k
@conf
def get_xlc_version(conf,cc):
	cmd=cc+['-qversion']
	try:
		out,err=conf.cmd_and_log(cmd,output=0)
	except Errors.WafError:
		conf.fatal('Could not find xlc %r'%cmd)
	for v in(r"IBM XL C/C\+\+.* V(?P<major>\d*)\.(?P<minor>\d*)",):
		version_re=re.compile(v,re.I).search
		match=version_re(out or err)
		if match:
			k=match.groupdict()
			conf.env['CC_VERSION']=(k['major'],k['minor'])
			break
	else:
		conf.fatal('Could not determine the XLC version.')
@conf
def add_as_needed(self):
	if self.env.DEST_BINFMT=='elf'and'gcc'in(self.env.CXX_NAME,self.env.CC_NAME):
		self.env.append_unique('LINKFLAGS','--as-needed')
class cfgtask(Task.TaskBase):
	def display(self):
		return''
	def runnable_status(self):
		return Task.RUN_ME
	def uid(self):
		return Utils.SIG_NIL
	def run(self):
		conf=self.conf
		bld=Build.BuildContext(top_dir=conf.srcnode.abspath(),out_dir=conf.bldnode.abspath())
		bld.env=conf.env
		bld.init_dirs()
		bld.in_msg=1
		bld.logger=self.logger
		try:
			bld.check(**self.args)
		except Exception:
			return 1
@conf
def multicheck(self,*k,**kw):
	self.start_msg(kw.get('msg','Executing %d configuration tests'%len(k)))
	class par(object):
		def __init__(self):
			self.keep=False
			self.cache_global=Options.cache_global
			self.nocache=Options.options.nocache
			self.returned_tasks=[]
			self.task_sigs={}
		def total(self):
			return len(tasks)
		def to_log(self,*k,**kw):
			return
	bld=par()
	tasks=[]
	for dct in k:
		x=cfgtask(bld=bld)
		tasks.append(x)
		x.args=dct
		x.bld=bld
		x.conf=self
		x.args=dct
		x.logger=Logs.make_mem_logger(str(id(x)),self.logger)
	def it():
		yield tasks
		while 1:
			yield[]
	p=Runner.Parallel(bld,Options.options.jobs)
	p.biter=it()
	p.start()
	for x in tasks:
		x.logger.memhandler.flush()
	for x in tasks:
		if x.hasrun!=Task.SUCCESS:
			self.end_msg(kw.get('errmsg','no'),color='YELLOW')
			self.fatal(kw.get('fatalmsg',None)or'One of the tests has failed, see the config.log for more information')
	self.end_msg('ok')
