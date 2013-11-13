#! /usr/bin/env python
# encoding: utf-8
# WARNING! Do not edit! http://waf.googlecode.com/git/docs/wafbook/single.html#_obtaining_the_waf_file

import os,imp,sys
from waflib import Utils,Errors,Logs
import waflib.Node
HEXVERSION=0x1070b00
WAFVERSION="1.7.11"
WAFREVISION="50f631bc5e00bdda966c68094229b99be9a21084"
ABI=98
DBFILE='.wafpickle-%s-%d-%d'%(sys.platform,sys.hexversion,ABI)
APPNAME='APPNAME'
VERSION='VERSION'
TOP='top'
OUT='out'
WSCRIPT_FILE='wscript'
launch_dir=''
run_dir=''
top_dir=''
out_dir=''
waf_dir=''
local_repo=''
remote_repo='http://waf.googlecode.com/git/'
remote_locs=['waflib/extras','waflib/Tools']
g_module=None
STDOUT=1
STDERR=-1
BOTH=0
classes=[]
def create_context(cmd_name,*k,**kw):
	global classes
	for x in classes:
		if x.cmd==cmd_name:
			return x(*k,**kw)
	ctx=Context(*k,**kw)
	ctx.fun=cmd_name
	return ctx
class store_context(type):
	def __init__(cls,name,bases,dict):
		super(store_context,cls).__init__(name,bases,dict)
		name=cls.__name__
		if name=='ctx'or name=='Context':
			return
		try:
			cls.cmd
		except AttributeError:
			raise Errors.WafError('Missing command for the context class %r (cmd)'%name)
		if not getattr(cls,'fun',None):
			cls.fun=cls.cmd
		global classes
		classes.insert(0,cls)
ctx=store_context('ctx',(object,),{})
class Context(ctx):
	errors=Errors
	tools={}
	def __init__(self,**kw):
		try:
			rd=kw['run_dir']
		except KeyError:
			global run_dir
			rd=run_dir
		class node_class(waflib.Node.Node):
			pass
		self.node_class=node_class
		self.node_class.__module__="waflib.Node"
		self.node_class.__name__="Nod3"
		self.node_class.ctx=self
		self.root=self.node_class('',None)
		self.cur_script=None
		self.path=self.root.find_dir(rd)
		self.stack_path=[]
		self.exec_dict={'ctx':self,'conf':self,'bld':self,'opt':self}
		self.logger=None
	def __hash__(self):
		return id(self)
	def load(self,tool_list,*k,**kw):
		tools=Utils.to_list(tool_list)
		path=Utils.to_list(kw.get('tooldir',''))
		for t in tools:
			module=load_tool(t,path)
			fun=getattr(module,kw.get('name',self.fun),None)
			if fun:
				fun(self)
	def execute(self):
		global g_module
		self.recurse([os.path.dirname(g_module.root_path)])
	def pre_recurse(self,node):
		self.stack_path.append(self.cur_script)
		self.cur_script=node
		self.path=node.parent
	def post_recurse(self,node):
		self.cur_script=self.stack_path.pop()
		if self.cur_script:
			self.path=self.cur_script.parent
	def recurse(self,dirs,name=None,mandatory=True,once=True):
		try:
			cache=self.recurse_cache
		except AttributeError:
			cache=self.recurse_cache={}
		for d in Utils.to_list(dirs):
			if not os.path.isabs(d):
				d=os.path.join(self.path.abspath(),d)
			WSCRIPT=os.path.join(d,WSCRIPT_FILE)
			WSCRIPT_FUN=WSCRIPT+'_'+(name or self.fun)
			node=self.root.find_node(WSCRIPT_FUN)
			if node and(not once or node not in cache):
				cache[node]=True
				self.pre_recurse(node)
				try:
					function_code=node.read('rU')
					exec(compile(function_code,node.abspath(),'exec'),self.exec_dict)
				finally:
					self.post_recurse(node)
			elif not node:
				node=self.root.find_node(WSCRIPT)
				tup=(node,name or self.fun)
				if node and(not once or tup not in cache):
					cache[tup]=True
					self.pre_recurse(node)
					try:
						wscript_module=load_module(node.abspath())
						user_function=getattr(wscript_module,(name or self.fun),None)
						if not user_function:
							if not mandatory:
								continue
							raise Errors.WafError('No function %s defined in %s'%(name or self.fun,node.abspath()))
						user_function(self)
					finally:
						self.post_recurse(node)
				elif not node:
					if not mandatory:
						continue
					raise Errors.WafError('No wscript file in directory %s'%d)
	def exec_command(self,cmd,**kw):
		subprocess=Utils.subprocess
		kw['shell']=isinstance(cmd,str)
		Logs.debug('runner: %r'%cmd)
		Logs.debug('runner_env: kw=%s'%kw)
		if self.logger:
			self.logger.info(cmd)
		if'stdout'not in kw:
			kw['stdout']=subprocess.PIPE
		if'stderr'not in kw:
			kw['stderr']=subprocess.PIPE
		try:
			if kw['stdout']or kw['stderr']:
				p=subprocess.Popen(cmd,**kw)
				(out,err)=p.communicate()
				ret=p.returncode
			else:
				out,err=(None,None)
				ret=subprocess.Popen(cmd,**kw).wait()
		except Exception ,e:
			raise Errors.WafError('Execution failure: %s'%str(e),ex=e)
		if out:
			if not isinstance(out,str):
				out=out.decode(sys.stdout.encoding or'iso8859-1')
			if self.logger:
				self.logger.debug('out: %s'%out)
			else:
				sys.stdout.write(out)
		if err:
			if not isinstance(err,str):
				err=err.decode(sys.stdout.encoding or'iso8859-1')
			if self.logger:
				self.logger.error('err: %s'%err)
			else:
				sys.stderr.write(err)
		return ret
	def cmd_and_log(self,cmd,**kw):
		subprocess=Utils.subprocess
		kw['shell']=isinstance(cmd,str)
		Logs.debug('runner: %r'%cmd)
		if'quiet'in kw:
			quiet=kw['quiet']
			del kw['quiet']
		else:
			quiet=None
		if'output'in kw:
			to_ret=kw['output']
			del kw['output']
		else:
			to_ret=STDOUT
		kw['stdout']=kw['stderr']=subprocess.PIPE
		if quiet is None:
			self.to_log(cmd)
		try:
			p=subprocess.Popen(cmd,**kw)
			(out,err)=p.communicate()
		except Exception ,e:
			raise Errors.WafError('Execution failure: %s'%str(e),ex=e)
		if not isinstance(out,str):
			out=out.decode(sys.stdout.encoding or'iso8859-1')
		if not isinstance(err,str):
			err=err.decode(sys.stdout.encoding or'iso8859-1')
		if out and quiet!=STDOUT and quiet!=BOTH:
			self.to_log('out: %s'%out)
		if err and quiet!=STDERR and quiet!=BOTH:
			self.to_log('err: %s'%err)
		if p.returncode:
			e=Errors.WafError('Command %r returned %r'%(cmd,p.returncode))
			e.returncode=p.returncode
			e.stderr=err
			e.stdout=out
			raise e
		if to_ret==BOTH:
			return(out,err)
		elif to_ret==STDERR:
			return err
		return out
	def fatal(self,msg,ex=None):
		if self.logger:
			self.logger.info('from %s: %s'%(self.path.abspath(),msg))
		try:
			msg='%s\n(complete log in %s)'%(msg,self.logger.handlers[0].baseFilename)
		except Exception:
			pass
		raise self.errors.ConfigurationError(msg,ex=ex)
	def to_log(self,msg):
		if not msg:
			return
		if self.logger:
			self.logger.info(msg)
		else:
			sys.stderr.write(str(msg))
			sys.stderr.flush()
	def msg(self,msg,result,color=None):
		self.start_msg(msg)
		if not isinstance(color,str):
			color=result and'GREEN'or'YELLOW'
		self.end_msg(result,color)
	def start_msg(self,msg):
		try:
			if self.in_msg:
				self.in_msg+=1
				return
		except AttributeError:
			self.in_msg=0
		self.in_msg+=1
		try:
			self.line_just=max(self.line_just,len(msg))
		except AttributeError:
			self.line_just=max(40,len(msg))
		for x in(self.line_just*'-',msg):
			self.to_log(x)
		Logs.pprint('NORMAL',"%s :"%msg.ljust(self.line_just),sep='')
	def end_msg(self,result,color=None):
		self.in_msg-=1
		if self.in_msg:
			return
		defcolor='GREEN'
		if result==True:
			msg='ok'
		elif result==False:
			msg='not found'
			defcolor='YELLOW'
		else:
			msg=str(result)
		self.to_log(msg)
		Logs.pprint(color or defcolor,msg)
	def load_special_tools(self,var,ban=[]):
		global waf_dir
		lst=self.root.find_node(waf_dir).find_node('waflib/extras').ant_glob(var)
		for x in lst:
			if not x.name in ban:
				load_tool(x.name.replace('.py',''))
cache_modules={}
def load_module(path):
	try:
		return cache_modules[path]
	except KeyError:
		pass
	module=imp.new_module(WSCRIPT_FILE)
	try:
		code=Utils.readf(path,m='rU')
	except(IOError,OSError):
		raise Errors.WafError('Could not read the file %r'%path)
	module_dir=os.path.dirname(path)
	sys.path.insert(0,module_dir)
	exec(compile(code,path,'exec'),module.__dict__)
	sys.path.remove(module_dir)
	cache_modules[path]=module
	return module
def load_tool(tool,tooldir=None):
	if tool=='java':
		tool='javaw'
	elif tool=='compiler_cc':
		tool='compiler_c'
	else:
		tool=tool.replace('++','xx')
	if tooldir:
		assert isinstance(tooldir,list)
		sys.path=tooldir+sys.path
		try:
			__import__(tool)
			ret=sys.modules[tool]
			Context.tools[tool]=ret
			return ret
		finally:
			for d in tooldir:
				sys.path.remove(d)
	else:
		global waf_dir
		try:
			os.stat(os.path.join(waf_dir,'waflib','extras',tool+'.py'))
		except OSError:
			try:
				os.stat(os.path.join(waf_dir,'waflib','Tools',tool+'.py'))
			except OSError:
				d=tool
			else:
				d='waflib.Tools.%s'%tool
		else:
			d='waflib.extras.%s'%tool
		__import__(d)
		ret=sys.modules[d]
		Context.tools[tool]=ret
		return ret
