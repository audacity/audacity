#! /usr/bin/env python
# encoding: utf-8
# WARNING! Do not edit! http://waf.googlecode.com/git/docs/wafbook/single.html#_obtaining_the_waf_file

import os,sys
from waflib.TaskGen import feature,after_method
from waflib import Utils,Task,Logs,Options
testlock=Utils.threading.Lock()
@feature('test')
@after_method('apply_link')
def make_test(self):
	if getattr(self,'link_task',None):
		self.create_task('utest',self.link_task.outputs)
class utest(Task.Task):
	color='PINK'
	after=['vnum','inst']
	vars=[]
	def runnable_status(self):
		if getattr(Options.options,'no_tests',False):
			return Task.SKIP_ME
		ret=super(utest,self).runnable_status()
		if ret==Task.SKIP_ME:
			if getattr(Options.options,'all_tests',False):
				return Task.RUN_ME
		return ret
	def run(self):
		filename=self.inputs[0].abspath()
		self.ut_exec=getattr(self.generator,'ut_exec',[filename])
		if getattr(self.generator,'ut_fun',None):
			self.generator.ut_fun(self)
		try:
			fu=getattr(self.generator.bld,'all_test_paths')
		except AttributeError:
			fu=os.environ.copy()
			lst=[]
			for g in self.generator.bld.groups:
				for tg in g:
					if getattr(tg,'link_task',None):
						s=tg.link_task.outputs[0].parent.abspath()
						if s not in lst:
							lst.append(s)
			def add_path(dct,path,var):
				dct[var]=os.pathsep.join(Utils.to_list(path)+[os.environ.get(var,'')])
			if Utils.is_win32:
				add_path(fu,lst,'PATH')
			elif Utils.unversioned_sys_platform()=='darwin':
				add_path(fu,lst,'DYLD_LIBRARY_PATH')
				add_path(fu,lst,'LD_LIBRARY_PATH')
			else:
				add_path(fu,lst,'LD_LIBRARY_PATH')
			self.generator.bld.all_test_paths=fu
		cwd=getattr(self.generator,'ut_cwd','')or self.inputs[0].parent.abspath()
		testcmd=getattr(Options.options,'testcmd',False)
		if testcmd:
			self.ut_exec=(testcmd%self.ut_exec[0]).split(' ')
		proc=Utils.subprocess.Popen(self.ut_exec,cwd=cwd,env=fu,stderr=Utils.subprocess.PIPE,stdout=Utils.subprocess.PIPE)
		(stdout,stderr)=proc.communicate()
		tup=(filename,proc.returncode,stdout,stderr)
		self.generator.utest_result=tup
		testlock.acquire()
		try:
			bld=self.generator.bld
			Logs.debug("ut: %r",tup)
			try:
				bld.utest_results.append(tup)
			except AttributeError:
				bld.utest_results=[tup]
		finally:
			testlock.release()
def summary(bld):
	lst=getattr(bld,'utest_results',[])
	if lst:
		Logs.pprint('CYAN','execution summary')
		total=len(lst)
		tfail=len([x for x in lst if x[1]])
		Logs.pprint('CYAN','  tests that pass %d/%d'%(total-tfail,total))
		for(f,code,out,err)in lst:
			if not code:
				Logs.pprint('CYAN','    %s'%f)
		Logs.pprint('CYAN','  tests that fail %d/%d'%(tfail,total))
		for(f,code,out,err)in lst:
			if code:
				Logs.pprint('CYAN','    %s'%f)
def set_exit_code(bld):
	lst=getattr(bld,'utest_results',[])
	for(f,code,out,err)in lst:
		if code:
			msg=[]
			if out:
				msg.append('stdout:%s%s'%(os.linesep,out.decode('utf-8')))
			if err:
				msg.append('stderr:%s%s'%(os.linesep,err.decode('utf-8')))
			bld.fatal(os.linesep.join(msg))
def options(opt):
	opt.add_option('--notests',action='store_true',default=False,help='Exec no unit tests',dest='no_tests')
	opt.add_option('--alltests',action='store_true',default=False,help='Exec all unit tests',dest='all_tests')
	opt.add_option('--testcmd',action='store',default=False,help='Run the unit tests using the test-cmd string'' example "--test-cmd="valgrind --error-exitcode=1'' %s" to run under valgrind',dest='testcmd')
