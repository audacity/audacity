#! /usr/bin/env python
# encoding: utf-8
# Thomas Nagy, 2017-2018 (ita)

"""
A system for fast partial rebuilds

Creating a large amount of task objects up front can take some time.
By making a few assumptions, it is possible to avoid posting creating
task objects for targets that are already up-to-date.

On a silly benchmark the gain observed for 1M tasks can be 5m->10s
for a single file change.

Usage::

	def options(opt):
		opt.load('fast_partial')

Assuptions:
* Mostly for C/C++/Fortran targets with link tasks (object-only targets are not handled)
* For full project builds: no --targets and no pruning from subfolders
* The installation phase is ignored
* `use=` dependencies are specified up front even across build groups
* Task generator source files are not obtained from globs

Implementation details:
* The first layer obtains file timestamps to recalculate file hashes only
  when necessary (similar to md5_tstamp); the timestamps are then stored
  in a dedicated pickle file
* A second layer associates each task generator to a file set to help
  detecting changes. Task generators are to create their tasks only when
  the related files have been modified. A specific db file is created
  to store such data (5m -> 1m10)
* A third layer binds build context proxies onto task generators, replacing
  the default context. While loading data for the full build uses more memory
  (4GB -> 9GB), partial builds are then much faster (1m10 -> 13s)
* A fourth layer enables a 2-level cache on file signatures to
  reduce the size of the main pickle file (13s -> 10s)
"""

import os
from waflib import Build, Context, Errors, Logs, Task, TaskGen, Utils
from waflib.TaskGen import feature, after_method, taskgen_method
import waflib.Node

DONE = 0
DIRTY = 1
NEEDED = 2

SKIPPABLE = ['cshlib', 'cxxshlib', 'cstlib', 'cxxstlib', 'cprogram', 'cxxprogram']

TSTAMP_DB = '.wafpickle_tstamp_db_file'

SAVED_ATTRS = 'root node_sigs task_sigs imp_sigs raw_deps node_deps'.split()

class bld_proxy(object):
	def __init__(self, bld):
		object.__setattr__(self, 'bld', bld)

		object.__setattr__(self, 'node_class', type('Nod3', (waflib.Node.Node,), {}))
		self.node_class.__module__ = 'waflib.Node'
		self.node_class.ctx = self

		object.__setattr__(self, 'root', self.node_class('', None))
		for x in SAVED_ATTRS:
			if x != 'root':
				object.__setattr__(self, x, {})

		self.fix_nodes()

	def __setattr__(self, name, value):
		bld = object.__getattribute__(self, 'bld')
		setattr(bld, name, value)

	def __delattr__(self, name):
		bld = object.__getattribute__(self, 'bld')
		delattr(bld, name)

	def __getattribute__(self, name):
		try:
			return object.__getattribute__(self, name)
		except AttributeError:
			bld = object.__getattribute__(self, 'bld')
			return getattr(bld, name)

	def __call__(self, *k, **kw):
		return self.bld(*k, **kw)

	def fix_nodes(self):
		for x in ('srcnode', 'path', 'bldnode'):
			node = self.root.find_dir(getattr(self.bld, x).abspath())
			object.__setattr__(self, x, node)

	def set_key(self, store_key):
		object.__setattr__(self, 'store_key', store_key)

	def fix_tg_path(self, *tgs):
		# changing Node objects on task generators is possible
		# yet, all Node objects must belong to the same parent
		for tg in tgs:
			tg.path = self.root.make_node(tg.path.abspath())

	def restore(self):
		dbfn = os.path.join(self.variant_dir, Context.DBFILE + self.store_key)
		Logs.debug('rev_use: reading %s', dbfn)
		try:
			data = Utils.readf(dbfn, 'rb')
		except (EnvironmentError, EOFError):
			# handle missing file/empty file
			Logs.debug('rev_use: Could not load the build cache %s (missing)', dbfn)
		else:
			try:
				waflib.Node.pickle_lock.acquire()
				waflib.Node.Nod3 = self.node_class
				try:
					data = Build.cPickle.loads(data)
				except Exception as e:
					Logs.debug('rev_use: Could not pickle the build cache %s: %r', dbfn, e)
				else:
					for x in SAVED_ATTRS:
						object.__setattr__(self, x, data.get(x, {}))
			finally:
				waflib.Node.pickle_lock.release()
		self.fix_nodes()

	def store(self):
		data = {}
		for x in Build.SAVED_ATTRS:
			data[x] = getattr(self, x)
		db = os.path.join(self.variant_dir, Context.DBFILE + self.store_key)

		try:
			waflib.Node.pickle_lock.acquire()
			waflib.Node.Nod3 = self.node_class
			x = Build.cPickle.dumps(data, Build.PROTOCOL)
		finally:
			waflib.Node.pickle_lock.release()

		Logs.debug('rev_use: storing %s', db)
		Utils.writef(db + '.tmp', x, m='wb')
		try:
			st = os.stat(db)
			os.remove(db)
			if not Utils.is_win32:
				os.chown(db + '.tmp', st.st_uid, st.st_gid)
		except (AttributeError, OSError):
			pass
		os.rename(db + '.tmp', db)

class bld(Build.BuildContext):
	def __init__(self, **kw):
		super(bld, self).__init__(**kw)
		self.hashes_md5_tstamp = {}

	def __call__(self, *k, **kw):
		# this is one way of doing it, one could use a task generator method too
		bld = kw['bld'] = bld_proxy(self)
		ret = TaskGen.task_gen(*k, **kw)
		self.task_gen_cache_names = {}
		self.add_to_group(ret, group=kw.get('group'))
		ret.bld = bld
		bld.set_key(ret.path.abspath().replace(os.sep, '') + str(ret.idx))
		return ret

	def is_dirty(self):
		return True

	def store_tstamps(self):
		# Called after a build is finished
		# For each task generator, record all files involved in task objects
		# optimization: done only if there was something built
		do_store = False
		try:
			f_deps = self.f_deps
		except AttributeError:
			f_deps = self.f_deps = {}
			self.f_tstamps = {}

		allfiles = set()
		for g in self.groups:
			for tg in g:
				try:
					staleness = tg.staleness
				except AttributeError:
					staleness = DIRTY

				if staleness != DIRTY:
					# DONE case: there was nothing built
					# NEEDED case: the tg was brought in because of 'use' propagation
					# but nothing really changed for them, there may be incomplete
					# tasks (object files) and in this case it is best to let the next build
					# figure out if an input/output file changed
					continue

				do_cache = False
				for tsk in tg.tasks:
					if tsk.hasrun == Task.SUCCESS:
						do_cache = True
						pass
					elif tsk.hasrun == Task.SKIPPED:
						pass
					else:
						# one failed task, clear the cache for this tg
						try:
							del f_deps[(tg.path.abspath(), tg.idx)]
						except KeyError:
							pass
						else:
							# just store the new state because there is a change
							do_store = True

						# skip the rest because there is no valid cache possible
						break
				else:
					if not do_cache:
						# all skipped, but is there anything in cache?
						try:
							f_deps[(tg.path.abspath(), tg.idx)]
						except KeyError:
							# probably cleared because a wscript file changed
							# store it
							do_cache = True

					if do_cache:

						# there was a rebuild, store the data structure too
						tg.bld.store()

						# all tasks skipped but no cache
						# or a successful task build
						do_store = True
						st = set()
						for tsk in tg.tasks:
							st.update(tsk.inputs)
							st.update(self.node_deps.get(tsk.uid(), []))

						# TODO do last/when loading the tgs?
						lst = []
						for k in ('wscript', 'wscript_build'):
							n = tg.path.find_node(k)
							if n:
								n.get_bld_sig()
								lst.append(n.abspath())

						lst.extend(sorted(x.abspath() for x in st))
						allfiles.update(lst)
						f_deps[(tg.path.abspath(), tg.idx)] = lst

		for x in allfiles:
			# f_tstamps has everything, while md5_tstamp can be relatively empty on partial builds
			self.f_tstamps[x] = self.hashes_md5_tstamp[x][0]

		if do_store:
			dbfn = os.path.join(self.variant_dir, TSTAMP_DB)
			Logs.debug('rev_use: storing %s', dbfn)
			dbfn_tmp = dbfn + '.tmp'
			x = Build.cPickle.dumps([self.f_tstamps, f_deps], Build.PROTOCOL)
			Utils.writef(dbfn_tmp, x, m='wb')
			os.rename(dbfn_tmp, dbfn)
			Logs.debug('rev_use: stored %s', dbfn)

	def store(self):
		self.store_tstamps()
		if self.producer.dirty:
			Build.BuildContext.store(self)

	def compute_needed_tgs(self):
		# assume the 'use' keys are not modified during the build phase

		dbfn = os.path.join(self.variant_dir, TSTAMP_DB)
		Logs.debug('rev_use: Loading %s', dbfn)
		try:
			data = Utils.readf(dbfn, 'rb')
		except (EnvironmentError, EOFError):
			Logs.debug('rev_use: Could not load the build cache %s (missing)', dbfn)
			self.f_deps = {}
			self.f_tstamps = {}
		else:
			try:
				self.f_tstamps, self.f_deps = Build.cPickle.loads(data)
			except Exception as e:
				Logs.debug('rev_use: Could not pickle the build cache %s: %r', dbfn, e)
				self.f_deps = {}
				self.f_tstamps = {}
			else:
				Logs.debug('rev_use: Loaded %s', dbfn)


		# 1. obtain task generators that contain rebuilds
		# 2. obtain the 'use' graph and its dual
		stales = set()
		reverse_use_map = Utils.defaultdict(list)
		use_map = Utils.defaultdict(list)

		for g in self.groups:
			for tg in g:
				if tg.is_stale():
					stales.add(tg)

				try:
					lst = tg.use = Utils.to_list(tg.use)
				except AttributeError:
					pass
				else:
					for x in lst:
						try:
							xtg = self.get_tgen_by_name(x)
						except Errors.WafError:
							pass
						else:
							use_map[tg].append(xtg)
							reverse_use_map[xtg].append(tg)

		Logs.debug('rev_use: found %r stale tgs', len(stales))

		# 3. dfs to post downstream tg as stale
		visited = set()
		def mark_down(tg):
			if tg in visited:
				return
			visited.add(tg)
			Logs.debug('rev_use: marking down %r as stale', tg.name)
			tg.staleness = DIRTY
			for x in reverse_use_map[tg]:
				mark_down(x)
		for tg in stales:
			mark_down(tg)

		# 4. dfs to find ancestors tg to mark as needed
		self.needed_tgs = needed_tgs = set()
		def mark_needed(tg):
			if tg in needed_tgs:
				return
			needed_tgs.add(tg)
			if tg.staleness == DONE:
				Logs.debug('rev_use: marking up %r as needed', tg.name)
				tg.staleness = NEEDED
			for x in use_map[tg]:
				mark_needed(x)
		for xx in visited:
			mark_needed(xx)

		# so we have the whole tg trees to post in the set "needed"
		# load their build trees
		for tg in needed_tgs:
			tg.bld.restore()
			tg.bld.fix_tg_path(tg)

		# the stale ones should be fully build, while the needed ones
		# may skip a few tasks, see create_compiled_task and apply_link_after below
		Logs.debug('rev_use: amount of needed task gens: %r', len(needed_tgs))

	def post_group(self):
		# assumption: we can ignore the folder/subfolders cuts
		def tgpost(tg):
			try:
				f = tg.post
			except AttributeError:
				pass
			else:
				f()

		if not self.targets or self.targets == '*':
			for tg in self.groups[self.current_group]:
				# this can cut quite a lot of tg objects
				if tg in self.needed_tgs:
					tgpost(tg)
		else:
			# default implementation
			return Build.BuildContext.post_group()

	def get_build_iterator(self):
		if not self.targets or self.targets == '*':
			self.compute_needed_tgs()
		return Build.BuildContext.get_build_iterator(self)

@taskgen_method
def is_stale(self):
	# assume no globs
	self.staleness = DIRTY

	# 1. the case of always stale targets
	if getattr(self, 'always_stale', False):
		return True

	# 2. check if the db file exists
	db = os.path.join(self.bld.variant_dir, Context.DBFILE)
	try:
		dbstat = os.stat(db).st_mtime
	except OSError:
		Logs.debug('rev_use: must post %r because this is a clean build')
		return True

	# 3. check if the configuration changed
	if os.stat(self.bld.bldnode.find_node('c4che/build.config.py').abspath()).st_mtime > dbstat:
		Logs.debug('rev_use: must post %r because the configuration has changed', self.name)
		return True

	# 3.a any tstamp data?
	try:
		f_deps = self.bld.f_deps
	except AttributeError:
		Logs.debug('rev_use: must post %r because there is no f_deps', self.name)
		return True

	# 4. check if this is the first build (no cache)
	try:
		lst = f_deps[(self.path.abspath(), self.idx)]
	except KeyError:
		Logs.debug('rev_use: must post %r because there it has no cached data', self.name)
		return True

	try:
		cache = self.bld.cache_tstamp_rev_use
	except AttributeError:
		cache = self.bld.cache_tstamp_rev_use = {}

	# 5. check the timestamp of each dependency files listed is unchanged
	f_tstamps = self.bld.f_tstamps
	for x in lst:
		try:
			old_ts = f_tstamps[x]
		except KeyError:
			Logs.debug('rev_use: must post %r because %r is not in cache', self.name, x)
			return True

		try:
			try:
				ts = cache[x]
			except KeyError:
				ts = cache[x] = os.stat(x).st_mtime
		except OSError:
			del f_deps[(self.path.abspath(), self.idx)]
			Logs.debug('rev_use: must post %r because %r does not exist anymore', self.name, x)
			return True
		else:
			if ts != old_ts:
				Logs.debug('rev_use: must post %r because the timestamp on %r changed %r %r', self.name, x, old_ts, ts)
				return True

	self.staleness = DONE
	return False

@taskgen_method
def create_compiled_task(self, name, node):
	# skip the creation of object files
	# assumption: object-only targets are not skippable
	if self.staleness == NEEDED:
		# only libraries/programs can skip object files
		for x in SKIPPABLE:
			if x in self.features:
				return None

	out = '%s.%d.o' % (node.name, self.idx)
	task = self.create_task(name, node, node.parent.find_or_declare(out))
	try:
		self.compiled_tasks.append(task)
	except AttributeError:
		self.compiled_tasks = [task]
	return task

@feature(*SKIPPABLE)
@after_method('apply_link')
def apply_link_after(self):
	# cprogram/cxxprogram might be unnecessary
	if self.staleness != NEEDED:
		return
	for tsk in self.tasks:
		tsk.hasrun = Task.SKIPPED

def path_from(self, node):
	# handle nodes of distinct types
	if node.ctx is not self.ctx:
		node = self.ctx.root.make_node(node.abspath())
	return self.default_path_from(node)
waflib.Node.Node.default_path_from = waflib.Node.Node.path_from
waflib.Node.Node.path_from = path_from

def h_file(self):
	# similar to md5_tstamp.py, but with 2-layer cache
	# global_cache for the build context common for all task generators
	# local_cache for the build context proxy (one by task generator)
	#
	# the global cache is not persistent
	# the local cache is persistent and meant for partial builds
	#
	# assume all calls are made from a single thread
	#
	filename = self.abspath()
	st = os.stat(filename)

	global_cache = self.ctx.bld.hashes_md5_tstamp
	local_cache = self.ctx.hashes_md5_tstamp

	if filename in global_cache:
		# value already calculated in this build
		cval = global_cache[filename]

		# the value in global cache is assumed to be calculated once
		# reverifying it could cause task generators
		# to get distinct tstamp values, thus missing rebuilds
		local_cache[filename] = cval
		return cval[1]

	if filename in local_cache:
		cval = local_cache[filename]
		if cval[0] == st.st_mtime:
			# correct value from a previous build
			# put it in the global cache
			global_cache[filename] = cval
			return cval[1]

	ret = Utils.h_file(filename)
	local_cache[filename] = global_cache[filename] = (st.st_mtime, ret)
	return ret
waflib.Node.Node.h_file = h_file

