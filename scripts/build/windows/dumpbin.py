import os
import re
import subprocess
from . import locate_msvc_tools
from . import resolve_dependency

_dumpbin_hint = None
_dumpbin = None

def _get_dumpbin():
    global _dumpbin

    if _dumpbin is not None:
        return _dumpbin

    if _dumpbin_hint is not None:
        location = os.path.join(
            os.path.dirname(_dumpbin_hint if os.path.isfile(_dumpbin_hint) else _dumpbin_hint),
            'dumpbin.exe')

        if os.path.isfile(location):
            _dumpbin = location
            return _dumpbin

    location = locate_msvc_tools.locate_msvc_tools()
    if location is not None:
        location = os.path.join(location, 'dumpbin.exe')
        if os.path.isfile(location):
            _dumpbin = location
            return _dumpbin

    return None


def _get_file_dependencies(binary, resolved_dependencies, additional_paths):
    # Binary is already resolved, no additional dependencies to collect
    if binary.lower() in resolved_dependencies:
        return []

    resolved_dependencies[binary.lower()] = True

    dumpbin = _get_dumpbin()

    if dumpbin is None:
        raise RuntimeError('Unable to locate dumpbin.exe')

    dependencies = []

    output = subprocess.check_output([dumpbin, '/dependents', binary]).decode('utf-8')

    if output is None:
        return dependencies

    for line in output.splitlines():
        if re.match(r'^\s+.*\.dll$', line, re.IGNORECASE):
            dependency_name = line.strip()
            dependency = resolve_dependency.resolve_dependency(dependency_name, additional_paths)

            if dependency is None:
                raise RuntimeError(f"Unable to resolve dependency {dependency_name} for {binary}")

            dependencies.append(dependency)
            if not dependency.is_system_library:
                dependencies.extend(_get_file_dependencies(dependency.path, resolved_dependencies, additional_paths))

    return dependencies

def set_dumpbin_hint(path):
    global _dumpbin_hint
    _dumpbin_hint = path

def collect_dependencies(binaries, additional_paths=None):
    dumpbin = _get_dumpbin()
    if dumpbin is None:
        raise RuntimeError('Unable to locate dumpbin.exe')

    dependencies = []
    resolved_dependencies = {}

    for binary in binaries:
        if not os.path.isfile(binary):
            continue

        paths_list = [os.path.dirname(binaries[0]), os.path.dirname(binary)]
        if additional_paths is not None:
            paths_list.extend(additional_paths)

        dependencies.extend(_get_file_dependencies(binary, resolved_dependencies, paths_list))

    return dependencies
