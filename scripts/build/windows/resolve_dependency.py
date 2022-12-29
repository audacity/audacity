from dependencies import Dependency
import os
import re

def _is_system_library(dependency):
    __additional_system_deps = [
        r'vcruntime.*\.dll',
        r'msvcp.*\.dll',
    ]

    return not any(re.match(pattern, dependency, re.IGNORECASE) for pattern in __additional_system_deps)


def resolve_dependency(dependency, additional_paths):
    if additional_paths is not None:
        for path in additional_paths:
            resolved_path = os.path.join(path, dependency)
            if os.path.exists(resolved_path):
                return Dependency(dependency, resolved_path, False)

    for path in [r'C:\Windows\System32', r'C:\Windows\SysWOW64', r'C:\Windows\System32\downlevel', r'C:\Windows\SysWOW64\downlevel']:
        resolved_path = os.path.join(path, dependency)
        if os.path.exists(resolved_path):
            return Dependency(dependency, resolved_path, _is_system_library(dependency))

    for lookup_dir in filter(None, os.environ["PATH"].split(';')):
        path = os.path.join(lookup_dir, dependency)
        if os.path.isfile(path):
            return Dependency(dependency, path, _is_system_library(dependency))

    if dependency.lower().startswith('api-ms-win-'):
        return Dependency(dependency, dependency, True)

    return None
