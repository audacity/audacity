import subprocess
import re
import os
from dependencies import Dependency

def _run_objdump(binary, resolver):
    print(f"Running objdump on {binary}")
    lines = subprocess.check_output(["objdump", "-x", binary]).decode("utf-8").splitlines()

    for line in lines:
        match = re.match(r"\s+NEEDED\s+(.+\.so.*)", line)
        if match:
            yield resolver(match.group(1))

def _recursive_collect_dependencies(binary, additional_paths, collected):
    def __resolver(name):
        for path in additional_paths:
            candidate = os.path.join(path, name)
            if os.path.exists(candidate):
                return Dependency(name, candidate, False)
        return Dependency(name, name, True)

    for dependency in _run_objdump(binary, __resolver):
        if dependency not in collected:
            collected.add(dependency)
            if not dependency.is_system_library:
                _recursive_collect_dependencies(dependency.path, additional_paths, collected)

def collect_dependencies(binaries, additional_paths):
    deps = set()

    for binary in binaries:
        _recursive_collect_dependencies(binary, additional_paths, deps)

    return list(deps)
