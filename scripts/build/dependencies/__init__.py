from dataclasses import dataclass

@dataclass(frozen=True)
class Dependency:
    name: str
    path: str
    is_system_library: bool
