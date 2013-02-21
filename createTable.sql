create table modules (path, mtime, id);
create table publicImports (importerId, importedId);
create table containers (name, protection, moduleId, id);
create table symbols (name, type, kind, containerId, id);
