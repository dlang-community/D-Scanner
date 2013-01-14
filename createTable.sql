create table files (path, mtime, id);
create table publicImports (importerId, importedId);
create table containers (name, protection, fileId, id);
create table symbols (name, type, kind, containerId, id);
