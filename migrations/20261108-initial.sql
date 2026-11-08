CREATE TABLE directories
  ( directory_id TEXT PRIMARY KEY
  , name TEXT NOT NULL
  , path TEXT NOT NULL
  , parent TEXT REFERENCES directories(directory_id) ON DELETE CASCADE
  );

CREATE TABLE files
  ( file_id TEXT NOT NULL
  , name TEXT NOT NULL
  , size INTEGER NOT NULL
  , path TEXT NOT NULL
  , directory TEXT NOT NULL REFERENCES directories(directory_id) ON DELETE CASCADE
  );

CREATE UNIQUE INDEX unique_directory_id ON directories (directory_id);
CREATE UNIQUE INDEX unique_directory_path ON directories (path);
CREATE UNIQUE INDEX unique_file_id ON files (file_id);
CREATE UNIQUE INDEX unique_files_path ON files (path);
