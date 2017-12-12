You may download compiled  [PaGoDump & PaGoRerstore v10.1 here](https://github.com/pashagolub/PaGoDump-PaGoRestore/releases)!

# PaGoDump

PaGoDump for PostgreSQL: GUI tool for extracting a PostgreSQL database into SQL script file, archived SQL file (GZIP), TAR archive, or pg_restore custom archive (*.backup).

PaGoDump is a GUI Windows utility for backing up a PostgreSQL database built with [Microolap PostgresDAC](http://microolap.com/products/connectivity/postgresdac/). It makes consistent backups even if the database is being used concurrently. PaGoDump does not block other users accessing the database (readers or writers), also it works with databases with any names (unicode) and dump them to any files (unicode again).

Dumps can be output in script or archive file formats. Script dumps are plain-text files containing the SQL commands required to reconstruct the database to the state it was in at the time it was saved. To restore from such a script, feed it to psql. Script files can be used to reconstruct the database even on other machines and other architectures; with some modifications even on other SQL database products.

The alternative archive file formats must be used with pg_restore to rebuild the database. They allow pg_restore to be selective about what is restored, or even to reorder the items prior to being restored. The archive file formats are designed to be portable across architectures.

When used with one of the archive file formats and combined with pg_restore, PaGoDump provides a flexible archival and transfer mechanism. PaGoDump can be used to backup an entire database, then pg_restore can be used to examine the archive and/or select which parts of the database are to be restored. The most flexible output file format is the "COMPRESS" format. It allows for selection and reordering of all archived items, and is compressed by default. The TAR format is not compressed and it is not possible to reorder data when loading, but it is otherwise quite flexible; moreover, it can be manipulated with standard Unix tools such as tar.

While running PaGoDump, one should examine the output for any warnings (Log tab), especially in light of the limitations listed below.

## Why PaGoDump?

* Support for all PostgreSQL versions from one place.
* It is portable, installation is not required. May be ran from any removable device.
* Full support of pg_dump options and output formats.
* Windows GUI.
* Unicode.
* Free. 

# PaGoRestore

PaGoRestore for PostgreSQL: GUI tool for restoring a database from PostgreSQL database dump.

PaGoRestore is a GUI Windows utility for restoring a PostgreSQL database from an archive built with pg_dump or PaGoDump. Utility itself built with [Microolap PostgresDAC](http://microolap.com/products/connectivity/postgresdac/). It will issue the commands necessary to reconstruct the database to the state it was in at the time it was saved. The archive files also allow PaGoRestore to be selective about what is restored, or even to reorder the items prior to being restored. The archive files are designed to be portable across architectures.

PaGoRestore can operate in two modes. If a database is specified as target, the archive is restored directly into the database. Otherwise, a script containing the SQL commands necessary to rebuild the database is created and written to a file. The script output is equivalent to the plain text output format of PaGoDump and pg_dump. Some of the options controlling the output are therefore analogous to PaGoDump and pg_dump options.

Obviously, PaGoRestore cannot restore information that is not present in the archive file. For instance, if the archive was made using the “dump data as INSERT commands” option, PaGoRestore will not be able to load the data using COPY statements.

While running PaGoRestore, one should examine the output for any warnings (Log tab), especially in light of the limitations listed below.

## Why PaGoRestore?

* Support for all PostgreSQL versions from one place.
* It is portable, installation is not required. May be ran from any removable device.
* Full support of pg_restore options and input formats.
* Windows GUI.
* Unicode.
* Free. 

