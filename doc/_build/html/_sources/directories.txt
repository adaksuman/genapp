Deployed Directory Structure
============================

A deployed genapp application contains a ``.genapp`` subdirectory that contains
all genapp related files.

The ``.genapp`` directory structure contains the following types of files:

**Control Scripts**
  Control scripts are located in ``.genapp/control``. They include, at a
  minimum, ``start``, which is used to start the application. Control scripts
  may also include scripts to check or manipulate the state of the application
  -- for example a script to collect performance metrics.

**Configuration Files**
  Control scripts often rely on external files to define application
  configuration settings. These are typically located in ``.genapp/control``.

**Metadata Env**
  genapp writes values contained in the special "env" metadata to
  ``.genapp/control/env``. This file may be sourced as a shell script to load
  the metadata environment for various scripts (e.g. ``start``).

**Logs**
  Plugins may configure applications to write log output to
  ``.genapp/log``. However, this is only a convention -- plugins are free to
  configure applications in any way they see fit.

**Metadata**
  genapp will write the original application metadata file to
  ``.genapp/metadata.json``.

**Reserved Ports**
  genapp may reserve one or more ports for an application. Port reservation is
  made by writing an empty file to ``.genapp/ports``.

**Setup Status**
  genapp writes the setup status for each plugin to
  ``.genapp/setup_status``. Plugins setup output and results are written to
  files named 'plugin\_' + PLUGIN_NAME + '_' + SETUP_EXIT_CODE.

Here's a sample genapp directory structure::

    ./.genapp
    ./.genapp/control
    ./.genapp/control/env
    ./.genapp/control/start
    ./.genapp/log
    ./.genapp/metadata.json
    ./.genapp/ports
    ./.genapp/ports/8024
    ./.genapp/setup_status
    ./.genapp/setup_status/ok
    ./.genapp/setup_status/plugin_simple_html_0

Most of the contents of the genapp directory structure can be queried using
genapp's application query interface. See :doc:`query_app` for details.
