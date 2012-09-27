Requirements
============

To use genapp, for either deploying applications or developing plugins, you
need the following software installed:

**Erlang**
  genapp is designed to work with earlier versions of Erlang. If the version of
  Erlang that's available for your system doesn't work, please :doc:`concact
  CloudBees <contact>` with the details and we'll help you resolve them.

.. note::

  If you are installing Erlang on Ubuntu or Debian using ``apt-get``, you need
  to install the ``erlang-dev`` package, which includes the tools needed to
  build genapp.

**curl**
  genapp uses the command line tool ``curl`` to download remote plugins. If you
  want to test genapp's remote plugin functionality, you'll need curl
  installed and available on the path.

**make**
  genapp uses ``make`` for building source code. This is usually provided by
  the system package of the same name.

**python** (optional for quick start example)
  If you follow the :doc:`quick start <quickstart>` example, you'll need Python
  to run the deployed application.
