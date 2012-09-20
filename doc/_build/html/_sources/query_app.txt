Querying Applications
=====================

genapp provides an interface for querying application data.

In a shell, use the command ``genapp:query_app``. Here's an example that
queries an application's setup status and setup logs::

    > genapp:query_app("f2145747", [ports, setup_status, setup_logs]).
    {ok,[{setup_logs,[{"simple_html",{ok,<<>>}}]},
     {setup_status,ok},
     {ports,[8822]}]}

The following options may be specified to the ``query_app`` command:

``ports``
  Includes a list of ports reserved for the application.

``metadata``
  Includes the application metadata.

``setup_status``
  Includes the application setup status. Refer to :ref:`setup_status` below for
  possible values.

``setup_logs``
  Includes the setup result and output for each plugin that has completed its
  setup, successfully or not. Note that the setup is abandoned after the first
  plugin setup failure, so the setup logs may not contain logs for every
  plugin.

``{log_tail, N}``
  Includes the last ``N`` bytes of ``.genapp/log/current``.

*Filename*
  Any other value is considered a file name and will be used to read and return
  the contents of a file located in ``.genapp``. This can be used to query
  values written by plugins during setup.

.. _setup_status:

Setup Status
------------

When querying an application's setup status, you will see one of the following
possible values:

``ok``
  Each of the plugins completed its setup without errors.

``error``
  A plung setup failed. Refer to the setup logs for the plugins for error
  details.

``pending``
  A setup is pending. Plugins that have completed setup will have setup logs.

``unkown``
  The setup status is unknown.
