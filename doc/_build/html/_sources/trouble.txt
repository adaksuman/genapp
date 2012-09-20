Troubleshooting
===============

When an error occurs during a genapp operation, you will see a detailed error
report printed in the genapp shell. Errors are denoted with either a ``CRASH
REPORT``, which looks like this::

    =CRASH REPORT==== 19-Sep-2012::15:41:50 ===
    <error details>

or an ``ERROR REPORT``, which looks like this::

    =ERROR REPORT==== 19-Sep-2012::15:41:50 ===
    <error details>

To resolve the error, look for the section starting with ``exception
exit``. Here's an example of a crash report that occurred because genapp could
not locate the metadata for a deployed application::

    exception exit: {{metadata_read,
                         {enoent, "<path to metadata.json>"}},
                     [{genapp_metadata,handle_read_file,2},
                      {genapp_deploy,new_app,2},
                      {genapp_deploy,init,1},
                      {e2_task,dispatch_init,4},
                      {e2_service,dispatch_init,3},
                      {gen_server,init_it,6},
                      {proc_lib,init_p_do_apply,3}]}

The sections below detail common error types for different operations and
provide suggestions for resolving the errors.

Errors during ``genapp:deploy``
-------------------------------

``{{metadata_read, {enoent, PathToMetadataJson}}``
..................................................

When genapp deploys an application, it tried to locate the file
``.genapp/metadata.json`` in the application directory. This error indicates
that the metadata.json does not exist.

Verify that you're specifying the top most directory for the application. This
must contain a ``.genapp`` subdirectory, which in turn must contain a valid
``metadata.json`` file for the application.

``{{json_error, Error}``
........................

This error will occur if the application metadata is not well formed. Verify
that ``.genapp/metadata.json`` located in the application root directory is
well formed JSON.
