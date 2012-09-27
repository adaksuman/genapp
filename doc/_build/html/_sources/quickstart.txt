Quick Start
===========

genapp is used to manage the lifecycle of user applications deployed to a
server. For background on genapp, see :doc:`overview`.

Before proceeding, refer to :doc:`requirements` for the list of software you
need to use genapp.

The instructions below use two symbols to designate the type of shell you
should type a command into:

*$*
  When you see a command that starts with *$* type that command (not
  including *$*) into a terminal shell.

*>*
  When you see a command that starts with *>* type that command (again, not
  including the *>*) into the genapp shell. See :ref:`run_genapp` below for
  instructions on starting the genapp shell.

Get the Code
------------

Use git to checkout genapp::

   $ git clone git@github.com:cloudbees/genapp.git

To troubleshoot problems, refer to :doc:`code`.

Compile genapp
--------------

Compile genapp using ``make``::

   $ cd genapp
   $ make

This will download any additional software used by genapp and compile the
Erlang application.

Configure genapp
----------------

genapp uses a single configuration file to specify the directories and other
settings it needs.

Create a development config file by copying the template provided by genapp::

   $ cp priv/dev.config.in priv/dev.config

The file must be ``priv/dev.config`` for genapp to find it.

There are three settings that genapp uses:

``devmode``
  Indicates whether or not genapp is running in development mode. When testing
  genapp or developing plugins, set this value to ``true``.

``apps_home``
  Specifies the location where genapp will install user applications. Below
  you will create a directory that will be used for this value.

``plugins_home``
  Specifies the location where genapp will find plugins. Below you will create
  a directory that will be used for this value.

Create directories for both genapp apps and plugins::

   $ mkdir -p ~/genapp/{apps,plugins}

Next, edit ``priv/dev.config`` and modify the three settings. Stripped of all
comments (i.e. lines starting with "%") the file should look like this::

   [{genapp,
     [
      {devmode, true},
      {apps_home, "HOME_DIR/genapp/apps"},
      {plugins_home, "HOME_DIR/genapp/plugins"}
   ]}].

Where ``HOME_DIR`` is the full path to your home directory. Note that the
symbol for user directory ``~`` will *not* be resolved by genapp -- you must
specify the full, absolute path to each location.

At this point your genapp installation is configured. In the next step you'll
create an application you can deploy using genapp.

Create an Application
---------------------

Applications are directories that contain a ``.genapp`` directory and
application specific files such as source code, binaries, web content etc.

In this example, we'll deploy a simple HTTP app using a sample plugin provided
with genapp.

Create the application directory structure::

   $ mkdir -p ~/genapp/sample-app/.genapp

Create the file ``~/genapp/sample-app/.genapp/metadata.json``::

   {
     "app": {
       "plugins": ["simple_html"]
     }
   }

This is the minimum required metadata for an application. It contains the list
of plugins that will be used to setup the application. In this case, it uses a
single plugin: ``simple_html``. Later, we'll install this plugin so it can be
used by genapp.

To complete the sample app, create the file
``~/genapp/sample-app/index.html``::

   <html>
   Welcome to genapp! Have a look at the <a href=".genapp">.genapp</a>
   directory -- it contains a lot of interesting things!
   </html>

This file will be installed as the root HTML file when genapp deploys the
application.

Confirm your sample application structure is complete by typing the command::

    $ find ~/genapp/sample-app | sort

You should see this output::

    ~/genapp/sample-app/
    ~/genapp/sample-app/.genapp
    ~/genapp/sample-app/.genapp/metadata.json
    ~/genapp/sample-app/index.html

where ``~`` is replaced by your home directory.

In the next step, we'll install ``simple_html``, which is the plugin used to
setup the application.

Install a Plugin
----------------

A plugin is a directory that contains a ``setup`` script and any other files
potentiall needed by the plugin when setting up applications.

The sample application created in the previous step uses a plugin named
``simple_html``. This plugin is provided with genapp as a sample.

Install ``simple_html`` by creating a symbolic link to its directory as
follows::

   $ ln -s GENAPP_REPO/examples/simple_html ~/genapp/plugins/simple_html

``GENAPP_REPO`` must be the full path to your local genapp directory, created
when you cloned the git repository earlier.

The symbolic link to the plugin directory must be created in the
``plugins_home`` directory as specified in ``priv/dev.config``.

To confirm that the plugin is installed, run this command::

    $ find -L ~/genapp/plugins | sort

You should see the plugin directory and its setup file::

    ~/genapp/plugins/
    ~/genapp/plugins/simple_http
    ~/genapp/plugins/simple_html/setup

where ``~`` is replaced by your home directory.

.. _run_genapp:

Run genapp
----------

From the genapp repo directory (the top-level directory that was created when
you cloned the genapp git repository), run the genapp shell::

   $ make shell

This starts a developer shell for genapp. You can type genapp commands into the
shell to test operations.

Test a command by typing the following command into the shell and pressing
``ENTER``::

    > genapp:query_apps([]).

Note that the command ends with a period "." -- all genapp commands issued in
the shell should end with a period. Execute the command by pressing ``ENTER``.

You you see this result::

    []

This is an empty list, which indicates that there are no deployed applications.

If you don't see anything after pressing ``ENTER``, verify that you've typed a
period at the end of the command. If you forgot the period, type it and press
``ENTER``.

If you get a ``syntax error``, check your command syntax and try again.

Deploy an Application
---------------------

In the genapp shell, type the following command to deploy your sample
application::

   > genapp:deploy("HOME_DIR/genapp/sample-app").

where ``HOME_DIR`` is the full path to your home directory. Note that genapp
does not recognize ``~`` as your home directory -- you must type the path
explicitly.

If the command was successful, you will see output that looks like this::

    =INFO REPORT==== 19-Sep-2012::12:48:05 ===
    {plugin_setup_ok,{<<"simple_html">>,"c86e0d7c",[]}}

genapp uses a randomly assigned application ID (``c86e0d7c`` in the example
above), so the output you see will be slightly different. Note the application
ID for the deployed application -- you'll use it when running the application
below.

If you get an error, refer to :doc:`trouble`.

If the application was deployed successfully, it will appear in the list when
you run ``query_apps``::

    > genapp:query_apps([setup_status]).

You should see a result similar to this::

    [{"c86e0d7c",[{setup_status,ok}]}]

Running an Application
----------------------

genapp is not responsible for running the application directly. A deployed
genapp application may be started by calling its ``start`` script, but genapp
itself will not do this.

This separation of concerns is intentional -- genapp only deploys the
application, preparing it for start, stop, and monitoring by other frameworks.

To run the sample application, execute its start script as follows::

    $ ~/genapp/apps/APP_ID/.genapp/control/start

where ``APP_ID`` is the application ID displayed by genapp when the application
was deployed.

You should see output that looks like this::

    Serving HTTP on 0.0.0.0 port 8265 ...

The port displayed will be different in your case -- genapp assigns ports
randomly within a configurable range (by default the range is from 8000 to
8999).

Test the Application
--------------------

To test the running sample application, view the following URL in your
browser:

    http://localhost:PORT

where ``PORT`` is the port assigned to the application (this is displayed in
the terminal shell when the application is started -- see above).

You should see a web page that says:

  *Welcome to genapp! Have a look at the .genapp directory -- it contains a lot
  of interesting things!*

Click on the *.genapp* link -- you'll see all of the files that genapp created
during setup.

You also list these files from a terminal::

    $ cd ~/genapp/apps/APP_ID
    $ find | sort

where ``APP_ID`` is the ID of the deployed application.

You should see something like this::

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
    ./index.html

All of the application's genapp related files are located within the
``.genapp`` subdirectory. The other files -- in this case just ``index.html``
is located outside that directory.

For details on the ``.genapp`` directory structure, see :doc:`directories`.

genapp provides an interface for querying this structure -- see
:doc:`query_app`.

Undeploy an Application
-----------------------

Type the following command in the genapp shell::

    > genapp:undeploy("APP_ID").

where ``APP_ID`` is the ID of the deployed application. (Note that in this case
application ID is a quoted string.)

This will remove the application directory and cleanup and related resources.

.. note::

   genapp simply deletes the application directory to undeploy it. You can
   do this from outside genapp to accomplish the same thing.

Summary
-------

In this quick start guide, you stepped through the core genapp functionality:

- Download, compile and configure genapp
- Create a simple genapp application
- Deploy an application using genapp
- Run and test a genapp deployed application
- Undeploy an application using genapp

Use the navigation bar below to view the next topic for a deeper dive into
genapp.
