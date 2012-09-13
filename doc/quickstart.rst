Quick Start
===========

genapp is used to manage the lifecycle of user applications deployed to a
server. For more background on genapp see :doc:`overview`.

Get the Code
------------

Refer to :doc:`code` for details on the genapp source code.

Use git to checkout genapp::

   git clone git@github.com:cloudbees/genapp.git

Compile genapp
--------------

Compile genapp using ``make``::

   cd genapp
   make

This will download any additional software used by genapp and compile the
Erlang application.

Configure genapp
----------------

genapp uses a single configuration file to specify the directories and other
settings it needs.

Create a development config file by copying the template provided by genapp::

   cp priv/dev.config.in priv/dev.config

The file must be ``priv/dev.config`` for genapp to find it.

There are three settings that genapp uses:

``devmode``
  Indicates whether or not genapp is running in development mode. When testing
  genapp or developing plugins, set this value to ``true``.

``apps_home``
  The location where genapp will install user applications. Below you'll create
  a directory and specify it here.

``plugins_home``
  The location where genapp will find plugins. For this quick start example,
  you can specify the genapp source ``examples`` directory.

Create a directory for the user applications::

   mkdir ~/genapp-apps

Next, edit ``priv/dev.config`` and modify the three settings. Stripped of all
comments (i.e. lines starting with "%") the file should look like this::

   [{genapp,
     [
      {devmode, true},
      {apps_home, "HOME_DIR/genapp-apps"},
      {plugins_home, "GENAPP_DIR/examples"}
   ]}].

Where ``HOME_DIR`` is the full path to your home directory and ``GENAPP_DIR``
is the directory containing genapp (i.e. the root directory of the genapp the
local git repository).


Create an Application
---------------------

Applications are directories that contain a ``.genapp`` directory and
application specific files such as source code, binaries, web content etc.

In this example, we'll deploy a simple HTTP site using a sample plugin provided
with genapp.

Create the application directory structure::

   mkdir ~/sample-genapp-app
   mkdir ~/sample-genapp-app/.genapp

Create the file ``~/sample-genapp-app/.genapp/metadata.json``::

   {
     "app": {
       "plugins": ["simple_html"]
     }
   }

This is the minimum required metadata for an application. It contains the list
of plugins that will be used to setup the application. In this case, it uses a
single plugin -- ``simple_html``.

Finally, create the file ``~/sample-genapp-app/index.html``::

   <html>
   Welcome to genapp! Have a look at the <a src=".genapp">.genapp</a>
   directory -- it contains a lot of interesting things!
   </html>

This file will be installed as a part of the sample application.

Run genapp
----------

From the genapp directory, run the genapp shell::

   make shell

