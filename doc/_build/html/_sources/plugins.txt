Plugins
=======

A genapp plugin is a directory that contains a script named ``setup``, which is
used by genapp to perform setup operations during application deployment.

Multiple plugins may be used to perform independent setup operations during
application deployment. At least one of the plugins must create a ``start``
control script that can be used to start the application.

Plugin operations consist of:

- Copying application source files to the application's install location
- Creating the ``start`` control script
- Writing configuration files that may be used by the start script
- Copying additional files such as runtime libraries to the application's
  install location

Below is a simple plugin that copies files from the application package to the
application target directory (install location) and creates a start script that
invokes a 'run' script, which must be provided by the application::

    cp -a $pkg_dir/* $app_dir/
    echo "exec $app_dir/run" > "$control_dir/start"
    chmod 755 "$control_dir/start"

This example represents the minimum work required of a plugin: install
application files and create a start script.

Plugins typically perform operations that are unique to a particular runtime
environment. For example, a "java" plugin may create a start script that
invokes the ``java`` command along with the appropriate list of arguments such
as classpath settings, JVM arguments and the main Java class.

Plugin Directory
----------------

A plugin is a directory that contains a setup script. The setup script must be
named "setup" and located in the root plugin directory.

The plugin directory may contain other files used by the plugin during
setup. These commonly include:

- Architecture specific runtime environments (e.g. Python, Java, Ruby)
- Libraries specific to the plugin's supported features (e.g. a Java logging
  library, if the plugin adds specialized logging features to a Java
  application)
- Control script and configuration file templates

Setup Script
------------

The setup script must be a shell script that performs setup related operation
when *sourced*. Sourcing is a method of reading and executing commands defined
in a file. It's similar to executing the file outright, but insteads runs the
file commands in the context of the sourcing process.

Plugin setup scripts do not have to be executable.

Setup scripts have access to a number of environment variables, which are
listed in the next section.

Start Scripts
-------------

One plugin listed in the application metadata must be responsible for creating
a correct start control script. An application cannot run with this script.

The start control script must be ``.genapp/control/setup``. Plugins may use the
``control_dir`` environment to reference the genapp control subdirectory during
setup (see below).

.. important::

   The start script must not exit until the application process
   exits. I.e. applications must not be run in the background.

.. hint::

   While not strictly required, it's a good idea to start application processes
   using the ``exec`` command, which will replace the start script shell with
   the application process itself. For more information on ``exec`` refer to
   the man page.

Setup Environment Variables
---------------------------

When genapp sources the plugin's setup script, it defines a number of genapp
specific environment variables, which the setup script may reference.

When the plugin configuration file is sourced, it has access to a number of
environment variables it can use to perform setup.

``app_dir``
...........

*The application directory (or, the application install location)*

Plugins must use this directory when copying

.. important::

   When referencing files located in the application directory, plugins must
   use absolute path names. Plugins cannot rely on a "current working
   directory" when an application is started. For example, when creating a
   script that references a file named "run" in the application directory, the
   plugin should use ``$app_dir/run`` instead of ``run`` or ``./run``.

``pkg_dir``
...........

*The application source package directory*

Plugins may use this variable to reference files located in the application
package -- i.e. the top-level directory containing the application source
files. Plugins typically copy files from the application package to the
application directory as follows::

    cp $app_pkg/myfile.txt $app_dir

``plugin_dir``
..............

*The plugin directory*

Plugins may use this variable to reference files provided by the plugin itself
(e.g. runtime libraries, control script templates, etc.) For example, to copy a
file name ``mylib.jar`` to the application "lib" subdirectory, a plugin can
execute::

    cp $plugin_dir/mylib.jar $app_dir/lib

``genapp_dir``
..............

*The genapp subdirectory located within the application directory*

This variable is equivalent to ``$app_dir/.genapp``.

Plugins may use this variable to reference files located in the genapp
subdirectory. These may be files created by other plugins that have already
performed their setup operations or files that the plugin itself creates.

``control_dir``
...............

*The control subdirectory located within the genapp directory*

This variable is equivalent to ``$genapp_dir/control``.

Plugins may use this variable when creating control scripts or control script
configuration files. For example, the start script that must be created by one
of the plugins is ``$control_dir/start``.

``log_dir``
...........

*The conventional location for application logs*

This variable is equivalent to ``$genapp_dir/log``.

genapp doesn't assume any particular logging functionality for application, but
does provide an empty ``log`` directory, located within the genapp directory,
where log files may be created.

``app_id``
..........

*The unique ID of the application being deployed*

Plugins may use this variable to reference the application ID. This might be
useful as start script configuration data if the application needs to be
uniquely identified on the system.

``app_user``
............

*The user associated with the application being deployed*

If genapp is not run in "dev" mode, it will create a unique user for each
deployed application and setup appropriate user permissions for the
application. Plugins may use this variable to reference that user.

If genapp is run in dev mode, ``app_user`` is the same as the user running
genapp.

``app_port``
............

*The genapp assigned port for the application*

If the application requires a port (the default behavior is that each
application requires one port), plugins may reference the assigned port using
this variable.

Once reserved, a port will not be assigned to another application.

This value is typically used to configure servers to listen to the reserved
port.

Application Metadata
....................

In addition to the variables above, genapp makes all attributes defined in the
application metadata available to plugins during setup.

.. note::

   The "app" section is not provided as environment variables.

Metadata variables are named using the following convention:

    *SECTION_NAME* + '_' + *ATTR_NAME*

*SECTION_NAME* is the section name (e.g. "env", "java", etc.)

*TRANSFORMED_ATTR_NAME* is the attribute name.

When naming metadata as environment variables, genapp replaces any character
that is not a number, a letter, or an underscore with an underscore.

The following application metadata::

    {
        "java": {
            "class": "MyClass",
            "args": "Hello"
        }

        "logging": {
            "file": "server.log"
        }
    }

will result in the following environment variables::

    java_class=MyClass
    java_args=Hello
    logging_file=server.log

.. note::

   genapp will force all environment names to be lower case, regardless of
   their case in the application metadata. As environment variables are case
   sensitive, plugins must reference them using all lower case names.

Installing a Plugin
-------------------

Installed plugins must be located within the ``plugins_home`` directory that is
specified in the genapp configuration (:doc:`quickstart` for information on
configuring genapp).

Plugin directories may be symlinked to the plugins home location rather than
copied.

Local Plugins
-------------

A local plugin is a plugin directory that is available on a local file
system. A local plugin does not have to be installed.

Local plugins are used for development purposes and are generally not used in
production. They are convenient when making changes to a plugin in an arbitrary
location (e.g. in a source control managed directory).

Local plugins are specified in application metadata using this syntax::

    {
        "apps": {
            "plugins": [
                {
                    "src": "local_plugin://PLUGIN_DIR"
                }
            ]
        }
    }

where ``PLUGIN_DIR`` is the location of the plugin.

Remote Plugins
--------------

Remote plugins are XXX.
