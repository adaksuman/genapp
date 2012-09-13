Overview
========

genapp is used to manage the lifecycle of user applications on the CloudBees
platform.

Each CloudBees server is capable of running multiple user applications. These
applications are installed and configured (i.e. deployed) by invoking a genapp
agent. genapp is also used to uninstall (i.e. undeploy) applications.

genapp handles:

- Application port allocation
- Application deployment by way of plugins
- Application undeployment

genapp does NOT handle:

- Application packaging
- Application package deployment
- Specific application setup (these are handled by plugins)
- Application start and stop
- Application monitoring

Applications
------------

Applications are directory structures that contain a ``.genapp``
subdirectory. The ``.genapp`` subdirectory contains information that genapp
uses to deploy the application.

In particular, the ``.genapp`` subdirectory contains a ``metadata.json`` file,
which defines the list of plugins used to setup the
application. ``metadata.json`` contains metadata sections, which provide
attributes to the plugins during setup.

.. toctree::

   metadata

Plugins
-------

genapp uses plugins to perform application setup. Plugins are specified in the
``plugins`` attribute of the ``app`` section in the metadata.

Plugins are used in the order they're specified in the plugins list. Each
plugin is called upon to perform setup operations for the application. If the
deployment is successful, the application will be ready to run.

.. toctree::

   plugins
