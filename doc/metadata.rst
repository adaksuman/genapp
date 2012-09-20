Application Metadata
====================

Application metadata is defined in a file named ``metadata.json`` located in
the ``.genapp`` subdirectory of an application.

The metadata file contains sections of attributes. Section names are arbitrary
with the exception of the ``app`` section, which is reserved for use by genapp.

Pluins may use attributes from any section in the metadata. Some plugins may
require specific sections and attribute names, which others may use sections
and attribute names as optional values.

Here's a sample metadata file::

    {
        "app": {
            "plugins": ["java"]
        },

        "java": {
            "class": "MyJavaClass",
            "args": "arg1 'this is arg2'",
            "java_version": "1.7"
        }

        "env": {
            "msg": "This is the 'msg' environment variable"
        }
    }

Each section has a specific purpose:

``app``
  The ``app`` section contains genapp specific settings. It must contain a
  ``plugins`` list containing at least one plugin. Plugins are used to perform
  setup operations for an application. The "app" section name is reserved for
  use by genapp.

``java``
  This section provides attributes for java related plugins. The section name
  is not enforced by genapp -- plugins are free to use any unreserved section
  name as they see fit.

``env``
  The env section contains environment variables that may be used by the
  application. genapp installs ``.genapp/control/env`` which can be sourced by
  application control scripts to recreate the environment defined in the
  metadata.

genapp copies the application metadata to ``.genapp/metadata`` during
deployment. This copy serves as a reference of the metadata and is not
otherwise used by genapp.
