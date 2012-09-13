Application Metadata
====================

Application metadata is defined in a file named ``metadata.json`` located in
the ``.genapp`` subdirectory of an application.

The metadata file contains sections of attributes. Section names are arbitrary
with the exception of the ``app`` section, which is reserved for use by genapp.

Pluins may use attributes from any section in the metadata. Some plugins may
require specific sections and attribute names, which others may use sections
and attribute names as optional values.
