-record(app, {id, meta, meta_home, dir, user, ports, service}).

-define(DEFAULT_SERVICES_HOME, "/var/genapp/services").
-define(DEFAULT_APPS_HOME, "/var/genapp/apps").
-define(DEFAULT_PLUGINS_CONF_HOME, "/etc/genapp/plugins.d").
-define(SETUP_STATUS, [pending, ok, error]).
