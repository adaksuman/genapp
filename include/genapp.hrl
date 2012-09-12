-record(app, {id, meta, pkg_dir, dir, user, ports}).

-define(DEFAULT_APPS_HOME, "/var/genapp/apps").
-define(DEFAULT_PLUGINS_HOME, "/etc/genapp/plugins.d").
-define(DEFAULT_TEMP_DIR, "/tmp").
-define(SETUP_STATUS, [pending, ok, error]).
-define(GENAPP_SUBDIR, ".genapp").
-define(GENAPP_LOG_SUBDIR, "log").
-define(GENAPP_CONTROL_SUBDIR, "control").
-define(GENAPP_PORTS_SUBDIR, "ports").
-define(GENAPP_METADATA_FILE, "metadata.json").
-define(GENAPP_METADATA_EXTRA_FILE, "metadata-extra.json").
-define(GENAPP_SETUP_STATUS_SUBDIR, "setup_status").
-define(GENAPP_ENV_FILE, "env").

-define(REMOTE_PLUGIN_CURL_RETRIES, 0).
-define(REMOTE_PLUGIN_CURL_TIMEOUT, 300).
-define(REMOTE_PLUGIN_TEMP_PREFIX, "genapp-remote-plugin-").
