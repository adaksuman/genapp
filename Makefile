rebar = ./rebar

compile: deps
	$(rebar) compile

deps:
	$(rebar) get-deps

update-deps:
	$(rebar) update-deps

clean:
	$(rebar) clean

CONFIG=dev
shell: compile
	@if [ -e priv/$(CONFIG).config ]; then \
	   erl -config priv/$(CONFIG) -pa ebin $(wildcard deps/*/ebin) -s genapp_reloader -s genapp; \
	else \
	   echo "WARNING: priv/$(CONFIG).config missing, skipping genapp start"; \
	   erl -pa ebin $(wildcard deps/*/ebin) -s genapp_reloader; \
	fi
