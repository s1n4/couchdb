couch_pb_apiebindir = $(localerlanglibdir)/couch_pb_api-0.1.0/ebin

couch_pb_api_file_collection = \
    src/couch_pb_api_app.erl \
    src/couch_pb_api_sup.erl \
    src/couch_pb_api_listener.erl \
    src/couch_pb_api_server.erl

couch_pb_apiebin_make_generated_file_list = \
    ebin/couch_pb_api_app.beam \
    ebin/couch_pb_api_sup.beam \
    ebin/couch_pb_api_listener.beam \
    ebin/couch_pb_api_server.beam

couch_pb_apiebin_DATA = \
    $(couch_pb_apiebin_make_generated_file_list)

EXTRA_DIST = \
	$(couch_pb_api_file_collection)

CLEANFILES = \
    $(couch_pb_apiebin_make_generated_file_list)

ERLC_FLAGS = -o ebin -I$(top_srcdir)/src

ebin/%.beam: src/%.erl
	$(ERLC) $(ERLC_FLAGS) $<
