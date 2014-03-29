protobuffsebindir = $(localerlanglibdir)/protobuffs-0.8.1p3/ebin

protobuffs_file_collection = \
    src/protobuffs.app.in \
    src/protobuffs.erl \
    src/protobuffs_compile.erl \
    src/protobuffs_file.erl \
    src/protobuffs_parser.yrl \
    src/protobuffs_scanner.xrl \
    src/pokemon_pb.erl

protobuffsebin_make_generated_file_list = \
    ebin/protobuffs.app \
    ebin/protobuffs.beam \
    ebin/protobuffs_compile.beam \
    ebin/protobuffs_file.beam \
    src/protobuffs_parser.erl \
    ebin/protobuffs_parser.beam \
    src/protobuffs_scanner.erl \
    ebin/protobuffs_scanner.beam \
    ebin/pokemon_pb.beam

protobuffsebin_DATA = \
    $(protobuffsebin_make_generated_file_list)

EXTRA_DIST = \
	$(protobuffs_file_collection)

CLEANFILES = \
    $(protobuffsebin_make_generated_file_list)

ERLC_FLAGS = +debug_info

ebin/:
	@mkdir -p $@

ebin/%.app: src/%.app.in
	cp $< $@

src/protobuffs_parser.erl: src/protobuffs_parser.yrl
	$(ERLC) -o src $(ERLC_FLAGS) $<

src/protobuffs_scanner.erl: src/protobuffs_scanner.xrl
	$(ERLC) -o src $(ERLC_FLAGS) $<

ebin/%.beam: src/%.erl
	$(ERLC) -o ebin $(ERLC_FLAGS) $<
