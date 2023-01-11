REBAR ?= rebar3
ROOT_TEST=_build/test/lib

ifndef suite
	SUITE_EXEC=
else
	SUITE_EXEC=-suite $(suite)_SUITE
endif

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

bench:
	$(REBAR) as bench compile
	erl -pa _build/bench/lib/*/ebin -noshell -eval "load_test:run(false, 10, 100000, 5*1024)." -eval "init:stop()." -config benchmark/sys.config

ct:
	mkdir -p log
	$(REBAR) ct --compile_only
	ct_run  -no_auto_compile \
			-cover test/cover.spec \
			-dir $(ROOT_TEST)/ebeanstalkd/test $(SUITE_EXEC) \
			-pa $(ROOT_TEST)/*/ebin \
			-logdir log
