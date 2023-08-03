deps:
	opam update
	opam install . --deps-only --with-test --yes
	opam install core
	opam install core_unix
	opam install bonsai
	opam install ppx_jane
	opam install ppx_css
	opam install core_unix
	opam install postgres_async
	opam install ppx_jsonaf_conv
build:
	dune build
run_base:
	dune exec ./bin/main.exe
test:
	dune test