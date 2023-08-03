deps:
	opam repo add janestreet-bleeding https://ocaml.janestreet.com/opam-repository
	opam repo add janestreet-bleeding-external https://github.com/janestreet/opam-repository.git#external-packages
	opam update
	opam upgrade
	opam install -y core core_unix bonsai ppx_jane ppx_css core_unix postgres_async ppx_jsonaf_conv
build:
	dune build
run_base:
	dune exec ./bin/main.exe
test:
	dune test
