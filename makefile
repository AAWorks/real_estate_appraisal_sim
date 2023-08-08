deps:
	opam repo add janestreet-bleeding https://ocaml.janestreet.com/opam-repository
	opam repo add janestreet-bleeding-external https://github.com/janestreet/opam-repository.git#external-packages
	opam update
	opam upgrade
	opam install -y core ocurl core_unix bonsai ppx_jane ppx_css core_unix postgres_async ppx_jsonaf_conv cohttp-lwt-unix cohttp_static_handler
build:
	dune build
run_server:
	git checkout prod_server
	./run-game-server.sh -port 8181
test:
	dune test
