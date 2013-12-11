
PREFIX:=../
DEST:=$(PREFIX)$(PROJECT)

REBAR=./rebar

rebuild:	del_deps \
	get_deps \
	clean \
	compile

edoc:
	@$(REBAR) doc

test: clean \
	compile
	@$(REBAR) ct

clean:
	@$(REBAR) clean
	@rm -rf ./test/*.beam
	@rm -f SMR-Master.zip

compile:
	@$(REBAR) compile

dialyzer:
	@$(REBAR) dialyze

get_deps:	del_deps
	@$(REBAR) get-deps

del_deps:
	@rm -rf ./deps

update-deps:
	@$(REBAR) update-deps
test-compile:
	@erlc -I include  -W0 -DTEST=true -o ./ebin src/*.erl

test_suite:clean \
		compile
		@$(REBAR) ct suite=packet_codec

rel: deps
	@$(REBAR) compile generate
	
release:
	rm -f RTSP-Server.zip
	rm -rf ./RTSP-Server
	rm -rf ./RTSP-server
	mkdir ./RTSP-Server
	cp -r ./src/ ./RTSP-Server/ 
	cp -r ./deps/ ./RTSP-Server/ 
	cp -r ./include/ ./RTSP-Server/ 
	cp ./rebar ./RTSP-Server/rebar
	cp ./rebar.config ./RTSP-Server/rebar.config
	cd ./RTSP-Server;./rebar clean;./rebar compile;mv ./deps/* ./;mkdir ./rtsp_server;mv ./ebin ./rtsp_server;mkdir ./rel;cd ./rel;../rebar create-node nodeid=RTSP-server;cp ../../reltool.config ./
	cd ./RTSP-Server;./rebar generate
	rm -rf ./RTSP-Server/src
	rm -rf ./RTSP-Server/deps
	rm -rf ./RTSP-Server/include
	cp -r ./RTSP-Server/rel/RTSP-server ./
	cp ./app.config ./RTSP-server/etc
	mkdir RTSP-server/git
	cp ../.git/HEAD RTSP-server/git/
	cp -r ../.git/refs RTSP-server/git/
	./rename
	rm -rf ./RTSP-Server
	rm -rf ./RTSP-server


	
	
	
