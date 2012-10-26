erl -sname data_broadcast -cookie data_broadcast -pa ebin -pa deps/*/ebin -boot start_sasl -s lager -s data_broadcast -config ./start.config -args_file start.args

