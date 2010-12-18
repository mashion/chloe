task :console do
  sh("erl -pa ebin -pa deps/yaws/ebin")
end

task :compile do
  sh("rebar compile")
end

desc "Run unit tests for chloe"
task :test do
  sh("rebar app=chloe eunit")
end
