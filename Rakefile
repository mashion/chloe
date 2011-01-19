desc "Install all necessary dependencies"
task :bootstrap do
  sh("rebar get-deps") rescue :expected_to_fail
  sh("cd deps/yaws; rebar compile")
end

desc "Start an erlang console"
task :console => :compile do
  sh(erl)
end

desc "Start an erlang console running chloe"
task :server => :compile do
  sh(erl "-s chloe")
end

desc "Compile Chloe"
task :compile => :test do
  sh("rebar compile")
end

desc "Run unit tests for chloe"
task :test do
  sh("rebar app=chloe eunit")
end

def erl(extra="")
  "erl -pa ebin -pa deps/yaws/ebin #{extra}"
end
