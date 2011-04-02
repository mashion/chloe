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
  sh(erl "-s chloe -config ./rel/files/app")
end

desc "Compile Chloe"
task :compile => :test do
  sh("rebar compile")
end

desc "Run unit tests for chloe"
task :test do
  sh("rebar app=chloe eunit")
end

desc "Run demo echo server"
task :demo do
  require './support/echo_server'
  Sinatra::Application.run!
end

begin
  require('jslintrb-v8')
  task :jslint do
    linter = JSLint.new(
      :white =>    false,
      :undef =>    true,
      :nomen =>    false,
      :eqeqeq =>   true,
      :plusplus => true,
      :bitwise =>  true,
      :regexp =>   false,
      :strict =>   false,
      :newcap =>   true,
      :immed =>    true,
      :indent =>   2,
      :predef =>   "Chloe"
    )
    errors = []
    path = File.join('public', '**', '*.js')
    Dir[path].each do |f|
      puts "checking #{f}"
      e = linter.check(File.read(f))
      errors << "\nIn [#{f}]:\n#{e}\n" if e
    end
    if errors.empty?
      puts "JSLinty-fresh!"
    else
      $stderr.write(errors.join("\n")+"\n");
      raise "JSLint Errors Found"
    end
  end
rescue LoadError
  puts "jslintrb_v8 not installed. Not adding jslint task"
end

def erl(extra="")
  "erl -pa apps/chloe/ebin -pa deps/yaws/ebin #{extra}"
end
