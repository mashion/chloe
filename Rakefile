require 'fileutils'
require 'bundler'
Bundler.require

desc "Install all necessary dependencies"
task :bootstrap do
  sh("bundle install")
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
task :compile => [:build_js, :test] do
  sh("rebar compile")
end

desc "Run unit tests for chloe"
task :test do
  sh("rebar app=chloe eunit")
end

desc "Generate a release on this box"
task :platform_release => :compile do
  # TODO (factor out version to not be hard coded)
  version = "0.0.3beta"
  FileUtils.rm_rf("./rel/chloe")
  FileUtils.rm_rf("./rel/chloe-#{version}")
  sh "rebar generate"
  sh "cp -r ./rel/chloe ./rel/chloe-#{version}"
  sh "cd ./rel && tar czf chloe-#{version}.tgz chloe-#{version}"
end

desc "Run demo echo server"
task :demo do
  require './support/echo_server'
  Sinatra::Application.run!
end

desc "Build chloe.js"
task :build_js do
  secretary = Sprockets::Secretary.new(
    :asset_root   => "public",
    :source_files => ["javascripts/chloe.js"]
  )

  secretary.concatenation.save_to("public/chloe.js")
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
