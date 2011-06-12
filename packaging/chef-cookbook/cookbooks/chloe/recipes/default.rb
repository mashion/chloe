#
# Cookbook Name:: chloe
# Recipe:: default
#
# Copyright 2011, Mashion, LLC
#
# MIT Licensed
#

directory "/opt"

gem_package "sinatra"

execute "untar chloe" do
  command "cd /opt && tar xzvf /tmp/chloe.tgz"
end

remote_file "/tmp/chloe.tgz" do
  source "https://s3.amazonaws.com/chloe-trotter/chloe-0.0.1-ubuntu32.tgz"
  mode   "0644"
  notifies :run, resource(:execute => "untar chloe")
end

remote_file "/tmp/chloe_chat_example.tgz" do
  source "https://s3.amazonaws.com/chloe-trotter/chloe_chat_example.tgz"
  mode "0644"
end

execute "untar echo server" do
  command "cd /opt && tar xzvf /tmp/chloe_chat_example.tgz"
  not_if { File.exist?("/opt/echo_server.tgz") }
end
