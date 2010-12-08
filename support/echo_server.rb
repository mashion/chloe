require 'rubygems'
require 'sinatra'

post '/updates' do
  puts "I got some data: #{request.body.read}"
  "success"
end
