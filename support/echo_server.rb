require 'rubygems'
require 'sinatra'
require 'net/http'
require 'uri'

set :public, File.dirname(__FILE__) + '/public'
set :views,  File.dirname(__FILE__) + '/views'

get '/' do
  erb :index
end

get '/demo.js' do
  content_type :js
  erb :demo
end

post '/updates' do
  data = request.body.read
  Net::HTTP.post_form(URI.parse("http://#{server_name}:8901/send"),
                      {"data" => "Handled by Sinatra: #{data}"})
  puts "I got some data: #{data}"
  "success"
end

def server_name
  @request.env["SERVER_NAME"]
end
