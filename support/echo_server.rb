require 'rubygems'
require 'sinatra'
require 'net/http'
require 'uri'
require 'digest/md5'

# SECRET = "YOUR_SECRET_GOES_HERE"

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
  puts request.inspect
  signature = request.env["HTTP_X_CHLOE_SIGNATURE"]
  raw_data = request.body.read
  data = "Handled by Sinatra: #{raw_data}"

  if SECRET && signature != Digest::MD5.hexdigest(raw_data + SECRET)
    puts "Signature invalid: signature=#{signature};calculated=#{Digest::MD5.hexdigest(raw_data + SECRET)}"
    return "failure"
  end

  sig  = Digest::MD5.hexdigest(data + SECRET)
  Net::HTTP.post_form(URI.parse("http://#{server_name}:8901/send"),
                      {"data" => data, "sig" => sig})
  puts "I got some data: #{data}"
  "success"
end

def server_name
  @request.env["SERVER_NAME"]
end
