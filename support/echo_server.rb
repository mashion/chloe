require 'rubygems'
require 'sinatra'
require 'net/http'
require 'uri'

set :public, File.dirname(__FILE__) + '/public'

get '/' do
  File.read(File.dirname(__FILE__) + '/public/index.html')
end

post '/updates' do
  data = request.body.read
  Net::HTTP.post_form(URI.parse("http://localhost:8901/send"),
                      {"data" => "Handled by Sinatra: #{data}"})
  puts "I got some data: #{data}"
  "success"
end
