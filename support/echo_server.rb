require 'rubygems'
require 'sinatra'
require 'net/http'

post '/updates' do
  data = request.body.read
  Net::HTTP.start("localhost", 8888) do |http|
    http.post('/send', "Handled by Sinatra: #{data}")
  end
  puts "I got some data: #{data}"
  "success"
end
