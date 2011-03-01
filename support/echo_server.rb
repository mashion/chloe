require 'rubygems'
require 'sinatra'
require 'net/http'

set :public, File.dirname(__FILE__) + '/public'

get '/' do
  File.read(File.dirname(__FILE__) + '/public/index.html')
end

post '/updates' do
  data = request.body.read
  Net::HTTP.start("localhost", 8888) do |http|
    http.post('/send', "Handled by Sinatra: #{data}")
  end
  puts "I got some data: #{data}"
  "success"
end
