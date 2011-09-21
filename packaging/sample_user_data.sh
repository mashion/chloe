#!/bin/bash
log="/tmp/init.log"

apt-get update >> $log
apt-get install -y ruby irb ri libopenssl-ruby1.8 libshadow-ruby1.8 ruby1.8-dev gcc g++ rsync curl >> $log

curl -L 'http://production.cf.rubygems.org/rubygems/rubygems-1.6.2.tgz' | tar xvzf -
cd rubygems* && ruby setup.rb --no-ri --no-rdoc >> $log
ln -sfv /usr/bin/gem1.8 /usr/bin/gem

gem install rdoc chef ohai --no-ri --no-rdoc --source http://gems.opscode.com --source http://gems.rubyforge.org >> $log

mkdir -p /etc/chef

cat << EOF > /etc/chef/solo.rb
file_cache_path  "/var/chef/cache"
file_backup_path "/var/chef/backup"
cookbook_path    ["/var/chef/cookbooks"]
role_path        "/var/chef/roles"
json_attribs     "/etc/chef/node.json"
verbose_logging  true
EOF

cat << EOF > /etc/chef/node.json
{
  "run_list": ["recipe[chloe]"]
}
EOF

chef-solo -r https://s3.amazonaws.com/chloe-trotter/chloe-chef-solo.tgz >> $log 2>&1

while ! ls /opt/chloe_chat_example
do
  sleep 1
done

cd /opt/chloe-0.0.5/bin && ./chloe start
/usr/bin/ruby /opt/chloe_chat_example/chat_server.rb > /var/chat_server.log 2>&1 &
